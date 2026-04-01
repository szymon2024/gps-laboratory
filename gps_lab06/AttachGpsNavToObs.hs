-- 2026-04-01

{-| The program attaches navigation records from a RINEX 3.04 NAV file
    containing ephemerides to corresponding observations from a RINEX
    3.04 OBS file. Applies to GPS navigation records only.

    Satellite observations in the RINEX file are grouped by
    observation time into observation records.

    Main steps of the algorithm:

    1. Build a map of healthy navigation records from the body of a
    RINEX 3.04 navigation file, keeping only the record with the
    maximum IODE for each (week, toe).
     
    2. Build a list of GPS observation records from RINEX 3.04
    observation file.
    
    3. Attach navigation records with GPS ephemerides from built map
    to GPS observations using navSelectEphemeris function.

    Input:
      - RINEX 3.04 navigation file name                navFn     defined in the code
      - RINEX 3.04 observation file name               obsFn     defined in the code

    Output:
    - observation records with maybe
      attached ephemerides to observations             obsNavRs
        
    
    Print of run:
    Total observation records:       651
    Total observations:             7161
    Number of observations
    without attached ephemerides:      0
  
-}

----------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module AttachGpsNavToObs where
----------------------------------------------------------------------

import qualified Data.Map.Strict            as Map
import           Data.Map.Strict                   (Map)
import qualified Data.IntMap.Strict         as IntMap
import           Data.IntMap.Strict                (IntMap)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar                (fromGregorian, diffDays)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..))
import           Data.Fixed                        (Pico)
import           Data.Int                          (Int64)
import           Data.Char                         (isSpace)
import           Text.Printf                       (printf)
import           Data.Array.Unboxed

-- For readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)

----------------------------------------------------------------------

-- | GPS navigation data record from RINEX 3.04 navigation file.
data NavRecord = NavRecord
  { navPrn      :: Int               -- ^ satellite number
  , toc         :: GpsTime           -- ^ clock data reference time
  , af0         :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1         :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2         :: Double            -- ^ SV clock drift rate correction coefficient [s/s^2]
  , iode        :: Int               -- ^ issue-of-data, ephemeris; ephemeris data issue number,
  , crs         :: Double            -- ^ orbital radius correction [m]
  , deltaN      :: Double            -- ^ mean motion difference [rad/s]
  , m0          :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc         :: Double            -- ^ latitude argument correction [rad]
  , e           :: Double            -- ^ eccentricity []
  , cus         :: Double            -- ^ latitude argument correction [rad]
  , sqrtA       :: Double            -- ^ sqare root of semi-major axis [m^0.5]
  , toe         :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic         :: Double            -- ^ inclination correction [rad]
  , omega0      :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis         :: Double            -- ^ inclination correction [rad]
  , i0          :: Double            -- ^ inclination at reference epoch [rad]
  , crc         :: Double            -- ^ orbital radius corrcetion [m]
  , omega       :: Double            -- ^ argument of perigee [rad]
  , omegaDot    :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot        :: Double            -- ^ rate of inclination angle [rad/s]
  , week        :: Integer           -- ^ number of GPS week for toe and toc
  , svHealth    :: Int               -- ^ SV health, 0 means ok
  , iodc        :: Int               -- ^ issue-of-data, clock; clock data issue number
  , ttom        :: Double            -- ^ transmission time of message - time stamp given by receiver [s]
  , fitInterval :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Eq, Show)    

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)
type EphWeekTow = GpsWeekTow
type NavMap     = IntMap (Map EphWeekTow NavRecord)       -- ^ key1:  prn (satellite identifier)
                                                          --   key2:  (week, toe)
                                                          --   value2: navigation record for a healthy satellite
                                                          --           and with max iode for (week, toe)
type ObsTime    = GpsTime                                 -- ^ observation time (epoch)
                                                          --   (don't confuse with observing time)

type ObsType   = L8.ByteString
type Sys       = Char
    
data ObsRecord = ObsRecord
    { obsPrn  :: Int
    } deriving Show
                 
type ObsDataRecord   = (ObsTime, [ObsRecord])

----------------------------------------------------------------------
    
main :: IO ()
main = do
  let navFn = "rinex.nav"                                   -- Input: RINEX 3.04 navigation file name
      obsFn = "rinex.obs"                                   -- Input: RINEX 3.04 observation file name
  
  navBs <- L8.readFile navFn
  obsBs <- L8.readFile obsFn

  let navMap        = navMapFromRinex navBs
      obsRs         = obsDataRecordsFromRinex obsBs
      obsMaybeNavRs = obsAttachNavs navMap obsRs            -- Output: observation records
                                                            -- with maybe attached ephemerides to observations
                      
      numObs =                                              -- Count observations
        foldl' (\acc (_, xs) -> acc + length xs
               ) 0 obsRs
               
      numObsNothingNavRs =                                  -- Count observations without navigation
        foldl' (\acc (_, xs) -> acc + foldl' count 0 xs     -- records (ephemerides)
               ) (0::Integer) obsMaybeNavRs
          where
            count n (_, Nothing) = n + 1
            count n _            = n

  printf "Total observation records:    %6d\n" (length obsRs)
  printf "Total observations:           %6d\n" numObs
  printf "Number of observations\n\
         \without attached ephemerides: %6d\n" numObsNothingNavRs



----------------------------------------------------------------------
-- READ OBSERVATION RINEX FILE BODY
----------------------------------------------------------------------

obsDataRecordsFromRinex :: L8.ByteString -> [ObsDataRecord]
obsDataRecordsFromRinex bs0
  | L8.null bs0           = error "Empty file"
  | rnxVer      /= "3.04" = error "Not RINEX 3.04 file"
  | rnxFileType /= "O"    = error "Not an observation file"
  | otherwise = let (sts, rnxBody) = readObsTypes bs0
                    tMap           = Map.fromList sts
                    nMap           = Map.map fst tMap
                in if L8.null rnxBody
                   then error "Cannot find observation data in the file."
                   else readRecords nMap rnxBody
  where
    readRecords :: (Map Sys Int) -> L8.ByteString -> [ObsDataRecord]
    readRecords nMap bs
        | L8.null bs         = []
        | L8.head bs == '>'  =
            let ((tobs,_,_), bs1) = readEpochGpsRecord bs
                (rs        , bs2) = readObsGpsRecords nMap bs1                   
            in (tobs, rs) : readRecords nMap bs2
        | otherwise          =
            error "Unexpected line: expected '>' at start of record"

    rnxVer      = trim $ takeField  0 9 bs0
    rnxFileType = trim $ takeField 20 1 bs0

----------------------------------------------------------------------

-- | Read observation time (epoch) record
readEpochGpsRecord :: L8.ByteString -> ((ObsTime, Int, Int), L8.ByteString)
readEpochGpsRecord bs
    | L8.null bs         = error "Cannot read epoch record. Empty input."
    | L8.head bs == '>'  = 
        let
            y   = getFieldInt     2  4 bs
            mon = getFieldInt     7  2 bs
            d   = getFieldInt    10  2 bs
            h   = getFieldInt    13  2 bs
            m   = getFieldInt    16  2 bs
            s   = getFieldDouble 19 11 bs
            flg = getFieldInt    32  1 bs
            !tobs = mkGpsTime (toInteger y) mon d h m (realToFrac s)
            -- number of satellites observed in current epoch
            n   = getFieldInt    33  3 bs
            bs' = dropLine bs
        in ((tobs, flg, n), bs')
    | otherwise          = error "Unexpected char. Expected '>'."
    where
      dropLine = snd . readEOL . dropToEOL . L8.drop 56

----------------------------------------------------------------------

-- | Read GPS observations records. The length of line is computed from
--   number of observation types for sat system.
readObsGpsRecords :: (Map Sys Int) -> L8.ByteString -> ([ObsRecord], L8.ByteString)
readObsGpsRecords m bs
    | L8.null bs  = ([], bs)            -- exit
    | sys == '>'  = ([], bs)            -- exit
    | sys == 'G'  =
        let
            r   = ObsRecord { obsPrn = getFieldInt 1 2 bs}
            bs1 = dropLine bs
            (rs, bs2) = readObsGpsRecords  m bs1
        in (r:rs, bs2)
    | otherwise   = readObsGpsRecords m (dropLine bs)
                    
    where
      sys      = L8.head bs
      dropLine = snd . readEOL . L8.drop (fromIntegral (3 + n *16))
      n        = case  (m Map.!? sys) of
                   Just x  -> x
                   Nothing -> error $
                              "Cannot find number of observation \
                              \types for '" ++ [sys] ++ "'"

----------------------------------------------------------------------

-- | Attach navigation records to observations.
obsAttachNavs :: NavMap -> [ObsDataRecord] -> [(ObsTime, [(ObsRecord, Maybe NavRecord)])]
obsAttachNavs navMap rs =
 [ (tobs, [ (obsR, eph)
          | obsR <- obsRs
          , let eph = selectGpsEphemeris tobs (obsPrn obsR) navMap
          ]
   )
 | (tobs, obsRs) <- rs
 ]


----------------------------------------------------------------------
-- READ OBSERVATION TYPES FROM OBSERVATION RINEX FILE HEADER
----------------------------------------------------------------------

obsTypeFieldLen :: Int64
obsTypeFieldLen = 3

maxObsTypesPerLine :: Int
maxObsTypesPerLine = 13

-- Observation type positions in the line:
-- 6?,13(1X,A3).
-- It is constructed only once, because it is top-level constant.
posA :: UArray Int Int64
posA = listArray (0,12) [7,11..55]
                     

----------------------------------------------------------------------
                    
readObsTypes :: 
     L8.ByteString
  -> ([(Sys, (Int, [ObsType]))], L8.ByteString)
readObsTypes bs
    | L8.null bs                            = ([], bs)
    | lookEOH bs                            = ([], dropLastLine bs)
    | lookLabel bs == "SYS / # / OBS TYPES" =
        let (sys, n, n', ts) = readObsTypesFirstLine bs
            bs1 = dropLine80 bs
            (ts', bs2) = readObsTypesContLines n' (reverse ts) bs1
            (sts, bs3) = readObsTypes bs2
        in ((sys, (n, ts')) : sts, bs3)
    | otherwise =
        readObsTypes (dropLine80 bs)
    where
      
      dropLastLine =  snd . readEOL . dropToEOL . L8.drop 73

----------------------------------------------------------------------                    
                      
readObsTypesFirstLine
    :: L8.ByteString
    -> (Sys, Int, Int, [ObsType])
readObsTypesFirstLine bs
    | sys `L8.elem` "GREJCIS"  =
      let 
            k   = min n maxObsTypesPerLine
            fs  = takeObsTypes k bs
        in if n>=0
           then (sys, n, n-k, fs)
           else errorWithoutStackTrace
                    "A negative number of observation types was declared."
    | sys == ' '               =
        errorWithoutStackTrace
        "No satellite system designation in the line \n\
              \with SYS / # / OBS TYPES label."
    | otherwise                =
        errorWithoutStackTrace $
          "Errr\n\
          \An unexpected satellite system '" ++ [sys]
          ++ "' in the input file header.\n\
          \Allowed: G, R, E, J, C, I, S."
    where
      sys = L8.index bs 0
      n   = getFieldInt 3 3 bs
                     
----------------------------------------------------------------------

readObsTypesContLines
     :: Int
     -> [ObsType]
     -> L8.ByteString
     -> ([ObsType], L8.ByteString)
readObsTypesContLines n acc bs
    | n == 0     = (reverse acc, bs)              -- exit
    | n <  0     = error "More observation types were read than declared."
    | L8.null bs = error "Unexpected end of header while reading observation types."
    | lookEOH bs = error "END OF HEADER reached before all observation types were read."

    | lookLabel bs == "SYS / # / OBS TYPES" =
        let k    = min n maxObsTypesPerLine
            fs   = takeObsTypes k bs
            bs1  = dropLine80 bs
            acc' = reverse fs ++ acc
        in readObsTypesContLines (n-k) acc' bs1

    | otherwise =
        readObsTypesContLines n acc (dropLine80 bs)

----------------------------------------------------------------------
                              
takeObsTypes
  :: Int                                -- ^ Number of observatin types to take
  -> L8.ByteString
  -> [ObsType]
takeObsTypes n bs = go 0
    where
      len = obsTypeFieldLen
            
      go i
          | i >= n    = []
          | otherwise =
              let pos = (posA ! i)
                  fld = takeField pos len bs
              in if L8.any isSpace fld
                 then error $
                          "Cannot read observation type at \
                          \position " ++ show pos ++ " from \""
                          ++ L8.unpack (L8.take (pos+len) bs) ++ "\"."
                 else fld : go (i+1)

----------------------------------------------------------------------
            
lookLabel :: L8.ByteString -> L8.ByteString
lookLabel = trim . L8.drop 60 . L8.take 80


----------------------------------------------------------------------
-- NAVIGATION RINEX FILE PROCESSING
----------------------------------------------------------------------

-- | Build a navigation map from GPS navigation records of RINEX 3.04
--   navigation body for healthy satellites and with max iode for
--   (week, toe).
navMapFromRinex :: L8.ByteString -> NavMap  
navMapFromRinex bs0
    | L8.null bs0           = error "Empty file"
    | rnxVer      /= "3.04" = error "Not RINEX 3.04 file"
    | rnxFileType /= "N"    = error "Not navigation file"
    | otherwise = let rnxBody = dropRnxHeader bs0
                  in if L8.null rnxBody
                     then error "Cannot find navigation data in the file."
                     else go IntMap.empty rnxBody
      where
        rnxVer      = trim $ takeField  0 9 bs0 
        rnxFileType = trim $ takeField 20 1 bs0
                   
        go :: NavMap -> L8.ByteString -> NavMap
        go m bs
          | L8.null bs = m
          | sys == 'G'  =
              let (r, bs') = readGpsNavRecord bs
                  m' | svHealth r == 0 = insertGpsNavRecord r m
                     | otherwise       =  m
              in go m' bs'
          | sys == 'R'  = go m (dropGlonassNavRecord bs)                 
          | sys == 'E'  = go m (dropGalileoNavRecord bs)
          | sys == 'J'  = go m (dropQZSSNavRecord    bs)
          | sys == 'C'  = go m (dropBDSNavRecord     bs)
          | sys == 'I'  = go m (dropIRNSSNavRecord   bs)                          
          | sys == 'S'  = go m (dropSBASNavRecord    bs)
          | otherwise   = error $
                           "Unexpected char at the beginning \
                           \of the line starting with \"" ++
                            L8.unpack (L8.take 30 bs) ++ "\"."
          where
            sys = L8.index bs 0


----------------------------------------------------------------------

-- | Drop header of RINEX 3.04 file. It can be used for both
--   navigation and observation rinex files. Uses information about
--   the label position and the fixed length of the header line
--   content.  The header record takes one line and consists of:
--    - data field   0-59,
--    - label field 60-79.
--   The function cannot use the RINEX 3.04 specification knowledge
--   that the width of a line should always be 80 characters, because
--   last line sometimes breaks this rule.
dropRnxHeader :: L8.ByteString -> L8.ByteString
dropRnxHeader bs0
    | L8.null bs0             = error
                                "Empty input."
    | rnxVer bs0 /= "3.04"    = error
                                "Not RINEX 3.04 file"
    | otherwise               = go bs0
    where
      go :: L8.ByteString -> L8.ByteString
      go bs
          | L8.null bs = error
                         "END OF HEADER not found."
          | lookEOH bs = dropLastLine bs
          | otherwise  = go . dropLine80 $ bs

      dropLastLine =  snd . readEOL . dropToEOL . L8.drop 73
                    
      rnxVer      = trim . takeField  0 9

-- | Read GPS satellite navigation record
readGpsNavRecord :: L8.ByteString -> (NavRecord, L8.ByteString)
readGpsNavRecord bs =
    let ((navPrn, toc, af0, af1, af2), bs1)  = readLine1NavData bs
              
        ((iodeD, crs, deltaN, m0), bs2)   = readLine2NavData bs1
              
        ((cuc, e, cus, sqrtA), bs3)       = readLine3NavData bs2
              
        ((toeD, cic, omega0, cis), bs4)   = readLine4NavData bs3
              
        ((i0, crc, omega, omegaDot), bs5) = readLine5NavData bs4
              
        ((iDot, weekD), bs6)              = readLine6NavData bs5

        ((svHealthD, iodcD), bs7)         = readLine7NavData bs6
              
        ((ttom, fitIntervalD), bs8)       = readLine8NavData bs7

        iode         = round      iodeD
        toe          = realToFrac toeD
        week         = round      weekD           -- conversion is needed for equality comparisons
        svHealth     = round      svHealthD
        iodc         = round      iodcD
        fitInterval  = round      fitIntervalD
              
    in (NavRecord {..}, bs8)

----------------------------------------------------------------------
                            
readLine1NavData
    :: L8.ByteString
    -> ((Int, GpsTime, Double, Double, Double), L8.ByteString)
readLine1NavData bs =
    let
        prn  = getFieldInt  1 2 bs       -- dropSpace is needed by readInt
        y    = getFieldInt  4 4 bs
        mon  = getFieldInt  9 2 bs
        d    = getFieldInt 12 2 bs
        h    = getFieldInt 15 2 bs
        m    = getFieldInt 18 2 bs
        s    = getFieldInt 21 2 bs
  
        toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)
            
        af0       = getFieldDouble 23 19 bs
        af1       = getFieldDouble 42 19 bs
        af2       = getFieldDouble 61 19 bs
                    
  in ((prn, toc, af0, af1, af2), dropLine80 bs)

----------------------------------------------------------------------
         
readLine2NavData
    :: L8.ByteString
    -> ((Double, Double, Double, Double), L8.ByteString)
readLine2NavData bs =
    let
        iode      = getFieldDouble  4 19 bs
        crs       = getFieldDouble 23 19 bs
        deltaN    = getFieldDouble 42 19 bs
        m0        = getFieldDouble 61 19 bs
                    
    in ((iode, crs, deltaN, m0), dropLine80 bs)

----------------------------------------------------------------------
         
readLine3NavData
    :: L8.ByteString
    -> ((Double, Double, Double, Double), L8.ByteString)
readLine3NavData bs =
    let
        cuc       = getFieldDouble  4 19 bs
        e         = getFieldDouble 23 19 bs
        cus       = getFieldDouble 42 19 bs
        sqrtA     = getFieldDouble 61 19 bs
                    
    in ((cuc, e, cus, sqrtA), dropLine80 bs)

----------------------------------------------------------------------
         
readLine4NavData
    :: L8.ByteString
    -> ((Double, Double, Double, Double), L8.ByteString)
readLine4NavData bs =
    let
        toe       = getFieldDouble  4 19 bs
        cic       = getFieldDouble 23 19 bs
        omega0    = getFieldDouble 42 19 bs
        cis       = getFieldDouble 61 19 bs
    in ((toe, cic, omega0, cis), dropLine80 bs)

----------------------------------------------------------------------
         
readLine5NavData
    :: L8.ByteString
    -> ((Double, Double, Double, Double), L8.ByteString)
readLine5NavData bs =
    let
        i0        = getFieldDouble  4 19 bs
        crc       = getFieldDouble 23 19 bs
        omega     = getFieldDouble 42 19 bs
        omegaDot  = getFieldDouble 61 19 bs
                    
    in ((i0, crc, omega, omegaDot), dropLine80 bs)

----------------------------------------------------------------------
         
readLine6NavData
    :: L8.ByteString
    -> ((Double, Double), L8.ByteString)
readLine6NavData bs =
    let
        iDot      = getFieldDouble  4 19 bs
        weekD     = getFieldDouble 42 19 bs
                    
    in ((iDot, weekD), dropLine80 bs)

----------------------------------------------------------------------
         
readLine7NavData
    :: L8.ByteString
    -> ((Double, Double), L8.ByteString)
readLine7NavData bs =
    let
        svHealthD  = getFieldDouble 23 19 bs
        iodc       = getFieldDouble 61 19 bs
                     
    in ((svHealthD, iodc), dropLine80 bs)

----------------------------------------------------------------------
         
readLine8NavData
    ::  L8.ByteString
    -> ((Double, Double), L8.ByteString)
readLine8NavData bs =
    let
        ttom          = getFieldDouble  4 19 bs
        fitIntervalD  = getFieldDouble 23 19 bs
                        
    in ((ttom, fitIntervalD), dropLastLine bs)
       
    where
      dropLastLine =  snd . readEOL . dropToEOL . L8.drop 42

----------------------------------------------------------------------

dropLine80 :: L8.ByteString -> L8.ByteString
dropLine80   =  snd . readEOL . L8.drop 80

----------------------------------------------------------------------

dropToEOL :: L8.ByteString -> L8.ByteString
dropToEOL = L8.dropWhile (not . (`L8.elem` "\r\n"))

----------------------------------------------------------------------
  
readEOL :: L8.ByteString -> (L8.ByteString, L8.ByteString)
readEOL bs =
    case L8.uncons bs of
      Just ('\n', rest)  -> ("\n", rest)
      Just ('\r', rest1) -> case L8.uncons rest1 of
                              Just ('\n', rest2) -> ("\r\n", rest2)
                              _                  -> ("\r"  , rest1)
      _                  -> error $ "Cannot find end of line in \""
                                  ++ L8.unpack (L8.take 30 bs) ++ "\""

----------------------------------------------------------------------

dropGalileoNavRecord :: L8.ByteString -> L8.ByteString
dropGalileoNavRecord = dropLastLine . drop7Lines
    where
      drop7Lines  bs = iterate dropLine80 bs !! 7
      dropLastLine = snd .readEOL . dropToEOL . L8.drop 23
      
----------------------------------------------------------------------

dropGlonassNavRecord :: L8.ByteString -> L8.ByteString
dropGlonassNavRecord = drop4Lines
    where
      drop4Lines  bs = iterate dropLine80 bs !! 4
      
----------------------------------------------------------------------

dropQZSSNavRecord :: L8.ByteString -> L8.ByteString
dropQZSSNavRecord = dropLastLine . drop7Lines
    where
      drop7Lines  bs = iterate dropLine80 bs !! 7
      dropLastLine = snd .readEOL . dropToEOL . L8.drop 42

----------------------------------------------------------------------
                     
dropBDSNavRecord :: L8.ByteString -> L8.ByteString
dropBDSNavRecord = dropLastLine . drop7Lines
    where
      drop7Lines  bs = iterate dropLine80 bs !! 7
      dropLastLine = snd .readEOL . dropToEOL . L8.drop 42
      
----------------------------------------------------------------------

dropSBASNavRecord :: L8.ByteString -> L8.ByteString
dropSBASNavRecord = drop4Lines
    where
      drop4Lines  bs = iterate dropLine80 bs !! 4
      
----------------------------------------------------------------------

dropIRNSSNavRecord :: L8.ByteString -> L8.ByteString
dropIRNSSNavRecord = dropLastLine . drop7Line . drop6Line . drop5Lines
    where
      drop5Lines  bs = iterate dropLine80 bs !! 5
      drop6Line    = snd .readEOL . dropToEOL . L8.drop 61
      drop7Line    = snd .readEOL . dropToEOL . L8.drop 61
      dropLastLine = snd .readEOL . dropToEOL . L8.drop 23
      
----------------------------------------------------------------------       

-- | Insert a navigation record into a 'NavMap'. If there is no entry
--   for the given PRN or epoch, the record is inserted. If an entry
--   already exists, the record is replaced only if the new record has
--   a greater IODE than the existing one.  This ensures that for each
--   @(week, toe)@ only the navigation record with the maximum IODE is
--   kept.
insertGpsNavRecord :: NavRecord -> NavMap -> NavMap
insertGpsNavRecord r =
  IntMap.alter updatePrn key1
  where
    key1 = navPrn r
    key2 = (week r, toe r)
    updatePrn Nothing =
        Just (Map.singleton key2 r)
    updatePrn (Just subMap) =
        Just (Map.alter (chooseNewer r) key2 subMap)

    chooseNewer :: NavRecord -> Maybe NavRecord -> Maybe NavRecord
    chooseNewer new Nothing    = Just new
    chooseNewer new (Just old) =
        if iode new > iode old
        then Just new
        else Just old

----------------------------------------------------------------------

-- | Selects a navigation record for a given observation time and
--   satellite PRN from NavMap. The navigation record with the nearest
--   (week, toe) to the specified observation time is selected.
selectGpsEphemeris
    :: GpsTime
    -> Int
    -> NavMap
    -> Maybe NavRecord
selectGpsEphemeris tobs prn navMap = do
    subMap <- IntMap.lookup prn navMap
    let t  = gpsTimeToWeekTow tobs
        past   = Map.lookupLE t subMap
        future = Map.lookupGE t subMap
        closest = case (past, future) of
          (Just (wtoeP, rP), Just (wtoeF, rF)) ->
              if abs (diffGpsWeekTow t wtoeP) <= abs (diffGpsWeekTow wtoeF t)
              then Just (wtoeP, rP)
              else Just (wtoeF, rF)
          (Just p, Nothing)  -> Just p
          (Nothing, Just f)  -> Just f
          (Nothing, Nothing) -> Nothing                              
    (_, r) <- closest
    if isEphemerisValid t r
      then Just r
      else Nothing

----------------------------------------------------------------------

-- | Ephemeris validity check based on fit interval ephemeris field for
--   a given observation time
isEphemerisValid
  :: GpsWeekTow                                             -- GPS week, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) eph =
    abs diffTime <= halfFitInterval
    where
      diffTime        = diffGpsWeekTow  (w, tow) (week eph, toe eph)
      halfFitInterval = realToFrac ((fitInterval eph) `div` 2 * 3600)

----------------------------------------------------------------------
-- HELPER FUNCTIONS
----------------------------------------------------------------------

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

----------------------------------------------------------------------

-- | Function import: double strtod(const char *nptr, char **endptr)
foreign import ccall unsafe "stdlib.h strtod"
    c_strtod :: CString -> Ptr (Ptr CChar) -> IO CDouble

-- | 2025-11-01 Data.ByteString.Char8 does not have a readDouble function.
--   Reads Double value from Data.ByteString.Lazy.Char8 ByteString.
--   unsafeUseAsCString function needs strict argument and L8.drop
--   needs Int64 argument.
readDouble :: L8.ByteString -> Maybe (Double, L8.ByteString)
readDouble bs = unsafePerformIO $
    BSU.unsafeUseAsCString (L8.toStrict bs) $ \cstr -> 
      alloca $ \endPtr -> do
        val <- c_strtod cstr endPtr
        end <- peek endPtr
        if end == cstr
          then return Nothing
          else do
            let offset = end `minusPtr` cstr
            let rest   = L8.drop (fromIntegral offset) bs
            return (Just (realToFrac val, rest))

----------------------------------------------------------------------

getFieldDouble
    :: Int64                            -- ^ start position of field
    -> Int64                            -- ^ length of field
    -> L8.ByteString                    
    -> Double
getFieldDouble start len bs = do
  case readDouble (trim f) of
    Just (val, rest)
      | L8.null rest -> val
    _                -> error $ unwords
                           [ "\nCannot read Double from field at pos = ", show start
                           , " length = ", show len
                           , " field = ", show f
                           , "\nLine: ", show $ L8.takeWhile (not . (`L8.elem` "\n\r")) bs
                           ]
  where
      f = takeField start len bs

----------------------------------------------------------------------

-- | Get Int value from ByteString field.
getFieldInt
    :: Int64                            -- ^ start position of field
    -> Int64                            -- ^ length of field
    -> L8.ByteString                    
    -> Int
getFieldInt start len bs = do
  case L8.readInt (trim f) of
    Just (val, rest)
      | L8.null rest -> val
    _                -> error $ unwords
                           [ "\nCannot read Int from field at pos = ", show start
                           , " length = ", show len
                           , " field = ", show f
                           , "\nLine: ", show $ L8.takeWhile (not . (`L8.elem` "\n\r")) bs
                           ]
  where
      f = takeField start len bs

----------------------------------------------------------------------

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

----------------------------------------------------------------------

-- | Conversion of GPS time to GPS week and time-of-week
gpsTimeToWeekTow
    :: GpsTime
    -> GpsWeekTow
gpsTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = diffDays date gpsStartDate           -- number of days since GPS start date
        (w, dow)     = days `divMod` 7                      -- GPS week, GPS day-of-week
        tow          =
            fromIntegral ( dow * 86400
                         + toInteger (h * 3600 + m * 60)
                         )
            + s
    in (w, tow)
                        
----------------------------------------------------------------------            

-- | Calculates the number of seconds between two (GPS week, tow).
diffGpsWeekTow
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week [s]
    -> GpsWeekTow                                           -- ^ GPS week, time-of-week [s]
    -> Pico                                                 -- ^ time difference [s]
diffGpsWeekTow (w2,tow2) (w1,tow1) =
    fromInteger (dw * 604800) + dtow
    where
      dw   = w2   - w1
      dtow = tow2 - tow1

----------------------------------------------------------------------

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

----------------------------------------------------------------------

-- | Drop leading whitespaces from a lazy ByteString.              
dropSpace :: L8.ByteString -> L8.ByteString
dropSpace = L8.dropWhile isSpace
             
----------------------------------------------------------------------

lookEOH :: L8.ByteString -> Bool
lookEOH   = (== "END OF HEADER") . L8.take 13 . dropSpace . L8.drop 60
