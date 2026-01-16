-- 2026-01-16

{- | The program selects a navigation record containing ephemeris from
     the RINEX 3.04 navigation file for a given GPS observation time
     (receiver time of signal reception) and GPS satellite.

     Navigation file entries are keyed by PRN, toc, and iode. However,
     this program does not use this key to select a navigation
     record. Instead, it builds

     type NavMap = IntMap (Map EphWeekTow NavRecord)

     from navigation records correspongind to healthy satellites.
     For each (week, toe) only the record with the
     maximum IODE is retained.

     Main steps of the algorithm:
     
     1. Building a map of healthy navigation records, keeping only the
     record with the maximum IODE for each (week, toe) from the body
     of a rinex 3.04 navigation file.
     
     2. Select the navigation record whose (week, toe) is nearest to
     the specified observation time. If no record can be selected, the
     program terminates.

     3. Check whether the selected record lies within the fitInterval
     relative to (week, toe). If the condition is satisfied, the
     record is printed; otherwise, the program terminates.

     Input:
       - RINEX 3.04 navigation file name                    fn
       - observation time
         (receiver time of signal reception)                tobs
       - satellite number                                   prn       


     Output:
       - navigation record with ephemeris                   r

     Print of run:
     Observation time: 2025 08 02 01 00 01.5
       Ephemeris time: 2025 08 02 02 00 00
     PRN:  6      toc: 2025 08 02 02 00 00
     af0:         -4.722196608782E-4
     af1:        -1.432454155292E-11
     af2:           0.000000000000E0

     iode:         69
     crs:          -2.368750000000E1
     deltaN:       3.821944913632E-9
     m0:           -2.959416369262E0

     cuc:         -1.283362507820E-6
     e:            3.335768124089E-3
     cus:          4.727393388748E-6
     sqrtA:         5.153617370605E3

     toe:           5.256000000000E5
     cic:          8.940696716309E-8
     omega0:       6.818382481570E-1
     cis:          4.470348358154E-8

     i0:           9.884960063693E-1
     crc:           3.054062500000E2
     omega:       -6.694838801080E-1
     omegaDot:    -7.833540584123E-9

     iDot:       -1.753644474903E-10
     skipped
     week:       2377
     skipped

     skipped
     svHealth:      0
     skipped
     iodc:         69

     ttom:          5.184000000000E5
     fitInterval:     4
----------------------------------

-}

----------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

----------------------------------------------------------------------

import qualified Data.Map.Strict    as Map
import           Data.Map.Strict                   (Map)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict                (IntMap)
import           Data.Char                         (isSpace)
import           Data.Int                          (Int64)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar                (fromGregorian, diffDays, addDays)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..))
import           Data.Time.Format                  (formatTime, defaultTimeLocale)
import           Data.Fixed                        (Pico)
import           Text.Printf                       (PrintfType, printf)

-- to readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)

----------------------------------------------------------------------

-- | GPS navigation data record from RINEX 3.04 navigation file (subset of fields).
data NavRecord = NavRecord
  { prn          :: Int               -- ^ satellite number
  , toc          :: GpsTime           -- ^ clock data reference time
  , af0          :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1          :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2          :: Double            -- ^ SV clock drift rate correction coefficient [s/s^2]
  , iode         :: Int               -- ^ issue-of-data, ephemeris; ephemeris data issue number,
  , crs          :: Double            -- ^ orbital radius correction [m]
  , deltaN       :: Double            -- ^ mean motion difference [rad/s]
  , m0           :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc          :: Double            -- ^ latitude argument correction [rad]
  , e            :: Double            -- ^ eccentricity []
  , cus          :: Double            -- ^ latitude argument correction [rad]
  , sqrtA        :: Double            -- ^ sqare root of semi-major axis [m^0.5]
  , toe          :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic          :: Double            -- ^ inclination correction [rad]
  , omega0       :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis          :: Double            -- ^ inclination correction [rad]
  , i0           :: Double            -- ^ inclination at reference epoch [rad]
  , crc          :: Double            -- ^ orbital radius corrcetion [m]
  , omega        :: Double            -- ^ argument of perigee [rad]
  , omegaDot     :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot         :: Double            -- ^ rate of inclination angle [rad/s]
  , week         :: Integer           -- ^ number of GPS week for toe and toc
  , svHealth     :: Int               -- ^ SV health, 0 means ok
  , iodc         :: Int               -- ^ issue-of-data, clock; clock data issue number
  , ttom         :: Double            -- ^ transmission time of message - time stamp given by receiver [s]
  , fitInterval  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Show)
    
type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)                      -- ^ GPS week, time-of-week
type EphWeekTow = GpsWeekTow
type NavMap     = IntMap (Map EphWeekTow NavRecord)    -- ^ key1:  prn (satellite identifier)
                                                       --   key2:  (week, toe)
                                                       --   value2: navigation record for a healthy satellite
                                                       --           and with max iode for (week, toe)

----------------------------------------------------------------------

-- Entry point of the program.
main :: IO ()
main = do
  let fn   = "rinex.nav"                                    -- Input: RINEX 3.04 navigation file name
      tobs = mkGpsTime 2025 08 02 01 00 01.5                -- Input: observation time (receiver time of signal reception)
      prn  = 6                                              -- Input: satellite number
  bs <- L8.readFile fn
  let navMap = navMapFromRinex bs
  case selectGpsEphemeris tobs prn navMap of
    Nothing -> printf "Cannot find valid ephemeris \
                      \for given prn and observation time\n"
    Just r  -> do                                           -- Output: GPS navigation record 
         printf "Observation time: %s\n"
                (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tobs)
         printfRecord r

----------------------------------------------------------------------                      
                      
-- | Selects a navigation record for a given observation time and
-- satellite PRN from NavMap. The navigation record with the nearest
-- (week, toe) to the specified observation time is selected.
selectGpsEphemeris
    :: GpsTime                                    -- ^ observation time
    -> Int                                        -- ^ PRN (satellite identfier)
    -> NavMap                                     -- ^ map with navigation records
    -> Maybe NavRecord                            -- ^ selected navigation record
selectGpsEphemeris tobs prn navMap = do
    subMap <- IntMap.lookup prn navMap
    let wtobs  = gpsTimeToWeekTow tobs
        past   = Map.lookupLE wtobs subMap
        future = Map.lookupGE wtobs subMap
        closest = case (past, future) of
          (Just (wtoeP, rP), Just (wtoeF, rF)) ->
              if abs (diffGpsWeekTow wtobs wtoeP) <= abs (diffGpsWeekTow wtoeF wtobs)
              then Just (wtoeP, rP)
              else Just (wtoeF, rF)
          (Just p, Nothing)  -> Just p
          (Nothing, Just f)  -> Just f
          (Nothing, Nothing) -> Nothing                              
    (_, r) <- closest
    if isEphemerisValid wtobs r
      then Just r
      else Nothing                      

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
          | sys == "G" =
              let (r, bs') = readGpsNavRecord bs
                  m' | svHealth r == 0 = insertNavRecord r m
                     | otherwise       =  m
              in go m' bs' 
          | sys == "R" = go m (dropGlonassNavRecord bs)
          | sys == "E" = go m (dropGalileoNavRecord bs)
          | sys == "J" = go m (dropQZSSNavRecord    bs)
          | sys == "C" = go m (dropBDSNavRecord     bs)
          | sys == "I" = go m (dropIRNSSNavRecord   bs)                         
          | sys == "S" = go m (dropSBASNavRecord    bs)
          | otherwise    = error $
                           "Unexpected char at the beginning \
                           \of the line starting with \"" ++
                            L8.unpack (L8.take 30 bs) ++ "\"."
          where
            sys = L8.take 1 bs

----------------------------------------------------------------------

-- | Drop header of RINEX 3.04 file. Uses information about
--   the label position and the fixed length of the header line
--   content.  The header record takes one line and consists of:
--    - data field   0-59,
--    - label field 60-79.
--   The function cannot use the RINEX 3.04 specification knowledge
--   that the width of a line should always be 80 characters, because
--   last line sometimes breaks this rule and ends after END OF
--   HEADER.
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

      lookEOH = (== "END OF HEADER") . L8.take 13 . dropSpace . L8.drop 60

      dropLastLine =  snd . readEOL . L8.dropWhile (not . (`L8.elem` "\r\n")) . L8.drop 73
                    
      rnxVer      = trim . takeField  0 9

----------------------------------------------------------------------

-- | Read GPS satellite navigation record
readGpsNavRecord :: L8.ByteString -> (NavRecord, L8.ByteString)
readGpsNavRecord bs =
    let ((prn, toc, af0, af1, af2), bs1)  = readLine1NavData bs
              
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
        prn  = getFieldInt  1 2 bs
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
    :: L8.ByteString
    -> ((Double, Double), L8.ByteString)
readLine8NavData bs =
    let
        ttom          = getFieldDouble  4 19 bs
        fitIntervalD  = getFieldDouble 23 19 bs
                        
    in ((ttom, fitIntervalD), dropLastLine bs)
       
    where
      dropLastLine =  snd . readEOL . snd . L8.break (`L8.elem` "\r\n") . L8.drop 42
      
----------------------------------------------------------------------

dropLine80 :: L8.ByteString -> L8.ByteString
dropLine80 =  snd . readEOL . L8.drop 80

----------------------------------------------------------------------
                      
readEOL :: L8.ByteString -> (L8.ByteString, L8.ByteString)
readEOL bs =
    case L8.uncons bs of
      Just ('\n', rest)  -> ("\n", rest)
      Just ('\r', rest1) -> case L8.uncons rest1 of
                              Just ('\n', rest2) -> ("\r\n", rest2)
                              _                  -> ("\r"  , rest1)
      _                  -> error "Cannot find end of line."

----------------------------------------------------------------------

dropGalileoNavRecord :: L8.ByteString -> L8.ByteString
dropGalileoNavRecord = dropLastLine . drop7Lines
    where
      drop7Lines  bs = iterate dropLine80 bs !! 7
      dropLastLine = snd .readEOL . L8.dropWhile (not . (`L8.elem` "\n\r")) . L8.drop 23
      
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
      dropLastLine = snd .readEOL . L8.dropWhile (not . (`L8.elem` "\n\r")) . L8.drop 42

----------------------------------------------------------------------
                     
dropBDSNavRecord :: L8.ByteString -> L8.ByteString
dropBDSNavRecord = dropLastLine . drop7Lines
    where
      drop7Lines  bs = iterate dropLine80 bs !! 7
      dropLastLine = snd .readEOL . L8.dropWhile (not . (`L8.elem` "\n\r")) . L8.drop 42
      
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
      drop6Line    = snd .readEOL . L8.dropWhile (not . (`L8.elem` "\n\r")) . L8.drop 61
      drop7Line    = snd .readEOL . L8.dropWhile (not . (`L8.elem` "\n\r")) . L8.drop 61
      dropLastLine = snd .readEOL . L8.dropWhile (not . (`L8.elem` "\n\r")) . L8.drop 23
      
----------------------------------------------------------------------

-- | Insert a navigation record into a 'NavMap'. If there is no entry
--   for the given PRN or (week, toe), the record is inserted. If an
--   entry already exists, the record is replaced only if the new
--   record has a greater IODE than the existing one.  This ensures
--   that for each (week, toe) only the navigation record with the
--   maximum IODE is kept.
insertNavRecord :: NavRecord -> NavMap -> NavMap
insertNavRecord r =
  IntMap.alter updatePrn key1
  where
    key1 = prn r
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

-- | Trim leading and trailing whitespace from a lazy ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

----------------------------------------------------------------------

-- | Drop leading whitespaces from a lazy ByteString.              
dropSpace :: L8.ByteString -> L8.ByteString
dropSpace = L8.dropWhile isSpace

----------------------------------------------------------------------

-- | Conversion of GPS time to GPS week and time-of-week
gpsTimeToWeekTow
    :: GpsTime
    -> GpsWeekTow
gpsTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = diffDays date gpsStartDate           -- number of days since GPS start date
        (w, dow)     = days `divMod` 7                      -- GPS week, GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + toInteger (h * 3600 + m * 60)
                                    )
                     + s
    in (w, tow)

----------------------------------------------------------------------

-- | Converts GPS week, time-of-week into GPS time
weekTowToGpsTime
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> GpsTime                                              -- ^ GPS time
weekTowToGpsTime (w, tow) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = w * 7                                -- number of days since GPS start date
        (towInt, towFrac) = properFraction tow
        (dow,sodInt) = towInt `divMod` 86400
        date         = addDays (days+dow) gpsStartDate
        (h,sohInt)   = sodInt `divMod` 3600
        (m,sInt)     = sohInt `divMod`   60
        s            = fromIntegral sInt + towFrac
    in LocalTime date (TimeOfDay (fromInteger h) (fromInteger m) s)

----------------------------------------------------------------------

-- | Calculates the number of seconds between two (GPS week, tow).
diffGpsWeekTow
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> Pico                                                 -- ^ time difference [s]
diffGpsWeekTow (w2,tow2) (w1,tow1) =
    fromInteger (dw * 604800) + dtow
    where
      dw   = w2   - w1
      dtow = tow2 - tow1

----------------------------------------------------------------------

-- | Ephemeris validity check based on fit interval ephemeris field for
--   a given observation time
isEphemerisValid
  :: GpsWeekTow                                             -- GPS week, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) r =
    abs diffTime <= halfFitInterval
    where
      diffTime        = diffGpsWeekTow  (w, tow) (week r, toe r)
      halfFitInterval = realToFrac ((fitInterval r) `div` 2 * 3600)

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

-- | Get Double value from ByteString field.
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

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

----------------------------------------------------------------------

-- | Prints a navigation record                          
printfRecord :: Text.Printf.PrintfType t => NavRecord -> t
printfRecord NavRecord{..} =
    printf "  Ephemeris time: %s\n\
           \PRN: %2d      toc: %s\n\
           \af0:        %19.12E\n\   
           \af1:        %19.12E\n\  
           \af2:        %19.12E\n\n\
                                    
           \iode:       %4d\n\      
           \crs:        %19.12E\n\  
           \deltaN:     %19.12E\n\  
           \m0:         %19.12E\n\n\
                                    
           \cuc:        %19.12E\n\  
           \e:          %19.12E\n\  
           \cus:        %19.12E\n\  
           \sqrtA:      %19.12E\n\n\
                                    
           \toe:        %19.12E\n\  
           \cic:        %19.12E\n\  
           \omega0:     %19.12E\n\  
           \cis:        %19.12E\n\n\
                                    
           \i0:         %19.12E\n\  
           \crc:        %19.12E\n\  
           \omega:      %19.12E\n\  
           \omegaDot:   %19.12E\n\n\
                                    
           \iDot:       %19.12E\n\  
           \skipped\n\              
           \week:       %4d\n\      
           \skipped\n\n\          
                                    
           \skipped\n\              
           \svHealth:   %4d\n\      
           \skipped\n\              
           \iodc:       %4d\n\n\    
                                    
           \ttom:       %19.12E\n\ 
           \fitInterval:  %4d\n\
           \----------------------------------\n"
     (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" (weekTowToGpsTime (week, toe)))
     prn (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" toc)
     af0            
     af1            
     af2            
     iode           
     crs            
     deltaN         
     m0             
     cuc            
     e              
     cus            
     sqrtA          
     ((realToFrac toe)::Double)
     cic            
     omega0         
     cis            
     i0             
     crc            
     omega          
     omegaDot       
     iDot           
     week           
     svHealth       
     iodc           
     ttom           
     fitInterval        

                          
