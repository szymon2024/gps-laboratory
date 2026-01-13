-- 2026-01-13

{- | Estimate ECEF satellite position for dual-frequency pseudorange
     observation (measurement) from single navigation record.  The
     navigation record is a RINEX 3.04 navigation file record
     containing the broadcast, initial orbital parameters
     (ephemeris). The position is computed at GPS transmission
     time. The computation is of low precision because the code
     pseudorange is of low precision and the orbital parameters
     (ephemeris) are approximate.
   
     NOTE 1:
       Three different clocks must be considered:
         - GPS clock
         - satellite clock
         - receiver clock
       All of them count time in GPS time system.

       The signal transmission time tt is by the GPS clock. The
       satellite time of signal transmission is by satellite clock.
       The receiver time of signal reception is by receiver clock.

       The term "epoch" refers to a moment in time. The receiver time
       of signal reception by receiver clock is also called the
       observation time, observation epoch, receiver time tag,
       receiver timestamp, measurement time.

     NOTE 2:
       Why is transmission time calculated? The transmission time is
       calculated to calculate the satellite position for the
       pseudorange.  It's irrelevant that transmission times vary for
       different satellites for the selected observation time.  What
       matters is that the satellite's position corresponds to the
       satellite-receiver distance.
              
     Input:
       - observation time
         (receiver time of signal reception)        tobs           (hand copied from RINEX observation file)
       - pseudorange for f1 [m]                     ps1            (hand copied from RINEX observation file)
       - pseudorange for f2 [m]                     ps2            (hand copied from RINEX observation file)
       - navigation data record in RINEX 3.04
         format                                     nav_record.txt (hand copied from a RINEX navigation file)

     Output:
       - signal transmission time by GPS clock      tt
       - satellite position in ECEF [m]
         at transmission time                       (x, y, z)

     Print of run:
     Observation time
     (receiver clock time of signal reception) : 2024 03 07 00 53 01
     Signal transmission time by GPS clock     : 2024 03 07 00 53 00.927812714088

     ECEF satellite position [m]:
     X =  4460302.794944842
     Y = 17049812.692289740
     Z = 19845264.366251267
-}

{-# LANGUAGE RecordWildCards #-}               -- to use Ephemeris {..}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Time.Calendar            (fromGregorian, diffDays, addDays)
import           Data.Time.LocalTime           (LocalTime (..), TimeOfDay(..))
import           Data.Time.Format
import           Data.Fixed                    (Pico)    
import           Text.Printf                   (printf)
import qualified Data.ByteString.Char8  as L8
import           Data.Char                     (isSpace)
    
import qualified Data.ByteString.Unsafe as BSU (unsafeUseAsCString)
import           Foreign                       (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types               (CChar, CDouble(CDouble))
import           Foreign.C.String              (CString)
import           System.IO.Unsafe              (unsafePerformIO)


-- | GPS navigation record (a subset of fields from RINEX 3.04 navigation file)
data NavRecord = NavRecord
  { prn          :: Int               -- ^ satellite number
  , toc          :: GpsTime           -- ^ clock data reference time
  , af0          :: Double            -- ^ satellite clock bias correction coefficient [s]
  , af1          :: Double            -- ^ satellite clock drift correction coefficient [s/s]
  , af2          :: Double            -- ^ satellite clock drift rate correction coefficient [s/s^2]
  , crs          :: Double            -- ^ orbital radius correction [m]
  , deltaN       :: Double            -- ^ mean motion difference [rad/s]
  , m0           :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc          :: Double            -- ^ cosine correction to argument of latitude [rad]
  , e            :: Double            -- ^ eccentricity []
  , cus          :: Double            -- ^ sine correction to argument of latitude [rad]
  , sqrtA        :: Double            -- ^ square root of semi-major axis [m^0.5]
  , toe          :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic          :: Double            -- ^ cosine correction to inclination [rad]
  , omega0       :: Double            -- ^ longitude of ascending node at toe [rad]
  , cis          :: Double            -- ^ sine correction to inclination [rad]
  , i0           :: Double            -- ^ inclination angle at reference epoch [rad]
  , crc          :: Double            -- ^ cosine correction to orbital radius [m]
  , omega        :: Double            -- ^ argument of perigee [rad]
  , omegaDot     :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot         :: Double            -- ^ rate of inclination angle [rad/s]
  , week         :: Integer           -- ^ GPS week to go with toe
  , fitInterval  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Show)

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)

----------------------------------------------------------------------

-- | Constants
mu, omegaEDot, c, fRel, f1, f2 :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]
c         = 299792458.0           -- speed of light [m/s]
fRel      = -4.442807633e-10      -- constant F in the relativistic correction [s/sqrt m]
f1        = 1575.42e6             -- L1 frequency [Hz]
f2        = 1227.60e6             -- L2 frequency [Hz]

----------------------------------------------------------------------

-- | Determining the GPS satellite position in ECEF from the GPS
--   ephemeris and for a (GPS week, tow).
satPosECEF
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ navigation record with ephemeris
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
satPosECEF (w, tow) eph =
  let
    a      = sqrtA eph * sqrtA eph                           -- semi-major axis [m]
    n0     = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]       
    n      = n0 + deltaN eph                                 -- corrected mean motion [rad/s]        
    tk     = realToFrac $
             diffGpsWeekTow (w, tow) (week eph, toe eph)     -- time elapsed since toe [s]           
    mk     = m0 eph + n*tk                                   -- mean anomaly at tk [rad]             
    ek     = keplerSolve mk (e eph)                          -- eccentric anomaly [rad]              
    vk     = atan2 (sqrt (1 - e eph *e eph ) * sin ek)
                   (cos ek - e eph)                          -- true anomaly                         
    phik   = vk + omega eph                                  -- argument of latitude                 
    duk    = cus eph * sin (2*phik)
           + cuc eph * cos (2*phik)                          -- argument of latitude correction      
    drk    = crs eph * sin (2*phik)
           + crc eph * cos (2*phik)                          -- radius correction                    
    dik    = cis eph * sin (2*phik)
           + cic eph * cos (2*phik)                          -- inclination correction               
    uk     = phik + duk                                      -- corrected argument of latitude       
    rk     = a * (1 - e eph * cos ek) + drk                  -- corrected radius                     
    ik     = i0 eph + dik + iDot eph * tk                    -- corrected inclination                
    xk'    = rk * cos uk                                     -- xk' in the orbital plane             
    yk'    = rk * sin uk                                     -- yk' in the orbital plane             
    omegak = omega0 eph                                                                                 
           + (omegaDot eph - omegaEDot)*tk
           - omegaEDot * realToFrac (toe eph)                -- corrected longitude of ascending node
    xk     = xk' * cos omegak - yk' * cos ik * sin omegak    -- transformation to ECEF               
    yk     = xk' * sin omegak + yk' * cos ik * cos omegak    -- transformation to ECEF               
    zk     =                    yk' * sin ik                 -- transformation to ECEF
  in (xk,yk,zk)

----------------------------------------------------------------------

-- | Iterative solution of Kepler's equation ek = m + e sin ek (Newtona-Raphsona method)
keplerSolve    
    :: Double                                                   -- ^ mean anomaly
    -> Double                                                   -- ^ eccentricity
    -> Double                                                   -- ^ eccentric anomaly [rad]
keplerSolve m e = loop e0 0               
  where
    e0 = m + e * sin m
    loop :: Double -> Int -> Double
    loop eN k
      | k > 20 = error "Kepler method iteration count exceeded"
      | abs (eN' - eN) < 1e-12 = eN'
      | otherwise = loop eN' (k+1)
          where    
            f    = eN - e * sin eN - m  
            fDot =  1 - e * cos eN                           -- derivative of the function f
            eN'  = eN - f/fDot                               -- iterative formula

----------------------------------------------------------------------

-- | Pseudorange for dual-frequency receiver (pseudorange corrected for ionospheric effects)
--   based on IS-GPS-200N 20.3.3.3.3.3 ready-made formulas.
dfPseudorange
    :: Double                                                -- ^ pseudorange for f1    [m]
    -> Double                                                -- ^ pseudorange for f2    [m]
    -> Double                                                -- ^ pseudorange corrected [m]
dfPseudorange ps1 ps2 = (ps2 - g*ps1)/(1 - g)
    where
      g = (f1/f2)^(2::Int)

----------------------------------------------------------------------

-- | Iterative calculation eccentric anomaly
eAnom
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ navigation recored with ephemeris
    -> Double                                                -- ^ eccentric anomaly [rad]
eAnom (w, tow) NavRecord{..} =                              
    let a  = (sqrtA)*(sqrtA)                                 -- semi-major axis
        n0 = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]
        n  = n0 + deltaN                                     -- corrected mean motion [rad/s]
        tk = realToFrac $
             diffGpsWeekTow (w, tow) (week, toe)             -- time elapsed since toe [s]
        mk = m0 + n*tk                                       -- mean anomaly for tk
    in keplerSolve mk e                                      -- eccentric anomaly [rad]

----------------------------------------------------------------------

-- | Compute relativistic correction for satellite clock
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
relCorr
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> NavRecord                                            -- ^ ephemeris
    -> Pico                                                 -- ^ dtr - relativistic correction [s]
relCorr (w, tow) NavRecord{..} = realToFrac (fRel * e * sqrtA * sin ek)
    where
      ek = eAnom (w, tow) NavRecord{..}                     -- eccentric anomaly [rad]

----------------------------------------------------------------------

-- | Compute satellite clock correction
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
clkCorr
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> NavRecord                                            -- ^ ephemeris                                 
    -> Pico                                                 -- ^ dtsv - satellite clock correction [s]
clkCorr (w, tow) NavRecord{..} = realToFrac (af0 + af1*dt + af2*dt^(2::Int))
    where
      dt  = realToFrac $ diffGpsWeekTow (w, tow) (week, tocTow)
      (_, tocTow) = gpsTimeToWeekTow toc

----------------------------------------------------------------------

-- | Iteratively compute signal transmission time because the transmission
--   time depends on the clock corrections and the clock corrections
--   depend on the transmission time.
transmissionTime            
  :: Double                                                 -- ^ pseudorange ps1 [m]
  -> Double                                                 -- ^ pseudorange ps2 [m]
  -> GpsWeekTow                                             -- ^ time of observation
                                                            --   (receiver time of signal reception)
  -> NavRecord                                              -- ^ navigation recored with ephemeris
  -> GpsWeekTow                                             -- ^ signal transmission time
transmissionTime ps1 ps2 wtob eph =  loop tt0 0
    where
      ps   = dfPseudorange ps1 ps2
      tsv  = diffSeconds wtob  (realToFrac (ps/c))           -- satelite time of signal transmission
      tt0 = tsv
      loop :: GpsWeekTow -> Int -> GpsWeekTow
      loop tt k
          | k >= 10                  = error "Number of time transmission iterations exceeded"
          | abs (diffGpsWeekTow tt' tt) < 1e-12 = tt'
          | otherwise                           = loop tt' (k+1)
          where
            dtb  = clkCorr tt eph                           -- clock correction
            dtr  = relCorr tt eph                           -- relativistic correction
            dtsv = dtb  + dtr
            tt' = diffSeconds tt0 dtsv

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

-- | Substract seconds from (GPS week,tow).
diffSeconds
    :: GpsWeekTow                                 -- ^ GPS week, time-of-week
    -> Pico                                       -- ^ period of time [s]
    -> GpsWeekTow                                 -- ^ GPS week, time-of-week
diffSeconds (week, tow) secs =
    let ds = tow - secs
        k = floor (ds / 604800)
        tow' = ds - fromIntegral k * 604800
    in (week + k, tow')

----------------------------------------------------------------------

-- | Function import: double strtod(const char *nptr, char **endptr)
foreign import ccall unsafe "stdlib.h strtod"
    c_strtod :: CString -> Ptr (Ptr CChar) -> IO CDouble

-- | 2025-11-01 Data.ByteString.Char8 does not have a readDouble function.
--   Reads Double value from Char8 ByteString.
readDouble :: L8.ByteString -> Maybe (Double, L8.ByteString)
readDouble bs = unsafePerformIO $
    BSU.unsafeUseAsCString bs $ \cstr -> 
      alloca $ \endPtr -> do
        val <- c_strtod cstr endPtr
        end <- peek endPtr
        if end == cstr
          then return Nothing
          else do
            let offset = end `minusPtr` cstr
            let rest   = L8.drop offset bs
            return (Just (realToFrac val, rest))

----------------------------------------------------------------------

-- | Get Double value from ByteString field.
--   Its purpose is to stop reading if it cannot read the entire field.
getDouble :: L8.ByteString -> Maybe Double
getDouble bs = do
  (val, rest) <- readDouble bs
  case L8.uncons rest of
    Nothing                 -> Just val
    Just (b, _) | isSpace b -> Just val
    _                       -> error $ "Cannot read \"" ++ L8.unpack rest
                               ++ "\" from \"" ++ L8.unpack bs ++ "\""

----------------------------------------------------------------------
                            
takeField :: Int -> Int -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

----------------------------------------------------------------------

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

----------------------------------------------------------------------

-- | Read GPS satellite navigation record
readRecord :: L8.ByteString -> (NavRecord, L8.ByteString)
readRecord bs =
    let ((prn, toc, af0, af1, af2), bs1) =
            case readLine1Data bs of
              Just d  -> d
              Nothing -> error "Cannot read first line of record."
              
        ((crs, deltaN, m0), bs2)         =
            case readLine2Data bs1 of
              Just d  -> d 
              Nothing -> error "Cannot read second line of record."
              
        ((cuc, e, cus, sqrtA), bs3)      =
            case readLine3Data bs2 of
              Just d  -> d
              Nothing -> error "Cannot read third line of record."
              
        ((toeD, cic, omega0, cis), bs4)  =
            case readLine4Data bs3 of
              Just d  -> d
              Nothing -> error "Cannot read fourth line of record."
              
        ((i0, crc, omega, omegaDot), bs5) =
            case readLine5Data bs4 of
              Just d  -> d
              Nothing -> error "Cannot read fifth line of record."
              
        ((iDot, weekD), bs6)              =
            case readLine6Data bs5 of
              Just d  -> d
              Nothing -> error "Cannot read sixth line of record."

        bs7 = dropLine bs6                        -- skip seventh line        
              
        ((fitIntervalD), bs8)             =
            case readLine8Data bs7 of
              Just d  -> d
              Nothing -> error "Cannot read eighth line of record."

        toe          = realToFrac toeD
        week         = round      weekD           -- conversion is needed for equality comparisons
        fitInterval  = round      fitIntervalD
              
    in (NavRecord {..}, bs8)
       
----------------------------------------------------------------------

dropLine :: L8.ByteString -> L8.ByteString
dropLine     =  snd . readEOL . L8.drop 80

----------------------------------------------------------------------
                      
dropLastLine :: L8.ByteString -> L8.ByteString
dropLastLine =  snd . readEOL . snd . L8.break (`L8.elem` "\r\n") . L8.drop 42

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
                            
readLine1Data :: L8.ByteString  -> Maybe ((Int, GpsTime, Double, Double, Double), L8.ByteString)
readLine1Data bs = do
  (prn, _)  <- L8.readInt $ L8.dropSpace $ takeField  1 2 bs       -- dropSpace is needed by readInt
  (y  , _)  <- L8.readInt $ L8.dropSpace $ takeField  4 4 bs
  (mon, _)  <- L8.readInt $ L8.dropSpace $ takeField  9 2 bs
  (d  , _)  <- L8.readInt $ L8.dropSpace $ takeField 12 2 bs
  (h  , _)  <- L8.readInt $ L8.dropSpace $ takeField 15 2 bs
  (m  , _)  <- L8.readInt $ L8.dropSpace $ takeField 18 2 bs
  (s  , _)  <- L8.readInt $ L8.dropSpace $ takeField 21 2 bs
  
  let toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)
            
  af0       <- getDouble $ L8.dropSpace $ takeField 23 19 bs
  af1       <- getDouble $ L8.dropSpace $ takeField 42 19 bs
  af2       <- getDouble $ L8.dropSpace $ takeField 61 19 bs
  return ((prn, toc, af0, af1, af2), dropLine bs)

----------------------------------------------------------------------
         
readLine2Data :: L8.ByteString  -> Maybe ((Double, Double, Double), L8.ByteString)
readLine2Data bs = do
  crs       <- getDouble $ L8.dropSpace $ takeField 23 19 bs
  deltaN    <- getDouble $ L8.dropSpace $ takeField 42 19 bs
  m0        <- getDouble $ L8.dropSpace $ takeField 61 19 bs
  return ((crs, deltaN, m0), dropLine bs)

----------------------------------------------------------------------
         
readLine3Data :: L8.ByteString  -> Maybe ((Double, Double, Double, Double), L8.ByteString)
readLine3Data bs = do
  cuc       <- getDouble $ L8.dropSpace $ takeField  4 19 bs
  e         <- getDouble $ L8.dropSpace $ takeField 23 19 bs
  cus       <- getDouble $ L8.dropSpace $ takeField 42 19 bs
  sqrtA     <- getDouble $ L8.dropSpace $ takeField 61 19 bs
  return ((cuc, e, cus, sqrtA), dropLine bs)

----------------------------------------------------------------------
         
readLine4Data :: L8.ByteString  -> Maybe ((Double, Double, Double, Double), L8.ByteString)
readLine4Data bs = do
  toe       <- getDouble $ L8.dropSpace $ takeField  4 19 bs
  cic       <- getDouble $ L8.dropSpace $ takeField 23 19 bs
  omega0    <- getDouble $ L8.dropSpace $ takeField 42 19 bs
  cis       <- getDouble $ L8.dropSpace $ takeField 61 19 bs
  return ((toe, cic, omega0, cis), dropLine bs)

----------------------------------------------------------------------
         
readLine5Data :: L8.ByteString  -> Maybe ((Double, Double, Double, Double), L8.ByteString)
readLine5Data bs = do
  i0        <- getDouble $ L8.dropSpace $ takeField  4 19 bs
  crc       <- getDouble $ L8.dropSpace $ takeField 23 19 bs
  omega     <- getDouble $ L8.dropSpace $ takeField 42 19 bs
  omegaDot  <- getDouble $ L8.dropSpace $ takeField 61 19 bs
  return ((i0, crc, omega, omegaDot), dropLine bs)

----------------------------------------------------------------------
         
readLine6Data :: L8.ByteString  -> Maybe ((Double, Double), L8.ByteString)
readLine6Data bs = do
  iDot      <- getDouble $ L8.dropSpace $ takeField  4 19 bs
  weekD     <- getDouble $ L8.dropSpace $ takeField 42 19 bs
  return ((iDot, weekD), dropLine bs)

----------------------------------------------------------------------
         
readLine8Data :: L8.ByteString  -> Maybe ((Double), L8.ByteString)
readLine8Data bs = do
  fitIntervalD  <- getDouble $ L8.dropSpace $ takeField 23 19 bs
  return ((fitIntervalD), dropLastLine bs)

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

-- | Converts GPS week and time-of-week (tow) into GPS time
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

-- | Ephemeris validity check based on fit interval ephemeris field for
--   a given observation time
isEphemerisValid
  :: GpsWeekTow                                             -- ^ GPS week, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) r =
    abs diffTime <= halfFitInterval
    where
      diffTime        = diffGpsWeekTow  (w, tow) (week r, toe r)
      halfFitInterval = fromIntegral ((fitInterval r) `div` 2 * 3600)

----------------------------------------------------------------------
                        
satPosForDfPs
  :: GpsTime                                                -- ^ time of observation
                                                            --   (receiver time of signal reception)
  -> Double                                                 -- ^ pseudorange for f1
  -> Double                                                 -- ^ pseudorange for f2
  -> NavRecord                                              -- ^ GPS navigation record
  -> (GpsWeekTow, (Double, Double, Double))                 -- ^ transmission time, satelite ECEF posistion at transmission time
satPosForDfPs tobs ps1 ps2 r =
    let wtobs = gpsTimeToWeekTow tobs
    in if isEphemerisValid wtobs r
       then let tt      = transmissionTime ps1 ps2 wtobs r  -- Output: signal transmission time by GPS clock [s]
                (x,y,z) = satPosECEF tt r                   -- Output: satelite ECEF position [m]
                in (tt, (x, y, z))
       else error $ "Ephemeris is not valid for " ++
            formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tobs

----------------------------------------------------------------------
                       
main :: IO ()
main = do
  let tobs = mkGpsTime 2024 03 07 00 53 01.0000000          -- Input: time of observation
                                                            --        (receiver time of signal reception)
      ps1  = 21548635.724                                   -- Input: pseudorange for f1 e.g. C1C
      ps2  = 21548628.027                                   -- Input: pseudorange for f2 e.g. C2X
      fn   = "nav_record.txt"                               -- Input: file name
  bs <- L8.readFile fn                                      -- bytestring from "nav_record.txt"
  if L8.take 1 bs == "G"
  then do
      let (r, _) = readRecord bs
          (wtt, (x,y,z)) = satPosForDfPs tobs ps1 ps2 r     -- Output: signal transmission time by GPS clock,
                                                            --         satelite ECEF position [m] at transmission time
          tt = weekTowToGpsTime wtt
               
      printf "Observation time\n"
      printf "(receiver clock time of signal reception) : %s\n"
              (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tobs)
      printf "Signal transmission time by GPS clock     : %s\n\n"
              (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tt)
      printf "ECEF satellite position [m]:\n"
      printf "X = %18.9f\n" x
      printf "Y = %18.9f\n" y
      printf "Z = %18.9f\n" z
   else
      printf "Cannot find a GPS navigation record in %s\n" fn   

