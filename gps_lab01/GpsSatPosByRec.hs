-- 2026-01-14

{- | The program for computing the position of a GPS satellite in the
     ECEF coordinate system based on sample orbital parameters
     (ephemeris), ephemeris validity interval and GPS time provided as
     input. The program is not intended for post‑processing
     applications.
     
     The algorithm follows the IS-GPS-200N specification, which
     provides ready-made formulas for satellite position. These
     formulas use only the time-of-week without the week number.  To
     make it possible they apply the wrapWeekCrossover function, which
     operates within a limited time interval of 302400 s.  Wrap week
     crossover is a simple mechanism for real-time receivers that
     always have fresh ephemeris (valid ephemeris).

     In summary, the program does not compute time differences as real
     time difference using the formula dw*604800+dtow, where dw is the
     week difference and dtow is time-of-week difference.  Instead, it
     applies the wrapWeekCrossover function, which has a limited range
     of applicability.

     The Pico is used to represent time-of-week values, ensuring high
     precision in time calculations. Satellite position calculations,
     however, require Double type.  As a consequence, the code
     includes conversion functions between `Pico` and `Double`, which
     may slightly reduce readability but preserve accuracy.
     
     Input:
       - GPS Ephemeris                  ephExample          defined in the code
       - ephemeris validity interval    curveFitInteval     defined in the code
         (in RINEX it is
         called fit interval)
       - GPS Time                       t                   defined in the code

     Output:
       - ECEF satellite position             (x, y, z)

     Print of run:
     Entered GPS time            : 2024 03 07 22 00 30
     Ephemeris reference GPS time: 2024 03 07 22 00 00
     Diference                   :            00 00 30

     ECEF satellite position [m]:
     X = 22151566.575334515
     Y = 13275548.286060918
     Z =  7260529.645377433
-}

----------------------------------------------------------------------

import Data.Time.Calendar  (fromGregorian, diffDays, addDays)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay(..))
import Text.Printf         (printf)
import Data.Fixed          (Pico)
import Data.Time           (diffLocalTime)
import Data.Time.Format

----------------------------------------------------------------------

-- | GPS ephemeris
data Ephemeris = Ephemeris
  { crs      :: Double                                      -- ^ orbital radius correction [m]
  , deltaN   :: Double                                      -- ^ mean motion difference [rad/s]
  , m0       :: Double                                      -- ^ mean anomaly at toe epoch [rad]
  , cuc      :: Double                                      -- ^ latitude argument correction [rad]
  , e        :: Double                                      -- ^ eccentricity []
  , cus      :: Double                                      -- ^ latitude argument correction [rad]
  , sqrtA    :: Double                                      -- ^ square root of semi-major axis [m^0.5]
  , toe      :: Pico                                        -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic      :: Double                                      -- ^ inclination correction [rad]
  , omega0   :: Double                                      -- ^ longitude of ascending node at toe epoch [rad]
  , cis      :: Double                                      -- ^ inclination correction [rad]
  , i0       :: Double                                      -- ^ inclination at reference epoch [rad]
  , crc      :: Double                                      -- ^ orbital radius correction [m]
  , omega    :: Double                                      -- ^ argument of perigee [rad]
  , omegaDot :: Double                                      -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double                                      -- ^ rate of inclination angle [rad/s]
  , week     :: Integer                                     -- ^ number of GPS week for toe
  } deriving (Show)

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)

----------------------------------------------------------------------

-- | Constants
mu, omegaEDot :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]

----------------------------------------------------------------------

-- | Computation of the GPS satellite position in ECEF from the GPS
--   ephemeris and for a GPS time-of-week based on IS-GPS-200N
--   20.3.3.4.3 ready-made mathematical formulas.
satPosECEF
    :: Pico                                                  -- ^ GPS time-of-week [s]
    -> Ephemeris                                             -- ^ ephemeris
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
satPosECEF tow eph =
    let
        a      = sqrtA eph * sqrtA eph                           -- semi-major axis [m]
        n0     = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]       
        n      = n0 + deltaN eph                                 -- corrected mean motion [rad/s]        
        tk     = realToFrac $ wrapWeekCrossover (tow - toe eph)  -- time elapsed since toe [s]           
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

-- | Iterative solution of Kepler's equation ek = m + e sin ek
--   (Newton-Raphson method)
keplerSolve    
    :: Double                                               -- ^ mean anomaly [rad]
    -> Double                                               -- ^ eccentricity []
    -> Double                                               -- ^ eccentric anomaly [rad]
keplerSolve m e = loop e0 0               
  where
    e0 = m + e * sin m
    loop :: Double -> Int -> Double
    loop eN k
      | k > 20 = error "Kepler method iteration count exceeded"
      | abs (eN' - eN) < 1e-12 = eN'
      | otherwise = loop eN' (k+1)
          where
            eN'   = eN - f/fDot                             -- iterative formula
            f     = eN - e * sin eN - m  
            fDot  =  1 - e * cos eN                         -- derivative of the function f


----------------------------------------------------------------------

-- | Calculates the correct number of seconds between two GPS tows
--   without week numbers.  Formula based on IS-GPS-200N
--   20.3.3.4.3. page 106.  Takes into account cases where tows are in
--   adjacent weeks.
--   NOTE: It works for assuming that the absolute time difference is
--   less than 302400 s. In practice it is ensured by ephemeris
--   validity check.
wrapWeekCrossover
    :: Pico                                                 -- ^ difference between two tow        [s]
    -> Pico                                                 -- ^ number of seconds between two tow [s]
wrapWeekCrossover dt
  -- 604800.0 is number of seconds of full week
  -- 302400.0 is number of seconds of half week
  | dt >  302400.0 = dt - 604800.0                          -- tow in adjacent week and earlier then toe
  | dt < -302400.0 = dt + 604800.0                          -- tow in adjacent week and later then toe
  | otherwise      = dt                                     -- tow and toe in the same week

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

-- | Ephemeris validity check for given (w, tow).  Checks if (w, tow)
--   is in half of interval since (week, toe).
isEphemerisValid
  :: GpsWeekTow                                   -- ^ GPS week, time-of-week
  -> Int                                          -- ^ fit interval [h]
  -> Ephemeris                                    -- ^ ephemeris GPS week, time-of-week of ephemeris
  -> Bool
isEphemerisValid (w, tow) fitInterval eph =
    abs diffTime <= halfFitInterval
    where
      diffTime = diffGpsWeekTow  (w, tow) (week eph, toe eph)
      halfFitInterval = fromIntegral ((fitInterval `div` 2) * 3600)

----------------------------------------------------------------------

-- | Determining the GPS satellite position by receiver in ECEF:
--   - checks possible curve fit interval values according to 20.3.4.4
--     IS-GPS-200N.     
--   - checks ephemeris validity to given (w, tow) and curveFitIterval
--   - computation of the GPS satellite position in ECEF from the GPS
--     ephemeris and for a GPS time-of-week based on IS-GPS-200N
--     20.3.3.4.3 ready-made mathematical formulas.
--   Curve fit interval is simply an interval.
satPosByRec       
    :: GpsTime
    -> Ephemeris                                             -- ^ ephemeris
    -> Int                                                   -- ^ curve fit interval [h]
                                                             --   (in RINEX it is called fit interval)
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
satPosByRec t eph curveFitInterval
    |    curveFitInterval ==  4
      || curveFitInterval ==  6
      || curveFitInterval ==  8
      || curveFitInterval == 14
      || curveFitInterval == 26 =
        let (w, tow) = gpsTimeToWeekTow t
        in if isEphemerisValid (w, tow) curveFitInterval eph
           then satPosECEF tow eph
           else error $ "Ephemeris is not valid for "
                    ++ formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" t
    | otherwise  =
        error "Curve fit interval must be one of [4,6,8,14,26]"

----------------------------------------------------------------------

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

----------------------------------------------------------------------

-- | Ephemeris example
ephExample :: Ephemeris
ephExample = Ephemeris
          { crs      =  134.75
          , deltaN   =  4.12374319903e-9
          , m0       = -2.23037324521
          , cuc      =  7.25500285625e-6
          , e        =  1.61300709005e-2
          , cus      =  8.00378620625e-6
          , sqrtA    =  5153.71900558
          , toe      =  424800.0
          , cic      =  7.07805156708e-8
          , omega0   = -2.84371723002
          , cis      =  2.6635825634e-7
          , i0       =  0.967821256634
          , crc      =  226.75
          , omega    = -1.2251476939
          , omegaDot = -7.73246494535e-9
          , iDot     =  2.60367988229e-10
          , week     =  2304
          }

----------------------------------------------------------------------

-- Calculates GPS satelite position for example GPS ephemeris and GPS
-- time. An ephemeris is a record of initial orbital parameters. The
-- function assumes the most common ephemeris validity interval (curve
-- fit interval) of 4h.
main :: IO ()
main = do
  let 
      eph              = ephExample                              -- Input: GPS Ephemeris
      curveFitInterval = 4                                       -- Input: ephemeris validity interval
                                                                 --        (in RINEX 3.04 it is called fit interval)
      t                = mkGpsTime 2024 03 07 22 00 30.0         -- Input: GPS Time
      (x, y, z)        = satPosByRec t eph curveFitInterval      -- Output: ECEF satellite position
                  
  printf "Entered GPS time            : %s\n"
             (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" t)
  -- ephemeris GPS reference time         
  let teph = weekTowToGpsTime (week eph, toe eph)
  printf "Ephemeris reference GPS time: %s\n"
             (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" teph)
  printf "Diference                   :%20s\n\n"
             (formatTime defaultTimeLocale "%2H %2M %2S%Q" (diffLocalTime t teph))
               
  printf "ECEF satellite position [m]:\n"
  printf "X = %18.9f\n" x
  printf "Y = %18.9f\n" y
  printf "Z = %18.9f\n" z
