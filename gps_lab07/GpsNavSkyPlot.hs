-- 2026-04-01

{- | The program creates sky plot of computed GPS satellites
   trajectories from RINEX 3.04 navigation file. A sky plot is polar
   representation of satellite azimuth (0-360°) and elevation (0-90°)
   from the observer position. Every satellite trajectory is drawn as
   sequence of intervals, with a satellite PRN marker and a direction
   arrow. The color of each orbit
   corresponds to the fitInterval field from the RINEX file.

   Due to the large number of satellites or a long time span in the
   RINEX navigation file, the resulting sky plot may become
   cluttered. In such cases, the prnFilter can be adjusted to plot
   only the trajectories of selected satellites.

   The intervals are not ephemeris validity intervals but are
   determined based on ephemeris validity intervals. Before computing
   sky‑plot points, the program detects overlapping ephemeris validity
   intervals and trims them to ensure consistent, non‑overlapping time
   ranges, taking into account GPS week and TOE boundaries.

   Main steps of the algorithm:                                          
                                                                        
   1. Read navigation records from RINEX 3.04 navigation file into a   
   map (navMapFromRinex function).                                       
                                                                        
   2. Compute (azimuth, elevation) points with a specific time step
   (in seconds) for each ephemeris (skyPoints function). The points
   are grouped based on the ephemeris validity interval, and
   overlapping intervals are trimmed.
                                                                        
   3. Generate an SVG file containing the sky plot by transforming       
   the computed points into the drawing area.                                
                                                                        
   Steps to compute (azimuth, elevation) from the observer to a          
   satellite (skyIntervalPoints function):                               
                                                                         
   1. Compute the satellite position in ECEF coordinates (satPosECEF     
   function).                                                            
                                                                        
   2. Transform the observer's position WGS84 coordinates into ECEF
   coordinates. Compute the vector from the observer to the satellite
   in ECEF, then rotate it into local ENU frame. (ecefVectorToENU
   function). The rotation requires conversion observer ECEF
   coordinates to WGS84 coordinates.
                                                                        
   3. Discard the point if the satellite is below the horizon u<=0.      
                                                                        
   4. Convert the ENU vector to azimuth and elevation angles (enuToAzEl  
   function).

   NOTE 1: Increasing the step size will cause trajectories ending close
   to elevation 0° to deviate further from the edge of grid 0° because
   the program does not compute a point for elevation 0.

   NOTE 2: The Sagnac correction is not applied because its result is
   too small to be visible on the graph.

   This project was developed with assistance from Microsoft Copilot.

   Input (to modify directly in the source code):
     - ECEF coordinates of observer position                obsPosECEF
     - rinex navigation file name                           fn
     - filter by satellite prn                              prnFilter
     - plot title                                           title

   Output:
     - sky plot SVG file                                    navSkyplot.svg
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict    as MS
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict as IMS
import           Data.Char                         (isSpace)
import           Data.Int                          (Int64)
import           Control.Monad                     (guard)
import           Data.Time.Calendar                (fromGregorian)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..))
import           Data.Fixed                        (Pico, mod')
import           Text.Printf                       (printf)
import           Data.List                         (unsnoc)

-- to process data faster than with Strings    
import qualified Data.ByteString.Lazy.Char8 as L8

-- to readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)

-- to create svg drawing faster than with Strings
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB 

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
type NavMap     = IntMap (Map EphWeekTow NavRecord)    -- ^ key1:  prn (satellite identifie)
                                                       --   key2:  (week, toe)
                                                       --   value2: navigation record for a healthy satellite
                                                       --           and with max iode for (week, toe)
    
type ECEF     = (Double, Double, Double)   -- ^ Geocentric coorditantes
type WGS84    = (Double, Double, Double)   -- ^ Geodetic coordinates for WGS84 ellipsoid
type ENU      = (Double, Double, Double)   -- ^ Local Tangent Plane coordinates
type AzEl     = (Double, Double)           -- ^ Azimuth, elevation in degrees
type Interval = (GpsWeekTow, GpsWeekTow)   -- ^ Time interval between two (GPS week, tow)

main :: IO ()
main = do
  let
      obsPosECEF = (3730927.8332,1134193.6047,5030402.1250) -- Input: ECEF coordinates of observer position
      fn = "rinex.nav"                                      -- Input: rinex navigation file name
  bs <- L8.readFile fn
  let
      navMap    = navMapFromRinex bs
      prnfilter = \prn _ -> prn >=1 && prn <=32             -- Input: filter by satellite prn
      step      = 90::Pico                                  -- [s] 
      skyMap    = skyPoints obsPosECEF step
                    (IMS.filterWithKey prnfilter navMap)
      title  = "Sky Plot of GPS Satellite \
                \Trajectories from the " <> T.pack fn       -- Input: title of plot
  TIO.writeFile "navSkyplot.svg"
         (svgCreateContent title skyMap)                    -- Output: navSkyplot.svg file         

-- | The viewport (drawing) in svg file will be 600px by 600px. The
-- upper-left corner is (0,0).
svgWidth, svgHeight :: Int
svgWidth = 600
svgHeight = 600

-- | Title height in pixels
svgTitleHeight :: Int
svgTitleHeight = 30

-- | Compute viewport center for svg file
svgCx, svgCy :: Double
svgCx = fromIntegral (svgWidth  `div` 2)
svgCy = fromIntegral (svgHeight `div` 2 + svgTitleHeight + 10)

-- | Compute maximum radius of sky plot for svg file
svgRMax :: Double
svgRMax = svgCx - fromIntegral (svgTitleHeight + 40)

-- | Convert degrees to radians
deg2rad :: Double -> Double
deg2rad d = d * pi / 180

-- | Compute x, y drawing coordinates for azimuth, elevation for svg
-- file
svgToXY :: AzEl -> (Double, Double)
svgToXY (az,el) =
  let azRad = deg2rad (90 - az)
      r     = (90 - el)/90 * svgRMax
      x     = svgCx + r * cos azRad
      y     = svgCy - r * sin azRad
  in (x,y)

-- | Choose color for fitInterval in svg file
svgColorByFitInterval :: Int -> T.Text
svgColorByFitInterval fitInterval =
    case fitInterval of
      4 -> "green"
      6 -> "yellow"
      8 -> "orange"
      _ -> "red"

-- | Format point list for svg file
svgPointsBuilder :: [(Double,Double)] -> TB.Builder
svgPointsBuilder pts =
  mconcat
    [ TB.fromString (printf "%.2f,%.2f " x y)
    | (x,y) <- pts
    ]

-- | Define a polyline with a satellite marker and an arrow marker
-- from interval points for svg file. The satellite marker space is
-- created by splitting the list of points into two lists and pacing
-- the marker to the end of the first list.
svgPolylineWithLabel :: T.Text -> Int -> [AzEl] -> TB.Builder
svgPolylineWithLabel color prn azels=
  let pts = [ svgToXY azel | azel <- azels ]
      n   = length azels
      (pts1, rest) = splitAt (n `div` 2) pts
      pts2 = case unsnoc pts1 of
               Nothing    -> rest
               Just (_,c) -> c:rest
  in
     -- First half with satellite marker
     "<polyline points=\""
  <> svgPointsBuilder pts1
  <> "\" stroke=\"" <> TB.fromText color
  <> "\" fill=\"none\" marker-end=\"url(#sat-"
  <> TB.decimal prn <> ")\"/>\n"

     -- Second half with arrow marker
  <> "<polyline points=\""
  <> svgPointsBuilder pts2
  <> "\" stroke=\"" <> TB.fromText color
  <> "\" fill=\"none\" marker-end=\"url(#arrow-"
  <> TB.fromText color <> ")\"/>\n"

-- | Satellite marker definition for svg file    
svgSatelliteMarker :: [Int] -> TB.Builder
svgSatelliteMarker prns =
  "<defs>\n"
  <> mconcat (map marker prns)
  <> "</defs>\n"
  where
    marker prn =
         "<marker id=\"sat-" <> TB.decimal prn
      <> "\" markerWidth=\"30\" markerHeight=\"30\" refX=\"23\" refY=\"15\" "
      <> "orient=\"auto\" markerUnits=\"userSpaceOnUse\">\n"
      <> "  <text x=\"15\" y=\"12\" font-size=\"14\" text-anchor=\"middle\">"
      <> TB.decimal prn <> "</text>\n"
      <> "</marker>\n"

-- | Arrow marker definition for svg file
svgArrowMarker :: TB.Builder
svgArrowMarker =
  "<defs>\n"
  <> marker "green"
  <> marker "yellow"
  <> marker "orange"
  <> marker "red"
  <> "</defs>\n"
  where
    marker color =
         "<marker id=\"arrow-" <> TB.fromText color
      <> "\" markerWidth=\"10\" markerHeight=\"10\" refX=\"10\" refY=\"5\" "
      <> "orient=\"auto\" markerUnits=\"strokeWidth\">\n"
      <> "<path d=\"M0,0 L10,5 L0,10 Z\" fill=\"" <> TB.fromText color <> "\" />\n"
      <> "</marker>\n"

-- | Grid of sky plot definition for svg file
svgGrid :: TB.Builder
svgGrid =
  let rings =
        mconcat
          [ TB.fromString (printf "<circle cx=\"%.1f\" cy=\"%.1f\" r=\"%.1f\" stroke=\"#cccccc\" fill=\"none\" />\n"
                                   svgCx svgCy (svgRMax * (90 - e)/90))
          | e <- [0,5,10,15,30,60 :: Double]
          ]
      axes =
        "<line x1=\"" <> TB.realFloat (svgCx-svgRMax)
        <> "\" y1=\"" <> TB.realFloat svgCy
        <> "\" x2=\"" <> TB.realFloat (svgCx+svgRMax)
        <> "\" y2=\"" <> TB.realFloat svgCy
        <> "\" stroke=\"#cccccc\" />\n"
        <> "<line x1=\"" <> TB.realFloat svgCx
        <> "\" y1=\"" <> TB.realFloat (svgCy-svgRMax)
        <> "\" x2=\"" <> TB.realFloat svgCx
        <> "\" y2=\"" <> TB.realFloat (svgCy+svgRMax)
        <> "\" stroke=\"#cccccc\" />\n"

      labels =
        mconcat
          [ "<text x=\"" <> TB.realFloat (svgCx+dx)
            <> "\" y=\"" <> TB.realFloat (svgCy+dy)
            <> "\" font-size=\"16\" text-anchor=\"middle\">" <> TB.fromText lbl <> "</text>\n"
          | (dx,dy,lbl) <- [(0,-svgRMax-10,"N"),(svgRMax+15,0,"E"),(0,svgRMax+20,"S"),(-svgRMax-20,0,"W")]
          ]

      ringLabels =
        mconcat
          [ let r = svgRMax * (90 - fromIntegral e)/90
                x = svgCx + 2
                y = svgCy - r + 15
            in "<text x=\"" <> TB.realFloat x
               <> "\" y=\"" <> TB.realFloat y
               <> "\" font-size=\"14\" style=\"fill:#aaaaaa\">" <> TB.decimal e <> "°</text>\n"
          | e <- [15,30,60 :: Int]
          ]
  in rings <> axes <> labels <> ringLabels

-- | Legend of sky plot definition for svg file
svgLegend :: TB.Builder
svgLegend =
  let x0 = 20::Int
      y0 = svgTitleHeight + 40
      dy = 20
      entry (i, color, label) =
        "<rect x=\"" <> TB.decimal x0
        <> "\" y=\"" <> TB.decimal (y0 + i*dy)
        <> "\" width=\"20\" height=\"10\" fill=\"" <> TB.fromText color <> "\" />"
        <> "<text x=\"" <> TB.decimal (x0 + 30)
        <> "\" y=\"" <> TB.decimal (y0 + i*dy + 10)
        <> "\" font-size=\"14\" fill=\"black\">" <> TB.fromText label <> "</text>\n"
      items =
        [ (0::Int, "green",  "fitInterval =  4 h")
        , (1, "yellow", "fitInterval =  6 h")
        , (2, "orange", "fitInterval =  8 h")
        , (3, "red",    "fitInterval ≥ 10 h")
        ]
  in mconcat (map entry items)

-- | Frame of viewport for svg file
svgFrame :: TB.Builder
svgFrame =
  "<rect x=\"1\" y=\"1\" width=\"" <> TB.decimal (svgWidth-2)
                 <> "\" height=\"" <> TB.decimal (svgHeight-2)
  <> "\" fill=\"none\" stroke=\"gray\" stroke-width=\"2\" />\n"

-- | Create content of svg file      
svgCreateContent :: T.Text -> IntMap [([AzEl], NavRecord)] -> T.Text
svgCreateContent title skyMap =
  TL.toStrict . TB.toLazyText $
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> TB.decimal svgWidth <> "\" height=\"" <> TB.decimal svgHeight <> "\">\n"
    <> "<style>text { font-family: 'Segoe UI',Arial, sans-serif; fill: #222222; }</style>\n"
    <> "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n"
    <> "<text x=\"" <> TB.realFloat svgCx <> "\" y=\"" <> TB.decimal svgTitleHeight
    <> "\" text-anchor=\"middle\" font-size=\"20\">" <> TB.fromText title <> "</text>\n"
    <> svgFrame
    <> svgArrowMarker
    <> svgSatelliteMarker (IMS.keys skyMap)
    <> svgGrid
    <> svgLegend
    <> mconcat (IMS.foldMapWithKey
                   (\prn ls -> [ svgPolylineWithLabel color prn azels
                               | (azels, r) <- ls
                               , let color  = svgColorByFitInterval $ fitInterval r
                               ]
                   ) skyMap
                )
    <> "</svg>\n"

-- | The function iterates through a list of ephemeris, determining
-- the validity interval of each ephemeris based on the fitInterval
-- field corresponding to (week, toe). If two adjacent compartments
-- overlap, they are trimmed in the middle of the overlapping
-- parts. However, the intervals may have different fitIntervals and
-- the middle of the interval could fall on the other side (week,
-- toe). To prevent this, it is checked whether the designated
-- midpoint of the interval has not exceeded (week, toe). Therefore,
-- trimming (clamping) has additional condition - the mid should not
-- fall outside [(week1, toe1),(week2, toe2)]. When mid0 falls beyond
-- TOE r1 (to the left), we trim to (week1,toe1). When mid1 can fall
-- beyond TOE r2 (to the right), then we trim to (week2,toe2).
trimmOverlapingIntervals :: [NavRecord] -> [(Interval, NavRecord)]
trimmOverlapingIntervals [] = []
trimmOverlapingIntervals (r0:rs) =
    let (a0, b0) = ephValidityInterval r0
    in go ((a0, b0), r0) rs
  where
    go :: (Interval, NavRecord) -> [NavRecord] -> [(Interval, NavRecord)]
    go prev [] = [prev]

    go ((pa, pb), pr) (r:rest) =
      let (a, b) = ephValidityInterval r
          (week1, toe1) = (week pr, toe pr)
          (week2, toe2) = (week r , toe r)

          ovStart = max pa a
          ovEnd   = min pb b
          overlap = diffGpsWeekTow ovEnd ovStart
      in
      if overlap <= 0
         -- no overlap
         then ((pa, pb), pr) : go ((a, b), r) rest
         else
           -- there is an overlap, we determine the mid
           let mid0 = addSeconds ovStart (overlap / 2)
                      
               mid1 = if mid0 < (week1, toe1) then (week1, toe1) else mid0
               mid2 = if mid1 > (week2, toe2) then (week2, toe2) else mid1

               prev' = ((pa, mid2), pr)
               curr' = ((mid2, b), r)
           in prev' : go curr' rest


-- | Calculate the ephemeris validity interval based on the
-- fitInterval field.
ephValidityInterval
    :: NavRecord                                  -- ^ navigation record with ephemeris
    -> (GpsWeekTow, GpsWeekTow)                   -- ^ (valid from, valid until)          
ephValidityInterval r =
    let t = (week r, toe r)
        halfFitInterval = fromIntegral ((fitInterval r) `div` 2 * 3600)
        a = diffSeconds t halfFitInterval
        b = addSeconds  t halfFitInterval
    in (a, b)

-- | Subtract seconds from (Gps week, tow)
diffSeconds :: GpsWeekTow -> Pico -> GpsWeekTow
diffSeconds (week, tow) secs =
    let ds = tow - secs
        k = floor (ds / 604800)
        tow' = ds - fromIntegral k * 604800
    in (week + k, tow')

-- | Add seconds to (Gps week, tow)
addSeconds :: GpsWeekTow -> Pico -> GpsWeekTow
addSeconds (week, tow) secs =
    let ds = tow + secs
        k = floor (ds / 604800)
        tow' = ds - fromIntegral k * 604800
    in (week + k, tow')

-- | Generate sampling times for an interval with a step specified in
-- seconds. Calculations are relative to the beginning of the interval
-- to avoid cumulative errors. The floor function was used to avoid
-- crossing the end of the interval.
genSampleTimes :: Interval -> Pico -> [GpsWeekTow]
genSampleTimes (a, b) step
    | step <= 0 = error "genSampleTimes: step [s] must be positive"
    | len < 0   = error "genSampleTimes: b < a"
    | otherwise =
        let
            nSteps = floor (len / step)::Integer
            ts = [ addSeconds a (fromIntegral k * step)
                 | k <- [0 .. nSteps]
                 ]
        in if last ts == b
           then ts
           else ts ++ [b]
    where
       len = diffGpsWeekTow b a

-- | Calculate (azimuth, elevation) points for the interval only when the
-- satellite is above the horizon u > 0, sampling with a step of seconds.
-- NOTE: the interval is not fitInterval.
skyIntervalPoints
  :: (Interval, NavRecord)
  -> Pico
  -> ECEF
  -> [AzEl]
skyIntervalPoints (i, r) step obsPosECEF =
  [ enuToAzEl enu
  | t   <- genSampleTimes i step                             -- sample time
  , let enu@(_, _, u) =
            ecefVectorToENU (satPosECEF t r) obsPosECEF
  , u > 0
  ]

-- | Calculate (azimuth, elevation) points with a specified step for
-- the ephemeris of a single satellite. Points can only be within the
-- ephemeris validity range. If the ephemeris validity ranges overlap,
-- they are trimmed to prevent overlap.
skyPrnPoints
  :: ECEF
  -> Pico
  -> Map EphWeekTow NavRecord
  -> [([AzEl], NavRecord)]
skyPrnPoints obsPosECEF step prnMap =
  [ (pts, r)
  | (i, r) <- trimmOverlapingIntervals $ MS.elems prnMap
  , let pts = skyIntervalPoints (i, r) step obsPosECEF
  ]

-- | Compute points (azimuth, elevation) for satellite ephemerides
-- data contained in NavMap.
skyPoints
  :: ECEF
  -> Pico
  -> NavMap
  -> IntMap [([AzEl],NavRecord)]
skyPoints  obsPosECEF step =
  IMS.map (skyPrnPoints obsPosECEF step)
           
-- | Calculate azimuth and elevation from ENU vector.
enuToAzEl :: ENU -> AzEl
enuToAzEl (e, n, u) = (azDeg, elDeg)
  where
    az = atan2 e n
    el = atan2 u (sqrt (e*e + n*n))

    -- conversion to degrees
    azDeg = (az * 180 / pi) `mod'` 360
    elDeg =  el * 180 / pi


-- | Calculate observer -> satellite ECEF vector and transform it to
--   ENU.
ecefVectorToENU
    :: ECEF                             -- ^ Satellite ECEF coordinates
    -> ECEF                             -- ^ Observer  ECEF coordinates
    -> ENU                              -- ^ Vector from observer to satellite in ENU coordinates
ecefVectorToENU (xs, ys, zs) (xo, yo, zo) =
    let 
        -- vector from observer to satellite in ECEF
        dx = xs - xo
        dy = ys - yo
        dz = zs - zo
        (lat, lon, _) = ecefToWGS84 (xo, yo, zo)
        -- rotation to local ENU
        sinLat = sin lat
        cosLat = cos lat
        sinLon = sin lon
        cosLon = cos lon
        e = -sinLon * dx + cosLon * dy
        n = -sinLat * cosLon * dx - sinLat * sinLon * dy + cosLat * dz
        u =  cosLat * cosLon * dx + cosLat * sinLon * dy + sinLat * dz
    in (e, n, u)

-- | Transform ECEF coordinates (X,Y,Z) to WGS84 coordinates
--   (latitude, longtitude, height) where latitude, longtitude are in
--   radians. Hoffmann‑Wellenhof, Bowring method. Works for all points
--   except the center of the Earth.
ecefToWGS84 :: ECEF -> WGS84
ecefToWGS84 (x, y, z) = iterateLatLon lat0 h0
  where
    -- WGS84 constants
    a  = 6378137.0
    f  = 1 / 298.257223563
    e2 = f * (2 - f)

    r   = sqrt (x*x + y*y)
    -- Compute longtitude
    lon = atan2 y x

    -- Latitude Przybliżenie początkowe Bowringa
    b   = a * (1 - f)
    ep2 = (a*a - b*b) / (b*b)
    theta = atan2 (z * a) (r * b)
    sT = sin theta
    cT = cos theta

    lat0 = atan2 (z + ep2 * b * sT*sT*sT)
                 (r - e2  * a * cT*cT*cT)

    n0 = a / sqrt (1 - e2 * sin lat0 ** 2)
    h0 = r / cos lat0 - n0

    iterateLatLon :: Double -> Double -> WGS84
    iterateLatLon lat h =
      let
        n    = a / sqrt (1 - e2 * sin lat ** 2)
        lat' = atan2 z (r * (1 - e2 * n / (n + h)))
        h'   = r / cos lat' - n
      in
        if abs (lat' - lat) < 1e-12
           then (lat', lon, h')
           else iterateLatLon lat' h'
        

-- | Build a navigation map from GPS navigation records of RINEX 3.04
--   navigation body for healthy satellites and with max iode for
--   (week, toe).
navMapFromRinex :: L8.ByteString -> NavMap  
navMapFromRinex bs0
    | L8.null bs0        = error "Empty file"
    | rinexVer /= "3.04" = error "Not RINEX 3.04 file"
    | fileType /= "N"    = error "Not navigation file"
    | otherwise = let rnxBody = rnxSkipHeader bs0
                  in if L8.null rnxBody
                     then error "Cannot find navigation data in the file."
                     else go IMS.empty rnxBody
      where
        rinexVer = trim $ takeField  0 9 bs0 
        fileType = trim $ takeField 20 1 bs0
                   
        go :: NavMap -> L8.ByteString -> NavMap
        go m bs
          | L8.null bs = m
          | L8.take 1 bs == "G" =
              let (r, bs') = readRecord bs
                  m' = if svHealth r == 0
                       then insertRecord r m
                       else m
              in go m' bs' 
          | otherwise =
              go m (skipRecord bs)

-- | Skips header of RINEX 3.04 navigation file.  Uses information
-- about the label position and the fixed length of the header line
-- content.
rnxSkipHeader :: L8.ByteString -> L8.ByteString
rnxSkipHeader bs0 = loop bs0
  where
    loop bs
      | L8.null bs                  = error "Cannot find header"
      | label bs == "END OF HEADER" = dropLastLine bs
      | otherwise                   = loop (dropLine bs)
      where
        label    = trim . L8.takeWhile (not . (`L8.elem` "\r\n")) . L8.drop 60
        dropLine =        L8.dropWhile        (`L8.elem` "\r\n")  . L8.drop 80
        -- The last line is very often not completed until 80 characters
        dropLastLine = L8.dropWhile        (`L8.elem` "\r\n")
                     . L8.dropWhile (not . (`L8.elem` "\r\n"))
                     . L8.drop 72                           -- don't check before 60 + length "END OF HEADER"



-- | Read GPS satellite navigation record eight lines.  It is based on
--   the knowledge that the content of a line should be 80 characters,
--   but last line often breaks this rule.
readRecordLines :: L8.ByteString -> ([L8.ByteString], L8.ByteString)
readRecordLines bs0 =
    let (l1, bs1) = readLine bs0
        (l2, bs2) = readLine bs1
        (l3, bs3) = readLine bs2
        (l4, bs4) = readLine bs3
        (l5, bs5) = readLine bs4
        (l6, bs6) = readLine bs5
        (l7, bs7) = readLine bs6
        (l8, bs8) = readLastLine bs7
    in ([l1,l2,l3,l4,l5,l6,l7,l8], bs8)
    where
      -- Read a line of 80 characters and a line separator.
      readLine :: L8.ByteString -> (L8.ByteString, L8.ByteString)
      readLine bs =
          let (line, bs') = L8.splitAt 80 bs
              dropLineSep = L8.dropWhile (`L8.elem` "\r\n")
          in (line, dropLineSep bs')

      --   Last line can have two, three or four fields
      --   and sometimes it is not completed to 80 characters.
      readLastLine :: L8.ByteString -> (L8.ByteString, L8.ByteString)
      readLastLine bs =
          let (part1, bs1) = L8.splitAt 42 bs                         -- read the part containing two fields
              (part2, bs2) = L8.break     (`L8.elem` "\r\n") bs1
              dropLineSep  = L8.dropWhile (`L8.elem` "\r\n")
          in (part1 <> part2, dropLineSep bs2)

-- | Read GPS satellite navigation record
readRecord :: L8.ByteString -> (NavRecord, L8.ByteString)
readRecord bs =
    let (ls, bs') = readRecordLines bs
    in case getRecord ls of
         Just r  -> (r, bs')
         Nothing ->
             error $ L8.unpack $ "Unable to get GPS navigation record from \n:"
                               <> L8.unlines ls

-- | Get GPS navigation record from GPS record lines.  Expects 8 lines
--   as input. It does not read fields one by one, as parsers do, but
--   by position in the line.
getRecord :: [L8.ByteString] -> Maybe NavRecord
getRecord ls =
  case ls of
    [l1,l2,l3,l4,l5,l6,l7,l8] -> do
            (prn, _)  <- L8.readInt $ trim $ takeField  1 2 l1              -- trim is needed by readInt
            (y  , _)  <- L8.readInt $ trim $ takeField  4 4 l1
            (mon, _)  <- L8.readInt $ trim $ takeField  9 2 l1
            (d  , _)  <- L8.readInt $ trim $ takeField 12 2 l1
            (h  , _)  <- L8.readInt $ trim $ takeField 15 2 l1
            (m  , _)  <- L8.readInt $ trim $ takeField 18 2 l1
            (s  , _)  <- L8.readInt $ trim $ takeField 21 2 l1
  
            let toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)

            af0       <- getDouble $ takeField 23 19 l1
            af1       <- getDouble $ takeField 42 19 l1
            af2       <- getDouble $ takeField 61 19 l1
                 
            iodeD     <- getDouble $ takeField  4 19 l2
            crs       <- getDouble $ takeField 23 19 l2
            deltaN    <- getDouble $ takeField 42 19 l2
            m0        <- getDouble $ takeField 61 19 l2
                 
            cuc       <- getDouble $ takeField  4 19 l3
            e         <- getDouble $ takeField 23 19 l3
            cus       <- getDouble $ takeField 42 19 l3
            sqrtA     <- getDouble $ takeField 61 19 l3

            toeD      <- getDouble $ takeField  4 19 l4
            cic       <- getDouble $ takeField 23 19 l4
            omega0    <- getDouble $ takeField 42 19 l4
            cis       <- getDouble $ takeField 61 19 l4

            i0        <- getDouble $ takeField  4 19 l5
            crc       <- getDouble $ takeField 23 19 l5
            omega     <- getDouble $ takeField 42 19 l5
            omegaDot  <- getDouble $ takeField 61 19 l5
                                                               
            iDot      <- getDouble $ takeField  4 19 l6
            weekD     <- getDouble $ takeField 42 19 l6

            svHealthD <- getDouble $ takeField 23 19 l7
            iodcD     <- getDouble $ takeField 61 19 l7
                     
            ttom      <- getDouble $ takeField  4 19 l8
            fitIntervalD  <- getDouble $ takeField 23 19 l8

            let iode         = round      iodeD
                toe          = realToFrac toeD
                week         = round      weekD             -- conversion is needed for equality comparisons
                svHealth     = round      svHealthD
                iodc         = round      iodcD
                fitInterval  = round      fitIntervalD
            return NavRecord {..}
    _ -> Nothing

-- | Skip record reading lines to begining of other record.  Used to
--   skip records of constellations other than GPS.
skipRecord :: L8.ByteString -> L8.ByteString
skipRecord bs =
  let (_, rest) = L8.break (== '\n') bs
      rest' = L8.drop 1 rest
      in if isNewRecordLine rest'
           then rest'
           else skipRecord rest'

-- | Returns True if the bs starts with other sign than ' '                
isNewRecordLine :: L8.ByteString -> Bool
isNewRecordLine bs = L8.take 1 bs /= " "

-- | Insert a navigation record into a 'NavMap'. If there is no entry
-- for the given PRN or epoch, the record is inserted. If an entry
-- already exists, the record is replaced only if the new record has a
-- greater IODE than the existing one.  This ensures that for each
-- @(week, toe)@ only the navigation record with the maximum IODE is
-- kept.
insertRecord :: NavRecord -> NavMap -> NavMap
insertRecord r =
  IMS.alter updatePrn key1
  where
    key1 = prn r
    key2 = (week r, toe r)
    updatePrn Nothing =
        Just (MS.singleton key2 r)
    updatePrn (Just subMap) =
        Just (MS.alter (chooseNewer r) key2 subMap)

    chooseNewer :: NavRecord -> Maybe NavRecord -> Maybe NavRecord
    chooseNewer new Nothing    = Just new
    chooseNewer new (Just old) =
        if iode new > iode old
        then Just new
        else Just old

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace             

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

-- | Get Double value from ByteString field.
--   Its purpose is to stop reading if it cannot read the entire field.
--   After reading, the rest may be empty or consist only of spaces.
getDouble :: L8.ByteString -> Maybe Double
getDouble bs = do
  (val, rest) <- readDouble bs
  case L8.uncons rest of
    Just (c, _) | c=='D' || c=='d' ->
      error $ "Unsupported number format with 'D': " ++ L8.unpack bs
    _ -> guard (L8.all (== ' ') rest) >> return val   

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)              

-- | Determining the GPS satellite position in ECEF from the GPS
--   ephemeris and for a (GPS week, tow).
satPosECEF
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week [s]
    -> NavRecord                                            -- ^ navigation record with ephemeris
    -> ECEF                                                 -- ^ satellite position in ECEF [m]
satPosECEF (w, tow) r =
  let
    -- | Constants
    mu, omegaEDot :: Double
    mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
    omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]

    sqrtAr = sqrtA r                                        -- optimization
             
    a      =  sqrtAr * sqrtAr                               -- semi-major axis [m]
    n0     = sqrt(mu/(a*a*a))                               -- computed mean motion [rad/sec]       
    n      = n0 + deltaN r                                  -- corrected mean motion [rad/s]        
    tk     = realToFrac $
             diffGpsWeekTow (w, tow) (week r, toe r)        -- time elapsed since toe [s]
    mk     = m0 r + n*tk                                    -- mean anomaly at tk [rad]             
    ek     = keplerSolve mk (e r)                           -- eccentric anomaly [rad]              
    vk     = atan2 (sqrt (1 - e r *e r ) * sin ek)
                   (cos ek - e r)                           -- true anomaly                         
    phik   = vk + omega r                                   -- argument of latitude
             
    sin2phik = sin (2*phik)                                 -- optimization
    cos2phik = cos (2*phik)                                 -- optimization
               
    duk    = cus r * sin2phik 
           + cuc r * cos2phik                               -- argument of latitude correction      
    drk    = crs r * sin2phik
           + crc r * cos2phik                               -- radius correction                    
    dik    = cis r * sin2phik
           + cic r * cos2phik                               -- inclination correction               
    uk     = phik + duk                                     -- corrected argument of latitude       
    rk     = a * (1 - e r * cos ek) + drk                   -- corrected radius                     
    ik     = i0 r + dik + iDot r * tk                       -- corrected inclination                
    xk'    = rk * cos uk                                    -- xk' in the orbital plane             
    yk'    = rk * sin uk                                    -- yk' in the orbital plane             
    omegak = omega0 r                                                                                 
           + (omegaDot r - omegaEDot)*tk
           - omegaEDot * realToFrac (toe r)                 -- corrected longitude of ascending node

    cosik = cos ik                                          -- optimization
    sinik = sin ik                                          -- optimization
    cosomegak = cos omegak                                  -- optimization
    sinomegak = sin omegak                                  -- optimization
                
    xk     = xk' * cosomegak - yk' * cosik * sinomegak      -- transformation to ECEF               
    yk     = xk' * sinomegak + yk' * cosik * cosomegak      -- transformation to ECEF               
    zk     =                   yk' * sinik                  -- transformation to ECEF
  in (xk,yk,zk)

-- | Iterative solution of Kepler's equation ek = m + e sin ek
--   (Newtona-Raphsona method)
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
            eN'  = eN - f/fDot                              -- iterative formula
            f    = eN - e * sin eN - m  
            fDot =  1 - e * cos eN                          -- derivative of the function f
       
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
