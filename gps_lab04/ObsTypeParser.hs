-- 2026-01-20

{- Observation Type Parser for RINEX 3.04 File

   The program reads the observation types from the rinex file header.
   It is assumed that the lines with observation types for a satellite
   system cannot be separated by other lines, e.g. a comment.

   How it works:
   
   The program assumes that the header lines except the last line are
   80 characters long. It reads the header line by line, checking
   whether the line contains the label "SYS / # / OBS TYPES" or the
   label "END OF HEADER".  When it encounters the label "SYS/#/OBS
   TYPES", it starts reading the observation types and computes how
   many observation types remain to be read.  The program terminates
   after passing the rinex file header. The returned list can be
   easily converted to a map.

   Input
   RINEX 3.04 navigation file name                fn

   Output
   list of observation types
   grouped by satelite system                     obsTs

   Print of run:
   [('G',(8,["C1C","L1C","D1C","S1C","C2X","L2X","D2X","S2X"]))]
   
-}

{-# LANGUAGE OverloadedStrings #-}

module ObsTypeParser where
    
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Int                          (Int64)
import           Data.Char                         (isSpace)    
import           Data.Array.Unboxed

----------------------------------------------------------------------

type Sys = Char

----------------------------------------------------------------------
    
main :: IO ()
main = do
  let fn = "rinex.obs"                            -- Input: RINEX 3.04 observation file name
  
  bs <- L8.readFile fn

  let (obsTs, _) = readObsTypes bs

  print obsTs

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
  -> ([(Sys, (Int, [L8.ByteString]))], L8.ByteString)
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
    -> (Sys, Int, Int, [L8.ByteString])
readObsTypesFirstLine bs
    | sys `L8.elem` "GREJCIS"  =
      let 
            k   = min n maxObsTypesPerLine
            fs  = takeObsTypes k bs
        in if n>=0
           then (sys, n, n-k, fs)
           else error "A negative number of observation types is declared."
    | sys == ' '               =
        error "Brak oznaczenia systemu satelitarnego w linii \n\
              \z etykietą SYS / # / OBS TYPES."
    | otherwise                =
        error $
          "Błąd\n\
          \Nieoczekiwany system satelitarny '" ++ [sys]
          ++ "' w nagłówku pliku wejściowego.\n\
          \Dozwolone: G, R, E, J, C, I, S."
    where
      sys = L8.index bs 0
      n   = getFieldInt 3 3 bs


----------------------------------------------------------------------

readObsTypesContLines
     :: Int
     -> [L8.ByteString]
     -> L8.ByteString
     -> ([L8.ByteString], L8.ByteString)
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
  -> [L8.ByteString]
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

lookEOH :: L8.ByteString -> Bool
lookEOH   = (== "END OF HEADER") . L8.take 13 . dropSpace . L8.drop 60

----------------------------------------------------------------------

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

----------------------------------------------------------------------

-- | Drop leading whitespaces from a lazy ByteString.              
dropSpace :: L8.ByteString -> L8.ByteString
dropSpace = L8.dropWhile isSpace
             
----------------------------------------------------------------------

takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

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
