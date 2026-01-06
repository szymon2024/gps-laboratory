-- 2026-01-06

{- | The program creates copy of a RINEX 3.04 file, replacing the letter
     'D' with 'e' in the data section so that scientific notation uses
     'e' instead of Fortran-style 'D'. The header remains unchanged.

     NOTE:
       It is important to detect END OF HEADER from column 60,
       because there can be END OF HEADER in comment fields.

     Input:
       - source RINEX file name      (to set in the code)   sn
       - destination RINEX file name (to set in the code)   dn

     Output: 
       - destination RINEX file with normalized scentific notation with letter 'e'
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8    
import           Data.Char                           (isSpace)
import           Data.Int                            (Int64)
import           Text.Printf

main :: IO ()
main = do
  let
      sn = "source.nav"                                     -- Input: source file name     
      dn = "destination.nav"                                -- Input: destination file name

  printf "Start processing\n"
  bs <- L8.readFile sn
  let bs' = rnxConvert bs
  L8.writeFile dn bs'
  printf "Processing complete\n"
           

-- | Convert scientific notation of RINEX 3.04 file by:
--   * separating the header (up to the line containing "END OF HEADER"),
--   * copying the header unchanged,
--   * replacing all 'D' or 'd' with 'E' in the body of file,      
rnxConvert :: L8.ByteString -> L8.ByteString                   
rnxConvert bs =
        let n = rnxHeaderLength bs
            (hdr, body) = L8.splitAt n bs
        in hdr <> (L8.map replaceD body)

-- | Count RINEX 3.04 header length.  The header record consists of:
--   - data field 0-59,
--   - label field 60-EOL.
--   It cannot use the RINEX 3.04 specification knowledge that the
--   width of a line should be 80 characters, because last line
--   sometimes breaks this rule.
rnxHeaderLength
    :: L8.ByteString                              -- ^ rinex content
    -> Int64                                      -- ^ length of header
rnxHeaderLength bs0
    | L8.null bs0          = error
                                   "Error\n\
                                   \Empty input."
    | rnxVer bs0 /= "3.04" = error
                                   "Error\n\
                                   \The input file is not 3.04 version 3.04."
    | otherwise            = count 0 bs0
    where
      rnxVer = trim . takeField  0 9

      count acc bs
          | L8.null bs = error
                                   "Error\n\
                                   \END OF HEADER could not be found.\
                                   \Empty input."
          | otherwise =
              let -- count length of line (length of header record)
                  bs1         = L8.drop 60 bs
                  (ss,   bs2) = L8.span isSpace bs1
                  (bs13, bs3) = L8.splitAt 13 bs2                 -- at length of "END OF HEADER"
                  (xs,   bs4) = L8.break (`L8.elem` "\n\r") bs3
                  (eol,  bs') = L8.span  (`L8.elem` "\n\r") bs4
                  n           = 60 + L8.length ss + 13            -- length of line
                                + L8.length xs + L8.length eol
              in case bs13 of
                   "END OF HEADER" -> acc + n
                   _               -> count (acc + n) bs'


-- | Replace 'D' or 'd' with 'e'.
--   In RINEX, floating-point numbers may use Fortran-style
--   scientific notation with 'D'. This function normalizes
--   them to the standard 'e' notation.
replaceD :: Char -> Char
replaceD c
  | c == 'D'  = 'e'
  | c == 'd'  = 'e'
  | otherwise = c

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start       

