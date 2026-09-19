module Kern82.Charset
  ( kernCharCode, kernIdentifierStart, kernIdentifierContinue
  , kernSpace, encodeIdentifier, extendedCodePoints
  ) where

import Data.Char (isLetter, ord)
import Data.List (elemIndex)
import Numeric (showHex)

-- Kern 82 source files may be stored as UTF-8 by the modern frontend, but
-- language characters are bytes.  The lower half is US ASCII and the upper
-- half is the Kern 82 extension, in byte order 0x80 through 0xff.
-- Runtime C tables are generated from this same mapping.
extendedCodePoints :: [Int]
extendedCodePoints = map ord $
  "ÄÖÜäöüßÀàÂâÆæÇçÈ" ++ -- 80-8f
  "èÉéÊêËëÎîÏïÔôŒœÙ" ++ -- 90-9f
  "ùÛûŸÿÅåØøÕõĀāĄąĆ" ++ -- a0-af
  "ćČčĒēĖėĘęĢģĪīĮįĶ" ++ -- b0-bf
  "ķĻļŁłŃńŅņÓóŚśŠšŪ" ++ -- c0-cf
  "ūŬŭŲųŹźŻżŽž«»§°±" ++ -- d0-df
  "×÷≠≤≥≈√∞µ£¢¥¤©¬¦" ++ -- e0-ef
  "·¡¿←↑→↓─│┌┐└┘┼█░"    -- f0-ff

kernCharCode :: Char -> Maybe Int
kernCharCode c
  | ord c < 128 = Just (ord c)
  | otherwise = (128 +) <$> elemIndex (ord c) extendedCodePoints

kernIdentifierStart :: Char -> Bool
kernIdentifierStart c = c == '_' || asciiLetter c ||
  (isLetter c && kernCharCode c /= Nothing)
  where asciiLetter x = x >= 'A' && x <= 'Z' || x >= 'a' && x <= 'z'

kernIdentifierContinue :: Char -> Bool
kernIdentifierContinue c = kernIdentifierStart c || c >= '0' && c <= '9'

kernSpace :: Char -> Bool
kernSpace c = c `elem` " \t\r\n\f\v"

encodeIdentifier :: String -> String
encodeIdentifier = concatMap encode
  where
    encode c = case kernCharCode c of
      Just n -> showHex n "_"
      Nothing -> error "internal: non-Kern character in resolved identifier"
