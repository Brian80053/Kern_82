module Kern82.Lexer (Token(..), TokenKind(..), lexSource) where

import Data.List (find, isPrefixOf)
import Text.Read (readMaybe)
import Kern82.AST
import Kern82.Charset

data TokenKind = Word String | Symbol String | IntegerToken Integer
  | FloatToken Float | CharacterToken Int | StringToken [Int] | EndOfFile
  deriving (Eq, Show)
data Token = Token { tokenPos :: Pos, tokenKind :: TokenKind } deriving (Eq, Show)

keywords :: [String]
keywords = words "GANZ LANG BOOL GLEIT CHAR KETTE KONST REGISTER FUNKTION PROZEDUR BEGIN END IF THEN ELSE FOR WHILE RÜCKGABE ABBRUCH WEITER WAHR FALSCH AND OR NOT XOR AUSGABE SCHREIBEN LESEN STRING TYP STRUCT ENUM PUBLIC PRIVATE ZEIGER REFERENZ ADDRESSE INHALT NULL ALLOKIEREN FREIGEBEN GRÖSSE TYPVON UNSAFE TRY CATCH RAISE MODUL IMPORT EXPORT GOTO SWITCH CASE DEFAULT FALLTHROUGH VOID UNION GENERISCH NEU VERGLEICHBAR GEORDNET NUMERISCH"

advance :: Pos -> Char -> Pos
advance (Pos l _) '\n' = Pos (l+1) 1
advance (Pos l c) _ = Pos l (c+1)
move :: Pos -> String -> Pos
move = foldl advance

lexSource :: String -> Result [Token]
lexSource input = scan (Pos 1 1) (case input of '\xfeff':xs -> xs; xs -> xs)
  where
    scan p [] = Right [Token p EndOfFile]
    scan p s@(c:cs)
      | kernCharCode c == Nothing = failure "lexer" p
          (unsupportedCharacter c)
      | kernSpace c = scan (advance p c) cs
      | "/*" `isPrefixOf` s = comment p (move p "/*") (drop 2 s)
      | "===" `isPrefixOf` s = failure "lexer" p "operator === does not exist; use =="
      | kernIdentifierStart c =
          let (w, rest) = span kernIdentifierContinue s
              kind = if w `elem` keywords then Symbol w else Word w
          in emit p w kind rest
      | asciiDigit c = number p s
      | c == '"' = quoted p (advance p c) '"' [] cs
      | c == '\'' = quoted p (advance p c) '\'' [] cs
      | otherwise = case find (`isPrefixOf` s) operators of
          Just op -> emit p op (Symbol op) (drop (length op) s)
          Nothing -> failure "lexer" p ("unexpected character " ++ show c)
    emit p consumed kind rest = (Token p kind :) <$> scan (move p consumed) rest
    operators = ["++","--","==","!=","<=",">=","&&","||","<<",">>"] ++
      map (:[]) "(){}[];,:.+-*/%=<>!&|^~"
    comment start p s = case s of
      [] -> failure "lexer" start "unterminated /* comment */"
      '*':'/':rest -> scan (move p "*/") rest
      x:_ | kernCharCode x == Nothing -> failure "lexer" p
        (unsupportedCharacter x)
      x:rest -> comment start (advance p x) rest
    number p s = do
      let (whole, r1) = span asciiDigit s
          (fraction, r2) = case r1 of
            '.':d:ds | asciiDigit d -> let (digits, rest) = span asciiDigit (d:ds)
                                  in ('.':digits, rest)
            _ -> ("", r1)
      (exponentPart, rest) <- case r2 of
        e:xs | e `elem` "eE" ->
          let (sign, ys) = case xs of
                z:tailDigits | z `elem` "+-" -> ([z], tailDigits)
                _ -> ("", xs)
              (digits, zs) = span asciiDigit ys
          in if null digits then failure "lexer" p "missing exponent digits"
             else Right (e:sign ++ digits, zs)
        _ -> Right ("", r2)
      let raw = whole ++ fraction ++ exponentPart
      case rest of
        x:_ | kernIdentifierStart x -> failure "lexer" p "invalid numeric literal"
        _ -> if null fraction && null exponentPart
          then emit p raw (IntegerToken (read whole)) rest
          else case readMaybe raw of
            Just n | not (isInfinite n || isNaN n) -> emit p raw (FloatToken n) rest
            _ -> failure "lexer" p "floating literal is out of range"
    quoted start p delimiter acc s = case s of
      [] -> failure "lexer" start "unterminated literal"
      '\n':_ -> failure "lexer" p "newline inside literal"
      '\r':_ -> failure "lexer" p "newline inside literal"
      c:rest | c == delimiter -> do
        let value = reverse acc
        kind <- if delimiter == '"' then Right (StringToken value)
          else case value of
            [code] -> Right (CharacterToken code)
            _ -> failure "lexer" start "CHAR requires exactly one 8-bit Kern 82 character"
        (Token start kind :) <$> scan (advance p c) rest
      '\\':e:rest -> case lookup e [('n','\n'),('r','\r'),('t','\t'),('0','\0'),('\\','\\'),('\'','\''),('"','"')] of
        Just ch -> quoted start (move p ['\\',e]) delimiter
          (maybe acc (:acc) (kernCharCode ch)) rest
        Nothing -> failure "lexer" p ("unknown escape \\" ++ [e])
      c:rest -> case kernCharCode c of
        Just code -> quoted start (advance p c) delimiter (code:acc) rest
        Nothing -> failure "lexer" p
          (unsupportedCharacter c)
    unsupportedCharacter c =
      "character is not representable in the Kern 82 8-bit character set (default): '" ++ [c] ++ "'"
    asciiDigit x = x >= '0' && x <= '9'
