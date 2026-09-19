module Kern82.ConstEval (caseConstant, ordinalConstant, integerConstant) where

import Data.Bits ((.&.), (.|.), xor, complement, shiftL, shiftR)
import Kern82.AST

-- Evaluate the typed AST, checking each intermediate result with the same
-- widths and integer operations as the runtime. The callback supplies typed
-- initializers of KONST bindings, never the values of mutable variables.
caseConstant :: (Binding -> Result Expr) -> Expr -> Result Integer
caseConstant initializer = evaluate []
  where
    evaluate seen e = do
      let ty = representation (checkedType e)
          err = failure "type" (exprPos e)
      if ty `notElem` [Ganz,Lang,CharT] && not (isEnum ty) then err "CASE requires an integer or CHAR constant expression"
      else do
        n <- case exprNode e of
          IntLit value -> pure value
          CharLit value -> pure (toInteger value)
          EnumLit _ value -> pure value
          Bound b
            | not (bindConst b) -> err "CASE value must be a compile-time constant"
            | bindingC b `elem` seen -> err "cyclic CASE constant initializer"
            | otherwise -> initializer b >>= evaluate (bindingC b:seen)
          Cast _ x -> evaluate seen x
          Unary op x -> do
            a <- evaluate seen x
            case op of
              "+" -> pure a; "-" -> pure (-a); "~" -> pure (complement a)
              _ -> err "CASE requires an integer or CHAR constant expression"
          Binary op x y -> do
            a <- evaluate seen x; b <- evaluate seen y
            let width = if ty == Ganz then 16 else 32
                shift operation = if b < 0 || b >= width
                  then err "invalid shift count in CASE constant"
                  else pure (operation a (fromInteger b))
            case op of
              "+" -> pure (a+b); "-" -> pure (a-b); "*" -> pure (a*b)
              "/" -> if b == 0 then err "division by zero in CASE constant" else pure (a `quot` b)
              "%" -> if b == 0 then err "division by zero in CASE constant" else pure (a `rem` b)
              "&" -> pure (a .&. b); "|" -> pure (a .|. b); "^" -> pure (a `xor` b)
              "<<" -> shift shiftL; ">>" -> shift shiftR
              _ -> err "CASE requires an integer or CHAR constant expression"
          _ -> err "CASE value must be a compile-time constant"
        let (low,high) = case ty of
              Ganz -> (-32768,32767)
              Lang -> (-2147483648,2147483647)
              CharT -> (0,255)
              _ -> (-2147483648,2147483647)
        if n < low || n > high then err "CASE constant overflow or cast out of range" else pure n

-- ENUM ordinals use checked signed 32-bit integer constant expressions.
ordinalConstant :: Expr -> Result Integer
ordinalConstant = integerConstant Lang

integerConstant :: Type -> Expr -> Result Integer
integerConstant width e = do
  let err = failure "semantic" (exprPos e)
      go = integerConstant width
  value <- case exprNode e of
    IntLit n -> pure n
    Cast ty x | representation ty `elem` [Ganz,Lang,CharT] -> do
      n <- go x
      let (lo,hi) = case representation ty of Ganz -> (-32768,32767); CharT -> (0,255); _ -> (-2147483648,2147483647)
      if n < lo || n > hi then err "ENUM constant cast out of range" else pure n
    Unary "-" x | IntLit n <- exprNode x -> pure (-n)
    Unary op x -> do
      n <- go x
      case op of
        "+" -> pure n; "-" -> pure (-n); "~" -> pure (complement n)
        _ -> err "ENUM value must be an integer constant expression"
    Binary op x y -> do
      a <- go x; b <- go y
      let shift operation = if b < 0 || b >= (if width == Ganz then 16 else 32) then err "invalid shift in ENUM value"
            else pure (operation a (fromInteger b))
      case op of
        "+" -> pure (a+b); "-" -> pure (a-b); "*" -> pure (a*b)
        "/" -> if b == 0 then err "division by zero in ENUM value" else pure (a `quot` b)
        "%" -> if b == 0 then err "division by zero in ENUM value" else pure (a `rem` b)
        "&" -> pure (a .&. b); "|" -> pure (a .|. b); "^" -> pure (a `xor` b)
        "<<" -> shift shiftL; ">>" -> shift shiftR
        _ -> err "ENUM value must be an integer constant expression"
    _ -> err "ENUM value must be an integer constant expression"
  let (lo,hi) = if width == Ganz then (-32768,32767) else (-2147483648,2147483647)
  if value < lo || value > hi then err "ENUM ordinal is outside signed 32-bit range or KONST integer range"
    else pure value
