module Kern82.Generics (validateGenericNames, checkConstraints, aliasRoutine) where

import Control.Monad (foldM, unless, when)
import qualified Data.Map.Strict as M
import Kern82.AST

-- The ordinary passes still own prototype matching. A template or NEU name,
-- however, declares one new symbol and cannot silently merge with a prototype.
validateGenericNames :: [Top] -> Result ()
validateGenericNames tops = do
  _ <- foldM add M.empty (concatMap names tops)
  pure ()
  where
    names top = case top of
      Generic parameters r _ -> [(routineName r,routinePos r,True,parameters)]
      Instantiate p name _ _ _ -> [(name,p,True,[])]
      Function r -> [(routineName r,routinePos r,False,[])]
      Global d -> [(bindName (declBinding d),bindPos (declBinding d),False,[])]
      TypeTop d -> let TypeId p name = typeDeclId d in (name,p,False,[]):case d of
        EnumDef _ members -> [(n,q,False,[]) | Enumerator q n _ <- members]
        _ -> []
    add seen (name,p,special,parameters) = do
      when (special && name == "main" && not (null parameters)) $
        failure "semantic" p "generic main is not allowed"
      case M.lookup name seen of
        Just old | special || old -> failure "semantic" p ("duplicate top-level symbol: " ++ name)
        _ -> pure ()
      _ <- foldM parameter M.empty parameters
      pure (M.insert name special seen)
    parameter seen (GenericParam p name _) = do
      when (M.member name seen) $ failure "semantic" p ("duplicate generic type parameter: " ++ name)
      pure (M.insert name () seen)

checkConstraints :: Pos -> String -> [GenericParam] -> [Type] -> Result ()
checkConstraints p template parameters arguments = do
  unless (length parameters == length arguments) $
    failure "type" p ("wrong number of generic type arguments: expected " ++
      show (length parameters) ++ ", got " ++ show (length arguments))
  mapM_ check (zip parameters arguments)
  where
    check (GenericParam _ name constraint,ty) = unless (admits constraint ty) $
      failure "type" p ("generic constraint " ++ label constraint ++ " is not satisfied by " ++
        typeName ty ++ " for " ++ name ++ " while instantiating " ++ template)
    admits Unconstrained _ = True
    admits Numeric ty = isNumeric ty
    admits Ordered ty = isNumeric ty || representation ty == CharT
    admits Comparable ty = isNumeric ty || representation ty `elem` [CharT,BoolT] || isEnum ty
    label Unconstrained = "unconstrained"
    label Numeric = "NUMERISCH"
    label Ordered = "GEORDNET"
    label Comparable = "VERGLEICHBAR"

-- Repeated instances share the first monomorphized body. An ordinary forwarding
-- routine preserves each explicitly declared name and its address/signature.
aliasRoutine :: Pos -> String -> Routine -> Routine
aliasRoutine p name target = target {routinePos=p,routineName=name,routineBody=Just body}
  where
    arguments = [expression p (Variable (bindName b)) | b <- routineParams target]
    call = expression p (Call (routineName target) arguments)
    statement = if routineResult target == Unit then Expression call else Return (Just call)
    body = Stmt p (Block [Stmt p statement])
