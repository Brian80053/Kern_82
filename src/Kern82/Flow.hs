module Kern82.Flow (returns) where

import Data.List (nub)
import Kern82.AST

data Exit = Normal | Returned | Broken | Continued | Fallen deriving Eq

returns :: Stmt -> Bool
returns s = exits s == [Returned]

sequenceExits :: [Stmt] -> [Exit]
sequenceExits [] = [Normal]
sequenceExits (s:ss) = nub $ concatMap next (exits s)
  where next Normal = sequenceExits ss; next outcome = [outcome]

exits :: Stmt -> [Exit]
exits (Stmt _ node) = case node of
  Return _ -> [Returned]
  Break -> [Broken]
  Continue -> [Continued]
  Fallthrough -> [Fallen]
  Block ss -> sequenceExits ss
  If _ yes no -> nub (exits yes ++ maybe [Normal] exits no)
  -- Preserve the conservative policy: loops may terminate, including infinite
  -- loops not proven here. Break and continue are consumed by this loop only.
  While _ b -> loopExits b
  For _ _ _ b -> loopExits b
  Switch _ arms -> nub (concat (armExits arms) ++ [Normal | not (any isDefault arms)])
  _ -> [Normal]
  where
    loopExits b = nub (Normal : [Returned | Returned `elem` exits b])
    isDefault a = armValue a == Nothing
    armExits [] = []
    armExits (a:as) =
      let rest = armExits as
          following = case rest of x:_ -> x; [] -> [Normal]
          resolve Fallen = following
          resolve outcome = [outcome]
      in nub (concatMap resolve (sequenceExits (armBody a))) : rest
