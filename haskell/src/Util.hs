
module Util where

import Control.Monad (mapM_)


-- | Helpful alias
todo = undefined

-- | Map over lists, sorthand to OR or AND values
ormap, andmap :: [Bool] -> Bool
ormap   = foldr (||) False
andmap  = foldr (&&) True
