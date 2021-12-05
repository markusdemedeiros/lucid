
module Util where

import Control.Monad (mapM_)


-- Helpful alias
todo = undefined

take_every_nth :: Int -> [a] -> [a]
take_every_nth _ [] = []
take_every_nth n l  = (head l):(take_every_nth n (drop n l))
