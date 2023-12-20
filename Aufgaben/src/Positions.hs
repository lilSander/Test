module Positions where

positions :: [a] -> [(a,Int)]
positions xs = zip xs [1..]