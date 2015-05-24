
module Main where

import Termbox

main :: IO ()
main = print =<< unsafeGetWinSize
