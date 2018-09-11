module Lib where

import Text.Trifecta

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop
