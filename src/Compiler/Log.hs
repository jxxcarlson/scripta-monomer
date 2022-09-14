module Compiler.Log(xlog) where

import Debug.Trace

xlog :: Show a => String -> a -> a
xlog msg a = Debug.Trace.trace (msg <> ": " <> show a) a

