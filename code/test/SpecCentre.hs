module SpecCentre (
    module Test.Hspec,
    module StateInterpreter,
    module Grammar,
    module Parser,
    module TailCalls,
    module RuntimeStructs,
    fromValue
) where

import Test.Hspec
import StateInterpreter
import Grammar
import Parser
import TailCalls
import RuntimeStructs

fromValue :: Value -> Integer
fromValue (VI v) = v
fromValue cons@(VC _) = error ("Suspension = " ++ show cons)