-- ---------------------------------------------------------- [ TestRunner.idr ]
-- Module      : TestRunner
-- Description : Defines and runs test.x
--
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module TestRunner

import public Effects
import public Effect.StdIO
import public Effect.Exception

-- --------------------------------------------------- [ Type Sigs and Aliases ]
TestEffs : List EFFECT
TestEffs = [STDIO, EXCEPTION String]

EffTest : Type
EffTest = {TestEffs} Eff ()

-- ------------------------------------------------------------- [ Test Runner ]
tests : List EffTest -> {TestEffs} Eff ()
tests Nil = with Effects do
    putStrLn "All tests passed"
    pure ()
tests (t::ts) = do
    result <- t
    tests ts

-- --------------------------------------------------------------------- [ EOF ]
