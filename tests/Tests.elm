module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
        [ test "dummy" (\_ -> Expect.equal "foo" "foo") ]
