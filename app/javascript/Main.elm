module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Suite exposing (suite)


main : BenchmarkProgram
main =
    program suite
