
open Printf
open OUnit

let cleanup = ref true

let tests =
  "Unit tests" >::: (Tests.all_tests ())

let verbose = ref false

let () =
  ignore (run_test_tt_main tests)
