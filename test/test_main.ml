open OUnit2
open Flex.Buffer_handles

let () =
  run_test_tt_main
    ("suite" >::: [ "" >:: exposed_buf_test ])

