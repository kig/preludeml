
let tests : OUnit.test list ref = ref []

let register x = tests := x :: !tests

let all_tests () = !tests
