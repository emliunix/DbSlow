(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* val testAQLToken : AQL.QLParser.token Alcotest.testable
;; *)

let test_token = Alcotest.testable x ()

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (String.lowercase_ascii "hELLO!")

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "string-case", [
          test_case "Lower case"     `Quick test_lowercase;
        ];
    ]
;;