let something = "hello";;

module QLLex = QLLex;;
module QLParser = QLParser;;
module AIL = AIL;;

let mytest () =
  QLLex.qparse (Lexing.from_string " select something from tbl_a where b <> 1")
  |> ignore
;;
