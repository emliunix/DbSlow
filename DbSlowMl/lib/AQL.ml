let something = "hello";;

let mytest () =
  QLLex.qparse (Lexing.from_string " select something from tbl_a where b <> 1")
  |> ignore
;;
