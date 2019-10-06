{
  open Option
  type token = QLToken.token

  exception SyntaxError of string

  let as_keyword s : token option =
  match String.lowercase_ascii s with
  | "select"    -> Some SELECT
  | "from"      -> Some FROM
  | "where"     -> Some WHERE
  | "as"        -> Some FROM
  | "and"       -> Some AND
  | "or"        -> Some OR
  | "in"        -> Some IN
  | _           -> None
}

let white = [' ' '\r' '\n' '\t']+
let id_keyword = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let n_1_9 = ['1'-'9'] 
let n_0_9 = ['0'-'9'] 
let int_lit = n_1_9 n_0_9*

rule qparse =
  parse
    white { qparse lexbuf }
  | id_keyword { 
      let s = Lexing.lexeme lexbuf in
      match as_keyword s with
      | Some v -> v
      | None -> IDENT s
    }
  | ',' { COMMA }
  | '=' { EQUAL }
  | "<>" { NEQUAL }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIVIDE }
  | '*' { MULTIPLY }
  | int_lit { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (SyntaxError (
          let pos = lexbuf.lex_curr_p in
          let fname = match pos.pos_fname with
            | "" -> "Unknown"
            | _ as s -> s
          in
          let s = Lexing.lexeme lexbuf in
          Printf.sprintf "Things become strange here: %s:%d:%d %s"
            fname pos.pos_lnum pos.pos_bol s
        ))
      }
  (* EOF seems quite special *)
  | eof { EOF }
