type token = 
  | WHERE
  | SELECT
  | RPAREN
  | OR
  | NEQUAL
  | COMMA
  | LPAREN
  | INTEGER of (int)
  | IN
  | IDENT of (string)
  | FROM
  | EQUAL
  | EOF
  | AS
  | AND
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
[@@derive show]
;;
