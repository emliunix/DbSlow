type type_tag =
  | StringType
  | IntType
  | BoolType
  | UnknownType
;;
type column = {
    name: string;
    col_type: type_tag;
  }
;;
type lit =
  | StringLit of string
  | IntLit of int
  | BoolLit of bool
;;
type arith_op =
  (* int / float *)
  | Plus
  | Minus
  | Div
  | Mul
  (* bool *)
  | And
  | Or
  | Eq
;;
type expr = {
    expr_type: type_tag;
    compu: compu;
  }
and compu =
  (* A placeholder *)
  | Empty
  | Lit of lit
  | Col of column
  | Cast of type_tag * expr  (* to_type * expr *)
  (* Arithmetic *)
  | Arith of type_tag * arith_op * (expr list)
;;
type stage =
  (* Logical Nodes *)
  | Projection of stage * (column list)
  | Filter of stage * expr
  | Table of string
  (* Physical Nodes *)
  | Scan of stage
  (* | Fetch of fetchable * stage *)
;;
type table_schema = column list;;
type csv_option = {
    skip_first_line: bool;
    val_delimiter: char;
    esc_char: char;
  }
;;
type file_format =
  | CSV of csv_option
  | JSON
;;
type table_opts = {
    file: string option;
    format: file_format option;
  }
;;
type table = string * table_schema * table_opts;;
