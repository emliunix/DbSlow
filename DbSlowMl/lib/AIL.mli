type type_tag =
  | StringType
  | IntType
  | BoolType
;;

type lit =
  | StringLit of string
  | IntLit of int
  | BoolLit of bool
;;

type expr =
  | Empty
  | Lit of type_tag * lit
  | Col of type_tag * column
  | Cast of type_tag * type_tag * expr
  | IPlus of expr * expr
  | IMinus of expr * expr
  | IDiv of expr * expr
  | IMul of expr * expr
  | BAnd of expr * expr
  | BOr of expr * expr
  | Eq of type_tag * expr * expr
and column = {
    name: string;
    type_tag: type_tag;
    value: expr
  }
;;

type stage =
  (* Logical Nodes *)
  | Projection of stage * (column list)
  | Filter of stage * expr
  | Table of string
  (*  *)
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
