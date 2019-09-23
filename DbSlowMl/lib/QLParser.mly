%{

%}
%token SELECT
%token FROM
%token WHERE
%token AS
%token EQUAL  (* = *)
%token NEQUAL (* <> | != *)
%token AND
%token OR
%token IN
%token LPAREN
%token RPAREN
%token <string> IDENT
%token <int> INTEGER
%token EOF
%start <token option> sql_stmt_p
%%

sql_stmt_p:
  | SELECT  { Some SELECT }

%%
