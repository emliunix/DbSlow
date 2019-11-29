# Parser Eval Ops

The builtin prime-ops' definition include the symbol and priority information used by the parser, and the type and computation part used by the evaluator. So effeort should be taken to arrange the assets properly.

## The evaluator

The evaluator looks up ops (prime-ops and functions) by context interface, so we can expose the prime ops by the context API. Also the normal functions can also be exposed with a context API, and the evaluator looks up ops through a merged context.

We need 

## The parser

Parser can be generated in a single-pass fashion, it takes a list of all the ops, group them by priority and generate the parser.

### Expression Parser

[PosgreSQL operator precedence and association](https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-PRECEDENCE-TABLE)

A hidden structure is that for a specific priority of oeprators, it's association and arity is fixed. This helped a lot in writing the parser. Actually, I successufuly wrote my parser after I observed this hidden information.