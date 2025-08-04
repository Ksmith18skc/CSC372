(* sample unit tests for LA1 *)
(* Usage:
    cat la1-parse.sml la1-tests.sml > cat.sml
    poly --script cat.sml
*)

(**** Tests for lexer function ****)

val () =
  Unit.checkExpectWith (Unit.listString tokenToString)
  "lexer 'RECTANGLE 20 20 300 250 red'"
  (fn () => lexer "RECTANGLE 20 20 300 250 red")
  [TokenRECTANGLE, TokenNUM 20, TokenNUM 20, TokenNUM 300,
   TokenNUM 250, TokenCOLOR "red", TokenEOF]

(* Additional test *)
val () =
  Unit.checkExpectWith (Unit.listString tokenToString)
  "lexer 'CIRCLE 50 50 100 blue'"
  (fn () => lexer "CIRCLE 50 50 100 blue")
  [TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100,
   TokenCOLOR "blue", TokenEOF]

(* New tests *)

val () =
  Unit.checkExpectWith (Unit.listString tokenToString)
  "lexer 'LINE 0 0 100 100 black'"
  (fn () => lexer "LINE 0 0 100 100 black")
  [TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100,
   TokenNUM 100, TokenCOLOR "black", TokenEOF]

val () =
    Unit.checkExpectWith (Unit.listString tokenToString)
    "rectangle 'RECTANGLE 0 0 500 500 green'"
    (fn () => lexer "RECTANGLE 0 0 500 500 green")
    [TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500,
     TokenNUM 500, TokenCOLOR "green", TokenEOF]

(*Parse tests*)
(*Parse a list of statements, returns a tuple containing a list of parsed statements (AST nodes) and the remaining list of tokens*)
fun astTupleToString (stmt, tokens) =
  "(" ^ astToString stmt ^ ", " ^ Unit.listString tokenToString tokens ^ ")"

val () = 
    Unit.checkExpectWith astTupleToString
    "parseStmt [TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100, TokenCOLOR 'blue', TokenEOF]"
    (fn () => parseStmt [TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100, TokenCOLOR "blue", TokenEOF])
    (StmtCircle (50, 50, 100, "blue"), [TokenEOF])

val () = 
    Unit.checkExpectWith astTupleToString
    "parseStmt [TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100, TokenNUM 100, TokenCOLOR 'black', TokenEOF]"
    (fn () => parseStmt [TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100, TokenNUM 100, TokenCOLOR "black", TokenEOF])
    (StmtLine (0, 0, 100, 100, "black"), [TokenEOF])

val () =
    Unit.checkExpectWith astTupleToString
    "parseStmt [TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500, TokenNUM 500, TokenCOLOR 'green', TokenEOF]"
    (fn () => parseStmt [TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500, TokenNUM 500, TokenCOLOR "green", TokenEOF])
    (StmtRectangle (0, 0, 500, 500, "green"), [TokenEOF])

(*parsStmts tests*)

fun astListTupleToString (stmts, tokens) =
  "(" ^ Unit.listString astToString stmts ^ ", " ^ Unit.listString tokenToString tokens ^ ")"


val () =
    Unit.checkExpectWith astListTupleToString
    "parseStmts [TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100, TokenCOLOR 'blue', TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100, TokenNUM 100, TokenCOLOR 'black', TokenEOF]"
    (fn () => parseStmts [TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100, TokenCOLOR "blue", TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100, TokenNUM 100, TokenCOLOR "black", TokenEOF])
    ([StmtCircle (50, 50, 100, "blue"), StmtLine (0, 0, 100, 100, "black")], [TokenEOF])

val () =
    Unit.checkExpectWith astListTupleToString
    "parseStmts [TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500, TokenNUM 500, TokenCOLOR 'green', TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100, TokenCOLOR 'blue', TokenEOF]"
    (fn () => parseStmts [TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500, TokenNUM 500, TokenCOLOR "green", TokenCIRCLE, TokenNUM 50, TokenNUM 50, TokenNUM 100, TokenCOLOR "blue", TokenEOF])
    ([StmtRectangle (0, 0, 500, 500, "green"), StmtCircle (50, 50, 100, "blue")], [TokenEOF])

val () =
    Unit.checkExpectWith astListTupleToString
    "parseStmts [TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100, TokenNUM 100, TokenCOLOR 'black', TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500, TokenNUM 500, TokenCOLOR 'green', TokenEOF]"
    (fn () => parseStmts [TokenLINE, TokenNUM 0, TokenNUM 0, TokenNUM 100, TokenNUM 100, TokenCOLOR "black", TokenRECTANGLE, TokenNUM 0, TokenNUM 0, TokenNUM 500, TokenNUM 500, TokenCOLOR "green", TokenEOF])
    ([StmtLine (0, 0, 100, 100, "black"), StmtRectangle (0, 0, 500, 500, "green")], [TokenEOF])



val () = Unit.report()
val () = Unit.reportWhenFailures () (* put me at the _end_ *)