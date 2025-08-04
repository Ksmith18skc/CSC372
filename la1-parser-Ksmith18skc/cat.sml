(* la1-parse.sml *)
(* Name: Kory Smith *)
(* Time spent on LA1: ~10 hours *)

(* Collaborators and references: [ChatGPT, CoPilot, Stackoverflow, Youtube] *)

use "Unit.sml"; (* for Unit.listString *)

exception Fail of string

(* Token datatype *)
datatype token =
    TokenCIRCLE
  | TokenLINE
  | TokenRECTANGLE
  | TokenNUM of int
  | TokenCOLOR of string
  | TokenEOF
  | TokenUNKNOWN

(* Convert a token to its string representation *)
fun tokenToString t = 
  case t of 
      TokenCIRCLE => "TokenCIRCLE"
    | TokenLINE => "TokenLINE"
    | TokenRECTANGLE => "TokenRECTANGLE"
    | TokenNUM n => "TokenNUM(" ^ Int.toString n ^ ")"
    | TokenCOLOR clr => "TokenCOLOR(" ^ clr ^ ")"
    | TokenEOF => "TokenEOF"
    | TokenUNKNOWN => "TokenUNKNOWN"

(* Convert a string into a token *)
fun stringToToken s = 
  case Int.fromString s of 
      SOME n => if s = "0" orelse String.sub(s, 0) <> #"0" then TokenNUM n
                else raise Fail ("Lexer error: invalid number " ^ s)
    | NONE => (case s of 
          "CIRCLE" => TokenCIRCLE
        | "LINE" => TokenLINE
        | "RECTANGLE" => TokenRECTANGLE
        | "red" => TokenCOLOR "red"
        | "blue" => TokenCOLOR "blue"
        | "green" => TokenCOLOR "green"
        | "yellow" => TokenCOLOR "yellow"
        | "black" => TokenCOLOR "black"
        | "white" => TokenCOLOR "white"
        | _ => raise Fail ("Lexer error: unknown token " ^ s))
        
(* Lexer function *)
fun lexer input_str =
  let
    val words = String.tokens (fn c => Char.isSpace c) input_str
    fun checkToken token =
      case token of
          TokenUNKNOWN => raise Fail ("Lexer error: unknown token " ^ tokenToString token)
        | _ => token
  in
    List.map (fn word => checkToken (stringToToken word)) words @ [TokenEOF]
  end;

(* Abstract Syntax Tree (AST) datatype *)
datatype ast =
    Program of ast list
  | StmtCircle of int * int * int * string
  | StmtLine of int * int * int * int * string
  | StmtRectangle of int * int * int * int * string

(* Convert AST to string *)
fun astToString (Program stmts) = 
      "Program [" ^ String.concatWith ", " (List.map astToString stmts) ^ "]"
  | astToString (StmtCircle (x, y, r, c)) = 
      "StmtCircle (" ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString r ^ ", \"" ^ c ^ "\")"
  | astToString (StmtLine (x1, y1, x2, y2, c)) = 
      "StmtLine (" ^ Int.toString x1 ^ ", " ^ Int.toString y1 ^ ", " ^ Int.toString x2 ^ ", " ^ Int.toString y2 ^ ", \"" ^ c ^ "\")"
  | astToString (StmtRectangle (x1, y1, x2, y2, c)) = 
      "StmtRectangle (" ^ Int.toString x1 ^ ", " ^ Int.toString y1 ^ ", " ^ Int.toString x2 ^ ", " ^ Int.toString y2 ^ ", \"" ^ c ^ "\")"

(* Match expected token *)
fun match tokens expect =
  case tokens of
    [] => raise Fail ("Parse error: expected " ^ tokenToString expect ^ ", but got empty list")
  | given :: ts =>
      if given = expect then ts
      else raise Fail ("Parse error: expected " ^ tokenToString expect ^ ", but found " ^ tokenToString given)

(* Parsing Statements *)
fun parseStmt (TokenCIRCLE :: TokenNUM x :: TokenNUM y :: TokenNUM r :: TokenCOLOR c :: ts) =
      (StmtCircle (x, y, r, c), ts)
  | parseStmt (TokenLINE :: TokenNUM x1 :: TokenNUM y1 :: TokenNUM x2 :: TokenNUM y2 :: TokenCOLOR c :: ts) =
      (StmtLine (x1, y1, x2, y2, c), ts)
  | parseStmt (TokenRECTANGLE :: TokenNUM x1 :: TokenNUM y1 :: TokenNUM x2 :: TokenNUM y2 :: TokenCOLOR c :: ts) =
      (StmtRectangle (x1, y1, x2, y2, c), ts)
  | parseStmt (tok::ts) = raise Fail ("Parse error in parseStmt: unexpected token " ^ tokenToString tok)
  | parseStmt [] = raise Fail "Parse error in parseStmt: no tokens left"

(* Parsing multiple statements *)
fun parseStmts (TokenEOF :: ts) = ([], TokenEOF :: ts)
  | parseStmts [] = ([], []) (* Explicit base case *)
  | parseStmts ts =
    let
      val (stmt, ts1) = parseStmt ts
      val (stmts, ts2) = parseStmts ts1
    in
      (stmt :: stmts, ts2)
    end

(* Parsing entire program *)
fun parseProgram ts =
  let
    val (stmts, ts1) = parseStmts ts
    val _ = match ts1 TokenEOF
  in
    Program stmts
  end

(* Recursive descent parser *)
fun parser tokens = parseProgram tokens

(* SVG Code Generation *)
fun svgCircle (cx, cy, r, fill) =
    "<circle cx=\"" ^ Int.toString cx ^
    "\" cy=\"" ^ Int.toString cy ^
    "\" r=\"" ^ Int.toString r ^
    "\" fill=\"" ^ fill ^ "\" />"

fun svgLine (x1, y1, x2, y2, stroke) =
    "<line x1=\"" ^ Int.toString x1 ^
    "\" y1=\"" ^ Int.toString y1 ^
    "\" x2=\"" ^ Int.toString x2 ^
    "\" y2=\"" ^ Int.toString y2 ^
    "\" stroke=\"" ^ stroke ^ "\" />"

fun svgRectangle (x1, y1, x2, y2, fill) =
    "<rect x=\"" ^ Int.toString x1 ^
    "\" y=\"" ^ Int.toString y1 ^
    "\" width=\"" ^ Int.toString (abs (x2 - x1)) ^
    "\" height=\"" ^ Int.toString (abs (y2 - y1)) ^
    "\" fill=\"" ^ fill ^ "\" />"

(* Code generation function *)
fun svgGen (Program stmts) = 
      "<svg xmlns=\"http://www.w3.org/2000/svg\">\n"
      ^ String.concatWith "\n" (List.map svgGen stmts)
      ^ "\n</svg>\n"
  | svgGen (StmtCircle (cx, cy, r, fill)) = svgCircle (cx, cy, r, fill)
  | svgGen (StmtLine (x1, y1, x2, y2, stroke)) = svgLine (x1, y1, x2, y2, stroke)
  | svgGen (StmtRectangle (x1, y1, x2, y2, fill)) = svgRectangle (x1, y1, x2, y2, fill)
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