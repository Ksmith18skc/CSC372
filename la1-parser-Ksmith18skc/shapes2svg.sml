(* shapes2svg.sml *)
(* Usage:
    cat la1-parse.sml shapes2svg.sml > compiler.sml
    poly --script compiler.sml < infile.shapes > infile-out.svg
*)

(* Main entry point *)
val () =
  let
    val infile_as_str = TextIO.inputAll TextIO.stdIn
    val tokens = lexer infile_as_str
    val ast = parser tokens
    val outfile_as_str = svgGen ast
  in
    TextIO.output (TextIO.stdOut, outfile_as_str)
  end
