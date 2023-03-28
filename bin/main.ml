(* bin/main.ml *)

(* Import the required modules *)
open Core
open Poppy

(* Read the input file *)
let read_file filename =
  In_channel.read_all filename

(* Parse the input *)
let parse_input input filename =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Poppy.Parser.main Poppy.Lexer.read_tok lexbuf

(* Entry point *)
let main () =
  try
    (* Get the command-line arguments *)
    let argv = Sys.get_argv () in
    let filename = argv.(1) in

    (* Read the input file *)
    let input = read_file filename in

    (* Parse the input *)
    let ast = parse_input input filename in

    (* Print the parsed AST *)
    print_endline (Sexp.to_string_hum (Ast.sexp_of_statement ast))
  with
  | Sys_error msg -> print_endline ("Error: " ^ msg)
  | Failure msg -> print_endline ("Syntax error: " ^ msg)
  | Poppy.Parser.Error ->
      let curr = (Lexing.from_string "").lex_curr_p in
      let line = curr.pos_lnum in
      let cnum = curr.pos_cnum - curr.pos_bol + 1 in
      Printf.printf "Syntax error at line %d, column %d\n" line cnum

(* Execute the main function *)
let () = main ()
