open Core
open Poppy_parser
open Poppy_codegen
open Llvm


exception ParseError of string [@@ocaml.warning "-38"]

(* Read the input file *)
let read_file filename =
  In_channel.read_all filename

(* Parse the input *)
let parse_input input filename =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    Parser.main Lexer.read_tok lexbuf
  with
  | Parser.Error ->
      let curr = lexbuf.lex_curr_p in
      let line = curr.pos_lnum in
      let cnum = curr.pos_cnum - curr.pos_bol + 1 in
      Printf.printf "Syntax error at line %d, column %d\n" line cnum;
      raise Parser.Error


let main () =
  try
    (* Get the command-line arguments *)
    let argv = Sys.get_argv () in
    let filename = argv.(1) in

    if not (Filename.check_suffix filename ".poppy") then
      failwith "Invalid file type. Please provide a file with the .poppy extension.";

    (* Read the input file *)
    let input = read_file filename in

    (* Parse the input *)
    let ast = parse_input input filename in

    (* Print the parsed AST *)
    print_endline (Sexp.to_string_hum (Ast.sexp_of_statements ast));

    (* Codegen *)
    let codegen_module = Codegen.the_module in
    List.iter ~f:(fun statement ->
      match statement with
      | Ast.Expr expr ->
        let _ = Codegen.codegen_expr expr in
        ()
      | Ast.FuncDecl _ ->
        let _ = Codegen.codegen_statement statement in
        ()
      | _ -> ()
    ) ast;
    

    (* Print the LLVM IR *)
    print_endline (string_of_llmodule codegen_module)
  with
  | Sys_error msg -> print_endline ("Error: " ^ msg)
  | Failure msg -> print_endline ("Syntax Error: " ^ msg)
  | ParseError msg -> print_endline ("ParseError: " ^ msg)

let () = main ()