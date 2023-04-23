open Core
open Poppy_parser
open Poppy_codegen
open Poppy_type_checker
open Llvm
open Parser_interface


exception ParseError of string [@@ocaml.warning "-38"]

(* Read the input file *)
let read_file filename =
  In_channel.read_all filename

let main () =
  try
    (* Link the core library to the main module *)
    let codegen_module = Codegen.the_module in
    Codegen.link_core_library codegen_module;
    print_endline("Linking successful");

    (* Get the command-line arguments *)
    let argv = Sys.get_argv () in
    let filename = argv.(1) in

    if not (Filename.check_suffix filename ".poppy") then
      failwith "Invalid file type. Please provide a file with the .poppy extension.";

    (* Read the input file *)
    let input = read_file filename in

    (* Parse the input *)
    let ast = parse_input input in

    (* Print the parsed AST *)
    print_endline (Sexp.to_string_hum (Ast.sexp_of_statements ast));

    (* Type check the AST *)
    Type_check.check_program ast;
    print_endline "Type checking successful";
    

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