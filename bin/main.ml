open Core
(* open Lexer *)
(* open Lexing *)
(* open Parser *)
open Poppy_parser
open Poppy_type_checker
open Desugar
open Poppy_codegen.Ir_symbol_table
open Poppy_codegen.Codegen_expr
open Poppy_codegen.Codegen_util 
open Data_race_checker.Type_data_race_program
open Poppy_type_checker.Type_env


module St = Poppy_codegen.Ir_symbol_table
module Envir = Poppy_type_checker.Type_env

let is_poppy_file filename = 
  String.split_on_chars ~on:['.'] filename |>  List.last_exn |> String.equal "poppy"

(* let get_output_file filename = 
  String.substr_replace_all filename ~pattern:".poppy" ~with_:".ir" *)

let poppy_file =
  let error_not_file filename =
    eprintf "'%s' is not a Poppy file. Hint: use the .poppy extension\n%!" filename ;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match is_poppy_file filename with
      | true  -> filename
      | false -> error_not_file filename)

let compile_program ?(should_pprint_past = false) ?(should_pprint_tast = false) 
?(should_pprint_dast = false) ?(should_print_llvm_table = false) ?compile_out_file lexbuf =
  let open Result in 
  (* let the_execution_engine = Llvm_executionengine.create the_module in *)
  let the_fpm = Llvm.PassManager.create_function the_module in

  Lex_and_parse.parse_program lexbuf (* AST *)
  >>= fun ast ->
  (if should_pprint_past then
      print_endline (Sexp.to_string_hum (Ast.sexp_of_program ast)));
  Type_program.type_program ast (* Typed AST *)
  >>= fun typed_ast ->
  (if should_pprint_tast then
      print_endline (Sexp.to_string_hum (Typed_ast.sexp_of_program typed_ast)));
    let open Poppy_type_checker.Type_env in

  type_data_race_program typed_ast
  >>= fun _ ->
  Desugar_program.desugar_program typed_ast (* Desugared AST *)
  >>= fun dprogram ->
  (if should_pprint_dast then
      print_endline (Sexp.to_string_hum (Desugared_ast.sexp_of_dprogram dprogram)));
  Ok(build_symbol_table dprogram)
  >>= fun llvmsymboltable ->
    (if should_print_llvm_table then
      St.print_symbol_table llvmsymboltable);
  Ok(codegen_ast dprogram llvmsymboltable the_fpm)
  >>= fun llvm_module ->
  match compile_out_file with 
  | Some filename -> 
    Out_channel.with_file filename ~f:(fun file_oc ->
      let ir_program = Llvm.string_of_llmodule llvm_module in
      Out_channel.output_string file_oc ir_program;
      Ok ())
  | None ->
    let ir_program = Llvm.string_of_llmodule llvm_module in
    print_endline ir_program;
    Ok ()
      
let command =
  Command.basic ~summary:"Run Poppy programs"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open should_pprint_past =
        flag "-printpast" no_arg ~doc:" Pretty print the parsed AST of the program"
      and should_pprint_tast =
        flag "-printtast" no_arg ~doc:" Pretty print the typed AST of the program"
      and should_pprint_dast = 
        flag "-printdast" no_arg ~doc:" Pretty print the desugared AST of the program"
      and should_print_llvm_table = 
        flag "-printsym" no_arg ~doc:" Pretty print the llvm symbol table"
      and filename = anon (maybe_with_default "-" ("filename" %: poppy_file)) in
      fun () ->
        In_channel.with_file filename ~f:(fun file_ic ->
            let lexbuf = Lexing.from_channel file_ic in
            match compile_program ~should_pprint_past ~should_pprint_tast ~should_pprint_dast ~should_print_llvm_table lexbuf with
            | Ok _ -> () (* Success, do nothing *)
            | Error e -> (* Handle error here *)
              eprintf "Compilation error: %s\n" (Core.Error.to_string_hum e);
              exit 1
        )
    ) 

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command