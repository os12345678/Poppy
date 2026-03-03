open Core
open Poppy_parser
open Poppy_type_checker
open Desugar
open Poppy_codegen
open Poppy_codegen.Codegen_expr
open Poppy_codegen.Codegen_util
open Data_race_checker.Type_data_race_program

let is_poppy_file filename =
  String.split_on_chars ~on:['.'] filename |>  List.last_exn |> String.equal "poppy"

let poppy_file =
  let error_not_file filename =
    eprintf "'%s' is not a Poppy file. Hint: use the .poppy extension\n%!" filename ;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match is_poppy_file filename with
      | true  -> filename
      | false -> error_not_file filename)

(* Link an external LLVM IR module (e.g. core_lib/bindings.ll) into the generated module *)
let link_bindings_module llvm_module bindings_path =
  let membuf = Llvm.MemoryBuffer.of_file bindings_path in
  let context = Llvm.module_context llvm_module in
  let bindings_mod = Llvm_irreader.parse_ir context membuf in
  Llvm_linker.link_modules' llvm_module bindings_mod

let compile_program ?(should_pprint_past = false) ?(should_pprint_tast = false)
?(should_pprint_dast = false) ?(ignore_data_races = false) ?(should_print_llvm_table = false)
?bindings_path ?compile_out_file lexbuf =
  let open Result in
  let the_fpm = Llvm.PassManager.create_function the_module in

  Lex_and_parse.parse_program lexbuf (* AST *)
  >>= fun ast ->
  (if should_pprint_past then
      print_endline (Sexp.to_string_hum (Ast.sexp_of_program ast)));

  Type_program.type_program ast (* Typed AST *)
  >>= fun (env, typed_ast) ->
  (if should_pprint_tast then
      print_endline (Sexp.to_string_hum (Typed_ast.sexp_of_program typed_ast)));

  type_data_race_program env typed_ast ~ignore_data_races (* Data-race checker *)
  >>= fun _ ->

  Desugar_program.desugar_program typed_ast (* Desugared AST *)
  >>= fun dprogram ->
  (if should_pprint_dast then
      print_endline (Sexp.to_string_hum (Desugared_ast.sexp_of_dprogram dprogram)));

  Ok(Ir_symbol_table.build_symbol_table dprogram) (* Build LLVM Symbol Table *)
  >>= fun llvmsymboltable ->
    (if should_print_llvm_table then
      Ir_symbol_table.print_symbol_table llvmsymboltable);

  Ok(codegen_ast dprogram llvmsymboltable the_fpm) (* Generate LLVM IR *)
  >>= fun llvm_module ->

  (* Link the bindings module into the generated module if a path is provided.
     This merges the C runtime (print, GC_malloc, thread_create, thread_join, etc.)
     into the generated module so external declarations are resolved. *)
  (match bindings_path with
   | Some path -> link_bindings_module llvm_module path
   | None -> ());

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
      and ignore_data_races =
        flag "-ignore-data-races" no_arg ~doc:" Ignore data races"
      and compile_out_file =
        flag "-o" (optional string) ~doc:"FILE Write LLVM IR output to a file"
      and bindings_path =
        flag "-link" (optional string) ~doc:"FILE Link an external LLVM IR module (e.g. core_lib/bindings.ll)"
      and filename = anon (maybe_with_default "-" ("filename" %: poppy_file)) in
      fun () ->
        In_channel.with_file filename ~f:(fun file_ic ->
            let lexbuf = Lexing.from_channel file_ic in
            match compile_program ~should_pprint_past ~should_pprint_tast ~should_pprint_dast ~ignore_data_races ~should_print_llvm_table ?bindings_path ?compile_out_file lexbuf with
            | Ok _ -> ()
            | Error e ->
              eprintf "Compilation error: %s\n" (Core.Error.to_string_hum e);
              exit 1
        )
    )

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
