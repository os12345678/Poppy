open Core
(* open Lexer *)
(* open Lexing *)
(* open Parser *)
open Poppy_parser
open Poppy_type_checker
(* open Core_unix *)

let is_poppy_file filename = 
  String.split_on_chars ~on:['.'] filename |>  List.last_exn |> String.equal "poppy"

(* let get_output_file filename = 
  String.substr_replace_all filename ~pattern:".poppy" ~with_:".ir" *)

  (* let maybe_pprint_ast should_pprint p_print ast =
    if should_pprint then
      ast
      |> p_print
      |> Sexp.to_string_hum
      |> print_endline;
    Result.return ast   *)

let poppy_file =
  let error_not_file filename =
    eprintf "'%s' is not a Poppy file. Hint: use the .poppy extension\n%!" filename ;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match is_poppy_file filename with
      | true  -> filename
      | false -> error_not_file filename)

let compile_program ?(should_pprint_past = false) ?(should_pprint_tast = false)
?compile_out_file lexbuf =
  let open Result in 
  Lex_and_parse.parse_program lexbuf 
  >>= fun ast ->
  (if should_pprint_past then
      print_endline (Sexp.to_string_hum (Ast.sexp_of_program ast)));
  Type_program.type_program ast
  >>= fun typed_ast ->
  (if should_pprint_tast then
      print_endline (Sexp.to_string_hum (Typed_ast.sexp_of_program typed_ast)));
  match compile_out_file with 
  | Some filename -> 
    Out_channel.with_file filename ~f:(fun file_oc ->
      (* Placeholder for IR generation *)
      let ir_program = "IR generation not implemented yet" in
      Out_channel.output_string file_oc ir_program;
      Ok ())
  | None ->
    (* Placeholder for IR generation *)
    let ir_program = "IR generation not implemented yet" in
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
      and filename = anon (maybe_with_default "-" ("filename" %: poppy_file)) in
      fun () ->
        In_channel.with_file filename ~f:(fun file_ic ->
            let lexbuf = Lexing.from_channel file_ic in
            match compile_program ~should_pprint_past ~should_pprint_tast lexbuf with
            | Ok _ -> () (* Success, do nothing *)
            | Error e -> (* Handle error here *)
              eprintf "Compilation error: %s\n" (Core.Error.to_string_hum e);
              exit 1
        )
    ) 

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command