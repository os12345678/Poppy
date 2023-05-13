open Core
open Poppy_parser.Lex_and_parse
(* open Poppy_codegen.Codegen *)
(* open Poppy_codegen.Codegen_util *)
(* open Poppy_type_checker *)
(* open Llvm *)
(* open Parser_interface *)


exception ParseError of string [@@ocaml.warning "-38"]

(* Read the input file *)
let read_file filename =
  In_channel.read_all filename

  let main () =
    try
      (* Get the command-line arguments *)
      let argv = Sys.get_argv () in
      let filename = argv.(1) in
  
      if not (Filename.check_suffix filename ".poppy") then
        failwith "Invalid file type. Please provide a file with the .poppy extension.";
  
      (* Read the input file *)
      let input = read_file filename in
  
      (* Create a lexbuf from the input *)
      let lexbuf = Lexing.from_string input in
  
      (* Parse the input *)
      let ast_result = parse_program lexbuf in
  
      (* Handle the result of the parsing *)
      match ast_result with
      | Ok ast ->
          (* Convert the AST to a string and print it *)
          let ast_string = print_program ast in
          print_endline ast_string
      | Error err ->
          (* Print the error message *)
          print_endline (Error.to_string_hum err)
  
    with
    | Failure msg -> print_endline msg
    | _ -> print_endline "An error occurred"

let () = main ()