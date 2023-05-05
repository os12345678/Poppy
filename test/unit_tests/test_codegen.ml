(* open Poppy_codegen.Codegen
open Poppy_parser.Parser_interface


let%expect_test "codegen_fn_application" = 
let input = "
fn mult(b: int) -> bool {
  let res:bool = False;
    if (b == 0) {
        res = True;
    } else {
      res = False;
    }
  return res;
}
fn main() -> int {
  let y:bool = mult(3);
}" in
let ast = parse_input input in
let llvm_ir_string = codegen_ast_to_string ast in
print_endline (llvm_ir_string);
[%expect{||}]

 *)
