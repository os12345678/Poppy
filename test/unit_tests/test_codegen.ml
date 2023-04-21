open Poppy_codegen
open Poppy_parser

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
  mult(3);
}" in
let ast = Parser_interface.parse_input input in
let llvm_ir_string = Codegen.codegen_ast_to_string ast in
print_endline (llvm_ir_string);
[%expect{|
  ; ModuleID = 'poppy_compiler'
  source_filename = "poppy_compiler"

  define i1 @mult(i64 %b) {
  entry:
    %res = alloca i1, align 1
    store i1 false, i1* %res, align 1
    %cmptmp = icmp eq i64 %b, 0
    %ifcond = icmp ne i1 %cmptmp, false
    br i1 %ifcond, label %then, label %else

  then:                                             ; preds = %entry
    store i1 true, i1* %res, align 1
    br label %ifcont

  else:                                             ; preds = %entry
    store i1 false, i1* %res, align 1
    br label %ifcont

  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i1 [ true, %then ], [ false, %else ]
    ret i1* %res
  }

  define i64 @main() {
  entry:
    %calltmp = call i1 @mult(i64 3)
    ret i64 0
  } |}]


