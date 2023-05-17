open Core

type loc = {
  lnum : int;
  cnum : int;
} [@@deriving sexp]


let loc_of_position {Lexing.pos_lnum; pos_cnum; pos_bol; _} =
  { lnum = pos_lnum; cnum = pos_cnum - pos_bol + 1 }

let string_of_loc {lnum; cnum} = sprintf "l:%d, c:%d" lnum cnum

module type ID = sig
  type t [@@deriving sexp]

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
  val compare: t -> t -> int
end

module String_id = struct
  type t = string [@@deriving sexp]

  let of_string x = x
  let to_string x = x
  let ( = ) = String.( = )
  let compare = String.compare
end

module Var_name : ID = String_id [@@deriving sexp]
(* module Class_name : ID = String_id [@@deriving sexp] *)
module Capability_name : ID = String_id [@@deriving sexp]
module Field_name : ID = String_id [@@deriving sexp]
module Method_name : ID = String_id [@@deriving sexp]
module Function_name : ID = String_id [@@deriving sexp]
module Struct_name : ID = String_id [@@deriving sexp]
module Interface_name : ID = String_id [@@deriving sexp]

type mode =
  | Linear
  | ThreadLocal
  | Read
  | Locked
  | ThreadSafe
  | Subordinate
  | Encapsulated
[@@deriving sexp]
let string_of_mode = function
  | Linear       -> "Linear"
  | ThreadLocal  -> "ThreadLocal"
  | Read         -> "Read"
  | Locked       -> "Locked"
  | ThreadSafe   -> "ThreadSafe"
  | Subordinate  -> "Subordinate"
  | Encapsulated -> "Encapsulated"
[@@deriving sexp]

type modifier = MConst | MVar [@@deriving sexp]
let string_of_modifier = function MConst -> "Const" | MVar -> "Var" [@@deriving sexp]

type borrowed_ref = Borrowed [@@deriving sexp]
let string_of_maybe_borrowed_ref = function Some Borrowed -> "Borrowed " | None -> "" [@@deriving sexp]

type generic_type = Generic [@@deriving sexp]
let string_of_maybe_generic = function Some Generic -> "<T>" | None -> "" [@@deriving sexp]

type type_expr =
  | TEInt
  | TEStruct of Struct_name.t * type_expr option
  | TEVoid
  | TEBool
  | TEGeneric
  [@@deriving sexp]
let rec string_of_type = function
| TEInt -> "Int"
| TEStruct (class_name, maybe_type_param) ->
  let maybe_type_param_str =
    match maybe_type_param with
    | Some type_param -> sprintf "<%s>" (string_of_type type_param)
    | None -> ""
  in
    sprintf "%s%s" (Struct_name.to_string class_name) maybe_type_param_str
    | TEVoid -> "Void"
    | TEBool -> "Bool"
    | TEGeneric -> "T"
    [@@deriving sexp]
      
type field_defn = TField of modifier * type_expr * Field_name.t * Capability_name.t list [@@deriving sexp]
type capability = TCapability of mode * Capability_name.t [@@deriving sexp]  
let string_of_cap (TCapability (mode, cap_name)) =
  sprintf "%s %s" (string_of_mode mode) (Capability_name.to_string cap_name)
  [@@deriving sexp]
        
  type param =
  | Param of type_expr * Var_name.t * Capability_name.t list option * borrowed_ref option
[@@deriving sexp]
let get_params_types params =
  List.map ~f:(fun (Param (param_type, _,_,_)) -> param_type) params
  [@@deriving sexp]
  
  type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpIntDiv
  | BinOpRem
  | BinOpLessThan
  | BinOpLessThanEq
  | BinOpGreaterThan
  | BinOpGreaterThanEq
  | BinOpAnd
  | BinOpOr
  | BinOpEq
  | BinOpNotEq
  [@@deriving sexp]
          
let string_of_bin_op = function
| BinOpPlus          -> "+"
| BinOpMinus         -> "-"
| BinOpMult          -> "*"
| BinOpIntDiv        -> "/"
| BinOpRem           -> "%"
| BinOpLessThan      -> "<"
| BinOpLessThanEq    -> "<="
| BinOpGreaterThan   -> ">"
| BinOpGreaterThanEq -> ">="
| BinOpAnd           -> "&&"
| BinOpOr            -> "||"
| BinOpEq            -> "=="
| BinOpNotEq         -> "!="

type un_op = UnOpNot | UnOpNeg [@@deriving sexp]
          
let string_of_un_op = function UnOpNot -> "!" | UnOpNeg -> "-"
[@@deriving sexp]          