open Core

type loc = {
  lnum : int;
  cnum : int;
} [@@deriving sexp_of]


let loc_of_position {Lexing.pos_lnum; pos_cnum; pos_bol; _} =
  { lnum = pos_lnum; cnum = pos_cnum - pos_bol + 1 }

module type ID = sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module String_id = struct
  type t = string [@@deriving sexp_of]

  let of_string x = x
  let to_string x = x
  let ( = ) = String.( = )
end

module Var_name : ID = String_id [@@deriving sexp_of]
module Class_name : ID = String_id [@@deriving sexp_of]
module Capability_name : ID = String_id [@@deriving sexp_of]
module Field_name : ID = String_id [@@deriving sexp_of]
module Method_name : ID = String_id [@@deriving sexp_of]
module Function_name : ID = String_id [@@deriving sexp_of]

type mode =
 | Linear
  [@@deriving sexp_of]

let string_of_mode = function
 | Linear       -> "Linear"
  [@@deriving sexp_of]

type modifier = MConst | MVar [@@deriving sexp_of]

let string_of_modifier = function MConst -> "Const" | MVar -> "Var" [@@deriving sexp_of]

type borrowed_ref = Borrowed [@@deriving sexp_of]

let string_of_maybe_borrowed_ref = function Some Borrowed -> "Borrowed " | None -> "" [@@deriving sexp_of]

type generic_type = Generic [@@deriving sexp_of]

let string_of_maybe_generic = function Some Generic -> "<T>" | None -> "" [@@deriving sexp_of]

type type_expr =
  | TEInt
  | TEClass   of Class_name.t * type_expr option  (** optionally specify type parameters *)
  | TEVoid
  | TEBool
  | TEGeneric
  [@@deriving sexp_of]

let rec string_of_type = function
  | TEInt -> "Int"
  | TEClass (class_name, maybe_type_param) ->
      let maybe_type_param_str =
        match maybe_type_param with
        | Some type_param -> sprintf "<%s>" (string_of_type type_param)
        | None            -> "" in
      sprintf "%s%s" (Class_name.to_string class_name) maybe_type_param_str
  | TEVoid -> "Void"
  | TEBool -> "Bool"
  | TEGeneric -> "T"
  [@@deriving sexp_of]

type field_defn = TField of modifier * type_expr * Field_name.t * Capability_name.t list [@@deriving sexp_of]

type capability = TCapability of mode * Capability_name.t [@@deriving sexp_of]
  
let string_of_cap (TCapability (mode, cap_name)) =
  sprintf "%s %s" (string_of_mode mode) (Capability_name.to_string cap_name)
  [@@deriving sexp_of]

type param = (*type_expr * Var_name.t * Capability_name.t list option * borrowed_ref option*)
  | Param of type_expr * Var_name.t [@@deriving sexp_of]

let get_params_types params =
  List.map ~f:(fun (Param (param_type, _)) -> param_type) params
  [@@deriving sexp_of]

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
[@@deriving sexp_of]

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

type un_op = UnOpNot | UnOpNeg [@@deriving sexp_of]

let string_of_un_op = function UnOpNot -> "!" | UnOpNeg -> "-"
[@@deriving sexp_of]





