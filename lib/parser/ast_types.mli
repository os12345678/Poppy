type loc = { lnum : int; cnum : int; }
val loc_of_sexp : Sexplib0.Sexp.t -> loc
val sexp_of_loc : loc -> Sexplib0.Sexp.t
val loc_of_position : Lexing.position -> loc
val string_of_loc : loc -> string
module type ID =
  sig
    type t
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end
module String_id :
  sig
    type t = string
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val of_string : 'a -> 'a
    val to_string : 'a -> 'a
    val ( = ) : string -> string -> bool
    val compare : string -> string -> int
    val hash : 'a -> int
  end
module Var_name : ID
module Capability_name : ID
module Field_name : ID
module Method_name : ID
module Function_name : ID
module Struct_name : ID
module Trait_name : ID
type mode =
    Linear
  | ThreadLocal
  | Read
  | Locked
  | ThreadSafe
  | Subordinate
  | Encapsulated
val mode_of_sexp : Sexplib0.Sexp.t -> mode
val sexp_of_mode : mode -> Sexplib0.Sexp.t
val string_of_mode : mode -> string
type modifier = MConst | MVar
val modifier_of_sexp : Sexplib0.Sexp.t -> modifier
val sexp_of_modifier : modifier -> Sexplib0.Sexp.t
val string_of_modifier : modifier -> string
type borrowed_ref = Borrowed
val borrowed_ref_of_sexp : Sexplib0.Sexp.t -> borrowed_ref
val sexp_of_borrowed_ref : borrowed_ref -> Sexplib0.Sexp.t
val string_of_maybe_borrowed_ref : borrowed_ref option -> string
type type_expr = TEInt | TEStruct of Struct_name.t | TEVoid | TEBool
val type_expr_of_sexp : Sexplib0.Sexp.t -> type_expr
val sexp_of_type_expr : type_expr -> Sexplib0.Sexp.t
val string_of_type : type_expr -> string
type field_defn =
    TField of modifier * type_expr * Field_name.t * Capability_name.t list
val field_defn_of_sexp : Sexplib0.Sexp.t -> field_defn
val sexp_of_field_defn : field_defn -> Sexplib0.Sexp.t
type capability = TCapability of mode * Capability_name.t
val capability_of_sexp : Sexplib0.Sexp.t -> capability
val sexp_of_capability : capability -> Sexplib0.Sexp.t
val string_of_cap : capability -> string
type param =
    Param of type_expr * Var_name.t * Capability_name.t list option *
      borrowed_ref option
val param_of_sexp : Sexplib0.Sexp.t -> param
val sexp_of_param : param -> Sexplib0.Sexp.t
val get_params_types : param list -> type_expr list
type bin_op =
    BinOpPlus
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
val bin_op_of_sexp : Sexplib0.Sexp.t -> bin_op
val sexp_of_bin_op : bin_op -> Sexplib0.Sexp.t
val string_of_bin_op : bin_op -> string
type un_op = UnOpNot | UnOpNeg
val un_op_of_sexp : Sexplib0.Sexp.t -> un_op
val sexp_of_un_op : un_op -> Sexplib0.Sexp.t
val string_of_un_op : un_op -> string
type function_signature = {
  name : Function_name.t;
  borrowed : borrowed_ref option;
  return_type : type_expr;
  params : param list;
}
val function_signature_of_sexp : Sexplib0.Sexp.t -> function_signature
val sexp_of_function_signature : function_signature -> Sexplib0.Sexp.t
type method_signature = {
  name : Method_name.t;
  borrowed : borrowed_ref option;
  capability : Capability_name.t list;
  params : param list;
  return_type : type_expr;
}
val method_signature_of_sexp : Sexplib0.Sexp.t -> method_signature
val sexp_of_method_signature : method_signature -> Sexplib0.Sexp.t
