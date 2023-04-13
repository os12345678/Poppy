exception ParseError of string
val read_file : string -> string
val parse_input : string -> string -> Poppy_parser.Ast.statement list
val main : unit -> unit
