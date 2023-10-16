module VarNameMap :
  sig
    module Key :
      sig
        type t = Poppy_parser.Ast_types.Var_name.t
        val t_of_sexp : Sexplib0.Sexp.t -> t
        val sexp_of_t : t -> Sexplib0.Sexp.t
        type comparator_witness =
            Core.Map.Make(Poppy_parser.Ast_types.Var_name).Key.comparator_witness
        val comparator : (t, comparator_witness) Core.Comparator.comparator
      end
    type 'a t = (Key.t, 'a, Key.comparator_witness) Base.Map.t
    val compare :
      'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare ->
      'a t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val empty : 'a t
    val singleton : Key.t -> 'a -> 'a t
    val map_keys :
      'v t -> f:(Key.t -> Key.t) -> [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val map_keys_exn : 'v t -> f:(Key.t -> Key.t) -> 'v t
    val of_alist :
      (Key.t * 'a) list -> [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_alist_or_error : (Key.t * 'a) list -> 'a t Base.Or_error.t
    val of_alist_exn : (Key.t * 'a) list -> 'a t
    val of_alist_multi : (Key.t * 'a) list -> 'a list t
    val of_alist_fold :
      (Key.t * 'a) list ->
      init:'weak1031 -> f:('weak1031 -> 'a -> 'weak1031) -> 'weak1031 t
    val of_alist_reduce : (Key.t * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val of_sorted_array : (Key.t * 'a) array -> 'a t Base.Or_error.t
    val of_sorted_array_unchecked : (Key.t * 'a) array -> 'a t
    val of_increasing_iterator_unchecked :
      len:int -> f:(int -> Key.t * 'weak1028) -> 'weak1028 t
    val of_increasing_sequence :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence :
      (Key.t * 'a) Base.Sequence.t ->
      [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_sequence_or_error :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence_exn : (Key.t * 'a) Base.Sequence.t -> 'a t
    val of_sequence_multi : (Key.t * 'a) Base.Sequence.t -> 'a list t
    val of_sequence_fold :
      (Key.t * 'a) Base.Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
    val of_sequence_reduce :
      (Key.t * 'a) Base.Sequence.t -> f:('a -> 'a -> 'a) -> 'a t
    val of_iteri :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) ->
      [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val of_iteri_exn :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) -> 'v t
    val of_tree :
      (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t -> 'a t
    val of_hashtbl_exn : (Key.t, 'a) Core.Hashtbl.t -> 'a t
    val of_key_set :
      (Key.t, Key.comparator_witness) Base.Set.t -> f:(Key.t -> 'v) -> 'v t
    val quickcheck_generator :
      Key.t Core.Quickcheck.Generator.t ->
      'a Core.Quickcheck.Generator.t -> 'a t Core.Quickcheck.Generator.t
    val invariants : 'a t -> bool
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val add : 'a t -> key:Key.t -> data:'a -> 'a t Base.Map.Or_duplicate.t
    val add_exn : 'a t -> key:Key.t -> data:'a -> 'a t
    val set : 'a t -> key:Key.t -> data:'a -> 'a t
    val add_multi : 'a list t -> key:Key.t -> data:'a -> 'a list t
    val remove_multi : 'a list t -> Key.t -> 'a list t
    val find_multi : 'a list t -> Key.t -> 'a list
    val change : 'a t -> Key.t -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> Key.t -> f:('a option -> 'a) -> 'a t
    val find : 'a t -> Key.t -> 'a option
    val find_exn : 'a t -> Key.t -> 'a
    val remove : 'a t -> Key.t -> 'a t
    val mem : 'a t -> Key.t -> bool
    val iter_keys : 'a t -> f:(Key.t -> unit) -> unit
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
    val iteri_until :
      'a t ->
      f:(key:Key.t -> data:'a -> Base.Map.Continue_or_stop.t) ->
      Base.Map.Finished_or_unfinished.t
    val iter2 :
      'a t ->
      'b t ->
      f:(key:Key.t -> data:('a, 'b) Base__Map_intf.Merge_element.t -> unit) ->
      unit
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b) -> 'b t
    val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold_until :
      'a t ->
      init:'acc ->
      f:(key:Key.t ->
         data:'a -> 'acc -> ('acc, 'final) Base.Container.Continue_or_stop.t) ->
      finish:('acc -> 'final) -> 'final
    val fold_right :
      'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold2 :
      'a t ->
      'b t ->
      init:'c ->
      f:(key:Key.t ->
         data:('a, 'b) Base__Map_intf.Merge_element.t -> 'c -> 'c) ->
      'c
    val filter_keys : 'a t -> f:(Key.t -> bool) -> 'a t
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val filteri : 'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    val filter_mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b option) -> 'b t
    val partition_mapi :
      'a t ->
      f:(key:Key.t -> data:'a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partition_map :
      'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partitioni_tf :
      'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t * 'a t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
    val combine_errors : 'a Base.Or_error.t t -> 'a t Base.Or_error.t
    val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> Key.t list
    val data : 'a t -> 'a list
    val to_alist :
      ?key_order:[ `Decreasing | `Increasing ] -> 'a t -> (Key.t * 'a) list
    val merge :
      'a t ->
      'b t ->
      f:(key:Key.t -> ('a, 'b) Base__Map_intf.Merge_element.t -> 'c option) ->
      'c t
    val merge_skewed :
      'v t -> 'v t -> combine:(key:Key.t -> 'v -> 'v -> 'v) -> 'v t
    val symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t Base.Sequence.t
    val fold_symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      init:'c ->
      f:('c -> (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t -> 'c) ->
      'c
    val min_elt : 'a t -> (Key.t * 'a) option
    val min_elt_exn : 'a t -> Key.t * 'a
    val max_elt : 'a t -> (Key.t * 'a) option
    val max_elt_exn : 'a t -> Key.t * 'a
    val for_all : 'a t -> f:('a -> bool) -> bool
    val for_alli : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
    val existsi : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val count : 'a t -> f:('a -> bool) -> int
    val counti : 'a t -> f:(key:Key.t -> data:'a -> bool) -> int
    val split : 'a t -> Key.t -> 'a t * (Key.t * 'a) option * 'a t
    val append :
      lower_part:'a t ->
      upper_part:'a t -> [ `Ok of 'a t | `Overlapping_key_ranges ]
    val subrange :
      'a t ->
      lower_bound:Key.t Base.Maybe_bound.t ->
      upper_bound:Key.t Base.Maybe_bound.t -> 'a t
    val fold_range_inclusive :
      'a t ->
      min:Key.t ->
      max:Key.t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val range_to_alist : 'a t -> min:Key.t -> max:Key.t -> (Key.t * 'a) list
    val closest_key :
      'a t ->
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than
      ] -> Key.t -> (Key.t * 'a) option
    val nth : 'a t -> int -> (Key.t * 'a) option
    val nth_exn : 'a t -> int -> Key.t * 'a
    val rank : 'a t -> Key.t -> int option
    val to_tree :
      'a t -> (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t
    val to_sequence :
      ?order:[ `Decreasing_key | `Increasing_key ] ->
      ?keys_greater_or_equal_to:Key.t ->
      ?keys_less_or_equal_to:Key.t -> 'a t -> (Key.t * 'a) Base.Sequence.t
    val binary_search :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'key -> int) ->
      Base.Binary_searchable.Which_target_by_key.t ->
      'key -> (Key.t * 'a) option
    val binary_search_segmented :
      'a t ->
      segment_of:(key:Key.t -> data:'a -> [ `Left | `Right ]) ->
      Base.Binary_searchable.Which_target_by_segment.t -> (Key.t * 'a) option
    val binary_search_subrange :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'bound -> int) ->
      lower_bound:'bound Base.Maybe_bound.t ->
      upper_bound:'bound Base.Maybe_bound.t -> 'a t
    val key_set : 'a t -> (Key.t, Key.comparator_witness) Base.Set.t
    val validate :
      name:(Key.t -> string) -> 'a Validate.check -> 'a t Validate.check
    val validatei :
      name:(Key.t -> string) ->
      (Key.t * 'a) Validate.check -> 'a t Validate.check
    val quickcheck_observer :
      Key.t Core.Quickcheck.Observer.t ->
      'v Core.Quickcheck.Observer.t -> 'v t Core.Quickcheck.Observer.t
    val quickcheck_shrinker :
      Key.t Core.Quickcheck.Shrinker.t ->
      'v Core.Quickcheck.Shrinker.t -> 'v t Core.Quickcheck.Shrinker.t
    module Provide_of_sexp :
      functor (Key : sig val t_of_sexp : Sexplib0.Sexp.t -> Key.t end) ->
        sig
          val t_of_sexp :
            (Sexplib0.Sexp.t -> 'a__002_) -> Sexplib0.Sexp.t -> 'a__002_ t
        end [@@ocaml.warning "-67"]
    module Provide_bin_io :
      functor
        (Key : sig
                 val bin_size_t : Key.t Bin_prot.Size.sizer
                 val bin_write_t : Key.t Bin_prot.Write.writer
                 val bin_read_t : Key.t Bin_prot.Read.reader
                 val __bin_read_t__ : (int -> Key.t) Bin_prot.Read.reader
                 val bin_shape_t : Bin_prot.Shape.t
                 val bin_writer_t : Key.t Bin_prot.Type_class.writer
                 val bin_reader_t : Key.t Bin_prot.Type_class.reader
                 val bin_t : Key.t Bin_prot.Type_class.t
               end)
        ->
        sig
          val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t
          val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1
          val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1
          val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1
          val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1
          val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer
          val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader
          val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t
        end [@@ocaml.warning "-67"]
    module Provide_hash :
      functor
        (Key : sig
                 val hash_fold_t :
                   Base.Hash.state -> Key.t -> Base.Hash.state
               end)
        ->
        sig
          val hash_fold_t :
            'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold ->
            'a t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
        end
    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end [@@ocaml.warning "-67"]
module StructNameMap :
  sig
    module Key :
      sig
        type t = Poppy_parser.Ast_types.Struct_name.t
        val t_of_sexp : Sexplib0.Sexp.t -> t
        val sexp_of_t : t -> Sexplib0.Sexp.t
        type comparator_witness =
            Core.Map.Make(Poppy_parser.Ast_types.Struct_name).Key.comparator_witness
        val comparator : (t, comparator_witness) Core.Comparator.comparator
      end
    type 'a t = (Key.t, 'a, Key.comparator_witness) Base.Map.t
    val compare :
      'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare ->
      'a t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val empty : 'a t
    val singleton : Key.t -> 'a -> 'a t
    val map_keys :
      'v t -> f:(Key.t -> Key.t) -> [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val map_keys_exn : 'v t -> f:(Key.t -> Key.t) -> 'v t
    val of_alist :
      (Key.t * 'a) list -> [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_alist_or_error : (Key.t * 'a) list -> 'a t Base.Or_error.t
    val of_alist_exn : (Key.t * 'a) list -> 'a t
    val of_alist_multi : (Key.t * 'a) list -> 'a list t
    val of_alist_fold :
      (Key.t * 'a) list ->
      init:'weak1031 -> f:('weak1031 -> 'a -> 'weak1031) -> 'weak1031 t
    val of_alist_reduce : (Key.t * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val of_sorted_array : (Key.t * 'a) array -> 'a t Base.Or_error.t
    val of_sorted_array_unchecked : (Key.t * 'a) array -> 'a t
    val of_increasing_iterator_unchecked :
      len:int -> f:(int -> Key.t * 'weak1028) -> 'weak1028 t
    val of_increasing_sequence :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence :
      (Key.t * 'a) Base.Sequence.t ->
      [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_sequence_or_error :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence_exn : (Key.t * 'a) Base.Sequence.t -> 'a t
    val of_sequence_multi : (Key.t * 'a) Base.Sequence.t -> 'a list t
    val of_sequence_fold :
      (Key.t * 'a) Base.Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
    val of_sequence_reduce :
      (Key.t * 'a) Base.Sequence.t -> f:('a -> 'a -> 'a) -> 'a t
    val of_iteri :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) ->
      [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val of_iteri_exn :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) -> 'v t
    val of_tree :
      (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t -> 'a t
    val of_hashtbl_exn : (Key.t, 'a) Core.Hashtbl.t -> 'a t
    val of_key_set :
      (Key.t, Key.comparator_witness) Base.Set.t -> f:(Key.t -> 'v) -> 'v t
    val quickcheck_generator :
      Key.t Core.Quickcheck.Generator.t ->
      'a Core.Quickcheck.Generator.t -> 'a t Core.Quickcheck.Generator.t
    val invariants : 'a t -> bool
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val add : 'a t -> key:Key.t -> data:'a -> 'a t Base.Map.Or_duplicate.t
    val add_exn : 'a t -> key:Key.t -> data:'a -> 'a t
    val set : 'a t -> key:Key.t -> data:'a -> 'a t
    val add_multi : 'a list t -> key:Key.t -> data:'a -> 'a list t
    val remove_multi : 'a list t -> Key.t -> 'a list t
    val find_multi : 'a list t -> Key.t -> 'a list
    val change : 'a t -> Key.t -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> Key.t -> f:('a option -> 'a) -> 'a t
    val find : 'a t -> Key.t -> 'a option
    val find_exn : 'a t -> Key.t -> 'a
    val remove : 'a t -> Key.t -> 'a t
    val mem : 'a t -> Key.t -> bool
    val iter_keys : 'a t -> f:(Key.t -> unit) -> unit
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
    val iteri_until :
      'a t ->
      f:(key:Key.t -> data:'a -> Base.Map.Continue_or_stop.t) ->
      Base.Map.Finished_or_unfinished.t
    val iter2 :
      'a t ->
      'b t ->
      f:(key:Key.t -> data:('a, 'b) Base__Map_intf.Merge_element.t -> unit) ->
      unit
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b) -> 'b t
    val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold_until :
      'a t ->
      init:'acc ->
      f:(key:Key.t ->
         data:'a -> 'acc -> ('acc, 'final) Base.Container.Continue_or_stop.t) ->
      finish:('acc -> 'final) -> 'final
    val fold_right :
      'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold2 :
      'a t ->
      'b t ->
      init:'c ->
      f:(key:Key.t ->
         data:('a, 'b) Base__Map_intf.Merge_element.t -> 'c -> 'c) ->
      'c
    val filter_keys : 'a t -> f:(Key.t -> bool) -> 'a t
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val filteri : 'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    val filter_mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b option) -> 'b t
    val partition_mapi :
      'a t ->
      f:(key:Key.t -> data:'a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partition_map :
      'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partitioni_tf :
      'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t * 'a t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
    val combine_errors : 'a Base.Or_error.t t -> 'a t Base.Or_error.t
    val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> Key.t list
    val data : 'a t -> 'a list
    val to_alist :
      ?key_order:[ `Decreasing | `Increasing ] -> 'a t -> (Key.t * 'a) list
    val merge :
      'a t ->
      'b t ->
      f:(key:Key.t -> ('a, 'b) Base__Map_intf.Merge_element.t -> 'c option) ->
      'c t
    val merge_skewed :
      'v t -> 'v t -> combine:(key:Key.t -> 'v -> 'v -> 'v) -> 'v t
    val symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t Base.Sequence.t
    val fold_symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      init:'c ->
      f:('c -> (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t -> 'c) ->
      'c
    val min_elt : 'a t -> (Key.t * 'a) option
    val min_elt_exn : 'a t -> Key.t * 'a
    val max_elt : 'a t -> (Key.t * 'a) option
    val max_elt_exn : 'a t -> Key.t * 'a
    val for_all : 'a t -> f:('a -> bool) -> bool
    val for_alli : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
    val existsi : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val count : 'a t -> f:('a -> bool) -> int
    val counti : 'a t -> f:(key:Key.t -> data:'a -> bool) -> int
    val split : 'a t -> Key.t -> 'a t * (Key.t * 'a) option * 'a t
    val append :
      lower_part:'a t ->
      upper_part:'a t -> [ `Ok of 'a t | `Overlapping_key_ranges ]
    val subrange :
      'a t ->
      lower_bound:Key.t Base.Maybe_bound.t ->
      upper_bound:Key.t Base.Maybe_bound.t -> 'a t
    val fold_range_inclusive :
      'a t ->
      min:Key.t ->
      max:Key.t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val range_to_alist : 'a t -> min:Key.t -> max:Key.t -> (Key.t * 'a) list
    val closest_key :
      'a t ->
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than
      ] -> Key.t -> (Key.t * 'a) option
    val nth : 'a t -> int -> (Key.t * 'a) option
    val nth_exn : 'a t -> int -> Key.t * 'a
    val rank : 'a t -> Key.t -> int option
    val to_tree :
      'a t -> (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t
    val to_sequence :
      ?order:[ `Decreasing_key | `Increasing_key ] ->
      ?keys_greater_or_equal_to:Key.t ->
      ?keys_less_or_equal_to:Key.t -> 'a t -> (Key.t * 'a) Base.Sequence.t
    val binary_search :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'key -> int) ->
      Base.Binary_searchable.Which_target_by_key.t ->
      'key -> (Key.t * 'a) option
    val binary_search_segmented :
      'a t ->
      segment_of:(key:Key.t -> data:'a -> [ `Left | `Right ]) ->
      Base.Binary_searchable.Which_target_by_segment.t -> (Key.t * 'a) option
    val binary_search_subrange :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'bound -> int) ->
      lower_bound:'bound Base.Maybe_bound.t ->
      upper_bound:'bound Base.Maybe_bound.t -> 'a t
    val key_set : 'a t -> (Key.t, Key.comparator_witness) Base.Set.t
    val validate :
      name:(Key.t -> string) -> 'a Validate.check -> 'a t Validate.check
    val validatei :
      name:(Key.t -> string) ->
      (Key.t * 'a) Validate.check -> 'a t Validate.check
    val quickcheck_observer :
      Key.t Core.Quickcheck.Observer.t ->
      'v Core.Quickcheck.Observer.t -> 'v t Core.Quickcheck.Observer.t
    val quickcheck_shrinker :
      Key.t Core.Quickcheck.Shrinker.t ->
      'v Core.Quickcheck.Shrinker.t -> 'v t Core.Quickcheck.Shrinker.t
    module Provide_of_sexp :
      functor (Key : sig val t_of_sexp : Sexplib0.Sexp.t -> Key.t end) ->
        sig
          val t_of_sexp :
            (Sexplib0.Sexp.t -> 'a__002_) -> Sexplib0.Sexp.t -> 'a__002_ t
        end [@@ocaml.warning "-67"]
    module Provide_bin_io :
      functor
        (Key : sig
                 val bin_size_t : Key.t Bin_prot.Size.sizer
                 val bin_write_t : Key.t Bin_prot.Write.writer
                 val bin_read_t : Key.t Bin_prot.Read.reader
                 val __bin_read_t__ : (int -> Key.t) Bin_prot.Read.reader
                 val bin_shape_t : Bin_prot.Shape.t
                 val bin_writer_t : Key.t Bin_prot.Type_class.writer
                 val bin_reader_t : Key.t Bin_prot.Type_class.reader
                 val bin_t : Key.t Bin_prot.Type_class.t
               end) 
        ->
        sig
          val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t
          val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1
          val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1
          val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1
          val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1
          val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer
          val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader
          val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t
        end [@@ocaml.warning "-67"]
    module Provide_hash :
      functor
        (Key : sig
                 val hash_fold_t :
                   Base.Hash.state -> Key.t -> Base.Hash.state
               end)
        ->
        sig
          val hash_fold_t :
            'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold ->
            'a t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
        end
    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end [@@ocaml.warning "-67"]
module FunctionNameMap :
  sig
    module Key :
      sig
        type t = Poppy_parser.Ast_types.Function_name.t
        val t_of_sexp : Sexplib0.Sexp.t -> t
        val sexp_of_t : t -> Sexplib0.Sexp.t
        type comparator_witness =
            Core.Map.Make(Poppy_parser.Ast_types.Function_name).Key.comparator_witness
        val comparator : (t, comparator_witness) Core.Comparator.comparator
      end
    type 'a t = (Key.t, 'a, Key.comparator_witness) Base.Map.t
    val compare :
      'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare ->
      'a t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val empty : 'a t
    val singleton : Key.t -> 'a -> 'a t
    val map_keys :
      'v t -> f:(Key.t -> Key.t) -> [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val map_keys_exn : 'v t -> f:(Key.t -> Key.t) -> 'v t
    val of_alist :
      (Key.t * 'a) list -> [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_alist_or_error : (Key.t * 'a) list -> 'a t Base.Or_error.t
    val of_alist_exn : (Key.t * 'a) list -> 'a t
    val of_alist_multi : (Key.t * 'a) list -> 'a list t
    val of_alist_fold :
      (Key.t * 'a) list ->
      init:'weak1031 -> f:('weak1031 -> 'a -> 'weak1031) -> 'weak1031 t
    val of_alist_reduce : (Key.t * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val of_sorted_array : (Key.t * 'a) array -> 'a t Base.Or_error.t
    val of_sorted_array_unchecked : (Key.t * 'a) array -> 'a t
    val of_increasing_iterator_unchecked :
      len:int -> f:(int -> Key.t * 'weak1028) -> 'weak1028 t
    val of_increasing_sequence :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence :
      (Key.t * 'a) Base.Sequence.t ->
      [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_sequence_or_error :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence_exn : (Key.t * 'a) Base.Sequence.t -> 'a t
    val of_sequence_multi : (Key.t * 'a) Base.Sequence.t -> 'a list t
    val of_sequence_fold :
      (Key.t * 'a) Base.Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
    val of_sequence_reduce :
      (Key.t * 'a) Base.Sequence.t -> f:('a -> 'a -> 'a) -> 'a t
    val of_iteri :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) ->
      [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val of_iteri_exn :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) -> 'v t
    val of_tree :
      (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t -> 'a t
    val of_hashtbl_exn : (Key.t, 'a) Core.Hashtbl.t -> 'a t
    val of_key_set :
      (Key.t, Key.comparator_witness) Base.Set.t -> f:(Key.t -> 'v) -> 'v t
    val quickcheck_generator :
      Key.t Core.Quickcheck.Generator.t ->
      'a Core.Quickcheck.Generator.t -> 'a t Core.Quickcheck.Generator.t
    val invariants : 'a t -> bool
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val add : 'a t -> key:Key.t -> data:'a -> 'a t Base.Map.Or_duplicate.t
    val add_exn : 'a t -> key:Key.t -> data:'a -> 'a t
    val set : 'a t -> key:Key.t -> data:'a -> 'a t
    val add_multi : 'a list t -> key:Key.t -> data:'a -> 'a list t
    val remove_multi : 'a list t -> Key.t -> 'a list t
    val find_multi : 'a list t -> Key.t -> 'a list
    val change : 'a t -> Key.t -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> Key.t -> f:('a option -> 'a) -> 'a t
    val find : 'a t -> Key.t -> 'a option
    val find_exn : 'a t -> Key.t -> 'a
    val remove : 'a t -> Key.t -> 'a t
    val mem : 'a t -> Key.t -> bool
    val iter_keys : 'a t -> f:(Key.t -> unit) -> unit
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
    val iteri_until :
      'a t ->
      f:(key:Key.t -> data:'a -> Base.Map.Continue_or_stop.t) ->
      Base.Map.Finished_or_unfinished.t
    val iter2 :
      'a t ->
      'b t ->
      f:(key:Key.t -> data:('a, 'b) Base__Map_intf.Merge_element.t -> unit) ->
      unit
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b) -> 'b t
    val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold_until :
      'a t ->
      init:'acc ->
      f:(key:Key.t ->
         data:'a -> 'acc -> ('acc, 'final) Base.Container.Continue_or_stop.t) ->
      finish:('acc -> 'final) -> 'final
    val fold_right :
      'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold2 :
      'a t ->
      'b t ->
      init:'c ->
      f:(key:Key.t ->
         data:('a, 'b) Base__Map_intf.Merge_element.t -> 'c -> 'c) ->
      'c
    val filter_keys : 'a t -> f:(Key.t -> bool) -> 'a t
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val filteri : 'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    val filter_mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b option) -> 'b t
    val partition_mapi :
      'a t ->
      f:(key:Key.t -> data:'a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partition_map :
      'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partitioni_tf :
      'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t * 'a t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
    val combine_errors : 'a Base.Or_error.t t -> 'a t Base.Or_error.t
    val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> Key.t list
    val data : 'a t -> 'a list
    val to_alist :
      ?key_order:[ `Decreasing | `Increasing ] -> 'a t -> (Key.t * 'a) list
    val merge :
      'a t ->
      'b t ->
      f:(key:Key.t -> ('a, 'b) Base__Map_intf.Merge_element.t -> 'c option) ->
      'c t
    val merge_skewed :
      'v t -> 'v t -> combine:(key:Key.t -> 'v -> 'v -> 'v) -> 'v t
    val symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t Base.Sequence.t
    val fold_symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      init:'c ->
      f:('c -> (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t -> 'c) ->
      'c
    val min_elt : 'a t -> (Key.t * 'a) option
    val min_elt_exn : 'a t -> Key.t * 'a
    val max_elt : 'a t -> (Key.t * 'a) option
    val max_elt_exn : 'a t -> Key.t * 'a
    val for_all : 'a t -> f:('a -> bool) -> bool
    val for_alli : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
    val existsi : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val count : 'a t -> f:('a -> bool) -> int
    val counti : 'a t -> f:(key:Key.t -> data:'a -> bool) -> int
    val split : 'a t -> Key.t -> 'a t * (Key.t * 'a) option * 'a t
    val append :
      lower_part:'a t ->
      upper_part:'a t -> [ `Ok of 'a t | `Overlapping_key_ranges ]
    val subrange :
      'a t ->
      lower_bound:Key.t Base.Maybe_bound.t ->
      upper_bound:Key.t Base.Maybe_bound.t -> 'a t
    val fold_range_inclusive :
      'a t ->
      min:Key.t ->
      max:Key.t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val range_to_alist : 'a t -> min:Key.t -> max:Key.t -> (Key.t * 'a) list
    val closest_key :
      'a t ->
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than
      ] -> Key.t -> (Key.t * 'a) option
    val nth : 'a t -> int -> (Key.t * 'a) option
    val nth_exn : 'a t -> int -> Key.t * 'a
    val rank : 'a t -> Key.t -> int option
    val to_tree :
      'a t -> (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t
    val to_sequence :
      ?order:[ `Decreasing_key | `Increasing_key ] ->
      ?keys_greater_or_equal_to:Key.t ->
      ?keys_less_or_equal_to:Key.t -> 'a t -> (Key.t * 'a) Base.Sequence.t
    val binary_search :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'key -> int) ->
      Base.Binary_searchable.Which_target_by_key.t ->
      'key -> (Key.t * 'a) option
    val binary_search_segmented :
      'a t ->
      segment_of:(key:Key.t -> data:'a -> [ `Left | `Right ]) ->
      Base.Binary_searchable.Which_target_by_segment.t -> (Key.t * 'a) option
    val binary_search_subrange :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'bound -> int) ->
      lower_bound:'bound Base.Maybe_bound.t ->
      upper_bound:'bound Base.Maybe_bound.t -> 'a t
    val key_set : 'a t -> (Key.t, Key.comparator_witness) Base.Set.t
    val validate :
      name:(Key.t -> string) -> 'a Validate.check -> 'a t Validate.check
    val validatei :
      name:(Key.t -> string) ->
      (Key.t * 'a) Validate.check -> 'a t Validate.check
    val quickcheck_observer :
      Key.t Core.Quickcheck.Observer.t ->
      'v Core.Quickcheck.Observer.t -> 'v t Core.Quickcheck.Observer.t
    val quickcheck_shrinker :
      Key.t Core.Quickcheck.Shrinker.t ->
      'v Core.Quickcheck.Shrinker.t -> 'v t Core.Quickcheck.Shrinker.t
    module Provide_of_sexp :
      functor (Key : sig val t_of_sexp : Sexplib0.Sexp.t -> Key.t end) ->
        sig
          val t_of_sexp :
            (Sexplib0.Sexp.t -> 'a__002_) -> Sexplib0.Sexp.t -> 'a__002_ t
        end [@@ocaml.warning "-67"]
    module Provide_bin_io :
      functor
        (Key : sig
                 val bin_size_t : Key.t Bin_prot.Size.sizer
                 val bin_write_t : Key.t Bin_prot.Write.writer
                 val bin_read_t : Key.t Bin_prot.Read.reader
                 val __bin_read_t__ : (int -> Key.t) Bin_prot.Read.reader
                 val bin_shape_t : Bin_prot.Shape.t
                 val bin_writer_t : Key.t Bin_prot.Type_class.writer
                 val bin_reader_t : Key.t Bin_prot.Type_class.reader
                 val bin_t : Key.t Bin_prot.Type_class.t
               end)
        ->
        sig
          val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t
          val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1
          val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1
          val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1
          val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1
          val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer
          val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader
          val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t
        end [@@ocaml.warning "-67"]
    module Provide_hash :
      functor
        (Key : sig
                 val hash_fold_t :
                   Base.Hash.state -> Key.t -> Base.Hash.state
               end)
        ->
        sig
          val hash_fold_t :
            'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold ->
            'a t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
        end
    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end [@@ocaml.warning "-67"]
module MethodNameMap :
  sig
    module Key :
      sig
        type t = Poppy_parser.Ast_types.Method_name.t
        val t_of_sexp : Sexplib0.Sexp.t -> t
        val sexp_of_t : t -> Sexplib0.Sexp.t
        type comparator_witness =
            Core.Map.Make(Poppy_parser.Ast_types.Method_name).Key.comparator_witness
        val comparator : (t, comparator_witness) Core.Comparator.comparator
      end
    type 'a t = (Key.t, 'a, Key.comparator_witness) Base.Map.t
    val compare :
      'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare ->
      'a t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val empty : 'a t
    val singleton : Key.t -> 'a -> 'a t
    val map_keys :
      'v t -> f:(Key.t -> Key.t) -> [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val map_keys_exn : 'v t -> f:(Key.t -> Key.t) -> 'v t
    val of_alist :
      (Key.t * 'a) list -> [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_alist_or_error : (Key.t * 'a) list -> 'a t Base.Or_error.t
    val of_alist_exn : (Key.t * 'a) list -> 'a t
    val of_alist_multi : (Key.t * 'a) list -> 'a list t
    val of_alist_fold :
      (Key.t * 'a) list ->
      init:'weak1031 -> f:('weak1031 -> 'a -> 'weak1031) -> 'weak1031 t
    val of_alist_reduce : (Key.t * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val of_sorted_array : (Key.t * 'a) array -> 'a t Base.Or_error.t
    val of_sorted_array_unchecked : (Key.t * 'a) array -> 'a t
    val of_increasing_iterator_unchecked :
      len:int -> f:(int -> Key.t * 'weak1028) -> 'weak1028 t
    val of_increasing_sequence :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence :
      (Key.t * 'a) Base.Sequence.t ->
      [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_sequence_or_error :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence_exn : (Key.t * 'a) Base.Sequence.t -> 'a t
    val of_sequence_multi : (Key.t * 'a) Base.Sequence.t -> 'a list t
    val of_sequence_fold :
      (Key.t * 'a) Base.Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
    val of_sequence_reduce :
      (Key.t * 'a) Base.Sequence.t -> f:('a -> 'a -> 'a) -> 'a t
    val of_iteri :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) ->
      [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val of_iteri_exn :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) -> 'v t
    val of_tree :
      (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t -> 'a t
    val of_hashtbl_exn : (Key.t, 'a) Core.Hashtbl.t -> 'a t
    val of_key_set :
      (Key.t, Key.comparator_witness) Base.Set.t -> f:(Key.t -> 'v) -> 'v t
    val quickcheck_generator :
      Key.t Core.Quickcheck.Generator.t ->
      'a Core.Quickcheck.Generator.t -> 'a t Core.Quickcheck.Generator.t
    val invariants : 'a t -> bool
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val add : 'a t -> key:Key.t -> data:'a -> 'a t Base.Map.Or_duplicate.t
    val add_exn : 'a t -> key:Key.t -> data:'a -> 'a t
    val set : 'a t -> key:Key.t -> data:'a -> 'a t
    val add_multi : 'a list t -> key:Key.t -> data:'a -> 'a list t
    val remove_multi : 'a list t -> Key.t -> 'a list t
    val find_multi : 'a list t -> Key.t -> 'a list
    val change : 'a t -> Key.t -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> Key.t -> f:('a option -> 'a) -> 'a t
    val find : 'a t -> Key.t -> 'a option
    val find_exn : 'a t -> Key.t -> 'a
    val remove : 'a t -> Key.t -> 'a t
    val mem : 'a t -> Key.t -> bool
    val iter_keys : 'a t -> f:(Key.t -> unit) -> unit
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
    val iteri_until :
      'a t ->
      f:(key:Key.t -> data:'a -> Base.Map.Continue_or_stop.t) ->
      Base.Map.Finished_or_unfinished.t
    val iter2 :
      'a t ->
      'b t ->
      f:(key:Key.t -> data:('a, 'b) Base__Map_intf.Merge_element.t -> unit) ->
      unit
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b) -> 'b t
    val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold_until :
      'a t ->
      init:'acc ->
      f:(key:Key.t ->
         data:'a -> 'acc -> ('acc, 'final) Base.Container.Continue_or_stop.t) ->
      finish:('acc -> 'final) -> 'final
    val fold_right :
      'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold2 :
      'a t ->
      'b t ->
      init:'c ->
      f:(key:Key.t ->
         data:('a, 'b) Base__Map_intf.Merge_element.t -> 'c -> 'c) ->
      'c
    val filter_keys : 'a t -> f:(Key.t -> bool) -> 'a t
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val filteri : 'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    val filter_mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b option) -> 'b t
    val partition_mapi :
      'a t ->
      f:(key:Key.t -> data:'a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partition_map :
      'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partitioni_tf :
      'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t * 'a t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
    val combine_errors : 'a Base.Or_error.t t -> 'a t Base.Or_error.t
    val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> Key.t list
    val data : 'a t -> 'a list
    val to_alist :
      ?key_order:[ `Decreasing | `Increasing ] -> 'a t -> (Key.t * 'a) list
    val merge :
      'a t ->
      'b t ->
      f:(key:Key.t -> ('a, 'b) Base__Map_intf.Merge_element.t -> 'c option) ->
      'c t
    val merge_skewed :
      'v t -> 'v t -> combine:(key:Key.t -> 'v -> 'v -> 'v) -> 'v t
    val symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t Base.Sequence.t
    val fold_symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      init:'c ->
      f:('c -> (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t -> 'c) ->
      'c
    val min_elt : 'a t -> (Key.t * 'a) option
    val min_elt_exn : 'a t -> Key.t * 'a
    val max_elt : 'a t -> (Key.t * 'a) option
    val max_elt_exn : 'a t -> Key.t * 'a
    val for_all : 'a t -> f:('a -> bool) -> bool
    val for_alli : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
    val existsi : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val count : 'a t -> f:('a -> bool) -> int
    val counti : 'a t -> f:(key:Key.t -> data:'a -> bool) -> int
    val split : 'a t -> Key.t -> 'a t * (Key.t * 'a) option * 'a t
    val append :
      lower_part:'a t ->
      upper_part:'a t -> [ `Ok of 'a t | `Overlapping_key_ranges ]
    val subrange :
      'a t ->
      lower_bound:Key.t Base.Maybe_bound.t ->
      upper_bound:Key.t Base.Maybe_bound.t -> 'a t
    val fold_range_inclusive :
      'a t ->
      min:Key.t ->
      max:Key.t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val range_to_alist : 'a t -> min:Key.t -> max:Key.t -> (Key.t * 'a) list
    val closest_key :
      'a t ->
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than
      ] -> Key.t -> (Key.t * 'a) option
    val nth : 'a t -> int -> (Key.t * 'a) option
    val nth_exn : 'a t -> int -> Key.t * 'a
    val rank : 'a t -> Key.t -> int option
    val to_tree :
      'a t -> (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t
    val to_sequence :
      ?order:[ `Decreasing_key | `Increasing_key ] ->
      ?keys_greater_or_equal_to:Key.t ->
      ?keys_less_or_equal_to:Key.t -> 'a t -> (Key.t * 'a) Base.Sequence.t
    val binary_search :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'key -> int) ->
      Base.Binary_searchable.Which_target_by_key.t ->
      'key -> (Key.t * 'a) option
    val binary_search_segmented :
      'a t ->
      segment_of:(key:Key.t -> data:'a -> [ `Left | `Right ]) ->
      Base.Binary_searchable.Which_target_by_segment.t -> (Key.t * 'a) option
    val binary_search_subrange :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'bound -> int) ->
      lower_bound:'bound Base.Maybe_bound.t ->
      upper_bound:'bound Base.Maybe_bound.t -> 'a t
    val key_set : 'a t -> (Key.t, Key.comparator_witness) Base.Set.t
    val validate :
      name:(Key.t -> string) -> 'a Validate.check -> 'a t Validate.check
    val validatei :
      name:(Key.t -> string) ->
      (Key.t * 'a) Validate.check -> 'a t Validate.check
    val quickcheck_observer :
      Key.t Core.Quickcheck.Observer.t ->
      'v Core.Quickcheck.Observer.t -> 'v t Core.Quickcheck.Observer.t
    val quickcheck_shrinker :
      Key.t Core.Quickcheck.Shrinker.t ->
      'v Core.Quickcheck.Shrinker.t -> 'v t Core.Quickcheck.Shrinker.t
    module Provide_of_sexp :
      functor (Key : sig val t_of_sexp : Sexplib0.Sexp.t -> Key.t end) ->
        sig
          val t_of_sexp :
            (Sexplib0.Sexp.t -> 'a__002_) -> Sexplib0.Sexp.t -> 'a__002_ t
        end [@@ocaml.warning "-67"]
    module Provide_bin_io :
      functor
        (Key : sig
                 val bin_size_t : Key.t Bin_prot.Size.sizer
                 val bin_write_t : Key.t Bin_prot.Write.writer
                 val bin_read_t : Key.t Bin_prot.Read.reader
                 val __bin_read_t__ : (int -> Key.t) Bin_prot.Read.reader
                 val bin_shape_t : Bin_prot.Shape.t
                 val bin_writer_t : Key.t Bin_prot.Type_class.writer
                 val bin_reader_t : Key.t Bin_prot.Type_class.reader
                 val bin_t : Key.t Bin_prot.Type_class.t
               end)
        ->
        sig
          val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t
          val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1
          val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1
          val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1
          val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1
          val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer
          val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader
          val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t
        end [@@ocaml.warning "-67"]
    module Provide_hash :
      functor
        (Key : sig
                 val hash_fold_t :
                   Base.Hash.state -> Key.t -> Base.Hash.state
               end)
        ->
        sig
          val hash_fold_t :
            'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold ->
            'a t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
        end
    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end [@@ocaml.warning "-67"]
module TraitNameMap :
  sig
    module Key :
      sig
        type t = Poppy_parser.Ast_types.Trait_name.t
        val t_of_sexp : Sexplib0.Sexp.t -> t
        val sexp_of_t : t -> Sexplib0.Sexp.t
        type comparator_witness =
            Core.Map.Make(Poppy_parser.Ast_types.Trait_name).Key.comparator_witness
        val comparator : (t, comparator_witness) Core.Comparator.comparator
      end
    type 'a t = (Key.t, 'a, Key.comparator_witness) Base.Map.t
    val compare :
      'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare ->
      'a t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val empty : 'a t
    val singleton : Key.t -> 'a -> 'a t
    val map_keys :
      'v t -> f:(Key.t -> Key.t) -> [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val map_keys_exn : 'v t -> f:(Key.t -> Key.t) -> 'v t
    val of_alist :
      (Key.t * 'a) list -> [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_alist_or_error : (Key.t * 'a) list -> 'a t Base.Or_error.t
    val of_alist_exn : (Key.t * 'a) list -> 'a t
    val of_alist_multi : (Key.t * 'a) list -> 'a list t
    val of_alist_fold :
      (Key.t * 'a) list ->
      init:'weak1031 -> f:('weak1031 -> 'a -> 'weak1031) -> 'weak1031 t
    val of_alist_reduce : (Key.t * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val of_sorted_array : (Key.t * 'a) array -> 'a t Base.Or_error.t
    val of_sorted_array_unchecked : (Key.t * 'a) array -> 'a t
    val of_increasing_iterator_unchecked :
      len:int -> f:(int -> Key.t * 'weak1028) -> 'weak1028 t
    val of_increasing_sequence :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence :
      (Key.t * 'a) Base.Sequence.t ->
      [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_sequence_or_error :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence_exn : (Key.t * 'a) Base.Sequence.t -> 'a t
    val of_sequence_multi : (Key.t * 'a) Base.Sequence.t -> 'a list t
    val of_sequence_fold :
      (Key.t * 'a) Base.Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
    val of_sequence_reduce :
      (Key.t * 'a) Base.Sequence.t -> f:('a -> 'a -> 'a) -> 'a t
    val of_iteri :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) ->
      [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val of_iteri_exn :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) -> 'v t
    val of_tree :
      (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t -> 'a t
    val of_hashtbl_exn : (Key.t, 'a) Core.Hashtbl.t -> 'a t
    val of_key_set :
      (Key.t, Key.comparator_witness) Base.Set.t -> f:(Key.t -> 'v) -> 'v t
    val quickcheck_generator :
      Key.t Core.Quickcheck.Generator.t ->
      'a Core.Quickcheck.Generator.t -> 'a t Core.Quickcheck.Generator.t
    val invariants : 'a t -> bool
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val add : 'a t -> key:Key.t -> data:'a -> 'a t Base.Map.Or_duplicate.t
    val add_exn : 'a t -> key:Key.t -> data:'a -> 'a t
    val set : 'a t -> key:Key.t -> data:'a -> 'a t
    val add_multi : 'a list t -> key:Key.t -> data:'a -> 'a list t
    val remove_multi : 'a list t -> Key.t -> 'a list t
    val find_multi : 'a list t -> Key.t -> 'a list
    val change : 'a t -> Key.t -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> Key.t -> f:('a option -> 'a) -> 'a t
    val find : 'a t -> Key.t -> 'a option
    val find_exn : 'a t -> Key.t -> 'a
    val remove : 'a t -> Key.t -> 'a t
    val mem : 'a t -> Key.t -> bool
    val iter_keys : 'a t -> f:(Key.t -> unit) -> unit
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
    val iteri_until :
      'a t ->
      f:(key:Key.t -> data:'a -> Base.Map.Continue_or_stop.t) ->
      Base.Map.Finished_or_unfinished.t
    val iter2 :
      'a t ->
      'b t ->
      f:(key:Key.t -> data:('a, 'b) Base__Map_intf.Merge_element.t -> unit) ->
      unit
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b) -> 'b t
    val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold_until :
      'a t ->
      init:'acc ->
      f:(key:Key.t ->
         data:'a -> 'acc -> ('acc, 'final) Base.Container.Continue_or_stop.t) ->
      finish:('acc -> 'final) -> 'final
    val fold_right :
      'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold2 :
      'a t ->
      'b t ->
      init:'c ->
      f:(key:Key.t ->
         data:('a, 'b) Base__Map_intf.Merge_element.t -> 'c -> 'c) ->
      'c
    val filter_keys : 'a t -> f:(Key.t -> bool) -> 'a t
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val filteri : 'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    val filter_mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b option) -> 'b t
    val partition_mapi :
      'a t ->
      f:(key:Key.t -> data:'a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partition_map :
      'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partitioni_tf :
      'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t * 'a t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
    val combine_errors : 'a Base.Or_error.t t -> 'a t Base.Or_error.t
    val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> Key.t list
    val data : 'a t -> 'a list
    val to_alist :
      ?key_order:[ `Decreasing | `Increasing ] -> 'a t -> (Key.t * 'a) list
    val merge :
      'a t ->
      'b t ->
      f:(key:Key.t -> ('a, 'b) Base__Map_intf.Merge_element.t -> 'c option) ->
      'c t
    val merge_skewed :
      'v t -> 'v t -> combine:(key:Key.t -> 'v -> 'v -> 'v) -> 'v t
    val symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t Base.Sequence.t
    val fold_symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      init:'c ->
      f:('c -> (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t -> 'c) ->
      'c
    val min_elt : 'a t -> (Key.t * 'a) option
    val min_elt_exn : 'a t -> Key.t * 'a
    val max_elt : 'a t -> (Key.t * 'a) option
    val max_elt_exn : 'a t -> Key.t * 'a
    val for_all : 'a t -> f:('a -> bool) -> bool
    val for_alli : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
    val existsi : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val count : 'a t -> f:('a -> bool) -> int
    val counti : 'a t -> f:(key:Key.t -> data:'a -> bool) -> int
    val split : 'a t -> Key.t -> 'a t * (Key.t * 'a) option * 'a t
    val append :
      lower_part:'a t ->
      upper_part:'a t -> [ `Ok of 'a t | `Overlapping_key_ranges ]
    val subrange :
      'a t ->
      lower_bound:Key.t Base.Maybe_bound.t ->
      upper_bound:Key.t Base.Maybe_bound.t -> 'a t
    val fold_range_inclusive :
      'a t ->
      min:Key.t ->
      max:Key.t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val range_to_alist : 'a t -> min:Key.t -> max:Key.t -> (Key.t * 'a) list
    val closest_key :
      'a t ->
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than
      ] -> Key.t -> (Key.t * 'a) option
    val nth : 'a t -> int -> (Key.t * 'a) option
    val nth_exn : 'a t -> int -> Key.t * 'a
    val rank : 'a t -> Key.t -> int option
    val to_tree :
      'a t -> (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t
    val to_sequence :
      ?order:[ `Decreasing_key | `Increasing_key ] ->
      ?keys_greater_or_equal_to:Key.t ->
      ?keys_less_or_equal_to:Key.t -> 'a t -> (Key.t * 'a) Base.Sequence.t
    val binary_search :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'key -> int) ->
      Base.Binary_searchable.Which_target_by_key.t ->
      'key -> (Key.t * 'a) option
    val binary_search_segmented :
      'a t ->
      segment_of:(key:Key.t -> data:'a -> [ `Left | `Right ]) ->
      Base.Binary_searchable.Which_target_by_segment.t -> (Key.t * 'a) option
    val binary_search_subrange :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'bound -> int) ->
      lower_bound:'bound Base.Maybe_bound.t ->
      upper_bound:'bound Base.Maybe_bound.t -> 'a t
    val key_set : 'a t -> (Key.t, Key.comparator_witness) Base.Set.t
    val validate :
      name:(Key.t -> string) -> 'a Validate.check -> 'a t Validate.check
    val validatei :
      name:(Key.t -> string) ->
      (Key.t * 'a) Validate.check -> 'a t Validate.check
    val quickcheck_observer :
      Key.t Core.Quickcheck.Observer.t ->
      'v Core.Quickcheck.Observer.t -> 'v t Core.Quickcheck.Observer.t
    val quickcheck_shrinker :
      Key.t Core.Quickcheck.Shrinker.t ->
      'v Core.Quickcheck.Shrinker.t -> 'v t Core.Quickcheck.Shrinker.t
    module Provide_of_sexp :
      functor (Key : sig val t_of_sexp : Sexplib0.Sexp.t -> Key.t end) ->
        sig
          val t_of_sexp :
            (Sexplib0.Sexp.t -> 'a__002_) -> Sexplib0.Sexp.t -> 'a__002_ t
        end [@@ocaml.warning "-67"]
    module Provide_bin_io :
      functor
        (Key : sig
                 val bin_size_t : Key.t Bin_prot.Size.sizer
                 val bin_write_t : Key.t Bin_prot.Write.writer
                 val bin_read_t : Key.t Bin_prot.Read.reader
                 val __bin_read_t__ : (int -> Key.t) Bin_prot.Read.reader
                 val bin_shape_t : Bin_prot.Shape.t
                 val bin_writer_t : Key.t Bin_prot.Type_class.writer
                 val bin_reader_t : Key.t Bin_prot.Type_class.reader
                 val bin_t : Key.t Bin_prot.Type_class.t
               end)
        ->
        sig
          val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t
          val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1
          val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1
          val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1
          val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1
          val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer
          val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader
          val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t
        end [@@ocaml.warning "-67"]
    module Provide_hash :
      functor
        (Key : sig
                 val hash_fold_t :
                   Base.Hash.state -> Key.t -> Base.Hash.state
               end)
        ->
        sig
          val hash_fold_t :
            'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold ->
            'a t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
        end [@@ocaml.warning "-67"]
    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end
module StructTraitMap :
  sig
    module Key :
      sig
        type t = StructNameMap.Key.t
        val t_of_sexp : Sexplib0.Sexp.t -> t
        val sexp_of_t : t -> Sexplib0.Sexp.t
        type comparator_witness = StructNameMap.Key.comparator_witness
        val comparator : (t, comparator_witness) Core.Comparator.comparator
      end
    type 'a t = (Key.t, 'a, Key.comparator_witness) Base.Map.t
    val compare :
      'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare ->
      'a t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val empty : 'a t
    val singleton : Key.t -> 'a -> 'a t
    val map_keys :
      'v t -> f:(Key.t -> Key.t) -> [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val map_keys_exn : 'v t -> f:(Key.t -> Key.t) -> 'v t
    val of_alist :
      (Key.t * 'a) list -> [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_alist_or_error : (Key.t * 'a) list -> 'a t Base.Or_error.t
    val of_alist_exn : (Key.t * 'a) list -> 'a t
    val of_alist_multi : (Key.t * 'a) list -> 'a list t
    val of_alist_fold :
      (Key.t * 'a) list ->
      init:'weak1031 -> f:('weak1031 -> 'a -> 'weak1031) -> 'weak1031 t
    val of_alist_reduce : (Key.t * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val of_sorted_array : (Key.t * 'a) array -> 'a t Base.Or_error.t
    val of_sorted_array_unchecked : (Key.t * 'a) array -> 'a t
    val of_increasing_iterator_unchecked :
      len:int -> f:(int -> Key.t * 'weak1028) -> 'weak1028 t
    val of_increasing_sequence :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence :
      (Key.t * 'a) Base.Sequence.t ->
      [ `Duplicate_key of Key.t | `Ok of 'a t ]
    val of_sequence_or_error :
      (Key.t * 'a) Base.Sequence.t -> 'a t Base.Or_error.t
    val of_sequence_exn : (Key.t * 'a) Base.Sequence.t -> 'a t
    val of_sequence_multi : (Key.t * 'a) Base.Sequence.t -> 'a list t
    val of_sequence_fold :
      (Key.t * 'a) Base.Sequence.t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
    val of_sequence_reduce :
      (Key.t * 'a) Base.Sequence.t -> f:('a -> 'a -> 'a) -> 'a t
    val of_iteri :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) ->
      [ `Duplicate_key of Key.t | `Ok of 'v t ]
    val of_iteri_exn :
      iteri:(f:(key:Key.t -> data:'v -> unit) -> unit) -> 'v t
    val of_tree :
      (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t -> 'a t
    val of_hashtbl_exn : (Key.t, 'a) Core.Hashtbl.t -> 'a t
    val of_key_set :
      (Key.t, Key.comparator_witness) Base.Set.t -> f:(Key.t -> 'v) -> 'v t
    val quickcheck_generator :
      Key.t Core.Quickcheck.Generator.t ->
      'a Core.Quickcheck.Generator.t -> 'a t Core.Quickcheck.Generator.t
    val invariants : 'a t -> bool
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val add : 'a t -> key:Key.t -> data:'a -> 'a t Base.Map.Or_duplicate.t
    val add_exn : 'a t -> key:Key.t -> data:'a -> 'a t
    val set : 'a t -> key:Key.t -> data:'a -> 'a t
    val add_multi : 'a list t -> key:Key.t -> data:'a -> 'a list t
    val remove_multi : 'a list t -> Key.t -> 'a list t
    val find_multi : 'a list t -> Key.t -> 'a list
    val change : 'a t -> Key.t -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> Key.t -> f:('a option -> 'a) -> 'a t
    val find : 'a t -> Key.t -> 'a option
    val find_exn : 'a t -> Key.t -> 'a
    val remove : 'a t -> Key.t -> 'a t
    val mem : 'a t -> Key.t -> bool
    val iter_keys : 'a t -> f:(Key.t -> unit) -> unit
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
    val iteri_until :
      'a t ->
      f:(key:Key.t -> data:'a -> Base.Map.Continue_or_stop.t) ->
      Base.Map.Finished_or_unfinished.t
    val iter2 :
      'a t ->
      'b t ->
      f:(key:Key.t -> data:('a, 'b) Base__Map_intf.Merge_element.t -> unit) ->
      unit
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b) -> 'b t
    val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold_until :
      'a t ->
      init:'acc ->
      f:(key:Key.t ->
         data:'a -> 'acc -> ('acc, 'final) Base.Container.Continue_or_stop.t) ->
      finish:('acc -> 'final) -> 'final
    val fold_right :
      'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val fold2 :
      'a t ->
      'b t ->
      init:'c ->
      f:(key:Key.t ->
         data:('a, 'b) Base__Map_intf.Merge_element.t -> 'c -> 'c) ->
      'c
    val filter_keys : 'a t -> f:(Key.t -> bool) -> 'a t
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val filteri : 'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
    val filter_mapi : 'a t -> f:(key:Key.t -> data:'a -> 'b option) -> 'b t
    val partition_mapi :
      'a t ->
      f:(key:Key.t -> data:'a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partition_map :
      'a t -> f:('a -> ('b, 'c) Base.Either.t) -> 'b t * 'c t
    val partitioni_tf :
      'a t -> f:(key:Key.t -> data:'a -> bool) -> 'a t * 'a t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
    val combine_errors : 'a Base.Or_error.t t -> 'a t Base.Or_error.t
    val compare_direct : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> Key.t list
    val data : 'a t -> 'a list
    val to_alist :
      ?key_order:[ `Decreasing | `Increasing ] -> 'a t -> (Key.t * 'a) list
    val merge :
      'a t ->
      'b t ->
      f:(key:Key.t -> ('a, 'b) Base__Map_intf.Merge_element.t -> 'c option) ->
      'c t
    val merge_skewed :
      'v t -> 'v t -> combine:(key:Key.t -> 'v -> 'v -> 'v) -> 'v t
    val symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t Base.Sequence.t
    val fold_symmetric_diff :
      'a t ->
      'a t ->
      data_equal:('a -> 'a -> bool) ->
      init:'c ->
      f:('c -> (Key.t, 'a) Base__Map_intf.Symmetric_diff_element.t -> 'c) ->
      'c
    val min_elt : 'a t -> (Key.t * 'a) option
    val min_elt_exn : 'a t -> Key.t * 'a
    val max_elt : 'a t -> (Key.t * 'a) option
    val max_elt_exn : 'a t -> Key.t * 'a
    val for_all : 'a t -> f:('a -> bool) -> bool
    val for_alli : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val exists : 'a t -> f:('a -> bool) -> bool
    val existsi : 'a t -> f:(key:Key.t -> data:'a -> bool) -> bool
    val count : 'a t -> f:('a -> bool) -> int
    val counti : 'a t -> f:(key:Key.t -> data:'a -> bool) -> int
    val split : 'a t -> Key.t -> 'a t * (Key.t * 'a) option * 'a t
    val append :
      lower_part:'a t ->
      upper_part:'a t -> [ `Ok of 'a t | `Overlapping_key_ranges ]
    val subrange :
      'a t ->
      lower_bound:Key.t Base.Maybe_bound.t ->
      upper_bound:Key.t Base.Maybe_bound.t -> 'a t
    val fold_range_inclusive :
      'a t ->
      min:Key.t ->
      max:Key.t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
    val range_to_alist : 'a t -> min:Key.t -> max:Key.t -> (Key.t * 'a) list
    val closest_key :
      'a t ->
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than
      ] -> Key.t -> (Key.t * 'a) option
    val nth : 'a t -> int -> (Key.t * 'a) option
    val nth_exn : 'a t -> int -> Key.t * 'a
    val rank : 'a t -> Key.t -> int option
    val to_tree :
      'a t -> (Key.t, 'a, Key.comparator_witness) Core.Map_intf.Tree.t
    val to_sequence :
      ?order:[ `Decreasing_key | `Increasing_key ] ->
      ?keys_greater_or_equal_to:Key.t ->
      ?keys_less_or_equal_to:Key.t -> 'a t -> (Key.t * 'a) Base.Sequence.t
    val binary_search :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'key -> int) ->
      Base.Binary_searchable.Which_target_by_key.t ->
      'key -> (Key.t * 'a) option
    val binary_search_segmented :
      'a t ->
      segment_of:(key:Key.t -> data:'a -> [ `Left | `Right ]) ->
      Base.Binary_searchable.Which_target_by_segment.t -> (Key.t * 'a) option
    val binary_search_subrange :
      'a t ->
      compare:(key:Key.t -> data:'a -> 'bound -> int) ->
      lower_bound:'bound Base.Maybe_bound.t ->
      upper_bound:'bound Base.Maybe_bound.t -> 'a t
    val key_set : 'a t -> (Key.t, Key.comparator_witness) Base.Set.t
    val validate :
      name:(Key.t -> string) -> 'a Validate.check -> 'a t Validate.check
    val validatei :
      name:(Key.t -> string) ->
      (Key.t * 'a) Validate.check -> 'a t Validate.check
    val quickcheck_observer :
      Key.t Core.Quickcheck.Observer.t ->
      'v Core.Quickcheck.Observer.t -> 'v t Core.Quickcheck.Observer.t
    val quickcheck_shrinker :
      Key.t Core.Quickcheck.Shrinker.t ->
      'v Core.Quickcheck.Shrinker.t -> 'v t Core.Quickcheck.Shrinker.t
    module Provide_of_sexp :
      functor (Key : sig val t_of_sexp : Sexplib0.Sexp.t -> Key.t end) ->
        sig
          val t_of_sexp :
            (Sexplib0.Sexp.t -> 'a__002_) -> Sexplib0.Sexp.t -> 'a__002_ t
        end [@@ocaml.warning "-67"]
    module Provide_bin_io :
      functor
        (Key : sig
                 val bin_size_t : Key.t Bin_prot.Size.sizer
                 val bin_write_t : Key.t Bin_prot.Write.writer
                 val bin_read_t : Key.t Bin_prot.Read.reader
                 val __bin_read_t__ : (int -> Key.t) Bin_prot.Read.reader
                 val bin_shape_t : Bin_prot.Shape.t
                 val bin_writer_t : Key.t Bin_prot.Type_class.writer
                 val bin_reader_t : Key.t Bin_prot.Type_class.reader
                 val bin_t : Key.t Bin_prot.Type_class.t
               end)
        ->
        sig
          val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t
          val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1
          val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1
          val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1
          val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1
          val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer
          val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader
          val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t
        end [@@ocaml.warning "-67"]
    module Provide_hash :
      functor
        (Key : sig
                 val hash_fold_t :
                   Base.Hash.state -> Key.t -> Base.Hash.state
               end)
        ->
        sig
          val hash_fold_t :
            'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold ->
            'a t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
        end
    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end [@@ocaml.warning "-67"]
type env =
    Global of Poppy_parser.Ast.struct_defn StructNameMap.t *
      Poppy_parser.Ast.trait_defn TraitNameMap.t *
      Poppy_parser.Ast.method_defn MethodNameMap.t *
      Poppy_parser.Ast.function_defn FunctionNameMap.t *
      TraitNameMap.Key.t list StructTraitMap.t
  | Function of env * Poppy_parser.Ast_types.type_expr VarNameMap.t
  | Block of env * Poppy_parser.Ast_types.type_expr VarNameMap.t
val env_of_sexp : Sexplib0.Sexp.t -> env
val sexp_of_env : env -> Sexplib0.Sexp.t
val init_global_scope : unit -> env
val equal_type_expr_list :
  Poppy_parser.Ast_types.type_expr list ->
  Poppy_parser.Ast_types.type_expr list -> bool
val equal_type_expr :
  Poppy_parser.Ast_types.type_expr ->
  Poppy_parser.Ast_types.type_expr -> bool
val find_global : env -> env
val lookup_struct :
  env ->
  StructNameMap.Key.t -> (Poppy_parser.Ast.struct_defn, Base.Error.t) result
val lookup_trait :
  env ->
  TraitNameMap.Key.t -> (Poppy_parser.Ast.trait_defn, Base.Error.t) result
val lookup_method :
  env ->
  MethodNameMap.Key.t -> (Poppy_parser.Ast.method_defn, Base.Error.t) result
val lookup_function :
  env ->
  FunctionNameMap.Key.t ->
  (Poppy_parser.Ast.function_defn, Base.Error.t) result
val lookup_impl :
  env ->
  StructTraitMap.Key.t ->
  (Poppy_parser.Ast_types.Trait_name.t list, Base.Error.t) result
val lookup_var :
  env ->
  Poppy_parser.Ast_types.Var_name.t ->
  Poppy_parser.Ast_types.loc ->
  (Poppy_parser.Ast_types.type_expr, Base.Error.t) result
val lookup_method_signature :
  Poppy_parser.Ast.trait_defn ->
  Poppy_parser.Ast_types.Method_name.t ->
  (Poppy_parser.Ast_types.method_signature, Base.Error.t) result
val lookup_method_in_impl :
  env ->
  Poppy_parser.Ast_types.Struct_name.t ->
  MethodNameMap.Key.t -> (Poppy_parser.Ast.method_defn, Base.Error.t) result
val get_method_map :
  Poppy_parser.Ast.method_defn list ->
  Poppy_parser.Ast.method_defn MethodNameMap.t
val get_struct_trait_map :
  env -> Poppy_parser.Ast_types.Trait_name.t list StructTraitMap.t
val update_env_with_maps :
  env ->
  method_map:Poppy_parser.Ast.method_defn MethodNameMap.t ->
  struct_trait_map:Poppy_parser.Ast_types.Trait_name.t list StructTraitMap.t ->
  env
val add_struct_to_global : env -> Poppy_parser.Ast.struct_defn -> env
val add_trait_to_global : env -> Poppy_parser.Ast.trait_defn -> env
val add_method_to_global : env -> Poppy_parser.Ast.method_defn -> env
val add_function_to_global : env -> Poppy_parser.Ast.function_defn -> env
val add_impl_to_global : env -> Poppy_parser.Ast.impl_defn -> env
val add_function_scope :
  env -> Poppy_parser.Ast_types.type_expr VarNameMap.t -> env
val add_block_scope :
  env -> Poppy_parser.Ast_types.type_expr VarNameMap.t -> env
val add_var_to_block_scope :
  env -> VarNameMap.Key.t -> Poppy_parser.Ast_types.type_expr -> env
val add_this_to_block_scope :
  env -> Poppy_parser.Ast_types.Struct_name.t -> env
val add_params_to_scope :
  env -> Poppy_parser.Ast_types.param list -> (env, 'a) result
val remove_scope : env -> (env, Base.Error.t) result
val elem_in_list :
  Poppy_parser.Ast_types.Capability_name.t ->
  Poppy_parser.Ast_types.Capability_name.t list -> bool
val get_struct_defn :
  Poppy_parser.Ast_types.Struct_name.t ->
  Typed_ast.struct_defn list -> Typed_ast.struct_defn
val get_obj_struct_defn :
  Poppy_parser.Ast_types.Var_name.t ->
  env ->
  Poppy_parser.Ast_types.loc ->
  (Poppy_parser.Ast.struct_defn, Base.Error.t) result
val get_struct_capabilities :
  StructNameMap.Key.t -> env -> Poppy_parser.Ast_types.capability list
val get_struct_fields :
  StructNameMap.Key.t -> env -> Poppy_parser.Ast_types.field_defn list
val get_method_field_capabilities :
  StructNameMap.Key.t -> env -> Poppy_parser.Ast_types.capability list
val has_duplicates : 'a list -> equal:('a -> 'a -> bool) -> bool
val check_no_duplicate_var_declarations_in_block :
  Poppy_parser.Ast.expr list ->
  Poppy_parser.Ast_types.loc -> (unit, Base.Error.t) result
val check_variable_declarable :
  Poppy_parser.Ast_types.Var_name.t ->
  Poppy_parser.Ast_types.loc -> (unit, Base.Error.t) result
val check_identifier_assignable :
  Poppy_parser.Ast.identifier ->
  env -> Poppy_parser.Ast_types.loc -> (unit, Base.Error.t) result
