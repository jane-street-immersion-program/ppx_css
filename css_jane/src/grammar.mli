(* open! Core

type 'a with_loc = 'a * Location.t

module rec Dimension : sig
  type t =
    | Length
    | Angle
    | Time
    | Frequency
  [@@deriving sexp_of]
end

and Component_value : sig
    type combinator =
    | GT
    | Plus
    | Tilde
    | ['|''|'];;
    type type_selector = 
    | Wq_name
    | Ns_prefix
    | Star_Sign='*';;
    type simple_selector = 
    | Type_selector
    | Subclass_selector ;;
    type selector_list =
    | Complex_selector_list;;
    type complex_selector_list = 
    | Complex_selector;;
    type compound_selector = 
    | Compound_selector;;
    type simple_selector_list = 
    | Simple_selector;;
    type 

    [@@deriving sexp_of]
end *)
