open! Core
open Css_parser

type 'a with_loc = 'a * Location.t

let sexp_of_with_loc sexp_of_a (a, _) = sexp_of_a a


module Dimension = struct
  type t = Types.dimension = 
    | Length
    | Angle
    | Time
    | Frequency

  let sexp_of_t =  function
  | Length -> [%sexp Length]
  | Angle -> [%sexp Angle]
  | Time -> [%sexp Time]
  | Frequency -> [%sexp Frequency]
end 

module  Component_value = struct
  type t = Types.Component_value.t = 
    | Paren_block of t with_loc list
    | Bracket_block of t with_loc list
    | Percentage of string
    | Ident of string
    | String of string
    | Uri of string
    | Operator of string
    | Delim of string
    | Function of string with_loc * t with_loc list with_loc
    | Hash of string
    | Number of string
    | Unicode_range of string
    | Float_dimension of (string * string * Dimension.t)
    | Dimension of (string * string)

  let rec sexp_of_t = function
  | Paren_block t_list -> [%sexp Paren_block (t_list : t with_loc list)]
  | Bracket_block t_list -> [%sexp Bracket_block (t_list : t with_loc list)]
  | Percentage s -> [%sexp Percentage (s : string)]
  | Ident s -> [%sexp Ident (s : string)]
  | String s -> [%sexp String (s : string)]
  | Uri s -> [%sexp Uri (s : string)]
  | Operator s -> [%sexp Operator (s : string)]
  | Delim s -> [%sexp Delim (s : string)]
  | Function (s, t_list) ->
    [%sexp Function (s, t_list : string with_loc * t with_loc list with_loc)]
  | Hash s -> [%sexp Hash (s : string)]
  | Number s -> [%sexp Number (s : string)]
  | Unicode_range s -> [%sexp Unicode_range (s : string)]
  | Float_dimension (s1, s2, dim) ->
    [%sexp Float_dimension (s1, s2, dim : string * string * Dimension.t)]
  | Dimension (s1, s2) -> [%sexp Dimension (s1, s2 : string * string)]

end

module Selector = struct
  type t = Component_value.t 
  
 let sexp_of_t = assert false
 end
 
module rec Declaration : sig 
  type t = 
  { name : string with_loc
  ; value : Component_value.t with_loc list with_loc
  ; important : bool with_loc
  ; loc : Location.t
  } [@@deriving sexp_of]

  val convert : Types.Declaration.t -> t
end 
= struct
  type t = 
    { name : string with_loc
    ; value : Component_value.t with_loc list with_loc
    ; important : bool with_loc
    ; loc : Location.t
    }
  let sexp_of_t = function
  | { name; value; important; loc = _ } ->
    [%sexp
      { name : string with_loc
      ; value : Component_value.t with_loc list with_loc
      ; important : bool with_loc
      }]

    let convert { Types.Declaration.name; value; important; loc}= 
    { name; value; important; loc}

end 

and Brace_block : sig 
  type t =
    | Empty
    | Declaration_list of Declaration_list.t
    | Stylesheet of Stylesheet.t
  [@@deriving sexp_of]
  val convert : Types.Brace_block.t -> t
end
  = struct
  type t = 
    | Empty
    | Declaration_list of Declaration_list.t
    | Stylesheet of Stylesheet.t

  let sexp_of_t = function
  | Empty -> [%sexp Empty]
  | Declaration_list d -> [%sexp Declaration_list (d : Declaration_list.t)]
  | Stylesheet s -> [%sexp Stylesheet (s : Stylesheet.t)]

  let convert = function
    | Types.Brace_block.Empty -> Empty
    | Declaration_list d -> Declaration_list (Declaration_list.convert d)
    | Stylesheet s -> Stylesheet (Stylesheet.convert s)
end 

and At_rule : sig 
  type t =
    { name : string with_loc
    ; prelude : Selector.t with_loc list with_loc
    ; block : Brace_block.t
    ; loc : Location.t
    } [@@deriving sexp_of]

    val convert : Types.At_rule.t -> t
end
  = struct
  type t  =
    { name : string with_loc
    ; prelude : Selector.t with_loc list with_loc
    ; block : Brace_block.t
    ; loc : Location.t
    }

  let sexp_of_t = function
  | { name; prelude; block; loc = _ } ->
    [%sexp
      { name : string with_loc
      ; prelude : Selector.t with_loc list with_loc
      ; block : Brace_block.t
      }]
    
  let convert {Types.At_rule.name; prelude; block; loc} =
     { name; prelude; block = Brace_block.convert block; loc }
end 

and Declaration_list : sig 
  type kind =
    | Declaration of Declaration.t
    | At_rule of At_rule.t
  [@@deriving sexp_of]

  type t = kind list with_loc [@@deriving sexp_of]
  val convert : Types.Declaration_list.t -> t
  (* val convert_kind : Types.Declaration_list.kind -> kind *)

end
  = struct
  type kind = 
    | Declaration of Declaration.t
    | At_rule of At_rule.t

  and t = kind list with_loc

  let sexp_of_kind = function
  | Declaration d -> [%sexp Declaration (d : Declaration.t)]
  | At_rule a -> [%sexp At_rule (a : At_rule.t)]

  let sexp_of_t = function
  | d -> [%sexp (d : kind list with_loc)]

  let convert_kind = function 
    | Types.Declaration_list.Declaration d -> Declaration ( Declaration.convert d)
    | At_rule a -> At_rule (At_rule.convert a)

  let convert (t, with_loc) =  
    ((List.map t ~f:(fun v -> convert_kind v )), with_loc)

  end 

and Style_rule : sig
  type t =
    { prelude : Selector.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : Location.t
    }
    [@@deriving sexp_of]
    val convert : Types.Style_rule.t -> t
  end
  = struct
  type t = 
    { prelude : Selector.t with_loc list with_loc
    ; block : Declaration_list.t
    ; loc : Location.t
    }
  let sexp_of_t = function
  | { prelude; block; loc = _ } ->
    [%sexp { prelude : Selector.t with_loc list with_loc; block : Declaration_list.t }]

  let convert {Types.Style_rule.prelude; block; loc} = {prelude; block = Declaration_list.convert block; loc}
  (* let convert (t, with_loc) = ((List.map t ~f:(fun v -> Rule.convert v )), with_loc) *)

    end  

and Rule : sig
  type t =
    | Style_rule of Style_rule.t
    | At_rule of At_rule.t
  [@@deriving sexp_of]

  val convert : Types.Rule.t -> t
end
  = struct
  type t = 
    | Style_rule of Style_rule.t
    | At_rule of At_rule.t

  let sexp_of_t = function
  | Style_rule s -> [%sexp Style_rule (s : Style_rule.t)]
  | At_rule s -> [%sexp At_rule (s : At_rule.t)]

  let convert = function
    | Types.Rule.Style_rule s -> Style_rule (Style_rule.convert s)
    | At_rule a -> At_rule (At_rule.convert a)
end 

and Stylesheet : sig
  type t = Rule.t list with_loc [@@deriving sexp_of]

  val to_string_hum : t -> string
  val to_string_minified : t -> string
  val of_string : ?pos:Source_code_position.t -> string -> t
  val sexp_of_t : t -> Sexp.t
  val convert : Types.Stylesheet.t -> t
end
  = struct
  type t = Rule.t list with_loc

  (* let to_string which t =
    let buffer = Buffer.create 64 in
    let formatter = Format.formatter_of_buffer buffer in
    Css_printer.Print.(css which) formatter t;
    Buffer.contents buffer
  ;; *)

  (* let to_string_hum = to_string Css_printer.Print.pretty_printer *)
  let to_string_hum = assert false ;;
  let to_string_minified = assert false ;;
    (* to_string Css_printer.Print.minify_printer *)
  let convert (t, with_loc) = 
    ((List.map t ~f:(fun v -> Rule.convert v )), with_loc) ;;

  let of_string ?pos s =
    (* the parser produces different output depending on if there is 
       a leading space or not.  They're equivalent semantically, but 
       I can add this to remove ambiguity.
       
       If the string input is not a CSS, then it should produce an error - how do we determine if's 
        CSS file? (use a list of selectors?)
       if string doesn't begin with a css selector = throw an error*)
    let s = " " ^ s in
    let type_stylesheet = Css_parser.Parser.parse_stylesheet ?pos s in 
    let converted_parser = convert (type_stylesheet) in
    converted_parser
  ;;

  let sexp_of_t = function
  | s -> [%sexp (s : Rule.t list with_loc)]
;;
end
