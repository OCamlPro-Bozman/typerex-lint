type t =
  | Expression of Parsetree.expression
  | Expression_opt of Parsetree.expression option
  | String of string
  | Pattern of Parsetree.pattern
  | Value_binding of Parsetree.value_binding
  | Value_bindings of Parsetree.value_binding list
  | Structure_item of Parsetree.structure_item
  | Structure of Parsetree.structure

let to_string =
  let open Pprintast in
  let to_string printer value =
    printer Format.str_formatter value;
    Format.flush_str_formatter ()
  in
  function
  | Expression e -> to_string expression e
  | String i -> i
  | Pattern p -> to_string pattern p
  | _ -> assert false

let from_structure e = Structure e
