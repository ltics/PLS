open Core.Std

type term =
  | Ident of string
  | Lambda of string * term
  | Apply of term * term
  | Let of string * term * term
  | LetRec of string * term * term

let rec term_to_string = function
  | Ident(name) -> name
  | Lambda(v, body) ->
      Printf.sprintf "fn %s => %s" v (term_to_string body)
  | Apply(fn, arg) ->
      Printf.sprintf "%s %s" (term_to_string fn) (term_to_string arg)
  | Let(v, defn, body) ->
      Printf.sprintf "let %s = %s in %s" v (term_to_string defn) (term_to_string body)
  | LetRec(v, defn, body) ->
      Printf.sprintf "letrec %s = %s in %s" v (term_to_string defn) (term_to_string body)

exception TypeError of string
exception ParseError of string

type tyvar =
  { tyvar_id : int;
    mutable tyvar_instance : ty option;
    mutable tyvar_name: string option;
  }
and tyop =
  { tyop_name : string;
    tyop_types: ty list;
  }
and ty =
  | TypeVariable of tyvar
  | TypeOperator of tyop

let next_variable_id = ref 0

let make_variable () =
  let new_var = { tyvar_id = !next_variable_id; tyvar_instance = None; tyvar_name = None } in
  next_variable_id := !next_variable_id + 1;
  TypeVariable(new_var)

let next_unique_name = ref "a"

let variable_name v = match v with
  | { tyvar_name = Some(name); _ } -> name
  | { tyvar_name = None; _ } ->
      let new_var_name = !next_unique_name in
      v.tyvar_name <- Some(new_var_name);
      next_unique_name := String.of_char @@ Option.value_exn (Char.of_int @@ (Char.to_int !next_unique_name.[0]) + 1);
      new_var_name

let rec type_to_string = function
  | TypeVariable({ tyvar_instance = Some(instance); _ }) -> type_to_string instance
  | TypeVariable({ tyvar_instance = None; _ } as v) -> variable_name v
  | TypeOperator({ tyop_name; tyop_types }) ->
      let length = List.length tyop_types in
      if length = 0
      then tyop_name
      else if length = 2
      then Printf.sprintf "(%s %s %s)"
            (type_to_string (List.nth_exn tyop_types 0))
            tyop_name
            (type_to_string (List.nth_exn tyop_types 1))
      else Printf.sprintf "%s %s" tyop_name
            (String.concat ~sep:" " (List.map ~f:type_to_string tyop_types))

type env = (string * ty) list

let make_fun_type from_type to_type = TypeOperator({ tyop_name = "->"; tyop_types = [from_type; to_type] })
let int_type = TypeOperator({ tyop_name = "int"; tyop_types = [] })
let bool_type = TypeOperator({ tyop_name = "bool"; tyop_types = [] })

let rec prune t = match t with
  | TypeVariable({ tyvar_instance = Some(instance); _ } as v) ->
      let new_instance = prune instance in
        v.tyvar_instance <- Some(new_instance);
        new_instance
  | _ -> t

let rec occurs_in_type v t2 = match prune t2 with
  | pruned_t2 when pruned_t2 = v -> true
  | TypeOperator({ tyop_types; _ }) -> occurs_in v tyop_types
  | _ -> false
and occurs_in t types =
  List.exists ~f:(fun t2 -> occurs_in_type t t2) types

let is_integer_literal name =
  match
    try Some(int_of_string name)
    with Failure(_) -> None
  with
  | None -> false
  | Some(_) -> true

let is_generic v non_generic = not @@ occurs_in v (Set.to_list non_generic)

let fresh t non_generic =
  let table = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  let rec freshrec tp =
    match prune tp with
    | TypeVariable(_) as p ->
      if is_generic p non_generic then
        match Hashtbl.find table p with
        | None ->
          let new_var = make_variable () in
            ignore (Hashtbl.add table ~key:p ~data:new_var);
            new_var
        | Some(var) -> var
      else p
    | TypeOperator({ tyop_types; _ } as op) ->
        TypeOperator({ op with tyop_types = List.map ~f:freshrec tyop_types })
  in freshrec t

let get_type name env non_generic =
  match List.Assoc.find env name with
  | Some(var) -> fresh var non_generic
  | None ->
      if is_integer_literal name
      then int_type
      else raise (ParseError ("Undefined symbol " ^ name))

let rec unify t1 t2 =
  match prune t1, prune t2 with
  | (TypeVariable(v) as a), b ->
      if a <> b then
        if occurs_in_type a b
        then raise (TypeError "Recursive unification");
        v.tyvar_instance <- Some(b)
  | (TypeOperator(_) as a), (TypeVariable(_) as b) -> unify b a
  | (TypeOperator({ tyop_name = name1; tyop_types = types1 }) as a), (TypeOperator({ tyop_name = name2; tyop_types = types2 }) as b) ->
    if (name1 <> name2 || List.length types1 <> List.length types2)
    then raise (TypeError (Printf.sprintf
      "Type mismatch %s != %s" (type_to_string a) (type_to_string b)));
    ignore (List.map2_exn ~f:unify types1 types2)

let analyse term' env' =
  let rec analyserec term env non_generic = match term with
  | Ident(name) -> get_type name env non_generic
  | Apply(fn, arg) ->
    let fun_ty = analyserec fn env non_generic in
    let arg_ty = analyserec arg env non_generic in
    let ret_ty = make_variable () in
    unify (make_fun_type arg_ty ret_ty) fun_ty;
    ret_ty
  | Lambda(arg, body) ->
    let arg_ty = make_variable () in
    let ret_ty = analyserec body (List.Assoc.add env arg arg_ty) (Set.add non_generic arg_ty) in
    make_fun_type arg_ty ret_ty
  | Let(v, defn, body) ->
    let defn_ty = analyserec defn env non_generic in
    analyserec body (List.Assoc.add env v defn_ty) non_generic
  | LetRec(v, defn, body) ->
    let new_ty = make_variable () in
    let new_env = (List.Assoc.add env v new_ty) in
    let defn_ty = analyserec defn new_env (Set.add non_generic new_ty) in
    unify new_ty defn_ty;
    analyserec body new_env non_generic
  in
  analyserec term' env' (Set.empty ~comparator:Comparator.Poly.comparator)

let try_exp env term =
  Printf.printf "%s : " (term_to_string term);
  let result =
    try
      let t = analyse term env in
      type_to_string t
    with
      | ParseError(msg) | TypeError(msg) -> msg in
  Printf.printf "%s\n" result

let () =
  let var1 = make_variable () in
  let var2 = make_variable () in
  let pair_ty = TypeOperator({ tyop_name = "*"; tyop_types = [var1; var2] }) in
  let var3 = make_variable () in

  let my_env =
  [ ("pair", make_fun_type var1 (make_fun_type var2 pair_ty));
    ("true", bool_type);
    ("cond", make_fun_type bool_type (make_fun_type var3 (make_fun_type var3 var3)));
    ("zero", make_fun_type int_type bool_type);
    ("pred", make_fun_type int_type int_type);
    ("times", make_fun_type int_type (make_fun_type int_type int_type))
  ] in

  let pair = Apply(Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))), Apply(Ident("f"), Ident("true"))) in
  let examples =
  [ LetRec("factorial", (* letrec factorial = *)
      Lambda("n",    (* fn n => *)
        Apply(
          Apply(   (* cond (zero n) 1 *)
            Apply(Ident("cond"),     (* cond (zero n) *)
              Apply(Ident("zero"), Ident("n"))),
            Ident("1")),
          Apply(    (* times n *)
            Apply(Ident("times"), Ident("n")),
            Apply(Ident("factorial"),
              Apply(Ident("pred"), Ident("n")))
          )
        )
      ),      (* in *)
      Apply(Ident("factorial"), Ident("5"))
    );

    (* Should fail: *)
    (* fn x => (pair(x(3) (x(true))) *)
      Lambda("x",
        Apply(
          Apply(Ident("pair"),
            Apply(Ident("x"), Ident("3"))),
          Apply(Ident("x"), Ident("true"))));

    (* pair(f(3), f(true)) *)
      Apply(
        Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
        Apply(Ident("f"), Ident("true")));

    (* letrec f = (fn x => x) in ((pair (f 4)) (f true)) *)
      Let("f", Lambda("x", Ident("x")), pair);

    (* fn f => f f (fail) *)
      Lambda("f", Apply(Ident("f"), Ident("f")));

    (* let g = fn f => 5 in g g *)
      Let("g",
        Lambda("f", Ident("5")),
        Apply(Ident("g"), Ident("g")));

    (* example that demonstrates generic and non-generic variables: *)
    (* fn g => let f = fn x => g in pair (f 3, f true) *)
      Lambda("g",
           Let("f",
             Lambda("x", Ident("g")),
             Apply(
              Apply(Ident("pair"),
                  Apply(Ident("f"), Ident("3"))
              ),
              Apply(Ident("f"), Ident("true")))));

    (* Function composition *)
    (* fn f (fn g (fn arg (f g arg))) *)
      Lambda("f", Lambda("g", Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))))
  ] in
  List.iter ~f:(try_exp my_env) examples