(* utils.ml *)
open Types

let rec expr_str (e : expr) : string =
  match e with
  | Num n ->
      string_of_int n
  | Bool b ->
      string_of_bool b
  | Var x ->
      x
  | Binop (o, e1, e2) ->
      let s =
        match o with
        | Sum ->
            "+"
        | Sub ->
            "-"
        | Mult ->
            "*"
        | Leq ->
            "<="
        | Geq ->
            ">="
        | Eq ->
            "="
        | Lt ->
            "<"
        | Gt ->
            ">"
        | _ -> raise(BugParser) 
      in
      "(" ^ expr_str e1 ^ " " ^ s ^ " " ^ expr_str e2 ^ ")"
  (* Pair related *)
  | Pair (e1, e2) ->
      "(" ^ expr_str e1 ^ ", " ^ expr_str e2 ^ ")"
  | Fst e1 ->
      "fst " ^ expr_str e1
  | Snd e1 ->
      "snd " ^ expr_str e1
  | If (e1, e2, e3) ->
      "(if " ^ expr_str e1 ^ " then " ^ expr_str e2 ^ " else " ^ expr_str e3
      ^ ")"
  (* Function related *)
  | Fn (x, e1) ->
      "(fn " ^ x ^ " => " ^ expr_str e1 ^ ")"
  | App (e1, e2) ->
      "(" ^ expr_str e1 ^ " " ^ expr_str e2 ^ ")"
  | Let (x, e1, e2) ->
      "(let " ^ x ^ " = " ^ expr_str e1 ^ " in " ^ expr_str e2 ^ ")"
  | LetRec (f, x, e1, e2) ->
      "(let rec " ^ f ^ " = fn " ^ x ^ " => " ^ expr_str e1 ^ "\nin "
      ^ expr_str e2 ^ ")"
  (* List related *)
  | Nil ->
      "Nil"
  | Cons (e1,e2) -> (expr_str e1) ^ "::" ^ (expr_str e2)
  | MatchList (e1, e2, x, xs, e3) ->
      "match " ^ expr_str e1 ^ " with nil => " ^ expr_str e2 ^ "| " ^ x ^ " :: "
      ^ xs ^ " => " ^ expr_str e3
  | MatchJust (e1, e2, x, e3) ->
      "match " ^ expr_str e1 ^ " with nothing => " ^ expr_str e2 ^ "|" ^ x ^ " => " ^ expr_str e3
  | Just e ->
      "Just " ^ expr_str e
  | Nothing -> "Nothing"
  | Pipe (e1, e2) -> expr_str e1 ^ "|>" ^ expr_str e2
(* Auxiliary function to convert types to string *)
let rec ttos (t:tipo) : string =
  match t with
  | TyInt ->
      "int"
  | TyBool ->
      "bool"
  | TyFn (t1, t2) ->
      "(" ^ ttos t1 ^ " -> " ^ ttos t2 ^ ")"
  | TyPair (t1, t2) ->
      "(" ^ ttos t1 ^ " * " ^ ttos t2 ^ ")"
  | TyList t ->
      ttos t ^ " list"
  | TyMaybe t ->
      "maybe " ^ ttos t
  | TyVar n ->
      "X" ^ string_of_int n

let rec print_constr (c:constraints) =
  match c with
    []       -> print_string "\n";
  | (t1,t2)::c' -> 
      print_string (ttos t1);
      print_string " = ";
      print_string (ttos t2);
      print_string "\n";
      print_constr c'

(* para imprimir substituições  *)
let rec print_subst (s:subst) =
  match s with
    []       -> print_string "\n";
  | (a,b)::s' -> 
      print_int a;
      print_string " |-> ";
      print_string (ttos b);
      print_subst s'

let rec vtos (v: valor) : string =
  match v with
    VNum n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VPair(v1, v2) ->
      "(" ^ vtos v1 ^ "," ^ vtos v2 ^ ")"
  | VClos _ ->  "fn"
  | VRClos _ -> "fn"
  | VNil -> "nil"
  | VNothing -> "nothing"
  | VCons (v1, v2) -> vtos v1 ^ "::" ^ vtos v2
  | VJust v1 -> "just" ^ vtos v1
