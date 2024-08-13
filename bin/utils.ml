let rec tipo_str (tp : tipo) : string =
  match tp with
  | TyInt ->
      "int"
  | TyBool ->
      "bool"
  | TyFn (t1, t2) ->
      "(" ^ tipo_str t1 ^ " -> " ^ tipo_str t2 ^ ")"
  | TyPair (t1, t2) ->
      "(" ^ tipo_str t1 ^ " * " ^ tipo_str t2 ^ ")"
  | TyEither (t1, t2) ->
      "either " ^ tipo_str t1 ^ " " ^ tipo_str t2
  | TyList t ->
      tipo_str t ^ " list"
  | TyMaybe t ->
      "maybe " ^ tipo_str t
  | TyVar n ->
      "X" ^ string_of_int n

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
        | And ->
            "&&"
        | Or ->
            "||"
        | Div ->
            "/"
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
      "(let " ^ x ^ " = " ^ expr_str e1 ^ "\nin " ^ expr_str e2 ^ ")"
  | LetRec (f, x, e1, e2) ->
      "(let rec " ^ f ^ " = fn " ^ x ^ " => " ^ expr_str e1 ^ "\nin "
      ^ expr_str e2 ^ ")"
  (* List related *)
  | Nil ->
      "Nil"
  | MatchList (e1, x, e2, xs, e3) ->
      "match " ^ expr_str e1 ^ " with nil => " ^ expr_str e2 ^ "| " ^ x ^ " :: "
      ^ xs ^ " => " ^ expr_str e3
  (* Maybe related *)
  | Nothing ->
      "nothing"
  | Just e ->
      "Just " ^ expr_str e

(* Auxiliary function to convert types to string *)
let rec ttos (t:tipo) : string =
  match t with
    TyInt  -> "int"
  | TyBool -> "bool"
  | TyFn(t1,t2)   ->  "("  ^ (ttos t1) ^ " --> " ^ (ttos t2) ^ ")"
  | TyPair(t1,t2) ->  "("  ^ (ttos t1) ^ " * "   ^ (ttos t2) ^ ")"
  | _ ->  raise (TypeError "Tipo não existente")


let rec print_constr (c:constraints) =
  match c with
    []       -> print_string "\n";
  | (t1,t2)::c' -> 
      print_string (tipo_str t1);
      print_string " = ";
      print_string (tipo_str t2);
      print_string "\n";
      print_constr c'


(* para imprimir substituições  *)
                
let rec print_subst (s:subst) =
  match s with
    []       -> print_string "\n";
  | (a,b)::s' -> 
      print_int a;
      print_string " |-> ";
      print_string (tipo_str b);
      print_string "\n";
      print_subst s'


           
