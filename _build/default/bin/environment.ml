(* environment.ml *)
open Types
open Utils

(* ambientes *) 
let rec lookup g x = 
  match g with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some t else lookup tl x
  
let update (g: tyenv) (x:ident) (t:tipo) : tyenv = 
  (x,t) :: g

(* código para geração de novas variáveis de tipo *)
let lastvar : int ref = ref 0

let newvar (_:unit) : int =
  let x:int = !lastvar
  in lastvar := (x+1) ; x

(*  coleta de restrições *)
exception CollectFail of string
       
let rec collect (g:tyenv) (e:expr) : (constraints * tipo)  =
  match e with
    Var x   ->
      (match lookup g x with
         None    -> raise (CollectFail x)
       | Some tp -> ([],tp))                    
  | Num _ -> ([],TyInt)
  | Bool _ -> ([],TyBool)
  | Binop (o,e1,e2) ->  
      if List.mem o [Sum;Sub;Mult]
      then
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyInt)
      else   
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyBool)    
  | Pair (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2, TyPair(tp1,tp2))    
  | Fst e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in
      (c1@[(tp1,TyPair(TyVar tA, TyVar tB))], TyVar tA)
  | Snd e1 ->
      let tA = newvar() in
      let tB = newvar() in
      let (c1,tp1) = collect g e1 in         
      (c1@[(tp1,TyPair(TyVar tA,TyVar tB))], TyVar tB)         
  | If (e1,e2,e3) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      let (c3,tp3) = collect g e3 in         
      (c1@c2@c3@[(tp1,TyBool);(tp2,tp3)], tp2)
  | Fn (x,e1) ->
      let tA       = newvar()        in
      let g'       = (x,TyVar tA)::g in
      let (c1,tp1) = collect g' e1   in
      (c1, TyFn(TyVar tA,tp1))
  | App (e1,e2) -> 
      let (c1,tp1) = collect  g e1  in
      let (c2,tp2) = collect  g e2  in
      let tX       = newvar()       in 
      (c1@c2@[(tp1,TyFn(tp2, TyVar tX))]
      , TyVar tX) 
  | Let (x,e1,e2) ->
      let (c1,tp1) = collect  g e1   in
      let tX       = newvar()        in 
      let g'       = (x,TyVar tX)::g       in
      let (c2,tp2) = collect  g' e2  in
      (c1@c2@[(TyVar tX,tp1)], tp2)
  | LetRec (f,x,e1,e2) ->
      let tX = newvar() in
      let tY = newvar() in
      let g2 = (f,TyVar tX)::g in
      let g1 = (x,TyVar tY)::g2               in
      let (c1,tp1) = collect g1 e1            in
      let (c2,tp2) = collect g2 e2            in
      (c1@c2@[(TyVar tX,TyFn(TyVar tY,tp1))],   tp2)
  | Nothing -> let tX = newvar() in 
      ([], TyMaybe (TyVar tX))
 (*TODO 
  | Just ->
      let t = newvar() in
  | MatchList ->
  | MatchJust ->
*)
  | Nil ->
      let tX = newvar() in
      ([], TyList (TyVar tX))
  (*TODO
  | Cons (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp2,TyList tp1)], tp2)*)

(* aplicação de substituição a tipo *)         
let rec appsubs (s:subst) (tp:tipo) : tipo =
  match tp with
    TyInt           -> TyInt
  | TyBool          -> TyBool       
  | TyFn   (t1,t2)  -> TyFn   (appsubs s t1, appsubs s t2)
  | TyPair (t1,t2)  -> TyPair (appsubs s t1, appsubs s t2)
  | TyVar  x        -> (match lookup s x with
                              None     -> TyVar x
                            | Some tp' -> tp')
  | TyList t1       -> TyList (appsubs s t1)
  | TyMaybe t1      -> TyMaybe (appsubs s t1) 

                
(* composição de substituições: s1 o s2  *)
let compose (s1:subst) (s2:subst) : subst =
  let r1 = List.map (fun (x,tp) -> (x, appsubs s1 tp))    s2 in
  let (vs,_) = List.split s2                                 in
  let r2 = List.filter (fun (x,_) -> not (List.mem x vs)) s1 in
  r1@r2


(* aplicação de substituição a coleção de constraints *)
                                                              
(* s [ (t11,t12), ....(tn1,tn2]) = [(s t11,s t12), ....(s tn1, s tn2] *) 
let appconstr (s:subst) (c:constraints) : constraints =
  List.map (fun (a,b) -> (appsubs s a,appsubs s b)) c

  
(* testa se variável ocorre em tipo *)
let rec var_in_tipo (v:int) (tp:tipo) : bool =
  match tp with
     TyInt             -> false
  |  TyBool            -> false       
  |  TyFn    (tp1,tp2) -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyPair  (tp1,tp2) -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyVar   x         -> v == x
  |  TyList  tp1       -> var_in_tipo v tp1 
  |  TyMaybe t         -> var_in_tipo v t


(* unificação *)
exception UnifyFail of (tipo*tipo)
           
let rec unify (c:constraints) : subst =
  match c with
    [] -> []
  | (TyInt, TyInt ) ::c' -> unify c'
  | (TyBool,TyBool) ::c' -> unify c'
  | (TyVar x1, TyVar x2) ::c' when x1==x2 -> unify c'
  | (TyFn(tp1,tp2),  TyFn(tp3,tp4)  ) ::c' -> unify ((tp1,tp3)::(tp2,tp4)::c')
  | (TyPair(tp1,tp2), TyPair(tp3,tp4)) ::c' -> unify ((tp1,tp3)::(tp2,tp4)::c')                          
  | (TyList tp1, TyList tp2) ::c' -> unify ((tp1,tp2)::c')
  | (TyMaybe tp1, TyMaybe tp2) ::c' -> unify ((tp1,tp2)::c')
  | (TyVar x1, tp2)::c' -> 
      if var_in_tipo x1 tp2
      then raise (UnifyFail(TyVar x1, tp2))
      else compose
          (unify (appconstr [(x1,tp2)] c'))
          [(x1,tp2)]  
  | (tp1,TyVar x2)::c'  -> 
      if var_in_tipo x2 tp1
      then raise (UnifyFail(tp1,TyVar x2))
      else compose
          (unify (appconstr [(x2,tp1)] c'))
          [(x2,tp1)] 
  | (tp1,tp2)::_' -> raise (UnifyFail(tp1,tp2))


(* INFERÊNCIA DE TIPOS - CHAMADA PRINCIPAL *)
let type_infer (e:expr) : tipo =
  print_string "expr:";
  print_string (expr_str e);
  print_string "\n";
  try
    let (c,tp) = collect [] e  in
    let s      = unify c       in
    let tf     = appsubs s tp  in
    print_string "Restrições:";
    print_constr c;
    print_string "\n";
    print_string "Tipo inferido: ";    
    print_string (ttos tp);
    print_string "\n";
    print_string "Substituição:";
    print_subst s;
    print_string "\n";
    print_string "Tipo inferido (após subs): ";
    print_string (ttos tf);
    print_string "\n\n";
    tf
  with
    
  | CollectFail x   -> print_string "Erro: variável ";
      print_string x;
      print_string "não declarada!\n\n";
      TyBool              
  | UnifyFail (tp1,tp2) -> print_string "Erro: impossível unificar os tipos\n* ";
      print_string (ttos tp1);
      print_string "\n* ";
      print_string (ttos tp2);
      print_string "\n\n";
      TyBool 
