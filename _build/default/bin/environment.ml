(* environment.ml *)

open Types
open Ast  (* To refer to the `expr` type *)

type valor = 
  | VNum of int
  | VBool of bool
  | VPair of valor * valor
  | VClos  of ident * expr * renv
  | VRClos of ident * ident * expr * renv 
  | Vlist of valor * valor
  | VJust of valor
  | VHead of valor
  | VTail of valor
  | VNothing
  | VNil

and renv = (ident * valor) list  

(* ambientes *)
            
type tyenv = (ident * tipo) list 
    
let empty_amb : tyenv = []
    
let rec lookup g x : tipo option = 
  match g with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some t else lookup tl x
  
let update (g: tyenv) (x:ident) (t:tipo) : tyenv = 
  (x,t) :: g

  
               
(* restrições de tipo  *)
  
type constraints = (tipo * tipo) list 

(* 
   a lista 
       [ (t1,t2) ; (u1,u2) ] 
   representa o conjunto de restrições 
       { t1=t2, u1=u2 } 
 *)
                 

(* imprime restrições *)

                 

(* código para geração de novas variáveis de tipo *)
                 
let lastvar : int ref = ref 0

let newvar (u:unit) : int =
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

  | Num n -> ([],TyInt)

  | True  -> ([],TyBool)

  | False -> ([],TyBool)

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
     
  | Nil ->
      let tA = newvar() in
      ([], TyList (TyVar tA))
     
  | Cons (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp2,TyList tp1)], tp2)
             
  | Head e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyVar tA)
         
  | Tail e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], tp1)

  | Raise ->
      let tA = newvar() in
      ([],TyVar tA)
          
  | Try (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp1,tp2)],tp1)      




(* substituições *)
                      
type subst = (int*tipo) list


(* aplicação de substituição a tipo *)
           
let rec appsubs (s:subst) (tp:tipo) : tipo =
  match tp with
    TyInt           -> TyInt
  |  TyBool          -> TyBool       
  |  TyFn   (t1,t2)  -> TyFn   (appsubs s t1, appsubs s t2)
  |  TyPair (t1,t2)  -> TyPair (appsubs s t1, appsubs s t2)
  |  TyVar  x        -> (match lookup s x with
        None     -> TyVar x
      | Some tp' -> tp')
  |  TyList t1       -> TyList (appsubs s t1)                      

                
(* composição de substituições: s1 o s2  *)
let rec compose (s1:subst) (s2:subst) : subst =
  let r1 = List.map (fun (x,tp) -> (x, appsubs s1 tp))    s2 in
  let (vs,_) = List.split s2                                 in
  let r2 = List.filter (fun (x,y) -> not (List.mem x vs)) s1 in
  r1@r2


(* aplicação de substituição a coleção de constraints *)
                                                              
(* s [ (t11,t12), ....(tn1,tn2]) = [(s t11,s t12), ....(s tn1, s tn2] *) 

let rec appconstr (s:subst) (c:constraints) : constraints =
  List.map (fun (a,b) -> (appsubs s a,appsubs s b)) c

  
(* testa se variável ocorre em tipo *)
                 
let rec var_in_tipo (v:int) (tp:tipo) : bool =
  match tp with
    TyInt           -> false
  |  TyBool          -> false       
  |  TyFn   (tp1,tp2)  -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyPair (tp1,tp2)  -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyVar  x        -> v==x
  |  TyList tp1       -> var_in_tipo v tp1                      
 
(* unificação *)
              
exception UnifyFail of (tipo*tipo)
           
let rec unify (c:constraints) : subst =
  match c with
    [] -> []
  | (TyInt,    TyInt )   ::c' -> unify c'
  | (TyBool,   TyBool)   ::c' -> unify c'
  | (TyVar x1, TyVar x2) ::c' when x1==x2 -> unify c'
                             
  | (TyFn(tp1,tp2),  TyFn(tp3,tp4)  )::c' -> unify ((tp1,tp3)::(tp2,tp4)::c')
  | (TyPair(tp1,tp2),TyPair(tp3,tp4))::c' -> unify ((tp1,tp3)::(tp2,tp4)::c') 
                                               
  | (TyList tp1,     TyList tp2)     ::c' -> unify ((tp1,tp2)::c')
  
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

  | (tp1,tp2)::c' -> raise (UnifyFail(tp1,tp2))


(* INFERÊNCIA DE TIPOS - CHAMADA PRINCIPAL *)
       
let type_infer (e:expr) : unit =
  print_string "\nexpr:\n";
  print_string (expr_str e);
  print_string "\n\n";
  try
    let (c,tp) = collect [] e  in
    let s      = unify c       in
    let tf     = appsubs s tp  in
    print_string "\nRestrições:\n";
    print_constr c;
    print_string "Tipo inferido: ";    
    print_string (tipo_str tp);
    print_string "\n\nSubstituição:\n";
    print_subst s;
    print_string "Tipo inferido (após subs): ";
    print_string (tipo_str tf);
    print_string "\n\n"

  with
    
  | CollectFail x   -> print_string "Erro: variável ";
      print_string x;
      print_string "não declarada!\n\n"
                     
  | UnifyFail (tp1,tp2) -> print_string "Erro: impossível unificar os tipos\n* ";
      print_string (tipo_str tp1);
      print_string "\n* ";
      print_string (tipo_str tp2);
      print_string "\n\n"
