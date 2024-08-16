(* types.ml *)

exception BugParser
exception TypeError of string

type tipo =
    TyInt
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo
  | TyVar of int
  | TyList of tipo
  | TyMaybe of tipo


type ident = string

type tyenv = (ident * tipo) list 

type op = Sum | Sub | Mult | Div | Eq | Gt | Lt | Geq | Leq 

type expr =
  | Num of int
  | Var of ident
  | Bool of bool
  | Binop of op * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | If of expr * expr * expr
  | Fn of ident * expr
  | App of expr * expr
  | Let of ident * expr * expr
  | LetRec of ident * ident * expr * expr
  | Nothing
  (*TODO
  | Just of expr
  | MatchJust of expr * expr * ident * expr 
  *)
  | Nil
  (*TODO| MatchList of expr * expr * ident * ident * expr
  | Cons of expr * expr*)

type valor = 
  | VNum of int
  | VBool of bool
  | VPair of valor * valor
  | VClos  of ident * expr * renv
  | VRClos of ident * ident * expr * renv 
  | VNothing
  (*TODO
  | VJust of valor*)
  | VNil
 (*TODO | VCons of valor * valor*)

and renv = (ident * valor) list  
(* restrições de tipo  *)
  
type constraints = (tipo * tipo) list 

(* 
   a lista 
       [ (t1,t2) ; (u1,u2) ] 
   representa o conjunto de restrições 
       { t1=t2, u1=u2 } 
 *)

type subst = (int*tipo) list
