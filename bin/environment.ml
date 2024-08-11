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

type tyenv = (ident * tipo) list

let empty_tyenv : tyenv = []
let empty_renv : renv = []

let rec lookup_tyenv g x : tipo option = 
  match g with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some t else lookup_tyenv tl x

let update_tyenv (g: tyenv) (x:ident) (t:tipo) : tyenv = 
  (x,t) :: g

let rec lookup_renv g x : valor option = 
  match g with
    [] -> None
  | (y,v) :: tl -> if (y=x) then Some v else lookup_renv tl x

let update_renv (g: renv) (x:ident) (v:valor) : renv = 
  (x,v) :: g

exception TypeError of string

(* Constraints for type inference *)
type constrains = (tipo * tipo) list

(* Collecting constraints for type inference *)
let rec collect (tenv: tyenv) (e: expr) : (constrains * tipo) =  
    match e with 
    | Num _  -> ([], TyInt)
    | Bool _ -> ([], TyBool)
    | Var x  -> 
            (match List.assoc_opt x tenv with
                | Some t -> ([],t)
                | None -> raise (TypeError ("variavel nao declarada:" ^ x)))

let rec typeinfer (tenv:tyenv) (e:expr) : tipo =
  match e with
    | Num _ -> TyInt
    | Var x ->
        (match List.assoc_opt x tenv with
          Some t -> t
        | None -> raise (TypeError ("variavel nao declarada:" ^ x)))
    | Bool _ -> TyBool 
    | Binop(oper,e1,e2) ->
        let t1 = typeinfer tenv e1 in
        let t2 = typeinfer tenv e2 in
        if t1 = TyInt && t2 = TyInt then
          (match oper with
            Sum | Sub | Mult |Div -> TyInt
          | Eq | Lt | Gt | Geq | Leq | And | Or -> TyBool)
        else raise (TypeError "operando nao é do tipo int")
    | Pair(e1,e2) -> TyPair(typeinfer tenv e1, typeinfer tenv e2)
    | Fst e1 ->
        (match typeinfer tenv e1 with
          TyPair(t1,_) -> t1
        | _ -> raise (TypeError "fst espera tipo par"))
    | Snd e1 ->
        (match typeinfer tenv e1 with
          TyPair(_,t2) -> t2
        | _ -> raise (TypeError "snd espera tipo par"))
    | If(e1,e2,e3) ->
        (match typeinfer tenv e1 with
          TyBool ->
            let t2 = typeinfer tenv e2 in
            let t3 = typeinfer tenv e3
            in if t2 = t3 then t2
            else raise (TypeError "then/else com tipos diferentes")
        | _ -> raise (TypeError "condição de IF não é do tipo bool"))
    | Fn(x,t,e1) ->
        let t1 = typeinfer ((x,t) :: tenv) e1
        in TyFn(t,t1)
    | App(e1,e2) ->
        (match typeinfer tenv e1 with
          TyFn(t, t') ->  if (typeinfer tenv e2) = t then t'
            else raise (TypeError "tipo argumento errado" )
        | _ -> raise (TypeError "tipo função era esperado"))
    | Let(x,t,e1,e2) ->
        if (typeinfer tenv e1) = t then typeinfer ((x,t) :: tenv) e2
        else raise (TypeError "expressão nao é do tipo declarado em Let" )
    | LetRec(f,(TyFn(t1,t2) as tf), Fn(x,tx,e1), e2) ->
        let tenv_com_tf = (f,tf) :: tenv in
        let tenv_com_tf_tx = (x,tx) :: tenv_com_tf in
        if (typeinfer tenv_com_tf_tx e1) = t2
        then typeinfer tenv_com_tf e2
        else raise (TypeError "tipo da funcao recursiva é diferente do declarado")
    | LetRec _ -> raise BugParser
    | Nothing -> raise (TypeError "Nothing não tem tipo especificado")


(* Auxiliary function to convert values to string *)
let rec vtos (v: valor) : string =
  match v with
    VNum n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VPair(v1, v2) ->
      "(" ^ vtos v1 ^ "," ^ vtos v1 ^ ")"
  | VClos _ ->  "fn"
  | VRClos _ -> "fn"

