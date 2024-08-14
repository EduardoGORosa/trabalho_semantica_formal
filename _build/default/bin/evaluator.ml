(* evaluator.ml *)
open Types
open Environment

exception BugTypeInfer

let compute (oper: op) (v1: valor) (v2: valor): valor = 
  match (oper, v1, v2) with
  | (Sum, VNum(n1), VNum(n2)) -> VNum (n1 + n2)
  | (Sub, VNum(n1), VNum(n2)) -> VNum (n1 - n2)
  | (Mult, VNum(n1),VNum(n2)) -> VNum (n1 * n2) 
  | (Div, VNum(n1),VNum(n2))  -> VNum (n1 / n2)    
  | (Eq, VNum(n1), VNum(n2))  -> VBool (n1 = n2) 
  | (Gt, VNum(n1), VNum(n2))  -> VBool (n1 > n2)  
  | (Lt, VNum(n1), VNum(n2))  -> VBool (n1 < n2)  
  | (Geq, VNum(n1), VNum(n2)) -> VBool (n1 >= n2) 
  | (Leq, VNum(n1), VNum(n2)) -> VBool (n1 <= n2)  
  | _ -> raise BugTypeInfer

let rec eval (renv:renv) (e:expr) :valor =
  match e with
    Num n -> VNum n
  | Var x ->
      (match lookup renv x with
         Some v -> v
       | None -> raise BugTypeInfer ) 
  | Bool b -> VBool b 
  | Binop(oper,e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2 in
      compute oper v1 v2
  | Pair(e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2
      in VPair(v1,v2)
  | Fst e ->
      (match eval renv e with
       | VPair(v1,_) -> v1
       | _ -> raise BugTypeInfer)
  | Snd e ->
      (match eval renv e with
       | VPair(_,v2) -> v2
       | _ -> raise BugTypeInfer)
  | If(e1,e2,e3) ->
      (match eval renv e1 with
         VBool true  -> eval renv e2
       | VBool false -> eval renv e3
       | _ -> raise BugTypeInfer )
  | Fn(x,e1)  -> VClos(x,e1, renv)
  | App(e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2 in
      (match v1 with 
         VClos(   x,e',renv') ->
           eval  (         (x,v2) :: renv')  e' 
       | VRClos(f,x,e',renv') -> 
           eval  ((f,v1) ::(x,v2) :: renv')  e' 
       | _  -> raise BugTypeInfer) 
  | Let(x,e1,e2) ->
      let v1 = eval renv e1
      in eval ((x,v1) :: renv) e2
  | LetRec(f, x, e1, e2) ->
      let renv'=  (f, VRClos(f,x,e1,renv)) :: renv
      in eval renv' e2
  | Nil -> VNil

