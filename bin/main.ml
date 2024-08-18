(* main.ml *)
open Types
open Evaluator
open Environment
open Utils

let int_bse (e:expr) : unit =
  try
    let t = type_infer e in
    let v = eval [] e
    in  print_string ("\n" ^ (vtos v) ^ " : " ^ (ttos t) ^ "\n")
  with
    TypeError msg ->  print_string ("\nerro de tipo - " ^ msg ^ "\n") 
  | BugTypeInfer  ->  print_string "\ncorrigir bug em typeinfer\n"
  | BugParser     ->  print_string "\ncorrigir bug no parser para let rec\n"

(* Teste para 'let x = true in if x then match Nothing with nothing => 0 | y => (y + 10) else Num 5' *)
let _ = int_bse (
  Let ("x", Bool true, 
    If (Var "x", 
      MatchJust (Nothing, Num 0, "y", Binop (Sum, Var "y", Num 10)), 
      Num 5)
  )
)  (* Esperado: 0 : int *)

(* Teste para 'let a = 3 in let b = 4 in let c = true in Just (if c then (a + b) else (a * b))' *)
let _ = int_bse (
  Let ("a", Num 3,
  Let ("b", Num 4,
  Let ("c", Bool true,
  Just (
    If (Var "c", 
      Binop (Sum, Var "a", Var "b"), 
      Binop (Mult, Var "a", Var "b")
    )
  ))))
)  (* Esperado: just 7 : maybe int *)

(* Teste para 'let x = Just (2 * 3) in let y = Nothing in match x with nothing => match y with nothing => 0 | z => (z + 5) | v => (if (v > 10) then v else (v + 1))' *)
let _ = int_bse (
  Let ("x", Just (Binop (Mult, Num 2, Num 3)), 
  Let ("y", Nothing,
  MatchJust (Var "x", 
    MatchJust (Var "y", Num 0, "z", Binop (Sum, Var "z", Num 5)), 
    "v", 
    If (Binop (Gt, Var "v", Num 10), Var "v", Binop (Sum, Var "v", Num 1))
  )))
)  (* Esperado: 7 : int *)

(* Teste para 'let x = Nil in let y = 1 :: x in let z = 2 :: y in match z with nil => 0 | a :: as => (a + match as with nil => 0 | b :: bs => (b * 2))' *)
let _ = int_bse (
  Let ("x", Nil, 
  Let ("y", Cons (Num 1, Var "x"), 
  Let ("z", Cons (Num 2, Var "y"),
  MatchList (Var "z", Num 0, "a", "as", 
    Binop (Sum, Var "a", 
      MatchList (Var "as", Num 0, "b", "bs", Binop (Mult, Var "b", Num 2))
    )
  ))))
)  (* Esperado: 4 : int *)

(* Teste para 'let l1 = 1 :: 2 :: 3 :: Nil in let l2 = 0 :: 1 :: Nil in let result = match l1 with nil => l2 | x :: xs => if (x < 2) then x :: xs else 0 :: xs in match result with nil => 0 | hd :: tl => if (hd = 0) then 1 else 2' *)
let _ = int_bse (
  Let ("l1", Cons (Num 1, Cons (Num 2, Cons (Num 3, Nil))),
  Let ("l2", Cons (Num 0, Cons (Num 1, Nil)),
  Let ("result", MatchList (Var "l1", Var "l2", "x", "xs", 
    If (Binop (Lt, Var "x", Num 2), 
      Cons (Var "x", Var "xs"), 
      Cons (Num 0, Var "xs"))),
  MatchList (Var "result", Num 0, "hd", "tl",
    If (Binop (Eq, Var "hd", Num 0), Num 1, Num 2)
  ))))
)  (* Esperado: 1 : int *)

(* Teste para 'let l = 5 :: 10 :: Nil in let total = match l with nil => 0 | hd :: tl => (hd + match tl with nil => 0 | hd2 :: tl2 => (hd2 + 1)) in if (total > 15) then total else 1' *)
let _ = int_bse (
  Let ("l", Cons (Num 5, Cons (Num 10, Nil)), 
  Let ("total", 
    MatchList (Var "l", Num 0, "hd", "tl", 
      Binop (Sum, Var "hd", 
        MatchList (Var "tl", Num 0, "hd2", "tl2", Binop (Sum, Var "hd2", Num 1))
      )
    ),
  If (Binop (Gt, Var "total", Num 15), Var "total", Num 1)
  ))
)  (* Esperado: 16 : int *)

(* Teste para 'let f = fn x => (x * 3) in let g = fn y => (y + 10) in let h = fn z => (z - 5) in 4 |> f |> g |> h' *)
let _ = int_bse (
  Let ("f", Fn ("x", Binop (Mult, Var "x", Num 3)), 
  Let ("g", Fn ("y", Binop (Sum, Var "y", Num 10)),
  Let ("h", Fn ("z", Binop (Sub, Var "z", Num 5)),
  Pipe (Pipe (Pipe (Num 4, Var "h"), Var "g"), Var "f"))))
)  (* Esperado: 27 : int *)

