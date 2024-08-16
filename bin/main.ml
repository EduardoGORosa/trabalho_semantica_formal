(* main.ml *)
open Types
open Evaluator
open Environment
open Utils

(*let int_bse (e:expr) : unit =
  try
    let t = type_infer e in
    let v = eval [] e
    in  print_string ((vtos v) ^ " : " ^ (ttos t))
  with
    TypeError msg ->  print_string ("erro de tipo - " ^ msg) 
  | BugTypeInfer  ->  print_string "corrigir bug em typeinfer"
  | BugParser     ->  print_string "corrigir bug no parser para let rec"
*)

(* Função para testar a inferência de tipos *)
let test_type_infer (e : expr) (expected_type : tipo) : bool =
  try
    let inferred_type = type_infer e in
    inferred_type = expected_type
  with
      TypeError msg ->  
          print_string ("erro de tipo - " ^ msg); 
          false    
    | BugTypeInfer  -> false   
    | BugParser     -> false

(* Lista de testes *)
let type_infer_tests () =

(* Testes para LetRec *)
  assert (test_type_infer 
    (LetRec ("f", "x", Fn ("x", Binop (Sum, Var "x", Num 1)), 
             App (Var "f", Num 2))) 
    (TyFn (TyInt, TyInt)) = true);

  (* Testes para Nil *)

  (*assert (test_type_infer (Cons (Num 5, Nil)) (TyList TyInt) = true);
  assert (test_type_infer (Cons (Bool true, Nil)) (TyList TyInt) = false); (* Tipo incorreto para a lista *)
*)
  (* Testes para Nothing *)
  (*assert (test_type_infer (Just (Num 5)) (TyOption TyInt) = true);
  assert (test_type_infer (Just (Bool true)) (TyOption TyInt) = false); (* Tipo incorreto para o Just *)
  *)
  print_endline "All tests passed.";;

let test_eval (e:expr) (expected_value: valor): bool = 
  try
      let v = eval [] e in
      print_string ("expr: " ^ expr_str e);
      print_string ("\n");
      print_string ("valor avaliado: " ^ vtos v);
      print_string ("\n");
      print_string ("\n");
      v = expected_value
  with
      TypeError msg ->  
          print_string ("erro de tipo - " ^ msg); 
          false    
    | BugTypeInfer  -> false   
    | BugParser     -> false

let eval_tests () =
  (* Teste de números *)
  assert (test_eval (Num 5) (VNum 5) = true);
  
  (* Teste de booleanos *)
  assert (test_eval (Bool true) (VBool true) = true);
  assert (test_eval (Bool false) (VBool false) = true);

  (* Teste de operações binárias *)
  assert (test_eval (Binop (Sum, Num 5, Num 6)) (VNum 11) = true);
  assert (test_eval (Binop (Sub, Num 10, Num 4)) (VNum 6) = true);
  assert (test_eval (Binop (Mult, Num 3, Num 7)) (VNum 21) = true);
  
  (* Teste de erro de tipo - Soma de inteiro com booleano *)
  assert (test_eval (Binop (Sum, Num 5, Bool true)) (VNum 5) = false);

  (* Teste de pares *)
  assert (test_eval (Pair (Num 3, Bool false)) (VPair (VNum 3, VBool false)) = true);

  (* Teste de if-then-else *)
  assert (test_eval (If (Bool true, Num 10, Num 20)) (VNum 10) = true);
  assert (test_eval (If (Bool false, Num 10, Num 20)) (VNum 20) = true);

  (* Teste de funções *)
  let succ = Fn ("x", Binop (Sum, Var "x", Num 1)) in
  assert (test_eval (App (succ, Num 5)) (VNum 6) = true);

  let invalid_succ = Fn ("x", Binop (Sum, Var "x", Bool true)) in
  assert (test_eval (App (invalid_succ, Num 5)) (VNum 5) = false);

  (* Teste de let *)
  assert (test_eval (Let ("x", Num 5, Binop (Sum, Var "x", Num 10))) (VNum 15) = true);

  print_endline "All eval tests passed."
;;

(* Chame ambos os testes *)
let run_all_tests () =
    type_infer_tests ();
    eval_tests ();
;;

run_all_tests ();;
