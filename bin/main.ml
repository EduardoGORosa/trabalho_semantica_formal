(* main.ml *)
open Types
open Evaluator
open Environment
open Utils

(*
let int_bse (e:expr) : unit =
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
  
    (* Teste com listas e Maybe combinados *)
  assert (
    test_type_infer
      (MatchList (
         Cons (Just (Num 5), Nil),   (* Lista de Maybe Int *)
         Num 0,                      (* Caso base: lista vazia *)
         "hd", "tl",                 (* Correspondência com "hd :: tl" *)
         MatchJust (Var "hd", Num 0, "x", Binop (Sum, Var "x", Var "x"))  (* Caso Just x: x + x *)
       ))
      TyInt = true
  );

  (* Teste com Let e MatchList aninhado *)
  assert (
    test_type_infer
      (Let ("lst", Cons (Num 5, Cons (Num 10, Nil)),   (* Lista [5; 10] *)
            MatchList (Var "lst", Num 0, "hd", "tl",   (* Correspondência de padrão *)
                       Binop (Sum, Var "hd", Var "hd")))) (* hd + hd *)
      TyInt = true
  );

  (* Teste com LetRec, funções, e listas *)
  assert (
    test_type_infer
      (LetRec (
         "sumList", "lst",           (* Função recursiva sumList *)
         MatchList (Var "lst", Num 0, "hd", "tl", Binop (Sum, Var "hd", App (Var "sumList", Var "tl"))),  (* Caso recursivo: hd + sumList(tl) *)
         App (Var "sumList", Cons (Num 1, Cons (Num 2, Cons (Num 3, Nil)))) (* Aplicação em [1; 2; 3] *)
       ))
      TyInt = true
  );

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
;;


let eval_tests () =

  (* Teste com listas e Maybe combinados *)
    assert (
    test_eval
      (MatchList (
         Cons (Just (Num 5), Nil),   (* Lista de Maybe Int *)
         Num 0,                      (* Caso base: lista vazia *)
         "hd", "tl",                 (* Correspondência com "hd :: tl" *)
         MatchJust (Var "hd", Num 0, "x", Binop (Sum, Var "x", Var "x"))  (* Caso Just x: x + x *)
       ))
      (VNum 10) = true  (* Resultado esperado: 5 + 5 *)
  );

  (* Teste com Let e MatchList aninhado *)
  assert (
    test_eval
      (Let ("lst", Cons (Num 5, Cons (Num 10, Nil)),   (* Lista [5; 10] *)
            MatchList (Var "lst", Num 0, "hd", "tl",   (* Correspondência de padrão *)
                       Binop (Sum, Var "hd", Var "hd")))) (* hd + hd *)
      (VNum 10) = true  (* Resultado esperado: 5 + 5 *)
  );

  (* Teste com LetRec, funções, e listas *)
  assert (
    test_eval
      (LetRec (
         "sumList", "lst",           (* Função recursiva sumList *)
         MatchList (Var "lst", Num 0, "hd", "tl", Binop (Sum, Var "hd", App (Var "sumList", Var "tl"))),  (* Caso recursivo: hd + sumList(tl) *)
         App (Var "sumList", Cons (Num 1, Cons (Num 2, Cons (Num 3, Nil)))) (* Aplicação em [1; 2; 3] *)
       ))
      (VNum 6) = true  (* Resultado esperado: 1 + 2 + 3 = 6 *)
  );

  (* Teste com Maybe aninhado *)
  assert (
    test_eval
      (MatchJust (
         Just (Just (Num 10)),        (* Just (Just 10) *)
         Nothing,                     (* Caso Nothing *)
         "x",                         (* Nome da variável para Just x *)
         MatchJust (Var "x", Num 0, "y", Var "y")  (* Padrão interno: Just y *)
       ))
      (VNum 10) = true  (* Resultado esperado: Just 10 *)
  );

  assert (
    test_eval (
  Pipe (
    Fn ("z", Binop (Mult, Var "z", Var "z")),  (* Multiplica por 11 *)
    Pipe (
      Fn ("y", Binop (Sum, Var "y", Num 3)),  (* Soma 3 *)
      Pipe (
        Fn ("x", Binop (Mult, Var "x", Num 2)),  (* Multiplica por 2 *)
        Num 4                                    (* Valor inicial: 4 *)
      )
    )
  ))
    (VNum 121) = true
  );

  print_endline "All eval tests passed."
;;



(* Chame ambos os testes *)
let run_all_tests () =
    type_infer_tests ();
    eval_tests ();
;;

run_all_tests ();;
