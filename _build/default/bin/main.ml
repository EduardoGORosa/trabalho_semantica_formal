(* main.ml *)
open Types
open Evaluator
open Environment
(*open Utils
*)
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
let run_tests () =
  assert (test_type_infer (Num 5) TyInt = true);
  assert (test_type_infer (Bool false) TyBool = true);
  assert (test_type_infer (Binop (Sum, Num 5, Num 6)) TyInt = true);
  assert (test_type_infer (Binop (Sum, Num 5, Bool true)) TyInt = false);
  assert (test_type_infer (Pair (Num 5, Binop (Sum, Num 5, Num 6))) (TyPair(TyInt, TyInt)) = false);
  assert (test_type_infer (Pair (Num 5, Binop (Sum, Num 5, Bool true))) (TyPair(TyInt, TyInt)) = false);
  (* Adicione mais testes conforme necessário *)
  print_endline "All tests passed."
;;

let () = run_tests ()
(*
check_type (Num 5) == true ;;

check_type (Bool false) == true ;;

(* Binary operations *)

let valid_binop = Binop (Sum, Num 5, Num 6) ;;

check_type valid_binop == true ;;

let invalid_binop = Binop (Sum, Num 5, Bool true) ;;

check_type invalid_binop == false ;;

(* Pairs *)

let valid_pair = Pair(Num 5, valid_binop);;

check_type valid_pair == true ;;

check_type (Pair (Num 5, invalid_binop)) == false ;;

check_type (Fst (Pair (Num 5, invalid_binop))) == false ;;

check_type (Snd (valid_pair)) == true ;;

(*check_type (If (Nothing, Num 5, Num 6)) == false ;;*)

(* Functions *)
let invalid_sucessor = Fn ("x", Binop (Sum, Var "x", Bool(true)));;

check_type invalid_sucessor == false ;;

let sucessor = Fn ("x", Binop (Sum, Var "x", Num 1)) ;;

check_type (App (sucessor, Num(2))) == true ;;

check_type (App (sucessor, Bool(false))) == false ;;

check_type (Let ("foo", Num(5), Num(6))) == true ;;

check_type (Let ("foo", invalid_sucessor, Num(6))) == false ;;

check_type (LetRec ("foo", "bar", Num(5), Num(6))) == true ;;

check_type (LetRec ("foo", "bar", invalid_sucessor, Num(6))) == false ;;

check_type (Pipe (Num(1), sucessor)) == true ;;

check_type (Pipe (Num(1), invalid_sucessor)) == false ;;

(* Lists *)

check_type (Nil) == true ;;

check_type (Node (Num(1), Nil)) == true ;;

check_type (Node (Num(1), Num(2))) == false ;;

let valid_list = (Node (Num(1), Node(Num(2), Nil))) ;;

check_type valid_list == true ;;

let invalid_list = (Node (Num(1), Node(Bool(true), Nil))) ;;

check_type invalid_list == false ;;

check_type (MatchList (valid_list, Num(1), "foo", "bar", Num(2))) == true ;;

check_type (MatchList (valid_list, Num(1), "foo", "bar", Bool(false))) == false ;;

(* Maybe *)

check_type (Nothing) == true ;;

check_type (Just (Num(5))) == true ;;

check_type (Just (invalid_sucessor)) == false ;;

let valid_nothing_maybe = (MatchMaybe (Nothing, Num(1), "foo", Num(2))) ;;

check_type valid_nothing_maybe == true ;;

let valid_just_maybe = (MatchMaybe (Just(Num(5)), Num(1), "foo", Num(2))) ;;

check_type valid_just_maybe == true ;;

check_type (MatchMaybe (Just(Num(5)), Bool(true), "foo", Num(2))) == false ;;

(* Either *)

check_type (Left(Num(5))) == true ;;

check_type (Right(invalid_binop)) == false ;;

(* Eval tests *)

(* Function for evals *)

let zero = Fn ("x", Num 0) ;;

let predecessor = Fn ("x", Binop (Sub, Var "x", Num 1)) ;;

(* Tests *)

exec (Num(2)) = VNum(2) ;;

exec (Bool(true)) = VBool(true) ;;

(exec (Binop(Sum, Num(4), Num(6))) = VNum(10)) == true ;;

(* Should raise ExecFail *)
(* exec (Binop(Sum, Num(4), Bool(false))) ;; *)

(* Pairs *)

exec (Pair(Num(1), Num(2))) = VPair(VNum(1), VNum(2)) ;;

(exec (Pair(Num(1), Num(3))) = VPair(VNum(1), VNum(2))) == false ;;

(exec (Fst(valid_pair)) = VNum(5)) == true ;;

(exec (Snd(valid_pair)) = VNum(11)) == true ;;

(exec (If (Binop(Gt, Num(5), Num(6)), Num 5, valid_binop))) = VNum(11) == true ;;

(exec (If (Binop(Lt, Num(5), Num(6)), Num 5, valid_binop))) = VNum(5) == true ;;

(* Functions *)

(exec (App (zero, Num 1))) = VNum(0) == true ;;

(exec (Pipe (Num(1), sucessor))) == VNum(2) ;;

(* Lists *)

((exec (MatchList(Nil, Num(1), "foo", "bar", Num(2)))) = VNum(1)) = true ;;

((exec (MatchList(valid_list, Num(1), "foo", "bar", Num(2)))) = VNum(2)) = true ;;

(* Maybe *)

(exec valid_nothing_maybe) = VNum(1) == true ;;

(exec valid_just_maybe) = VNum(2) == true ;;

(* Either *)
(exec (MatchEither(Left(Bool true), "x", Num(1), "y", Num(2)))) = VNum(1) == true ;;

(exec (MatchEither(Right(Bool true), "x", Num(1), "y", Num(2)))) = VNum(2) ;;
*)
