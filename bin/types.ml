(* types.ml *)

exception BugParser
exception TypeError of string

type tipo =
    TyInt
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo
  | TyNothing of tipo  
  | TyVar of int
  | TyList of tipo
  | TyMatch of tipo

