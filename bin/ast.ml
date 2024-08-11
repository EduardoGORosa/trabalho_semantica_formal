(* ast.ml *)
open Types

type ident = string

type op = Sum | Sub | Mult | Div | Eq | Gt | Lt | Geq | Leq | And | Or 

type expr =
  | Num of int
  | Var of ident
  | Bool of bool
  | Binop of op * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | If of expr * expr * expr
  | Fn of ident * tipo * expr
  | App of expr * expr
  | Let of ident * tipo * expr * expr
  | LetRec of ident * tipo * expr * expr
  | MatchList of expr * ident * expr * ident * expr
  | MatchJust of expr * expr 
  | Cons of expr * expr
  | Just of expr
  | Nothing
  | Nil

