(* $Id: absyn.mli,v 1.1 2019-01-28 20:15:09-08 - - $ *)
(* Quang Dang *)
(* qvdang *)
(*
* Abstract syntax definitions for SB.
*)

type linenr    = int
type ident     = string
type label     = string
type number    = float
type oper      = string

type printable = Printexpr of expr
               | String of string

and  memref    = Arrayref of ident * expr
               | Variable of ident

and  expr      = Number of number
               | Memref of memref
               | Unary of oper * expr
               | Binary of oper * expr * expr

type stmt      = Dim of ident * expr
               | Let of memref * expr
               | Goto of label
               | If of expr * label
               | Print of printable list
               | Input of memref list

type progline  = linenr * label option * stmt option

type program   = progline list

