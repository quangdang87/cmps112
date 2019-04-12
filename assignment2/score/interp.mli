(* $Id: interp.mli,v 1.2 2019-01-29 22:36:11-08 - - $ *)

(*
* Quang Dang
* qvdang
*
*
* Interpreter for Silly Basic
*)

val want_dump : bool ref

val interpret_program : Absyn.program -> unit

