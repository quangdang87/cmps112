(* $Id: etc.mli,v 1.1 2019-01-28 20:15:09-08 - - $ *)

(*
* Main program and system access.
*)

val warn : string list -> unit

val die : string list -> unit

val syntax_error : Lexing.position -> string list -> unit

val usage_exit : string list -> unit

val read_number : unit -> float

