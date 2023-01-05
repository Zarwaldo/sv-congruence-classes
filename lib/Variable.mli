(* $Id: 28100a907d7973ff143e263290f36ba9e8e5525e $ *)


(**
   Program Variables.

   Variables are totally ordered and hashable.

   @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.OrderedType.html>
     Set.OrderedType
   @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.HashedType.html>
     Hashtbl.HashedType
 *)


(**
   The printable type of variables.
 *)
include Print.S with type t = string

(**
   Total ordering on variables.
 *)
val compare : t -> t -> int

(**
   Equality on variables.
 *)
val equal : t -> t -> bool

(**
   Hashing on variables.
 *)
val hash : t -> int
