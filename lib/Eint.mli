(* $Id: aeab284c0509b5930d931085cec458b5e4909575 $ *)


(**
   Extended arbitrary-precision integers.

   This module extends Zarith's arbitrary-precision integers (i.e., values of
   type [Z.t]) with -∞ and +∞.  The corresponding new type [eint] comes with a
   pretty-printer and with functions for comparisons and arithmetic operations.
   All these notions are based on the following mathematical definitions.

   Let {b Z} = ℤ ∪ \{-∞, +∞\} denote the set of {e extended integers}.  The
   usual order ≤ on ℤ is extended to {b Z} by -∞ < z < +∞ for every z ∈ ℤ.  This
   extension is a total order on {b Z}.  Arithmetic operations on ℤ are extended
   to {b Z} as follows, where n, p, z denote arbitrary integers in ℤ such that
   n < 0 and p > 0.

   - Addition:
     {C z + (-∞) = (-∞) + z = (-∞) + (-∞) = -∞}
     {C z + (+∞) = (+∞) + z = (+∞) + (+∞) = +∞}
     Observe that (-∞) + (+∞) and (+∞) + (-∞) are undefined.

   - Subtraction:
     {C z - (-∞) = (+∞) - z = (+∞) - (-∞) = +∞}
     {C z - (+∞) = (-∞) - z = (-∞) - (+∞) = -∞}
     Observe that (-∞) - (-∞) and (+∞) - (+∞) are undefined.

   - Multiplication:
     {C 0 * (-∞) = (-∞) * 0 = 0 * (+∞) = (+∞) * 0 = 0}
     {C p * (-∞) = (-∞) * p = n * (+∞) = (+∞) * n = (-∞) * (+∞) = (+∞) * (-∞) = -∞}
     {C p * (+∞) = (+∞) * p = n * (-∞) = (-∞) * n = (+∞) * (+∞) = (-∞) * (-∞) = +∞}
     Observe that multiplication is defined for every pair of extended integers.

   - Truncated division:
     {C z / (-∞) = z / (+∞) = 0}
     {C (-∞) / p = (+∞) / n = -∞}
     {C (+∞) / p = (-∞) / n = +∞}
     Observe that (-∞) / (-∞), (-∞) / (+∞), (+∞) / (-∞) and (+∞) / (+∞) are
     undefined.  The same holds for (-∞) / 0 and (+∞) / 0.

   - The extension of {e flooring division} /{_ <} to extended integers uses the
     same rules as for truncated division except that the first rule is replaced
     by:
     {C 0 /{_ <} (-∞) = 0 /{_ <} (+∞) = n /{_ <} (-∞) = p /{_ <} (+∞) = 0}
     {C n /{_ <} (+∞) = p /{_ <} (-∞) = -1}

   - The extension of {e ceiling division} /{_ >} to extended integers uses the
     same rules as for truncated division except that the first rule is replaced
     by:
     {C 0 /{_ >} (-∞) = 0 /{_ >} (+∞) = n /{_ >} (+∞) = p /{_ >} (-∞) = 0}
     {C n /{_ >} (-∞) = p /{_ >} (+∞) = 1}

   In the implementation provided by this module, arithmetic operations raise
   [Invalid_argument] when they are undefined (except that, for consistency with
   the standard division, [div] raises [Division_by_zero] when its second
   argument is zero).

   This module is a helper for the {! DomInterval} module.

   @see <http://antoinemine.github.io/Zarith/doc/latest/Z.html> Z
 *)


(** The type of extended integers. *)
type eint =
  | Integer of Z.t      (** Standard integer. *)
  | Neg_infty           (** The value -∞. *)
  | Pos_infty           (** The value +∞. *)

(** Pretty-printer for extended integers. *)
val pp_print_eint : Format.formatter -> eint -> unit


(** {2 Comparisons} *)

(** Equality. *)
val ( = ) : eint -> eint -> bool

(** See {! Eint.( <= )}. *)
val ( < ) : eint -> eint -> bool

(** See {! Eint.( <= )}. *)
val ( > ) : eint -> eint -> bool

(** Extension of the usual order on integers to extended integers. *)
val ( <= ) : eint -> eint -> bool

(** See {! Eint.( <= )}. *)
val ( >= ) : eint -> eint -> bool

(** Minimum. *)
val min : eint -> eint -> eint

(** Maximum. *)
val max : eint -> eint -> eint


(** {2 Arithmetic operations} *)

(** Addition. *)
val ( + ) : eint -> eint -> eint

(** Subtraction. *)
val ( - ) : eint -> eint -> eint

(** Multiplication. *)
val ( * ) : eint -> eint -> eint

(** Truncated division.  The result is rounded towards zero. *)
val ( / ) : eint -> eint -> eint

(** Flooring division.  The result is rounded towards -∞. *)
val ( /< ) : eint -> eint -> eint

(** Ceiling division.  The result is rounded towards +∞. *)
val ( /> ) : eint -> eint -> eint

(** Unary addition (identity). *)
val ( ~+ ) : eint -> eint

(** Unary negation. *)
val ( ~- ) : eint -> eint
