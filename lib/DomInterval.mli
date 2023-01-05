(* $Id: daeea98a4aee119b5c69d6693cdc652ff49fe375 $ *)


(**
   Numerical domain for range analysis.

   The underlying abstract lattice contains the element ⊥ as well as all pairs
   (l, u) in (ℤ ∪ \{-∞\}) × (ℤ ∪ \{+∞\}) such that l ≤ u.

   The functions [Op.equality] and [Op.inequality] are optimal in the special
   case where the first argument [a] is empty or a singleton.  But they are not
   optimal in general.

   Consider for instance the “linear equality” \[5, 7\]*x + \[-53, -51\] = 0,
   with no condition on x.  There is no solution, because (k*x) ∉ \[51, 53\] for
   every k ∈ \[5, 7\] and every x ∈ ℤ.  But [Op.equality] returns the interval
   \[8, 10\].  The bounds of this interval are obtained as follows: the lower
   bound is ⌈51/7⌉ = 8 and the upper bound is ⌊53/5⌋ = 10.

   The implementation of this module uses extended arbitrary-precision integers
   (see {! Eint}).
 *)


include NumericalDomain.S
