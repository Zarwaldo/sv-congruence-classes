(* $Id: 54b290d77e690f9b8429eb8463ee8e97293d9378 $ *)


(**
   Automata-based Programs.

   A {e program automaton} is a labeled directed graph (see {! DiGraph}) where
   edge labels are commands (see {! Command}).  Nodes are called {e locations}
   and edges are called {e transitions}.  A program automaton also comes with an
   initial location, a final location and a set of variables.  This set shall
   contain all variables occurring in the commands of the automaton.

   Program automata give a control-flow graph representation of programs.  All
   program analyzers in Spa take program automata as input.
 *)


(**
   Program automata are directed graphs with edges labeled by commands.
 *)
include DiGraph.S with module Label = Command

(**
   [name a] returns the name of the program automaton [a].
 *)
val name : t -> string

(**
   [variables a] returns the list of variables of the program automaton [a].
 *)
val variables : t -> Variable.t list

(**
   [initial a] returns the initial location of the program automaton [a].
 *)
val initial : t -> Node.t

(**
   [final a] returns the final location of the program automaton [a].
 *)
val final : t -> Node.t

(**
   [reverse a] returns the program automaton obtained from the program automaton
   [a] by reversing its transitions and by swapping its initial and final
   locations.
 *)
val reverse : t -> t

(**
   Textual formats of program automata that the [read] function can read.
 *)
type format =
  | Aut
  | Prg

(**
   [read fmt chan] reads from [chan] the textual description of a program
   automaton, constructs the corresponding automaton, and returns it.  The [fmt]
   argument specifies the input format.  Raises [IO.Read_error] on error.
 *)
val read : format -> in_channel -> t
