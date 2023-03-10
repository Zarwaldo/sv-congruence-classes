(* $Id: d7be60b466aa473253370202c56b8a1efec4f130 $ *)


(**
   Least fixpoint computation through round-robin iteration.

   This module provides a conservative least fixpoint engine (see {! Fixpoint})
   that implements the basic round-robin iteration.  Nodes of the input graph
   are processed in the order of the list returned by the function [nodes] (see
   {! DiGraph}).
 *)


module Make : Fixpoint.S
