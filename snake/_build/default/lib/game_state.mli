open! Base

(** A [t] represents the current state of the game. *)
type t =
  | In_progress
  | Game_over of string (* The string is the reason the game ended. *)
  | Win_snake1 of string
  | Win_snake2 of string
[@@deriving sexp_of, compare]

(** [to_string] pretty-prints the current game state into a string. *)
val to_string : t -> string
