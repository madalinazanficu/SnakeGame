open! Base

type t =
  | In_progress
  | Game_over of string
  | Win_snake1 of string
  | Win_snake2 of string
[@@deriving sexp_of, compare]

let to_string t =
  match t with
  | In_progress -> ""
  | Game_over x -> "Game over: " ^ x
  | Win_snake1 x -> "WIN 1!"
  | Win_snake2 x -> "WIN 2!"
;;
