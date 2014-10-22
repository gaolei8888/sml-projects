use "hw2provided.sml";

(* card_color *)
fun card_color(card) =
  case card of
      (Clubs, _) => Red
    | (Diamonds, _) => Black
    | (Hearts, _) => Red
    | (Spade, _) => Black

fun all_same(input_list) =
  case input_list of
      [] => true
    | x :: xs' =>
      case xs' of
          x1 :: x1s' => x = x1 andalso all_same(xs')
        | [] => true