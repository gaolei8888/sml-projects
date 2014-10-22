use "hw2provided.sml";

(* ------------- 1 ------------*)
(* a all_except_option *)
fun all_except_option(str, str_list) =
  case str_list of
      [] => NONE
    | tl_str :: lst' =>
      case same_string(str, tl_str) of
        true => SOME lst'
       |false =>
            case all_except_option(str, lst') of
              NONE => NONE
              | SOME v => SOME(tl_str :: v)

(* b get_substitution1 *)
fun get_substitutions1(input, s) =
  case input of
    [] => []
    | xs :: lst' =>
      case all_except_option(s, xs) of
        NONE => get_substitutions1(lst', s)
        | SOME v => v @ get_substitutions1(lst', s)

(* c get_substitutions2 *)
fun get_substitutions2(input, s) =
  let
    fun aux(the_input, s, acc) =
      case the_input of
          [] => acc
        | xs :: lst' =>
          case all_except_option(s, xs) of
            NONE => aux(lst', s, acc)
            | SOME v => v @ aux(lst', s, acc)
  in
    aux(input, s, [])
  end

(* d similar_names *)
fun similar_names (subs, full_name) =
  case full_name of
    {first=f, middle=m, last=l} =>
      let
        val strings = get_substitutions2(subs, f)
        fun append_local(strings, f,m,l) =
          case strings of
              [] => []
            | xs :: lst' => {first=xs,middle=m,last=l}::append_local(lst', f, m, l)
      in
        full_name::append_local(strings, f, m, l)
      end

(* ------------- 2 ------------*)
(* card_color *)
fun card_color(card) =
  case card of
      (Clubs, _) => Red
    | (Diamonds, _) => Black
    | (Hearts, _) => Red
    | (Spade, _) => Black

(* card_value *)
fun card_value(card) =
  case card of
      (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11
    | (_, Num(n)) => n

(* remove_card *)
fun remove_card(cs, c, e) =
  case cs of
      [] => raise e
    | x :: xs' =>
      if (c = x)
      then  xs'
      else x :: remove_card(xs', c, e)

(* all_same_color *)
fun  all_same_color(card_list) =
  case card_list of
      [] => true
    | x :: xs' =>
      case xs' of
          x1 :: x1s' => card_color(x) = card_color(x1) andalso all_same_color(xs')
        | [] => true

(* sum_cards *)
fun sum_cards(card_list) =
  let
    fun local_sum(card_list, acc) =
      case card_list of
          [] => acc
        | x :: xls' => local_sum(xls', acc + card_value(x))
  in
    local_sum(card_list, 0)
  end

(* score *)
fun score(card_list, goal) =
  let
    val card_sum = sum_cards(card_list)
    val pre_score = case card_sum > goal of
        true => 3 * (card_sum - goal)
      | false => goal - card_sum
  in
    case all_same_color(card_list) of
        true => pre_score
      | false => pre_score div 2
  end

fun officiate(card_list, move_list, goal) =
  
