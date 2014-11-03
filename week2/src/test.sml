use "hw2provided.sml";

(* randomly generate a card list *)
fun my_rand(base) = 
  LargeInt.toInt(LargeInt.mod(Time.toMilliseconds(Time.now()), base)) 

exception IllegalInput

fun rand_suit() = 
  case my_rand(4) of
      0 => Clubs 
    | 1 => Diamonds
    | 2 => Hearts
    | 3 => Spades
    | _ => raise IllegalInput

fun rand_rank() = 
  let
    val rand_num = my_rand(14) + 1
    val diff = rand_num - 10 
  in
    case diff <= 0 of
        true => Num(rand_num)
      | _ => case diff of
        4 => Jack
      | 3 => Queen
      | 2 => King
      | 1 => Ace
      | _ => let
             in
               print(Int.toString(diff));
               raise IllegalInput
             end  
  end

fun run_until_ace() = 
  let 
    val the_rank = run_until_ace()
  in
    case the_rank of
        (cnt, Ace) => (0, Ace)
      | (cnt, _) => (1 + 0, rand_rank())
  end

fun run_cnt() = 
  let 
    val n = run_cnt()
  in
    if (n = 100)
    then 0
    else 100 - n
  end
