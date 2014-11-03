use "./hw3provided.sml";

(* 1 only_capitals *)
fun only_capitals(string_list) = 
  List.filter(fn str => Char.isUpper(String.sub(str, 0))) string_list

(* 2 longest_string1 *)
fun longest_string1(string_list) =  case string_list of
      [] => 
 
