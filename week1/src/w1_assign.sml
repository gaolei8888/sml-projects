(* 1. is_older *)
fun is_older(date1 : int * int * int, date2 : int * int * int) = 
  if (#1 date1 <> #1 date2)
  then #1 date1 < #1 date2
  else 
    if (#2 date1 <> #2 date2)
    then #2 date1 < #2 date2
    else 
      #3 date1 < #3 date2

(* 2. number_in_month *)
fun number_in_month(dates : (int * int * int) list, mon : int) = 
  if null dates
  then 0
  else
    if #2 (hd dates) = mon
    then 1 + number_in_month(tl dates, mon)
    else number_in_month(tl dates, mon)

(* 3. number_in_months *)
fun number_in_months(dates : (int * int * int) list, mons : int list) =
  if null dates orelse null mons
  then 0
  else 
    number_in_month(dates, hd mons) + number_in_months(dates, tl mons)

(* 4. dates_in_month *)
fun dates_in_month(dates : (int * int * int) list, mon : int) =
  if null dates
  then []
  else 
    if #2 (hd dates) = mon
    then hd dates :: dates_in_month(tl dates, mon)
    else dates_in_month(tl dates, mon)

(* 5. dates_in_months *)
fun dates_in_months(dates : (int * int * int) list, mons : int list) =
  if null dates orelse null mons
  then []
  else 
   dates_in_month(dates, hd mons) @ dates_in_months(dates, tl mons)

(* 6 get_nth *)
fun get_nth(strs : string list, n : int) = 
  if n = 1
  then hd strs
  else get_nth(tl strs, n - 1)

(* 7. date_to_string *)
fun date_to_string(date : (int * int * int)) = 
  let
    val mons_strs = ["January", "Frebruary", "March", "April", "May",
      "June", "July", "Auguest", "September", "October", "November", "Decenber"] 
  in
    get_nth(mons_strs, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end

(* 8 - number_before_reaching_sum *)
fun number_before_reaching_sum(limit : int, nums : int list) =
  let 
    fun limit_sum(the_nums : int list, curr_rst : int * int) = 
      if null the_nums
      then (0,0)
      else 
        let 
          val new_sum = hd the_nums + #1 curr_rst
          val new_cnt = 1 + #2 curr_rst
        in
          if new_sum >= limit
          then 
            (new_sum, new_cnt -1)
          else 
            limit_sum(tl the_nums, (new_sum, new_cnt))
        end      
  in      
    #2 (limit_sum(nums, (0, 0)))
  end

(* 9 - what_month *)
fun what_month(days : int) =
  let 
    val days_of_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(days, days_of_months) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if day2 < day1
  then []
  else
    if day1 = day2
    then
      [what_month(day2)]
    else
      what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else
    if null(tl dates)
    then SOME (hd dates)
    else
      let val older1 = valOf(oldest(tl dates))
      in
        if is_older(hd dates, older1)
        then
          SOME (hd dates)
        else
          SOME older1
      end

(* helper function - contains *)
fun contains(num : int, the_nums : int list) = 
  if null the_nums
  then false
  else
    if (hd the_nums = num)
    then true
    else 
      contains(num, tl the_nums)

(* helper function - remove dup *)
fun remove_dup_num(the_list : int list) =
  if null the_list
  then []
  else
    let
      val rest_values = remove_dup_num(tl the_list) 
    in
      if contains(hd the_list, tl the_list)
      then 
        rest_values
      else
        hd the_list :: rest_values      
    end

(*12 - number_in_months_challenge *)
fun number_in_months_challenge(dates : (int * int * int) list, mons_with_dup : int list) =
  if null dates orelse null mons_with_dup 
  then 0
  else
    let val mons = remove_dup_num mons_with_dup
    in
      number_in_months(dates, mons)
    end

(*12 - dates_in_months_challenge *)
fun dates_in_months_challenge(dates : (int * int * int) list, mons_with_dup : int list) =
  if null dates orelse null mons_with_dup
  then 
    []
  else
    let val mons = remove_dup_num mons_with_dup
    in
      dates_in_months(dates, mons)
    end

(*13 - reasonable_date *)
fun reasonable_date(date : int * int * int) =
  let 
    fun is_leap_year(year : int) =
      if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
      then true
      else false

    fun get_nth_local(n : int, nums : int list) = 
      if n = 1
      then hd nums
      else 
        get_nth_local(n - 1, tl nums)

    val days_of_months_regular = [31,28,31,30,31,30,31,31,30,31,30,31]
    val days_of_months_leap = [31,29,31,30,31,30,31,31,30,31,30,31]          
  in
    if #1 date < 1 orelse #2 date < 1 orelse #3 date < 1 orelse (#2 date > 12)
    then false
    else 
      if is_leap_year(#1 date)
      then 
        if #3 date <= get_nth_local(#2 date, days_of_months_leap)
        then true
        else false
      else
        if #3 date <= get_nth_local(#2 date, days_of_months_regular)
        then true
        else false          
  end        