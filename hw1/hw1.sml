(* Assignment #1:    
 * 
 * Tim Farrell, tmf@bu.edu 
 * Programming Lang (Coursera/ UofW), 2013 
 * 150607
 *)

fun is_older (d1 : int*int*int, d2 : int*int*int) =
  (* is d2 older than d1 ? *) 
  if   (#1 d1) <> (#1 d2)
  then (#1 d1) <  (#1 d2)  
  else (if   (#2 d1) <> (#2 d2)
	then (#2 d1) < (#2 d2)
	else (#3 d1) < (#3 d2))
	   
fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else (if #2 (hd dates) = month then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months: int list) =
  if null dates
  then 0
  else (if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months))

fun dates_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else (if #2 (hd dates) = month
	then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month))

fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null dates
  then []
  else (if null months
	then []
	else dates_in_month (dates, hd months) @ dates_in_months(dates, tl months))

fun get_nth (strs : string list, n : int) =
  if n = 1
  then hd strs
  else get_nth (tl strs, n - 1) 

fun date_to_string (date : int*int*int) =
  get_nth(["January", "February", "March", "April", "May", "June", "July",
	   "August", "September", "October", "November", "December"], #2 date)
  ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)

fun number_before_reaching_sum (sum : int, xs : int list) =
  let
      fun sum_list (xs_ : int list) =
	if length xs_ = 0
	then 0
	else (if length xs_ = 1
	      then hd xs_
	      else (hd xs_) + sum_list (tl xs_))
  in
      if sum_list xs < sum
      then length xs
      else number_before_reaching_sum(sum, rev (tl (rev xs)))
  end 

fun what_month (day : int) =
  1 + number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31,
				   30, 31, 30, 31])

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)
      
fun oldest (dates : (int*int*int) list) =
  let 
      fun nth (xs : (int*int*int) list, n : int) =
	if n = 1
	then hd xs
	else nth (tl xs, n - 1)

      fun older (d1 : (int*int*int), d2 : (int*int*int)) =
	if is_older(d1, d2) then d1 else d2
  in
      if length dates = 0
      then NONE
      else (if length dates = 1
	    then SOME (nth(dates, 1))
	    else (if length dates = 2
		  then SOME (older(nth(dates, 1), nth(dates, 2)))
		  else oldest(older(nth(dates, 1), nth(dates, 2)) :: tl (tl dates))))
  end
      
  
