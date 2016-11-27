(*Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.*)
fun is_older (date1: int * int * int, date2: int * int * int) =
	(#1 date1 < #1 date2) orelse
	(#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
	(#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(*Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.*)
fun number_in_month (date_list: (int * int * int) list, month : int) = 
	let fun have_month (date: int * int * int) =
		if #2 date = month then 1 else 0
	in
		if null date_list 
		then 0
		else have_month (hd date_list) + number_in_month (tl date_list, month)
	end

(*Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated.*)
fun number_in_months (date_list: (int * int * int) list, month_list: int list) =
	if null month_list
	then 0
	else number_in_month(date_list, hd month_list) + number_in_months(date_list, tl month_list)

(*Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.*)
fun dates_in_month (date_list: (int * int * int) list, month: int) =
	let fun match_date (date: int * int * int) =
		if #2 date = month then SOME date else NONE
	in
		if null date_list 
		then []
		else let val result = match_date (hd date_list)
			in
				if isSome result
				then valOf result :: dates_in_month(tl date_list, month)
				else dates_in_month(tl date_list, month)
			end
	end		

(*Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated.*)
fun dates_in_months (date_list: (int * int * int) list, month_list: int list) =
	if null month_list
	then []
	else dates_in_month(date_list, hd month_list) @ dates_in_months(date_list, tl month_list)

(* Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the list where the head of the list is 1st. *)
fun get_nth (str_list: string list, n) =
	if null str_list
	then ""
	else if n = 1
		then hd str_list
		else get_nth(tl str_list, n-1)

(*Write a function date_to_string that takes a date and returns a string of the form January 20, 2013.*)
fun date_to_string (date: int * int * int) =
	let val month_names = ["January", "February", "March", "April", "May", "June", 
		"July", "August", "September", "October", "November", "December"]
	in 
		get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

(*Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. *)
fun number_before_reaching_sum (sum: int, ints: int list) =
	if null ints
	then 0
	else
		if sum - hd ints > 0
		then 1 + number_before_reaching_sum(sum - hd ints, tl ints)
		else 0

(*Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.).*)
fun what_month day_of_year =
	let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		1 + number_before_reaching_sum(day_of_year, month_days)
	end

(*Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.*)
fun month_range (day1, day2) =
	if day1 > day2 
	then []
	else (what_month day1) :: month_range(day1 + 1, day2)

(*Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*)
fun oldest (date_list: (int * int * int) list) =
	if null date_list
	then NONE
	else let val sub_older = oldest(tl date_list)
	in
		if isSome sub_older
		then if is_older(hd date_list, valOf sub_older)
			then SOME (hd date_list)
			else sub_older
		else SOME (hd date_list)
	end

(*Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once*)
fun remove_duplicate (dup_list) = 
	let fun remove_ele (ele, alist) =
			if null alist
			then []
			else 
				if ele = hd alist
				then remove_ele(ele, tl alist)
				else hd alist :: remove_ele(ele, tl alist)
	in 
		if null dup_list
		then []
		else (hd dup_list) :: remove_duplicate(remove_ele(hd dup_list, tl dup_list))
	end

fun number_in_months_challenge (date_list: (int * int * int) list, month_list: int list) =
	number_in_months(date_list, remove_duplicate month_list)

fun dates_in_months_challenge (date_list: (int * int * int) list, month_list: int list) =
	dates_in_months(date_list, remove_duplicate month_list)

(*Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.*)
fun reasonable_date (date: int * int * int) =
	let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		fun get_nth_int (int_list: int list, n) =
			if null int_list
			then 0
			else if n = 1
				then hd int_list
				else get_nth_int(tl int_list, n-1)
		fun is_leap_year _ = (((#1 date) mod 4 = 0) andalso ((#1 date) mod 100 > 0)) orelse ((#1 date) mod 400 = 0) 
		fun valid_year _ = (#1 date) > 0
		fun valid_month _ = (#2 date) <=12 andalso (#2 date) > 0
		fun valid_day _ =
			((#2 date) <> 2 andalso (#3 date) > 0 andalso (#3 date) <= get_nth_int(month_days, #2 date))
			orelse ((#2 date) = 2 andalso is_leap_year 0 andalso (#3 date) > 0 andalso (#3 date) <= 29)
			orelse ((#2 date) = 2 andalso not (is_leap_year 0) andalso (#3 date) > 0 andalso (#3 date) <= 28)
	in 
		valid_year 0 andalso valid_month 0 andalso valid_day 0
	end
