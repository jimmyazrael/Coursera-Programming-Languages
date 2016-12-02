(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution.*)
fun only_capitals str_list =
	List.filter (fn lst => (Char.isUpper o String.sub) (lst, 0)) str_list

(*Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive).*)
fun longest_string1 str_list =
	List.foldl (fn (x,y) => if (String.size x) > (String.size y) then x else y) "" str_list

(*Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size.*)
fun longest_string2 str_list =
	List.foldl (fn (x,y) => if (String.size x) >= (String.size y) then x else y) "" str_list

(*Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.
• longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper.*)
fun longest_string_helper f str_list =
	List.foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" str_list

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(*Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2.*)
val longest_capitalized = longest_string1 o only_capitals

(*Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module.*)
val rev_string = String.implode o List.rev o String.explode

(*Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments
are curried). The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer.*)
fun first_answer f alst =
	case alst of
		x::y::xs => (case (f x) of SOME v => v | NONE => first_answer f (y::xs))
		| [x] => (case (f x) of SOME v => v | NONE => raise NoAnswer)
		| [] => raise NoAnswer

(*Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).*)
fun all_answers f alst =
	let fun construct_lst lst acc acc_lst =
		case lst of
			x::xs => (case (f x) of SOME v => (construct_lst xs (SOME (acc_lst @ v)) (acc_lst @ v)) | NONE => NONE)
			| [] => acc
	in
		construct_lst alst (SOME []) []
	end

(*(a) Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains.
(b) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. (Use String.size. We care only about variable names; the
constructor names are not relevant.)
(c) Use g to define a function count_some_var that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern. We care only about
variable names; the constructor names are not relevant.
*)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

fun count_some_var (str, pat) = g (fn _ => 0) (fn s => if s = str then 1 else 0) pat

(*Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
names are not relevant.*)
fun check_pat pat =
	let fun make_lst p acc = 
			case p of
			    Variable x        => acc @ [x]
			  | TupleP ps         => List.foldl (fn (p, sub_acc) => make_lst p sub_acc) acc ps
			  | ConstructorP(_,p) => make_lst p acc
			  | _                 => acc

		fun is_distinct lst acc = 
			case lst of
				[] => acc
				| x::xs => is_distinct xs (not (List.exists (fn y => y = x) xs) andalso acc)
	in
		is_distinct (make_lst pat []) true
	end

(*Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME [].*)
fun match (value, pattern) =
	(case pattern of
			Wildcard => SOME []
		  | Variable s => SOME [(s, value)]
		  | UnitP => (case value of 
		  				Unit => SOME [] 
		  				| _ => NONE)
		  | ConstP x => (case value of 
		  					Const y => if x = y then SOME [] else NONE 
		  					| _ => NONE)
		  | TupleP ps => (case value of 
		  					Tuple vs => (all_answers match (ListPair.zipEq(vs, ps))) 
		  					| _ => NONE)
		  | ConstructorP (s1, pt) => (case value of 
		  								Constructor(s2, va) => if s1 = s2 then match(va, pt) else NONE 
		  								| _ => NONE))
	handle UnequalLengths => NONE

(*Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. *)
fun first_match value pattern_lst =
	SOME (first_answer (fn x => match(value, x)) pattern_lst)
	handle NoAnswer => NONE

(*Write a function typecheck_patterns that “type-checks” a pattern list.
typecheck_patterns should have type ((string * string * typ) list) * (pattern list) -> typ option.
The first argument contains elements that look like ("foo","bar",IntT), which means constructor foo
makes a value of type Datatype "bar" given a value of type IntT. Assume list elements all have different
first fields (the constructor name), but there are probably elements with the same second field (the datatype
name). Under the assumptions this list provides, you “type-check” the pattern list to see if there exists
some typ (call it t) that all the patterns in the list can have. If so, return SOME t, else return NONE.
You must return the “most lenient” type that all the patterns can have.*)
fun typecheck_patterns (datatyp_lst, pattern_lst) =
	let fun match_tuple_aux f tup_pairs acc = (* Generate a valid tye list for 2 TupleT *)
			case tup_pairs of
				(a, b)::xs => (case (f [a, b]) of SOME t => match_tuple_aux f xs (acc @ [t]) | NONE => NONE)
				| [] => SOME acc

		fun match_tuple f ts lst =
			let fun gen_tup_typ ts' tail =
				 	case (match_tuple_aux f (ListPair.zipEq(ts, ts')) []) of
				 		SOME t => match_tuple f t tail
				 		| NONE => raise NoAnswer
			in
				(case lst of
					Anything::xs => match_tuple f ts xs
					(* Matching every tuple in the remaining list *)
					| (TupleT ts2)::xs => gen_tup_typ ts2 xs
					| [] => SOME (TupleT ts)
					| _ => NONE)
				handle _ => NONE (* if 2 TupleT is of unequal len or NoAnswer *)
			end
			

		fun match_construct_aux f (s1, typ1) (s2, t, typ2) =
			s2 = s1 andalso (case typ2 of 
								Anything => true 
								| _ => (case (f [typ2, typ1]) of SOME t => true | NONE => false))

		fun match_construct f dt_lst (s_main, typ_main) =
			let val compare_fn = match_construct_aux f (s_main, typ_main)
			in
				case dt_lst of
					(s, t, typ)::xs => (if (compare_fn (s, t, typ)) 
										then (Datatype t)
										else match_construct f xs (s_main, typ_main) )
					| [] => raise NoAnswer
			end

		(* Given a typ list then narrow it down to one typ to match all typ in that list *)
		fun narrow_down_type typ_lsts =
			case typ_lsts of
				x::y::xs => (case x of 
								Anything => narrow_down_type (y::xs)
								| TupleT ts => match_tuple narrow_down_type ts (y::xs)
								| _ => if (List.all (fn z => case z of
																Anything => true
																| _ => z = x) (y::xs)) 
									then SOME x 
									else NONE)
				| x::[] => (case x of Anything => SOME Anything | _ => SOME x)
				| [] => NONE

		(* Remove the squre braket of a one-element list *)
		fun unfold_lst_ele lst = (case lst of x::[] => x | _ => raise NoAnswer) 

		fun main_aux plst acc = 
			case plst of
					[] => acc
				  	| x::xs => (case x of
		  					  	Wildcard => main_aux xs (Anything::acc)
		  						| Variable _ => main_aux xs (Anything::acc)
		  						| UnitP => main_aux xs (UnitT::acc)
		  						| ConstP _ => main_aux xs (IntT::acc)
		  						| TupleP ps => ( main_aux xs 
		  										((TupleT(List.rev (main_aux ps [])))::acc) )
				  				| ConstructorP (s, p) => ( main_aux xs 
				  								((match_construct narrow_down_type datatyp_lst 
				  									(s, (unfold_lst_ele (main_aux [p] []))))::acc) )
			  			)
	in
		narrow_down_type(main_aux pattern_lst [])
		handle NoAnswer => NONE
	end
	