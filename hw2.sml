(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once.*)
fun all_except_option (str, str_list) =
	let fun construct_lst lst =
		case lst of
			[] => []
			| x :: xs => if same_string(x, str) 
						then construct_lst xs
						else x :: construct_lst xs
		val result = construct_lst str_list
	in
		if length str_list = length result
		then NONE
		else SOME result
	end

(*Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result.*)
fun get_substitutions1 (lst, s) =
	case lst of
		[] => []
		| x::xs => case all_except_option(s, x) of
			NONE => get_substitutions1(xs, s)
			| SOME l => l @ get_substitutions1(xs, s)

(*Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function.*)
fun get_substitutions2 (lst, s) =
	let fun aux (ls, acc) =
		case ls of
			[] => acc
			| x::xs => aux(xs, case all_except_option(s, x) of
										NONE => acc
										| SOME l => acc @ l)
	in
		aux(lst, [])
	end

(*Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names).*)
fun similar_names (lst, {first=x, middle=y, last=z}) =
	let val result = get_substitutions2(lst, x)
		fun aux ls =
			case ls of
				[] => []
				| s::slist => {first=s, middle=y, last=z}::aux slist
	in
		{first=x, middle=y, last=z}::(aux result)
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough.*)
fun card_color cd =
	case cd of
		(Clubs, _) => Black
		| (Spades, _) => Black
		| (_, _) => Red

(*Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10).*)
fun card_value cd =
	case cd of
		(_, Num v) => v
		| (_, Ace) => 11
		| (_, _) => 10

(*Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e.*)
fun remove_card (cs, c, e) =
	let fun is_illegal ex =
		 	case ex of
		 		IllegalMove => true
		 		| _ => false
	in
		case cs of
			[] => if is_illegal e then raise e else []
			| x::xs => 	if x = c andalso is_illegal e
						then remove_card(xs, c, Empty) 
						else x::remove_card(xs, c, e)
	end

(*Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color.*)
fun all_same_color cs =
	case cs of
		[] => true
		| x::xs => case xs of
			[] => true
			| y::ys => card_color x = card_color y andalso all_same_color xs

(*Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive.*)
fun sum_cards cs =
	let fun aux(acc, lst) =
		case lst of
			[] => acc
			| x::xs => aux(acc + card_value x, xs)
	in
		aux(0, cs)
	end

(*Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above.*)
fun score (cs, goal) =
	let val sumcs = sum_cards cs
	in
		if sumcs > goal andalso all_same_color cs then 3 * (sumcs - goal) div 2
		else if sumcs <= goal andalso all_same_color cs then (goal - sumcs) div 2 
		else if sumcs > goal then 3 * (sumcs - goal)
		else goal - sumcs
	end

(*Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game.*)
fun officiate (cs, moves, goal) =
	let fun make_move (cards, helds, ms) =
			case ms of
				[] => score(helds, goal)
				| x::xs => case x of
					Discard c => make_move(cards, remove_card(helds, c, IllegalMove), xs)
					| Draw => case cards of
						[] => score(helds, goal)
						| y::ys => 	if (sum_cards (y::helds)) > goal
									then score(y::helds, goal)
									else make_move(ys, y::helds, xs)
	in
		make_move(cs, [], moves)
	end

(*Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
possible score. *)
fun replace_ace lst =
	case lst of
		[] => []
		| (s, Ace)::xs => (s, Num 1)::xs
		| x::xs => x::replace_ace xs

fun score_challenge (cs, goal) =
	let val score_Ace_11 = score(cs, goal)
		val score_Ace_1 = score(replace_ace cs, goal)
	in
		Int.min(score_Ace_1, score_Ace_11)
	end

fun officiate_challenge (cs, moves, goal) =
	let fun make_move (cards, helds, ms) =
			case ms of
				[] => score_challenge(helds, goal)
				| x::xs => case x of
					Discard c => make_move(cards, remove_card(helds, c, IllegalMove), xs)
					| Draw => case cards of
						[] => score_challenge(helds, goal)
						| y::ys => 	if (sum_cards (y::helds)) > goal andalso (sum_cards (replace_ace (y::helds))) > goal
									then score_challenge(y::helds, goal)
									else make_move(ys, y::helds, xs)
	in
		make_move(cs, [], moves)
	end

(*Write careful_player, which takes a card-list and a goal and returns a move-list*)
fun careful_player (cs, goal) =
	let fun possible_zero_score (held_ls, next_card, sub_ls, acc_bool, last_removed)  =
			if acc_bool 
			then SOME last_removed
			else case sub_ls of
				[] => NONE
				| x::xs => possible_zero_score(held_ls, next_card, xs, 
					acc_bool orelse score(next_card::(remove_card(held_ls, x, IllegalMove)), goal) = 0, x)

		fun construct_moves (clst, helds, acc) =
			case clst of
				[] => acc
				| x::xs => 	if score(helds, goal) = 0
							then acc
							else case possible_zero_score(helds, x, helds, false, x) of
								SOME c => acc @ [Discard c, Draw]
								| NONE => 	if (sum_cards (x::helds)) > goal
											then case helds of
												[] => acc
												| y::ys => construct_moves (clst, ys, acc @ [Discard y])
											else construct_moves(xs, (x::helds), acc @ [Draw])
	in
		construct_moves(cs, [], [])
	end
