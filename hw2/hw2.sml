(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1. *)
(* (a) *) 
fun all_except_option (str, str_lst) =
  let
      fun in_ (a, []   ) = false
	| in_ (a, x::xs) = same_string(a, x) orelse in_(a, xs)
      fun remove (str, []   ) = [] 
        | remove (str, x::xs) = if not (same_string(str, x))
				then [x] @ remove(str, xs)
				else []  @ remove(str, xs)
  in 			   
      if not (in_(str, str_lst)) then NONE
      else SOME (remove(str, str_lst))
  end
(* (b) *)
fun get_substitutions1 (str_lst_lst, str) =
  let
      fun get_some (NONE    ) = []
	| get_some (SOME lst) = lst
  in 
      case str_lst_lst of
	  []    => []
	| x::xs => get_some(all_except_option(str, x)) @
		   get_substitutions1(xs, str)
  end 
(* (c) *) 
fun get_substitutions2 (str_lst_lst, str) =
  let
      fun get_some (NONE    ) = []
	| get_some (SOME lst) = lst
      fun aux ([],    s) = []
        | aux (x::xs, s) = get_some(all_except_option(s, x)) @ aux(xs, s)
  in
      aux(str_lst_lst, str)
  end
(* (d) *)
fun similar_names (str_lst_lst, {first=f, middle=m, last=l}) =
  let
      fun somefirst2full (some_first : string) =
	{first=some_first, middle=m, last=l}
  in
      [{first=f, middle=m, last=l}] @
      (map somefirst2full (get_substitutions2(str_lst_lst, f)))
  end
    
(* 2. *)
(* given *) 
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 

type card = suit * rank
datatype color = Red | Black

exception IllegalMove			   
datatype move = Discard of card | Draw 

(* for testing *)
val c1  = (Clubs, Jack);
val cs1 = [c1, (Hearts, King), (Hearts, Num 6), (Spades, Ace)]; 

(* solutions *) 
fun card_color (c : card) =
  case c of
      (Clubs,    _) => Black
    | (Spades,   _) => Black
    | (Hearts,   _) => Red
    | (Diamonds, _) => Red

fun card_value (c : card) =
  case c of
     (_, Num n) => n
   | (_, Ace  ) => 11
   | (_, _    ) => 10

fun remove_card (cs : card list, c : card, e : exn) =
  let
      fun in_ (a, []   ) = false
	| in_ (a, x::xs) = a = x orelse in_(a, xs)

      fun  remove (c_, cs_) =
	case cs_ of
	      []    => []
	    | x::xs => if x = c_
		      then []  @ remove(c_, xs)
		      else [x] @ remove(c_, xs)
  in
      if not (in_(c, cs)) then raise e
      else remove(c, cs)
  end 
	 
fun all_same_color (cs : card list) =
  case cs of
      []            => true
   |  [c1]          => true
   |  c1::[c2]      => card_color(c1) = card_color(c2) 
   |  c1::(c2::cs_) => card_color(c1) = card_color(c2)
		       andalso all_same_color(c2::cs_) 

fun sum_cards (cs : card list) =
  let
      fun aux ([]    ) = 0
	| aux (c::cs_) = card_value(c) + aux(cs_)
  in
      aux(cs) 
  end

fun score (held_cards : card list, goal : int) =
  let
      val s = sum_cards(held_cards)

      val pre_score = if s > goal then 3 * (s - goal)
		      else (goal - s)
  in
      if all_same_color(held_cards) then (pre_score div 2)
      else pre_score
  end

fun officiate (card_lst : card list, move_lst : move list, goal : int) =
  let
      fun eval (card_lst_, held_cards, move_lst_) =
	case move_lst_ of
	    []    => score(held_cards, goal) 
	  | m::ms => case m of
			 Discard c => eval(card_lst_,
					   remove_card(held_cards, c, IllegalMove),
					   ms)
		       | Draw => case card_lst_ of
				    []    => score(held_cards, goal) 
				  | c::cs => if sum_cards(c::held_cards) > goal
					     then score(c::held_cards, goal)
					     else eval(cs, c::held_cards, ms)
								
  in
      eval(card_lst, [], move_lst) 
  end
      
