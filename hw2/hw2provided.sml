(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1. *) 
fun all_except_option (str, str_lst) =
  let
      fun in_ (a, [])    = false
	| in_ (a, x::xs) = same_string(a, x) orelse in_(a, xs)
      fun remove (str, []) = [] 
        | remove (str, x::xs) = if not (same_string(str, x))
				then [x] @ remove(str, xs)
				else []  @ remove(str, xs)
  in 			   
      if not (in_(str, str_lst))
      then NONE
      else SOME (remove(str, str_lst))
  end
(* 2. *)
fun get_substitutions1 (str_lst_lst, str) =
  let
      fun get_some (NONE    ) = []
	| get_some (SOME lst) = lst
  in 
      case str_lst_lst of
	  []    => []
	| x::xs => get_some(all_except_option(x, str)) @
		   get_substitutions1(xs, str)
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
