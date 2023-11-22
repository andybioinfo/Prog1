(* ### Liste der Deklarationen ### *)

let rec iter f n s = if n < 1 then s else iter f (n-1) (f s);; 

let rec first (p:int->bool) (s:int) : int = if p s then s else first p (s+1);;

let rec iterup m n s f = if m > n then s else iterup (m + 1) n (f(m,s)) f;;

let rec iterdn n m s f = if n < m then s else iterdn (n - 1) m (f(n,s)) f;;

(*Listenfaltung*)

 let rec foldr f xl a = match xl with 					
 | [] -> a 				
 | h::tl -> f h (foldr f tl a);; 							
 
 let rec foldl f xl a = match xl with 
 | [] -> a
 | h::tl -> foldl f tl (f h a);;
