signature LISTUTIL =
sig
    val range : int -> int -> int list
    val assoc : 'a -> ('a * 'b) list -> 'b option
    val invassoc : 'b -> ('a * 'b) list -> 'a option
    val listsOfN : int -> 'a list
    val randomSelect : int -> 'a list -> 'a list
    val sort : (int * int) -> order -> 'a list -> 'a list
end

structure ListUtil =
struct

(* return a list of all integers from i to j (counts backwards if i > j) *)
fun range i j =
    if j < i then
    	rev (range j i)
    else
	List.tabulate(j-i+1, (fn x => x+i))

(* find a value in an associative list based on a key *)
fun assoc k ps =
    case List.find (fn (k',_) => k' = k) ps of
	SOME (k,v) => SOME v
      | NONE       => NONE

(* find a key in an associative list based on a value *)
fun invassoc v ps =
    let val (ks, vs) = ListPair.unzip ps
    in
	assoc v (ListPair.zip (vs, ks))
    end		  

(* turns a list into a list of lists, where all sub-lists are of length n or less *)
fun listsOfN n xs =
    let fun loop (m, xs, sub, acc) =
	    case xs of
		[]     => (case sub of
			       [] => acc
			     | _  => sub::acc)
	      | x::xs' => let val sub' = x::sub
			  in
			      if m > 1 then
				  loop(m-1, xs', sub', acc)
			      else
				  loop(n, xs', [], (rev sub')::acc)
			  end
    in
	(rev o loop) (n, xs, [], [])
    end

fun mkRandomSeed () =
    let val date = Date.fromTimeLocal(Time.now())
	val min  = Date.minute(date)
	val sec  = Date.second(date)
    in
	Random.rand(min, sec)
    end

fun deleteAt (xs, n) = List.take(xs,n) @ List.drop(xs,n+1)

(* randomly select n elements from a list *)
fun randomSelect n xs =
    let val r = mkRandomSeed()
	fun loop (n, max, xs, acc) =
	    if n < 1 orelse max = 0 then
		acc
	    else
		let val k = Random.randRange (0,max-1) r
		    val x = List.nth(xs, k)
 		in
		    loop(n-1, max-1, deleteAt(xs,k), x::acc)
		end
    in
	loop(n, length xs, xs, [])
    end

(* sort - implemented via merge sort *)
local
    fun split' f xs =
	let
	    fun loop (xs, lacc, racc, left) =
		case xs of
		    []     => (f lacc, f racc)
		  | x::xs' => if   left
			      then loop(xs', x::lacc, racc, false)
			      else loop(xs', lacc, x::racc, true)
	in
	    loop (xs, [], [], true)
	end
	    
    fun merge f (xs, ys) =
	case (xs, ys) of
	    ([], ys)         => ys
	  | (xs, [])         => xs
	  | (x::xs', y::ys') => (case f(y,x) of
				     LESS => y::(merge f (xs, ys'))
				   | _    => x::(merge f (xs', ys)))
in
fun sort f xs =
    case xs of
	[]    => []
      | x::[] => [x]
      | _     => merge f (split' (sort f) xs)
end
		       
end
