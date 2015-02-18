signature CARD =
sig
    datatype suit = Spades | Clubs | Diamonds | Hearts
    type card
    val Card : int * suit -> card
    val sameSuit : card -> card -> bool
    val sameRank : card -> card -> bool
    val same : card -> card -> bool
    val hasRank : card -> card list -> bool
    val find : card -> card list -> card option
    val remove : card -> card list -> card list
    val suit : card -> suit
    val value : card -> int
    val compareRank : card -> card -> order
    val toString   : card -> string
    val toStrings  : card list -> string
    val toStrings' : int -> card list -> unit
    val shuffledDeck : unit -> card list
end

structure Card :> CARD =
struct

datatype rank = Ace | King | Queen | Jack | Num of int
datatype suit = Spades | Clubs | Diamonds | Hearts
type card     = rank * suit

exception NotACard
		       
(* helper functions *)
fun range i j =
    if j < i then
    	rev (range j i)
    else
	List.tabulate(j-i+1, (fn x => x+i))

fun mapFind k zip =
    case List.find (fn (k',_) => k' = k) zip of
	SOME (k,v) => SOME v
      | NONE       => NONE
			  
fun mkRandomSeed () =
    let val date = Date.fromTimeLocal(Time.now())
	val min  = Date.minute(date)
	val sec  = Date.second(date)
    in
	Random.rand(min, sec)
    end

fun deleteAt (xs, n) = List.take(xs,n) @ List.drop(xs,n+1)

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

(* turns a list into a list of lists, where all sub-lists are of length n 
   [the last sub-list may be shorter] *)
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
		
(* ranks, associated string representations, and associated values *)
val ranks       = [Ace, King, Queen, Jack] @ map Num (range 10 2)
val rankStrings = ["A", "K", "Q", "J"] @ map Int.toString (range 10 2)
val rankStrMap  = ListPair.zip(ranks, rankStrings)
val rankValus   = range 14 2
val rankValuMap = ListPair.zip(ranks, rankValus)
val valuRankMap = ListPair.zip(rankValus, ranks)

(* suits and associated string representations *)
val suits       = [Spades, Diamonds, Clubs, Hearts]
val suitStrings = ["S", "D", "C", "H"] (* ["♠", "♦", "♣", "♥"] *)    
val suitStrMap  = ListPair.zip(suits, suitStrings)
			      
(* card comparisons *)
fun sameSuit (r1, s1) (r2, s2) = s1 = s2
fun sameRank (r1, s1) (r2, s2) = r1 = r2
fun same c1 c2 = (sameSuit c1 c2) andalso (sameRank c1 c2)

(* check if a list of cards contains the rank of a certain card *)
fun hasRank c = List.exists (fn c' => sameRank c c')

(* find and remove cards from a list *)
fun find c = List.find (fn c' => same c c') 
fun remove c' cs = 
    case cs of
	[]     => []
      | c::cs' => if   same c' c
		  then cs'
		  else c::(remove c' cs')
			  
(* get card suit *)
fun suit (r, s) = s
						  
(* calculate card value *)
fun value (r, s) =
    case mapFind r rankValuMap of
	SOME v => v
      | NONE   => raise NotACard

fun compareRank c1 c2 =
    let val v1 = value c1
	val v2 = value c2
    in
	Int.compare(v1,v2)
    end

(* convert cards to strings and printing functions *)
fun toString (r, s) =
    case (mapFind r rankStrMap, mapFind s suitStrMap) of
	(NONE, _)          => raise NotACard
      | (_, NONE)          => raise NotACard
      | (SOME r', SOME s') => r' ^ s'

val toStrings = (String.concatWith " ") o (map toString)
		      
fun toStrings' cardsPerLine =
    print o String.concat o (map (fn s => toStrings s ^ "\n")) o (listsOfN cardsPerLine)

(* deck generation *)
val fullDeck = foldl op@ [] (map (fn s => (map (fn r => (r, s)) ranks)) suits)
fun shuffled cs = randomSelect (length cs) cs
fun shuffledDeck () = shuffled fullDeck

(* card constructor *)
fun Card (n, s) =
    case mapFind n valuRankMap of
	SOME r => let val c = (r, s)
		  in
		      print (toString c ^ "\n"); c
		  end
      | NONE   => raise NotACard
		     
end	      
