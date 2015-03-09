use "ListUtil.sml";

signature CARD =
sig
    datatype suit = Spades | Clubs | Diamonds | Hearts
    type card
    type ord_key = card
    val compare : card * card -> order
    val Card : int * suit -> card
    val Card' : int * int -> card (* ONLY FOR TESTING!! *)
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
    val toLongString   : card -> string
    val toLongStrings  : card list -> string
    val toLongStrings' : int -> card list -> unit
    val shuffledDeck : unit -> card list
end

structure Card :> CARD =
struct

datatype rank = Ace | King | Queen | Jack | Num of int
datatype suit = Spades | Clubs | Diamonds | Hearts
type card     = rank * suit

exception NotACard
		       
(* helper functions imported from ListUtil *)
val range = ListUtil.range
val assoc = ListUtil.assoc
val invassoc = ListUtil.invassoc
val randomSelect = ListUtil.randomSelect
val listsOfN = ListUtil.listsOfN
    
(* rank string representations *)
val ranks        = [Ace, King, Queen, Jack] @ map Num (range 10 2)
val rankStrings  = ["A", "K", "Q", "J"] @ map Int.toString (range 10 2)
val rankLongStrs = ["Ace", "King", "Queen", "Jack"] @ map Int.toString (range 10 2)
val rankStrMap   = ListPair.zip(ranks, rankStrings)
val rankLStrMap  = ListPair.zip(ranks, rankLongStrs)
			       
(* rank values *)
val rankValus    = range 14 2
val rankValuMap  = ListPair.zip(ranks, rankValus)

(* suit string representations *)
val suits        = [Spades, Diamonds, Clubs, Hearts]
val suitStrings  = ["S", "D", "C", "H"] (* ["♠", "♦", "♣", "♥"] *)
val suitLongStrs = ["Spades", "Diamonds", "Clubs", "Hearts"]
val suitStrMap   = ListPair.zip(suits, suitStrings)
val suitLStrMap  = ListPair.zip(suits, suitLongStrs)

(* suit ids *)
val suitIds    = range 4 1
val suitIdMap  = ListPair.zip(suits, suitIds)       
			       
(* card comparisons *)
fun sameSuit (r1, s1) (r2, s2) = s1 = s2
fun sameRank (r1, s1) (r2, s2) = r1 = r2
fun same c1 c2 = (sameSuit c1 c2) andalso (sameRank c1 c2)

(* check if a list of cards contains the rank of a certain card *)
fun hasRank c = List.exists (fn c' => sameRank c c')

(* find and remove cards from a list *)
fun find c = List.find (fn c' => same c c') 
fun remove c cs = 
    case cs of
	[]      => []
      | c'::cs' => if   same c c'
		   then cs'
		   else c'::(remove c cs')
			  
(* get card suit *)
fun suit (r, s) = s
						  
(* calculate card value *)
fun value (r, s) =
    case assoc r rankValuMap of
	SOME v => v
      | NONE   => raise NotACard

fun compareRank c1 c2 =
    let val vs = (value c1, value c2)
    in
	Int.compare vs
    end

(* unique card key for implementing Ordered Maps and Sets *)
type ord_key = card
		   
fun compare (c1,c2) =
    let fun cardId (r, s) =
	    case assoc s suitIdMap of
		NONE     => raise NotACard
	      | SOME sid => 100 * sid + value (r, s)
    in
	Int.compare (cardId c1, cardId c2)
    end
							  
(* convert cards to strings *)
local 
    fun toStringHelper rMap sMap midChr (r, s) =
	case (assoc r rMap, assoc s sMap) of
	    (NONE, _)          => raise NotACard
	  | (_, NONE)          => raise NotACard
	  | (SOME r', SOME s') => r' ^ midChr ^ s'
						    
in
val toString     = toStringHelper rankStrMap suitStrMap ""
val toLongString = toStringHelper rankLStrMap suitLStrMap " of "
end
				  
(* convert list of cards to a string *)
local
    fun toStringsHelper f delimChr = (String.concatWith delimChr) o (map f)
in
val toStrings     = toStringsHelper toString " "
val toLongStrings = toStringsHelper toLongString ", "
end
					
(* print cards in list, specifying how many cards should appear on each line *)
local
    fun toStringsHelper' f cardsPerLine cs =
	let val lines = map (fn s => f s ^ "\n") (listsOfN cardsPerLine cs)
	in
	    print (String.concat lines)
	end
in
fun toStrings' cpl     = toStringsHelper' toStrings cpl
fun toLongStrings' cpl = toStringsHelper' toLongStrings cpl
end
								     
(* deck generation *)
val fullDeck        = foldl op@ [] (map (fn s => (map (fn r => (r, s)) ranks)) suits)
fun shuffled cs     = randomSelect (length cs) cs
fun shuffledDeck () = shuffled fullDeck

(* card constructor *)
fun Card (i, s) =
    case invassoc i rankValuMap of
	SOME r => (r, s)
      | NONE   => raise NotACard

(* alternate card constructor - ONLY FOR TESTING! *)
fun Card' (i, j) =
    case invassoc i rankValuMap of
	NONE   => raise NotACard
      | SOME r => (case invassoc j suitIdMap of
		       NONE   => raise NotACard
		     | SOME s => (r, s))

end
