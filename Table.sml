use "Card.sml";

signature TABLE =
sig
    type table = (Card.card * Card.card option) list
    type player
    val Table : Card.card list -> table
    val sameTable : table -> table -> bool (* Primarily for testing, but leave in final code *)
    val add : Card.card -> table -> table
    val Player : string * Card.card list -> player
    val name : player -> string
    val hand : player -> Card.card list
    val remove : Card.card -> table -> table
    val attack : player -> Card.card -> player -> table -> player * table
    val defend : player -> Card.card -> Card.card -> table -> Card.suit ->
		 player * table
end

structure Table :> TABLE =
struct

(* a table is represented as a list of "trick" pairs, which is an attacking card paired with either a defending card or nothing (hence the use of an option for the second item of the pair) *)
(* TODO: consider representing the table as Map rather than a list... *)
type table  = (Card.card * Card.card option) list
type player = {name: string, hand: Card.card list}

exception MissingCard
exception NotEnoughCards
exception NoMatchingRank
exception CannotBeatCard
exception MissingAttackCard

(* Create a new table with some unbeaten cards already played *)
fun Table cs = map (fn c => (c, NONE)) cs

(* check if two tables have the same card layout *)
fun sameTable tbl1 tbl2 =
    let fun sameCardOpt c1opt c2opt =
	    case (c1opt, c2opt) of
		(SOME c1, SOME c2) => Card.same c1 c2
	      | (NONE, NONE)       => true
	      | _                  => false
	fun sameCardPair ((atk1, def1), (atk2, def2)) =
	    (Card.same atk1 atk2) andalso (sameCardOpt def1 def2)
    in
	ListPair.allEq sameCardPair (tbl1, tbl2)
    end

(* add an unbeaten card to the table *)
fun add c tbl = (c, NONE)::tbl

(* remove a card/trick pair from the table. Do nothing if the specified card 
   cannot be found *)
fun remove c tbl =
    case tbl of
	[]             => []
      | (c1, c2)::tbl' => if   Card.same c c1
			  then tbl'
			  else (c1, c2)::(remove c tbl')

(* return a list of all cards on the table that have not been beaten yet *)
fun unbeatenCards tbl =
    let val (beat, unbeat) = List.partition (fn (_, def) => isSome def) tbl
    in
	map #1 unbeat
    end

(* return a list of all cards on the table *)
fun allCards tbl =
    let fun loop (tbl, acc) =
	case tbl of
	    []                    => acc
	  | (atk, SOME def)::tbl' => loop (tbl', def::atk::acc)
	  | (atk, NONE)::tbl'     => loop (tbl', atk::acc)
    in
	loop (tbl, [])
    end

(* get player/hand information *)
fun Player (n, cs) = {name = n, hand = cs}
fun name (p : player) = #name p
fun hand (p : player) = #hand p
val numCards = length o hand
fun numExcessCards p tbl = numCards p - (length o unbeatenCards) tbl

(* check if the defending card 'c' beats the attacking card 'atk'. 'c' defeats 'atk'    if 'c' is the same suit and higher rank than 'atk', or if 'c' has the 
   trump suit *)
fun beats cDef cAtk trump =
    if Card.sameSuit cDef cAtk then
	case Card.compareRank cDef cAtk of
	    GREATER => true
	  | _       => false
    else Card.suit cDef = trump

(* specified player plays a card that another player has to beat. 
   Can only play card if:
   1. The defending player has more cards in their hand than there are unbeaten 
      cards already on the table
   AND
   2. EITHER the rank of the attacking card is already on the field 
             OR no cards have been played yet *)
local
    fun attackHelper c p tbl =
	if numExcessCards p tbl > 0 then
	    case tbl of
		[] => (c, NONE)::tbl
	      | _  => if   Card.hasRank c (allCards tbl)
		      then add c tbl
		      else raise NoMatchingRank
	else
	    raise NotEnoughCards
in
fun attack pAtk c pDef tbl =
    (let val h = hand pAtk
     in
	 case Card.find c h of
	     SOME c' => let val name' = name pAtk
			    val hand' = Card.remove c' h
			    val tbl'  = attackHelper c' pDef tbl
			in
			    (Player (name', hand'), tbl')
			end
	   | NONE    => raise MissingCard
     end)
    handle MissingCard    => (print ("You don't have a " ^ Card.toString c ^ "!"); (pAtk, tbl))
	 | NotEnoughCards => (print (name pDef ^ " does not have enough cards to defend against this attack!"); (pAtk, tbl))
	 | NoMatchingRank => (print "This rank has not been played yet! You can only attack with ranks that have already been played during this round."; (pAtk, tbl))
end

(* specified player plays a card to beat an attacking card on the table. 
   Must specify the trump suit *)	
local
    fun defendHelper cDef cAtk tbl trump =
	case Card.find cAtk (unbeatenCards tbl) of
	    SOME _ => if beats cDef cAtk trump then
			  (cAtk, SOME cDef)::(remove cAtk tbl)
		      else
			  raise CannotBeatCard
	  | NONE   => raise MissingAttackCard
in		
fun defend pDef cDef cAtk tbl trump =
    (let val h = hand pDef
     in
	 case Card.find cDef h of
	     SOME c' => let val name' = name pDef
			    val hand' = Card.remove cDef h
			    val tbl'  = defendHelper cDef cAtk tbl trump
			in
			    (Player (name', hand'), tbl')
			end
	   | NONE    => raise MissingCard
     end)
    handle MissingCard       => (print ("You don't have a " ^ Card.toString cDef ^ "!"); (pDef, tbl))
	 | CannotBeatCard    => (print ("Cannot beat " ^ Card.toString cAtk ^ " with " ^ Card.toString cDef ^ "!"); (pDef, tbl))
	 | MissingAttackCard => (print (Card.toString cAtk ^ " has not been played or has already been beaten!"); (pDef, tbl))
end
	
end
	
(* TESTING *)
(* val d = Card.shuffledDeck() *)
(* val dStr = Card.toStrings d *)

(* val p1 = Table.Player ("Max", List.take (d, 6)) *)
(* val p1Str = Card.toStrings (Table.hand p1) *)
(* val d' = List.drop (d, 6) *)
	    
(* val p2 = Table.Player ("Rishi", List.take (d', 6)) *)
(* val p2Str = Card.toStrings (Table.hand p2) *)
(* val d'' = List.drop (d', 6) *)
