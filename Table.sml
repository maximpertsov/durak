use "Card.sml";

signature TABLE =
sig
    type table = (Card.card * Card.card option) list
    type player
    val Player : string * Card.card list -> player
    val name : player -> string
    val hand : player -> Card.card list
    val attack : player -> Card.card -> player -> table -> player * table
end

structure Table :> TABLE =
struct

type table  = (Card.card * Card.card option) list
type player = {name: string, hand: Card.card list}

exception MissingCard
exception NotEnoughCards
exception NoMatchingRank

fun unbeatenCards tbl =
    let val (beat, unbeat) = List.partition (fn (_, def) => isSome def) tbl
    in
	map #1 unbeat
    end

fun allCards tbl =
    let fun loop (tbl, acc) =
	case tbl of
	    []                    => acc
	  | (atk, SOME def)::tbl' => loop (tbl', def::atk::acc)
	  | (atk, NONE)::tbl'     => loop (tbl', atk::acc)
    in
	loop (tbl, [])
    end

(* player info functions *)
fun Player (n, cs) = {name = n, hand = cs}
fun name (p : player) = #name p
fun hand (p : player) = #hand p

val numCards = length o hand
			      
fun numExcessCards p tbl = numCards p - (length o unbeatenCards) tbl

(* check if the defending card 'c' beats the attacking card 'atk'. 'c' defeats 'atk' if 'c' is the same suit and higher rank than 'atk', or if 'c' has the trump suit *)
fun beats c atk trump =
    if Card.sameSuit c atk then
	case Card.compareRank c atk of
	    GREATER => true
	  | _       => false
    else Card.suit c = trump

(* play a card that another player has to beat. Can only play card if:
1. The defending player has more cards in their hand than there are unbeaten cards already on the table
   AND
2. The rank of the attacking card is already on the field OR no cards have been played yet *)
fun attackHelper c p tbl =
    if numExcessCards p tbl > 0 then
	case tbl of
	    [] => (c, NONE)::tbl
	  | _  => if Card.hasRank c (allCards tbl) then
		      (c, NONE)::tbl
		  else
		      raise NoMatchingRank
    else
	raise NotEnoughCards
	      
fun attack pAtk c pDef tbl =
    (let val h = hand pAtk
     in
	 case Card.find c h of
	     SOME c' => (Player (name pAtk, Card.remove c' h), attackHelper c' pDef tbl)
	   | NONE    => raise MissingCard
     end)
    handle MissingCard    => (print ("You don't have a " ^ Card.toString c ^ "!"); (pAtk, tbl))
	 | NotEnoughCards => (print (name pDef ^ " does not have enough cards to defend against this attack!"); (pAtk, tbl))
	 | NoMatchingRank => (print "This rank has not been played yet! You can only attack with ranks that have already been played during this round."; (pAtk, tbl))
			      		      
end
	
(* TESTING *)
val d = Card.shuffledDeck()
val dStr = Card.toStrings d

val p1 = Table.Player ("Max", List.take (d, 6))
val p1Str = Card.toStrings (Table.hand p1)
val d' = List.drop (d, 6)
	    
val p2 = Table.Player ("Rishi", List.take (d', 6))
val p2Str = Card.toStrings (Table.hand p2)
val d'' = List.drop (d', 6)
