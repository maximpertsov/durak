use "Card.sml";

signature TABLE =
sig
    type table = (Card.card * Card.card option) list
    type player
    val attack : Card.card -> player -> table -> table
end

structure Table :> TABLE =
struct

type table  = (Card.card * Card.card option) list
type player = {name: string, hand: Card.card list}

exception NotEnoughCards
exception MissingRank

fun lookup c tbl = List.find (fn (c',_) => c' = c) tbl

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
fun name (p : player) = #name p
fun hand (p : player) = #hand p
    
fun numExcessCards p tbl = (length o hand) p - (length o unbeatenCards) tbl

(* check if the defending card 'c' beats the attacking card 'atk'. 'c' defeats 'atk' if 'c' is the same suit and higher rank than 'atk', or if 'c' has the trump suit *)
fun beats c atk trump =
    if Card.sameSuit c atk then
	case Card.compareRank c atk of
	    GREATER => true
	  | _       => false
    else Card.suit c = trump

(* play a card that another player has to beat. Can only play card if:
1. The field is empty
2. The rank of the attacking card is already on the field AND the defending player has more cards in their hand than there are unbeaten cards already on the table *)
fun attack c p tbl =
    (if numExcessCards p tbl > 0 then
	 case tbl of
	     [] => (c, NONE)::tbl
	   | _  => if Card.hasRank c (allCards tbl) then
		       (c, NONE)::tbl
		   else
		       raise MissingRank
     else
	 raise NotEnoughCards)
    handle NotEnoughCards => (print ((#name p) ^ " does not have enough cards to defend against this attack!"); tbl)
	 | MissingRank    => (print "This rank has not been played yet! You can only attack with ranks that have already been played during this round."; tbl)
end

(* TESTING *)
val d = Card.shuffledDeck()
val dStr = Card.toStrings d

val p1 = {name= "Max", hand= List.take (d, 6)}
val d' = List.drop (d, 6)
	     
val p2  = {name= "Rishi", hand= List.take (d', 6)}
val d'' = List.drop (d', 6)
