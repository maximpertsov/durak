use "Card.sml";

structure Game =
struct

type player = {hand : Card.card list}

datatype moves = Draw
	       | Pickup
	       | Attack of Card.card
	       | Defend of (Card.card -> Card.card)
	       | Discard of Card.card

exception NoCards
exception NotImplemented

(* add card to player's hand *)
fun takeCard c {hand = h} = {hand = c::h}
    
(* player draws from card list -> returns tuple containing card list and player with updated hand after drawing *)
fun draw cs p =
    (case cs of
	 []     => raise NoCards
       | c::cs' => (cs', takeCard c p))
    handle NoCards => (print "No more cards to draw!"; (cs, p))

end
