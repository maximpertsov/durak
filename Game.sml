use "Card.sml";

structure Game =
struct

datatype moves = Draw
	       | Pickup
	       | Attack of Card.card
	       | Defend of (Card.card -> Card.card)
	       | Discard of Card.card

				
    
end
