use "Card.sml";

signature PLAYER =
sig
    type player
    val Player : string * Card.card list -> player
    val name : player -> string
    val hand : player -> Card.card list
    val handSize : player -> int
    val same : player -> player -> bool
    val draw : Card.card -> player -> player
    val draws : Card.card list -> player -> player
    val drawsUntil : int -> Card.card list -> player -> player * Card.card list
    val allDrawUntil : int -> Card.card list -> player list ->
		       (player list * Card.card list)
    val discard : Card.card -> player -> player
    val toString : player -> string
    val toLongString : player -> string
end

structure Player :> PLAYER =
struct

type player = {name: string, hand: Card.card list}

(* player constuctor and getters *)
fun Player (n, cs)    = {name = n, hand = cs}
fun name (p : player) = #name p
fun hand (p : player) = #hand p
val handSize = length o hand

(* check if players are the same *)
fun same p1 p2 =
    let val hands     = (hand p1, hand p2)
	val sameCards = ListPair.allEq (fn (c1,c2) => Card.same c1 c2) 
    in
	(name p1 = name p2) andalso (sameCards hands)
    end

(* basic player actions *)
fun draw c p    = Player (name p, c::(hand p))
fun draws cs p  = foldl (fn (c,p) => draw c p) p cs
fun discard c p = Player (name p, Card.remove c (hand p))

(* draw cards until a certain card limit *)
fun drawsUntil limit cs p =
    let val cardDeficit = Int.max (0, limit - handSize p)
	val cardsToDraw = List.take (cs, cardDeficit)
    in
	(draws cardsToDraw p, List.drop (cs, cardDeficit))
    end

(* all players draw in order until all reach the card limit or the deck runs out *)
fun allDrawUntil limit cs ps =
    let fun loop (limit, cs, ps, acc) =
	    case ps of
		[]     => (rev acc, cs)
	      | p::ps' => let val (p', cs') = drawsUntil limit cs p
			  in
			      loop (limit, cs', ps', p'::acc) 
			  end
    in
	loop (limit, cs, ps, [])
    end
		       
			 
(* string representation *)
local
    fun toStringHelper f p = (name p) ^ ": " ^ (f o hand) p
in
val toString     = toStringHelper Card.toStrings
val toLongString = toStringHelper Card.toLongStrings
end

end
