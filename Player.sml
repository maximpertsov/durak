use "Card.sml";

signature PLAYER =
sig
    type player
    val Player : string * Card.card list -> player
    val name : player -> string
    val hand : player -> Card.card list
    val same : player -> player -> bool
    val draw : Card.card -> player -> player
    val draws : Card.card list -> player -> player
    val discard : Card.card -> player -> player
end

structure Player :> PLAYER =
struct

type player = {name: string, hand: Card.card list}

(* player constuctor and getters *)
fun Player (n, cs)    = {name = n, hand = cs}
fun name (p : player) = #name p
fun hand (p : player) = #hand p

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
						   
end
