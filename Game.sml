use "Table.sml";

structure Game =
struct

datatype move = Draw   of Player.player * Card.card list
	      | Attack of Player.player * Player.player * Card.card
	      | Defend of Player.player * Card.card * Card.card 
	      | Pickup of Player.player * Card.card list
	      | Skip   of Player.player
	      | Giveup of Player.player
	      | Clear  of Card.card list 

exception NoPlayers
exception InvalidMove
exception NotImplemented

fun Move c (p1, p2, c1, c2, deck, tbl) =
    let val moveMap = [(#"1", Attack (p1, p2, c1)),
		       (#"2", Defend (p1, c1, c2)),
		       (#"3", Skip (p1)),
		       (#"4", Giveup (p1))]
    in
	case List.find (fn (k,_) => k = c) moveMap of
	    SOME (k,m) => SOME m
	  | _          => NONE
    end

fun pickCard p = raise NotImplemented

fun readKey () =
    case TextIO.input1 TextIO.stdIn of
	NONE   => readKey ()
      | SOME c => Move c 
    	      
fun game ps deck =
    let fun loop (ps, deck, tbl, moves) =
	    case ps of
		[]          => raise NoPlayers
	      | p::[]       => moves
	      | at::df::ps' => (case moves of
				    []   => raise NotImplemented
				  | m::_ => raise NotImplemented)
    in
	loop (ps, deck, Table.Table [], [])
    end
	      
end
