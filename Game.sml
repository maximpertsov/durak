use "Table.sml";

structure Game =
struct

type suit = Card.suit
type card = Card.card
type player = Player.player
type table = Table.table

datatype action = Play    (* attack or defend *)
		| Stay    (* refrain from attacking *)
		| Giveup  (* concede attack and pick up cards *)


datatype moveF = AttackF of int -> player -> card -> player -> table -> player * table
	       | DefendF of player -> card -> card -> table -> suit -> player * table
		      
(* for capturing move *)
datatype move = Draw   of Player.player * Card.card list
	      | Attack of Player.player * Player.player * Card.card
	      | Defend of Player.player * Card.card * Card.card 
	      | Pickup of Player.player * Card.card list
	      | Skip   of Player.player
	      | Clear  of Card.card list 

exception NoPlayers
exception InvalidMove
exception NotImplemented

val moveMap1 = [(#"1", AttackF Table.attackUntil),
		(#"2", DefendF Table.defend)]

fun readKey moveMap =
    case TextIO.input1 TextIO.stdIn of
	NONE   => readKey moveMap
      | SOME c => (case List.find (fn (k,_) => k = c) moveMap of
		       SOME (k,m) => SOME m
		     | _          => raise InvalidMove)
    	      
(* fun game ps deck = *)
(*     let fun loop (ps, deck, tbl, moves) = *)
(* 	    case ps of *)
(* 		[]          => raise NoPlayers *)
(* 	      | p::[]       => moves *)
(* 	      | at::df::ps' =>  *)
(*     in *)
(* 	loop (ps, deck, Table.Table [], []) *)
(*     end *)
	      
end
