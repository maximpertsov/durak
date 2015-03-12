use "Player.sml";

(* Naming conventions: *)
(* ca = attacking card *)
(* cd = defending card *)
(* pa = attacking player *)
(* pd = defending player *)

signature TABLE =
sig
    type table
    val Table : Card.card list -> table
    val same : table -> table -> bool (* Primarily for testing, but leave in final *)
    val addCard : Card.card -> table -> table
    val addPair : Card.card -> Card.card -> table -> table	  
    val remove : Card.card -> table -> table
    val attack : Player.player -> Card.card -> Player.player -> table ->
		 Player.player * table
    val attackUntil : int -> Player.player -> Card.card -> Player.player -> table ->
    		      Player.player * table
    val defend : Player.player -> Card.card -> Card.card -> table -> Card.suit ->
    		 Player.player * table
    val pickup : Player.player -> table -> Player.player * table
    val toString : table -> string
    val toLongString : table -> string (* might not need this... *)
end

structure Table :> TABLE =
struct

(* a table is represented as a list of "trick" pairs, which is an attacking card 
   paired with either a defending card or nothing, hence the use of an option for 
   the second item of the pair *)
(* TODO: consider representing the table as Map rather than a list *)
type table  = (Card.card * Card.card option) list

exception NoCard       of Card.card * Player.player * table
exception TooFewCards  of Player.player * Player.player * table
exception ExceedsLimit of int * Player.player * table
exception BadRank      of Player.player * table
exception CannotBeat   of Card.card * Card.card * Player.player * table
exception BadAttack    of Card.card * Player.player * table
exception NotImplemented

(* exception handler functions *)
fun handler exn =
    let fun handleOutput (pstr, res) = (print (pstr ^ "\n"); res)
	fun handleHelper exn =
	    case exn of
		NoCard (c,p,tbl)          => ("You don't have a " ^ Card.toString c ^ "!"
					     ,(p, tbl))
	      | TooFewCards (pd,pa,tbl)   => (Player.name pd ^ " does not have enough cards to defend against this attack!"
					     ,(pa, tbl))
	      | ExceedsLimit (lim,pa,tbl) => ("The maximum number of attacking cards (" ^ Int.toString lim ^ ") has been reached!"
					     ,(pa, tbl))
	      | BadRank (pa,tbl)          => ("This rank has not been played yet! You can only attack with ranks that have already been played during this round."
					     ,(pa, tbl))
	      | CannotBeat (ca,cd,pd,tbl) => ("Cannot beat " ^ Card.toString ca ^ " with " ^ Card.toString cd ^ "!"
					     ,(pd, tbl))
	      | BadAttack (ca,pd,tbl)     => (Card.toString ca ^ " has not been played or has already been beaten!"
					     ,(pd, tbl)) 
	      | _                         => raise NotImplemented
    in
	handleOutput (handleHelper exn)
    end

(* Create a new table with some unbeaten cards already played *)
fun Table cs = map (fn c => (c, NONE)) cs

(* check if two tables have the same card layout *)
fun same tbl1 tbl2 =
    let fun sameCardOpt c1opt c2opt =
	    case (c1opt, c2opt) of
		(SOME c1, SOME c2) => Card.same c1 c2
	      | (NONE, NONE)       => true
	      | _                  => false
	fun sameCardPair ((ca1, cd1), (ca2, cd2)) =
	    (Card.same ca1 ca2) andalso (sameCardOpt cd1 cd2)
    in
	ListPair.allEq sameCardPair (tbl1, tbl2)
    end

(* add an unbeaten card to the table *)
fun addCard c tbl = (c, NONE)::tbl

(* add a trick pair to the table *)	       
fun addPair ca cd tbl = (ca, SOME cd)::tbl

(* remove a card/trick pair from the table  *)
(* do nothing if the specified card cannot be found *)
fun remove c tbl =
    case tbl of
	[]             => []
      | (c1, c2)::tbl' => if   Card.same c c1
			  then tbl'
			  else (c1, c2)::(remove c tbl')   
    
(* return a list of all cards on the table that have not been beaten yet *)
fun unbeatenCards tbl =
    let val (beat, unbeat) = List.partition (fn (_, cd) => isSome cd) tbl
    in
	map #1 unbeat
    end

(* return a list of all cards on the table *)
fun allCards tbl =
    let fun loop (tbl, acc) =
	case tbl of
	    []                  => acc
	  | (ca, SOME cd)::tbl' => loop (tbl', cd::ca::acc)
	  | (ca, NONE)::tbl'    => loop (tbl', ca::acc)
    in
	loop (tbl, [])
    end

local
    (* check if player has enough cards to accept additional attacks *)
    fun hasExcessCards pd tbl =
	Player.handSize pd - (length o unbeatenCards) tbl > 0

    (* add card to table if table is empty or if a matching rank is on the table *)
    fun addIfHasRank pa ca tbl =
	case tbl of
	    [] => (ca, NONE)::tbl
	  | _  => if   Card.hasRank ca (allCards tbl)
		  then addCard ca tbl
		  else raise BadRank (pa, tbl)
			      
    fun attackHelper pa ca pd tbl =
	if   hasExcessCards pd tbl
	then addIfHasRank pa ca tbl
	else raise TooFewCards (pd, pa, tbl)
in
fun attack pa ca pd tbl =
    (case Player.find ca pa of
	 NONE     => raise NoCard (ca, pa, tbl)
       | SOME ca' => let val pa'  = Player.discard ca' pa
			 val tbl' = attackHelper pa ca' pd tbl
		     in
			 (pa', tbl')
		     end)
    handle exn => handler exn
			  
(* same as attackHelper but subject to a maximum attack limit *)
fun attackUntil lim pa ca pd tbl =
    (if   length tbl < lim
     then attack pa ca pd tbl
     else raise ExceedsLimit (lim, pa, tbl))
    handle exn => handler exn
end

(* specified player plays a card to beat an attacking card on the table. *)
local
    (* add pair (pa, SOME pd) to table if pd beats pa *)
    fun addIfBeats pd cd ca tbl trump =
	if   Card.beats cd ca trump
	then addPair ca cd (remove ca tbl)
	else raise CannotBeat (ca, cd, pd, tbl)
    
    fun defendHelper pd cd ca tbl trump =
	case Card.find ca (unbeatenCards tbl) of
	    NONE   => raise BadAttack (ca, pd, tbl)
	  | SOME _ => addIfBeats pd cd ca tbl trump
in
fun defend pd cd ca tbl trump =
    (case Player.find cd pd of
	 NONE     => raise NoCard (cd, pd, tbl)
       | SOME cd' => let val pd' = Player.discard cd' pd
			 val tbl'  = defendHelper pd cd' ca tbl trump
		     in
			 (pd', tbl')
		     end)
    handle exn => handler exn 
end

(* player picks up all cards on the table *)
fun pickup p tbl =
    (Player.draws (allCards tbl) p, Table [])

local
    fun toStringHelper f tbl =
	let fun toStringPair f (c, cOpt) =
		case cOpt of
		    NONE    => f c
		  | SOME c' => "{" ^ (f c) ^ "-" ^ (f c') ^ "}"
	in
	    String.concatWith " " (map (toStringPair f) tbl)
	end
in
val toString = toStringHelper Card.toString
val toLongString = toStringHelper Card.toLongString
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

val p = Player.Player ("A", [Card.Card' (2,4)]);

val t = Table.Table [];

val p' = p;

val c = Card.Card' (2,4);
Table.attack p (Card.Card' (3,3)) p' t;
