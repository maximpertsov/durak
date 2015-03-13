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
    val attack : Player.player -> Card.card -> Player.player -> table -> int ->
    		 Player.player * table
    (* val attackUntil : int -> Player.player -> Card.card -> Player.player -> table -> *)
    (* 		      Player.player * table *)
    val defend : Player.player -> Card.card -> Card.card -> table -> Card.suit ->
    		 Player.player * table
    val pickup : Player.player -> table -> Player.player * table
    val toString : table -> string
    val toLongString : table -> string (* might not need this... *)
end

structure Table :> TABLE =
struct

(* a table is a list of card trick pairs *)
datatype trick = Attack of Card.card
	       | Beat of Card.card * Card.card
type table = trick list
(* type table     = (Card.card * Card.card option) list *)

exception NoCard       of Card.card * Player.player * table
exception TooFewCards  of Player.player * Player.player * table
exception ExceedsLimit of int * Player.player * table
exception BadRank      of Player.player * table
exception CannotBeat   of Card.card * Card.card * Player.player * table
exception NoAttack    of Card.card * Player.player * table
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
	      | NoAttack (ca,pd,tbl)     => (Card.toString ca ^ " has not been played or has already been beaten!"
					     ,(pd, tbl)) 
	      | _                         => raise NotImplemented
    in
	handleOutput (handleHelper exn)
    end

(* Create a new table with some unbeaten cards already played *)
(* fun Table cs = map (fn c => (c, NONE)) cs *)
fun Table cs = map Attack cs

fun beaten t =
    case t of
	Attack _ => false
      | Beat _   => true

fun attackCard t =
    case t of
	Attack ca    => ca
      | Beat (ca, _) => ca

fun defendCard t =
    case t of
	Attack _     => NONE
      | Beat (_, cd) => SOME cd

fun sameTrick t1 t2 =
    case (t1, t2) of
	(Attack ca1, Attack ca2)           => Card.same ca1 ca2
      | (Beat (ca1, cd1), Beat (ca2, cd2)) => Card.same ca1 ca2
					      andalso Card.same cd1 cd2
      | _                                  => false 

(* check if two tables have the same card layout *)
fun same tbl1 tbl2 = ListPair.allEq (fn (t1, t2) => sameTrick t1 t2) (tbl1, tbl2)

(* add an unbeaten card to the table *)
fun addCard ca tbl = (Attack ca)::tbl
		   
(* add a trick pair to the table *)
fun addPair ca cd tbl = (Beat (ca, cd))::tbl
				    
(* remove a card/trick pair from the table  *)
fun remove c tbl =
    case tbl of
	[]      => []
      | t::tbl' => if Card.same c (attackCard t)
		   then tbl'
		   else t::(remove c tbl')
					     
(* return a list of all cards on the table that have not been beaten yet *)
fun unbeatenCards tbl =
	map attackCard (List.filter (not o beaten) tbl)
	
(* return a list of all cards on the table *)
fun allCards tbl =
    let fun loop (tbl, acc) =
	    case tbl of
		[]                    => acc
	      | (Attack ca)::tbl'     => loop (tbl', ca::acc)
	      | (Beat (ca, cd))::tbl' => loop (tbl', cd::ca::acc)
    in
	loop (tbl, [])
    end

local
    (* check if the number of attack cards on the table is below the attack limit *)
    fun ifBelowLimit (pa, ca, pd, tbl, lim) =
	if length tbl < lim
	then (pa, ca, pd, tbl)
	else raise ExceedsLimit (lim, pa, tbl)
	    		   
    (* check if player has card and remove it if they do *)
    fun ifHasCard (pa, ca, pd, tbl) =
	case Player.find ca pa of
	    NONE   => raise NoCard (ca, pa, tbl)
	  | SOME _ => (Player.discard ca pa, ca, pd, tbl)

    (* check if defending player has enough cards to accept additional attacks *)
    fun ifHasExcess (pa, ca, pd, tbl) =
	if Player.handSize pd - (length o unbeatenCards) tbl > 0
	then (pa, ca, pd, tbl)
	else raise TooFewCards (pd, pa, tbl)

    (* add card to table if table is empty or if a matching rank is on the table *)
    fun ifLegalRank (pa, ca, pd, tbl) =
	if null tbl orelse Card.hasRank ca (allCards tbl)
	then (pa, addCard ca tbl)
	else raise BadRank (pa, tbl)

    (* process legal attacks and appropriately handle illegal attacks *)
    fun ifLegalAttack (pa, ca, pd, tbl, lim) =
	(ifLegalRank o ifHasExcess o ifHasCard o ifBelowLimit) (pa, ca, pd, tbl, lim)
	handle exn => handler exn 
in
(* primary attack function *)
fun attack pa ca pd tbl lim = ifLegalAttack (pa, ca, pd, tbl, lim)
end

local
    (* check if player has card and remove it if they do *)
    fun ifHasCard (pd, cd, ca, tbl, trump) =
	case Player.find cd pd of
	    NONE   => raise NoCard (cd, pd, tbl)
	  | SOME _ => (Player.discard cd pd, cd, ca, tbl, trump)

    (* check if corresponding attack card exists and is unbeaten *)
    fun ifAttackExists (pd, cd, ca, tbl, trump) =
	case Card.find ca (unbeatenCards tbl) of
	    NONE   => raise NoAttack (ca, pd, tbl)
	  | SOME _ => (pd, cd, ca, tbl, trump)

    (* check if card 'cd' can beat card 'ca' *)
    (* add the Beat pair to the table if it does *)
    fun ifBeats (pd, cd, ca, tbl, trump) =
	if   Card.beats cd ca trump
	then (pd, addPair ca cd (remove ca tbl))
	else raise CannotBeat (ca, cd, pd, tbl)

    (* process legal attacks and appropriately handle illegal attacks *)
    fun ifLegalDefense (pd, cd, ca, tbl, trump) =
	(ifBeats o ifAttackExists o ifHasCard) (pd, cd, ca, tbl, trump)
	handle exn => handler exn	    
in
(* primary defense function *)
fun defend pd cd ca tbl trump = ifLegalDefense (pd, cd, ca, tbl, trump)
end

(* (* player picks up all cards on the table *) *)
fun pickup p tbl =
    (Player.draws (allCards tbl) p, Table [])
	
local
    fun toStringHelper f tbl =
	let fun toStringPair f t =
		case t of
		    Attack ca    => f ca
		  | Beat (ca,cd) => "{" ^ (f ca) ^ "-" ^ (f cd) ^ "}"
	in
	    String.concatWith " " (map (toStringPair f) tbl)
	end
in
val toString = toStringHelper Card.toString
val toLongString = toStringHelper Card.toLongString
end

end
	
(* TESTING *)

(* val p = Player.Player ("A", [Card.Card' (2,4)]); *)

(* val t = Table.Table []; *)

(* val p' = p; *)

(* val c = Card.Card' (2,4); *)
(* Table.attack p (Card.Card' (3,3)) p' t; *)
