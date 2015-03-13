(* Import Modules *)
use "Table.sml";

signature TESTS =
sig
    (* Helper functions to test equality for custom data types *)
    val sameCardOpt     : Card.card option * Card.card option -> bool
    val sameCardList    : Card.card list * Card.card list -> bool
    val samePlayerTable : (Player.player * Table.table) *
			  (Player.player * Table.table) -> bool
    (* Testing variables *)
    val Alice : Player.player
    val SixS : Card.card
    val JH : Card.card
    val SixH : Card.card
    val KC : Card.card
    val TenH : Card.card
    val EightS : Card.card
    val KingS : Card.card
		    
    (* Print results of all tests applied *)
    val testResults : unit
end
    
structure Tests :> TESTS =
struct

(* Standard testing function. User must provide the following information:
   - Name for this set of tests (e.g. the name of the function being tested).
     This will be used to indicate which tests, if any, fail
   - Comparison function that should be used to compare inputs against expected
     results. This is primarily for cases where results are of a custom datatype
     that does not have an Equality type. Usually 'op=' (no quotes) will work
   - Function being tested
   - List of inputs to apply function to
   - List of expected outputs to compare against inputs after applying function *)
fun runTests name compf f inps exps =
    (name, ListPair.allEq compf ((map f inps), exps))

(* Check list of test outputs *)
fun checkAllTests ts =
    case List.filter (fn (n,t) => not t) ts of
	[]  => print "\n*** PASSED ALL TESTS! ***\n\n"
      | ts' => (print "\nxxx FAILED SOME TESTS: xxx\n";
		app (fn (n, _) => print ("* " ^ n ^ "\n")) ts';
		print "\n")

(* Card comparison helper functions *)
fun sameCardOpt (c1opt, c2opt) =
    case (c1opt, c2opt) of
	(SOME c1, SOME c2) => Card.same c1 c2
      | (NONE, NONE)       => true
      | _                  => false

fun sameCardList (cs1, cs2) =
    ListPair.allEq (fn (c1,c2) => Card.same c1 c2) (cs1, cs2)

(* Table comparison helper functions *)
fun sameTable (tbl1, tbl2) = Table.same tbl1 tbl2

fun samePlayer (p1, p2) = Player.same p1 p2

fun samePlayerTable ((p1, tbl1), (p2, tbl2)) =
    (samePlayer (p1, p2)) andalso (sameTable (tbl1, tbl2))

(* Card test variables *)
val Card = Card.Card
val (Hearts, Clubs, Diamonds, Spades) =
    (Card.Hearts, Card.Clubs, Card.Diamonds, Card.Spades)
val JH =   Card (11, Hearts)
val SixH = Card (6, Hearts)
val SixS = Card (6, Spades)
val AllCards =
    let val twoTo14 = List.tabulate (13, fn i => i + 2)
    	fun cardSuit s = map (fn r => Card (r, s)) twoTo14
    in
    	List.concat (map cardSuit [Hearts, Clubs, Diamonds, Spades])
    end
(* Card tests *)
val cardTests =
    [runTests "sameSuit" op=
	      (fn (c1, c2) => Card.sameSuit c1 c2)
	      [(JH, SixH), (SixH, SixS)]
	      [true, false]
    ,runTests "sameRank" op=
	      (fn (c1, c2) => Card.sameRank c1 c2)
	      [(JH, SixH), (SixH, SixS)]
	      [false, true]
    ,runTests "same" op=
	      (fn (c1, c2) => Card.same c1 c2)
	      [(JH, JH), (SixH, JH), (SixS, SixS), (JH, SixS)]
	      [true, false, true, false]
    ,runTests "hasRank" op=
	      (fn (c, cs) => Card.hasRank c cs)
	      [(JH, [SixH, SixS]), (SixH, [JH, SixS]), (JH, [])]
	      [false, true, false]
    ,runTests "find" sameCardOpt
	      (fn (c, cs) => Card.find c cs)
	      [(SixH, [SixH, SixS]), (SixH, [JH, SixS]), (JH, [SixH, JH])]
	      [SOME SixH, NONE, SOME JH]
    ,runTests "remove" sameCardList
	      (fn (c, cs) => Card.remove c cs)
	      [ (* standard case *)
		(JH, [SixH, JH, SixS]),
		(* do nothing if specified card is missing from the list *)
		(JH, [SixH, SixS]),
		(* only remove one card if there are duplicates in the list *)
		(JH, [JH, SixS, JH])]
	      [[SixH, SixS], [SixH, SixS], [SixS, JH]]
    ,runTests "suit" op= Card.suit
	      [SixH, JH, SixS]
	      [Card.Hearts, Card.Hearts, Card.Spades]
    ,runTests "value" op= Card.value
	      [SixH, JH, SixS]
	      [6, 11, 6]
    ,runTests "compareRank" op=
	      (fn (c1, c2) => Card.compareRank c1 c2)
	      [(SixH, SixS), (JH, SixS), (SixS, JH)]
	      [EQUAL, GREATER, LESS]
    ,runTests "toString" op= Card.toString
	      [SixH, JH, SixS]
	      ["6H", "JH", "6S"]
    ,runTests "toLongString" op= Card.toLongString
	      [SixH, JH, SixS]
	      ["6 of Hearts", "Jack of Hearts", "6 of Spades"]
    ,runTests "toStrings" op= Card.toStrings
	      [[SixH, JH, SixS]]
	      ["6H JH 6S"]
    ,runTests "toLongStrings" op= Card.toLongStrings
	      [[SixH, JH, SixS]]
	      ["6 of Hearts, Jack of Hearts, 6 of Spades"]
    ,runTests "shuffledDeck" sameCardList
	      (fn cs => foldl (fn (rc, cs') => Card.remove rc cs') cs AllCards)
	      [Card.shuffledDeck()]
	      [[]]
    ]
(* Player variables *)
val P = Player.Player
val p1 = P ("A", [])
val p2 = P ("B", [])
val playerTests =
    [runTests "drawsUntil(Player)" op=
	      (fn (l,cs,p) => (Player.toString o #1) (Player.drawsUntil l cs p))
	      [(3, AllCards, p1), (3, AllCards, p2)]
	      ["A: 4H 3H 2H", "B: 4H 3H 2H"]
    ,runTests "drawsUntil(Cards)" op=
	      (fn (l,cs,p) => (Card.toStrings o #2) (Player.drawsUntil l cs p))
	      [(3, AllCards, p1)]
	      [Card.toStrings (List.drop (AllCards, 3))]
    ,runTests "allDrawsUntil(Players)" op=
	      (fn (l,cs,ps) => ((map Player.toString) o #1)
				    (Player.allDrawUntil l cs ps))
	      [(3, AllCards, [p1, p2])]
	      [["A: 4H 3H 2H", "B: 7H 6H 5H"]]
    ,runTests "allDrawsUntil(Cards)" op=
	      (fn (l,cs,ps) => (Card.toStrings o #2)
				    (Player.allDrawUntil l cs ps))
	      [(3, AllCards, [p1, p2])]
	      [Card.toStrings (List.drop (AllCards, 6))]
    ]
(* Table variables *)
val T = Table.Table
val [QS, KD, SevenH, KC, TenH, EightS, KingS] =
    map Card [(12, Spades), (13, Diamonds), (7, Hearts),
	      (13, Clubs), (10, Hearts), (8, Spades), (13, Spades)]
val Alice = P ("Aggressive Alice", [SixS, JH, SixH])
val Bob   = P ("By-The-Book Bob",  [QS, KD, SevenH])
val tbl1  = T [KC, TenH, EightS, KingS]
val tbl2  = T [KC, TenH]
val tableTests =
    [runTests "sameTable" op=
	      (fn (t1,t2) => Table.same t1 t2)
	      [(tbl1, T []), (tbl1, tbl1), (T [], T [])]
	      [false, true, true]
    ,runTests "name" op= Player.name
	      [Alice, Bob]
	      ["Aggressive Alice", "By-The-Book Bob"]
    ,runTests "hand" sameCardList Player.hand
	      [Alice, Bob]
	      [[SixS, JH, SixH], [QS, KD, SevenH]]
    ,runTests "draw" samePlayer
	      (fn (c,p) => Player.draw c p)
	      [(JH, Alice), (JH, Bob)]
	      [P ("Aggressive Alice", [JH, SixS, JH, SixH]),
	       P ("By-The-Book Bob",  [JH, QS, KD, SevenH])]
    ,runTests "discard" samePlayer
	      (fn (c,p) => Player.discard c p)
	      [(JH, Alice), (QS, Bob), (QS, Alice)]
	      [P ("Aggressive Alice", [SixS, SixH]),
	       P ("By-The-Book Bob",  [KD, SevenH]),
	       P ("Aggressive Alice", [SixS, JH, SixH])]
    ,runTests "attack" samePlayerTable
	      (fn (pa,ca,pd,t) => Table.attack pa ca pd t 6)
	      [(Alice, SixS, Bob, T []),
	       (Bob, KD, Alice, T [KC, TenH])]
	      [(P ("Aggressive Alice", [JH, SixH]), T [SixS]),
	       (P ("By-The-Book Bob", [QS, SevenH]), T [KD, KC, TenH])]
    ,runTests "defend" samePlayerTable
	      (fn (pd,cd,ca,t,s) => Table.defend pd cd ca t s)
	      [(Alice, JH, TenH, T [KC, TenH, EightS, KingS], Spades)]
	      [(Player.discard JH Alice,
		Table.addPair TenH JH (T [KC, EightS, KingS]))]
    ,runTests "pickup" samePlayerTable
	      (fn (p,t) => Table.pickup p t)
	      [(Alice, T [KC, TenH, EightS, KingS])]
	      [(Player.draws (rev [KC, TenH, EightS, KingS]) Alice, T [])]
    ]

val testResults = checkAllTests (cardTests @ playerTests @ tableTests)

end
