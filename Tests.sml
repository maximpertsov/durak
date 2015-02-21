(* Import Modules *)
use "Card.sml";

signature TESTS =
sig
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
	      
local
    (* Card comparison helper functions *)
    fun sameCardOpt (c1opt, c2opt) =
	case (c1opt, c2opt) of
	    (SOME c1, SOME c2) => Card.same c1 c2
	  | (NONE, NONE)       => true
	  | _                  => false
    fun sameCardList (cs1, cs2) =
	ListPair.allEq (fn (c1,c2) => Card.same c1 c2) (cs1, cs2)
    (* variables *)
    val Card = Card.Card
    val (Hearts, Clubs, Diamonds, Spades) =
	(Card.Hearts, Card.Clubs, Card.Diamonds, Card.Spades)
    val JH =   Card (11, Hearts)
    val SixH = Card (6, Hearts)
    val SixS = Card (6, Spades)
    (* [JH, SixH, SixS] *)
    val AllCards =
    	let val twoTo14 = List.tabulate (13, fn i => i + 2)
    	    fun cardSuit s = map (fn r => Card (r, s)) twoTo14
    	in
    	    List.concat (map cardSuit [Hearts, Clubs, Diamonds, Spades])
    	end
    (* tests *)
    val tests = 
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
	,runTests "shuffledDeck" sameCardList
		  (fn cs => foldl (fn (rc, cs') => Card.remove rc cs') cs AllCards)
		  [Card.shuffledDeck()]
		  [[]]
	]
in
val testResults = checkAllTests tests
end

end
