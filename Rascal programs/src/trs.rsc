module trs

import IO;
import List;
import Set;
import Node;

import to_latex;

//////////////////////
// TRS Specifiction //
//////////////////////

data Term
 = Zero()
 | One()
 | u()
 | v()
 | w()
 | p(Term r)
 | S(Term r)
 | Q(Term r)
 | P(Term r)
 | N(Term r)
 | ba0(Term r)
 | ba1(Term r)
 | plus(Term r, Term l)
 | mult(Term r, Term l)
 | neg(Term r)
 ;
 
// Aliasses for oft-used datatypes that indicates what they represent.
// Note that in some places in the code, list[step] is used instead of 
// path because in those instances the steps do not represent a 
// straight line path, but rather different options for rewriting a term.
alias step = tuple[str l, Term r];
alias path = list[step];

/////////////////////////////
// Finding rewriting paths //
/////////////////////////////

// Wrapper for the assignment of l to the i-th subtree of t.
// This way that operation returns the new term and does not modify t. 
Term replace(Term t, int i, Term l) {
	t[i] = l;
	return t;
}

// Returns a list of rewriting steps applicable to the outer function call.
list[step] rewrite_outer(Term t) {
	list[tuple[str label, Term result]] output = [];
	if(N(Zero()) := t) output += <"\\text{n1}", Zero()>;
	if(S(S(Zero())) := t) output += <"\\text{n2}", N(S(Zero()))>;
	if(S(S(N(Term x))) := t) output += <"\\text{n3}", N(S(x))>;
	if(plus(Term x, Zero()) := t) output += <"\\text{n4}", x>;
	if(plus(Term x, S(Term y)) := t) output += <"\\text{n5}", S(plus(x, y))>;
	if(plus(Term x, N(Term y)) := t) output += <"\\text{n6}", plus(plus(x, y), y)>;
	if(mult(Term x, Zero()) := t) output += <"\\text{n7}", Zero()>;
	if(mult(Term x, S(Term y)) := t) output += <"\\text{n8}", plus(mult(x, y), x)>;
	if(mult(Term x, N(Term y)) := t) output += <"\\text{n9}", N(mult(x, y))>;
	if(neg(Zero()) := t) output += <"\\text{n10}", Zero()>;
	if(neg(neg(Term x)) := t) output += <"\\text{n11}", x>;
	if(N(neg(Term x)) := t) output += <"\\text{n12}", neg(N(x))>;
	if(S(neg(Term x)) := t) output += <"\\text{n13}", neg(P(x))>;
	if(P(Zero()) := t) output += <"\\text{n14}", neg(S(Zero()))>;
	if(P(S(Term x)) := t) output += <"\\text{n15}", x>;
	if(P(N(Term x)) := t) output += <"\\text{n16}", S(N(P(x)))>;
	if(P(neg(Term x)) := t) output += <"\\text{n17}", neg(S(x))>;
	if(plus(Term x, neg(Term y)) := t) output += <"\\text{n18}", neg(plus(neg(x), y))>;
	if(mult(Term x, neg(Term y)) := t) output += <"\\text{n19}", neg(mult(x, y))>;
	return output;
}

// Returns a list of rewriting steps applicable at all levels.
list[step] rewrite_full(Term t) {
	return rewrite_outer(t) + [<s.l, replace(t, i, s.r)> | int i <- [0..arity(t)], step s <- rewrite_full(t[i])];
}

// Returns a list of each possible rewriting path for t.
list[path] find_rewritings(Term t) {
	list[path] output = [s + p | step s <- rewrite_full(t), path p <- find_rewritings(s.r)];
	return output == [] ? [[]] : output;
}

/////////////////////////////////////////////////////
// Creating a latex confluence counterexample tree //
/////////////////////////////////////////////////////

// Check if t can be rewritten to two different final forms, and print latex code
// to the console of the tree that results from this. 
void print_latex(Term t) {
	list[path] paths = find_rewritings(t);
	set[Term] end_results = {p[-1].r | p <- paths};
	if(size(end_results) < 2) println("No counterexample was found.");
	else if(size(end_results) > 2) println("More than one counterexample was found!");
	else {
		// Find the shortest path for each of the end results.
		ce = for(Term er <- end_results) {
			append sort([p | p <- paths, p[-1].r == er], bool(path a, path b){ return size(a) < size(b); })[0];
		};
		
		// Start printing the latex code.
		println("\\begin{displaymath}");
		println("	\\xymatrix{");

		// Print the top node.
		print("		& " + tolatex(t) + "\\ar@{-\>}[dl]_{[<ce[0][0][0]>]}\\ar@{-\>}[dr]^{[<ce[1][0][0]>]}");
		
		// Print the rest of the nodes.
		for(int n <- [0..max([size(ce[0]), size(ce[1])])]) {
			print("\\\\\n		");
			if(n < size(ce[0])) {
				print(tolatex(ce[0][n][1]));
				if(n < (size(ce[0]) - 1)) print("\\ar@{-\>}[d]_{[<ce[0][n+1][0]>]}");
			}
			print(" && ");
			if(n < size(ce[1])) {
				print(tolatex(ce[1][n][1]));
				if(n < (size(ce[1]) - 1)) print("\\ar@{-\>}[d]^{[<ce[1][n+1][0]>]}");
			}
		}
		println("\n	}");
		println("\\end{displaymath}");
	}
}
