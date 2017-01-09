module trs

import IO;
import List;
import Set;

//////////////////////
// TRS Specifiction //
//////////////////////

data Term
 = z()
 | u()
 | v()
 | w()
 | S(Term r)
 | P(Term r)
 | N(Term r)
 | plus(Term r,  Term l)
 | mult(Term r, Term l)
 | neg(Term r)
 ;

tuple[str label, Term result] step(N(z())) = <"1", z()>;
//tuple[str label, Term result] step(N(neg(Term x))) = <"2", neg(N(x))>;

//tuple[str label, Term result] step(neg(z())) = <"3", z()>;
//tuple[str label, Term result] step(neg(neg(Term x))) = <"4", x>;

tuple[str label, Term result] step(S(S(z()))) = <"5", N(S(z()))>;
tuple[str label, Term result] step(S(S(N(Term x)))) = <"6", N(S(x))>;
tuple[str label, Term result] step(S(neg(Term x))) = <"7", neg(P(x))>;
tuple[str label, Term result] step(S(plus(Term x, Term y))) = <"+1", plus(S(x), y)>;

//tuple[str label, Term result] step(P(z())) = <"8", neg(S(z()))>;
//tuple[str label, Term result] step(P(N(Term x))) = <"9", S(N(P(x)))>;
//tuple[str label, Term result] step(P(S(Term x))) = <"10", x>;
//tuple[str label, Term result] step(P(neg(Term x))) = <"11", neg(S(x))>;

tuple[str label, Term result] step(plus(Term x, z())) = <"12", x>;
tuple[str label, Term result] step(plus(Term x, S(Term y))) = <"13", plus(S(x), y)>;
tuple[str label, Term result] step(plus(Term x, N(Term y))) = <"14", plus(plus(x, y), y)>;
//tuple[str label, Term result] step(plus(Term x, neg(Term y))) = <"15", neg(plus(neg(x), y))>;

//tuple[str label, Term result] step(plus(Term x, z())) = <"*1", x>;
//tuple[str label, Term result] step(plus(z(), Term x)) = <"*2", x>;
//tuple[str label, Term result] step(plus(S(Term x), Term y)) = <"*3", S(plus(x, y))>;
//tuple[str label, Term result] step(plus(Term x, S(Term y))) = <"*4", S(plus(x, y))>;
//tuple[str label, Term result] step(plus(N(Term x), N(Term y))) = <"*5", N(plus(x, y))>;

tuple[str label, Term result] step(mult(Term x, z())) = <"16", z()>;
tuple[str label, Term result] step(mult(Term x, S(Term y))) = <"17", plus(x, mult(x, y))>;
tuple[str label, Term result] step(mult(Term x, N(Term y))) = <"18", mult(N(x), y)>;
//tuple[str label, Term result] step(mult(Term x, neg(Term y))) = <"19", mult(neg(x), y)>;
//
//tuple[str label, Term result] step(S(P(Term x))) = <"+1", x>;
//tuple[str label, Term result] step(plus(Term x, P(Term y))) = <"+2", plus(P(x), y)>;

tuple[str label, Term result] step(plus(Term x, plus(Term w, Term v))) = <"+c1", plus(plus(x, w), v)>;
tuple[str label, Term result] step(mult(x, plus(y, z))) = <"+c2", plus(mult(x, y), mult(x, z))>;
//tuple[str label, Term result] step(mult(Term x, plus(S(Term w), Term v)))
//	 = <"+c2", plus(mult(x, plus(w, v)), x)>;

default tuple[str label, Term result] step(Term x) = <"NA", x>;

////////////////////////////////////
// Convert a Term to latex syntax //
////////////////////////////////////

// 'tolatex' takes a Term and recursively creates a string in latex syntax. 
// The function is overloaded many times to make sure not too many brackets are used.

str tolatex(z()) = "0";
str tolatex(u()) = "u";
str tolatex(v()) = "v";
str tolatex(w()) = "w";
str tolatex(S(Term t)) = "S(" + tolatex(t) + ")";
str tolatex(P(Term t)) = "P(" + tolatex(t) + ")";
str tolatex(N(Term t)) = "N(" + tolatex(t) + ")";

str tolatex(neg(neg(Term t))) = "-(" + tolatex(neg(t)) + ")";
str tolatex(neg(plus(Term t, Term r))) = "-(" + tolatex(plus(t, r)) + ")";
str tolatex(neg(mult(Term t, Term r))) = "-(" + tolatex(mult(t, r)) + ")";
default str tolatex(neg(Term t)) = "-" + tolatex(t);
	
str tolatex(plus(plus(Term g, Term h), Term r)) = tolatex_p("(" + tolatex(plus(g, h)) + ")", r);
str tolatex(plus(mult(Term g, Term h), Term r)) = tolatex_p("(" + tolatex(mult(g, h)) + ")", r);
str tolatex(plus(neg(Term g), Term r)) = tolatex_p("-(" + tolatex(g) + ")", r);
default str tolatex(plus(Term t, Term r)) = tolatex_p(tolatex(t), r);
	
str tolatex_p(str r, plus(Term g, Term h)) = r + "+" + "(" + tolatex(plus(g, h)) + ")";
str tolatex_p(str r, mult(Term g, Term h)) = r + "+" + "(" + tolatex(mult(g, h)) + ")";
default str tolatex_p(str r, Term t) = r + "+" + tolatex(t);

str tolatex(mult(plus(Term g, Term h), Term r)) = 
	tolatex_m("(" + tolatex(plus(g, h)) + ")", r);
str tolatex(mult(mult(Term g, Term h), Term r)) = 
	tolatex_m("(" + tolatex(mult(g, h)) + ")", r);
str tolatex(mult(neg(Term g), Term r)) = 
	tolatex_m("-(" + tolatex(g) + ")", r);
default str tolatex(mult(Term t, Term r)) = 
	tolatex_m(tolatex(t), r);
	
str tolatex_m(str r, plus(Term g, Term h)) = r + "\\cdot " + 
	"(" + tolatex(plus(g, h)) + ")";
str tolatex_m(str r, mult(Term g, Term h)) = r + "\\cdot " + 
	"(" + tolatex(mult(g, h)) + ")";
default str tolatex_m(str r, Term t) = r + "\\cdot " + tolatex(t);

//////////////////////////////
// Performing the rewriting //
//////////////////////////////

// Rewrite the (n+1)-th subterm in t and return the 
// new term along with the label of the rule that was used. If there are no 
// applicable rules the term itself along with label "N/A" is returned.  
tuple[str label, Term result] rewrite(Term t, int n) {
	int c = 0;
	str lbl = "";
	Term new_t = visit(t) {
		case Term r: { 
			if(c == n) {
				c = c + 1;
				tuple[str label, Term result] next_t = step(r);
				lbl = next_t.label;
				insert next_t.result;
			}
			c = c + 1;
		}
		default: 
			c = c + 1;
	};
	return <lbl, new_t>;
}

// Return a list of all possible ways of rewriting t. Each element in the list represents
// a rewriting sequence, as a list of labels of rewriting steps each followed by the term
// that resulted from that application (so the last element in the list is the final form).
// It does so using recursion after having found a rewriting step for a subterm. When no
// steps are possible, a list consisting of an empty list is returned. 
list[list[value]] find_reducts(Term t) {
	list[list[value]] out = [];
	int k = size([r | /Term r := t]);
	for(int n <- [0..k]) {
		tuple[str label, Term result] strg = rewrite(t, n);
		if(strg.label != "NA") {
			out = out + [[*[strg[0], strg[1], *path] | path <- find_reducts(strg.result)]];
		}
	}
	return (out == []) ? [[]] : out;
}

// Check if t can be rewritten to two different final forms, and print latex code
// to the console of the tree that results from this. 
void print_latex(Term t) {
	list[list[value]] tr = find_reducts(t);
	list[value] forms = toList({path[-1] | path <- tr});
	if(size(forms) < 2) println("No counterexample was found.");
	else if(size(forms) > 2) println("More than one counterexample was found!");
	else {
		int len1 = 999999999999999999999;
		int len2 = 999999999999999999999;
		list[value] path1 = [];
		list[value] path2 = [];
		println("\\begin{displaymath}");
		println("	\\xymatrix{");
		// Check which of the paths is shortest.
		for(list[value] path <- tr) {
			if(path[-1] == forms[0]) {
				if(size(path) < len1) {
					path1 = path;
					len1 = size(path);
				}
			}
			else if(path[-1] == forms[1]) {
				if(size(path) < len2) { 
					path2 = path;
					len2 = size(path);
				}
			}
		}
		// Print the top node.
		print("		& " + tolatex(t) + "\\ar@{-\>}[dl]_{[<path1[0]>]}\\ar@{-\>}[dr]^{[<path2[0]>]}");
		// Print the rest of the nodes.
		for(int n <- [0..(max([len1, len2]) / 2)]) {
			print("\\\\\n		");
			int k = n*2;
			if(k <= (len1 - 1) && Term r := path1[k + 1]) {
				print(tolatex(r));
				if((n+1)*2 < len1) print("\\ar@{-\>}[d]_{[<path1[k+2]>]}");
			}
			print(" && ");
			if(k < (len2 - 1) && Term r := path2[k + 1]) {
				print(tolatex(r));
				if((n+1)*2 < len2) print("\\ar@{-\>}[d]^{[<path2[k+2]>]}");
			}
		}
		println("\n	}");
		println("\\end{displaymath}");
	}
}
