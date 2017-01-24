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
 | P(Term r)
 | ba0(Term r)
 | ba1(Term r)
 | plus(Term r,  Term l)
 | mult(Term r, Term l)
 | neg(Term r)
 ;
 
alias step = tuple[str l, Term r];
alias path = list[step];

tuple[str label, Term result] step(N(z())) = <"1", z()>;
//tuple[str label, Term result] step(N(neg(Term x))) = <"2", neg(N(x))>;

//tuple[str label, Term result] step(neg(z())) = <"3", z()>;
//tuple[str label, Term result] step(neg(neg(Term x))) = <"4", x>;

tuple[str label, Term result] step(S(S(z()))) = <"5", N(S(z()))>;
tuple[str label, Term result] step(S(S(N(Term x)))) = <"6", N(S(x))>;
tuple[str label, Term result] step(S(neg(Term x))) = <"7", neg(P(x))>;
//tuple[str label, Term result] step(S(plus(Term x, Term y))) = <"+1", plus(S(x), y)>;

//tuple[str label, Term result] step(P(z())) = <"8", neg(S(z()))>;
//tuple[str label, Term result] step(P(N(Term x))) = <"9", S(N(P(x)))>;
//tuple[str label, Term result] step(P(S(Term x))) = <"10", x>;
//tuple[str label, Term result] step(P(neg(Term x))) = <"11", neg(S(x))>;

tuple[str label, Term result] step(plus(Term x, z())) = <"12", x>;
tuple[str label, Term result] step(plus(Term x, S(Term y))) = <"13", S(plus(x, y))>;
tuple[str label, Term result] step(plus(Term x, N(Term y))) = <"14", plus(plus(x, y), y)>;
//tuple[str label, Term result] step(plus(Term x, neg(Term y))) = <"15", neg(plus(neg(x), y))>;

//tuple[str label, Term result] step(plus(Term x, z())) = <"*1", x>;
//tuple[str label, Term result] step(plus(z(), Term x)) = <"*2", x>;
//tuple[str label, Term result] step(plus(S(Term x), Term y)) = <"*3", S(plus(x, y))>;
//tuple[str label, Term result] step(plus(Term x, S(Term y))) = <"*4", S(plus(x, y))>;
//tuple[str label, Term result] step(plus(N(Term x), N(Term y))) = <"*5", N(plus(x, y))>;

tuple[str label, Term result] step(mult(Term x, z())) = <"16", z()>;
tuple[str label, Term result] step(mult(Term x, S(Term y))) = <"17", plus(mult(x, y), x)>;
tuple[str label, Term result] step(mult(Term x, N(Term y))) = <"18", mult(N(x), y)>;
//tuple[str label, Term result] step(mult(Term x, neg(Term y))) = <"19", mult(neg(x), y)>;
//
//tuple[str label, Term result] step(S(P(Term x))) = <"+1", x>;
//tuple[str label, Term result] step(plus(Term x, P(Term y))) = <"+2", plus(P(x), y)>;

tuple[str label, Term result] step(plus(Term x, plus(Term w, Term v)))
	 = <"+c1", plus(plus(x, w), v)>;
//tuple[str label, Term result] step(mult(Term x, plus(Term y, Term z)))
//	 = <"+c2", plus(mult(x, z), mult(x, y))>;
//tuple[str label, Term result] step(plus(z(), Term x)) = <"+c3", x>;
//tuple[str label, Term result] step(mult(Term x, plus(S(Term w), Term v)))
//	 = <"+c2", plus(mult(x, plus(w, v)), x)>;

tuple[str label, Term result] step(plus(Term x, Zero())) = <"test1", S(v())>;
tuple[str label, Term result] step(plus(Zero(), Term x)) = <"test2", S(w())>;

default tuple[str label, Term result] step(Term x) = <"NA", x>;


void apply_rule2(Term t, bool l) {
	int c = 0;
	top-down-break visit(t) {
		case Term r: {
			println(r);
			//if(r := rules) {
			//	// get new result and label
			//	// pop rule used from the rules for this subterm
			//	println("
			//}
		}
		case list[Term] l: {
			println("hello");
		}
	}
}

list[Term] testingu3(Term t) {
	int nsubterms = size([r | /Term r := t]);
	list[Term] successors = [];
	// make list of size n with rules
	bool stop = false;
	while(rule_list == new_rule_list) {
		println("hello");
	}
	visit(t) {
		case Term r: println("hello");
	}
}

Term replace(Term t, int i, Term l) {
	t[i] = l;
	return t;
}

list[step] rewrite_outer(Term t) {
	list[tuple[str label, Term result]] output = [];
	//if(plus(Zero(), Zero()) := t) output += <"1", Zero()>;
	if(plus(S(Term r), Term l) := t) output += <"2", S(plus(r, l))>; 
	//if(plus(Term r, S(Term l)) := t) output += <"3", S(plus(r, l))>; 
	//if(S(Zero()) := t) output += <"4", One()>;
	//if(Zero() := t) output += <"+1", One()>;
	return output;
}

list[step] rewrite_full(Term t) {
	return rewrite_outer(t) + [<s.l, replace(t, i, s.r)> | int i <- [0..arity(t)], s <- rewrite_full(t[i])];
}

list[path] find_rewritings(Term t) {
	
}
//////////////////////////////
// Performing the rewriting //
//////////////////////////////

// Rewrite the (n+1)-th subterm in t and return the 
// new term along with the label of the rule that was used. If there are no 
// applicable rules the term itself along with label "N/A" is returned.  
tuple[str label, Term result] rewrite2(Term t, int n) {
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
		// Check the lengths of the paths.
		
		// comprehension gebruiken voor odnerstaande if cases 
		
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
