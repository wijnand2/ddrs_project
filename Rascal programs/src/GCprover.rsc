module GCprover

import IO;
import List;
import Set;

import to_latex;

//////////////////////
// ADT Specifiction //
//////////////////////

// Datatype for the terms in the signature. There should be as many 
// variables (here, u through w) as the maximum value for the function arities.

public data Term
 = Zero()
 | One()
 | Two()
 | Three()
 | Four()
 | Five()
 | Six()
 | Seven()
 | Eight()
 | Nine()
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
 | N(Term r)
 ;
  
// Returns true if the given Term denotes a variable.
bool is_var(u()) = true;
bool is_var(v()) = true;
bool is_var(w()) = true;
bool is_var(p(Term t)) = is_var(t);
default bool is_var(Term t) = false;

//// Returns true if the given term is in normal form.
//bool NF(Zero()) = true;
//default bool NF(Term t) = NFp(t) || NFm(t);
//
//bool NFp(One()) = true;
//bool NFp(ba0(Term t)) = NFp(t);
//bool NFp(ba1(Term t)) = NFp(t);
//default bool NFp(Term t) = false;
//
//bool NFm(neg(Term t)) = NFp(t);
//default bool NFm(Term t) = false;
//
//// Variables are considered in NFp for the induction cases.
//// This is because in those cases the first call is on a 
//// term that is assumed to be in NF.
//bool NFp(u()) = true;
//bool NFp(v()) = true;
//bool NFp(w()) = true;
//bool NFp(p(Term t)) = true;

// Returns true if the given term is in normal form.
bool NF(Zero()) = true;
default bool NF(Term t) = NFs(t) || NFn(t) || NFm(t);

bool NFs(Term t) {
	switch(t) {
		case S(Zero()): return true;
		case S(Term t): return NFn(t);
		default: return false;
	}
}

bool NFn(N(Term t)) = NFn(t) || NFs(t);
default bool NFn(Term t) = false;

bool NFm(neg(Term t)) = NFs(t) || NFn(t);
default bool NFm(Term t) = false;

// Variables are considered in NFp for the induction cases.
// This is because in those cases the first call is on a 
// term that is assumed to be in NF.
bool NFs(u()) = true;
bool NFs(v()) = true;
bool NFs(w()) = true;
bool NFs(p(Term t)) = true;

bool NFn(u()) = true;
bool NFn(v()) = true;
bool NFn(w()) = true;
bool NFn(p(Term t)) = true;

//////////////////////
// TRS Specifiction //
//////////////////////

//tuple[str label, Term result] step(ba0(Zero())) = <"b1.0", Zero()>;
//tuple[str label, Term result] step(ba1(Zero())) = <"b1.1", One()>;
//tuple[str label, Term result] step(S(Zero())) = <"b2", One()>;
//tuple[str label, Term result] step(S(One())) = <"b3", ba0(One())>;
//tuple[str label, Term result] step(S(ba0(Term t))) = <"b4", ba1(t)>;
//tuple[str label, Term result] step(S(ba1(Term t))) = <"b5", ba0(S(t))>;
//tuple[str label, Term result] step(plus(Term t, Zero())) = <"b6", t>;
//tuple[str label, Term result] step(plus(Zero(), Term t)) = <"b7", t>;
//tuple[str label, Term result] step(plus(Term t, One())) = <"b8", S(t)>;
//tuple[str label, Term result] step(plus(One(), Term t)) = <"b9", S(t)>;
//tuple[str label, Term result] step(plus(ba0(Term t), ba0(Term r))) = <"b10.0.0", ba0(plus(t, r))>;
//tuple[str label, Term result] step(plus(ba0(Term t), ba1(Term r))) = <"b10.0.1", S(ba0(plus(t, r)))>;
//tuple[str label, Term result] step(plus(ba1(Term t), ba0(Term r))) = <"b10.1.0", ba1(plus(t, r))>;
//tuple[str label, Term result] step(plus(ba1(Term t), ba1(Term r))) = <"b10.1.1", S(ba1(plus(t, r)))>;
//tuple[str label, Term result] step(mult(Term t, Zero())) = <"b11", Zero()>;
//tuple[str label, Term result] step(mult(Term t, One())) = <"b12", t>;
//tuple[str label, Term result] step(mult(Term t, ba0(Term r))) = <"b13.0", plus(ba0(mult(t, r)), mult(t, Zero()))>;
//tuple[str label, Term result] step(mult(Term t, ba1(Term r))) = <"b13.1", plus(ba0(mult(t, r)), mult(t, One()))>;
//tuple[str label, Term result] step(neg(Zero())) = <"b16", Zero()>;
//tuple[str label, Term result] step(neg(neg(Term t))) = <"b17", t>;
//tuple[str label, Term result] step(P(Zero())) = <"b18", neg(One())>;
//tuple[str label, Term result] step(P(One())) = <"b19", Zero()>;
//tuple[str label, Term result] step(P(ba0(Term t))) = <"b20", ba1(P(t))>;
//tuple[str label, Term result] step(P(ba1(Term t))) = <"b21", ba0(t)>;
//tuple[str label, Term result] step(P(neg(Term t))) = <"b22", neg(S(t))>;
//tuple[str label, Term result] step(ba0(neg(Term t))) = <"b26", neg(ba0(t))>;
//tuple[str label, Term result] step(ba1(neg(Term t))) = <"b27", neg(ba1(P(t)))>;
//tuple[str label, Term result] step(plus(neg(One()), Term t)) = <"b28", P(t)>;
//tuple[str label, Term result] step(plus(neg(ba0(Term t)), ba0(Term r))) = <"b31.0.0", ba0(plus(neg(t), r))>;
//tuple[str label, Term result] step(plus(neg(ba1(Term t)), ba0(Term r))) = <"b31.0.1", P(ba0(plus(neg(t), r)))>;
//tuple[str label, Term result] step(plus(neg(ba0(Term t)), ba1(Term r))) = <"b31.1.0", ba1(plus(neg(t), r))>;
//tuple[str label, Term result] step(plus(neg(ba1(Term t)), ba1(Term r))) = <"b31.1.1", P(ba1(plus(neg(t), r)))>;
//tuple[str label, Term result] step(mult(Term t, neg(Term r))) = <"b33", neg(mult(t, r))>;
//tuple[str label, Term result] step(plus(Term t, neg(Term r))) = <"+1", neg(plus(neg(t), r))>;
//tuple[str label, Term result] step(S(neg(Term t))) = <"+2", neg(P(t))>;
//
//default tuple[str label, Term result] step(Term t) = <"NA", t>;

tuple[str label, Term result] step(N(Zero())) = <"\\text{n1}", Zero()>;
tuple[str label, Term result] step(S(S(Zero()))) = <"\\text{n2}", N(S(Zero()))>;
tuple[str label, Term result] step(S(S(N(Term x)))) = <"\\text{n3}", N(S(x))>;
tuple[str label, Term result] step(plus(Term x, Zero())) = <"\\text{n4}", x>;
tuple[str label, Term result] step(plus(Term x, S(Term y))) = <"\\text{n5}", S(plus(x, y))>;
tuple[str label, Term result] step(plus(Term x, N(Term y))) = <"\\text{n6}", plus(plus(x, y), y)>;
tuple[str label, Term result] step(mult(Term x, Zero())) = <"\\text{n7}", Zero()>;
tuple[str label, Term result] step(mult(Term x, S(Term y))) = <"\\text{n8}", plus(mult(x, y), x)>;
tuple[str label, Term result] step(mult(Term x, N(Term y))) = <"\\text{n9}", N(mult(x, y))>;
tuple[str label, Term result] step(neg(Zero())) = <"\\text{n10}", Zero()>;
tuple[str label, Term result] step(neg(neg(Term x))) = <"\\text{n11}", x>;
tuple[str label, Term result] step(N(neg(Term x))) = <"\\text{n12}", neg(N(x))>;
tuple[str label, Term result] step(S(neg(Term x))) = <"\\text{n13}", neg(P(x))>;
tuple[str label, Term result] step(P(Zero())) = <"\\text{n14}", neg(S(Zero()))>;
tuple[str label, Term result] step(P(S(Term x))) = <"\\text{n15}", x>;
tuple[str label, Term result] step(P(N(Term x))) = <"\\text{n16}", S(N(P(x)))>;
tuple[str label, Term result] step(P(neg(Term x))) = <"\\text{n17}", neg(S(x))>;
tuple[str label, Term result] step(plus(Term x, neg(Term y))) = <"\\text{n18}", neg(plus(neg(x), y))>;
tuple[str label, Term result] step(mult(Term x, neg(Term y))) = <"\\text{n19}", neg(mult(x, y))>;

default tuple[str label, Term result] step(Term x) = <"NA", x>;

/////////////////////////
// GCprover components //
/////////////////////////

// Replace occurances of i in h with j.
Term replace(Term i, Term h, Term j) {
	return visit(h) {
		case i => j
	}
}
 
// Replace v in t with each possible shape of a normal form,
// and return the results as a list of tuples.
list[tuple[Term, Term]] case_distinction(Term t, Term v) {
	//list[Term] shapes = [
	//	Zero(),
	//	One(),
	//	ba0(p(v)),
	//	ba1(p(v)),
	//	neg(One()),
	//	neg(ba0(p(v))),
	//	neg(ba1(p(v)))
	//];
	list[Term] shapes = [
		Zero(),
		S(Zero()),
		S(N(p(v))),
		N(p(v)),
		neg(S(Zero())),
		neg(S(N(p(v)))),
		neg(N(p(v)))
	];  
	return [<replace(v, t, s), s> | s <- shapes];
}

// Returns a possibly empty list of ground-confluence counterexamples for t.
// The execution of this function can be summarized as follows
//   i. If 'first' is false and t is in N, return the empty list, else
//  ii. If t can be rewritten, return the empty list, else
// iii. If vars is not empty, apply case distinction on one of them
//		and call this function on each case, else
//  iv. None of the above applied so add t to the counterexamples and return it.
list[Term] handle_case(Term t, list[Term] vars, bool first) {
	list[Term] counterexamples = [];
	// Check if t is in N. Skipped the first time this function is called.
	if(!first) {
		if(NF(t)) {
			print("then $t \\in N$. \n");	
			return [];
		}
	}
	// It is not in N and so it should have a rewriting step available.
	if(<s, r> := step(t)) {
		// Handle the case where a rule can be applied.
		if(s != "NA") {
			print("then $t \\rightarrow <tolatex(r)>$ by equation $[<s>]$.\n");
			return [];
		}
		// We can not rewrite the term, so we try instantiating variables. 
		else if(vars != []) {
			Term v = vars[-1];
			print("apply case distinction on $<tolatex(v)>$.\n");
			println("\\begin{itemize}");
			for(<Term r, Term l> <- case_distinction(t, v)) {
				print("\\item[] $<tolatex(v)> = <tolatex(l)>$: \\,");
				counterexamples += handle_case(r, vars - v, false);
			}
			println("\\end{itemize}");
		}
		// This case could not be shown to belong to N or be rewritable, and no 
		// variables to substitute remain so the proof has failed. 
		else {
			print("the term $<tolatex(t)>$ could not be resolved.\n");
			counterexamples += t;
		}
	}
	return counterexamples;
}

//////////////////////////
// GCprover outer level //
//////////////////////////

// Checks if the TRS specified above is ground confluent. The induction cases
// and case_distinction cases (as well as the rules and NF's) should be 
// specified before calling this function.
void check_gc() {
	list[Term] counterexamples = [];
	// A list for the induction cases. 
	list[Term] ts = [
		S(u()),
		P(u()),
		neg(u()),
		N(u()),
		plus(u(), v()),
		mult(u(), v())
	];
	//list[Term] ts = [
	//	S(u()),
	//	P(u()),
	//	neg(u()),
	//	ba0(u()),
	//	ba1(u()),
	//	plus(u(), v()),
	//	mult(u(), v())
	//];
	
	// Iterate over the induction cases and handle each case. 
	// Comprehension
	println("\\begin{enumerate}[(1)]");
	for(Term t <- ts) {
		print("\\item $t = <tolatex(t)>$: \\,");
		list[Term] var_ocs = [];
		for(/Term r := t) if(is_var(r) && !(r in var_ocs)) var_ocs += r;
		counterexamples += handle_case(t, var_ocs, true);
	}
	println("\\end{enumerate}");
	
	// Report whether the proof was succesfull.
	if(counterexamples != []) {
		println("The following counterexamples were found:");
		println("\\begin{gather*}");
		for(ce <- counterexamples[..-1]) println("<tolatex(ce)>,\\\\");
		println("<tolatex(counterexamples[-1])>.");
		println("\\end{gather*}");
	}
	else {
		println("This completes the proof.");
	}
}