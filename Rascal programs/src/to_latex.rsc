module to_latex

// 'tolatex' takes a Term and recursively creates a string in latex syntax. 
// The function is overloaded many times to make sure not too many brackets are used.

// Datatype for the terms in the signature. There should be as many 
// variables (here, u through w) as the maximum value for the function arities.

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

str tolatex(Zero()) = "0";
str tolatex(One()) = "1";
str tolatex(p(Term t)) = tolatex(t) + "^{\\prime}";
str tolatex(u()) = "u";
str tolatex(v()) = "v";
str tolatex(w()) = "w";
str tolatex(S(Term t)) = "S(" + tolatex(t) + ")";
str tolatex(P(Term t)) = "P(" + tolatex(t) + ")";
str tolatex(N(Term t)) = "N(" + tolatex(t) + ")";

str tolatex(ba0(plus(Term t, Term r))) = "(" + tolatex(plus(t, r)) + ") \\apbz";
str tolatex(ba0(mult(Term t, Term r))) = "(" + tolatex(mult(t, r)) + ") \\apbz";
str tolatex(ba0(ba0(Term t))) = "(" + tolatex(ba0(t)) + ")" + "\\apbz";
str tolatex(ba0(ba1(Term t))) = "(" + tolatex(ba1(t)) + ") \\apbz";
str tolatex(ba0(neg(Term t))) = "(" + tolatex(neg(t)) + ") \\apbz";
default str tolatex(ba0(Term t)) = tolatex(t) + "\\apbz"; 

str tolatex(ba1(plus(Term t, Term r))) = "(" + tolatex(plus(t, r)) + ") \\apbu";
str tolatex(ba1(mult(Term t, Term r))) = "(" + tolatex(mult(t, r)) + ") \\apbu";
str tolatex(ba1(ba0(Term t))) = "(" + tolatex(ba0(t)) + ")" + "\\apbu";
str tolatex(ba1(ba1(Term t))) = "(" + tolatex(ba1(t)) + ") \\apbu";
str tolatex(ba1(neg(Term t))) = "(" + tolatex(neg(t)) + ") \\apbu";
default str tolatex(ba1(Term t)) = tolatex(t) + "\\apbu"; 

str tolatex(neg(neg(Term t))) = "-(" + tolatex(neg(t)) + ")";
str tolatex(neg(plus(Term t, Term r))) = "-(" + tolatex(plus(t, r)) + ")";
str tolatex(neg(mult(Term t, Term r))) = "-(" + tolatex(mult(t, r)) + ")";
str tolatex(neg(ba0(Term t))) = "-(" + tolatex(ba0(t)) + ")";
str tolatex(neg(ba1(Term t))) = "-(" + tolatex(ba1(t)) + ")";
default str tolatex(neg(Term t)) = "-" + tolatex(t);
	
str tolatex(plus(plus(Term g, Term h), Term r)) = tolatex_p("(" + tolatex(plus(g, h)) + ")", r);
str tolatex(plus(mult(Term g, Term h), Term r)) = tolatex_p("(" + tolatex(mult(g, h)) + ")", r);
str tolatex(plus(neg(Term g), Term r)) = tolatex_p("(" + tolatex(neg(g)) + ")", r);
default str tolatex(plus(Term t, Term r)) = tolatex_p(tolatex(t), r);
	
str tolatex_p(str r, plus(Term g, Term h)) = r + "+" + "(" + tolatex(plus(g, h)) + ")";
str tolatex_p(str r, mult(Term g, Term h)) = r + "+" + "(" + tolatex(mult(g, h)) + ")";
str tolatex_p(str r, ba0(Term g)) = r + "+" + "(" + tolatex(ba0(g)) + ")";
str tolatex_p(str r, ba1(Term g)) = r + "+" + "(" + tolatex(ba1(g)) + ")";
default str tolatex_p(str r, Term t) = r + "+" + tolatex(t);

str tolatex(mult(plus(Term g, Term h), Term r)) = 
	tolatex_m("(" + tolatex(plus(g, h)) + ")", r);
str tolatex(mult(mult(Term g, Term h), Term r)) = 
	tolatex_m("(" + tolatex(mult(g, h)) + ")", r);
str tolatex(mult(neg(Term g), Term r)) = 
	tolatex_m("(" + tolatex(neg(g)) + ")", r);
default str tolatex(mult(Term t, Term r)) = 
	tolatex_m(tolatex(t), r);
	
str tolatex_m(str r, plus(Term g, Term h)) = r + "\\cdot " + 
	"(" + tolatex(plus(g, h)) + ")";
str tolatex_m(str r, mult(Term g, Term h)) = r + "\\cdot " + 
	"(" + tolatex(mult(g, h)) + ")";
str tolatex_m(str r, ba0(Term g)) = r + "\\cdot " + "(" + tolatex(ba0(g)) + ")";
str tolatex_m(str r, ba1(Term g)) = r + "\\cdot " + "(" + tolatex(ba1(g)) + ")";
default str tolatex_m(str r, Term t) = r + "\\cdot " + tolatex(t);