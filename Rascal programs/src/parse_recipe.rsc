module parse_recipe

import IO;
import util::Eval;
import util::Clipboard;
import String;
import List;
import Map;

///////////////////////
// Syntax definition //
///////////////////////

layout Whitespace = [\t\n\r\f\ ]*;

lexical Constant = [0-9]+;
lexical Variable = [a-z][\'*]*;
lexical Name = [A-Z] | [\\A-Za-z0-9][A-Za-z0-9\'*]+([.][0-9])*;
lexical Expression = [+\-\\(\\)/\\*0-9a-z\']+;

syntax Term
 = A: Constant c
 > B: Name n "()"
 | C: Variable v
 > D: Name n Variable v
 | J: "plus^" Constant c "(" Argument a ")"
 | F: "sum^" Constant c "(" Argument a ")" 
 > G: Name n "_" Expression e "(" Argument a ")"
 > E: Name n "(" Argument a ")"
 ;
syntax Argument
 = Term r
 | Argument a "," Argument a
 ;
syntax Rule 
 = Label l Equality e
 | Whitespace
 ;
syntax Label
 = "[" Name n "]"
 | "[" Name n ItVars i "]{" Constant s ".." Constant e "}"
 ;
syntax ItVars
 = "." Variable v
 | ItVars i ItVars j
 ;
syntax Equality
 = Term t "=" Term r
 ;
 
Term F(Constant c, Argument a) {
	switch("<c>") {
		case "1": return [Term] "<a>";
		default: return [Term] "plus(<a>, sum^<toInt("<c>")-1>(<a>))";
	}
} 
Term J(Constant c, Argument a) {
	if((Argument)`<Term a>, <Term b>` := a) {
		switch("<c>") {
			case "1": return [Term] "plus(<a>, <b>)";
			default: return [Term] "plus^<toInt("<c>")-1>(plus(<a>, <b>), <b>)";
		}
	}
} 

map[str, str] names = ("0": "Zero",
			 		   "1": "One",
			 		   "2": "Two",
			 		   "3": "Three",
			 		   "4": "Four",
			 		   "5": "Five",
			 		   "6": "Six",
			 		   "7": "Seven",
			 		   "8": "Eight",
			 		   "9": "Nine");
 
///////////////////////////////////
// Generate programs from recipe //
///////////////////////////////////
 
list[Rule] to_rules(loc path) {
	list[str] outputLines = []; 
	for(str line <- readFileLines(path)) {
		if((Rule)`[<Name n><ItVars i>]{<Constant c>..<Constant k>} <Equality e>` := [Rule] line) {
			list[str] itvars = ["<v>" | /(ItVars)`.<Variable v>` := i]; 
			line = "[<n><i>] <e>";
			value range = eval("[<c>..<k>+1];")[0];
			for(list[str] combination <- [["<n>" | /int n := row] | value row <- list_prod(range, size(itvars))]) {
				outputLines += replaceAll(line, itvars, combination);
			}
		}
		else outputLines += line;
	}
	outputLines = mapper(outputLines, replace_notation);
	list[Rule] outputRules = [[Rule] line | str line <- outputLines];
	outputRules = outermost visit(outputRules) {
		case (Term)`<Name f>_<Expression e>(<Argument a>)`: { 
			if("<f>" != "sum" && "<f>" != "plus") insert [Term] apply_func("<f>", "<a>", eval("<e>;")[0]);
		}
	}
	return outputRules; 
}

// Create a set of rules that can be used by provers like AProVE and CSI.
void to_trs(loc path, bool clip) {
	list[Rule] rules = to_rules(path);
	list[str] output = [];
	set[str] vars = {"<v>" | /(Variable)`<Variable v>` := rules};
	output += "(VAR <intercalate(" ", [*vars])>)";
	output += "(RULES";
	visit(rules) {
		case (Rule)`[<Name _>] <Term t> = <Term r>`: {
			output += "<t> -\> <r>";
		}
	}
	output += ")";
	print_lines(output);
	if(clip) copy_lines(output);
}

// Create the step function used by GCprover.
void to_stepfunction(loc path, bool clip) {
	list[Rule] rules = to_rules(path);
	list[str] output = [];
	visit(rules) {
		case (Rule)`[<Name n>] <Term t> = <Term r>`: {
			t = visit(t) {
				case (Term)`<Variable v>` => [Term] "Term <v>" 
				case (Term)`<Constant c>` => [Term] "<names["<c>"]>()"
			}
			r = visit(r) {
				case (Term)`<Constant c>` => [Term] "<names["<c>"]>()"
			}
			output += "tuple[str label, Term result] step(<t>) = \<\"\\\\text{<n>}\", <r>\>;";
		}
	}
	output += "\ndefault tuple[str label, Term result] step(Term x) = \<\"NA\", x\>;";
	print_lines(output);
	if(clip) copy_lines(output);
}

// Create the 'if' cases for the trs.
void to_ifcases(loc path, bool clip) {
	list[Rule] rules = to_rules(path);
	list[str] output = [];
	output += "list[step] rewrite_outer(Term t) {";
	output += "	list[tuple[str label, Term result]] output = [];";
	visit(rules) {
		case (Rule)`[<Name n>] <Term t> = <Term r>`: {
			t = visit(t) {
				case (Term)`<Variable v>` => [Term] "Term <v>" 
				case (Term)`<Constant c>` => [Term] "<names["<c>"]>()"
			}
			r = visit(r) {
				case (Term)`<Constant c>` => [Term] "<names["<c>"]>()"
			}
			output += "	if(<t> := t) output += \<\"\\\\text{<n>}\", <r>\>;";
		}
	}
	output += "	return output;";
	output += "}";
	print_lines(output);
	if(clip) copy_lines(output);
}

void print_lines(list[str] lines) {
	for(str line <- lines) println(line);
}

void copy_lines(list[str] lines) {
	copy(intercalate("\n", lines));
}

str replaceAll(str s, list[str] find, list[str] replacement) {
	assert size(find) == size(replacement): "find and replacement lists must match in size.";
	for(int i <- [0..size(find)]) s = replaceAll(s, find[i], replacement[i]);
	return s;
}

// Returns the n-th power of the list l. Output consists of nested tuples.
default list[value] list_prod(list[value] l, int n) {
	return l * list_prod(l, n-1);
}
list[value] list_prod(list[value] l, 1) {
	return l;
}

// Computes the meaning of the notation used in 'Datatype defining ...'.
str replace_notation(str s) {
	return outermost visit(s) {
				case /<s: [0-9]+['][']>/ => "<toInt(s[0..-2]) - 1>"
				case /<s: [0-9]+[']>/ => "<toInt(s[0..-1]) + 1>"
				case /<s: [0-9]+[*]>/ => "<10 - toInt(s[0..-1])>"
			}
}

// Apply 'name' n times to 'args'.
default str apply_func(str name, str args, int n) {
	return "<name>(<apply_func(name, args, n-1)>)";
}
str apply_func(str name, str args, 0) {
	return args;
}