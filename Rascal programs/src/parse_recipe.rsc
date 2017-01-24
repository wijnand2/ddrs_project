module parse_recipe

import IO;
import util::Eval;

///////////////////////
// Syntax definition //
///////////////////////

layout Whitespace = [\t\n\r\f\ ]*;

// Map maken voor constanten naar de namen van die constanten.

lexical Constant = [0-9]+;
lexical Variable = [a-z];
lexical Name = [A-Z] | [\\A-Za-z0-9][A-Za-z0-9]+;
lexical Expression = [+\-\\(\\)/\\*0-9a-z ]+;

syntax Term
 = Constant c
 | Variable v
 | Name n "(" Argument a ")"
 | Name n "_" Expression e "(" Argument a ")"
 ;
syntax Argument
 = Term r
 | Term r "," Argument a
 ;
syntax Label
 = "[" Name n "]"
 | "[" Name n ItVars i "]{" Constant s ".." Constant e "}"
 ;
syntax ItVars
 = "." Variable v
 | "." Variable v ItVars i
 ;
start syntax Rule 
 = Label l Term t "=" Term r
 | Whitespace
 ;
 
///////////////////////////////////
// Generate programs from recipe //
///////////////////////////////////
 
void to_trs(loc path) {
	list[str] recipe = readFileLines(path);
	for(str line <- recipe) {
		if((Rule)`<Label l> <Term t> = <Term r>` := [Rule] line) {
			println("---");
			print("The line is ");
			println(line);
			println("The parse is ");
			if((Label)`[<Name n>]` := l) {
				println("<t> + test");
			}
			//println(l + "test");
			//println(t);
			//println(r);
		} 
	}
}