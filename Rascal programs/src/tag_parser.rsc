module tag_parser

import IO;

void edit(loc path) {
	list[str] outputLines = []; 
	for(str line <- readFileLines(path)) {
		if(/<t:\\tag\*\{>\[<n:[a-z]+><c: [0-9]+>\.\$<r: [^\$]*\$\}>/ := line) {
			outputLines += "<t>$[\\text{<n>}<c>.<r>";
		}
		else if(/<t:\\tag\*\{>\[<n:[a-z]+><c: [0-9]+>\]\}/ := line) {
			outputLines += "<t>$[\\text{<n>}<c>]$}";
		}
		else {
			outputLines += line;
		}
	}
	for(str line <- outputLines) println(line);
}