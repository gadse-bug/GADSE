import saarland.cispa.se.tribble.dsl._

// WARNING : The source Grammar may be wrong !!!!!!!!
// the source file is https://github.com/dballinger/javacc-calculator   ArithmeticParser.jj
// but the calculation expression may defined error and no use EQUALS/LPAREN/RPAREN, 
// in order to obey the author's code ,i don't fix this bug !

Grammar(
	'Start := 'calculation ,
	
	//'EQUALS  := "=" ,
	'MULTIPLY:= "*" ,
	'DIVIDE  := "/" ,
	'ADD     := "+" ,
	'SUBTRACT:= "-" ,
	//'LPAREN  := "(" ,
	//'RPAREN  := ")" ,
	
	'VALUE := ("[0-9]".regex).rep(1,Int.MaxValue) | ("[0-9]".regex).rep(1,Int.MaxValue) ~ "." ~ ("[0-9]".regex).rep(1,Int.MaxValue) ,
	
	'calculation := 'VALUE ~ ('MULTIPLY|'DIVIDE|'ADD|'SUBTRACT) ~ 'VALUE ,
)