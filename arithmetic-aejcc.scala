import saarland.cispa.se.tribble.dsl._

// WARNING : The source Grammar may be wrong !!!!!!!!
// the source file is https://github.com/ubc-cpsc411-2016w2/AEJCC  AEParser.jj
// but the number expression may defined error , in order to obey the author's code ,i don't fix this bug !

Grammar(
	'start :=  'OneExpression ,
	'LPAREN := "(",
	'RPAREN := ")",
	'ADD := "+",
	'SUB := "-",
	'NUM := ("[0-9]".regex).rep(1,Int.MaxValue) ,
	
	'OneExpression := 'Expression ,
	'Expression := 'NUM | 'LPAREN ~ 'Op ~ 'RPAREN ,
	'Op := ('ADD | 'SUB) ~ " " ~ 'Expression ~ " " ~ 'Expression ,
)