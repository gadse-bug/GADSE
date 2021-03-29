import saarland.cispa.se.tribble.dsl._

// WARNING : The source Grammar may be wrong !!!!!!!!
// the source file is https://github.com/benjismith/bling   BlingParser.jj
// but the number expression may defined error , in order to obey the author's code ,i don't fix this bug !

Grammar(
	'Start := 'Expression ,
  
	//< PLUS : "+" > | < MINUS: "-" >| < STAR : "*" >| < SLASH : "/" >| < CARAT : "^" >| < LPAREN : "(" > | < RPAREN : ")" > 
	// | < NUMBER :(["0"-"9"])+("."(["0"-"9"])+)? (["e","E"](["+","-"])?(["0"-"9"])+)?>
	'PLUS :=  "+" ,
	'MINUS := "-" ,
	'STAR :=  "*" ,
	'SLASH := "/" ,
	'CARAT := "^" ,
	'LPAREN:= "(" ,
	'RPAREN:= ")" ,
	'NUMBER:= ("[0-9]".regex).rep(1,Int.MaxValue) ~ ("." ~ ("[0-9]".regex).rep(1,Int.MaxValue)).? ~ (("e"|"E") ~ ("+"|"-").? ~ ("[0-9]".regex).rep(1,Int.MaxValue) ).? ,
	
	'Expression := 'AdditiveExpression ,
	'AdditiveExpression := 'MultiplicativeExpression ~ " " ~('MINUS|'PLUS)~ " " ~ 'MultiplicativeExpression ,
	'MultiplicativeExpression := 'ExponentialExpression ~ " " ~ ('STAR|'SLASH) ~ " " ~ 'ExponentialExpression ,
	'ExponentialExpression := 'LPAREN ~ 'NegatableExpression ~ " " ~ 'CARAT ~ " " ~ 'NegatableExpression ~ 'RPAREN ,
	'NegatableExpression := 'MINUS.? ~ 'PrimeExpression ,
	'PrimeExpression := 'NumberLiteral | 'GroupedExpression ,
	'NumberLiteral := " " ~ 'NUMBER ~ " "  ,
	'GroupedExpression := 'LPAREN ~ 'AdditiveExpression ~ 'RPAREN ,
)