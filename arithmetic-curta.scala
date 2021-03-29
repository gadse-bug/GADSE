import saarland.cispa.se.tribble.dsl._

// WARNING : The source Grammar may be wrong !!!!!!!!
// the source file is https://github.com/bkiers/Curta  CurtaParser.jjt
// but the number expression may defined error , in order to obey the author's code ,i don't fix this bug !

Grammar(
	'start :=  'EOS.rep(0,Int.MaxValue) ~ 'statements ,
	'EOS        := ";" | "\r\n" | "\n" | "\r" , 
	'OParen     := "(" ,
	'CParen     := ")" ,
	'Assign     := " = " ,
	'Or         := " || " ,
	'And        := " && " ,
	'BOr        := " | " ,
	'BXOr       := " ^ " ,
	'BAnd       := " & " ,
	'Add        := " + " ,
	'Sub        := " - " ,
	'Pow        := " ** " ,
	'Mul        := " * " ,
	'Div        := " / " ,
	'Mod        := " % " ,
	'GTE        := " >= " ,
	'GT         := " > " ,
	'LTE        := " <= " ,
	'LT         := " < " ,
	'SLShift    := " << " ,
	'URShift    := " >>> " ,
	'SRShift    := " >> " ,
	'Eq         := " == " ,
	'NEq        := " != " ,
	'Not        := " ! " ,
	'BNot       := " ~ " ,
	'Comma      := "," ,
	'True       := " true " ,
	'False      := " false " ,
	'Null       := " null " ,
	'Return     := " return " ,
	
	
	'NumDouble  := 'Digit.rep(1,Int.MaxValue) ~ "." ~  'Digit.rep(1,Int.MaxValue) | "." ~ 'Digit.rep(1,Int.MaxValue) ,
	'NumLong    := 'Digit.rep(1,Int.MaxValue) ,
	'Identifier := ("_" | 'Letter ) ~ ("_" | 'AlphaNum ).rep(0,Int.MaxValue) ,
	
	'Digit    := "[0-9]".regex ,
	'Letter   := "[a-zA-z]".regex ,
	'AlphaNum := 'Letter | 'Digit ,
	
	
	
	'statements := ( 'assignment ~ 'EOS.rep(1,Int.MaxValue)).rep(0,Int.MaxValue) ~ 'ret ,
	'ret := 'Return.?  ~ 'expr ~  'EOS.rep(0,Int.MaxValue) ,
	'assignment := 'id ~ 'Assign ~ 'expr ,
	'expr := 'or ,
	'or  := 'and ~( 'Or  ~ 'and ).rep(0,Int.MaxValue) ,
	'and := 'bor ~( 'And ~ 'bor ).rep(0,Int.MaxValue) ,
	'bor := 'bxor~( 'BOr ~ 'bxor).rep(0,Int.MaxValue) ,
	'bxor:= 'band~( 'BXOr~ 'band).rep(0,Int.MaxValue) ,
	'band:= 'equality ~ ( 'BAnd ~ 'equality).rep(0,Int.MaxValue) ,
	
	
	
	'equality := 'relational ~ ('Eq ~ 'relational | 'NEq ~ 'relational).rep(0,Int.MaxValue) ,
	'relational:='shift ~ ('GTE ~ 'shift
						|  'LTE ~ 'shift
						|  'GT ~ 'shift
						|  'LT ~ 'shift).rep(0,Int.MaxValue) ,
	'shift := 'add ~ ('SLShift ~ 'add
					| 'URShift ~ 'add 
					| 'SRShift ~ 'add ).rep(0,Int.MaxValue) ,
	'add := 'mul ~ ( ('Add ~ 'mul ) | ('Sub ~ 'mul ) ).rep(0,Int.MaxValue) ,
	'mul := 'unary ~ ( ('Mul ~ 'unary) | ('Div ~ 'unary) | ('Mod ~ 'unary) ).rep(0,Int.MaxValue) ,
	'unary := 'OParen ~ (('Sub ~ 'unary) | ('Add ~ 'unary) | ('Not ~ 'unary) |  'bitwiseUnary) ~ 'CParen  ,
	'bitwiseUnary := ('BNot ~ 'unary) | 'pow  ,
	'pow := 'atom ~ ( 'Pow ~ 'unary ).rep(0,Int.MaxValue) ,
	
	
	'atom := 'function | 'id | 'num | '_null | 'bool | ('OParen ~ 'expr ~ 'CParen) ,
	'function := 'id ~ 'params ,
	'num := " " ~ ('NumDouble | 'NumLong) ~ " " ,
	'_null := " " ~ 'Null ~ " ",
	'id := " " ~ 'Identifier ~ " " ,
	'bool := 'True | 'False ,
	'params := 'OParen ~ ('expr ~ ('Comma ~ 'expr).rep(0,Int.MaxValue)).? ~ 'CParen ,
)