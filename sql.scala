import saarland.cispa.se.tribble.dsl._

// the source file is https://github.com/gtxistxgao/SQLParser   SQLParser.jj

Grammar(
	'Start := 'Query ,
	
	'SELECT := " SELECT " ,
	'FROM := " FROM " ,
	'WHERE := " WHERE " ,
	'AND := " AND " ,
	
	'OPEN_PAR := "(" ,
	'CLOSE_PAR := ")" ,
	'DOT := "." ,
	'COMMA := "," ,
	'QUO := "\"" ,

	'OPERATOR := " > " | " < " | " = " | " >= " | " <= " | " <> " | " != " ,
	
	'DIGITS := ("[0-9]".regex).rep(1,Int.MaxValue) ,
	'NAME := ("[a-z0-9]".regex).rep(1,Int.MaxValue) ,
	
	'Query := 'SFWStatement ,
	'SFWStatement := 'SelectClause ~ 'FromClause ~ 'WhereClause ,
	'SelectClause := 'SELECT ~ 'Attr ,
	'Attr := 'NAME ~ 'DOT ~ 'NAME ~ ('COMMA ~ 'Attr).rep(0,Int.MaxValue) ,
	'FromClause := 'FROM ~ 'RelVal ,
	'RelVal := 'NAME ~ ('COMMA ~ 'RelVal).rep(0,Int.MaxValue) ,
	'WhereClause := 'WHERE ~ 'Expression ,
	'Expression := 'OPEN_PAR ~ 'Expression ~ 'CLOSE_PAR ~ ('AND ~ 'Expression).rep(0,Int.MaxValue) 
				| 'Factor ~ ('AND ~ 'Expression).rep(0,Int.MaxValue) ,
	'Factor := 'BooleanAttr ~ 'OPERATOR ~ 'BooleanAttr ,
	'BooleanAttr := 'DIGITS | ('NAME ~ 'DOT ~ 'NAME) | ('QUO ~ 'NAME ~ 'QUO)  ,
)