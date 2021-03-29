import saarland.cispa.se.tribble.dsl._

// Notes: the xpath grammar is from https://github.com/zhegexiaohuozi/JsoupXpath Xpath.g4

Grammar(
	'START := 'expr ,
	
	'locationPath :=  'relativeLocationPath |  'absoluteLocationPathNoroot ,

	'absoluteLocationPathNoroot := ('PATHSEP|'ABRPATH) ~ 'relativeLocationPath ,

	'relativeLocationPath := 'step ~ (('PATHSEP|'ABRPATH) ~ 'step).rep(0,Int.MaxValue) ,

	'step :=  ('axisSpecifier ~ 'nodeTest ~ 'predicate.rep(0,Int.MaxValue)) |  'abbreviatedStep ,

	'axisSpecifier :=  'AxisName ~ " :: "  |  "@".? ,

	'nodeTest :=  'nameTest
				| 'NodeType ~ "(" ~ ")"
				| "processing-instruction" ~ "(" ~ 'Literal ~ ")" ,

	'predicate := "[" ~ 'expr ~ "]" ,

	'abbreviatedStep :=  " . "  |  " .. " ,

	'expr  :=  'orExpr ,

	'primaryExpr :=  'variableReference
				|   "(" ~ 'expr ~ ")"
				|  'Literal
				|  'Number
				|  'functionCall ,

	'functionCall :=  'functionName ~ "(" ~ ( 'expr ~ ("," ~ 'expr).rep(0,Int.MaxValue) ).? ~ ")" ,

	'unionExprNoRoot :=  ('pathExprNoRoot ~ ('PIPE ~ 'unionExprNoRoot).?)
					|  ('PATHSEP ~ 'PIPE ~ 'unionExprNoRoot) ,

	'pathExprNoRoot :=  'locationPath |  ('filterExpr ~ (('PATHSEP|'ABRPATH) ~ 'relativeLocationPath).?) ,

	'filterExpr :=  'primaryExpr ~ 'predicate.rep(0,Int.MaxValue) ,

	'orExpr  :=  'andExpr ~ (" or " ~ 'andExpr).rep(0,Int.MaxValue) ,

	'andExpr  :=  'equalityExpr ~ (" and " ~ 'equalityExpr).rep(0,Int.MaxValue) ,

	'equalityExpr := 'relationalExpr ~(('EQUALITY | 'INEQUALITY) ~ 'relationalExpr).rep(0,Int.MaxValue) ,

	'relationalExpr :=  'additiveExpr ~ (('LESS | 'MORE_ | 'LE | 'GE | 'START_WITH | 'END_WITH | 'CONTAIN_WITH | 'REGEXP_WITH | 'REGEXP_NOT_WITH ) ~ 'additiveExpr).rep(0,Int.MaxValue) ,

	'additiveExpr :=  'multiplicativeExpr ~ (( 'PLUS | 'MINUS ) ~ 'multiplicativeExpr).rep(0,Int.MaxValue) ,

	'multiplicativeExpr :=  'unaryExprNoRoot ~ (( 'MUL | 'DIVISION | 'MODULO ) ~ 'multiplicativeExpr).? ,

	'unaryExprNoRoot :=  'MINUS.? ~ 'unionExprNoRoot ,

	'qName  :=  'nCName ~ (" : " ~ 'nCName).? ,

	'functionName :=  'qName   ,

	'variableReference :=  " $ " ~ 'qName ,

	'nameTest := " * " | 'nCName ~ " : * " |  'qName ,

	'nCName  :=  'NCName |  'AxisName ,

	'NodeType:=  " comment "
				|  " text "
				|  " processing-instruction "
				|  " node "
				|  " num " 
				|  " allText "  
				|  " outerHtml "  
				|  " html " ,

	'Number  :=  'Digits ~ ("." ~ 'Digits.?).? | "." ~ 'Digits ,

	'Digits  :=  ("[0-9]".regex).rep(1,Int.MaxValue) ,

	'AxisName:=  " ancestor "
				|  " ancestor-or-self "
				|  " attribute "
				|  " child "
				|  " descendant "
				|  " descendant-or-self "
				|  " following "
				|  " following-sibling "
				//  |  "namespace"
				|  " parent "
				|  " preceding "
				|  " preceding-sibling "
				|  " self "
				|  " following-sibling-one "
				|  " preceding-sibling-one "
				|  " sibling " ,


	'PATHSEP  := " / " ,
	'ABRPATH := " // ",
	//'LPAR := "(",
	//'RPAR := ")",
	//'LBRAC := "[",
	//'RBRAC := "]",
	'MINUS := " - ",
	'PLUS  := " + ",
	//'DOT   := " . ",
	'MUL   := " * ",
	'DIVISION := " `div` ",
	'MODULO := " `mod` ",
	//'DOTDOT := " .. ",
	//'AT   := " @ ",
	//'COMMA:= ",",
	'PIPE := " | ",
	'LESS  := " < ",
	'MORE_ := " > ",
	'LE    := " <= ",
	'GE    := " >= ",
	'EQUALITY := " = ",
	'INEQUALITY := " != ",
	'START_WITH := " ^= ",
	'END_WITH := " $= ",
	'CONTAIN_WITH := " *= ",
	'REGEXP_WITH :=  " ~= ",
	'REGEXP_NOT_WITH := " !~ ",
	//'COLON := " : ",
	//'CC    := " :: ",
	//'APOS  := " \' ",
	//'QUOT  := " \" ",
  
	//'Literal  :=  "\"" ~ ("[^\\\"]".regex).rep(0,Int.MaxValue) ~ "\""
	//			| "\'" ~ ("[^\\\']".regex).rep(0,Int.MaxValue) ~ "\'" ,
	'Literal  :=  "\"" ~ ("[a-zA-Z0-9]".regex).rep(0,Int.MaxValue) ~ "\""
				| "\'" ~ ("[a-zA-Z0-9]".regex).rep(0,Int.MaxValue) ~ "\'" ,


	'NCName  :=  " " ~ 'NCNameStartChar ~ 'NCNameChar.rep(0,Int.MaxValue) ~ " " ,

	//modified by zgf
	'NCNameStartChar := "[a-zA-Z]".regex | "_" ,

	'NCNameChar :=  'NCNameStartChar | "-" | " . " | "[0-9]".regex ,
	
)