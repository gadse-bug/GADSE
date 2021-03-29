import saarland.cispa.se.tribble.dsl._

Grammar(
	'start := 'translationUnit,
	
	'FUNCTION := " function " ,
	'RETURN   := " return "   ,
	'IF       := " if "       ,
	'ELSE     := " else "     ,
	'WHILE    := " while "    ,
	
	'ASSIGN   := " = "  ,
	'EQUAL    := " == " ,
	'NOT_EQUAL:= " != " ,
	'GT := " > "  ,
	'GE := " >= " ,
	'LT := " < "  ,
	'LE := " <= " ,
	'STAR := " * " ,
	'DIV  := " / " ,
	'PLUS := " + " ,
	'MINUS:= " - " ,
	'LOR  := " || " ,
	'LAND := " && " ,
	
	'LPAREN := " ( " ,
	'RPAREN := " ) " ,
	'LCURLY := " { " ,
	'RCURLY := " } " ,
	//'LBRACK := " [ " ,
	//'RBRACK := " ] " ,
	'SEMI   := " ; " ,
	'COMMA  := " , " ,
	'DOT    := "." ,


	//IDENTIFIER: (<LETTER>|"_"|"$") (<LETTER>|<DIGIT>|"_"|"$")*
	'IDENTIFIER := ('LETTER | "_" | "$" ) ~ ('LETTER | 'DIGIT | "_" | "$").rep(0,Int.MaxValue) ,
	'LETTER := "[a-z]".regex | "[A-Z]".regex ,
	'DIGIT := "[0-9]".regex ,
	'NON_ZERO_DIGIT := "[1-9]".regex ,
	'ESC := "\\" ~ ("n" | "t" | "b" | "r" | "f" | "\\" | "'" | "\"" ) ,

	//INT_LITERAL:  "0" | <NON_ZERO_DIGIT> (<DIGIT>)*
	'INT_LITERAL := "0" | 'NON_ZERO_DIGIT ~ 'DIGIT.rep(0,Int.MaxValue) ,

	//DOUBLE_LITERAL: (<DIGIT>)* ((<DOT>){1} (((<DIGIT>)+ (["e","E"])?) (["+","-"])? (<DIGIT>)+ | (<DIGIT>)+) (["d","D"])?)?
	'DOUBLE_LITERAL := 'NON_ZERO_DIGIT ~ 'DIGIT.rep ~ ( ('DOT).rep(0,1) ~ ((('DIGIT).rep(1,Int.MaxValue) ~ ("e"|"E").? ) ~ ("+"|"-").? ~ ('DIGIT).rep(1,Int.MaxValue) | ('DIGIT).rep(1,Int.MaxValue)) ~ ("d"|"D").? ).? ,
	'DOUBLE_LITERAL2 := "0" ~ ( 'DOT ~ ((('DIGIT).rep(1,Int.MaxValue) ~ ("e"|"E").? ) ~ ("+"|"-").? ~ 'DIGIT.rep(1,Int.MaxValue) | 'DIGIT.rep(1,Int.MaxValue)) ~ ("d"|"D").? ).? ,

	//STRING_LITERAL: "\"" (<ESC> | ~["\"","\\","\n","\r"])* "\""
	//'STRING_LITERAL := "\"" ~ ('ESC | "[^\\n\\r]".regex ).rep ~ "\"" ,
	'STRING_LITERAL := "\"" ~ ('ESC | "[a-zA-Z0-9]".regex ).rep ~ "\"" ,
	
	//translationUnit ::= {function}
	'translationUnit := 'Function.rep(0,Int.MaxValue) ,
	
	//qualifiedIdentifier ::= IDENTIFIER {DOT IDENTIFIER}
	'qualifiedIdentifier := 'IDENTIFIER ~ ('DOT ~ 'IDENTIFIER).rep ,
	
	//Function ::= FUNCTION IDENTIFIER LPAREN [IDENTIFIER {COMMA IDENTIFIER}] RPAREN Block
	'Function := 'FUNCTION ~ 'IDENTIFIER ~ 'LPAREN ~ ('IDENTIFIER ~ ( 'COMMA ~ 'IDENTIFIER ).rep(0,Int.MaxValue) ).? ~ 'RPAREN ~ 'Block ,
	
	//Block ::= LCURLY {blockStatement} RCURLY
	'Block := 'LCURLY ~ 'blockStatement.rep ~ 'RCURLY ,
	
	//blockStatement ::= variableDeclarator | statement
	'blockStatement := 'variableDeclarator | 'statement ,
	
	//variableDeclarator ::= IDENTIFIER [ASSIGN expression] SEMI
	'variableDeclarator := 'IDENTIFIER ~ ('ASSIGN ~ 'expression).? ~ 'SEMI ,
	
	//arguments ::= LPAREN [expression {COMMA expression}] RPAREN
	'arguments := 'LPAREN ~ ('expression ~ ('COMMA ~ 'expression).rep(0,Int.MaxValue) ).? ~ 'RPAREN ,
	
	//parExpression ::= LPAREN expression RPAREN
	'parExpression := 'LPAREN ~ 'expression ~ 'RPAREN ,
	
	//statement ::= block
	//                | IF parExpression statement [ELSE statement]
	//                | WHILE parExpression statement
	//                | RETURN [expression] SEMI
	//                | SEMI
	//                | expression SEMI
	'statement := 'Block
			| 'IF ~ 'parExpression ~ 'statement ~ ('ELSE ~ 'statement).?
			| 'WHILE ~ 'parExpression ~ 'statement
			| 'RETURN ~ 'expression.? ~ 'SEMI
			| 'SEMI
			| 'expression ~ 'SEMI ,
	
	'expression := 'assignmentExpression ,
	//'expression := 'assignmentExpression | 'additiveExpression,
	
	//assignmentExpression ::= conditionalOrExpression [(ASSIGN) assignmentExpression]
	'assignmentExpression := 'conditionalOrExpression ~ ('ASSIGN ~ 'assignmentExpression).? ,
	
	//conditionalOrExpression ::= conditionalAndExpression {LOR conditionalAndExpression}
	'conditionalOrExpression := 'conditionalAndExpression ~ ( 'LOR ~ 'conditionalAndExpression ).rep(0,Int.MaxValue) ,
	
	//conditionalAndExpression ::= inclusiveOrExpression {LAND inclusiveOrExpression}
	'conditionalAndExpression := 'inclusiveOrExpression ~ ('LAND ~ 'inclusiveOrExpression).rep(0,Int.MaxValue) ,
	
	//equalityExpression ::= relationalExpression {(EQUAL|NOT_EQUAL) relationalExpression}
	'equalityExpression := 'relationalExpression ~ (('EQUAL|'NOT_EQUAL) ~ 'relationalExpression).rep(0,Int.MaxValue) ,
	
	//relationalExpression ::= primary  (GT | LT | GE | LE) primary
	'relationalExpression := 'primary ~ ('GT | 'LT | 'GE | 'LE) ~ 'primary ,
	
	//inclusiveOrExpression ::=	exclusiveOrExpression |	inclusiveOrExpression '|' exclusiveOrExpression
	//'inclusiveOrExpression := 'exclusiveOrExpression |	('inclusiveOrExpression ~ 'LOR~ 'exclusiveOrExpression) ,
	'inclusiveOrExpression := 'equalityExpression | 'inclusiveOrExpression ~ 'LAND ~ 'equalityExpression ,
	
	//exclusiveOrExpression ::=	andExpression |	exclusiveOrExpression '^' andExpression
	//'exclusiveOrExpression := 'andExpression |	'exclusiveOrExpression ~ " ^ " ~ 'andExpression ,
	
	//andExpression ::=	equalityExpression | andExpression '&' equalityExpression
	//'andExpression := 'equalityExpression | 'andExpression ~ 'LAND ~ 'equalityExpression ,
	
	//additiveExpression ::= multiplicativeExpression {(PLUS|MINUS) multiplicativeExpression}
	'additiveExpression := 'multiplicativeExpression ~ (( 'PLUS | 'MINUS ) ~ 'multiplicativeExpression).rep(0,Int.MaxValue) ,
	
	//multiplicativeExpression ::= unaryExpression {(STAR|DIV) unaryExpression}
	'multiplicativeExpression := 'unaryExpression ~ (('STAR|'DIV).rep(1,1) ~ 'unaryExpression).rep(0,Int.MaxValue) ,
	
	'unaryExpression := 'INT_LITERAL | 'DOUBLE_LITERAL | 'DOUBLE_LITERAL2 ,
	
	//primary ::= parExpression
	//		| IDENTIFIER [arguments]
	//		| IDENTIFIER
	//		| literal
	//		| qualifiedIdentifier // arguments?
	'primary := 'parExpression
			| 'IDENTIFIER ~ 'arguments.?
			| 'IDENTIFIER
			| 'literal
			| 'additiveExpression
			| 'qualifiedIdentifier ,
	
	//literal ::= INT_LITERAL | STRING_LITERAL
	'literal := 'INT_LITERAL | 'STRING_LITERAL | 'DOUBLE_LITERAL | 'DOUBLE_LITERAL2 ,
)