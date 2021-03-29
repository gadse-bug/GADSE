import saarland.cispa.se.tribble.dsl._

// Translated from https://github.com/masoniya/JSIJCC

Grammar(
	'START := 'primaryExpression ,

	'BREAK := " break " ,
	'CASE := " case " ,
	'CATCH := " catch " ,
	'CONTINUE := " continue " ,
	'DFLT := " default " ,
	'DELETE := " delete " ,
	'DO := " do " ,
	'ELSE := " else " ,
	'FINALLY := " finally " ,
	'FOR := " for " ,
	'FUNCTION := " function " ,
	'IF := " if " ,
	'IN := " in " ,
	'INSTANCEOF := " instanceof " ,
	'NEW := " new " ,
	'RETURN := " return " ,
	'SWITCH := " switch " ,
	'THIS := " this " ,
	'THROW := " throw " ,
	'TRY := " try " ,
	'TYPEOF := " typeof " ,
	'VAR := " var " ,
	'VOID := " void " ,
	'WHILE := " while " ,
	
	'NULL_LITERAL := " null " ,
	'BOOLEAN_LITERAL := " true " | " false " ,
	'STRING_LITERAL := 'DOUBLE_QUOTES_STRING | 'SINGLE_QUOTES_STRING ,
	'DOUBLE_QUOTES_STRING := "\"" ~ ("[0-9a-zA-Z]".regex ).rep(0,Int.MaxValue) ~ "\"" ,
	'SINGLE_QUOTES_STRING := "'" ~ ("[0-9a-zA-Z]".regex ).rep(0,Int.MaxValue) ~  "'" ,
	'NUMERIC_LITERAL := 'DECIMAL_LITERAL | 'BINARY_INTEGER_LITERAL | 'OCTAL_INTEGER_LITERAL | 'HEX_INTEGER_LITERAL ,
	
	'DECIMAL_LITERAL := ( 'DECIMAL_INTEGER_LITERAL |
						  'DECIMAL_INTEGER_LITERAL ~ "." ~ 'DECIMAL_DIGITS.? |
                          "." ~ 'DECIMAL_DIGITS ) ~ 'EXPONENT_PART.? ,
	
	'EXPONENT_PART := ("e" | "E") ~ ("+" | "-").? ~ 'DECIMAL_DIGITS ,

	'DECIMAL_INTEGER_LITERAL := "0" | 'NON_ZERO_DIGIT ~ 'DECIMAL_DIGITS.? ,
	'BINARY_INTEGER_LITERAL := "0" ~ ("b" | "B") ~ 'BINARY_DIGITS ,
	'OCTAL_INTEGER_LITERAL := "0" ~ ("o" | "O") ~ 'OCTAL_DIGITS ,
	'HEX_INTEGER_LITERAL := "0" ~ ("X" | "x") ~ 'HEX_DIGITS ,

	'NON_ZERO_DIGIT := "[1-9]".regex ,

	'DECIMAL_DIGITS := 'DECIMAL_DIGIT.rep(1,Int.MaxValue) ,
	'BINARY_DIGITS := 'BINARY_DIGIT.rep(1,Int.MaxValue) ,
	'OCTAL_DIGITS := 'OCTAL_DIGIT.rep(1,Int.MaxValue) ,
	'HEX_DIGITS := 'HEX_DIGIT.rep(1,Int.MaxValue) ,

	'DECIMAL_DIGIT := "[0-9]".regex ,
	'BINARY_DIGIT := "0" | "1" ,
	'OCTAL_DIGIT := "[0-8]".regex ,
	'HEX_DIGIT := "[0-9A-F]".regex , 
	
	'IDENTIFIER_NAME := ("[a-zA-Z]".regex | "_" | "$" ) ~ ("[a-zA-Z0-9]".regex | "_" | "$" ) ,
	
	'primaryExpression := 'THIS
						| 'IDENTIFIER_NAME
						| 'literal
						| 'arrayLiteral
						| 'objectLiteral
						| 'functionExpression
						| 'parenthesizedExpression ,
	
	'literal := 'NULL_LITERAL | 'BOOLEAN_LITERAL | 'NUMERIC_LITERAL | 'STRING_LITERAL ,
	'arrayLiteral := "[" ~ 'ellision.? ~ ( 'elementList ~ 'ellision.? ).? ~ "]" ,
	'ellision := ",".rep(1,Int.MaxValue) ,
	'elementList := 'arrayElement ~ ( ('ellision ~ 'arrayElement) ~ 'ellision ~ 'arrayElement).rep(0,Int.MaxValue) ,
	'arrayElement := 'assignmentExpression | 'spreadElement ,
	
	'spreadElement := " ... " ~ 'assignmentExpression ,
	'objectLiteral := "{" ~ 'propertyDefinitionList.? ~ ",".? ~ "}" ,
	
	'propertyDefinitionList := 'propertyDefinition ~ ( "," ~ 'propertyDefinition ).rep(0,Int.MaxValue) ,
	'propertyDefinition := 'IDENTIFIER_NAME ~ " : " ~ 'assignmentExpression ,
	'functionExpression := 'namedFunction | 'anonymousFunction ,
	
	'parenthesizedExpression := "(" ~ 'expression ~ ")" ,
	
	
	'leftSideExpression := 'callExpression | 'newExpression ,
	'callExpression := 'memberExpression ~ 'arguments.rep(0,Int.MaxValue) ,
	'arguments := "(" ~ 'argumentList.? ~ ")" ,
	'argumentList := 'assignmentExpression ~ ("," ~ 'assignmentExpression).rep(0,Int.MaxValue) ,
	'newExpression := 'memberExpression ,
	'memberExpression := 'NEW ~ 'memberExpression ~ ('arguments
		| 'primaryExpression ~ ( "[" ~ 'expression ~ "]").rep(1,Int.MaxValue) | ("." ~ 'IDENTIFIER_NAME).rep(0,Int.MaxValue) ) ,
		
		
	'updateExpression := (" ++" | " --") ~ 'leftSideExpression 
		| 'leftSideExpression ~ ("++ " | "-- ")
		| 'leftSideExpression ,

	'unaryExpression := ('DELETE | 'VOID | 'TYPEOF | " ~ " | " ! " | " - " | " + " ).rep(0,Int.MaxValue) ~ 'updateExpression ,

	'exponentiationExpression := 'unaryExpression ~ (" ** " ~ 'unaryExpression).? ,

	'multiplicativeExpression := 'exponentiationExpression ~ ((" * " | " / " | " % ") ~ 'exponentiationExpression).rep(0,Int.MaxValue) ,

	'additiveExpression := 'multiplicativeExpression ~ ((" + " | " - ") ~ 'multiplicativeExpression).rep(0,Int.MaxValue) ,

	'shiftExpression := 'additiveExpression ~ ((" << " | " >> " | " >>> ") ~ 'shiftExpression).? ,

	'relationalExpression := 'shiftExpression ~ ((" < " | " > " | " <= " | " >= " | 'INSTANCEOF | 'IN ) ~ 'relationalExpression).? ,

	'equalityExpression := 'relationalExpression ~ ((" == " | " != " | " === " | " !== ") ~ 'equalityExpression).? ,

	'bitwiseAndExpression := 'equalityExpression ~ (" & " ~ 'bitwiseAndExpression).? ,
	
	'bitwiseXorExpression := 'bitwiseAndExpression ~ (" ^ " ~ 'bitwiseXorExpression).? ,

	'bitwiseOrExpression := 'bitwiseXorExpression ~ (" | " ~ 'bitwiseOrExpression).? ,
	
	'logicalAndExpression := 'bitwiseOrExpression ~ (" && " ~ 'logicalAndExpression).? ,

	'logicalOrExpression := 'logicalAndExpression ~ (" || " ~ 'logicalOrExpression).? ,

	'conditionalExpression := 'logicalOrExpression ~ (" ? " ~ 'assignmentExpression ~ " : " ~ 'assignmentExpression).? ,

	'assignmentExpression := 'leftSideExpression ~ (" = " | 'assignmentOperator) ~  'assignmentExpression
		| 'conditionalExpression ,

	'assignmentOperator := " *= " | " /= " | " += " | " -= " | " %= " | " <<= " 
		| " >>= " | " >>>= " | " &= " | " ^= " | " |= " | " **= " ,

	'expression := 'assignmentExpression ~ ("," ~ 'assignmentExpression).rep(0,Int.MaxValue) ,
	
	
    'statement := 'emptyStatement
		|'variableDefinition 
		|'block
		|'labeledStatement
		|'ifStatement
		|'switchStatement 
		|'doStatement 
		|'whileStatement 
		|'forStatement 
		|'continueStatement  
		|'breakStatement 
		|'returnStatement 
		|'throwStatement 
		|'tryStatement 
		|'expressionStatement  ,

    'emptyStatement := ";" ,

    'expressionStatement := 'expression ,
    'variableDefinition := 'VAR ~ 'variableDeclarationList ,

    'variableDeclarationList := 'variableDeclaration ~ ("," ~ 'variableDeclaration).rep(0,Int.MaxValue) ,

    'variableDeclaration := 'IDENTIFIER_NAME ~ (" = " ~ 'assignmentExpression).? ,

    'block := "{" ~ 'statement.rep(0,Int.MaxValue) ~ "}" ,

    'labeledStatement := 'IDENTIFIER_NAME ~ " : " ~ ( 'statement | 'namedFunction ) ,

    'ifStatement := 'IF ~ "(" ~ 'expression ~ ")" ~ 'statement ~ ('ELSE ~ 'statement).? ,

    'switchStatement := 'SWITCH ~ "(" ~ 'expression ~ ")" ~ 'caseBlock ,

    'caseBlock := "{" ~ 'caseClauses.? ~ ('defaultClause ~ 'caseClauses.? ).? ~ "}" ,
	
    'caseClauses := 'caseClause.rep(1,Int.MaxValue) ,

    'caseClause := 'CASE ~ 'expression ~ " : " ~ 'statement.rep(0,Int.MaxValue) ,

    'defaultClause := 'DFLT ~ " : " ~ 'statement.rep(0,Int.MaxValue) ,

    'doStatement := 'DO ~ 'statement ~ 'WHILE ~ "(" ~ 'expression ~ ")" ,

    'whileStatement := 'WHILE ~ "(" ~ 'expression ~ ")" ~ 'statement ,

    'forStatement := 'FOR ~ "(" ~ ( 'forHeader | 'forInHeader ) ~ ")" ~ 'statement ,

    'forHeader := 'forInitializer.? ~ ";" ~ 'expression.? ~ ";" ~ 'expression.? ,

    'forInitializer := 'expression | 'variableDefinition ,

    'forInHeader := 'forInBinding ~ 'IN ~ 'expression ,

    'forInBinding := 'leftSideExpression
		| ('VAR ~ 'variableDeclaration) ,

    'continueStatement := 'CONTINUE ~ 'IDENTIFIER_NAME.? ,

    'breakStatement := 'BREAK ~ 'IDENTIFIER_NAME.? ,

    'returnStatement := 'RETURN ~ 'expression.? ,

    'throwStatement := 'THROW ~ 'expression ,

    'tryStatement := 'TRY ~ 'block ~ 'catchClauses.? ~ 'finallyClause.? ,

    'catchClauses := 'catchClause.rep(1,Int.MaxValue) ,

    'catchClause := 'CATCH ~ "(" ~ 'IDENTIFIER_NAME ~ ")" ~ 'block ,

    'finallyClause := 'FINALLY ~ 'block ,
	
	'namedFunction := 'FUNCTION ~ 'IDENTIFIER_NAME ~ "(" ~ 'formalParameters.? ~ ")" ~ "{" ~ 'functionBody.? ~ "}" ,

    'anonymousFunction := 'FUNCTION ~ "(" ~ 'formalParameters.? ~ ")" ~ "{" ~ 'functionBody.? ~ "}" ,
	
    'formalParameters := 'IDENTIFIER_NAME ~ ("," ~ 'IDENTIFIER_NAME).rep(0,Int.MaxValue) ,

    'functionBody := 'topStatements ,

    'topStatement :=  'statement | 'namedFunction ,

    'topStatements := 'topStatement.rep(1,Int.MaxValue) ,

)