import saarland.cispa.se.tribble.dsl._

Grammar(
	'START := 'procedure ,

	'DIGIT := "[0-9]".regex ,
    'INTEGER_LITERAL := "[0-9]".regex ~ 'DIGIT.rep(0,Int.MaxValue) ,
    'REAL_LITERAL := 'DIGIT.rep(1,Int.MaxValue)
					|'DIGIT.rep(1,Int.MaxValue) ~ "."
					|'DIGIT.rep(1,Int.MaxValue) ~ "." ~ 'DIGIT.rep(1,Int.MaxValue)
					|"." ~ 'DIGIT.rep(1,Int.MaxValue) ,
    //'STRING := "\"" ~ ( ("[^\\n\\r\\\"]".regex).rep(0,Int.MaxValue) | (("\\" ~ ("\"" | "\n" | "\\" | "'\n\'")))).rep(0,Int.MaxValue) ~ "\"" ,  //maybe wrong
	'STRING := "\"" ~ ( ("[a-zA-Z0-9]".regex).rep(0,Int.MaxValue) | ("\\" ~ ("\"" | "\n" | "\\" | "'\n\'"))).rep(0,Int.MaxValue) ~ "\"" ,
	
	'IF := " if " ,
    'ELSE := " else " ,
    'WHILE := " while " ,
    'FOR := " for " ,
    'READ := " read " ,
    'WRITE := " write " ,
    'INT := " int " ,
    'REAL:= " real " ,
    'VOID:= " void " ,
    'CHAR:= " char " ,
    'RETURN:= " return " ,
	
	'PLUS := " + " ,
    'MINUS := " - " ,
    'MUL := " * " ,
    'DIV := " / " ,
    'ASSIGN := " = " ,
    'LT := " < " ,
    'GT := " > " ,
    'LET := " <= " ,
    'GET := " >= " ,
    'EQ := " == " ,
    'NEQ := " <> " ,
	
	'LPS := " ( " ,
    'RPS := " ) " ,
    //'COMMA := "," ,
    //'SEMI := ";" ,
    'LBRACE := " { " ,
    'RBRACE := " } " ,
    //'LBRACKET := "[" ,
    //'RBRACKET := "]" ,
    //'SINQS := "'" ,
    //'DOUQS := "\"" ,
    //'ADDR := "&" ,
	
	'IDENTIFIERS := ("[a-zA-Z]".regex | "_") ~ ("[a-zA-Z0-9]".regex | "_").rep(0,Int.MaxValue) ,
	'procedure := 'Statement.rep(0,Int.MaxValue) ,
	'Statement := 'SequenceStatement
				| 'IfStatement
				| 'While
				| 'For
				| 'Read
				| 'Write
				| 'StatementBlock
				| 'DeclareFunction
				| 'FunctionCall
				| 'AssignStatement
				| 'Return ,
	'type := 'INT | 'REAL | 'VOID | 'CHAR ,
	
	'SequenceStatement := ('Declare | 'AssignStatement) ~ ";" ,
	'Declare := 'type ~ 'IDENTIFIERS ~ ('ASSIGN ~ 'expression).? ~ ("," ~ 'IDENTIFIERS ~ ('ASSIGN ~ 'expression).? ).rep(0,Int.MaxValue) ,
	'AssignStatement := 'IDENTIFIERS ~ 'ASSIGN ~ 'expression ~ ";" ,
	
	'IfStatement := 'IF ~ 'LPS ~ 'expression ~ 'RPS ~ 'Statement ~ ( 'ELSE ~ 'Statement).? ,
	'While := 'WHILE ~ 'LPS ~ 'expression ~ 'RPS ~ 'Statement ,
	'For := 'FOR ~ 'LPS ~ 'SequenceStatement ~ 'expression ~ ";" ~ 'AssignStatement ~ 'RPS ~ 'Statement ,
	'Read := 'READ ~ 'LPS ~ 'IDENTIFIERS ~ 'RPS ~ ";" ,
	'Write := 'WRITE ~ 'LPS ~ 'expression ~ 'RPS ~ ";" ,
	
	'DeclareFunction := 'type ~ 'IDENTIFIERS ~ 'LPS ~ ('type ~ 'IDENTIFIERS ~ ("," ~ 'type ~ 'IDENTIFIERS).rep(0,Int.MaxValue) ).? ~ 'RPS ~ 'StatementBlock ,
	'StatementBlock := 'LBRACE ~ 'Statement.rep(0,Int.MaxValue) ~ 'RBRACE ,
	
	'expression := 'polynomial ~ ('EQ ~ 'polynomial 
								| 'NEQ ~ 'polynomial 
								| 'GT ~ 'polynomial
								| 'GET ~ 'polynomial
								| 'LT ~ 'polynomial
								| 'LET ~ 'polynomial).? ,
	'polynomial := 'term ~ (('PLUS|'MINUS) ~ 'polynomial).? ,
	'term := 'factor ~ (('DIV|'MUL) ~ 'term).? ,
	'factor := 'constant 
			| 'LPS ~ 'expression ~ 'RPS 
			| 'PLUS ~ 'term 
			| 'MINUS ~ 'term 
			| 'IDENTIFIERS ~ ('LPS ~ ('expression ~ ("," ~ 'expression).rep(0,Int.MaxValue) ).? ~ 'RPS ).? ,
	'constant := 'INTEGER_LITERAL | 'REAL_LITERAL | 'STRING ,
	'FunctionCall := 'IDENTIFIERS ~ 'LPS ~ ('expression ~ ("," ~ 'expression).rep(0,Int.MaxValue) ).? ~ 'RPS ,
	'Return := 'RETURN ~ 'expression ~ ";" ,
)