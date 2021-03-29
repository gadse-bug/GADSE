import saarland.cispa.se.tribble.dsl._

Grammar(
	'start := 'file ,
	'ALPHA_CHAR := "[A-Z]".regex | "[a-z]".regex ,
	'NUM_CHAR := "[0-9]".regex ,
	//'ALPHANUM_CHAR := "[A-Za-z0-9]".regex ,
	'IDENTIFIER_CHAR := 'ALPHA_CHAR | 'NUM_CHAR | "_" | "-" | "." | ":" ,
	'IDENTIFIER := 'ALPHA_CHAR ~ 'IDENTIFIER_CHAR.rep(0,Int.MaxValue) ,
	
	//QUOTED_STRING_NB :("'" (~[ "'", "\r", "\n" ])* "'")|("\"" (~[ "\"", "\r", "\n" ])* "\"")
	//'QUOTED_STRING_NB := "'" ~ ("[^\\n\\r']".regex).rep(0,Int.MaxValue) ~ "'"
	//					|"\"" ~ ("[^\\n\\r\\\"]".regex).rep(0,Int.MaxValue) ~ "\"" ,
	'QUOTED_STRING_NB := "'" ~ 'IDENTIFIER_CHAR.rep(0,Int.MaxValue) ~ "'"
						|"\"" ~ 'IDENTIFIER_CHAR.rep(0,Int.MaxValue) ~ "\"" ,
	
	//QUOTED_STRING :("'" (~[ "'" ])* "'")  |("\"" (~[ "\"" ])* "\"")
	//'QUOTED_STRING := "'" ~ ("[^']".regex).rep(0,Int.MaxValue) ~ "'" 
	//					|"\"" ~ ("[^\\\"]".regex).rep(0,Int.MaxValue) ~ "\"" ,
	'QUOTED_STRING :=    "'" ~ 'IDENTIFIER_CHAR.rep(0,Int.MaxValue) ~ "'" 
						|"\"" ~ 'IDENTIFIER_CHAR.rep(0,Int.MaxValue) ~ "\"" ,
	
	'WHITESPACE := " "| "\t"| "\n"| "\r" ,
	'NEWLINE := "\r\n"| "\r"| "\n" ,
	'QUOTE := "'"| "\"" ,
    
	//DEFAULT :
	//< EOL :(" "| "\t")*< NEWLINE > >
	//| < TAG_START : "<" > : LexStartTag
	//| < ENDTAG_START : "</" > : LexStartTag
	//| < COMMENT_START : "<!--" > : LexComment
	//| < DECL_START : "<!" > : LexDecl
	//| < TEXT : (~[ "<", "\r", "\n" ])+ >
	
	'DEFAULT := 'EOL | 'TAG_START ~ 'LexStartTag | 'ENDTAG_START ~ 'LexStartTag 
			| 'COMMENT_START ~ 'LexComment | 'DECL_START ~ 'LexDecl | 'TEXT ,
	'EOL := (" "|"\t").rep(0,Int.MaxValue) ~ 'NEWLINE ,
	'TAG_START := "<" ,
	'ENDTAG_START := "</" ,
	'COMMENT_START := "<!--" ,
	'DECL_START := "<!" ,
	//'TEXT := ("[^\\n\\r<]".regex).rep(1,Int.MaxValue) ,
	'TEXT := ("[A-Za-z0-9]".regex).rep(1,Int.MaxValue) ,
	
	'LexStartTag := 'TAG_NAME ~ 'LexInTag ,
	'TAG_NAME := " " ~ 'IDENTIFIER ~ " " ,
	
	//LexInTag :
	//< ATTR_NAME : < IDENTIFIER > >
	//| < TAG_END : ">" > : DEFAULT
	//| < TAG_SLASHEND : "/>" > : DEFAULT
	//| < ATTR_EQ : "=" > : LexAttrVal
	
	'LexInTag := 'WHITESPACE.rep(1,Int.MaxValue) 
			| 'ATTR_NAME
			| 'TAG_END ~ 'DEFAULT
			| 'TAG_SLASHEND ~ 'DEFAULT
			| 'ATTR_EQ ~ 'LexAttrVal ,

	'ATTR_NAME := " " ~ 'IDENTIFIER ~ " " ,
	'TAG_END := ">" ,
	'TAG_SLASHEND := "/>" ,
	'ATTR_EQ := "=" ,
	
	//< ATTR_VAL : < QUOTED_STRING > | (~[ ">", "\"", "'", " ", "\t", "\n", "\r" ])+ > : LexInTag
	
	'LexAttrVal := 'WHITESPACE | 'ATTR_VAL ~ 'LexInTag ,
	//'ATTR_VAL := 'QUOTED_STRING | ("[^\\t\\n\\r\\\">' ]".regex).rep(1,Int.MaxValue) ,
	'ATTR_VAL := 'QUOTED_STRING  ,
	
	//< COMMENT_END :("--" (" ")* ">" | "->") >: DEFAULT
	//| < DASH : "-" >
	//| < COMMENT_EOL : < NEWLINE > >
	//| < COMMENT_WORD :( (~[ "\n", "\r", "'", "\"", "-" ])+
	//		| < QUOTED_STRING_NB >
	//		| < QUOTE >
	//		) >
	'LexComment := 'COMMENT_END ~ 'DEFAULT
			| 'DASH
			| 'COMMENT_EOL
			| 'COMMENT_WORD ,
	'COMMENT_END :=  " -- " ~ " ".rep(0,Int.MaxValue) ~ ">" | "->" ,
	'DASH := " - " ,
	'COMMENT_EOL := 'NEWLINE ,
	'COMMENT_WORD := ("[^\\n\\r\\\"-']".regex).rep(1,Int.MaxValue)
			| 'QUOTED_STRING_NB
			| 'QUOTE ,
	
	//< DECL_ANY :(< QUOTED_STRING_NB >
	//				| < QUOTE >
	//				| ~[ ">" ]
	//				)+ >
	//| < DECL_END : ">" > : DEFAULT
	'LexDecl := 'DECL_ANY | 'DECL_END ~ 'DEFAULT ,
	'DECL_ANY := ('QUOTED_STRING_NB | 'QUOTE | "[^>]".regex).rep(1,Int.MaxValue) ,
	'DECL_END := ">" ,
	
	//'file := 'elementSequence ~ 'EOF ,
	'file := 'elementSequence ,
	'elementSequence := 'element.rep(0,Int.MaxValue) ,
	'element :=  'tag | 'endTag | 'comment | 'decl | 'text | 'EOL ,
	
	'tag := 'TAG_START ~ 'TAG_NAME ~ 'attribute ~ ('TAG_END|'TAG_SLASHEND) ,
	'endTag := 'ENDTAG_START ~ 'TAG_NAME ~ 'TAG_END ,
	'attribute := 'ATTR_NAME ~ 'attribute_value ,
	'attribute_value := 'ATTR_EQ ~ 'ATTR_VAL ,
	'text := 'TEXT ,
	//'comment := 'COMMENT_START ~ ('DASH |'COMMENT_EOL|'COMMENT_WORD).rep(0,Int.MaxValue) ~ ('EOF|'COMMENT_END) ,
	'comment := 'COMMENT_START ~ ('DASH |'COMMENT_EOL|'COMMENT_WORD).rep(0,Int.MaxValue) ~ 'COMMENT_END ,
	'decl := 'DECL_START ~ 'DECL_ANY ~ 'DECL_END ,
)