import saarland.cispa.se.tribble.dsl._


Grammar(
	'start := 'form,
	'form  := 'pred
           | 'pred ~ 'terms
           | "!" ~ 'form
           | "(" ~ 'form ~ "&" ~ 'form ~ ")"
           | "(" ~ 'form ~ "|" ~ 'form ~ ")"
           | "(" ~ 'form ~ "->" ~ 'form ~ ")"
           | "exists " ~ 'var ~ "." ~ 'form
           | "forall " ~ 'var ~ "." ~ 'form,

	'term  := 'var | 'fun | 'fun ~ 'terms,
  
	'terms := "(" ~ 'term ~ ")" 
		   | "(" ~ 'term ~ "," ~ 'terms ~ ")",
		   	   
	'var := 'upper ~ 'inner.rep,
	
	'fun := 'lower ~ 'inner.rep,
	
	'pred := 'lower ~ 'inner.rep,
	
	'lower := "[a-z]".regex,
	'upper := "[A-Z]".regex,
	'digit := "[0-9]".regex,
	'under := "_",
	'inner := 'upper 
			| 'under
			| 'lower 
			| 'digit,			
)
