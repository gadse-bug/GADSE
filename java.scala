import saarland.cispa.se.tribble.dsl._

// https://github.com/javaparser/javaparser java.jj

Grammar(
	'start := 'CompilationUnit ,
	
	//modified by zgf
	'StringLiteral := "\"" ~ 'StringCharacter.rep(0,Int.MaxValue) ~ "\"" ,
	'StringCharacter:= "[a-zA-Z0-9]".regex ,
	'CharacterLiteral:= "'" ~ 'SingleCharacter ~ "'" ,
	'SingleCharacter:= "[a-zA-Z0-9]".regex ,
	
	'IntegerLiteral:= 'DecimalIntegerLiteral 
				| 'HexIntegerLiteral
				| 'OctalIntegerLiteral
				| 'BinaryIntegerLiteral ,
	'LongLiteral:= 'DecimalIntegerLiteral ~ ("l"|"L") 
				|  'HexIntegerLiteral ~ ("l"|"L") 
				|  'OctalIntegerLiteral ~ ("l"|"L") 
				|  'BinaryIntegerLiteral ~ ("l"|"L") ,
	'IntegerTypeSuffix:= "l"|"L" ,
	
	'DecimalIntegerLiteral:= 'DecimalNumeral ~ 'IntegerTypeSuffix.? ,
	'DecimalNumeral := "0" 
			| 'NonZeroDigit ~ 'Digits.?
			| 'NonZeroDigit ~ 'Underscores ~ 'Digits ,
	'NonZeroDigit:= "[1-9]".regex ,
	'Digits:= 'Digit | 'Digit ~ 'DigitsAndUnderscores.? ~ 'Digit ,
	'Digit:= "0"| 'NonZeroDigit ,
	'DigitsAndUnderscores:= 'DigitOrUnderscore ~ 'DigitOrUnderscore.rep(0,Int.MaxValue) ,
	'DigitOrUnderscore:= 'Digit| "_" ,
	
	'HexIntegerLiteral := 'HexNumeral ~ 'IntegerTypeSuffix.? ,
	'HexNumeral:= ("0x"|"0X") ~ 'HexDigits ,
	'HexDigits:= 'HexDigit | 'HexDigit ~ 'HexDigitsAndUnderscores.? ~ 'HexDigit ,
	'HexDigit := "[0-9a-fA-F]".regex ,
	'HexDigitsAndUnderscores:= 'HexDigitOrUnderscore ~ 'HexDigitOrUnderscore.rep(0,Int.MaxValue) ,
	'HexDigitOrUnderscore:= 'HexDigit| "_" ,
	
	'OctalIntegerLiteral:= 'OctalNumeral ~ 'IntegerTypeSuffix.? ,
	'OctalNumeral:= "0" ~ 'Underscores.? ~ 'OctalDigits ,
	'OctalDigits := 'OctalDigit | 'OctalDigit ~ 'OctalDigitsAndUnderscores.? ~ 'OctalDigit ,
	'OctalDigit:= "[0-7]".regex ,
	'OctalDigitsAndUnderscores:= 'OctalDigitOrUnderscore ~ 'OctalDigitOrUnderscore.rep(0,Int.MaxValue) ,
	'OctalDigitOrUnderscore:= 'OctalDigit | "_" ,
	
	'BinaryIntegerLiteral:= 'BinaryNumeral ~ 'IntegerTypeSuffix.? ,
	'BinaryNumeral:= ("0b"|"0B") ~ 'BinaryDigits ,
	'BinaryDigits:= 'BinaryDigit | 'BinaryDigit ~ 'BinaryDigitsAndUnderscores.? ~ 'BinaryDigit ,
	'BinaryDigit:= "0" | "1" ,
	'BinaryDigitsAndUnderscores := 'BinaryDigitOrUnderscore ~ 'BinaryDigitOrUnderscore.rep(0,Int.MaxValue) ,
	'BinaryDigitOrUnderscore:= 'BinaryDigit | "_" ,
	
	'FloatingPointLiteral:= 'DecimalFloatingPointLiteral | 'HexadecimalFloatingPointLiteral ,
	'DecimalFloatingPointLiteral:=
			 'Digits ~ "." ~ 'Digits.? ~ 'ExponentPart.? ~ 'FloatTypeSuffix.? 
			|"." ~ 'Digits.? ~ 'ExponentPart.? ~ 'FloatTypeSuffix.? 
			|'Digits ~ 'ExponentPart ~ 'FloatTypeSuffix.? 
			|'Digits ~ 'ExponentPart.? ~ 'FloatTypeSuffix ,
	'ExponentPart:= 'ExponentIndicator ~ 'SignedInteger ,
	'ExponentIndicator:= "e"|"E" ,
	'SignedInteger:= 'Sign.? ~ 'Digits ,
	'Sign:= "+" | "-" ,
	'FloatTypeSuffix:= "f"|"F"|"d"|"D" ,
	
	'HexadecimalFloatingPointLiteral:= 'HexSignificand ~ 'BinaryExponent ~ 'FloatTypeSuffix.? ,
	'HexSignificand:= 'HexNumeral ~ ".".?
			|("0x"|"0X") ~ 'HexDigits.? ~ "." ~ 'HexDigits ,
	'BinaryExponent:= 'BinaryExponentIndicator ~ 'SignedInteger ,
	'BinaryExponentIndicator:= "p"|"P" ,
	
	'Underscores := "_" ,
	
///////// some defined by zgf , maybe something error /////////
	'Name := 'SingleCharacter.rep(0,Int.MaxValue) ,
	'TypeName := 'Name ~ ("." ~ 'Name).rep(0,Int.MaxValue) ,
	'ExpressionName := 'Name ,
	'MethodName := 'Name ,
	'ModuleName := 'Name ~ ("." ~ 'Name).rep(0,Int.MaxValue) ,
	'PackageName := 'Name ~ ("." ~ 'Name).rep(0,Int.MaxValue) ,
	'PackageOrTypeName := 'PackageName | 'TypeName ,
	
	'ArrayAccess := "[" ~ 'Expression ~ "]" ,
	'InterfaceModifier := 'ConstructorModifier ,
	
	'PostIncrementExpression := 'ExpressionName ~ "++;" ,
	'PostDecrementExpression := 'ExpressionName ~ "--;" ,
	'ExpressionStatement := 'Expression ~ 'Statement ,
	
	'LocalVariableDeclaration := 'TypeName ~ 'Assignment ~ ";" ,
	'LocalVariableDeclarationStatement := 'TypeName ~ 'Assignment ~ 'Statement ,
	'ConstantDeclaration := " final " ~ 'LocalVariableDeclaration ~ ";" ,
	'MethodBody := 'BlockStatement ,
	
	'FieldAccess := 'TypeName ~ "." ~ 'Name ,
	'UnannType := 'Type ,
	'UnannClassType := 'ClassType ,
	'ExtendsInterfaces := " extends " ~ 'InterfaceType ,
	'TypeIdentifier := 'Identifier ,

///////// THE JAVA LANGUAGE GRAMMAR STARTS HERE ///////////////   	

//	   CompilationUnit:
//         OrdinaryCompilationUnit
//         ModularCompilationUnit
//     OrdinaryCompilationUnit:
//         [PackageDeclaration] {ImportDeclaration} {TypeDeclaration}
//     ModularCompilationUnit:
//         {ImportDeclaration} ModuleDeclaration

	'CompilationUnit:= 'OrdinaryCompilationUnit | 'ModularCompilationUnit ,
	'OrdinaryCompilationUnit := 'PackageDeclaration.? ~ 'ImportDeclaration.rep(0,Int.MaxValue) ~ 'TypeDeclaration.rep(0,Int.MaxValue) ,
	'ModularCompilationUnit := 'ImportDeclaration.rep(0,Int.MaxValue) ~ 'ModuleDeclaration ,
	
//     TypeDeclaration:
//         ClassDeclaration
//         InterfaceDeclaration
//         ;	
//     InterfaceDeclaration:
//         NormalInterfaceDeclaration
//         AnnotationTypeDeclaration	
	'TypeDeclaration := 'ClassDeclaration | 'InterfaceDeclaration | ";" ,
	'InterfaceDeclaration := 'NormalInterfaceDeclaration | 'AnnotationTypeDeclaration ,

//     PackageDeclaration:
//         {PackageModifier} package Identifier {. Identifier} ;
//     PackageModifier:
//         Annotation

	'PackageDeclaration := 'PackageModifier.rep(0,Int.MaxValue) ~ " package " ~ 'Identifier ~ ("." ~ 'Identifier).rep(0,Int.MaxValue) ~ ";" ,
	'PackageModifier := 'Annotation ,

//     ImportDeclaration:
//         SingleTypeImportDeclaration
//         TypeImportOnDemandDeclaration
//         SingleStaticImportDeclaration
//         StaticImportOnDemandDeclaration
//     SingleTypeImportDeclaration:
//         "import " TypeName ;
//     TypeImportOnDemandDeclaration:
//         "import " PackageOrTypeName . * ;
//     SingleStaticImportDeclaration:
//         "import " static TypeName . Identifier ;
//     StaticImportOnDemandDeclaration:
//         "import " static TypeName . * ;
	
	'ImportDeclaration := 'SingleTypeImportDeclaration 
					| 'TypeImportOnDemandDeclaration
					| 'SingleStaticImportDeclaration
					| 'StaticImportOnDemandDeclaration ,
	'SingleTypeImportDeclaration := " import " ~ 'TypeName ~ ";" ,
	'TypeImportOnDemandDeclaration := " import " ~ 'PackageOrTypeName ~ ".*;" ,
	'SingleStaticImportDeclaration := " import " ~ " static " ~ 'TypeName ~ "." ~ 'Identifier ~ ";" ,
	'StaticImportOnDemandDeclaration := " import " ~ " static " ~ 'TypeName ~ ".*;" ,
	
//     ClassModifier:
//         (one of)
//         Annotation public protected private
//         abstract static final strictfp
//     FieldModifier:
//        (one of)
//         Annotation public protected private
//         static final transient volatile
//     MethodModifier:
//         (one of)
//         Annotation public protected private
//         abstract static final synchronized native strictfp
//     ConstructorModifier:
//         (one of)
//         Annotation public protected private

	'ClassModifier:= 'Annotation | " public " | " protected " | " private "
					|" private " | " static " | " final " | " strictfp " ,
	'FieldModifier:= 'Annotation | " public " | " protected " | " private "
					|" static " | " final " | " transient " | " volatile " ,
	'MethodModifier:='Annotation | " public " | " protected " | " private "
					|" abstract " | " static " | " final " | " synchronized " | " native " | " strictfp " ,
	'ConstructorModifier := 'Annotation | " public " | " protected " | " private " ,
	
//     ClassDeclaration:
//         NormalClassDeclaration
//         EnumDeclaration
//     NormalClassDeclaration:
//         {ClassModifier} class TypeIdentifier [TypeParameters] [Superclass] [Superinterfaces] ClassBody	
//     NormalInterfaceDeclaration:
//         {InterfaceModifier} interface TypeIdentifier [TypeParameters] [ExtendsInterfaces] InterfaceBody
//     Superclass:
//         extends ClassType
	'ClassDeclaration:= 'NormalClassDeclaration | 'EnumDeclaration ,
	'NormalClassDeclaration := 'ClassModifier.rep(0,Int.MaxValue) ~ " class " ~
			'TypeIdentifier ~ 'TypeParameters.? ~ 'Superclass.? ~ 'Superinterfaces.? ~ 'ClassBody ,		
	'NormalInterfaceDeclaration := 'InterfaceModifier.rep(0,Int.MaxValue) ~ " interface " ~
			'TypeIdentifier ~ 'TypeParameters.? ~ 'ExtendsInterfaces.? ~ 'InterfaceBody ,
	'Superclass := " extends " ~ 'ClassType ,
	
//     ClassOrInterfaceType:
//         ClassType
//         InterfaceType
//     ClassType:
//         {Annotation} TypeIdentifier [TypeArguments]
//         PackageName . {Annotation} TypeIdentifier [TypeArguments]
//         ClassOrInterfaceType . {Annotation} TypeIdentifier [TypeArguments]
//     InterfaceType:
//         ClassType
	'ClassOrInterfaceType:= 'ClassType |'InterfaceType ,
	'ClassType := 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeArguments.?
				| 'PackageName ~ "." ~ 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeArguments.?
				| 'ClassOrInterfaceType ~ "." ~ 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeArguments.? ,
	'InterfaceType := 'ClassType ,

//     Superinterfaces:
//         implements InterfaceTypeList
//     InterfaceTypeList:
//         InterfaceType {, InterfaceType}
	'Superinterfaces := " implements " ~ 'InterfaceTypeList ,
	'InterfaceTypeList := 'InterfaceType ~ ("," ~ 'InterfaceType).rep(0,Int.MaxValue) ,

//     EnumDeclaration:
//         {ClassModifier} enum TypeIdentifier [Superinterfaces] EnumBody
//     EnumBody:
//         { [EnumConstantList] [,] [EnumBodyDeclarations] }
//     EnumConstantList:
//         EnumConstant {, EnumConstant}
//     EnumConstant:
//         {EnumConstantModifier} Identifier [( [ArgumentList] )] [ClassBody]
//     EnumConstantModifier:
//         Annotation
//     EnumBodyDeclarations:
//         ; {ClassBodyDeclaration}
	'EnumDeclaration := 'ClassModifier.rep(0,Int.MaxValue) ~ " enum " ~ 'TypeIdentifier ~ 'Superinterfaces.? ~ 'EnumBody ,
	'EnumBody := "{" ~ 'EnumConstantList.? ~ ",".? ~ 'EnumBodyDeclarations.? ~ "}" ,
	'EnumConstantList := 'EnumConstant ~ ("," ~ 'EnumConstant).rep(0,Int.MaxValue) ,
	'EnumConstant := 'EnumConstantModifier.rep(0,Int.MaxValue) ~ 'Identifier ~ "[(" ~ 'ArgumentList.? ~ ")]" ~ 'ClassBody.? ,
	'EnumConstantModifier:= 'Annotation ,
	'EnumBodyDeclarations := ";" ~ 'ClassBodyDeclaration.rep(0,Int.MaxValue) ,

//     TypeParameters:
//         < TypeParameterList >
//     TypeParameterList:
//         TypeParameter {, TypeParameter}
//     TypeParameter:
//         {TypeParameterModifier} TypeIdentifier [TypeBound]
//     TypeParameterModifier:
//         Annotation
//     TypeBound:
//         extends TypeVariable
//         extends ClassOrInterfaceType {AdditionalBound}
//     AdditionalBound:
//         & InterfaceType
//     InterfaceType:
//         ClassType
//     TypeVariable:
//         {Annotation} TypeIdentifier
	'TypeParameters := "<" ~ 'TypeParameterList ~ ">" ,
	'TypeParameterList := 'TypeParameter ~ ("," ~ 'TypeParameter).rep(0,Int.MaxValue) ,
	'TypeParameter:= 'TypeParameterModifier.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeBound.? ,
	'TypeParameterModifier:= 'Annotation ,
	'TypeBound:= " extends " ~ 'TypeVariable 
				|" extends " ~ 'ClassOrInterfaceType ~ 'AdditionalBound.rep(0,Int.MaxValue) ,
	'AdditionalBound:= "&" ~ 'InterfaceType ,
	'TypeVariable := 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ,

//     ClassBody:
//         { {ClassBodyDeclaration} }
//     ClassBodyDeclaration:
//         ClassMemberDeclaration
//         InstanceInitializer
//         StaticInitializer
//         ConstructorDeclaration
//     ClassMemberDeclaration:
//         FieldDeclaration
//         MethodDeclaration
//         ClassDeclaration
//         InterfaceDeclaration
//         ;
//     InterfaceBody:
//         { {InterfaceMemberDeclaration} }
//     InterfaceMemberDeclaration:
//         ConstantDeclaration
//         InterfaceMethodDeclaration
//         ClassDeclaration
//         InterfaceDeclaration
//         ;
	'ClassBody:= "{" ~ 'ClassBodyDeclaration.rep(0,Int.MaxValue) ~ "}",
	'ClassBodyDeclaration:=  'ClassMemberDeclaration
							|'InstanceInitializer
							|'StaticInitializer
							|'ConstructorDeclaration ,
	'ClassMemberDeclaration:= 'FieldDeclaration
							| 'MethodDeclaration
							| 'ClassDeclaration
							| 'InterfaceDeclaration
							| ";" ,
	
	'InterfaceBody:= "{" ~ 'InterfaceMemberDeclaration.rep(0,Int.MaxValue) ~ "}" ,
	'InterfaceMemberDeclaration:= 'ConstantDeclaration
								//| 'InterfaceMethodDeclaration
								| 'ClassDeclaration
								| 'InterfaceDeclaration
								| ";" ,

//     FieldDeclaration:
//         {FieldModifier} UnannType VariableDeclaratorList ;
//     VariableDeclaratorList:
//         VariableDeclarator {, VariableDeclarator}
//     VariableDeclarator:
//         VariableDeclaratorId [= VariableInitializer]
//     VariableDeclaratorId:
//         Identifier [Dims]
//     VariableInitializer:
//         Expression
//         ArrayInitializer
	
	'FieldDeclaration:= 'FieldModifier.rep(0,Int.MaxValue) ~ 'UnannType ~ 'VariableDeclaratorList ~ ";" ,
	'VariableDeclaratorList:= 'VariableDeclarator ~ ("," ~ 'VariableDeclarator).rep(0,Int.MaxValue) ,
	'VariableDeclarator := 'VariableDeclaratorId ~ ("=" ~  'VariableInitializer).? ,
	'VariableDeclaratorId:= 'Identifier ~ 'Dims.? ,
	'VariableInitializer := 'Expression | 'ArrayInitializer ,

//     Identifier:
//         IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
//     IdentifierChars:
//         JavaLetter {JavaLetterOrDigit}
//     JavaLetter:
//         any Unicode character that is a "Java letter"
//     JavaLetterOrDigit:
//         any Unicode character that is a "Java letter-or-digit"
//     Dims:
//         {Annotation} [ ] {{Annotation} [ ]}

	// modified by zgf ,don't use unicode char instead of a-zA-Z0-9
	'Identifier := 'IdentifierChars ,
	'IdentifierChars:= 'JavaLetter ~ 'JavaLetterOrDigit.rep(0,Int.MaxValue) ,
	'JavaLetter := "[a-zA-Z]".regex ,
	'JavaLetterOrDigit:= "[0-9a-zA-Z]".regex , 
	'Dims := 'Annotation.rep(0,Int.MaxValue) ~ "[ ]" ~ ('Annotation.rep(0,Int.MaxValue) ~ "[ ]").rep(0,Int.MaxValue) ,

//     ArrayInitializer:
//         { [VariableInitializerList] [,] }
//     VariableInitializerList:
//         VariableInitializer {, VariableInitializer}
//     VariableInitializer:
//         Expression
//         ArrayInitializer
	'ArrayInitializer:="{" ~ 'VariableInitializerList.? ~ ",".? ~ "}" ,
	'VariableInitializerList:= 'VariableInitializer ~ ("," ~ 'VariableInitializer).rep(0,Int.MaxValue) ,
	
//     MethodDeclaration:
//         {MethodModifier} MethodHeader MethodBody
//     MethodHeader:
//         Result MethodDeclarator [Throws]
//         TypeParameters {Annotation} Result MethodDeclarator [Throws]
//     MethodDeclarator:
//         Identifier ( [ReceiverParameter ,] [FormalParameterList] ) [Dims]
//     ReceiverParameter:
//         {Annotation} UnannType [Identifier .] this	
	
	'MethodDeclaration:= 'MethodModifier.rep(0,Int.MaxValue) ~ 'MethodHeader ~ 'MethodBody ,
	'MethodHeader := 'Result ~ 'MethodDeclarator ~ " throws ".? 
					|'TypeParameters ~ 'Annotation.rep(0,Int.MaxValue) ~ 'Result ~ 'MethodDeclarator ~ " throws ".? ,
	'MethodDeclarator := 'Identifier ~ "(" ~ ('ReceiverParameter ~ ",").? ~ 'FormalParameterList.? ~ ")" ~ 'Dims ,
	'ReceiverParameter:= 'Annotation ~ 'UnannType ~ ('Identifier ~ ".").? ~ " this " ,

//     FormalParameterList:
//         FormalParameter {, FormalParameter}
//     FormalParameter:
//         {VariableModifier} UnannType VariableDeclaratorId
//         VariableArityParameter
//     VariableArityParameter:
//         {VariableModifier} UnannType {Annotation} ... Identifier
//     VariableModifier:
//         Annotation
//         final
	
	'FormalParameterList:= 'FormalParameter ~ ("," ~ 'FormalParameter).rep(0,Int.MaxValue) ,
	'FormalParameter :=  'VariableModifier.rep(0,Int.MaxValue) ~ 'UnannType ~ 'VariableDeclaratorId
						|'VariableArityParameter ,
	'VariableArityParameter:= 'VariableModifier.rep(0,Int.MaxValue) ~ 'UnannType ~ 'Annotation.rep(0,Int.MaxValue) ~ "..." ~ 'Identifier ,
	'VariableModifier:= 'Annotation | " final " ,
	
//     LambdaParameters:
//         ( [LambdaParameterList] )
//         Identifier
//     LambdaParameterList:
//         LambdaParameter {, LambdaParameter}
//         Identifier {, Identifier}
//     LambdaParameter:
//         {VariableModifier} LambdaParameterType VariableDeclaratorId
//         VariableArityParameter
//     LambdaParameterType:
//         UnannType
//         var

//	'LambdaParameters:= "(" ~ 'LambdaParameterList.? ~ ")"
//						| 'Identifier ,
//	'LambdaParameterList := 'LambdaParameter ~ ("," ~ 'LambdaParameter).rep(0,Int.MaxValue)
//						| 'Identifier ~ ("," ~ 'Identifier).rep(0,Int.MaxValue) ,
//	'LambdaParameter :=   'VariableModifier.rep(0,Int.MaxValue) ~ 'LambdaParameterType ~ 'VariableDeclaratorId
//						| 'VariableArityParameter ,
//	'LambdaParameterType:= 'UnannType | " var " ,

//     ConstructorDeclaration:
//         {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
//     ConstructorDeclarator:
//         [TypeParameters] SimpleTypeName ( [ReceiverParameter ,] [FormalParameterList] )
//     SimpleTypeName:
//         TypeIdentifier
//     ConstructorBody:
//         { [ExplicitConstructorInvocation] [BlockStatements] }
//     ExplicitConstructorInvocation:
//         [TypeArguments] this ( [ArgumentList] ) ;
//         [TypeArguments] super ( [ArgumentList] ) ;
//         ExpressionName . [TypeArguments] super ( [ArgumentList] ) ;
//         Primary . [TypeArguments] super ( [ArgumentList] ) ;
	'ConstructorDeclaration := 'ConstructorModifier.rep(0,Int.MaxValue) ~  'ConstructorDeclarator ~ " throws ".? ~ 'ConstructorBody ,
	'ConstructorDeclarator := 'TypeParameters.? ~ 'SimpleTypeName ~ "(" ~ ('ReceiverParameter ~ ",").? ~ 'FormalParameterList.? ~ ")" ,
	'SimpleTypeName := 'TypeIdentifier ,
	'ConstructorBody:= "{" ~ 'ExplicitConstructorInvocation.? ~ 'BlockStatements.? ~ "}" ,
	'ExplicitConstructorInvocation :=
			 'TypeArguments.? ~ " this " ~ "(" ~ 'ArgumentList.? ~ ")" ~ ";"
			|'TypeArguments.? ~ " super " ~ "(" ~ 'ArgumentList.? ~ ")" ~ ";"
			|'ExpressionName ~ "." ~ 'TypeArguments.? ~ " super " ~ "(" ~ 'ArgumentList.? ~ ")" ~ ";"
			|'Primary ~ "." ~ 'TypeArguments.? ~ " super " ~ "(" ~ 'ArgumentList.? ~ ")" ~ ";" ,
	
//     Statement:
//         StatementWithoutTrailingSubstatement
//         LabeledStatement
//         IfThenStatement
//         IfThenElseStatement
//         WhileStatement
//         ForStatement
//     StatementNoShortIf:
//         StatementWithoutTrailingSubstatement
//         LabeledStatementNoShortIf
//         IfThenElseStatementNoShortIf
//         WhileStatementNoShortIf
//         ForStatementNoShortIf
//     StatementWithoutTrailingSubstatement:
//         Block
//         EmptyStatement
//         ExpressionStatement
//         AssertStatement
//         SwitchStatement
//         DoStatement
//         BreakStatement
//         ContinueStatement
//         ReturnStatement
//         SynchronizedStatement
//         ThrowStatement
//         TryStatement
//         YieldStatement	
	'Statement := 'StatementWithoutTrailingSubstatement
				| 'LabeledStatement
				| 'IfThenStatement
				| 'IfThenElseStatement
				| 'WhileStatement
				| 'ForStatement ,
	'StatementNoShortIf:=
				  'StatementWithoutTrailingSubstatement
				| 'LabeledStatementNoShortIf
				| 'IfThenElseStatementNoShortIf
				| 'WhileStatementNoShortIf
				| 'ForStatementNoShortIf ,
	'StatementWithoutTrailingSubstatement :=
				  'Block
				| 'EmptyStatement
				| 'ExpressionStatement
				| 'AssertStatement
				| 'SwitchStatement
				| 'DoStatement
				| 'BreakStatement
				| 'ContinueStatement
				| 'ReturnStatement
				| 'SynchronizedStatement
				| 'ThrowStatement
				| 'TryStatement 
				| 'YieldStatement ,
//     InstanceInitializer:
//         Block
//     StaticInitializer:
//         static Block 
//     Type:
//         PrimitiveType
//         ReferenceType
//     ReferenceType:
//         ClassOrInterfaceType
//         TypeVariable
//         ArrayType
//     ArrayType:
//         PrimitiveType Dims
//         ClassOrInterfaceType Dims
//         TypeVariable Dims
	'InstanceInitializer:= 'Block ,
	'StaticInitializer:= " static " ~ 'Block ,
	'Type:= 'PrimitiveType | 'ReferenceType ,	
	'ReferenceType:=  'ClassOrInterfaceType
					| 'TypeVariable
					| 'ArrayType ,
	//'ClassOrInterfaceType :=
	//				| 'ClassType
	//				| 'InterfaceType ,
	//'ClassType:=    | 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeArguments.?
	//				| 'PackageName ~ "." ~ 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeArguments.?
	//				| 'ClassOrInterfaceType ~ "." ~ 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ~ 'TypeArguments.? ,
	//'InterfaceType:= 'ClassType ,
	//'TypeVariable := 'Annotation.rep(0,Int.MaxValue) ~ 'TypeIdentifier ,
	'ArrayType :=     'PrimitiveType ~ 'Dims
					| 'ClassOrInterfaceType ~ 'Dims
					| 'TypeVariable ~ 'Dims ,

//     TypeArguments:
//         < TypeArgumentList >
//     TypeArgumentList:
//         TypeArgument {, TypeArgument}
//     TypeArgument:
//         ReferenceType
//         Wildcard
//     Wildcard:
//         {Annotation} ? [WildcardBounds]
//     WildcardBounds:
//         extends ReferenceType
//         super ReferenceType
				
	'TypeArguments := "<" ~ 'TypeArgumentList ~ ">" ,
	'TypeArgumentList := 'TypeArgument ~ ("," ~ 'TypeArgument).rep(0,Int.MaxValue) ,
	'TypeArgument := 'ReferenceType | 'Wildcard	,	
	'Wildcard := 'Annotation.rep(0,Int.MaxValue) ~ "?" ~ 'WildcardBounds.? ,
	'WildcardBounds := " extends " ~ 'ReferenceType
					|  " super " ~ 'ReferenceType ,

//     PrimitiveType:
//         {Annotation} NumericType
//         {Annotation} boolean
//     NumericType:
//         IntegralType
//         FloatingPointType
//     IntegralType:
//         (one of)
//         byte short int long char
//     FloatingPointType:
//         (one of)
//         float double
//     Result:
//         UnannType
//         void			
	'PrimitiveType:= 'Annotation.rep(0,Int.MaxValue) ~ 'NumericType | 'Annotation.rep(0,Int.MaxValue) ~ " boolean " ,		
	'NumericType:= 'IntegralType | 'FloatingPointType ,
	'IntegralType:= " byte " | " short " | " int " | " long " | " char " ,
	'FloatingPointType := " float " | " double " ,
	'Result:= 'UnannType | " void " ,
	
//     Keyword:
//         (one of)
//
//         abstract   continue   for          new         switch
//         assert     default    if           package     synchronized
//         boolean    do         goto         private     this
//         break      double     implements   protected   throw
//         byte       else       import       public      throws
//         case       enum       instanceof   return      transient
//         catch      extends    int          short       try
//         char       final      interface    static      void
//         class      finally    long         strictfp    volatile
//         const      float      native       super       while
//         _ (underscore)
//	'Keyword := " abstract " |  " continue " |  " for "       |  " new "       |  " switch "
//			|	" assert "   |  " default "  |  " if "        |  " package "   |  " synchronized "
//			|	" boolean "  |  " do "       |  " goto "      |  " private "   |  " this "
//			|	" break "    |  " double "   |  " implements "|  " protected " |  " throw "
//			|	" byte "     |  " else "     |  " import "    |  " public "    |  " throws "
//			|	" case "     |  " enum "     | " instanceof " |  " return "    |  " transient "
//			|	" catch "    |  " extends "  |  " int "       |  " short "     |  " try "
//			|	" char "     |  " final "    |  " interface " |  " static "    |  " void "
//			|	" class "    |  " finally "  |  " long "      |  " strictfp "  |  " volatile "
//			|	" const "    | " float "     | " native "     | " super "      | " while " ,
	
	'Expression :=  'Assignment | 'ConditionalExpression ,

//     AssignmentExpression:
//         ConditionalExpression
//         Assignment
//     Assignment:
//         LeftHandSide AssignmentOperator Expression
//     LeftHandSide:
//         ExpressionName
//         FieldAccess
//         ArrayAccess
//     AssignmentOperator:
//         (one of)
//         =  *=  /=  %=  +=  -=  <<=  >>=  >>>=  &=  ^=  |=	
	//'AssignmentExpression:= 'ConditionalExpression | 'Assignment ,
	'Assignment:= 'LeftHandSide ~ 'AssignmentOperator ~ 'Expression ,
	'LeftHandSide:= 'ExpressionName | 'FieldAccess | 'ArrayAccess ,
	'AssignmentOperator := "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=" ,

//     ConditionalExpression:
//         ConditionalOrExpression
//         ConditionalOrExpression ? Expression : ConditionalExpression
//         ConditionalOrExpression ? Expression : LambdaExpression
//     ConditionalOrExpression:
//         ConditionalAndExpression
//         ConditionalOrExpression || ConditionalAndExpression	
//     ConditionalAndExpression:
//         InclusiveOrExpression
//         ConditionalAndExpression && InclusiveOrExpression
	'ConditionalExpression := 'ConditionalOrExpression
			| 'ConditionalOrExpression ~ "?" ~ 'Expression ~ ":" ~ 'ConditionalExpression ,
			//| 'ConditionalOrExpression ~ "?" ~ 'Expression ~ ":" ~ 'LambdaExpression ,			
	'ConditionalOrExpression:= 'ConditionalAndExpression
			| 'ConditionalOrExpression ~ "||" ~ 'ConditionalAndExpression ,
	'ConditionalAndExpression:= 'InclusiveOrExpression 
					| 'ConditionalAndExpression ~ "&&" ~ 'InclusiveOrExpression ,

//     AndExpression:
//         EqualityExpression
//         AndExpression & EqualityExpression
//     ExclusiveOrExpression:
//         AndExpression
//         ExclusiveOrExpression ^ AndExpression
//     InclusiveOrExpression:
//         ExclusiveOrExpression
//         InclusiveOrExpression | ExclusiveOrExpression
//     EqualityExpression:
//         RelationalExpression
//         EqualityExpression == RelationalExpression
//         EqualityExpression != RelationalExpression
	'AndExpression:= 'EqualityExpression
					|'AndExpression ~ "&" ~ 'EqualityExpression ,
	'ExclusiveOrExpression:= 'AndExpression
 					|'ExclusiveOrExpression ~ "^" ~ 'AndExpression ,
	'InclusiveOrExpression:= 'ExclusiveOrExpression
					|'InclusiveOrExpression ~ "|" ~ 'ExclusiveOrExpression ,
	'EqualityExpression:= 'RelationalExpression
					|'EqualityExpression ~ "==" ~ 'RelationalExpression
					|'EqualityExpression ~ "!=" ~ 'RelationalExpression ,

//     RelationalExpression:
//         ShiftExpression
//         RelationalExpression < ShiftExpression
//         RelationalExpression > ShiftExpression
//         RelationalExpression <= ShiftExpression
//         RelationalExpression >= ShiftExpression
//         RelationalExpression instanceof ReferenceType
//     RelationalExpression:
//         ...
//         RelationalExpression instanceof ReferenceType
//         RelationalExpression instanceof Pattern
//     Pattern:
//         ReferenceType Identifier
	'RelationalExpression:=
					 'ShiftExpression
					|'RelationalExpression ~ "<" ~ 'ShiftExpression
					|'RelationalExpression ~ ">" ~ 'ShiftExpression
					|'RelationalExpression ~ "<=" ~ 'ShiftExpression
					|'RelationalExpression ~ ">=" ~ 'ShiftExpression
					|'RelationalExpression ~ " instanceof " ~ 'ReferenceType 
					|"..."
					|'RelationalExpression ~ " instanceof " ~ 'Pattern  ,
	'Pattern := 'ReferenceType ~ 'Identifier ,

//     ShiftExpression:
//         AdditiveExpression
//         ShiftExpression << AdditiveExpression
//         ShiftExpression >> AdditiveExpression
//         ShiftExpression >>> AdditiveExpression
//     AdditiveExpression:
//         MultiplicativeExpression
//         AdditiveExpression + MultiplicativeExpression
//         AdditiveExpression - MultiplicativeExpression
//     MultiplicativeExpression:
//         UnaryExpression
//         MultiplicativeExpression * UnaryExpression
//         MultiplicativeExpression / UnaryExpression
//         MultiplicativeExpression % UnaryExpression
	'ShiftExpression:=
					 'AdditiveExpression
					|'ShiftExpression ~ "<<" ~ 'AdditiveExpression
					|'ShiftExpression ~ ">>" ~ 'AdditiveExpression
					|'ShiftExpression ~ ">>>" ~ 'AdditiveExpression ,
	'AdditiveExpression:=
					 'MultiplicativeExpression
					|'AdditiveExpression ~ "+" ~ 'MultiplicativeExpression
					|'AdditiveExpression ~ "-" ~ 'MultiplicativeExpression ,
	'MultiplicativeExpression:=
					 'UnaryExpression
					|'MultiplicativeExpression ~ "*" ~ 'UnaryExpression
					|'MultiplicativeExpression ~ "/" ~ 'UnaryExpression
					|'MultiplicativeExpression ~ "%" ~ 'UnaryExpression ,

//     UnaryExpression:
//         PreIncrementExpression
//         PreDecrementExpression
//         + UnaryExpression
//         - UnaryExpression
//         UnaryExpressionNotPlusMinus
//     PreIncrementExpression:
//         ++ UnaryExpression
//     PreDecrementExpression:
//         -- UnaryExpression
//     UnaryExpressionNotPlusMinus:
//         PostfixExpression
//         ~ UnaryExpression
//         ! UnaryExpression
//         CastExpression
//         SwitchExpression

	'UnaryExpression:=
					 'PreIncrementExpression
					|'PreDecrementExpression
					|"+" ~ 'UnaryExpression
					|"-" ~ 'UnaryExpression
					|'UnaryExpressionNotPlusMinus ,
	'PreIncrementExpression:= "++" ~ 'UnaryExpression ,
	'PreDecrementExpression:= "--" ~ 'UnaryExpression ,
	'UnaryExpressionNotPlusMinus:=
					 'PostfixExpression
					|"~" ~ 'UnaryExpression
					|"!" ~ 'UnaryExpression
					|'CastExpression
					|'SwitchExpression ,
					
//     PostfixExpression:
//         Primary
//         ExpressionName
//         PostIncrementExpression
//         PostDecrementExpression
//     CastExpression:
//         ( PrimitiveType ) UnaryExpression
//         ( ReferenceType {AdditionalBound} ) UnaryExpressionNotPlusMinus
//         ( ReferenceType {AdditionalBound} ) LambdaExpression
 
	'PostfixExpression:=
					 'Primary
					|'ExpressionName
					|'PostIncrementExpression
					|'PostDecrementExpression ,
	'CastExpression:=
					 "(" ~ 'PrimitiveType ~ ")" ~ 'UnaryExpression
					|"(" ~ 'ReferenceType ~ 'AdditionalBound.rep(0,Int.MaxValue) ~ ")" ~ 'UnaryExpressionNotPlusMinus ,
					//|"(" ~ 'ReferenceType ~ 'AdditionalBound.rep(0,Int.MaxValue) ~ ")" ~ 'LambdaExpression ,
					
//     Primary:
//         PrimaryNoNewArray
//         ArrayCreationExpression
//     PrimaryNoNewArray:
//         Literal
//         ClassLiteral
//         this
//         TypeName . this
//         ( Expression )
//         ClassInstanceCreationExpression
//         FieldAccess
//         ArrayAccess
//         MethodInvocation
//         MethodReference	
				
	'Primary:=   'PrimaryNoNewArray
				|'ArrayCreationExpression ,
	'PrimaryNoNewArray:=
				 'Literal
				//|'ClassLiteral
				|" this "
				|'TypeName ~ ".this"
				|"(" ~ 'Expression ~ ")"
				|'ClassInstanceCreationExpression
				|'FieldAccess
				|'ArrayAccess 
				|'MethodInvocation ,
				//|'MethodReference ,
				
//     Literal:
//         IntegerLiteral
//         FloatingPointLiteral
//         BooleanLiteral
//         CharacterLiteral
//         StringLiteral
//         TextBlock
//         NullLiteral
//     BooleanLiteral:
//         (one of)
//         true false
//     NullLiteral:
//         null
	'Literal:=	 'IntegerLiteral
				|'LongLiteral
				|'FloatingPointLiteral
				|'BooleanLiteral
				|'CharacterLiteral
				|'StringLiteral
				//|'TextBlock
				|'NullLiteral ,			
	'BooleanLiteral:= " true " | " false " ,			
	'NullLiteral   := " null " ,

//     MethodInvocation:
//         MethodName ( [ArgumentList] )
//         TypeName . [TypeArguments] Identifier ( [ArgumentList] )
//         ExpressionName . [TypeArguments] Identifier ( [ArgumentList] )
//         Primary . [TypeArguments] Identifier ( [ArgumentList] )
//         super . [TypeArguments] Identifier ( [ArgumentList] )
//         TypeName . super . [TypeArguments] Identifier ( [ArgumentList] )
//     ArgumentList:
//         Expression {, Expression}
	
	'MethodInvocation:=  'MethodName ~ "(" ~ 'ArgumentList.? ~ ")"
						|'TypeName ~ "." ~ 'TypeArguments.? ~ 'Identifier ~ "(" ~ 'ArgumentList.? ~ ")" 
						|'ExpressionName ~ "." ~  'TypeArguments.? ~ 'Identifier ~ "(" ~ 'ArgumentList.? ~ ")"
						|'Primary ~ "." ~  'TypeArguments.? ~ 'Identifier ~ "(" ~ 'ArgumentList.? ~ ")"
						|" super " ~ "." ~  'TypeArguments.? ~ 'Identifier ~ "(" ~ 'ArgumentList.? ~ ")"
						|'TypeName ~ "." ~ " super " ~ "." ~  'TypeArguments.? ~ 'Identifier ~ "(" ~ 'ArgumentList.? ~ ")" ,
	'ArgumentList:= 'Expression ~ ("," ~ 'Expression).rep(0,Int.MaxValue) ,
	
//     ClassInstanceCreationExpression:
//         UnqualifiedClassInstanceCreationExpression
//         ExpressionName . UnqualifiedClassInstanceCreationExpression
//         Primary . UnqualifiedClassInstanceCreationExpression
//     UnqualifiedClassInstanceCreationExpression:
//         new [TypeArguments] ClassOrInterfaceTypeToInstantiate ( [ArgumentList] ) [ClassBody]
//     ClassOrInterfaceTypeToInstantiate:
//         {Annotation} Identifier {. {Annotation} Identifier} [TypeArgumentsOrDiamond]
//     TypeArgumentsOrDiamond:
//         TypeArguments
//         <>	
	
	'ClassInstanceCreationExpression:=
			 'UnqualifiedClassInstanceCreationExpression
			|'ExpressionName ~ "." ~ 'UnqualifiedClassInstanceCreationExpression
			|'Primary  ~ "." ~ 'UnqualifiedClassInstanceCreationExpression ,
	'UnqualifiedClassInstanceCreationExpression:=
			" new " ~ 'TypeArguments.? ~ 'ClassOrInterfaceTypeToInstantiate ~ "(" ~ 'ArgumentList.? ~ ")" ~ 'ClassBody.? ,
	'ClassOrInterfaceTypeToInstantiate:=
			'Annotation.rep(0,Int.MaxValue) ~ 'Identifier ~ ("." ~ 'Annotation.rep(0,Int.MaxValue) ~ 'Identifier).rep(0,Int.MaxValue) ~ 'TypeArgumentsOrDiamond.? ,
	'TypeArgumentsOrDiamond:= 'TypeArguments | "<>" ,

//     ArrayCreationExpression:
//         new PrimitiveType DimExprs [Dims]
//         new ClassOrInterfaceType DimExprs [Dims]
//         new PrimitiveType Dims ArrayInitializer
//         new ClassOrInterfaceType Dims ArrayInitializer
//     DimExprs:
//         DimExpr {DimExpr}
//     DimExpr:
//         {Annotation} [ Expression ]

	'ArrayCreationExpression:=
			 " new " ~ 'PrimitiveType ~ 'DimExprs ~ 'Dims.?
			|" new " ~ 'ClassOrInterfaceType ~ 'DimExprs ~ 'Dims.?
			|" new " ~ 'PrimitiveType ~ 'Dims ~ 'ArrayInitializer
			|" new " ~ 'ClassOrInterfaceType ~ 'Dims ~ 'ArrayInitializer ,
	'DimExprs:= 'DimExpr ~ 'DimExpr.rep(0,Int.MaxValue) ,
	'DimExpr:=  'Annotation.rep(0,Int.MaxValue) ~ "[" ~ 'Expression ~ "]" ,

//     AssertStatement:
//         assert Expression ;
//         assert Expression : Expression ;
	'AssertStatement:=   " assert " ~ 'Expression ~ ";" 
						|" assert " ~ 'Expression ~ ":" ~ 'Expression ~ ";" ,
						
//     LabeledStatement:
//         Identifier : Statement
//     LabeledStatementNoShortIf:
//         Identifier : StatementNoShortIf					
//     Block:
//         { [BlockStatements] }
//     BlockStatements:
//         BlockStatement {BlockStatement}
//     BlockStatement:
//         LocalVariableDeclarationStatement
//         ClassDeclaration
//         Statement						
	'LabeledStatement:= 'Identifier ~ ":" ~ 'Statement ,
	'LabeledStatementNoShortIf:= 'Identifier ~ ":" ~ 'StatementNoShortIf ,
	'Block:= "{" ~ 'BlockStatements.? ~ "}" ,
	'BlockStatements:= 'BlockStatement ~ 'BlockStatement.rep(0,Int.MaxValue) ,
	'BlockStatement:= 'LocalVariableDeclarationStatement
					 |'ClassDeclaration
					 |'Statement ,
//     EmptyStatement:
//         ;
//     LambdaBody:
//         Expression
//         Block 
	'EmptyStatement:= ";" ,	
	//'LambdaBody:= 'Expression | 'Block ,

//     SwitchExpression:
//         switch ( Expression ) SwitchBlock
//     SwitchStatement:
//         switch ( Expression ) SwitchBlock
//     SwitchBlock:
//         { SwitchRule {SwitchRule} }
//         { {SwitchBlockStatementGroup} {SwitchLabel :} }
//     SwitchRule:
//         SwitchLabel -> Expression ;
//         SwitchLabel -> Block
//         SwitchLabel -> ThrowStatement
//     SwitchBlockStatementGroup:
//         SwitchLabel : {SwitchLabel :} BlockStatements
//     SwitchLabel:
//         case CaseConstant {, CaseConstant}
//         default
//     CaseConstant:
//         ConditionalExpression	
	'SwitchExpression:= " switch " ~ "(" ~ 'Expression ~ ")" ~ 'SwitchBlock ,
	'SwitchStatement := " switch " ~ "(" ~ 'Expression ~ ")" ~ 'SwitchBlock ,
	'SwitchBlock:=
			 "{" ~ 'SwitchRule ~ 'SwitchRule.rep(0,Int.MaxValue) ~ "}"
			|"{" ~ 'SwitchBlockStatementGroup.rep(0,Int.MaxValue) ~ ('SwitchLabel ~ ":").rep(0,Int.MaxValue) ~ "}" ,
	'SwitchRule:=
			 'SwitchLabel ~ "->" ~ 'Expression ~ ";"
			|'SwitchLabel ~ "->" ~ 'Block
			|'SwitchLabel ~ "->" ~ 'ThrowStatement ,
	'SwitchBlockStatementGroup:= 'SwitchLabel ~ ":" ~ ('SwitchLabel ~ ":").rep(0,Int.MaxValue) ~ 'BlockStatements ,
	'SwitchLabel:=
			 " case " ~ 'CaseConstant ~ ("," ~ 'CaseConstant).rep(0,Int.MaxValue)
			|" default " ,
	'CaseConstant:= 'ConditionalExpression ,
	
//     IfThenStatement:
//         if ( Expression ) Statement
//     IfThenElseStatement:
//         if ( Expression ) StatementNoShortIf else Statement
//     IfThenElseStatementNoShortIf:
//         if ( Expression ) StatementNoShortIf else StatementNoShortIf	
	'IfThenStatement:= " if " ~ "(" ~ 'Expression ~ ")" ~ 'Statement ,
	'IfThenElseStatement := " if " ~ "(" ~ 'Expression ~ ")" ~ 'StatementNoShortIf ~ " else " ~ 'Statement ,
	'IfThenElseStatementNoShortIf:= " if " ~ "(" ~ 'Expression ~ ")" ~ 'StatementNoShortIf ~ " else " ~ 'StatementNoShortIf ,

//     WhileStatement:
//         while ( Expression ) Statement
//     WhileStatementNoShortIf:
//         while ( Expression ) StatementNoShortIf
//     DoStatement:
//         do Statement while ( Expression ) ;	
//     ForStatement:
//         BasicForStatement
//         EnhancedForStatement
//     ForStatementNoShortIf:
//         BasicForStatementNoShortIf
//         EnhancedForStatementNoShortIf
	'WhileStatement:= " while " ~ "(" ~ 'Expression ~ ")" ~ 'Statement ,
	'WhileStatementNoShortIf:= " while " ~ "(" ~ 'Expression ~ ")" ~ 'StatementNoShortIf ,
	'DoStatement:= " do " ~ 'Statement ~ " while " ~ "(" ~ 'Expression ~ ")" ~ ";" ,
	//'ForStatement:= 'BasicForStatement | 'EnhancedForStatement ,
	'ForStatement:= 'BasicForStatement  ,
	//'ForStatementNoShortIf:= 'BasicForStatementNoShortIf | 'EnhancedForStatementNoShortIf ,
	'ForStatementNoShortIf:= 'BasicForStatementNoShortIf ,

//     BasicForStatement:
//         for ( [ForInit] ; [Expression] ; [ForUpdate] ) Statement
//     BasicForStatementNoShortIf:
//         for ( [ForInit] ; [Expression] ; [ForUpdate] ) StatementNoShortIf
//     ForInit:
//         StatementExpressionList
//         LocalVariableDeclaration
//     ForUpdate:
//         StatementExpressionList
//     StatementExpressionList:
//         StatementExpression {, StatementExpression}
	'BasicForStatement := " for " ~ "(" ~ 'ForInit.? ~ ";" ~ 'Expression.? ~ ";" ~ 'ForUpdate.? ~ ")" ~ 'Statement ,
	'BasicForStatementNoShortIf := " for " ~ "(" ~ 'ForInit.? ~ ";" ~ 'Expression.? ~ ";" ~ 'ForUpdate.? ~ ")" ~ 'StatementNoShortIf ,
	'ForInit:= 'StatementExpressionList | 'LocalVariableDeclaration ,
	'ForUpdate:= 'StatementExpressionList ,
	//'StatementExpressionList:= 'StatementExpression ~ ("," ~ 'StatementExpression).rep(0,Int.MaxValue) ,
	'StatementExpressionList:= 'ExpressionStatement ~ ("," ~ 'ExpressionStatement).rep(0,Int.MaxValue) ,
	
//     BreakStatement:
//         break [Identifier] ;
//     YieldStatement:
//         yield Expression ;	
//     ContinueStatement:
//         continue [Identifier] ;
//     ReturnStatement:
//         return [Expression] ;
//     ThrowStatement:
//         throw Expression ;
//     SynchronizedStatement:
//         synchronized ( Expression ) Block
 
	'BreakStatement:= " break " ~ 'Identifier.? ~ ";" ,
	'YieldStatement:= "yield" ~ 'Expression ~ ";" ,
	'ContinueStatement:= " continue " ~ 'Identifier.? ~ ";" ,
	'ReturnStatement:= " return " ~ 'Expression.? ~ ";" ,
	'ThrowStatement:= " throw " ~ 'Expression ~ ";" ,
	'SynchronizedStatement:= " synchronized " ~ "(" ~ 'Expression ~ ")" ~ 'Block ,

//     TryStatement:
//         try Block Catches
//         try Block [Catches] Finally
//         TryWithResourcesStatement
//     Catches:
//         CatchClause {CatchClause}
//     CatchClause:
//         catch ( CatchFormalParameter ) Block
//     CatchFormalParameter:
//        {VariableModifier} CatchType VariableDeclaratorId
//     CatchType:
//         UnannClassType {| ClassType}
//     Finally:
//         finally Block
	
	'TryStatement:=
			 " try " ~ 'Block ~ 'Catches
			|" try " ~ 'Block ~ 'Catches.? ~ 'Finally 
			|'TryWithResourcesStatement ,
	'Catches:= 'CatchClause ~ 'CatchClause.rep(0,Int.MaxValue) ,
	'CatchClause:= " catch " ~ "(" ~ 'CatchFormalParameter ~ ")" ~ 'Block ,
	'CatchFormalParameter:= 'VariableModifier.rep(0,Int.MaxValue) ~ 'CatchType ~ 'VariableDeclaratorId ,
	'CatchType:= 'UnannClassType ~ ("|" ~ 'ClassType).rep(0,Int.MaxValue) ,
	'Finally:= " finally " ~ 'Block ,

//     TryWithResourcesStatement:
//         try ResourceSpecification Block [Catches] [Finally]
//     ResourceSpecification:
//         ( ResourceList [;] )
//     ResourceList:
//         Resource {; Resource}
//     Resource:
//         {VariableModifier} LocalVariableType Identifier = Expression
//         VariableAccess
//     VariableAccess:
//         ExpressionName
//         FieldAccess
//     LocalVariableType:
//         UnannType
//         var	
	'TryWithResourcesStatement := " try " ~ 'ResourceSpecification ~ 'Block ~ 'Catches.? ~ 'Finally.? ,
	'ResourceSpecification:= "(" ~ 'ResourceList ~ ";".? ~ ")" ,
	'ResourceList:= 'Resource ~ (";" ~ 'Resource).rep(0,Int.MaxValue) ,
	'Resource:=
		 'VariableModifier.rep(0,Int.MaxValue) ~ 'LocalVariableType ~ 'Identifier ~ "=" ~ 'Expression
		|'VariableAccess ,
	'VariableAccess:= 'ExpressionName | 'FieldAccess ,
	'LocalVariableType:= 'UnannType | " var " ,

//     Annotation:
//         NormalAnnotation
//         MarkerAnnotation
//         SingleElementAnnotation
//     NormalAnnotation:
//         @ TypeName ( [ElementValuePairList] )
//     ElementValuePairList:
//         ElementValuePair {, ElementValuePair}
//     ElementValuePair:
//         Identifier = ElementValue
//     ElementValue:
//         ConditionalExpression
//         ElementValueArrayInitializer
//         Annotation
//     ElementValueArrayInitializer:
//         { [ElementValueList] [,] }
//     ElementValueList:
//         ElementValue {, ElementValue} 
	'Annotation:= 'NormalAnnotation
				| 'MarkerAnnotation
				| 'SingleElementAnnotation ,
	'NormalAnnotation:= " @ " ~ 'TypeName ~ "(" ~ 'ElementValuePairList.? ~ ")" ,
	'ElementValuePairList:= 'ElementValuePair ~ ("," ~ 'ElementValuePair).rep(0,Int.MaxValue) ,
	'ElementValuePair:= 'Identifier ~ "=" ~ 'ElementValue ,
	'ElementValue:=
			 'ConditionalExpression
			|'ElementValueArrayInitializer
			|'Annotation ,
	'ElementValueArrayInitializer:= "{" ~ 'ElementValueList.? ~ ",".? ~ "}" ,
	'ElementValueList:= 'ElementValue ~ ("," ~ 'ElementValue).rep(0,Int.MaxValue) ,

//     MarkerAnnotation:
//         @ TypeName
//     SingleElementAnnotation:
//         @ TypeName ( ElementValue )
	'MarkerAnnotation:= " @ " ~ 'TypeName ,
	'SingleElementAnnotation:= " @ " ~ 'TypeName ~ "(" ~ 'ElementValue ~ ")" ,

//     AnnotationTypeDeclaration:
//         {InterfaceModifier} @ interface TypeIdentifier AnnotationTypeBody
//     AnnotationTypeBody:
//         { {AnnotationTypeMemberDeclaration} }
//     AnnotationTypeMemberDeclaration:
//         AnnotationTypeElementDeclaration
//         ConstantDeclaration
//         ClassDeclaration
//         InterfaceDeclaration
//         ;	
	'AnnotationTypeDeclaration:= 'InterfaceModifier.rep(0,Int.MaxValue) ~ " @ " ~ " interface " ~ 'TypeIdentifier ~ 'AnnotationTypeBody ,
	'AnnotationTypeBody:= "{" ~ 'AnnotationTypeMemberDeclaration.rep(0,Int.MaxValue) ~ "}" ,
	'AnnotationTypeMemberDeclaration:=
			 'AnnotationTypeElementDeclaration
			|'ConstantDeclaration
			|'ClassDeclaration
			|'InterfaceDeclaration
			|";" ,
			
//     AnnotationTypeElementDeclaration:
//         {AnnotationTypeElementModifier} UnannType Identifier ( ) [Dims] [DefaultValue] ;
//     AnnotationTypeElementModifier:
//         (one of)
//         Annotation public
//         abstract
//     DefaultValue:
//     	   default ElementValue
	'AnnotationTypeElementDeclaration:= 'AnnotationTypeElementModifier.rep(0,Int.MaxValue) ~ 'UnannType ~ 'Identifier ~ "( )" ~ 'Dims.? ~ 'DefaultValue.? ~ ";" ,
	'AnnotationTypeElementModifier:= 'Annotation ~ " public " | " abstract " ,
	'DefaultValue:= " default " ~ 'ElementValue ,

//     ModuleDirective:
//         requires {RequiresModifier} ModuleName ;
//         exports PackageName [to ModuleName {, ModuleName}] ;
//         opens PackageName [to ModuleName {, ModuleName}] ;
//         uses TypeName ;
//         provides TypeName with TypeName {, TypeName} ;
//     RequiresModifier:
//         (one of)
//         transitive static
//     ModuleDeclaration:
//         {Annotation} [open] module Identifier {. Identifier} { {ModuleDirective} }	
	'ModuleDirective:=
		 " requires " ~ 'RequiresModifier.rep(0,Int.MaxValue) ~ 'ModuleName ~ ";" 
		|" exports "  ~ 'PackageName ~ (" to " ~ 'ModuleName ~ ("," ~ 'ModuleName).rep(0,Int.MaxValue)).? ~ ";"
		|" opens "    ~ 'PackageName ~ (" to " ~ 'ModuleName ~ ("," ~ 'ModuleName).rep(0,Int.MaxValue)).? ~ ";"
		|" uses " ~ 'TypeName ~ ";"
		|" provides " ~ 'TypeName ~ " with " ~ 'TypeName ~ ("," ~ 'TypeName).rep(0,Int.MaxValue) ~ ";" ,
	'RequiresModifier:= "transitive" | " static " ,
	
	'ModuleDeclaration:= 'Annotation.rep(0,Int.MaxValue) ~ " open ".? ~ " module " ~ 'Identifier ~ ("." ~ 'Identifier).rep(0,Int.MaxValue) ~ "{" ~ 'ModuleDirective.rep(0,Int.MaxValue) ~ "}" ,
	
	
	
)