structure Tigh = struct 

val interactive = tiglex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))

fun lexfile file = let val strm = TextIO.openIn file
		   in tiglex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end

fun whitespace(1,Space) = " "
	| whitespace(n,Space) = " "^(whitespace(n-1,Space));

fun tokenconvo token = case token of 
			(Comments x) => x
		| (Const x) => Int.toString (x)
		| (Quote x) => x
		| (Newline) => "\n"
		| (END) => ""
		| (Char (x,Space)) => whitespace(x,Space)
		| (Text x)  => x
		| (Key Array) => "array"
		| (Key If) => "if"
		| (Key Else) => "else"
		| (Key While) => "while"
		| (Key For) => "for"
		| (Key To) => "to"
		| (Key Do) => "do"
		| (Key Let) => "let"
		| (Key In) => "in"
		| (Key End) => "end"
		| (Key Of) => "of"
		| (Key Break) => "break"
		| (Key Nil) => "nil"
		| (Key Function) => "function"
		| (Key Var) => "var"
		| (Key Type) => "type"
		| (Key Import) => "import"
		| (Key Primitive) => "primitive"
		| (Obj Class) => "class"
		| (Obj Extends) => "extends"
		| (Obj Method) => "method"
		| (Obj New) => "new"
		| (Sym Coma) => ","
		| (Sym Colon) => ":"
		| (Sym Semicolon) => ";"
		| (Sym Lbrac) => "("
		| (Sym Rbrac) => "("
		| (Sym Lsqbrac) => "["
		| (Sym Rsqbrac) => "]"
		| (Sym Lflbrac) => "{"
		| (Sym Rflbrac) => "}"
		| (Sym Dot) => "."
		| (Sym Plus) => "+"
		| (Sym Minus) => "-"
		| (Sym Mul) => "*"
		| (Sym Div) => "/"
		| (Sym Equalto) => "="
		| (Sym LtGt) => "<>"
		| (Sym Lt) => "<"
		| (Sym Lteq) => "<="
		| (Sym Gt) => ">"
		| (Sym Gteq) => ">="
		| (Sym AND) => "&"
		| (Sym OR) => "|"
		| (Sym ColonEq) => ":="
		



fun colorprint token = case token of 
			(Comments x)  => print("\027[30m"^(tokenconvo token))
			| (Quote x)   => print("\027[96m"^(tokenconvo token))
			| (Newline)   => print(tokenconvo token)
			| (Char(x,Space)) => print(tokenconvo token)
			| (Const x)   => print("\027[94m"^(tokenconvo token))
			| (Text x)    => print("\027[32m"^(tokenconvo token))
			| (Key _)    => print("\027[33m"^(tokenconvo token))
			| (Obj _)    => print("\027[34m"^(tokenconvo token))
			| (Sym _)    => print("\027[37m"^(tokenconvo token))
			| (END)      => print("\027[0m"^(tokenconvo token))
			
 



fun runWithLexer lexer = let fun loop stack = case lexer () of
						END     => colorprint (END)
						| token => loop(colorprint token)
			
			 in loop ()
			 end


val _ =  ( case CommandLine.arguments() of
	       [] => runWithLexer interactive
	    |  xs => (List.map (runWithLexer o lexfile) xs; ())
	 )

end

