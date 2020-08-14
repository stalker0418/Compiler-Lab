datatype keywords = Array
		| If
		| Else
		| While
		| For
		| To
		| Do
		| Let
		| In
		| End
		| Of
		| Break
		| Nil
		| Function
		| Var
		| Type
		| Import
		| Primitive


datatype objectkey = Class
		| Extends
		| Method
		| New

datatype symbols = Coma
		| Colon
		| Semicolon
		| Lbrac
		| Rbrac
		| Lsqbrac
		| Rsqbrac
		| Lflbrac
		| Rflbrac
		| Dot
		| Plus
		| Minus
		| Mul
		| Div
		| Equalto
		| LtGt
		| Lt
		| Lteq
		| Gt
		| Gteq
		| AND
		| OR
		| ColonEq

datatype whitecharacters = Space
			
			

datatype Token = Newline
		| END
		| Key of keywords
		| Obj of objectkey
		| Sym of symbols
		| Char of (int*whitecharacters)
		| Comments of string
		| Const of int
		| Quote of string
		| Text of string
		


