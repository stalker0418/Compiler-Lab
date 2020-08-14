(*structure Ast = struct

    datatype Binop = Plus
                    | Mul
                    | Div
                    | Minus
		    | GreaterEqualto
		    | LesserEqualto
		    | Equal
		    | GreaterLesser
		    | Lesser
		    | Greater
		    | And
		    | Or

    datatype Expr = (*id*) 
		Const of int
                | Var of string
                | Op of (Expr * Binop * Expr)
                | If of (Expr * Expr)
                | For of (Expr * Expr * Expr * Expr)
                | While of (Expr * Expr)
                | Let of (Expr list * Expr list)
                | Assign of (Expr * Expr)
                (*| Equal of (Expr * Expr)*)

	and Id = Var of string

	and Tyfield = 
			 Ty1 of Id * Id  
			|Ty2 of Id * Id * Tyfield

	and Ty = 
		 I1 of Id
		|T of Tyfield
		|Array of Id
		|Class1 of Id
	
	and VarId = Lol of Id * Id * Expr
			

	and ClassField = 
			 CfvId of VarId
			|Method of Id

    fun plus    x y = Op    (x, Plus, y)
    fun minus   x y = Op    (x, Minus, y)
    fun mul     x y = Op    (x, Mul, y)
    fun div     x y = Op    (x, Div, y)
    fun grteqto x y = Op    (x, GreaterEqualto, y)
    fun lsteqto x y = Op    (x, LesserEqualto, y)
    fun equal   x y = Op    (x, Equal, y)
    fun grtlst  x y = Op    (x, GreaterLesser, y)
    fun lesser  x y = Op    (x, Lesser, y)
    fun greater x y = Op    (x, Greater, y)
    fun and_o   x y = Op    (x, And, y)
    fun or_o    x y = Op    (x, Or, y)   
    fun LET     x y = Let   (x,y)
    fun IF      x y = If    (x,y)
    fun FOR x y z w = For   (x,y,z,w)
    fun WHILE   x y = While (x,y)
    fun ASSIGN  x y = Assign(x,y)
    fun EQUAL   x y = Op    (x, Equal, y)
end



structure Ast = struct

datatype Program = Expr of Exp
		| Decr of Dec list

and Exp   	= Nil
		| Const		of int
		| Quote		of string
		| ArrayCreate	of Id * Exp * Exp
		| RecordCreate	of Id * Help1
		| New		of Id
		| I1		of Id
		| L		of Lvalue
		| FunctionCall	of Id * Exp list
		| MethodCall1	of Id * Id * Exp list
		| MethodCall2	of Lvalue * Id * Exp list
		| Monominus	of Exp
                | Op         	of Exp * BinOp * Exp
		| OpList	of Exp list
		| Assignment1 	of Id * Exp
		| Assignment2 	of Lvalue * Exp
		| If 	     	of Exp * Exp
		| Else       	of Exp * Exp * Exp
		| While      	of Exp * Exp
		| For 	     	of Id * Exp * Exp * Exp
		| Break
		| Let        	of Dec list * Exp list

and Id = Var of string

and Help1 = 	Help11	of Id * Exp
	      | Help12	of Id * Exp * Help1

and Lvalue = Lvalue1 of Id * Id
		| Lvalue2 of Id * Exp
		| Lvalue3 of Lvalue * Id
		| Lvalue4 of Lvalue * Exp

 	and Dec	= TyDec		of Id * Ty
		| ClassAlt1	of Id * ClassField list
		| ClassAlt2	of Id * Id * ClassField list
		| VarDec	of Id * Exp
		| FunDec1	of Id * TyField * Exp
		| FunDec2	of Id * TyField * Id * Exp
		| PrimDec1	of Id * TyField
		| PrimDec2	of Id * TyField * Id
		| Import	of Exp


and ClassField =  AttrDec	of Id * Id * Exp
		| Method	of Id

and Ty		= I2		of Id
		| ArrayDef	of Id
		| T		of TyField
		| ClassCan	of Id

and TyField
		= TyField1	of Id * Id
		| TyField2	of Id * Id * TyField

      and BinOp = Plus
                | Minus
                | Mul
		| Div
		| Equal
		| Ltgt
		| Gt
		| Lt
		| Gtequal
		| Ltequal
		| And
		| Or

fun ARRAYCREATE		a b c	= ArrayCreate (a,b,c)
fun RECORDCREATE	a b	= RecordCreate (a,b)
fun HELP11		a b	= Help11 (a,b)
fun HELP12		a b c	= Help12 (a,b,c)

fun FUNCTIONCALL  	a b	= FunctionCall (a,b)
fun METHODCALL1		a b c	= MethodCall1 (a,b,c)
fun METHODCALL2		a b c	= MethodCall2 (a,b,c)
fun LVALUE1		a b	= Lvalue1 (a,b)
fun LVALUE2		a b	= Lvalue2 (a,b)
fun LVALUE3		a b	= Lvalue3 (a,b)
fun LVALUE4		a b	= Lvalue4 (a,b)
fun ASSIGNMENT1  	a b     = Assignment1 (a, b)
fun ASSIGNMENT2  	a b     = Assignment2 (a, b)

fun IF 		a b     = If (a, b)
fun ELSE 	a b c   = Else (a,b,c)
fun WHILE 	a b     = While (a, b)
fun FOR 	a b c d = For (a, b, c, d)
fun LET		a b	= Let (a,b)

fun PLUS    a b = Op (a, Plus, b)
fun MINUS   a b = Op (a, Minus, b)
fun MUL     a b = Op (a, Mul, b)
fun DIV     a b = Op (a, Div, b)
fun EQUAL   a b = Op (a, Equal, b)
fun LTGT    a b = Op (a, Ltgt, b)
fun GT      a b = Op (a, Gt, b)
fun LT      a b = Op (a, Lt, b)
fun GTEQUAL a b = Op (a, Gtequal, b)
fun LTEQUAL a b = Op (a, Ltequal, b)
fun AND     a b = Op (a, And, b)
fun OR      a b = Op (a, Or, b)
		
fun TYDEC	a b	= TyDec (a,b)
fun CLASSALT1	a b	= ClassAlt1 (a,b)
fun CLASSALT2	a b c	= ClassAlt2 (a,b,c)
fun VARDEC	a b	= VarDec (a,b)
fun FUNDEC1	a b c	= FunDec1 (a,b,c)
fun FUNDEC2	a b c d	= FunDec2 (a,b,c,d)
fun ATTRDEC	a b c	= AttrDec (a,b,c)
fun PRIMDEC1	a b	= PrimDec1 (a,b)
fun PRIMDEC2	a b c	= PrimDec2 (a,b,c)

fun TYFIELD1	a b	= TyField1 (a,b)
fun TYFIELD2	a b c	= TyField2 (a,b,c)

end
*)
structure Ast = struct

datatype Program = Expression of EXP
				| Decls of 	DEC list

and EXP = Nil
	| Const of int
	| Quote of string
	| IDENT of Id
	| IF of EXP * EXP
	| ELSE of EXP * EXP * EXP
	| WHILE of EXP * EXP
	| FOR of Id * EXP * EXP * EXP
	| Break
	| Let of DEC list * EXP list
	| OP of EXP * BinOp * EXP
	| UMINUS of EXP
	| Exps of EXP list
	| ArrayCreate of Id * EXP
	| RecordCreate of Id * (Id * EXP)list
	| New of Id
	| Functioncall of ( Id * EXP list)
	| Lvalue1 of (EXP * Id)
	| Lvalue2 of (EXP * EXP)
	| Assign of (EXP * EXP)
	| Methodcall of (EXP * Id * EXP list)

and Id = Var of string

and BinOp = PLUS 
			| MINUS 
			| MUL 
			| DIVIDE 
			| EQUAL 
			| LESSGREAT 
			| LESS 
			| GREAT
			| GREATEQUAL
			| LESSEQUAL
			| AND 
			| OR

and DEC = Import of EXP
		| Typedecl of Id * TY
		| Functiondecl1 of Id * TYFIELDS * EXP
		| Functiondecl2 of Id * Id * TYFIELDS * EXP
		| Primitivedecl1 of Id * TYFIELDS
		| Primitivedecl2 of Id * Id * TYFIELDS
		| ClassDec1 of Id * CLASSFIELD list
		| ClassDec2 of Id * Id * CLASSFIELD list
		| DVarDec	of Vardec

and TYFIELDS = Tyfields of (Id * Id ) list

and TY = TyIDENT of Id
		| Tyf of TYFIELDS
		| Array of Id
		| Classfields of CLASSFIELD list
		| Classfields2 of Id * CLASSFIELD list

and CLASSFIELD = ClassVar of Vardec
				| Method1 of Id * TYFIELDS * EXP
				| Method2 of Id *Id * TYFIELDS * EXP


and Vardec = Vardec1 of ( Id * Id * EXP)
			| Vardec2 of ( Id * EXP)

fun plus a b			= OP (a,PLUS,b)
fun minus a b			= OP (a,MINUS,b)
fun mul a b				= OP (a,MUL,b)
fun divide a b			= OP (a,DIVIDE,b)
fun equal a b			= OP (a,EQUAL,b)
fun lessgreat a b		= OP (a,LESSGREAT,b)
fun less a b			= OP (a,LESS,b)
fun great a b			= OP (a,GREAT,b)
fun lessequal a b		= OP (a,LESSEQUAL,b)
fun greatequal a b		= OP (a,GREATEQUAL,b)
fun And a b				= OP (a,AND,b)
fun or a b				= OP (a,OR,b)
fun If a b 				= IF (a, b)
fun Else a b c 			= ELSE (a, b, c)
fun While a b 			= WHILE (a, b)
fun For a b c d			= FOR (a, b, c, d)
end
