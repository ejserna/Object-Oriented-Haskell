{- 
  Patito.y

  Archivo de gramática de Happy, el cual es un generador de parsers basado en Yacc.
  El archivo contiene la gramática del lenguaje LittleDuck2018. Para poder correrlo, se ocupa GHCI instalado, así como Alex y Happy.

  Autor: Eduardo Serna A01196007
-}

{
module Main where
import Scanner
import Data.Decimal
}


%name ObjectiveOrientedHaskell
%tokentype { Token }
%error { parseError }
%token
  float_literal { TFloatLiteral _ $$ }
  integer_literal     { TIntLiteral _ $$ }
  "if"                { TIf _ }
  "else"              { TElse _ }
  "program"				    { TProgram _ }
  ";"                 { TSemiColon _ }
  "var"               { TVar _ }
  ":"                 { TColon _ }
  "int"               { TInt _ }
  "float"             { TFloat _ }
  "="                 { TEquals _ }
  "{"                 { TLeftBrace _ }
  "}"                 { TRightBrace _ }
  "print"             { TPrint _ }
  "("                 { TLeftParen _ }
  ")"                 { TRightParen _ }
  ">"                 { TGreaterThan _ }
  "<"                 { TLowerThan _ }
  "<>"                { TNotEqual _ }
  ","                 { TComma _ }
  "+"                 { TPlus _ }
  "-"                 { TMinus _ }
  "*"                 { TMultiply _ }
  "/"                 { TDivide _ }
  identifier          { TIdent _ $$ }
  string_literal      { TStringLiteral _ $$ }
%%

Prog : 
        "program" identifier ";" Vars Bloque { ProgVars $2 $4 $5}
      | "program" identifier ";" Bloque { ProgBloque $2 $4 }

Vars : 
    "var" VarIds { Vars $2 }


-- var_ids es una regla auxiliar de vars, para así poder poner varias variales separadas por ;
VarIds : {- empty -} { VarIdsEmpty }
      | VarIds VarId   { VarIds $1 $2 }

VarId : 
      identifier Ids ":" Tipo ";" { VarId $1 $2 $4 }

-- ids es una regla auxiliar de ids, para poder poner varias variabbles de un mismo tipo separadas por ,.
-- La caracteristica comun que tienen estas variables es que comparten el tipo que se les asigne en var_ids
Ids : 
    {- empty -} { IdsEmpty }
    | Ids "," identifier  { Ids $1 $3 }


Tipo : 
    "int" { TypeInt }
  | "float" { TypeFloat }     


Bloque :
      "{" Estatutos "}" { Bloque $2 }    

-- estatutos es una regla auxiliar de bloque para poder poner varios en un bloque
Estatutos : 
  {- empty -} { EstatutosEmpty }
  | Estatutos Estatuto { Estatutos $1 $2 }

Estatuto :
    Asignacion { EstatutoAsig $1 }
  | Condicion  { EstatutoCond $1 }
  | Escritura  { EstatutoEsc $1 }


Asignacion : 
    identifier "=" Expresion ";" { Asignacion $1 $3 }

Expresion : 
      Exp ">" Exp { ExpresionGreater $1 $3 }
    | Exp "<" Exp { ExpresionLower $1 $3 }
    | Exp "=" Exp { ExpresionEquals $1 $3 }
    | Exp "<>" Exp{ ExpresionNotEquals $1 $3 }
    | Exp { Expresion $1 }

Exp :
  Term Exps { Exp $1 $2 }

Exps : 
  {- empty -} { ExpsEmpty }
  | Exps "+" Term  { ExpsPlus $1 $3 }
  | Exps "-" Term  { ExpsMinus $1 $3 }

Term :
    Fact Terms { Term $1 $2}

Terms : 
  {- empty -} { TermsEmpty }
  | Terms "*" Fact { TermsMult $1 $3 }
  | Terms "/" Fact { TermsDiv $1 $3 }

Fact :
    "(" Expresion ")" { FactExp $2 }
  | "+" VarCte        { FactPos $2 }
  | "-" VarCte        { FactNeg $2 }
  | VarCte            { FactCte $1 }

VarCte :
    identifier { VarCteId $1 }
  | integer_literal { VarCteInt $1 }
  | float_literal {VarCteFloat $1 }

Condicion :
        "if" "(" Expresion ")" Bloque ";" { CondicionIf $3 $5 }
      | "if" "(" Expresion ")" Bloque "else" Bloque ";" { CondicionIfElse $3 $5 $7 }

Escritura :
        "print" "(" Expresion Escrituras ")" ";" { EscrituraPrintExpresion $3 $4 }
      | "print" "(" string_literal Escrituras ")" ";" { EscrituraPrintStrings $3 $4 }

-- escrituras permite poner varias escritura separadas por coma
Escrituras : {- empty -} { EscriturasEmpty }
      | Escrituras "," Expresion { EscriturasExpresiones $1 $3 }
      | Escrituras "," string_literal { EscriturasStrings $1 $3 } 

{
parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) 
  in 
  error ("parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

-- A continuacion se realiza el data model de la gramatica. Esto sirve para crear el AST.

data Program 
    = ProgVars String Vars Bloque
    | ProgBloque String Bloque
  deriving (Show, Eq)

data Vars
    = Vars VarIds
  deriving (Show, Eq)

data VarIds
    = VarIds VarIds VarId
    | VarIdsEmpty
  deriving (Show, Eq)

data VarId
    = VarId String Ids Tipo
  deriving (Show, Eq)

data Ids
    = Ids Ids String
    | IdsEmpty
  deriving (Show, Eq)

data Tipo
    = TypeInt
    | TypeFloat
  deriving (Show, Eq)

data Bloque
    = Bloque Estatutos
  deriving (Show, Eq)

data Estatutos
    = Estatutos Estatutos Estatuto
    | EstatutosEmpty
  deriving (Show, Eq)

data Estatuto
    = EstatutoAsig Asignacion
    | EstatutoCond Condicion
    | EstatutoEsc Escritura
  deriving (Show, Eq)

data Asignacion
    = Asignacion String Expresion
  deriving (Show, Eq)

data Expresion
    = ExpresionGreater Exp Exp
    | ExpresionLower Exp Exp
    | ExpresionEquals Exp Exp
    | ExpresionNotEquals Exp Exp
    | Expresion Exp
  deriving (Show, Eq)

data Exp
    = Exp Term Exps
  deriving (Show, Eq)

data Exps
    = ExpsEmpty
    | ExpsPlus Exps Term
    | ExpsMinus Exps Term
  deriving (Show, Eq)

data Term
    = Term Fact Terms
    deriving (Show, Eq)

data Terms
    = TermsEmpty
    | TermsMult Terms Fact
    | TermsDiv Terms Fact
  deriving (Show, Eq)

data Fact
    = FactExp Expresion
    | FactPos VarCte
    | FactNeg VarCte
    | FactCte VarCte
  deriving (Show, Eq)

data VarCte 
    = VarCteId String
    | VarCteInt Int
    | VarCteFloat Decimal
    deriving (Show, Eq)

data Condicion 
    = CondicionIf Expresion Bloque
    | CondicionIfElse Expresion Bloque Bloque
  deriving (Show, Eq)

data Escritura 
    = EscrituraPrintExpresion Expresion Escrituras
    | EscrituraPrintStrings String Escrituras
  deriving (Show, Eq)

data Escrituras
    = EscriturasExpresiones Escrituras Expresion
    | EscriturasStrings Escrituras String
    | EscriturasEmpty
  deriving (Show, Eq)


-- Funcion que se utiliza para obtener el nombre del programa que se parseo
getProgramName :: Program -> String
getProgramName (ProgVars name _ _) = name
getProgramName (ProgBloque name _) = name

main = do 
  inStr <- getContents
  let parseTree = patito (alexScanTokens2 inStr)
  putStrLn ("Parsing Success: program " ++ getProgramName(parseTree) )
  putStrLn ("Parsing Success: program " ++ show(parseTree) )
}
