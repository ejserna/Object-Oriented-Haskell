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
import Text.Show.Pretty
}
%name ooh
%tokentype { Token }
%error { parseError }
%token
  "="                 { TEquals _ }
  ","                 { TComma _ }
  "if"                { TIf _ }
  "else"              { TElse _ }
  "case"              { TCase _ }
  "of"                { TOf _ }
  "otherwise"         { TOtherwise _ }
  "for"               { TFor _ }
  "while"             { TWhile _ }
  "read"              { TRead _ }
  "display"           { TDisplay _ }
  "+"                 { TPlus _ }
  "-"                 { TMinus _ }
  "*"                 { TMultiply _ }
  "/"                 { TDivide _ }
  "++"                { TPlusPlus _ }
  "--"                { TMinusMinus _ }
  "^"                 { TPower _ }
  "!"                 { TNot _ }
  "True"              { TTrue _ }
  "False"             { TTrue _ }
  ">"                 { TGreaterThan _ }
  "<"                 { TLessThan _ }
  ">="                { TGreaterEqualThan _ }
  "<="                { TLessEqualThan _ }
  "=="                { TDoubleEqual _ }
  "!="                { TNotEqual _ }
  "&&"                { TAnd _ }
  "||"                { TOr _ }
  "main"              { TMain _ }
  "class"             { TClass _ }
  ":"                 { TColon _ }
  "[+]"               { TPublic _ }
  "[-]"               { TPrivate _ }
  "=>"                { TEqualsRightArrow _ }
  "->"                { TDashRightArrow _ }
  "::"                { TDoubleColon _ }
  "["                 { TLeftBracket _ }
  "]"                 { TRightBracket _ }
  "List"              { TList _ }
  ".."                { TDoublePoint _ }
  ";"                 { TSemiColon _ }
  "{"                 { TLeftBrace _ }
  "}"                 { TRightBrace _ }
  "("                 { TLeftParen _ }
  ")"                 { TRightParen _ }
  "Int"               { TInt _ }
  "Integer"           { TInteger _ }
  "Double"            { TDouble _ }
  "Money"             { TMoney _ }
  "String"            { TString _ }
  "Bool"              { TBool _ }
  "Nothing"           { TString _ }
  "return"            { TReturn _ }
  "."                 { TPoint _ }
  "%"                 { TMod _ }
  decimal_literal     { TDecimalLiteral _ $$ }
  integer_literal     { TIntegerLiteral _ $$ }  
  var_identifier      { TVarIdent _ $$ }
  class_identifier    { TClassIdent _ $$ }
  string_literal      { TStringLiteral _ $$ }
%%



Program : 
          Classes Functions Variables "main" Block {Program $1 $2 $3 $5}

Classes : 
        {- empty -} {[]}
      | Class Classes { $1 : $2}

Functions :
        {- empty -} {[]}
      | Function Functions { $1 : $2}  

Variables :
        {- empty -} {[]}
      | Variable Variables { $1 : $2}  

Function : 
          var_identifier "=>" TypeFuncReturn "::" TypeFuncParams var_identifier Params Block  {Function $1 $3 (($5,$6) : $7) $8}
        | var_identifier "=>" TypeFuncReturn Block  {FunctionEmptyParams $1 $3 $4}

Params : 
        {- empty -} { [] }
        |  "->" TypeFuncParams var_identifier Params { ($2,$3) : $4 }

TypeFuncReturn : 
          Primitive {TypeFuncReturnPrimitive $1}
        | class_identifier {TypeFuncReturnClassId $1}
        | "Nothing" {TypeFuncReturnNothing}

TypeFuncParams :
          Primitive ArrayIndexesDeclaration { TypeFuncParamsPrimitive $1 $2}
        | class_identifier ArrayIndexesDeclaration { TypeFuncParamsClassId $1 $2}

Primitive :
          "Int" {PrimitiveInt}
        | "Double" {PrimitiveDouble}
        | "Money" {PrimitiveMoney}
        | "String" {PrimitiveString}
        | "Bool" {PrimitiveBool}
        | "Integer" {PrimitiveInteger}

Type :
          Primitive ArrayIndexesDeclaration {TypePrimitive $1 $2}
        | class_identifier ArrayIndexesDeclaration {TypeClassId $1 $2}

ArrayIndexesDeclaration :
          {- empty -} { [] }
        |  "[" integer_literal "]" ArrayIndexesDeclaration { ("[",$2,"]") : $4 }

ArrayIndexesExpression :
          "[" integer_literal "]" ArrayIndexesExpression { (ArrayAccessLiteral $2) : $4 } 
        | "[" var_identifier "]" ArrayIndexesExpression { (ArrayAccessVar $2) : $4 }
        -- | "[" Expression "]" ArrayIndexesExpression { (ArrayAccessExoression $2) : $4 }   

ListType :
          "List" "of" class_identifier {ListTypeClassId $3}
        | "List" "of" Primitive {ListTypePrimitive $3}

Variable :
          Type var_identifier VarIdentifiers ";" {VariableNoAssignment $1 $2 $3 }
        | Type var_identifier "=" LiteralOrVariable ";" {VariableLiteralOrVariable $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment1D ";" {VariableAssignment1D $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment2D ";" {VariableAssignment2D $1 $2 $4 }
        | ListType var_identifier "=" ListAssignment ";" {VariableListAssignment $1 $2 $4}
        | ListType var_identifier VarIdentifiers ";" {VariableListNoAssignment $1 $2 $3}

VarIdentifiers :
        {- empty -} { [] }
      |  "," var_identifier VarIdentifiers { $2 : $3 }

ArrayAssignment1D :
        "[" LiteralOrVariable Array1DAssignments "]" { $2 : $3 }
      | "["  "]" {[]}

Array1DAssignments :
        {- empty -} { [] }
      |  "," LiteralOrVariable Array1DAssignments { $2 : $3 }

ArrayAssignment2D :
        "[" ArrayAssignment1D Array2DAssignments"]" { $2 : $3 }

Array2DAssignments :
        {- empty -} { [] }
      |  "," ArrayAssignment1D Array2DAssignments { $2 : $3 }

ListAssignment : 
        ArrayAssignment1D {ListAssignmentArray $1}
      | "[" integer_literal ".." integer_literal "]" {ListAssignmentRange $2 $4}

Class : 
        "class" class_identifier ":" class_identifier ClassBlock {ClassInheritance $2 $4 $5}
      | "class" class_identifier ClassBlock {ClassNormal $2 $3}


ClassBlock : 
        "{" ClassMembers ClassConstructor ClassMembers "}" {ClassBlock $2 $3 $4}
      | "{" ClassMembers "}" {ClassBlockNoConstructor $2}

ClassMembers : 
        {- empty -} {[]} 
      | ClassAttribute ClassMembers { (ClassMemberAttribute $1) : $2} 
      | ClassFunction ClassMembers { (ClassMemberFunction $1) : $2}

ClassAttribute :
        "[+]" Variable {ClassAttributePublic $2 }
      | "[-]" Variable {ClassAttributePrivate $2 }

ClassFunction :
        "[+]" Function {ClassFunctionPublic $2}
      | "[-]" Function {ClassFunctionPrivate $2}

ClassConstructor :
        "::" TypeFuncParams var_identifier Params Block {ClassConstructor (($2,$3) : $4) $5}
      | "::" Block { ClassConstructorEmpty }

LiteralOrVariable :
        var_identifier {VarIdentifier $1}
      | integer_literal {IntegerLiteral $1}
      | decimal_literal {DecimalLiteral $1}
      | string_literal {StringLiteral $1}

Block:
    "{" "}" {Block}

{
parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) 
  in 
  error ("Parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

-- Esta sección tiene las producciones semánticas para producir el árbol abstracto de sintaxis 

data Program 
    = Program [Class] [Function] [Variable] Block
  deriving (Show, Eq)

data Function 
    = Function String TypeFuncReturn [(TypeFuncParams,String)] Block
    | FunctionEmptyParams String TypeFuncReturn Block
  deriving (Show, Eq)

data TypeFuncReturn 
    = TypeFuncReturnPrimitive Primitive 
    | TypeFuncReturnClassId String
    | TypeFuncReturnNothing
  deriving (Show, Eq)

data TypeFuncParams 
    = TypeFuncParamsPrimitive Primitive [(String,Integer,String)]
    | TypeFuncParamsClassId String [(String,Integer,String)]
    | TypeFuncParamsList ListType
  deriving (Show, Eq)

data Primitive 
    = PrimitiveInt
    | PrimitiveDouble
    | PrimitiveMoney
    | PrimitiveString
    | PrimitiveBool
    | PrimitiveInteger
  deriving (Show, Eq)

data Type 
    = TypePrimitive Primitive [(String,Integer,String)]
    | TypeClassId String [(String,Integer,String)] 
  deriving (Show, Eq)

data ListType 
    = ListTypeClassId String
    | ListTypePrimitive Primitive
  deriving (Show, Eq)

data Variable 
    = VariableNoAssignment Type String [String]
    | VariableLiteralOrVariable Type String LiteralOrVariable
    | VariableAssignment1D Type String [LiteralOrVariable]
    | VariableAssignment2D Type String [[LiteralOrVariable]]
    | VariableListAssignment ListType String ListAssignment
    | VariableListNoAssignment ListType String [String]
  deriving (Show, Eq)

data ListAssignment 
    = ListAssignmentArray [LiteralOrVariable]
    | ListAssignmentRange Integer Integer
  deriving (Show, Eq)

data Class 
    = ClassInheritance String String ClassBlock
    | ClassNormal String ClassBlock
  deriving (Show, Eq)

data ClassBlock 
    = ClassBlock [ClassMember] ClassConstructor [ClassMember]
    | ClassBlockNoConstructor [ClassMember]
  deriving (Show, Eq)

data ClassMember
    = ClassMemberAttribute ClassAttribute
    | ClassMemberFunction ClassFunction
  deriving (Show, Eq) 

data ClassAttribute 
    = ClassAttributePublic Variable
    | ClassAttributePrivate Variable
  deriving (Show, Eq)

data ClassFunction 
    = ClassFunctionPublic Function
    | ClassFunctionPrivate Function
  deriving (Show, Eq)

data ClassConstructor 
    = ClassConstructorEmpty
    | ClassConstructor [(TypeFuncParams,String)] Block
  deriving (Show, Eq)

data LiteralOrVariable 
    = VarIdentifier String
    | IntegerLiteral Integer
    | DecimalLiteral Decimal
    | StringLiteral String
  deriving (Show, Eq)

data Block 
    = Block
  deriving (Show, Eq)  

data ArrayAccess
    = ArrayAccessLiteral Integer
    | ArrayAccessVar String
  deriving (Show,Eq)
    -- | ArrayAccessExpression Expression

main = do 
  inStr <- getContents
  putStrLn(show(inStr))
  let parseTree = ooh (alexScanTokens2 inStr)
  putStrLn ("SUCCESS " ++ show(parseTree) )
  putStrLn $ ppShow $ parseTree
}
