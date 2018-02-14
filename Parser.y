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
  decimal_literal     { TDecimalLiteral _ $$ }
  integer_literal     { TIntegerLiteral _ $$ }  
  var_identifier      { TVarIdent _ $$ }
  class_identifier    { TClassIdent _ $$ }
  string_literal      { TStringLiteral _ $$ }
%%

Program : 
          FunctionsVariablesClasses "main" Block {Program $1 $3}

FunctionsVariablesClasses :
         {- empty -} { FVCEmpty }
        | FunctionsVariablesClasses Function {FVCFunction $1 $2}
        | FunctionsVariablesClasses Variable {FVCVariable $1 $2}
        | FunctionsVariablesClasses Class {FVCClass $1 $2}

Function : 
          var_identifier "=>" TypeFuncReturn "::" TypeFuncParams var_identifier Params Block  {Function $1 $3 $5 $6 $7 $8}
        | var_identifier "=>" TypeFuncReturn Block  {FunctionEmptyParams $1 $3 $4}

Params : 
        {- empty -} { ParamsEmpty }
        |  Params "->" TypeFuncParams var_identifier {Params $1 $3 $4}

TypeFuncReturn : 
          Primitive ClosingBracketsNoIdentifier {TypeFuncReturnPrimitive $1 $2}
        | class_identifier ClosingBracketsNoIdentifier {TypeFuncReturnClassId $1 $2}
        | ListType {TypeFuncReturnList $1}
        | "Nothing" {TypeFuncReturnNothing}

TypeFuncParams :
          Primitive ClosingBracketsNoIdentifier { TypeFuncParamsPrimitive $1 $2}
        | class_identifier ClosingBracketsNoIdentifier { TypeFuncParamsClassId $1 $2}
        | ListType {TypeFuncParamsList $1}

ClosingBracketsNoIdentifier :
          {- empty -} { ClosingBracketsNoIdentifierEmpty }
        | ClosingBracketsNoIdentifier "[" "]" {ClosingBracketsNoIdentifier $1 }

Primitive :
          "Int" {PrimitiveInt}
        | "Double" {PrimitiveDouble}
        | "Money" {PrimitiveMoney}
        | "String" {PrimitiveString}
        | "Bool" {PrimitiveBool}
        | "Integer" {PrimitiveInteger}

Type :
          Primitive ArrayIndexes {TypePrimitive $1 $2}
        | class_identifier ArrayIndexes {TypeClassId $1 $2}

ArrayIndexes :
          {- empty -} { ArrayIndexesEmpty }
        | ArrayIndexes "[" var_identifier "]"  {ArrayIndexesIdentifier $1 $3 }
        | ArrayIndexes "[" integer_literal "]" {ArrayIndexesIntLiteral $1 $3 }

ListType :
          "List" "of" class_identifier {ListTypeClassId $3}
        | "List" "of" Primitive {ListTypePrimitive $3}

Variable :
          Type var_identifier VarIdentifiers ";" {VariableNoAssignment $1 $2 $3 }
        | Type var_identifier "=" LiteralOrVariable ";" {VariableLiteralOrVariable $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment1D ";" {VariableAssignment1D $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment2D ";" {VariableAssignment2D $1 $2 $4 }
        | ListType var_identifier "=" ListAssignment ";" {VariableListAssignment $1 $2 $4}
        | ListType var_identifier ";" {VariableList $1 $2}

VarIdentifiers :
        {- empty -} { VarIdentifiersEmpty }
      | VarIdentifiers "," var_identifier {VarIdentifiers $1 $3 }

ArrayAssignment1D :
        "[" var_identifier Array1DAssignments "]" {ArrayAssignmentVarIdentifier $2 $3 }
      | "[" integer_literal Array1DAssignments "]" {ArrayAssignmentIntLiteral $2 $3 }
      | "["  "]" {ArrayAssignment1DEmpty}

Array1DAssignments :
        {- empty -} { Array1DAssignmentsEmpty }
      | Array1DAssignments "," integer_literal {Array1DAssignmentsInteger $1 $3 }
      | Array1DAssignments "," var_identifier {Array1DAssignmentsVarIdentifier $1 $3}

ArrayAssignment2D :
        "[" ArrayAssignment1D Array2DAssignments"]" {ArrayAssignment2D $2 $3 }

Array2DAssignments :
        {- empty -} { Array2DAssignmentsEmpty }
      | Array2DAssignments "," ArrayAssignment1D {Array2DAssignments $1 $3 }

ListAssignment : 
        ArrayAssignment1D {ListAssignmentArray $1}
      | "[" integer_literal ".." integer_literal "]" {ListAssignmentRange $2 $4}

Class : 
        "class" class_identifier ":" class_identifier ClassBlock {ClassInheritance $2 $4 $5}
      | "class" class_identifier ClassBlock {ClassNormal $2 $3}

ClassBlock :
        "{" ClassAttributes ClassConstructor ClassFunctions "}" {ClassBlock $2 $3 $4}
      | "{" "}" {ClassBlockEmpty}

ClassAttributes :
        {- empty -} { ClassAttributesEmpty }
      | ClassAttributes ClassAttribute {ClassAttributes $1 $2}

ClassAttribute :
        "[+]" Variable {ClassAttributePublic $2 }
      | "[-]" Variable {ClassAttributePrivate $2 }

ClassFunctions :
        {- empty -} { ClassFunctionsEmpty }
      | ClassFunctions ClassFunction {ClassFunctions $1 $2}

ClassFunction :
        "[+]" Function {ClassFunctionPublic $2}
      | "[-]" Function {ClassFunctionPrivate $2}

ClassConstructor :
        "::" TypeFuncParams var_identifier Params Block {ClassConstructor $2 $3 $4 $5}
      | "::" Block { ClassConstructorEmpty }

LiteralOrVariable :
        var_identifier {VarIdentifier $1}
      | integer_literal {IntegerLiteral $1}
      | decimal_literal {DecimalLiteral $1}
      | string_literal {StringLiteral $1}

Block :
    "{" "}" {Block}

{
parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) 
  in 
  error ("Parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

-- Esta sección tiene las producciones semánticas para producir el árbol abstracto de sintaxis 

data Program 
    = Program FunctionsVariablesClasses Block
  deriving (Show, Eq)

data FunctionsVariablesClasses 
    = FVCFunction FunctionsVariablesClasses Function
    | FVCVariable FunctionsVariablesClasses Variable
    | FVCClass FunctionsVariablesClasses Class
    | FVCEmpty
  deriving (Show, Eq)

data Function 
    = Function String TypeFuncReturn TypeFuncParams String Params Block
    | FunctionEmptyParams String TypeFuncReturn Block
  deriving (Show, Eq)

data Params 
    = ParamsEmpty
    | Params Params TypeFuncParams String
  deriving (Show, Eq)

data TypeFuncReturn 
    = TypeFuncReturnPrimitive Primitive ClosingBracketsNoIdentifier
    | TypeFuncReturnClassId String ClosingBracketsNoIdentifier
    | TypeFuncReturnList ListType
    | TypeFuncReturnNothing
  deriving (Show, Eq)

data TypeFuncParams 
    = TypeFuncParamsPrimitive Primitive ClosingBracketsNoIdentifier
    | TypeFuncParamsClassId String ClosingBracketsNoIdentifier
    | TypeFuncParamsList ListType
  deriving (Show, Eq)

data ClosingBracketsNoIdentifier 
    = ClosingBracketsNoIdentifierEmpty
    | ClosingBracketsNoIdentifier ClosingBracketsNoIdentifier
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
    = TypePrimitive Primitive ArrayIndexes
    | TypeClassId String ArrayIndexes
  deriving (Show, Eq)

data ArrayIndexes 
    = ArrayIndexesEmpty
    | ArrayIndexesIdentifier ArrayIndexes String
    | ArrayIndexesIntLiteral ArrayIndexes Integer
  deriving (Show, Eq)

data ListType 
    = ListTypeClassId String
    | ListTypePrimitive Primitive
  deriving (Show, Eq)

data Variable 
    = VariableNoAssignment Type String VarIdentifiers
    | VariableLiteralOrVariable Type String LiteralOrVariable
    | VariableAssignment1D Type String ArrayAssignment1D
    | VariableAssignment2D Type String ArrayAssignment2D
    | VariableListAssignment ListType String ListAssignment
    | VariableList ListType String
  deriving (Show, Eq)

data VarIdentifiers 
    = VarIdentifiersEmpty
    | VarIdentifiers VarIdentifiers String
  deriving (Show, Eq)

data ArrayAssignment1D 
    = ArrayAssignmentVarIdentifier String Array1DAssignments
    | ArrayAssignmentIntLiteral Integer Array1DAssignments
    | ArrayAssignment1DEmpty
  deriving (Show, Eq)

data Array1DAssignments 
    = Array1DAssignmentsEmpty
    | Array1DAssignmentsInteger Array1DAssignments Integer
    | Array1DAssignmentsVarIdentifier Array1DAssignments String
  deriving (Show, Eq)

data ArrayAssignment2D 
    = ArrayAssignment2D ArrayAssignment1D Array2DAssignments
  deriving (Show, Eq)

data Array2DAssignments 
    = Array2DAssignmentsEmpty
    | Array2DAssignments Array2DAssignments ArrayAssignment1D
  deriving (Show, Eq)

data ListAssignment 
    = ListAssignmentArray ArrayAssignment1D
    | ListAssignmentRange Integer Integer
  deriving (Show, Eq)

data Class 
    = ClassInheritance String String ClassBlock
    | ClassNormal String ClassBlock
  deriving (Show, Eq)

data ClassBlock 
    = ClassBlock ClassAttributes ClassConstructor ClassFunctions
    | ClassBlockEmpty
  deriving (Show, Eq)

data ClassAttributes 
    = ClassAttributesEmpty
    | ClassAttributes ClassAttributes ClassAttribute
  deriving (Show, Eq)

data ClassAttribute 
    = ClassAttributePublic Variable
    | ClassAttributePrivate Variable
  deriving (Show, Eq)

data ClassFunctions 
    = ClassFunctionsEmpty
    | ClassFunctions ClassFunctions ClassFunction
  deriving (Show, Eq)

data ClassFunction 
    = ClassFunctionPublic Function
    | ClassFunctionPrivate Function
  deriving (Show, Eq)

data ClassConstructor 
    = ClassConstructorEmpty
    | ClassConstructor TypeFuncParams String Params Block
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


main = do 
  inStr <- getContents
  putStrLn(show(inStr))
  let parseTree = ooh (alexScanTokens2 inStr)
  putStrLn ("SUCCESS " ++ show(parseTree) )
}
