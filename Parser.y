{- 
  Patito.y

  Archivo de gramática de Happy, el cual es un generador de parsers basado en Yacc.
  El archivo contiene la gramática del lenguaje LittleDuck2018. Para poder correrlo, se ocupa GHCI instalado, así como Alex y Happy.

  Autor: Eduardo Serna A01196007
-}

{
module Main where
import Scanner
import DataTypes
import Data.Decimal
import Text.Show.Pretty
import TypeChecker
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

  %left     "||"
  %left     "&&"

  -- -- -- Compare
  %left     "=="
  %nonassoc "<" "<=" ">" ">=" "!=" "!"

  -- -- Arithmetic
  %left     "+" "-"
  %left     "*" "/" "%"
  %right    "^"
  %left NEG
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
          "[" Expression "]"  { [(ArrayAccessExpression $2)]  }   
        | "[" Expression "]" "[" Expression "]" { (ArrayAccessExpression $2) : (ArrayAccessExpression $5) : [] }

ListType :
          "List" "of" class_identifier {ListTypeClassId $3}
        | "List" "of" Primitive {ListTypePrimitive $3}

Variable :
          Type var_identifier VarIdentifiers ";" {VariableNoAssignment $1 ($2:$3) }
        | Type var_identifier "=" LiteralOrVariable ";" {VariableAssignmentLiteralOrVariable $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment1D ";" {VariableAssignment1D $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment2D ";" {VariableAssignment2D $1 $2 $4 }
        | Type var_identifier "=" ObjectCreation ";" {VariableAssignmentObject $1 $2 $4 }
        | ListType var_identifier "=" ListAssignment ";" {VariableListAssignment $1 $2 $4}
        | ListType var_identifier VarIdentifiers ";" {VariableListNoAssignment $1 ($2:$3)}

ObjectCreation : 
          class_identifier "(" Expression CallParams ")" { ObjectCreation $1 ((ParamsExpression $3) : $4) }
        | class_identifier "("  ")" { ObjectCreation $1 [] }

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
    "{" BlockStatement "}" {Block $2}

BlockStatement :
        {- empty -} { [] }
      | Statement BlockStatement {$1 : $2}

Statement :
        Assignment ";" {AssignStatement $1}
      | Display ";"    {DisplayStatement $1}
      | Reading ";"       {ReadStatement $1}
      | DoublePlusMinus ";" {DPMStatement $1}
      | FunctionCall ";"     {FunctionCallStatement $1}
      | Return ";"     {ReturnStatement $1}
      | Variable   {VariableStatement $1}
      | Condition  {ConditionStatement $1}
      | Cycle      {CycleStatement $1}

Assignment :
        var_identifier "=" Expression        {VarAssignExpression $1 $3}
      | var_identifier "=" FunctionCall      {VarAssignFunctionCall $1 $3}
      | var_identifier "=" ObjectMember      {VarAssignObjMem $1 $3}
      | ObjectMember "=" Expression   {ObjMemAssignExpression $1 $3}
      | ObjectMember "=" FunctionCall        {ObjMemAssignFunctionCall $1 $3}
      | ObjectMember "=" ObjectMember        {ObjMemAssignObjMem $1 $3}
      | var_identifier ArrayIndexesExpression "=" Expression     {VarArrayAssignExpression $1 $2 $4}
      | var_identifier ArrayIndexesExpression "=" FunctionCall       {VarArrayAssignFunctionCall $1 $2 $4}
      | var_identifier ArrayIndexesExpression "=" ObjectMember     {VarArrayAssignObjMem $1 $2 $4}
      | ObjectMember ArrayIndexesExpression "=" Expression     {ObjMemArrayAssignExpression $1 $2 $4}
      | ObjectMember ArrayIndexesExpression "=" FunctionCall     {ObjMemArrayAssignFunctionCall $1 $2 $4}
      | ObjectMember ArrayIndexesExpression "=" ObjectMember     {ObjMemArrayAssignObjMem $1 $2 $4}

Reading :
  "read" "(" var_identifier ")" {Reading $3}

Display :
        "display" "(" integer_literal ")" {DisplayInt $3}
      | "display" "(" decimal_literal ")" {DisplayDec $3}
      | "display" "(" string_literal ")"  {DisplayString $3}
      | "display" "(" var_identifier ")"  {DisplayVar $3}
      | "display" "(" ObjectMember ")"    {DisplayObjMem $3}
      | "display" "(" FunctionCall ")"    {DisplayFunctionCall $3}
      | "display" "(" var_identifier ArrayIndexesExpression ")" {DisplayVarArray $3 $4}
      | "display" "(" ObjectMember ArrayIndexesExpression ")" {DisplayObjMemArray $3 $4}

Expression :
      Expression ">" Expression { ExpressionGreater $1 $3 }
    | Expression "<" Expression { ExpressionLower $1 $3 }
    | Expression ">=" Expression { ExpressionGreaterEq $1 $3 }
    | Expression "<=" Expression { ExpressionLowerEq $1 $3 }
    | Expression "==" Expression { ExpressionEqEq $1 $3 }
    | Expression "!=" Expression { ExpressionNotEq $1 $3 }
    | Expression "&&" Expression { ExpressionAnd $1 $3 }
    | Expression "||" Expression { ExpressionOr $1 $3 }
    | Expression "+" Expression {ExpressionPlus $1 $3}
    | Expression "-" Expression {ExpressionMinus $1 $3}
    | Expression "/" Expression {ExpressionDiv $1 $3}
    | Expression "*" Expression {ExpressionMult $1 $3}
    | Expression "^" Expression {ExpressionPow $1 $3}
    | Expression "%" Expression {ExpressionMod $1 $3}
    | LiteralOrVariable {ExpressionLitVar $1}
    | var_identifier ArrayIndexesExpression {ExpressionVarArray $1 $2}
    | "!" Expression  {ExpressionNot $2}
    | "True"          {ExpressionTrue}
    | "False"         {ExpressionFalse}
    -- | "-" Expression %prec NEG {ExpressionNeg $2}
    | "(" Expression ")" {ExpressionPars $2}

Condition :
      If      {ConditionIf $1}
   {- | Case    {ConditionCase $1} -}

If :
  "if" "(" Expression ")" Block Else {If $3 $5 $6}

Else :
     {- empty -}  {NoElse}
   | "else" Block {Else $2}

{-Case :
    "case" var_identifier "of" "{" CaseBlock "otherwise" "=>" CaseStatement "end" ";" "}"

CaseBlock :

  | LiteralOrVariable "=>" CaseStatement "end" ";" CaseBlock

CaseStatement :
    
    | Statement CaseStatement -}


Cycle :
    While {CycleWhile $1}
  | For   {CycleFor $1}

While :
    "while" "(" Expression ")" Block {While $3 $5}

For :
    "for" "(" integer_literal ".." integer_literal ")" Block {For $3 $5}

DoublePlusMinus :
        var_identifier "++" {DoublePP $1}
      | var_identifier "--" {DoubleMM $1}

FunctionCall :
        ObjectMember "(" Expression CallParams")" {FunctionCallObjMem $1 ((ParamsExpression $3) : $4)  }--FunctionCallObjMem $1 $3}
      | var_identifier "("  Expression  CallParams")" {FunctionCallVar $1  ((ParamsExpression $3) : $4) }--FunctionCallVar $1 $3}
      | ObjectMember "(" ")" {FunctionCallObjMem $1 [] }--FunctionCallObjMem $1 $3}
      | var_identifier "(" ")" {FunctionCallVar $1 [] }--FunctionCallVar $1 $3}

CallParams :
      {- empty -}   { []  }
     | "," Expression CallParams   { (ParamsExpression $2) : $3 }

ObjectMember :
        var_identifier "." var_identifier {ObjectMember $1 $3}

Return :
        "return" FunctionCall      {ReturnFunctionCall $2}
      | "return" Expression        {ReturnExp $2}

{
parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) 
  in 
  error ("Parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

main = do 
  inStr <- getContents
  let parseTree = ooh (alexScanTokens2 inStr)
  -- putStrLn ("SUCCESS " ++ show(parseTree) )
  putStrLn $ ppShow $ parseTree

  startSemanticAnalysis parseTree
}
