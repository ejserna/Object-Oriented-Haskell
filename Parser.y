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
          "[" integer_literal "]" ArrayIndexesExpression { (ArrayAccessLiteral $2) : $4 } 
        | "[" var_identifier "]" ArrayIndexesExpression { (ArrayAccessVar $2) : $4 }
        | "[" Expression "]" ArrayIndexesExpression { (ArrayAccessExpression $2) : $4 }   

ListType :
          "List" "of" class_identifier {ListTypeClassId $3}
        | "List" "of" Primitive {ListTypePrimitive $3}

Variable :
          Type var_identifier VarIdentifiers ";" {VariableNoAssignment $1 $2 $3 }
        | Type var_identifier "=" LiteralOrVariable ";" {VariableAssignmentLiteralOrVariable $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment1D ";" {VariableAssignment1D $1 $2 $4 }
        | Type var_identifier "=" ArrayAssignment2D ";" {VariableAssignment2D $1 $2 $4 }
        | Type var_identifier "=" ObjectCreation ";" {VariableAssignmentObject $1 $2 $4 }
        | ListType var_identifier "=" ListAssignment ";" {VariableListAssignment $1 $2 $4}
        | ListType var_identifier VarIdentifiers ";" {VariableListNoAssignment $1 $2 $3}

ObjectCreation : 
          class_identifier "(" LiteralOrVariable ObjectCreationParams")" { ObjectCreation $1 ( (ParamsLiteralOrVariable $3) : $4) }
        | class_identifier "(" ")" { ObjectCreation $1 [] }

ObjectCreationParams :
          {-empty-} {[]}
        | "," LiteralOrVariable ObjectCreationParams { (ParamsLiteralOrVariable $2) : $3} 
        -- | Expression "," ObjectCreationParams { (ParamsExpression $1) : $3} 

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
      | Variable ";"   {VariableStatement $1}
      | Condition  {ConditionStatement $1}
      | Cycle      {CycleStatement $1}

Assignment :
        var_identifier "=" LiteralOrVariable {VarAssignLiteralOrVariable $1 $3}
      | var_identifier "=" Expression        {VarAssignExpression $1 $3}
      | var_identifier "=" FunctionCall      {VarAssignFunctionCall $1 $3}
      | var_identifier "=" ObjectMember      {VarAssignObjMem $1 $3}
      | ObjectMember "=" LiteralOrVariable   {ObjMemAssignLiteralOrVariable $1 $3}
      | ObjectMember "=" Expression          {ObjMemAssignExpression $1 $3}
      | ObjectMember "=" FunctionCall        {ObjMemAssignFunctionCall $1 $3}
      | ObjectMember "=" ObjectMember        {ObjMemAssignObjMem $1 $3}
      | var_identifier ArrayIndexesExpression "=" LiteralOrVariable     {VarArrayAssignLiteralOrVariable $1 $2 $4}
      | var_identifier ArrayIndexesExpression "=" Expression       {VarArrayAssignExpression $1 $2 $4}
      | var_identifier ArrayIndexesExpression "=" FunctionCall     {VarArrayAssignFunctionCall $1 $2 $4}
      | var_identifier ArrayIndexesExpression "=" ObjectMember     {VarArrayAssignObjMem $1 $2 $4}
      | ObjectMember ArrayIndexesExpression "=" LiteralOrVariable     {ObjMemArrayAssignLiteralOrVariable $1 $2 $4}
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
    | Expression "=" Expression { ExpressionEquals $1 $3 }
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
    | var_identifier ArrayIndexesExpression {ExpressionVarArray $1 $2}
    | var_identifier {ExpressionVar $1}
    | integer_literal {ExpressionInt $1}
    | decimal_literal {ExpressionDec $1}
    | "!" Expression  {ExpressionNot $2}
    | "True"          {ExpressionTrue}
    | "False"         {ExpressionFalse}
    | "-" Expression %prec NEG {ExpressionNeg $2}
    | "(" Expression ")" {ExpressionPars $2}

Condition :
      If      {ConditionIf $1}
   {- | Case    {ConditionCase $1} -}

If :
  "if" "(" Expression ")" Block "else" Block {If $3 $5 $7}

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
        ObjectMember "(" FunctionCallParam ")" {FunctionCallObjMem $1 $3}
      | var_identifier "(" FunctionCallParam ")" {FunctionCallVar $1 $3}

FunctionCallParam :
        LiteralOrVariable                       {FunctionCallLitOrVarParam $1}
      | Expression                              {FunctionCallExpParam $1}


ObjectMember :
        var_identifier "." var_identifier {ObjectMember $1 $3}

Return :
        "return" LiteralOrVariable {ReturnLitOrVar $2}
      | "return" FunctionCall      {ReturnFunctionCall $2}
      | "return" Expression        {ReturnExp $2}



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
    | VariableAssignmentLiteralOrVariable Type String LiteralOrVariable
    | VariableAssignment1D Type String [LiteralOrVariable]
    | VariableAssignment2D Type String [[LiteralOrVariable]]
    | VariableAssignmentObject Type String ObjectCreation
    | VariableListAssignment ListType String ListAssignment
    | VariableListNoAssignment ListType String [String]
  deriving (Show, Eq)

data ObjectCreation 
    = ObjectCreation String [Params]
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
    = Block [Statement]
  deriving (Show, Eq)  

data ArrayAccess
    = ArrayAccessLiteral Integer
    | ArrayAccessVar String
    | ArrayAccessExpression Expression
  deriving (Show,Eq)

data Params
    = ParamsLiteralOrVariable LiteralOrVariable
    -- | ParamsExpression Expression
  deriving (Show,Eq)

data Statement
    = AssignStatement Assignment
    | DisplayStatement Display
    | ReadStatement Reading
    | DPMStatement DoublePlusMinus
    | FunctionCallStatement FunctionCall
    | ReturnStatement Return
    | VariableStatement Variable
    | ConditionStatement Condition
    | CycleStatement Cycle
  deriving (Show,Eq)

data Assignment
    = VarAssignLiteralOrVariable String LiteralOrVariable
    | VarAssignExpression String Expression
    | VarAssignFunctionCall String FunctionCall
    | VarAssignObjMem String ObjectMember
    | ObjMemAssignLiteralOrVariable ObjectMember LiteralOrVariable
    | ObjMemAssignExpression ObjectMember Expression
    | ObjMemAssignFunctionCall ObjectMember FunctionCall
    | ObjMemAssignObjMem ObjectMember ObjectMember
    | VarArrayAssignLiteralOrVariable String ArrayAccess LiteralOrVariable
    | VarArrayAssignExpression String [ArrayAccess] Expression
    | VarArrayAssignFunctionCall String [ArrayAccess] FunctionCall
    | VarArrayAssignObjMem String [ArrayAccess] ObjectMember
    | ObjMemArrayAssignLiteralOrVariable ObjectMember [ArrayAccess] LiteralOrVariable
    | ObjMemArrayAssignExpression ObjectMember [ArrayAccess] Expression
    | ObjMemArrayAssignFunctionCall ObjectMember [ArrayAccess] FunctionCall
    | ObjMemArrayAssignObjMem ObjectMember [ArrayAccess] ObjectMember
  deriving(Show,Eq)

data Reading
    = Reading String
  deriving(Show,Eq)

data Display
    = DisplayInt Integer
    | DisplayDec Decimal
    | DisplayString String
    | DisplayVar String
    | DisplayObjMem ObjectMember
    | DisplayFunctionCall FunctionCall
    | DisplayVarArray String [ArrayAccess]
    | DisplayObjMemArray ObjectMember [ArrayAccess]
  deriving(Show,Eq) 

data Expression
    = ExpressionGreater Expression Expression
    | ExpressionLower Expression Expression
    | ExpressionGreaterEq Expression Expression
    | ExpressionLowerEq Expression Expression
    | ExpressionEquals Expression Expression
    | ExpressionEqEq Expression Expression 
    | ExpressionNotEq Expression Expression 
    | ExpressionAnd Expression Expression
    | ExpressionOr Expression Expression 
    | ExpressionPlus Expression Expression 
    | ExpressionMinus Expression Expression
    | ExpressionDiv Expression Expression
    | ExpressionMult Expression Expression
    | ExpressionPow Expression Expression
    | ExpressionMod Expression Expression
    | ExpressionVarArray String [ArrayAccess]
    | ExpressionVar String
    | ExpressionInt Integer
    | ExpressionDec Decimal
    | ExpressionNot Expression 
    | ExpressionTrue
    | ExpressionFalse 
    | ExpressionNeg Expression 
    | ExpressionPars Expression 
  deriving(Show, Eq)

data Condition
    = ConditionIf If
  deriving(Show,Eq)

data If
    = If Expression Block Block
  deriving(Show,Eq)

data Cycle
    = CycleWhile While
    | CycleFor For
  deriving(Show,Eq)

data While
    = While Expression Block
  deriving(Show,Eq)

data For
    = For Integer Integer
  deriving(Show,Eq)

data DoublePlusMinus
    = DoublePP String
    | DoubleMM String
  deriving(Show,Eq)

data FunctionCall
    = FunctionCallObjMem ObjectMember FunctionCallParam
    | FunctionCallVar String FunctionCallParam
  deriving(Show,Eq)

data FunctionCallParam
    = FunctionCallLitOrVarParam LiteralOrVariable
    | FunctionCallExpParam Expression 
    | FunctionCallLitOrVarMult LiteralOrVariable
    | FunctionCallExpMult Expression
  deriving(Show,Eq)

data ObjectMember
    = ObjectMember String String
  deriving(Show,Eq)

data Return
    = ReturnLitOrVar LiteralOrVariable
    | ReturnFunctionCall FunctionCall
    | ReturnExp Expression 
  deriving(Show,Eq)


main = do 
  inStr <- getContents
  putStrLn(show(inStr))
  let parseTree = ooh (alexScanTokens2 inStr)
  putStrLn ("SUCCESS " ++ show(parseTree) )
  putStrLn $ ppShow $ parseTree
}
