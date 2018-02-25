module DataTypes where
import Data.Decimal
-- Esta sección tiene las producciones semánticas para producir el árbol abstracto de sintaxis 
type Identifier = String
type ClassIdentifier = String

data Program 
    = Program [Class] [Function] [Variable] Block
  deriving (Show, Eq)

data Function 
    = Function Identifier TypeFuncReturn [(TypeFuncParams,Identifier)] Block
    | FunctionEmptyParams Identifier TypeFuncReturn Block
  deriving (Show, Eq)

data TypeFuncReturn 
    = TypeFuncReturnPrimitive Primitive 
    | TypeFuncReturnClassId ClassIdentifier
    | TypeFuncReturnNothing
  deriving (Show, Eq)

data TypeFuncParams 
    = TypeFuncParamsPrimitive Primitive [(String,Integer,String)]
    | TypeFuncParamsClassId ClassIdentifier [(String,Integer,String)]
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
    | TypeClassId ClassIdentifier [(String,Integer,String)] 
  deriving (Show, Eq)

data ListType 
    = ListTypeClassId ClassIdentifier
    | ListTypePrimitive Primitive
  deriving (Show, Eq)

data Variable 
    = VariableNoAssignment Type [Identifier]
    | VariableAssignmentLiteralOrVariable Type Identifier LiteralOrVariable
    | VariableAssignment1D Type Identifier [LiteralOrVariable]
    | VariableAssignment2D Type Identifier [[LiteralOrVariable]]
    | VariableAssignmentObject Type Identifier ObjectCreation
    | VariableListAssignment ListType Identifier ListAssignment
    | VariableListNoAssignment ListType [Identifier]
  deriving (Show, Eq)

data ObjectCreation 
    = ObjectCreation ClassIdentifier [Params]
  deriving (Show, Eq)

data ListAssignment 
    = ListAssignmentArray [LiteralOrVariable]
    | ListAssignmentRange Integer Integer
  deriving (Show, Eq)

data Class 
    = ClassInheritance ClassIdentifier ClassIdentifier ClassBlock
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
    | ClassConstructor [(TypeFuncParams,Identifier)] Block
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
    | ArrayAccessVar Identifier
    | ArrayAccessExpression Expression
  deriving (Show,Eq)

data Params
    = ParamsLiteralOrVariable LiteralOrVariable
    | ParamsExpression Expression
    | ParamsStringLiteral String
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
    = VarAssignExpression Identifier Expression
    | VarAssignFunctionCall Identifier FunctionCall
    | VarAssignObjMem Identifier ObjectMember
    | ObjMemAssignExpression ObjectMember Expression
    | ObjMemAssignFunctionCall ObjectMember FunctionCall
    | ObjMemAssignObjMem ObjectMember ObjectMember
    | VarArrayAssignExpression Identifier [ArrayAccess] Expression
    | VarArrayAssignFunctionCall Identifier [ArrayAccess] FunctionCall
    | VarArrayAssignObjMem Identifier [ArrayAccess] ObjectMember
    | ObjMemArrayAssignExpression ObjectMember [ArrayAccess] Expression
    | ObjMemArrayAssignFunctionCall ObjectMember [ArrayAccess] FunctionCall
    | ObjMemArrayAssignObjMem ObjectMember [ArrayAccess] ObjectMember
  deriving(Show,Eq)

data Reading
    = Reading Identifier
  deriving(Show,Eq)

data Display
    = DisplayInt Integer
    | DisplayDec Decimal
    | DisplayString String
    | DisplayVar Identifier
    | DisplayObjMem ObjectMember
    | DisplayFunctionCall FunctionCall
    | DisplayVarArray Identifier [ArrayAccess]
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
    | ExpressionVarArray Identifier [ArrayAccess]
    | ExpressionNot Expression 
    | ExpressionLitVar LiteralOrVariable
    | ExpressionTrue
    | ExpressionFalse 
    | ExpressionNeg Expression 
    | ExpressionPars Expression 
  deriving(Show, Eq)

data Condition
    = ConditionIf If
  deriving(Show,Eq)

data If
    = If Expression Block Else
  deriving(Show,Eq)

data Else
    = NoElse
    | Else Block
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
    = DoublePP Identifier
    | DoubleMM Identifier
  deriving(Show,Eq)

data FunctionCall
    = FunctionCallObjMem ObjectMember [Params]
    | FunctionCallVar String [Params]
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
