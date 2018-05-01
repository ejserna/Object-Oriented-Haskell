module DataTypes where
import Data.Decimal
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate)
import Quadruple

-- Esta sección tiene las producciones semánticas para producir el árbol abstracto de sintaxis 
type Identifier = String
type ClassIdentifier = String
type AncestorsMap = Map.HashMap ClassIdentifier [ClassIdentifier]

data Program 
    = Program [Class] [Function] [Variable] Block
  deriving (Show, Eq)

data Function 
    = Function Identifier TypeFuncReturn [(Type,Identifier)] Block
  deriving (Show, Eq)

data TypeFuncReturn 
    = TypeFuncReturnPrimitive Primitive [(String,Integer,String)] 
    | TypeFuncReturnClassId ClassIdentifier [(String,Integer,String)]
    | TypeFuncReturnNothing
  deriving (Show, Eq)

data Primitive 
    = PrimitiveInt
    | PrimitiveDouble
    | PrimitiveMoney
    | PrimitiveString
    | PrimitiveBool
    | PrimitiveInteger
  deriving (Show)

instance Eq Primitive where 
   PrimitiveInt == PrimitiveInteger  =  True
   PrimitiveInteger == PrimitiveInt  =  True
   PrimitiveDouble == PrimitiveMoney  =  True
   PrimitiveMoney == PrimitiveDouble  =  True
   PrimitiveInt == PrimitiveInt  =  True
   PrimitiveInteger == PrimitiveInteger  =  True
   PrimitiveBool == PrimitiveBool  =  True
   PrimitiveString == PrimitiveString  =  True
   PrimitiveMoney == PrimitiveMoney  =  True
   PrimitiveDouble == PrimitiveDouble  =  True
   _ == _ = False

data Type 
    = TypePrimitive Primitive [(String,Integer,String)]
    | TypeClassId ClassIdentifier [(String,Integer,String)]
    | TypeListClassId ClassIdentifier
    | TypeListPrimitive Primitive
  deriving (Show, Eq)

data Variable 
    = VariableNoAssignment Type [Identifier]
    | VariableAssignmentLiteralOrVariable Type Identifier LiteralOrVariable
    | VariableAssignment1D Type Identifier [LiteralOrVariable]
    | VariableAssignment2D Type Identifier [[LiteralOrVariable]]
    | VariableAssignmentObject Type Identifier ObjectCreation
    | VariableListAssignment Type Identifier ListAssignment
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
    | ClassNormal ClassIdentifier ClassBlock
  deriving (Show, Eq)

data ClassBlock 
    = ClassBlock [ClassMember] ClassConstructor
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
    = ClassConstructorEmpty ClassIdentifier
    | ClassConstructor ClassIdentifier [(Type,Identifier)] Block
  deriving (Show, Eq)

data LiteralOrVariable 
    = VarIdentifier Identifier
    | IntegerLiteral Integer
    | DecimalLiteral Decimal
    | StringLiteral String
    | BoolLiteral Bool
  deriving (Eq)

  -- deriving (Show,Eq)

instance Show LiteralOrVariable where
    show (VarIdentifier identifier) = id identifier
    show (IntegerLiteral integer) =  id $ show integer
    show (DecimalLiteral dec) =  id $ show dec
    show (StringLiteral str) = id $ str
    show (BoolLiteral bool) = id $ show bool

data Block 
    = Block [Statement]
  deriving (Show, Eq)  

data ArrayAccess
    = ArrayAccessExpression Expression
  deriving (Show,Eq)



data Params
    = ParamsExpression Expression
  deriving (Show,Eq)

data Statement
    = AssignStatement Assignment
    | DisplayStatement [Display]
    | ReadStatement Reading
    | DPMStatement Assignment
    | FunctionCallStatement FunctionCall
    | ReturnStatement Return
    | VariableStatement Variable
    | ConditionStatement If
    | CycleStatement Cycle
    | CaseStatement Case
  deriving (Show,Eq)

data Assignment
    = AssignmentExpression Identifier Expression
    | AssignmentFunctionCall Identifier FunctionCall
    | AssignmentObjectMember Identifier ObjectMember
    | AssignmentObjectMemberExpression ObjectMember Expression
    | AssignmentArrayExpression Identifier [ArrayAccess] Expression
  deriving(Show,Eq)

data Reading
    = Reading Identifier
  deriving(Show,Eq)

data Display
    = DisplayLiteralOrVariable LiteralOrVariable Operation 
    | DisplayObjMem ObjectMember Operation
    | DisplayFunctionCall FunctionCall Operation
    | DisplayVarArrayAccess Identifier [ArrayAccess] Operation
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
    | ExpressionNeg Expression 
    | ExpressionPars Expression
    | ExpressionFuncCall FunctionCall
  deriving(Show, Eq)


class ExpressionOperation a where
   (|+|) :: a -> a -> a
   (|*|) :: a -> a -> a
   (|-|) :: a -> a -> a
   (|/|) :: a -> a -> a
   (|^|) :: a -> a -> a
   (|==|) :: a -> a -> a
   (|!=|) :: a -> a -> a
   (|&&|) :: a -> a -> a
   (|-||-|) :: a -> a -> a
   (|%|) :: a -> a -> a
   (|!|) :: a -> a
   (|>|) :: a -> a -> a
   (|<|) :: a -> a -> a
   (|>=|) :: a -> a -> a
   (|<=|) :: a -> a -> a

class TypeSemant a where
   semantarithmetic :: a -> a -> (Either String Type)
   semantadd :: a -> a -> (Either String Type)
   semantmod :: a -> a -> (Either String Type)
   semantneg :: a -> (Either String Type)
   semantequivalence :: a -> a -> (Either String Type)
   semantbooleanrelational :: a -> a -> (Either String Type)
   semantrelational :: a -> a -> (Either String Type)
   semantnot :: a -> (Either String Type)

instance ExpressionOperation Expression where
   a |+| b = (ExpressionPlus a b)
   a |*| b = (ExpressionMult a b)
   a |-| b = (ExpressionMinus a b)
   a |/| b = (ExpressionDiv a b)
   a |^| b = (ExpressionPow a b)
   a |==| b = (ExpressionEqEq a b)
   a |!=| b = (ExpressionNotEq a b)
   a |&&| b = (ExpressionAnd a b)
   a |-||-| b = (ExpressionOr a b)
   a |>| b = (ExpressionGreater a b)
   a |<| b = (ExpressionLower a b)
   a |>=| b = (ExpressionGreaterEq a b)
   a |<=| b = (ExpressionLowerEq a b)
   a |%| b = (ExpressionMod a b)
   (|!|) a   = (ExpressionNot a)
   

data If
    = If Expression Block
    | IfElse Expression Block Block
  deriving(Show,Eq)

data Case
    = Case Expression [(Expression,[Statement])] [Statement]
  deriving (Show,Eq)

data Cycle
    = CycleWhile While
    | CycleFor For
    | CycleForVar [Statement]
  deriving(Show,Eq)

data While
    = While Expression Block
  deriving(Show,Eq)

data For
    = For Integer Integer Block
  deriving(Show,Eq)


data FunctionCall
    = FunctionCallObjMem ObjectMember [Params]
    | FunctionCallVar Identifier [Params]
  deriving(Show,Eq)

data ObjectMember
    = ObjectMember Identifier Identifier
  deriving(Show,Eq)

data Return
    = ReturnFunctionCall FunctionCall
    | ReturnExp Expression 
  deriving(Show,Eq)

intToDecimal :: Integer -> Decimal
intToDecimal int = let num = show(int)
                    in (strToDecimal num)

strToDecimal :: String -> Decimal
strToDecimal str = read str :: Decimal

decToDouble :: (DecimalRaw Integer) -> Double 
decToDouble dec = let num = show(dec)
                    in (strToDouble num)

strToDouble :: String -> Double
strToDouble str = read str :: Double

intToDouble :: Integer -> Double
intToDouble int = let num = show(int)
                  in (strToDouble num)


doubleToDecimal :: Double -> Decimal
doubleToDecimal doub = let num = show(doub)
                        in (strToDecimal num)

decToInt :: Decimal -> Integer
decToInt dec = let num = show(roundTo 0 dec)
                  in (strToInt num)

strToInt :: String -> Integer
strToInt str = read str :: Integer 
