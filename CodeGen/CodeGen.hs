module CodeGen where 
import Data.Decimal
import DataTypes
import Quadruple
import SymbolTable
import ClassSymbolTable
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map

type TypeIdentifier = String -- Integer, Decimal, String, Bool

type Memory = Map.HashMap TypeIdentifier MemoryMap

type MemoryMap = Map.HashMap Address (Maybe LiteralOrVariable)

-- Estos tipos le sirven a ExpressionCodeGen saber qué Identifiador/Constante están mappeados en memorias con qué dirección
type IdentifierAddressMap = Map.HashMap Identifier Address
type ConstantAddressMap = Map.HashMap String Address

startCodeGen :: Program -> SymbolTable -> ClassSymbolTable -> [Quadruple]
startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab = []


