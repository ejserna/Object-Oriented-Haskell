module CodeGenDataTypes where
import Data.Decimal
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate)
import DataTypes
import SymbolTable
import ClassSymbolTable
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad

type Address = Integer
type QuadNum = Integer

-- Los primeros 4 son los contadores de variables de tipo Integers,Decimales,Strings,Bool 
type VariableCounters = (Address,Address,Address,Address,Address) 

-- Contadores de literales de integers,decimales,strings y booleanos
type LiteralCounters = (Address,Address,Address,Address) 

type TypeIdentifier = String -- Integer, Decimal, String, Bool

-- Estos tipos le sirven a ExpressionCodeGen saber qué Identifiador/Constante están mappeados en memorias con qué dirección
type IdentifierAddressMap = Map.HashMap Identifier Address
type ConstantAddressMap = Map.HashMap String Address
type ObjectAddressMap = Map.HashMap Address IdentifierAddressMap
type FunctionMap = Map.HashMap String FunctionData

data FunctionData = FunctionData
                {   instructions :: [Quadruple], 
                    paramsAddresses :: [Address] -- Params tiene una lista de direcciones que corresponden a direcciones locales de la funcion. Es decir
                                                  -- Nos dicen cómo están representados internamente en la función
                }
                deriving (Show)

data Quadruple = 
    QuadrupleThreeAddresses  QuadNum Operation Address Address Address -- + 1002 1003 1004
  | QuadrupleTwoAddresses QuadNum Operation Address Address -- Assignment 1002 _ 1003
  | QuadrupleOneAddressOneQuad QuadNum Operation Address QuadNum -- GOTOF 1002 _ 8
  | QuadrupleOneQuad QuadNum Operation QuadNum -- GOTO _ _ 8
  | QuadrupleOneAddress QuadNum Operation Address -- GOTO _ _ 8
  | QuadrupleEmpty QuadNum Operation  -- No Operation
  | QuadrupleTwoAddressesOneQuad QuadNum Operation Address Address QuadNum -- FOR 6002 6003 SALTO

instance Show Quadruple where
    show (QuadrupleTwoAddresses quadNum ACCESS_INDEX address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show ACCESS_INDEX,"[" ++ (show address1) ++ "]", id "_" ,show address2]
    show (QuadrupleTwoAddresses quadNum PUT_INDEX address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show PUT_INDEX,show address1, id "_" ,"[" ++ (show address2) ++ "]"]
    show (QuadrupleThreeAddresses quadNum ADD_INDEX address1 address2 address3) = show quadNum ++ ". " ++ intercalate "\t" [show ADD_INDEX, "[" ++ (show address1) ++ "]", show address2, show address3]
    show (QuadrupleThreeAddresses quadNum op address1 address2 address3) = show quadNum ++ ". " ++ intercalate "\t" [show op,show address1, show address2, show address3]
    show (QuadrupleTwoAddresses quadNum op address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show address2]
    show (QuadrupleOneAddressOneQuad quadNum op address1 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show quadNumAssigned]
    show (QuadrupleOneQuad quadNum op quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show quadNumAssigned]
    show (QuadrupleOneAddress quadNum DISPLAY_VALUE_IN_INDEX address) = show quadNum ++ ". "  ++ intercalate "\t" [show DISPLAY_VALUE_IN_INDEX,id "_", id "_" , "[" ++ (show address) ++ "]" ]
    show (QuadrupleOneAddress quadNum op address) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show address]
    show (QuadrupleEmpty quadNum op) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,id "_"]
    show (QuadrupleTwoAddressesOneQuad quadNum op address1 address2 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, show address2 ,show quadNumAssigned]



data CGEnvironment = CGEnvironment
                {   classTab :: ClassSymbolTable,
                    objAddressMap :: ObjectAddressMap,
                    idAddressMap :: IdentifierAddressMap, 
                    constAddressMap :: ConstantAddressMap,
                    funcMap :: FunctionMap,
                    currentModule :: String -- Nos dice cuál es el módulo actual. Main, class Humano, etc...
                    
                }
                deriving (Show)

data CGState = CGState
                {   symTab :: SymbolTable, 
                    varCounters :: VariableCounters,
                    currentQuadNum :: QuadNum
                }
                deriving (Show)


setCGState :: SymbolTable -> VariableCounters -> QuadNum -> CGState
setCGState s v q = CGState s v q

setCGEnvironment :: ClassSymbolTable -> ObjectAddressMap -> IdentifierAddressMap -> ConstantAddressMap -> FunctionMap -> String -> CGEnvironment
setCGEnvironment c om im cm fm m = CGEnvironment c om im cm fm m

getCGState :: CGState -> (SymbolTable,VariableCounters,QuadNum)
getCGState (CGState s v q) = (s,v,q) 

getCGEnvironment :: CGEnvironment -> (ClassSymbolTable,ObjectAddressMap,IdentifierAddressMap,ConstantAddressMap,FunctionMap,String)
getCGEnvironment (CGEnvironment c om im cm fm m) = (c,om,im,cm,fm,m)

type CodeGen a =  RWST CGEnvironment [Quadruple] CGState IO a

type CG =  CodeGen ()

startIntGlobalMemory = 1
endIntGlobalMemory = 100000000000000000000000000000000000000000000000000

startDecimalGlobalMemory = 100000000000000000000000000000000000000000000000001
endDecimalGlobalMemory = 200000000000000000000000000000000000000000000000000

startStringGlobalMemory = 200000000000000000000000000000000000000000000000001
endStringGlobalMemory = 300000000000000000000000000000000000000000000000000

startBoolGlobalMemory = 300000000000000000000000000000000000000000000000001
endBoolGlobalMemory = 400000000000000000000000000000000000000000000000000

startIntLocalMemory = 400000000000000000000000000000000000000000000000001
endIntLocalMemory = 500000000000000000000000000000000000000000000000000

startDecimalLocalMemory = 500000000000000000000000000000000000000000000000001
endDecimalLocalMemory = 600000000000000000000000000000000000000000000000000

startStringLocalMemory = 600000000000000000000000000000000000000000000000001
endStringLocalMemory = 700000000000000000000000000000000000000000000000000

startBoolLocalMemory = 700000000000000000000000000000000000000000000000001
endBoolLocalMemory = 800000000000000000000000000000000000000000000000000

startIntLiteralMemory = 800000000000000000000000000000000000000000000000001
endIntLiteralMemory = 900000000000000000000000000000000000000000000000000

startDecimalLiteralMemory = 900000000000000000000000000000000000000000000000001
endDecimalLiteralMemory = 1000000000000000000000000000000000000000000000000000

startStringLiteralMemory = 1000000000000000000000000000000000000000000000000001
endStringLiteralMemory = 1100000000000000000000000000000000000000000000000000

startBoolLiteralMemory = 1100000000000000000000000000000000000000000000000001
endBoolLiteralMemory = 1200000000000000000000000000000000000000000000000000

startObjectLocalMemory = 1200000000000000000000000000000000000000000000000001
endObjectLocalMemory = 1300000000000000000000000000000000000000000000000000

startObjectGlobalMemory = 1300000000000000000000000000000000000000000000000001
endObjectGlobalMemory = 1400000000000000000000000000000000000000000000000000

-- startIntGlobalMemory = 1
-- endIntGlobalMemory = 4000

-- startDecimalGlobalMemory = 4001
-- endDecimalGlobalMemory = 8000

-- startStringGlobalMemory = 8001
-- endStringGlobalMemory = 12000

-- startBoolGlobalMemory = 12001
-- endBoolGlobalMemory = 16000

-- startIntLocalMemory = 16001
-- endIntLocalMemory = 20000

-- startDecimalLocalMemory = 20001
-- endDecimalLocalMemory = 24000

-- startStringLocalMemory = 24001
-- endStringLocalMemory = 26000

-- startBoolLocalMemory = 26001
-- endBoolLocalMemory = 30000

-- startIntLiteralMemory = 64001
-- endIntLiteralMemory = 68000

-- startDecimalLiteralMemory = 68001
-- endDecimalLiteralMemory = 72000

-- startStringLiteralMemory = 76001
-- endStringLiteralMemory = 80000

-- startBoolLiteralMemory = 80001
-- endBoolLiteralMemory = 84000

-- startObjectLocalMemory = 84001
-- endObjectLocalMemory = 88000

-- startObjectGlobalMemory = 100001
-- endObjectGlobalMemory = 104000
