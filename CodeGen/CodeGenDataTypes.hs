module CodeGenDataTypes where
import Data.Decimal
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate,findIndex)
import DataTypes
import SymbolTable
import Quadruple
import ClassSymbolTable
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import Text.Show.Pretty


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
                    paramsAddresses :: [Address], -- Params tiene una lista de direcciones que corresponden a direcciones locales de la funcion. Es decir
                                                  -- Nos dicen cómo están representados internamente en la función
                    funcIdMap :: IdentifierAddressMap,
                    funcObjMap :: ObjectAddressMap              
                }

instance Show FunctionData where
    show fd = case fd of
        FunctionData instructions paramsAddresses funcIdMap funcObjMap  -> "Function Data  "  ++ intercalate ", " [ppShow instructions, ppShow paramsAddresses, ppShow funcIdMap, ppShow funcObjMap] ++ "\n\n\n"

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

onlyAttributes :: Symbol -> Bool
onlyAttributes (SymbolFunction _ _ _ _ _ _ _) = False
onlyAttributes _ = True

getClassNameFromCurrentModule :: String -> String
getClassNameFromCurrentModule currentModule = let currentModule1 = drop 1 currentModule
                                                  numToTakeSecond = findIndex(`elem` "_") currentModule1
                                                  className = take (fromJust numToTakeSecond) currentModule1
                                              in className  


-- getAttributesOfCurrentClass :: String -> ClassSymbolTable -> [Identifier]
-- getAttributesOfCurrentClass currentModule classSymTab = 
--                                             let className = getClassNameFromCurrentModule currentModule
--                                             in case (Map.lookup className classSymTab) of 
--                                                 Just symTableOfClass -> let filteredSymTab = (Map.filter onlyAttributes symTableOfClass)
--                                                                             attributesList = (Map.toList filteredSymTab)
--                                                                             identifiers = (map (\f -> fst f) attributesList)
--                                                                          in identifiers
--                                                 _ -> []

getAttributesOfCurrentClass :: String -> ClassSymbolTable -> [Identifier]
getAttributesOfCurrentClass currentModule classSymTab = let attributesList = getAttributesOfCurrentClassWithSymbol currentModule classSymTab
                                                            identifiers = (map (\f -> fst f) attributesList)
                                                         in identifiers


getAttributesOfCurrentClassWithSymbol :: String -> ClassSymbolTable -> [(Identifier,Symbol)]
getAttributesOfCurrentClassWithSymbol currentModule classSymTab = 
                                            let className = getClassNameFromCurrentModule currentModule
                                            in case (Map.lookup className classSymTab) of 
                                                Just symTableOfClass -> let filteredSymTab = (Map.filter onlyAttributes symTableOfClass)
                                                                            attributesList = (Map.toList filteredSymTab)
                                                                        in attributesList
                                                _ -> []

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
