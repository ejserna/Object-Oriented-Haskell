module CodeGenDataTypes where
import Data.Decimal
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Ordered as OMap
import Data.List (intercalate,findIndex)
import DataTypes
import SymbolTable
import Quadruple
import MemoryLimits
import ClassSymbolTable
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import Text.Show.Pretty


-- Los primeros  son los contadores de variables de tipo Integers,Decimales,Strings,Bool
-- El ultimo contador es para los objetos 
type VariableCounters = (Address,Address,Address,Address,Address) 

-- Contadores de literales de integers,decimales,strings y booleanos
type LiteralCounters = (Address,Address,Address,Address) 

type TypeIdentifier = String -- Integer, Decimal, String, Bool

-- Estos tipos le sirven a ExpressionCodeGen saber qué Identifiador/Constante están mappeados en memorias con qué dirección
type IdentifierAddressMap = OMap.OMap Identifier Address
type ConstantAddressMap = Map.HashMap String Address
type ObjectAddressMap = Map.HashMap Address IdentifierAddressMap
type FunctionMap = Map.HashMap String FunctionData
-- El typemap sirve para saber qué dirección de objeto es qué tipo. Esto sirve para tener
-- polimorfismo en ejecución
type TypeMap = Map.HashMap Address String



data FunctionData = FunctionData
                {   instructions :: [Quadruple], 
                    paramsAddresses :: [Address], -- Params tiene una lista de direcciones que corresponden a direcciones locales de la funcion. Es decir
                                                  -- Nos dicen cómo están representados internamente en la función
                    funcIdMap :: IdentifierAddressMap,
                    funcObjMap :: ObjectAddressMap,
                    funcTypeMap :: TypeMap              
                }

instance Show FunctionData where
    show fd = case fd of
        FunctionData instructions paramsAddresses funcIdMap funcObjMap funcTypeMap  -> "Function Data  "  ++ intercalate ", " [ppShow instructions, ppShow paramsAddresses, ppShow funcIdMap, ppShow funcObjMap, ppShow funcTypeMap] ++ "\n\n\n"

data CGEnvironment = CGEnvironment
                {   classTab :: ClassSymbolTable,
                    objAddressMap :: ObjectAddressMap,
                    idAddressMap :: IdentifierAddressMap, 
                    constAddressMap :: ConstantAddressMap,
                    funcMap :: FunctionMap,
                    currentModule :: String, -- Nos dice cuál es el módulo actual. Main, class Humano, etc...
                    cgAMap :: AncestorsMap 
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

setCGEnvironment :: ClassSymbolTable -> ObjectAddressMap -> IdentifierAddressMap -> ConstantAddressMap -> FunctionMap -> String -> AncestorsMap -> CGEnvironment
setCGEnvironment c om im cm fm m a = CGEnvironment c om im cm fm m a

getCGState :: CGState -> (SymbolTable,VariableCounters,QuadNum)
getCGState (CGState s v q) = (s,v,q) 

getCGEnvironment :: CGEnvironment -> (ClassSymbolTable,ObjectAddressMap,IdentifierAddressMap,ConstantAddressMap,FunctionMap,String,AncestorsMap)
getCGEnvironment (CGEnvironment c om im cm fm m a) = (c,om,im,cm,fm,m,a)

type CodeGen a =  RWST CGEnvironment [Quadruple] CGState IO a

type CG =  CodeGen ()

onlyAttributes :: Symbol -> Bool
onlyAttributes (SymbolFunction _ _ _ _ _ _ _) = False
onlyAttributes _ = True

onlyRecursiveAttributes :: Symbol -> Symbol -> Bool
onlyRecursiveAttributes (SymbolFunction _ _ _ _ _ _ _) _ = False
onlyRecursiveAttributes _ (SymbolFunction _ _ _ _ _ _ _)  = False
onlyRecursiveAttributes symbol1 symbol2 = symbol1 == symbol2

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


