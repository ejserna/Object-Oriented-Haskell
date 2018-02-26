module TypeChecker where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

import SymbolTable
import ClassSymbolTable
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

newtype ClassTypeChecker = State ClassSymbolTable



startSemanticAnalysis :: Program -> IO ()
startSemanticAnalysis (Program classList functionList varsList block) =  do 
            putStrLn $ ppShow $ analyzeClasses classList emptyClassSymbolTable
            putStrLn $ ppShow $ analyzeVariables varsList globalScope Nothing emptySymbolTable

-- Analyze classes regresa una tabla de simbolos de clase y un booleano. Si es true, significa que hubo errores, si es false, no hubo errores
analyzeClasses :: [Class] -> ClassSymbolTable -> (ClassSymbolTable, Bool)
analyzeClasses [] _ = (emptyClassSymbolTable, False) 
analyzeClasses (cl : classes) classSymTab =
                                            let (newClassSymTab1, hasErrors1) = analyzeClass cl classSymTab
                                            in if hasErrors1 then (classSymTab, True)
                                               else let (newClassSymTab2, hasErrors2) = analyzeClasses classes newClassSymTab1
                                                    in if hasErrors2 then (classSymTab, True)
                                                       else ((Map.union newClassSymTab1 newClassSymTab2), False)
           

analyzeClass :: Class -> ClassSymbolTable -> (ClassSymbolTable, Bool)
analyzeClass (ClassInheritance subClass parentClass classBlock) classSymTab = if Map.member subClass classSymTab
                                                                    then (classSymTab, True) -- regresamos que si hay error
                                                                    -- Solo vamos a heredar si la clase padre esta en la tabla de simbolos de clase
                                                                    else if Map.member parentClass classSymTab  
                                                                        then 
                                                                            let newClassSymTable = Map.insert subClass emptySymbolTable classSymTab -- Si si es miembro, entonces si se puede heredar
                                                                            in (newClassSymTable, False) -- No hay error, devolvemos la nueva
                                                                        else (classSymTab,True) -- Si el parent class no es miembro, entonces error, no puedes heredar de una clase no declarada

analyzeClass (ClassNormal classIdentifier classBlock) classSymTab = if Map.member classIdentifier classSymTab
                                                                    then (classSymTab, False)
                                                                    else 
                                                                        let newClassSymTable = Map.insert classIdentifier emptySymbolTable classSymTab
                                                                        in (newClassSymTable, False)


analyzeVariables :: [Variable] -> Scope -> Maybe Bool -> SymbolTable -> (SymbolTable, Bool)
analyzeVariables [] _ _ _ = (emptySymbolTable, False)
analyzeVariables (var : vars) scp isVarPublic symTab = let (newSymTab1, hasErrors1) = analyzeVariable var scp isVarPublic symTab
                                               in if hasErrors1 then (symTab, True)
                                               else let (newSymTab2, hasErrors2) = analyzeVariables vars scp isVarPublic newSymTab1
                                                    in if hasErrors2 then (symTab, True)
                                                       else ((Map.union newSymTab1 newSymTab2), False)

{-
    data Variable 
    = VariableNoAssignment Type [Identifier]
    | VariableAssignmentLiteralOrVariable Type Identifier LiteralOrVariable
    | VariableAssignment1D Type Identifier [LiteralOrVariable]
    | VariableAssignment2D Type Identifier [[LiteralOrVariable]]
    | VariableAssignmentObject Type Identifier ObjectCreation
    | VariableListAssignment ListType Identifier ListAssignment
    | VariableListNoAssignment ListType [Identifier]
  deriving (Show, Eq)
-}
analyzeVariable :: Variable -> Scope -> Maybe Bool -> SymbolTable -> (SymbolTable, Bool)
analyzeVariable (VariableNoAssignment dataType identifiers) scp isVarPublic symTab = insertIdentifiers identifiers (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
analyzeVariable (VariableAssignmentLiteralOrVariable dataType identifier literalOrVariable) scp isVarPublic symTab =
                                        -- En esta parte usamos en ambos not, porque si alguno de los dos métodos que se llaman regresa falso, significa que no se encontro/ no son el mismo tipo de dato
                                        if not (checkLiteralOrVariableInSymbolTable literalOrVariable symTab) || not (checkDataTypes dataType literalOrVariable symTab) -- es not porque si son iguales, entonces no hay error
                                            then (symTab, True)
                                            else insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab -- si no entro en el if, significa que no hubo error

                     
insertIdentifiers :: [Identifier] -> Symbol -> SymbolTable -> (SymbolTable,Bool)
insertIdentifiers [] _ _ = (emptySymbolTable, False)
insertIdentifiers (identifier : ids) symbol symTab = let (newSymTab1, hasErrors1) = insertInSymbolTable identifier symbol symTab
                                            in if hasErrors1 then (symTab, True)
                                               else let (newSymTab2, hasErrors2) = insertIdentifiers ids symbol newSymTab1
                                                    in if hasErrors2 then (symTab, True)
                                                       else ((Map.union newSymTab1 newSymTab2), False)

insertInSymbolTable :: Identifier -> Symbol -> SymbolTable -> (SymbolTable,Bool)
insertInSymbolTable identifier symbol symTab = 
                                -- Si esta ese identificador en la tabla de simbolos, entonces regreso error
                                if Map.member identifier symTab
                                  then (symTab, True)
                                  else ((Map.insert identifier symbol symTab),False)

checkLiteralOrVariableInSymbolTable :: LiteralOrVariable -> SymbolTable -> Bool
checkLiteralOrVariableInSymbolTable (VarIdentifier identifier) symTab =  Map.member identifier symTab
checkLiteralOrVariableInSymbolTable ()
checkLiteralOrVariableInSymbolTable _ _ = True -- Si es otra cosa que var identifier, entonces regresamos true

checkDataTypes :: Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkDataTypes dType (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just symbol -> (dataType symbol) == dType -- Si son iguales, regresamos true
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkDataTypes dType _ symTab = True
