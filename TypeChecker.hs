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
            let (classSymbolTable, classErrors) = analyzeClasses classList emptyClassSymbolTable
            if (classErrors) 
                then putStrLn $ show "[1] ERROR: Semantic Error in Class Checking."
                else putStrLn $ ppShow $ "[1]: Semantic Class Analysis Passed."
            putStrLn $ ppShow $ classSymbolTable
            let (symbolTable,semanticError) = analyzeVariables varsList globalScope Nothing emptySymbolTable classSymbolTable
            putStrLn $ ppShow $ symbolTable
            if (semanticError) 
                then putStrLn $ show "[2] ERROR: Semantic Error in Variable Checking."
                else putStrLn $ ppShow $ "[2]: Semantic Variable Analysis Passed."

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


analyzeVariables :: [Variable] -> Scope -> Maybe Bool -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeVariables [] _ _ _ _ = (emptySymbolTable, False)
analyzeVariables (var : vars) scp isVarPublic symTab classTab = let (newSymTab1, hasErrors1) = analyzeVariable var scp isVarPublic symTab classTab
                                               in if hasErrors1 then (emptySymbolTable, True)
                                               else let (newSymTab2, hasErrors2) = analyzeVariables vars scp isVarPublic newSymTab1 classTab
                                                    in if hasErrors2 then (emptySymbolTable, True)
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
analyzeVariable :: Variable -> Scope -> Maybe Bool -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeVariable (VariableNoAssignment dataType identifiers) scp isVarPublic symTab classTab = 
    -- Checamos si existe ese tipo
    if (checkTypeExistance dataType classTab) 
        then insertIdentifiers identifiers (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab classTab
        else (emptySymbolTable, True) -- No existio esa clase, error
analyzeVariable (VariableAssignmentLiteralOrVariable dataType identifier literalOrVariable) scp isVarPublic symTab classTab =
                                        -- En esta parte nos aseguramos que el tipo este declarado, el literal or variable exista y que la asignacion de tipos de datos sea correcta
                                        if (checkTypeExistance dataType classTab) &&  (checkLiteralOrVariableInSymbolTable literalOrVariable symTab) && (checkDataTypes dataType literalOrVariable symTab)
                                            then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
                                            else (emptySymbolTable, True)  -- hubo error, entonces regresamos la tabla vacia
analyzeVariable (VariableAssignment1D dataType identifier literalOrVariables) scp isVarPublic symTab classTab = 
                                        -- En esta parte nos aseguramos que la lista de asignaciones concuerde con el tipo de dato declarado
                                        if (checkTypeExistance dataType classTab) && (checkLiteralOrVariablesAndDataTypes dataType literalOrVariables symTab) 
                                            then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
                                            else (emptySymbolTable, True)  -- hubo error, entonces regresamos la tabla vacia


                     
insertIdentifiers :: [Identifier] -> Symbol -> SymbolTable -> ClassSymbolTable -> (SymbolTable,Bool)
insertIdentifiers [] _ _ _ = (emptySymbolTable, False)
insertIdentifiers (identifier : ids) symbol symTab classTab = let (newSymTab1, hasErrors1) = insertInSymbolTable identifier symbol symTab 
                                            in if hasErrors1 then (symTab, True)
                                               else let (newSymTab2, hasErrors2) = insertIdentifiers ids symbol newSymTab1 classTab
                                                    in if hasErrors2 then (symTab, True)
                                                       else ((Map.union newSymTab1 newSymTab2), False)

insertInSymbolTable :: Identifier -> Symbol -> SymbolTable -> (SymbolTable,Bool)
insertInSymbolTable identifier symbol symTab  = 
                                -- Si esta ese identificador en la tabla de simbolos, entonces regreso error
                                if Map.member identifier symTab
                                  then (symTab, True)
                                  else ((Map.insert identifier symbol symTab),False)

checkLiteralOrVariablesAndDataTypes :: Type -> [LiteralOrVariable] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes _ [] _ = True
checkLiteralOrVariablesAndDataTypes dataType (litVar : litVars) symTab =  
                            if (checkLiteralOrVariableInSymbolTable litVar symTab) &&  (checkDataTypes dataType litVar symTab)
                                then checkLiteralOrVariablesAndDataTypes dataType litVars symTab
                                else False -- Alguna literal o variable asignada no existe, o bien, el tipo de dato que se esta asignando no concuerda con la declaracion

checkLiteralOrVariableInSymbolTable :: LiteralOrVariable -> SymbolTable  -> Bool
checkLiteralOrVariableInSymbolTable (VarIdentifier identifier) symTab =  Map.member identifier symTab
checkLiteralOrVariableInSymbolTable _ _= True -- Si es otra cosa que var identifier, entonces regresamos true

checkTypeExistance :: Type -> ClassSymbolTable -> Bool
checkTypeExistance (TypeClassId classIdentifier _) classTab = 
                                                  case (Map.lookup classIdentifier classTab) of
                                                  Just _ -> True -- Si existe esa clase
                                                  _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkTypeExistance _ _ = True -- Todos lo demas regresa true



-- Aqui checamos si el literal or variable que se esta dando esta de acuerdo al que se esta asignando! O sea,
-- no es valido decir Double d = 1.22; Money m = d;
checkDataTypes :: Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkDataTypes dType (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just symbol -> (dataType symbol) == dType -- Si son iguales, regresamos true
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkDataTypes (TypePrimitive (PrimitiveInt) _) (IntegerLiteral _) _  = True
checkDataTypes (TypePrimitive (PrimitiveDouble) _) (DecimalLiteral _) _ = True
checkDataTypes (TypePrimitive (PrimitiveMoney) _) (DecimalLiteral _) _ = True
checkDataTypes (TypePrimitive (PrimitiveString) _) (StringLiteral _) _ = True
checkDataTypes (TypePrimitive (PrimitiveInteger) _) (IntegerLiteral _) _ = True
checkDataTypes _ _ _ = False -- Todo lo demas, falso



