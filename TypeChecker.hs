module TypeChecker where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

import SymbolTable
import ClassSymbolTable
-- import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as Map

import Data.List (intercalate, maximumBy, union)
import Data.Ord (comparing)

newtype ClassTypeChecker = State ClassSymbolTable

startSemanticAnalysis :: Program -> IO ()
startSemanticAnalysis (Program classList functionList varsList (Block statements)) =  do 
            let (classSymbolTable, classErrors) = analyzeClasses classList emptyClassSymbolTable
            if (classErrors) 
                then putStrLn $ show "[1] ERROR: Semantic Error in Class Checking."
                else do putStrLn $ ppShow $ "[1]: Semantic Class Analysis Passed."
                        putStrLn $ ppShow $ classSymbolTable
            let (symbolTableWithFuncs,semanticErrorFuncs) = analyzeFunctions functionList globalScope Nothing emptySymbolTable classSymbolTable
            let (symbolTableWithFuncsVars,semanticErrorVars) = analyzeVariables varsList globalScope Nothing symbolTableWithFuncs classSymbolTable
            let returnListInMain = getReturnStatements statements
            let (symbolTableStatements,semanticErrorBlock) = analyzeStatements statements defScope symbolTableWithFuncsVars  classSymbolTable
            if (semanticErrorFuncs || semanticErrorVars || semanticErrorBlock || (length returnListInMain) > 0) 
                then putStrLn $ show "[2] ERROR: Semantic Error in Variable Checking."
                else do putStrLn $ ppShow $ "[2]: Semantic Variable Analysis Passed."
                        putStrLn $ ppShow $ symbolTableStatements
            

-- Analyze classes regresa una tabla de simbolos de clase y un booleano. Si es true, significa que hubo errores, si es false, no hubo errores
analyzeClasses :: [Class] -> ClassSymbolTable -> (ClassSymbolTable, Bool)
analyzeClasses [] _ = (emptyClassSymbolTable, False) 
analyzeClasses (cl : classes) classSymTab =
                                            -- se obtiene la symbol table de esa clase, donde tiene funciones y atributos
                                            let (varsSymTabForClass,hasErrors) = analyzeClassBlock cl emptySymbolTable classSymTab
                                            in if (hasErrors) then (emptyClassSymbolTable, True)
                                                -- Metemos las variables y funciones de la clase 
                                                else let (newClassSymTab1, hasErrors1) = analyzeClass cl varsSymTabForClass classSymTab
                                                 in if hasErrors1 then (emptyClassSymbolTable, True)
                                                   else let (newClassSymTab2, hasErrors2) = analyzeClasses classes newClassSymTab1
                                                        in if hasErrors2 then (emptyClassSymbolTable, True)
                                                           else ((Map.union newClassSymTab1 newClassSymTab2), False)

analyzeClassBlock :: Class -> SymbolTable -> ClassSymbolTable ->  (SymbolTable, Bool)
-- Debido a que se está heredando, hay que meter la symbol table de la clase padre en la hijo
analyzeClassBlock (ClassInheritance classIdentifier parentClass classBlock) symTab classSymTab = 
                case (Map.lookup parentClass classSymTab) of
                    Just symTabOfClass -> analyzeMembersOfClassBlock classBlock classIdentifier defScope (Map.union symTab symTabOfClass) classSymTab
                    Nothing -> (emptySymbolTable, True)
analyzeClassBlock (ClassNormal classIdentifier classBlock) symTab classSymTab = analyzeMembersOfClassBlock classBlock classIdentifier defScope symTab classSymTab

analyzeMembersOfClassBlock :: ClassBlock -> ClassIdentifier -> Scope -> SymbolTable -> ClassSymbolTable  -> (SymbolTable,Bool)
analyzeMembersOfClassBlock (ClassBlockNoConstructor classMembers) classIdentifier scp symTab classSymTab = analyzeClassMembers classMembers classIdentifier scp symTab classSymTab
analyzeMembersOfClassBlock (ClassBlock classMembers (ClassConstructor params block)) classIdentifier scp symTab classSymTab = 
                                        let (newSymTab1, hasErrors) = analyzeClassMembers classMembers classIdentifier scp symTab classSymTab
                                        in if (hasErrors == True) then (emptySymbolTable,True)
                                            else let (symTabFunc, hasErrors2) = analyzeFuncParams params emptySymbolTable classSymTab   
                                                in if (hasErrors2 == True) then (emptySymbolTable, True)
                                                    -- Debido a que funciones pueden tener identificadores en sus parametros,
                                                    -- hay que verificar que no interfieran con otros identificadores dentro de la
                                                    -- clase
                                                    else if (Map.size (Map.intersection symTabFunc newSymTab1)) == 0
                                                         then let newSymTab = (Map.insert "_constructor" (SymbolFunction {returnType = (Just (TypeClassId classIdentifier [])), scope = scp, body = block, shouldReturn = False ,isPublic = (Just True), symbolTable = symTabFunc, params = params}) symTab)
                                                                in analyzeClassMembers classMembers classIdentifier scp newSymTab classSymTab
                                                          else (emptySymbolTable, True)                       
-- let (newSymTab2,hasErrors) = analyzeClassMember cm scp newSymTab
--                                             if hasErrors then (emptySymbolTable, True)
--                                                 else let (newSymTab3,hasErrors2) = analyzeMembersOfClassBlock 
--                                                     (Map.union newSymTab2
analyzeClassMembers :: [ClassMember] -> ClassIdentifier -> Scope -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeClassMembers [] _ _ symTab _ = (symTab,False)
analyzeClassMembers (cm : cms) classIdentifier scp symTab classSymbolTable = 
                                                        let (newSymTab, hasErrors) = analyzeClassMember cm classIdentifier scp symTab classSymbolTable
                                                        in if (hasErrors) then (emptySymbolTable,True)
                                                            else let (newSymTab2,hasErrors2) = analyzeClassMembers cms classIdentifier scp newSymTab classSymbolTable
                                                                in if (hasErrors2) then (emptySymbolTable,True)
                                                                    else ((Map.union newSymTab newSymTab2), False)

analyzeClassMember :: ClassMember -> ClassIdentifier -> Scope -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeClassMember (ClassMemberAttribute (ClassAttributePublic variable)) classIdentifier scp symTab classSymTab = analyzeVariable variable scp  (Just True) symTab classSymTab
analyzeClassMember (ClassMemberAttribute (ClassAttributePrivate variable)) classIdentifier scp symTab classSymTab = analyzeVariable variable scp  (Just False) symTab classSymTab 
analyzeClassMember (ClassMemberFunction (ClassFunctionPublic function)) classIdentifier scp symTab classSymTab = analyzeFunction function scp (Just True) symTab classSymTab
analyzeClassMember (ClassMemberFunction (ClassFunctionPrivate function)) classIdentifier scp symTab classSymTab = analyzeFunction function scp (Just False) symTab classSymTab

analyzeClass :: Class -> SymbolTable -> ClassSymbolTable -> (ClassSymbolTable, Bool)
analyzeClass (ClassInheritance subClass parentClass classBlock) varSymTab classSymTab = if Map.member subClass classSymTab
                                                                    then (classSymTab, True) -- regresamos que si hay error
                                                                    -- Solo vamos a heredar si la clase padre esta en la tabla de simbolos de clase
                                                                    else if Map.member parentClass classSymTab  
                                                                        then 
                                                                            let newClassSymTable = Map.insert subClass varSymTab classSymTab -- Si si es miembro, entonces si se puede heredar
                                                                            in (newClassSymTable, False) -- No hay error, devolvemos la nueva
                                                                        else (classSymTab,True) -- Si el parent class no es miembro, entonces error, no puedes heredar de una clase no declarada

analyzeClass (ClassNormal classIdentifier classBlock) varSymTab classSymTab = if Map.member classIdentifier classSymTab
                                                                    then (classSymTab, False)
                                                                    else 
                                                                        let newClassSymTable = Map.insert classIdentifier varSymTab classSymTab
                                                                        in (newClassSymTable, False)

analyzeFunctions :: [Function] -> Scope -> Maybe Bool -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeFunctions [] _ _ _ _ = (emptySymbolTable, False)
analyzeFunctions (func : funcs) scp isFuncPublic symTab classTab = let (newSymTab1, hasErrors1) = analyzeFunction func scp isFuncPublic symTab classTab
                                               in if hasErrors1 then (emptySymbolTable, True)
                                               else let (newSymTab2, hasErrors2) = analyzeFunctions funcs scp isFuncPublic newSymTab1 classTab
                                                    in if hasErrors2 then (emptySymbolTable, True)
                                                       else ((Map.union newSymTab1 newSymTab2), False)

analyzeVariables :: [Variable] -> Scope -> Maybe Bool -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeVariables [] _ _ _ _ = (emptySymbolTable, False)
analyzeVariables (var : vars) scp isVarPublic symTab classTab = let (newSymTab1, hasErrors1) = analyzeVariable var scp isVarPublic symTab classTab
                                               in if hasErrors1 then (emptySymbolTable, True)
                                               else let (newSymTab2, hasErrors2) = analyzeVariables vars scp isVarPublic newSymTab1 classTab
                                                    in if hasErrors2 then (emptySymbolTable, True)
                                                       else ((Map.union newSymTab1 newSymTab2), False)                              


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
                                        case dataType of
                                            TypePrimitive _ (("[",size,"]") : []) ->  
                                                makeCheckFor1DAssignment size
                                            TypeClassId _ (("[",size,"]") : []) ->  
                                                makeCheckFor1DAssignment size
                                            _ -> (emptySymbolTable, True)
                                        where
                                            makeCheckFor1DAssignment size = if (checkTypeExistance dataType classTab) 
                                                                                && (checkLiteralOrVariablesAndDataTypes dataType literalOrVariables symTab) 
                                                                                && ((length literalOrVariables) <= fromIntegral size)
                                                    then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
                                                    else (emptySymbolTable, True)  -- hubo error, entonces regresamos la tabla vacia
                                        
analyzeVariable (VariableAssignment2D dataType identifier listOfLiteralOrVariables) scp isVarPublic symTab classTab = 
                                        case dataType of
                                            TypePrimitive _ (("[",sizeRows,"]") : ("[",sizeCols,"]") : []) ->  
                                                makeCheckFor2DAssignment sizeRows sizeCols
                                            TypeClassId _ (("[",sizeRows,"]") : ("[",sizeCols,"]") : []) ->  
                                                makeCheckFor2DAssignment sizeRows sizeCols
                                            _ -> (emptySymbolTable, True)
                                        where
                                            makeCheckFor2DAssignment sizeRows sizeCols = if (checkTypeExistance dataType classTab) && 
                                                                               (checkLiteralOrVariablesAndDataTypes2D dataType listOfLiteralOrVariables symTab)
                                                                               && ((length listOfLiteralOrVariables) <= fromIntegral sizeRows) -- checamos que sea el numero correcto de renglones
                                                                               && ((getLongestList listOfLiteralOrVariables) <= fromIntegral sizeCols)
                                                    then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
                                                    else (emptySymbolTable, True)  -- hubo error, entonces regresamos la tabla vacia
                                            getLongestList :: [[LiteralOrVariable]] -> Int
                                            getLongestList [] = 0
                                            getLongestList (x : xs) = max (length x) (getLongestList xs) 
analyzeVariable (VariableAssignmentObject dataType identifier (ObjectCreation classIdentifier params)) scp isVarPublic symTab classTab = 
                                        case dataType of
                                            TypePrimitive _ _ -> (emptySymbolTable, True)
                                            -- Checamos si el constructor es del mismo tipo que la clase
                                            TypeClassId classIdentifierDecl _ -> if (classIdentifierDecl == classIdentifier)
                                                                                 -- Checamos los parametros que se mandan con los del constructor
                                                                                 && (checkIfParamsAreCorrect params classIdentifier symTab classTab) 
                                                                                 then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
                                                                                 else (emptySymbolTable, True)
analyzeVariable (VariableListAssignment (TypeListClassId classIdentifier) identifier (ListAssignmentArray literalOrVariables)) scp isVarPublic symTab classTab = 
                                        -- Checamos si la clase esta declarada
                                         if (checkTypeExistance (TypeClassId classIdentifier []) classTab)
                                         -- Checamos que las asignaciones sean del mismo tipo que la clase
                                         && (checkLiteralOrVariablesAndDataTypes (TypeClassId classIdentifier []) literalOrVariables symTab)
                                            then insertInSymbolTable identifier (SymbolVar {dataType = (TypeListClassId classIdentifier), scope = scp, isPublic = isVarPublic}) symTab
                                            else (emptySymbolTable, True)
analyzeVariable (VariableListAssignment (TypeListPrimitive (PrimitiveInt)) identifier (ListAssignmentRange initial limit)) scp isVarPublic symTab classTab = 
                                         -- Se inserta si y solo si el typelist recibe es un primitive int, o sea, solo si 
                                         -- List of Int 1..2
                                         insertInSymbolTable identifier (SymbolVar {dataType = (TypeListPrimitive (PrimitiveInt)), scope = scp, isPublic = isVarPublic}) symTab
analyzeVariable (VariableListAssignment (TypeListPrimitive (PrimitiveInteger)) identifier (ListAssignmentRange initial limit)) scp isVarPublic symTab classTab = 
                                         -- Se inserta si y solo si el typelist recibe es un primitive int, o sea, solo si 
                                         -- List of Integer 1..2
                                         insertInSymbolTable identifier (SymbolVar {dataType = (TypeListPrimitive (PrimitiveInteger)), scope = scp, isPublic = isVarPublic}) symTab
analyzeVariable _ _ _ _ _  = (emptySymbolTable, True)


analyzeFunction :: Function -> Scope -> Maybe Bool -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeFunction (Function identifier (TypeFuncReturnPrimitive primitive) params (Block statements)) scp isPublic symTab classSymTab = 
                    if  not (Map.member identifier symTab)
                        then let (newFuncSymTab, hasErrors) = (analyzeFuncParams params emptySymbolTable classSymTab)
                                    -- Si hay errores o literalmente hay identificadores que son iguales que otros miembros, error
                                   in if (hasErrors) || ((Map.size (Map.intersection symTab newFuncSymTab)) /= 0) then (emptySymbolTable,True)
                                        -- Metemos ahora a la symbol table de la funcion la symbol table que exista en los statements
                                        -- Hacemos union para que adentro de los statements se puedan reconocer miembros internos!
                                        else let (newFuncSymTabWithStatements,hasErrors) = analyzeStatements statements scp (Map.union newFuncSymTab symTab) classSymTab
                                                    in if (hasErrors) then (emptySymbolTable,True)
                                                        -- Hacemos el difference porque newFuncSymTabWithStatements tiene como miembros los simbolos de la clase actual
                                                        else let newSymTabFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypePrimitive primitive [])), scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.difference newFuncSymTabWithStatements symTab), params = params}) symTab
                                                                in let areRetTypesOk = areReturnTypesOk (TypePrimitive primitive []) statements newSymTabFunc newFuncSymTabWithStatements classSymTab
                                                                in if areRetTypesOk == True then (newSymTabFunc, False)
                                                                   else (emptySymbolTable, True) 
                                -- in  -- if (checkCorrectReturnType (TypeClassId classIdentifier) block newSymTabFunc ) 
                        else (emptySymbolTable, True)
analyzeFunction (Function identifier (TypeFuncReturnClassId classIdentifier) params (Block statements)) scp isPublic symTab classSymTab = 
                if (checkTypeExistance (TypeClassId classIdentifier []) classSymTab)
                    then if not (Map.member identifier symTab)
                        then let (newFuncSymTab, hasErrors) = (analyzeFuncParams params emptySymbolTable classSymTab)
                                    -- Si hay errores o literalmente hay identificadores que son iguales que otros miembros, error
                                   in if (hasErrors) || ((Map.size (Map.intersection symTab newFuncSymTab)) /= 0) then (emptySymbolTable,True)
                                     -- Metemos ahora a la symbol table de la funcion la symbol table que exista en los statements
                                        else let (newFuncSymTabWithStatements,hasErrors) = analyzeStatements statements scp (Map.union newFuncSymTab symTab) classSymTab
                                                    in if (hasErrors) then (emptySymbolTable,True)
                                                        else let newSymTabFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypeClassId classIdentifier [])), scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.difference newFuncSymTabWithStatements symTab), params = params}) symTab
                                                          in let areRetTypesOk = areReturnTypesOk (TypeClassId classIdentifier []) statements newSymTabFunc newFuncSymTabWithStatements classSymTab
                                                             in if areRetTypesOk == True then (newSymTabFunc, False)
                                                                else (emptySymbolTable, True)
                        else (emptySymbolTable, True)
                    else (emptySymbolTable, True)
    -- Como no regresa nada, no hay que buscar que regrese algo el bloque
analyzeFunction (Function identifier (TypeFuncReturnNothing) params (Block statements)) scp isPublic symTab classSymTab =  
                  if  not (Map.member identifier symTab)
                        then let (newFuncSymTab, hasErrors) = (analyzeFuncParams params emptySymbolTable classSymTab)
                                    -- Si hay errores o literalmente hay identificadores que son iguales que otros miembros o bien, que el usuario quiere regresar algo adentro de una funcion cuyo valor de retorno es nothing
                                   in if (hasErrors) || (length (getReturnStatements statements)) > 0 || ((Map.size (Map.intersection symTab newFuncSymTab)) /= 0) then (emptySymbolTable,True)
                                        -- Metemos ahora a la symbol table de la funcion la symbol table que exista en los statements
                                        else let (newFuncSymTabWithStatements,hasErrors) = analyzeStatements statements scp (Map.union newFuncSymTab symTab) classSymTab
                                                    in if (hasErrors) then (emptySymbolTable,True)
                                                        else let newSymTabFunc = Map.insert identifier (SymbolFunction {returnType = Nothing, scope = scp, body = (Block statements), shouldReturn = False ,isPublic = isPublic, symbolTable = (Map.difference newFuncSymTabWithStatements symTab), params = params}) symTab
                                                            in if (hasErrors) then (emptySymbolTable,True)
                                                                else (newSymTabFunc,False)
                        else (emptySymbolTable, True)


areReturnTypesOk :: Type -> [Statement] -> SymbolTable -> SymbolTable -> ClassSymbolTable -> Bool
areReturnTypesOk funcRetType [] symTab ownFuncSymTab classTab = False
areReturnTypesOk funcRetType statements symTab ownFuncSymTab classTab =
    -- Nos aseguramos que al final haya un return statement
    case (last statements) of 
        (ReturnStatement _) -> let returnList = getReturnStatements statements 
                                in if (length returnList) == 0 then False -- Se esperaba que regresara un tipo
                                else (checkCorrectReturnTypes funcRetType returnList symTab ownFuncSymTab classTab)
        _ -> False

-- Aqui sacamos todos los returns que pueda haber, inclusive si estan en statements anidados
getReturnStatements :: [Statement]  -> [Return]
getReturnStatements [] = []
getReturnStatements ((ReturnStatement returnExp) : sts) = (returnExp) : (getReturnStatements sts)
getReturnStatements ((ConditionStatement (If _ (Block statements))) : sts) = (getReturnStatements statements) ++ (getReturnStatements sts)
getReturnStatements ((ConditionStatement (IfElse _ (Block statements) (Block statementsElse))) : sts) = (getReturnStatements statements) ++ (getReturnStatements statementsElse) ++ (getReturnStatements sts)
getReturnStatements ((CycleStatement (CycleWhile (While _ (Block statements)))) : sts) = (getReturnStatements statements) ++ (getReturnStatements sts)
getReturnStatements ((CycleStatement (CycleFor (For _ _ (Block statements)))) : sts) = (getReturnStatements statements) ++ (getReturnStatements sts)
getReturnStatements (_ : sts) =  (getReturnStatements sts)

analyzeFuncParams :: [(Type,Identifier)] -> SymbolTable -> ClassSymbolTable -> (SymbolTable,Bool)
analyzeFuncParams [] _  _ = (emptySymbolTable, False)
analyzeFuncParams ((dataType,identifier) : rest) symTab classSymTab = 
    let (newSymTab, hasErrors) = analyzeVariable ((VariableNoAssignment dataType [identifier] )) defScope Nothing symTab classSymTab
        in if (hasErrors) then (emptySymbolTable,True)
            else let (newSymTab2, hasErrors2) = analyzeFuncParams rest newSymTab classSymTab
                 in if (hasErrors2 == True) then (emptySymbolTable,True) 
                    else ((Map.union newSymTab newSymTab2), False)



checkCorrectReturnTypes :: Type -> [Return] -> SymbolTable -> SymbolTable -> ClassSymbolTable -> Bool
checkCorrectReturnTypes _ [] _ _ _ = True
checkCorrectReturnTypes  dataType ((ReturnFunctionCall (FunctionCallVar identifier callParams)) : rets) symTab ownFuncSymTab classTab =  
                    case (Map.lookup identifier (Map.union symTab ownFuncSymTab)) of
                        Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                        let funcParamTypes = map (\p -> fst p) params
                                        in  case returnTypeFunc of
                                                Just retType -> dataType == retType
                                                                && (compareListOfTypesWithFuncCall funcParamTypes callParams (Map.union symTab ownFuncSymTab))
                                                                && checkCorrectReturnTypes dataType rets symTab ownFuncSymTab classTab
                                                _ -> False           
                        _ -> False
checkCorrectReturnTypes  dataType ((ReturnFunctionCall (FunctionCallObjMem (ObjectMember identifier functionIdentifier) callParams)) : rets) symTab ownFuncSymTab classTab =  
                    case (Map.lookup identifier (Map.union symTab ownFuncSymTab)) of
                        Just (SymbolVar symDataType _ _)  -> 
                                      case symDataType of
                                        TypeClassId classIdentifier _ -> 
                                                    case (Map.lookup classIdentifier classTab) of
                                                        Just symbolTable ->
                                                                case (Map.lookup functionIdentifier symbolTable) of
                                                                    Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                        case returnTypeFunc of
                                                                            Just retType -> retType == dataType
                                                                                && (compareListOfTypesWithFuncCall (map (\p -> fst p) (params)) callParams (Map.union symTab ownFuncSymTab))
                                                                                && checkCorrectReturnTypes dataType rets symTab ownFuncSymTab classTab
                                                                            _ -> False   
                                                                    _ -> False   
                                                        _ -> False
                                        _ -> False
                        _ -> False
checkCorrectReturnTypes  dataType ((ReturnExp (ExpressionLitVar literalOrVariable)) : rets) symTab ownFuncSymTab classTab=  
                                            (checkDataTypes dataType literalOrVariable (Map.union symTab ownFuncSymTab))
                                            && checkCorrectReturnTypes dataType rets symTab ownFuncSymTab classTab
checkCorrectReturnTypes  dataType ((ReturnExp (ExpressionVarArray identifier _)) : rets) symTab ownFuncSymTab classTab =  
                                            (checkArrayAssignment dataType (VarIdentifier identifier) (Map.union symTab ownFuncSymTab)) && checkCorrectReturnTypes dataType rets symTab ownFuncSymTab classTab-- MARK TODO Expressions

-- checkCorrectReturnType 

compareListOfTypesWithFuncCall :: [Type] -> [Params] -> SymbolTable -> Bool
compareListOfTypesWithFuncCall [] [] _ = True
compareListOfTypesWithFuncCall [] (sp : sps) _ = False
compareListOfTypesWithFuncCall (rpType : rps) [] _ = False
compareListOfTypesWithFuncCall (rpType : rps) (sp : sps) symTab = 
                    case sp of
                        (ParamsExpression (ExpressionLitVar literalOrVariable)) -> 
                               (checkDataTypes rpType literalOrVariable symTab)
                               && (compareListOfTypesWithFuncCall rps sps symTab)
                        (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression innerExp) : []))) ->
                                -- Checamos que sea un arreglo
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive _ (("[",size,"]") : []) ) _ _) ->
                                        checkArrayAssignment rpType (VarIdentifier identifier) symTab -- TODO MARK: Check expression
                                        && compareListOfTypesWithFuncCall rps sps symTab
                                    Just (SymbolVar (TypeClassId _ (("[",size,"]") : []) ) _ _) ->
                                        checkArrayAssignment rpType (VarIdentifier identifier) symTab -- TODO MARK: Check expression
                                        && compareListOfTypesWithFuncCall rps sps symTab
                                    _ -> False
                        (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : []))) ->
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]") : [])) _ _) ->
                                        checkArrayAssignment rpType (VarIdentifier identifier) symTab -- TODO MARK: Check expression
                                        && compareListOfTypesWithFuncCall rps sps symTab
                                    Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [])) _ _) ->
                                        checkArrayAssignment rpType (VarIdentifier identifier) symTab -- TODO MARK: Check expression
                                        && compareListOfTypesWithFuncCall rps sps symTab
                                    _ -> False
                        (ParamsExpression expression) -> 
                               (compareListOfTypesWithFuncCall rps sps symTab) && 
                               True -- MARK TODO: Checar que la expresion sea del tipo
                               -- checkDataTypes(dataTypeConstructor,checkExpression(expression))

-- Checamos aqui que la llamada al constructor sea correcta
checkIfParamsAreCorrect :: [Params] -> ClassIdentifier -> SymbolTable -> ClassSymbolTable -> Bool
checkIfParamsAreCorrect sendingParams classIdentifier symTab classTab = 
                                    case (Map.lookup classIdentifier classTab) of
                                        Just symbolTableOfClass -> 
                                                case (Map.lookup "_constructor" symbolTableOfClass) of
                                                    Just (symbolFunc) -> (compareBoth (params symbolFunc) sendingParams)
                                                    Nothing -> True -- MARK TODO : Change to False
                                                where 
                                                    -- sp = sending param
                                                    -- rp = receiving param
                                                    compareBoth :: [(Type,Identifier)] -> [Params] -> Bool
                                                    compareBoth [] [] = True
                                                    compareBoth [] (sp : sps) = False -- Hay mas parametros que se mandan de los que se reciben
                                                    compareBoth (rp : rps) [] = False -- Hay mas en el constructor que de los que se mandan
                                                    compareBoth (rp : rps) (sp : sps) = 
                                                                    case sp of
                                                                        (ParamsExpression (ExpressionLitVar literalOrVariable)) -> 
                                                                               let (dataTypeConstructor,_) = rp
                                                                               in (checkDataTypes dataTypeConstructor literalOrVariable symTab)
                                                                                && compareBoth rps sps
                                                                        (ParamsExpression expression) -> 
                                                                               True -- MARK TODO: Checar que la expresion sea del tipo
                                                                               -- checkDataTypes(dataTypeConstructor,checkExpression(expression))
                                        Nothing -> False 

analyzeStatements :: [Statement] -> Scope -> SymbolTable -> ClassSymbolTable -> (SymbolTable,Bool)
analyzeStatements [] _ _ _ = (emptySymbolTable,False)
analyzeStatements (st : sts) scp symTab classTab = 
                                let (newSymTab, hasErrors) = analyzeStatement st scp symTab classTab
                                in if (hasErrors) then (emptySymbolTable, True)
                                    else let (newSymTab2,hasErrors2) = analyzeStatements sts scp newSymTab classTab
                                         in if (hasErrors2) then (emptySymbolTable,True)
                                            else ((Map.union newSymTab newSymTab2), False)

analyzeStatement :: Statement -> Scope -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeStatement (AssignStatement assignment) scp symTab classTab = if (isAssignmentOk assignment scp symTab classTab)
                                                                        then (symTab, False)
                                                                        else (emptySymbolTable, True)
analyzeStatement (DisplayStatement displays) scp symTab classTab = if analyzeDisplays displays 
                                                                    then (symTab, False)
                                                                    else (emptySymbolTable, True)
                                                                where 
                                                                    analyzeDisplays :: [Display] -> Bool
                                                                    analyzeDisplays [] = True
                                                                    analyzeDisplays (disp : disps) = 
                                                                            analyzeDisplay disp scp symTab classTab
                                                                            && analyzeDisplays disps

analyzeStatement (ReadStatement (Reading identifier)) scp symTab classTab = 
                                    case (Map.lookup identifier symTab) of
                                        Just (SymbolVar (TypePrimitive _ []) varScp _) ->
                                            if varScp >= scp then
                                                (symTab,False)
                                            else (emptySymbolTable, True)
                                        _ -> (emptySymbolTable, True)
analyzeStatement (DPMStatement assignment) scp symTab classTab = analyzeStatement (AssignStatement assignment) scp symTab classTab
analyzeStatement (FunctionCallStatement functionCall) scp symTab classTab = if (analyzeFunctionCall functionCall scp symTab classTab) 
                                                                                then (symTab, False)
                                                                                else (emptySymbolTable, True) 
analyzeStatement (VariableStatement var) scp symTab classTab = analyzeVariable var scp Nothing symTab classTab
analyzeStatement (ConditionStatement (If expression (Block statements))) scp symTab classTab = analyzeStatements statements (scp - 1) symTab classTab -- TODO MARK: Check expression
analyzeStatement (CycleStatement (CycleWhile (While expression (Block statements)))) scp symTab classTab = analyzeStatements statements (scp - 1) symTab classTab -- TODO MARK: Check expression
analyzeStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) scp symTab classTab = 
                                                                            if (greaterRange > lowerRange) 
                                                                                then analyzeStatements statements (scp - 1) symTab classTab
                                                                                else (emptySymbolTable,True)
analyzeStatement (ReturnStatement _) scp symTab classTab = (symTab,False)

analyzeDisplay :: Display -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
analyzeDisplay (DisplayLiteralOrVariable (VarIdentifier identifier)) scp symTab classTab = 
                            case (Map.lookup identifier symTab) of 
                                Just (SymbolVar (TypePrimitive prim _) varScp _) ->
                                    varScp >= scp
                                Just (SymbolVar (TypeClassId classIdentifier _) varScp _) ->
                                    varScp >= scp
                                _ -> False
-- Podemos desplegar primitivos sin problemas
analyzeDisplay (DisplayLiteralOrVariable _) scp symTab classTab = True
analyzeDisplay (DisplayObjMem (ObjectMember objectIdentifier attrIdentifier)) scp symTab classTab =
                            case (Map.lookup objectIdentifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _) -> 
                                        if (objScp >= scp) 
                                            then
                                            case (Map.lookup classIdentifier classTab) of
                                                Just symbolTableOfClass ->
                                                        case (Map.lookup attrIdentifier symbolTableOfClass) of
                                                            -- Si y solo si es publico el atributo, la accedemos
                                                            Just (SymbolVar attrDataType attrScp (Just True)) ->
                                                                True 
                                                            _ -> False   
                                                _ -> False
                                        else False
                                    _ -> False
analyzeDisplay (DisplayFunctionCall functionCall) scp symTab classTab =
                            analyzeFunctionCall functionCall scp symTab classTab
analyzeDisplay (DisplayVarArrayAccess identifier ((ArrayAccessExpression expressionIndex) : []) ) scp symTab classTab =
                            case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) varScp _ ) -> 
                                        varScp >= scp
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _ ) -> 
                                        varScp >= scp  
                                    _ -> False                            
analyzeDisplay (DisplayVarArrayAccess identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : []) ) scp symTab classTab =
                            case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : [])) varScp _ ) -> 
                                        varScp >= scp
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _ ) -> 
                                        varScp >= scp 
                                    _ -> False 
analyzeFunctionCall :: FunctionCall -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
analyzeFunctionCall (FunctionCallVar funcIdentifier callParams) _ symTab classTab = 
                            case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    let funcParamTypes = map (\p -> fst p) params
                                                    in (compareListOfTypesWithFuncCall funcParamTypes callParams symTab)         
                                    _ -> False
analyzeFunctionCall (FunctionCallObjMem (ObjectMember objectIdentifier functionIdentifier) callParams) scp symTab classTab = 
                           case (Map.lookup objectIdentifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _ ) -> 
                                        if objScp >= scp 
                                            then case (Map.lookup classIdentifier classTab) of
                                                Just symbolTableOfClass ->
                                                        case (Map.lookup functionIdentifier symbolTableOfClass) of
                                                            -- Si y solo si es publica la funcion, la accedemos
                                                            Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                let funcParamTypes = map (\p -> fst p) params
                                                                in (compareListOfTypesWithFuncCall funcParamTypes callParams symTab)  
                                                            _ -> False   
                                                _ -> False
                                        else False
                                    _ -> False

isAssignmentOk :: Assignment -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
isAssignmentOk (AssignmentExpression identifier expression) scp symTab classSymTab = case (Map.lookup identifier symTab) of
                                                                                        Just (SymbolVar (TypePrimitive prim []) varScp _) -> 
                                                                                                    -- TODO: Checar que el tipo del identifier sea el mismo que la expression
                                                                                                    -- Si el scope de esta variable es mayor al scope de este assignment, si puedo accederlo
                                                                                                    if (varScp >= scp) 
                                                                                                        then True
                                                                                                        else False
                                                                                        Just (SymbolVar (TypeClassId classIdentifier []) varScp _) -> 
                                                                                                    -- TODO: Checar que el tipo del identifier sea el mismo que la expression
                                                                                                    -- Si el scope de esta variable es mayor al scope de este assignment, si puedo accederlo
                                                                                                    if (varScp >= scp) 
                                                                                                        then True
                                                                                                        else False
                                                                                        _ -> False
isAssignmentOk (AssignmentFunctionCall identifier (FunctionCallObjMem (ObjectMember objectIdentifier functionIdentifier) callParams)) scp symTab classSymTab = 
                                                                                    case (Map.lookup identifier symTab) of
                                                                                        Just (SymbolVar dataType varScp _) ->
                                                                                            if varScp >= scp
                                                                                                then case (Map.lookup objectIdentifier symTab) of
                                                                                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _ ) -> 
                                                                                                        if objScp >= scp 
                                                                                                            then case (Map.lookup classIdentifier classSymTab) of
                                                                                                                Just symbolTableOfClass ->
                                                                                                                        case (Map.lookup functionIdentifier symbolTableOfClass) of
                                                                                                                            -- Si y solo si es publica la funcion, la accedemos
                                                                                                                            Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                                                                                let funcParamTypes = map (\p -> fst p) params
                                                                                                                                in case returnTypeFunc of
                                                                                                                                        Just retType -> dataType == retType
                                                                                                                                                        && (compareListOfTypesWithFuncCall funcParamTypes callParams symTab)
                                                                                                                                        _ -> False   
                                                                                                                            _ -> False   
                                                                                                                _ -> False
                                                                                                        else False
                                                                                                    _ -> False
                                                                                            else False
                                                                                        _ -> False
isAssignmentOk  (AssignmentFunctionCall identifier (FunctionCallVar funcIdentifier callParams)) scp symTab classSymTab =  
                      case (Map.lookup identifier symTab) of
                        Just (SymbolVar dataType varScp _) ->
                            if (varScp >= scp)
                                then case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    let funcParamTypes = map (\p -> fst p) params
                                                    in case returnTypeFunc of
                                                            Just retType -> dataType == retType
                                                                            && (compareListOfTypesWithFuncCall funcParamTypes callParams symTab)
                                                            _ -> False           
                                    _ -> False
                            else False
isAssignmentOk  (AssignmentObjectMember identifier (ObjectMember objectIdentifier attrIdentifier)) scp symTab classSymTab =  
                      case (Map.lookup identifier symTab) of
                        Just (SymbolVar dataType varScp _) ->
                                    if (varScp >= scp)
                                        then 
                                        case (Map.lookup objectIdentifier symTab) of
                                            Just (SymbolVar (TypeClassId classIdentifier _) objScp _) -> 
                                                if (objScp >= scp) 
                                                    then
                                                    case (Map.lookup classIdentifier classSymTab) of
                                                        Just symbolTableOfClass ->
                                                                case (Map.lookup attrIdentifier symbolTableOfClass) of
                                                                    -- Si y solo si es publico el atributo, la accedemos
                                                                    Just (SymbolVar attrDataType attrScp (Just True)) ->
                                                                        dataType == attrDataType  
                                                                    _ -> False   
                                                        _ -> False
                                                else False
                                            _ -> False
                                    else False
                        _ -> False
isAssignmentOk  (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) scp symTab classSymTab =  
                      case (Map.lookup objectIdentifier symTab) of
                        Just (SymbolVar (TypeClassId classIdentifier _) objScp _) -> 
                            if (objScp >= scp) 
                                then
                                case (Map.lookup classIdentifier classSymTab) of
                                    Just symbolTableOfClass ->
                                            case (Map.lookup attrIdentifier symbolTableOfClass) of
                                                -- Si y solo si es publico el atributo, la accedemos
                                                Just (SymbolVar attrDataType attrScp (Just True)) ->
                                                    -- TODO: Checar que el tipo de dato de la expresion sea el mismo que data type del atributo
                                                    --attrDataType  
                                                    True
                                                _ -> False   
                                    _ -> False
                            else False
                        _ -> False
isAssignmentOk  (AssignmentArrayExpression identifier ((ArrayAccessExpression innerExp) : []) expression) scp symTab classSymTab =  
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    True
                                -- TODO: Checar que expresion sea del mismo data type que array assignment
                            else False
                        Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    True
                                -- TODO: Checar que expresion sea del mismo data type que array assignment
                            else False
                        _ -> False
isAssignmentOk  (AssignmentArrayExpression identifier ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) expression) scp symTab classSymTab = 
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",sizeRows,"]") : ("[",sizeCols,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    True
                                -- TODO: Checar que expresion sea del mismo data type que array assignment
                            else False
                        Just (SymbolVar (TypeClassId classIdentifier (("[",sizeRows,"]") : ("[",sizeCols,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    True
                                -- TODO: Checar que expresion sea del mismo data type que array assignment
                            else False
                        _ -> False 

isAssignmentOk _ _ _ _ = False

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
                                  then (emptySymbolTable, True)
                                  else ((Map.insert identifier symbol symTab),False)

-- Aqui checamos que la asignacion de un una lista de literales o variables sea del tipo receptor
checkLiteralOrVariablesAndDataTypes :: Type -> [LiteralOrVariable] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes _ [] _ = True
checkLiteralOrVariablesAndDataTypes dataType (litVar : litVars) symTab =  
                            if (checkLiteralOrVariableInSymbolTable litVar symTab) &&  (checkArrayAssignment dataType litVar symTab)
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


checkLiteralOrVariablesAndDataTypes2D :: Type -> [[LiteralOrVariable]] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes2D _ [] _ = True
checkLiteralOrVariablesAndDataTypes2D dataType (listOfLitVars : rest) symTab =  
                            if (checkLiteralOrVariablesAndDataTypes dataType listOfLitVars symTab)  
                                then checkLiteralOrVariablesAndDataTypes2D dataType rest symTab
                                else False -- Alguna literal o variable asignada no existe, o bien, el tipo de dato que se esta asignando no concuerda con la declaracion

-- Aqui checamos si el literal or variable que se esta asignando al arreglo sea del tipo indicado
-- es decir, en Humano [10] humanos = [h1,h2,h3,h4] checa que h1,h2,h3 y h4 sean del tipo humano
checkArrayAssignment :: Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkArrayAssignment (TypePrimitive prim arrayDeclaration) (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just symbol -> 
                                            case (dataType symbol) of
                                                TypePrimitive primId _ -> prim == primId
                                                _ -> False 
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkArrayAssignment (TypeClassId classIdentifier arrayDeclaration) (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just symbol -> 
                                            case (dataType symbol) of
                                                TypeClassId classId _ -> classId == classIdentifier
                                                _ -> False 
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkArrayAssignment dataType litOrVar symTab  = checkDataTypes dataType litOrVar symTab

-- Aqui checamos si el literal or variable que se esta dando esta de acuerdo al que se esta asignando! O sea,
-- no es valido decir Int i = 1; Money m = i; 
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
-- TODO MARK: Meter validacion type bool con bool literal
checkDataTypes _ _ _ = False -- Todo lo demas, falso



