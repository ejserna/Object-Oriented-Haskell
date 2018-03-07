module TypeChecker where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

import SymbolTable
import ClassSymbolTable
import Expression
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
                        putStrLn $ ppShow $  symbolTableStatements
            

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
                    Just symTabOfClass -> analyzeMembersOfClassBlock classBlock classIdentifier globalScope (Map.union symTab symTabOfClass) classSymTab
                    Nothing -> (emptySymbolTable, True)
analyzeClassBlock (ClassNormal classIdentifier classBlock) symTab classSymTab = analyzeMembersOfClassBlock classBlock classIdentifier globalScope symTab classSymTab

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
analyzeFunctions [] _ _ symTab _ = (symTab, False)
analyzeFunctions (func : funcs) scp isFuncPublic symTab classTab = let (newSymTab1, hasErrors1) = analyzeFunction func scp isFuncPublic symTab classTab
                                               in if hasErrors1 then (emptySymbolTable, True)
                                               else let (newSymTab2, hasErrors2) = analyzeFunctions funcs scp isFuncPublic newSymTab1 classTab
                                                    in if hasErrors2 then (emptySymbolTable, True)
                                                       else ((Map.union newSymTab1 newSymTab2), False)

analyzeVariables :: [Variable] -> Scope -> Maybe Bool -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
analyzeVariables [] _ _ symTab _ = (symTab, False)
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
                                        if (checkTypeExistance dataType classTab) &&  (checkLiteralOrVariableInSymbolTable scp literalOrVariable symTab) && (checkDataTypes scp dataType literalOrVariable symTab)
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
                                                                                && (checkLiteralOrVariablesAndDataTypes scp dataType literalOrVariables symTab) 
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
                                                                               (checkLiteralOrVariablesAndDataTypes2D scp dataType listOfLiteralOrVariables symTab)
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
                                                                                 && (checkIfParamsAreCorrect scp params classIdentifier symTab classTab) 
                                                                                 then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic}) symTab
                                                                                 else (emptySymbolTable, True)
analyzeVariable (VariableListAssignment (TypeListClassId classIdentifier) identifier (ListAssignmentArray literalOrVariables)) scp isVarPublic symTab classTab = 
                                        -- Checamos si la clase esta declarada
                                         if (checkTypeExistance (TypeClassId classIdentifier []) classTab)
                                         -- Checamos que las asignaciones sean del mismo tipo que la clase
                                         && (checkLiteralOrVariablesAndDataTypes scp (TypeClassId classIdentifier []) literalOrVariables symTab)
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
                    if  not (Map.member identifier symTab) then
                        -- La newFuncSymTab me da la symbol table de la funcion ya con sus parametros añadidos
                             let (newFuncSymTab, hasErrors) = (analyzeFuncParams params emptySymbolTable classSymTab)
                                    -- Si hay errores o literalmente hay identificadores que son iguales que otros miembros, error
                                   in if (hasErrors) || ((Map.size (Map.intersection symTab newFuncSymTab)) /= 0) then (emptySymbolTable,True)
                                        -- Metemos ahora a la symbol table de la funcion la symbol table que exista en los statements
                                        -- Hacemos doble union para que adentro de los statements se puedan reconocer miembros internos!
                                        -- Asi mismo, es MUY importante crear de una vez la funcion, para que pueda ser usada dentro de nuestros statements sin problemas! (Recursion)
                                        else let symTabFuncWithOwnFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypePrimitive primitive [])), scope = scp, body = (Block []), shouldReturn = True ,isPublic = isPublic, symbolTable = newFuncSymTab, params = params}) symTab
                                             in let (newFuncSymTabWithStatements,hasErrors) = analyzeStatements statements scp (Map.union (Map.union symTabFuncWithOwnFunc symTab) newFuncSymTab) classSymTab
                                                    in if (hasErrors) then (emptySymbolTable,True)
                                                        -- Hacemos el difference porque newFuncSymTabWithStatements tiene como miembros los simbolos de la clase actual
                                                        -- Se actualiza con el insert
                                                        else let newSymTabFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypePrimitive primitive [])), scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.difference newFuncSymTabWithStatements symTab), params = params}) symTab
                                                                in let areRetTypesOk = areReturnTypesOk scp (TypePrimitive primitive []) statements newSymTabFunc newFuncSymTabWithStatements classSymTab
                                                                in if areRetTypesOk == True then (newSymTabFunc, False)
                                                                   else (emptySymbolTable, True) 
                                -- in  -- if (checkCorrectReturnType (TypeClassId classIdentifier) block newSymTabFunc ) 
                        else (emptySymbolTable, True)
analyzeFunction (Function identifier (TypeFuncReturnClassId classIdentifier) params (Block statements)) scp isPublic symTab classSymTab = 
                if (checkTypeExistance (TypeClassId classIdentifier []) classSymTab)
                    then if not (Map.member identifier symTab) then
                         let (newFuncSymTab, hasErrors) = (analyzeFuncParams params emptySymbolTable classSymTab)
                                    -- Si hay errores o literalmente hay identificadores que son iguales que otros miembros, error
                                   in if (hasErrors) || ((Map.size (Map.intersection symTab newFuncSymTab)) /= 0) then (emptySymbolTable,True)
                                        else let symTabFuncWithOwnFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypeClassId classIdentifier [])), scope = scp, body = (Block []), shouldReturn = True ,isPublic = isPublic, symbolTable = newFuncSymTab, params = params}) symTab 
                                            -- Metemos ahora a la symbol table de la funcion la symbol table que exista en los statements
                                             in let (newFuncSymTabWithStatements,hasErrors) = analyzeStatements statements scp (Map.union (Map.union symTabFuncWithOwnFunc symTab) newFuncSymTab) classSymTab
                                                    in if (hasErrors) then (emptySymbolTable,True)
                                                        else let newSymTabFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypeClassId classIdentifier [])), scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.difference newFuncSymTabWithStatements symTab), params = params}) symTab
                                                          in let areRetTypesOk = areReturnTypesOk scp (TypeClassId classIdentifier []) statements newSymTabFunc newFuncSymTabWithStatements classSymTab
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
                                        else let symTabFuncWithOwnFunc = Map.insert identifier (SymbolFunction {returnType = Nothing, scope = scp, body = (Block []), shouldReturn = True ,isPublic = isPublic, symbolTable = newFuncSymTab, params = params}) symTab 
                                             -- Metemos ahora a la symbol table de la funcion la symbol table que exista en los statements
                                             in let (newFuncSymTabWithStatements,hasErrors) = analyzeStatements statements scp (Map.union (Map.union symTabFuncWithOwnFunc symTab) newFuncSymTab) classSymTab
                                                    in if (hasErrors) then (emptySymbolTable,True)
                                                        else let newSymTabFunc = Map.insert identifier (SymbolFunction {returnType = Nothing, scope = scp, body = (Block statements), shouldReturn = False ,isPublic = isPublic, symbolTable = (Map.difference newFuncSymTabWithStatements symTab), params = params}) symTab
                                                            in if (hasErrors) then (emptySymbolTable,True)
                                                                else (newSymTabFunc,False)
                        else (emptySymbolTable, True)


areReturnTypesOk :: Scope -> Type -> [Statement] -> SymbolTable -> SymbolTable -> ClassSymbolTable -> Bool
areReturnTypesOk _ _ [] _ _ _  = False 
areReturnTypesOk scp funcRetType ((ReturnStatement ret) : []) symTab ownFuncSymTab classTab = 
                                                checkCorrectReturnTypes scp funcRetType ret symTab ownFuncSymTab classTab
areReturnTypesOk scp funcRetType ((ConditionStatement (If _ (Block statements))) : sts) symTab ownFuncSymTab classTab = 
                                                areReturnTypesOk (scp - 1) funcRetType statements symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab 
areReturnTypesOk scp funcRetType ((ConditionStatement (IfElse _ (Block statements) (Block statementsElse))) : sts) symTab ownFuncSymTab classTab = 
                                                areReturnTypesOk (scp - 1) funcRetType statements symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType statementsElse symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab  
areReturnTypesOk scp funcRetType (_ : sts) symTab ownFuncSymTab classTab =  areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab 

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
    let (newSymTab, hasErrors) = analyzeVariable ((VariableNoAssignment dataType [identifier] )) globalScope Nothing symTab classSymTab
        in if (hasErrors) then (emptySymbolTable,True)
            else let (newSymTab2, hasErrors2) = analyzeFuncParams rest newSymTab classSymTab
                 in if (hasErrors2 == True) then (emptySymbolTable,True) 
                    else ((Map.union newSymTab newSymTab2), False)



-- El tipo de regreso de una funcion solo puede ser un elemento atomico!
checkCorrectReturnTypes :: Scope -> Type -> Return -> SymbolTable -> SymbolTable -> ClassSymbolTable -> Bool
checkCorrectReturnTypes  scp dataType (ReturnFunctionCall (FunctionCallVar identifier callParams)) symTab ownFuncSymTab classTab =  
                    case (Map.lookup identifier (Map.union symTab ownFuncSymTab)) of
                        Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                        let funcParamTypes = map (\p -> fst p) params
                                        in  case returnTypeFunc of
                                                Just retType -> dataType == retType
                                                                && (compareListOfTypesWithFuncCall scp funcParamTypes callParams (Map.union symTab ownFuncSymTab))
                                                _ -> False           
                        _ -> False
checkCorrectReturnTypes scp dataType (ReturnFunctionCall (FunctionCallObjMem (ObjectMember identifier functionIdentifier) callParams)) symTab ownFuncSymTab classTab =  
                    case (Map.lookup identifier (Map.union symTab ownFuncSymTab)) of
                        Just (SymbolVar symDataType varScp _)  
                                   | varScp >= scp ->
                                      case symDataType of
                                        TypeClassId classIdentifier _ -> 
                                                    case (Map.lookup classIdentifier classTab) of
                                                        Just symbolTable ->
                                                                case (Map.lookup functionIdentifier symbolTable) of
                                                                    Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                        case returnTypeFunc of
                                                                            Just retType -> retType == dataType
                                                                                && (compareListOfTypesWithFuncCall scp (map (\p -> fst p) (params)) callParams (Map.union symTab ownFuncSymTab))
                                                                            _ -> False   
                                                                    _ -> False   
                                                        _ -> False
                                        _ -> False
                                    | otherwise -> False
                        _ -> False
checkCorrectReturnTypes scp dataType (ReturnExp expression) symTab ownFuncSymTab classTab =  
                                            case (preProcessExpression scp expression (Map.union symTab ownFuncSymTab)) of
                                                Just expType -> dataType == expType
                                                _ -> False   

compareListOfTypesWithFuncCall :: Scope -> [Type] -> [Params] -> SymbolTable -> Bool
compareListOfTypesWithFuncCall _ [] [] _ = True
compareListOfTypesWithFuncCall _ [] (sp : sps) _ = False
compareListOfTypesWithFuncCall _ (rpType : rps) [] _ = False
compareListOfTypesWithFuncCall scp (rpType : rps) (sp : sps) symTab = 
                        case sp of 
                            (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression innerExp) : []))) ->
                                -- Checamos que sea un arreglo
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) varScp _) 
                                       | varScp >= scp -> 
                                            case (expressionProcess scp innerExp symTab) of 
                                                Just PrimitiveInt ->  (TypePrimitive prim []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                                                Just PrimitiveInteger ->  (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                                                _ -> False
                                            
                                       | otherwise -> False
                                    Just (SymbolVar (TypeClassId classId (("[",size,"]") : []) ) varScp _) 
                                       | varScp >= scp -> 
                                                case (expressionProcess scp innerExp symTab) of 
                                                    Just PrimitiveInt -> (TypeClassId classId []) == rpType 
                                                        && compareListOfTypesWithFuncCall scp rps sps symTab
                                                    Just PrimitiveInteger -> (TypeClassId classId []) == rpType 
                                                        && compareListOfTypesWithFuncCall scp rps sps symTab
                                                    _ -> False
                                       | otherwise -> False
                                    _ -> False
                            (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : []))) ->
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [])) varScp _)
                                        | varScp >= scp -> 
                                            let rowExpType = expressionProcess scp rowExp symTab
                                                colExpType = expressionProcess scp colExp symTab
                                            in if(rowExpType == colExpType) 
                                                then case rowExpType of 
                                                    Just PrimitiveInt -> (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                                                    Just PrimitiveInteger -> (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                                                    _ -> False
                                                else False
                                        | otherwise -> False
                                    Just (SymbolVar (TypeClassId classId (("[",rows,"]") : ("[",cols,"]") : [])) varScp _)
                                        | varScp >= scp -> 
                                            let rowExpType = expressionProcess scp rowExp symTab
                                                colExpType = expressionProcess scp colExp symTab
                                            in if(rowExpType == colExpType) 
                                                then case rowExpType of 
                                                    Just PrimitiveInt -> (TypeClassId classId []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                                                    Just PrimitiveInteger -> (TypeClassId classId []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                                                    _ -> False
                                                else False
                                        | otherwise -> False
                                    _ -> False
                            (ParamsExpression (ExpressionLitVar ((VarIdentifier identifier)))) ->
                                            checkDataTypes scp rpType (VarIdentifier identifier) symTab
                                            && compareListOfTypesWithFuncCall scp rps sps symTab
                            (ParamsExpression expression) -> 
                                case (expressionProcess scp expression symTab) of
                                    Just expType -> (TypePrimitive expType []) == rpType && (compareListOfTypesWithFuncCall scp rps sps symTab)
                                    Nothing -> False 
-- Checamos aqui que la llamada al constructor sea correcta
checkIfParamsAreCorrect :: Scope -> [Params] -> ClassIdentifier -> SymbolTable -> ClassSymbolTable -> Bool
checkIfParamsAreCorrect scp sendingParams classIdentifier symTab classTab = 
                                    case (Map.lookup classIdentifier classTab) of
                                        Just symbolTableOfClass -> 
                                                case (Map.lookup "_constructor" symbolTableOfClass) of
                                                    Just (symbolFunc) -> (compareListOfTypesWithFuncCall scp (map (\f -> fst f) (params symbolFunc)) sendingParams symTab)
                                                    Nothing -> False 
                                        Nothing -> False 

analyzeStatements :: [Statement] -> Scope -> SymbolTable -> ClassSymbolTable -> (SymbolTable,Bool)
analyzeStatements [] _ symTab _ = (symTab,False)
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
analyzeStatement (ConditionStatement (If expression (Block statements))) scp symTab classTab = 
                                                case (expressionProcess scp expression symTab) of
                                                    -- Si la expresión del if regresa booleano, entonces está bien
                                                    Just (PrimitiveBool) -> analyzeStatements statements (scp - 1) symTab classTab
                                                   -- De lo contrario, no se puede tener esa expresión en el if
                                                    _ -> (emptySymbolTable, True)  
analyzeStatement (CycleStatement (CycleWhile (While expression (Block statements)))) scp symTab classTab = 
                case (expressionProcess scp expression symTab) of
                    Just PrimitiveBool -> analyzeStatements statements (scp - 1) symTab classTab
                    _ -> (emptySymbolTable, True) 
analyzeStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) scp symTab classTab = 
                                                                            if (greaterRange > lowerRange) 
                                                                                then analyzeStatements statements (scp - 1) symTab classTab
                                                                                else (emptySymbolTable,True)
analyzeStatement (CycleStatement (CycleForVar statements)) scp symTab classTab = analyzeStatements statements scp symTab classTab
analyzeStatement (ReturnStatement (ReturnFunctionCall functionCall)) scp symTab classTab =  
            let isFuncCallOk = analyzeFunctionCall functionCall scp symTab classTab
            in if (isFuncCallOk) then (symTab,False)
                else (emptySymbolTable, True)
analyzeStatement (ReturnStatement (ReturnExp expression)) scp symTab classTab =  
            case (preProcessExpression scp expression symTab) of
                Just expType -> (symTab,False)
                Nothing -> (emptySymbolTable, True)

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
                                        if (varScp >= scp) then
                                         let typeIndexExp = (preProcessExpression scp expressionIndex symTab)
                                                        in case typeIndexExp of
                                                               Just (TypePrimitive primExp _) ->  
                                                                    primExp == prim
                                                               _ -> False
                                         else False
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _ ) -> 
                                        if (varScp >= scp) then
                                         let typeIndexExp = (preProcessExpression scp expressionIndex symTab)
                                                        in case typeIndexExp of
                                                               Just (TypeClassId classId _) ->  
                                                                    classIdentifier == classId
                                                               _ -> False
                                         else False 
                                    _ -> False                            
analyzeDisplay (DisplayVarArrayAccess identifier ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) ) scp symTab classTab =
                            case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : [])) varScp _ ) -> 
                                        if (varScp >= scp) 
                                            then
                                                let typeRowExp = (expressionProcess scp innerExpRow symTab)
                                                    typeColExp = (expressionProcess scp innerExpCol symTab)
                                                        in if (typeColExp == typeRowExp) then
                                                            case typeRowExp of
                                                               Just PrimitiveInt ->  True
                                                               Just PrimitiveInteger -> True
                                                            else False
                                            else False

                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _ ) -> 
                                        if (varScp >= scp)
                                            then
                                                let typeRowExp = (expressionProcess scp innerExpRow symTab)
                                                    typeColExp = (expressionProcess scp innerExpCol symTab)
                                                        in if (typeColExp == typeRowExp) then
                                                            case typeRowExp of
                                                               Just PrimitiveInt -> True
                                                               Just PrimitiveInteger -> True
                                                            else False
                                            else False 
                                    _ -> False 
analyzeFunctionCall :: FunctionCall -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
analyzeFunctionCall (FunctionCallVar funcIdentifier callParams) scp symTab classTab = 
                            case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    let funcParamTypes = map (\p -> fst p) params
                                                    in (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab)         
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
                                                                in (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab)  
                                                            _ -> False   
                                                _ -> False
                                        else False
                                    _ -> False

isAssignmentOk :: Assignment -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
isAssignmentOk (AssignmentExpression identifier expression) scp symTab classSymTab = case (Map.lookup identifier symTab) of
                                                                                        Just (SymbolVar (TypePrimitive prim []) varScp _) -> 
                                                                                                    -- Si el scope de esta variable es mayor al scope de este assignment, si puedo accederlo
                                                                                                    if (varScp >= scp) 
                                                                                                        then 
                                                                                                            let typeExp = (preProcessExpression scp expression symTab)
                                                                                                                in case typeExp of
                                                                                                                       Just (TypePrimitive primExp _) ->  
                                                                                                                            primExp == prim
                                                                                                                       _ -> False
                                                                                                        else False
                                                                                        Just (SymbolVar (TypeClassId classIdentifier []) varScp _) -> 
                                                                                                    -- Si el scope de esta variable es mayor al scope de este assignment, si puedo accederlo
                                                                                                    if (varScp >= scp) 
                                                                                                        then let typeExp = (preProcessExpression scp expression symTab)
                                                                                                                in case typeExp of
                                                                                                                       Just (TypeClassId classId _) ->  
                                                                                                                            classIdentifier == classId
                                                                                                                       _ -> False
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
                                                                                                                                                        && (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab)
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
                                                                            && (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab)
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
                                                    let typeExp = (preProcessExpression scp expression symTab)
                                                        in case typeExp of
                                                               Just dataTypeExp ->  
                                                                    dataTypeExp == attrDataType
                                                               _ -> False
                                                _ -> False   
                                    _ -> False
                            else False
                        _ -> False
isAssignmentOk  (AssignmentArrayExpression identifier ((ArrayAccessExpression innerExp) : []) expression) scp symTab classSymTab =  
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                   let typeIndexExp = (expressionProcess scp innerExp symTab)
                                            in case typeIndexExp of
                                                   Just PrimitiveInt ->  
                                                        case (preProcessExpression scp expression symTab) of
                                                            Just (TypePrimitive primExp _) -> primExp == prim
                                                            _ -> False 
                                                   Just PrimitiveInteger -> 
                                                         case (preProcessExpression scp expression symTab) of
                                                            Just (TypePrimitive primExp _) -> primExp == prim
                                                            _ -> False  
                                                   _ -> False
                            else False
                        Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    let typeIndexExp = (expressionProcess scp innerExp symTab)
                                            in case typeIndexExp of
                                                   Just PrimitiveInt ->  
                                                        case (preProcessExpression scp expression symTab) of
                                                            Just (TypeClassId classId _) -> classId == classIdentifier
                                                            _ -> False 
                                                   Just PrimitiveInteger -> 
                                                         case (preProcessExpression scp expression symTab) of
                                                            Just (TypeClassId classId _) -> classId == classIdentifier
                                                            _ -> False  
                                                   _ -> False
                            else False
                        _ -> False
isAssignmentOk  (AssignmentArrayExpression identifier ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) expression) scp symTab classSymTab = 
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",sizeRows,"]") : ("[",sizeCols,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    let typeRowExp = (expressionProcess scp innerExpRow symTab)
                                        typeColExp = (expressionProcess scp innerExpCol symTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Just PrimitiveInt ->  
                                                    case (preProcessExpression scp expression symTab) of 
                                                        Just (TypePrimitive primAssignment _) -> 
                                                            primAssignment == prim
                                                        _ -> False
                                                   Just PrimitiveInteger ->  
                                                    case (preProcessExpression scp expression symTab) of 
                                                        Just (TypePrimitive primAssignment _) -> 
                                                            primAssignment == prim
                                                        _ -> False
                                                   _ -> False
                                                else False
                            else False
                        Just (SymbolVar (TypeClassId classIdentifier (("[",sizeRows,"]") : ("[",sizeCols,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    let typeRowExp = (expressionProcess scp innerExpRow symTab)
                                        typeColExp = (expressionProcess scp innerExpCol symTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Just PrimitiveInt ->  
                                                    case (preProcessExpression scp expression symTab) of 
                                                        Just (TypeClassId classIdAssignment _) -> 
                                                            classIdentifier == classIdAssignment
                                                        _ -> False
                                                   Just PrimitiveInteger ->  
                                                    case (preProcessExpression scp expression symTab) of 
                                                        Just (TypeClassId classIdAssignment _) -> 
                                                            classIdentifier == classIdAssignment
                                                        _ -> False
                                                   _ -> False
                                                else False
                            else False
                        _ -> False 

isAssignmentOk _ _ _ _ = False

insertIdentifiers :: [Identifier] -> Symbol -> SymbolTable -> ClassSymbolTable -> (SymbolTable,Bool)
insertIdentifiers [] _ symTab _ = (symTab, False)
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
checkLiteralOrVariablesAndDataTypes :: Scope -> Type -> [LiteralOrVariable] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes _ _ [] _ = True
checkLiteralOrVariablesAndDataTypes scp dataType (litVar : litVars) symTab =  
                            if (checkLiteralOrVariableInSymbolTable scp litVar symTab) &&  (checkArrayAssignment scp dataType litVar symTab)
                                then checkLiteralOrVariablesAndDataTypes scp dataType litVars symTab
                                else False -- Alguna literal o variable asignada no existe, o bien, el tipo de dato que se esta asignando no concuerda con la declaracion

checkLiteralOrVariableInSymbolTable :: Scope -> LiteralOrVariable -> SymbolTable  -> Bool
checkLiteralOrVariableInSymbolTable scp (VarIdentifier identifier) symTab =  
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar _ varScp _) ->
                                        varScp >= scp
                                    _ -> False
checkLiteralOrVariableInSymbolTable _ _ _  = True -- Si es otra cosa que var identifier, entonces regresamos true, o sea, sin error

checkTypeExistance :: Type -> ClassSymbolTable -> Bool
checkTypeExistance (TypeClassId classIdentifier _) classTab = 
                                                  case (Map.lookup classIdentifier classTab) of
                                                  Just _ -> True -- Si existe esa clase
                                                  _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkTypeExistance _ _ = True -- Todos lo demas regresa true


checkLiteralOrVariablesAndDataTypes2D :: Scope -> Type -> [[LiteralOrVariable]] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes2D _ _ [] _ = True
checkLiteralOrVariablesAndDataTypes2D scp dataType (listOfLitVars : rest) symTab =  
                            if (checkLiteralOrVariablesAndDataTypes scp dataType listOfLitVars symTab)  
                                then checkLiteralOrVariablesAndDataTypes2D scp dataType rest symTab
                                else False -- Alguna literal o variable asignada no existe, o bien, el tipo de dato que se esta asignando no concuerda con la declaracion

-- Aqui checamos si el literal or variable que se esta asignando al arreglo sea del tipo indicado
-- es decir, en Humano [10] humanos = [h1,h2,h3,h4] checa que h1,h2,h3 y h4 sean del tipo humano
checkArrayAssignment :: Scope -> Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkArrayAssignment scp (TypePrimitive prim arrayDeclaration) (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive primVar []) varScp _) 
                                        | varScp >= scp -> 
                                                primVar == prim
                                        | otherwise -> False
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkArrayAssignment scp (TypeClassId classIdentifier arrayDeclaration) (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypeClassId classId []) varScp _)  
                                        |  varScp >= scp ->  
                                                classId == classIdentifier
                                        | otherwise -> False
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkArrayAssignment scp dataType litOrVar symTab  = checkDataTypes scp dataType litOrVar symTab

-- Aqui checamos si el literal or variable que se esta dando esta de acuerdo al que se esta asignando! O sea,
-- no es valido decir Int i = 1; Money m = i; 
checkDataTypes :: Scope -> Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkDataTypes scp dType (VarIdentifier identifier) symTab =  
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar dataType varScp _) 
                                        | varScp >= scp -> dataType == dType -- Si son iguales, regresamos true
                                        | otherwise -> False
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkDataTypes _ (TypePrimitive (PrimitiveInt) _) (IntegerLiteral _) _  = True
checkDataTypes _ (TypePrimitive (PrimitiveDouble) _) (DecimalLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveMoney) _) (DecimalLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveString) _) (StringLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveInteger) _) (IntegerLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveBool) _) (BoolLiteral _) _ = True
checkDataTypes _ _ _ _ = False -- Todo lo demas, falso

-- Podriamos necesitar preprocesar una expresion en busqueda de un literal or variable que sea un tipo de alguna clase
preProcessExpression :: Scope -> Expression -> SymbolTable -> Maybe Type
preProcessExpression scp (ExpressionLitVar (VarIdentifier identifier)) symTab =
                         case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier []) varScp _)  
                                        | varScp >= scp ->
                                            Just (TypeClassId classIdentifier [])
                                        | otherwise -> Nothing
                                    Just (SymbolVar (TypePrimitive prim []) varScp _)  
                                        | varScp >= scp ->
                                            Just (TypePrimitive prim [])
                                        | otherwise -> Nothing
                                    _ -> Nothing
preProcessExpression scp (ExpressionVarArray identifier ((ArrayAccessExpression expressionIndex) : [])) symTab =
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) varScp _)  
                                       | varScp >= scp ->
                                            let typeIndexExp = (expressionProcess scp expressionIndex symTab)
                                            in case typeIndexExp of
                                                   Just PrimitiveInt ->  Just (TypePrimitive prim [])
                                                   Just PrimitiveInteger ->  Just (TypePrimitive prim [])
                                                   _ -> Nothing
                                       | otherwise -> Nothing
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : []) ) varScp _)  
                                       | varScp >= scp ->
                                            let typeIndexExp = (expressionProcess scp expressionIndex symTab)
                                            in case typeIndexExp of
                                                   Just PrimitiveInt ->  Just (TypeClassId classIdentifier [])
                                                   Just PrimitiveInteger ->  Just (TypeClassId classIdentifier [])
                                                   _ -> Nothing
                                       | otherwise -> Nothing
                                    _ -> Nothing
preProcessExpression scp (ExpressionVarArray identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : [])) symTab =
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [])) varScp _) 
                                       | varScp >= scp ->
                                            let typeRowExp = (expressionProcess scp rowExp symTab)
                                                typeColExp = (expressionProcess scp colExp symTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Just PrimitiveInt ->  Just (TypePrimitive prim [])
                                                   Just PrimitiveInteger ->  Just (TypePrimitive prim [])
                                                   _ -> Nothing
                                                else Nothing
                                       | otherwise -> Nothing
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",rows,"]") : ("[",cols,"]") : [])) varScp _) 
                                       | varScp >= scp ->
                                            let typeRowExp = (expressionProcess scp rowExp symTab)
                                                typeColExp = (expressionProcess scp colExp symTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Just PrimitiveInt ->  Just (TypeClassId classIdentifier [])
                                                   Just PrimitiveInteger ->  Just (TypeClassId classIdentifier [])
                                                   _ -> Nothing
                                                else Nothing
                                      | otherwise -> Nothing
                                    _ -> Nothing
preProcessExpression scp expression symTab = case (expressionProcess scp expression symTab) of
                                                Just prim -> (Just (TypePrimitive prim []) )
                                                Nothing -> Nothing