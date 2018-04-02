{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module MemoryAllocator where 
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import Data.Decimal
import DataTypes
import CodeGenDataTypes
import Quadruple
import SymbolTable
import ClassSymbolTable
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import ExpressionOptimizer
import CodeGen
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe


data SymbolEnvironment = SymbolEnvironment
                {   symTabMem :: SymbolTable, 
                    classTabMem :: ClassSymbolTable
                }
                deriving (Show)

data MemoryState = MemoryState
                {   idAddressMapMem :: IdentifierAddressMap, 
                    constTable :: ConstantAddressMap,
                    objAddressMapMem :: ObjectAddressMap,
                    funcMapMem :: FunctionMap,
                    varCountersMem :: VariableCounters,
                    literalCounters :: LiteralCounters
                }
                deriving (Show)


type MemoryAllocator a =  RWST SymbolEnvironment () MemoryState IO a

type MA =  MemoryAllocator ()

setMemoryState :: IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> FunctionMap -> VariableCounters -> LiteralCounters -> MemoryState
setMemoryState idMap consMap objMap funcMap varCounters litCounters = MemoryState  idMap consMap objMap funcMap varCounters litCounters

getCurrentMemoryState :: MemoryState -> (IdentifierAddressMap,ConstantAddressMap,ObjectAddressMap,FunctionMap,VariableCounters,LiteralCounters)
getCurrentMemoryState (MemoryState i c o f v l) = (i,c,o,f,v,l)

setEnvironment :: SymbolTable -> ClassSymbolTable -> SymbolEnvironment
setEnvironment symTab classSymTab = SymbolEnvironment symTab classSymTab

startMemoryAllocation :: Program -> SymbolTable -> ClassSymbolTable -> IO()
startMemoryAllocation (Program classes functions variables (Block statements)) symTab classSymTab =
           do 
            let env = (setEnvironment symTab classSymTab)
            let memState = setMemoryState Map.empty Map.empty Map.empty Map.empty (startIntGlobalMemory,startDecimalGlobalMemory,startStringGlobalMemory,startBoolGlobalMemory, startObjectGlobalMemory) (startIntLiteralMemory,startDecimalLiteralMemory,startStringLiteralMemory,startBoolLiteralMemory)
            (stateAfterConstants1,_) <-  execRWST (prepareConstantAddressMap statements) env memState
            (stateAfterConstants2,_) <- execRWST (fillFromExpression (ExpressionLitVar $ DecimalLiteral 0.0) ) env stateAfterConstants1 
            (stateAfterConstants3,_) <- execRWST (fillFromExpression (ExpressionLitVar $ IntegerLiteral 0 ) ) env stateAfterConstants2
            (stateAfterConstants4,_) <- execRWST (fillFromExpression (ExpressionLitVar $ StringLiteral "") ) env stateAfterConstants3
            (stateAfterConstants5,_) <- execRWST (fillFromExpression (ExpressionLitVar $ BoolLiteral True) ) env stateAfterConstants4
            let constantAddressMap = (constTable stateAfterConstants5)
            (stateAfterVariablesInStatements,_) <- execRWST (prepareAddressMapsFromSymbolTable "_main_") env stateAfterConstants5
            let (idMap,constMap, objMap, funcMap, varCounters, litCounters) = getCurrentMemoryState stateAfterVariablesInStatements
            -- putStrLn $ ppShow $(sortBy (compare `on` fst) (Map.toList funcMap) ) 
            -- let (varCountersMem,newIdMap,objectAddressMap) = (prepareAddressMapsFromSymbolTable symTab classSymTab (startIntGlobalMemory,startDecimalGlobalMemory,startStringGlobalMemory,startBoolGlobalMemory, startObjectGlobalMemory)
            --                                                     (Map.empty) (Map.empty))
        
            startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab varCounters idMap constMap objMap funcMap "_main_"

prepareConstantAddressMap :: [Statement] -> MA
prepareConstantAddressMap []  = return ()
prepareConstantAddressMap (st : sts) = 
            do 
                fillFromStatement st
                prepareConstantAddressMap sts
                return ()

fillFromStatement :: Statement -> MA
fillFromStatement (AssignStatement assignment)  = fillFromAssignment assignment
fillFromStatement (VariableStatement var)  = fillFromVariable var
fillFromStatement (ConditionStatement (If expression (Block statements))) = do 
                                                                                fillFromExpression expression
                                                                                prepareConstantAddressMap statements
fillFromStatement (ConditionStatement (IfElse expression (Block statements) (Block statements2))) = do 
                                                                                                        fillFromExpression expression
                                                                                                        prepareConstantAddressMap statements
                                                                                                        prepareConstantAddressMap statements2
fillFromStatement (CycleStatement (CycleWhile (While expression (Block statements)))) = do 
                                                                                          fillFromExpression expression
                                                                                          prepareConstantAddressMap statements
fillFromStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) = do
                                                                                                    fillFromExpression (ExpressionLitVar $ IntegerLiteral $ lowerRange)
                                                                                                    fillFromExpression (ExpressionLitVar $ IntegerLiteral $ greaterRange)
                                                                                                    prepareConstantAddressMap statements
fillFromStatement (CycleStatement (CycleForVar statements)) = prepareConstantAddressMap statements
fillFromStatement (DPMStatement assignment) = fillFromStatement (AssignStatement assignment)
fillFromStatement (FunctionCallStatement functionCall) = fillFromFunctionCall functionCall
fillFromStatement (ReturnStatement (ReturnExp expression)) = fillFromExpression expression
fillFromStatement (ReturnStatement (ReturnFunctionCall functionCall))  = fillFromFunctionCall functionCall
fillFromStatement (DisplayStatement displays) = fillFromDisplays displays
                                                                where 
                                                                    fillFromDisplays :: [Display] -> MA
                                                                    fillFromDisplays [] = return ()
                                                                    fillFromDisplays (disp : disps)  =
                                                                        do 
                                                                            fillFromDisplay disp
                                                                            fillFromDisplays disps 

                                                                    fillFromDisplay :: Display -> MA
                                                                    fillFromDisplay (DisplayLiteralOrVariable litOrVar _)  =
                                                                        fillFromExpression (ExpressionLitVar litOrVar) 
                                                                    fillFromDisplay (DisplayFunctionCall funcCall _) =
                                                                        fillFromExpression (ExpressionFuncCall funcCall)
                                                                    fillFromDisplay (DisplayVarArrayAccess identifier arrayAccess _) =
                                                                        fillFromExpression (ExpressionVarArray identifier arrayAccess)  
                                                                    fillFromDisplay _  = return ()
fillFromStatement _  = return ()

fillFromVariable :: Variable  -> MA
fillFromVariable (VariableAssignmentLiteralOrVariable dataType identifier literalOrVariable)  =
                                case dataType of
                                    (TypePrimitive _ []) ->  fillFromExpression (ExpressionLitVar literalOrVariable) 
                                    (TypePrimitive _ (("[",size,"]") : []) ) -> 
                                            do 
                                                fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral size))) : []))
                                                fillFromExpression (ExpressionLitVar literalOrVariable) 
                                    (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]") : [] ) ) -> 
                                            do 
                                                fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral rows))) : (ArrayAccessExpression (ExpressionLitVar (IntegerLiteral cols))) : [])) 
                                                fillFromExpression (ExpressionLitVar literalOrVariable)
                                    (TypeClassId _ []) ->  
                                            do fillFromExpression (ExpressionLitVar literalOrVariable) 
                                    (TypeClassId _ (("[",size,"]") : []) ) -> 
                                            do 
                                                fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral size))) : []))
                                                fillFromExpression (ExpressionLitVar literalOrVariable) 
                                    (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] ) ) -> 
                                            do 
                                                fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral rows))) : (ArrayAccessExpression (ExpressionLitVar (IntegerLiteral cols))) : [])) 
                                                fillFromExpression (ExpressionLitVar literalOrVariable)
fillFromVariable (VariableNoAssignment dataType _)  =
                                case dataType of
                                    (TypePrimitive _ []) ->  return ()
                                    (TypePrimitive _ (("[",size,"]") : []) ) -> 
                                        fillFromExpression (ExpressionVarArray "" ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral size))) : [])) 
                                    (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]") : [] ) ) -> 
                                        fillFromExpression  (ExpressionVarArray "" ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral rows))) : (ArrayAccessExpression (ExpressionLitVar (IntegerLiteral cols))) : [])) 
                                    (TypeClassId _ []) ->  return ()
                                    (TypeClassId _ (("[",size,"]") : []) ) -> 
                                       fillFromExpression (ExpressionVarArray "" ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral size))) : [])) 
                                    (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] ) ) -> 
                                        fillFromExpression (ExpressionVarArray "" ((ArrayAccessExpression $ ExpressionLitVar $ IntegerLiteral rows) : (ArrayAccessExpression $ ExpressionLitVar $ IntegerLiteral cols) : [])) 
fillFromVariable (VariableAssignment1D dataType identifier literalOrVariables)  = 
                                 case dataType of
                                    (TypePrimitive _ (("[",size,"]") : []) ) -> 
                                        do 
                                            fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral size))) : [])) 
                                            fillFromLiteralOrVariables literalOrVariables
                                    (TypeClassId _ (("[",size,"]") : []) ) -> 
                                        do 
                                            fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral size))) : [])) 
                                            fillFromLiteralOrVariables literalOrVariables 
                                    
fillFromVariable (VariableAssignment2D dt identifier listOfListVars) =
                                  case dt of
                                    (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]") : [] ) ) -> 
                                        do 
                                            fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral rows))) : (ArrayAccessExpression (ExpressionLitVar (IntegerLiteral cols))) : []))
                                            fillFromListOfLiteralOrVariables listOfListVars
                                    (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] ) ) -> 
                                        do 
                                            fillFromExpression (ExpressionVarArray identifier ((ArrayAccessExpression (ExpressionLitVar (IntegerLiteral rows))) : (ArrayAccessExpression (ExpressionLitVar (IntegerLiteral cols))) : []))
                                            fillFromListOfLiteralOrVariables listOfListVars
fillFromVariable (VariableAssignmentObject _ _ (ObjectCreation _ params)) = 
                                fillFromCallParams params
fillFromVariable _  = return ()


fillFromLiteralOrVariables :: [LiteralOrVariable] -> MA
fillFromLiteralOrVariables []  = return ()
fillFromLiteralOrVariables (litVar : litVars)  = 
            do 
                fillFromExpression (ExpressionLitVar litVar)
                fillFromLiteralOrVariables litVars

fillFromListOfLiteralOrVariables :: [[LiteralOrVariable]] -> MA
fillFromListOfLiteralOrVariables []  = return ()
fillFromListOfLiteralOrVariables (listLitVars : rest) = do 
                                                            fillFromLiteralOrVariables listLitVars
                                                            fillFromListOfLiteralOrVariables rest


fillFromAssignment :: Assignment -> MA
fillFromAssignment (AssignmentExpression identifier expression) = fillFromExpression expression
fillFromAssignment  (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) =  fillFromExpression expression
fillFromAssignment  (AssignmentArrayExpression _ ((ArrayAccessExpression innerExp) : []) expression) =  
                                                                                                    do
                                                                                                        fillFromExpression innerExp
                                                                                                        fillFromExpression expression

fillFromAssignment  (AssignmentArrayExpression _ ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) expression) = 
                                                                                                    do
                                                                                                        fillFromExpression innerExpRow
                                                                                                        fillFromExpression innerExpCol
                                                                                                        fillFromExpression expression
fillFromAssignment _  = return ()


fillFromFunctionCall :: FunctionCall -> MA
fillFromFunctionCall (FunctionCallVar _ callParams) = 
                            fillFromCallParams  callParams
fillFromFunctionCall (FunctionCallObjMem (ObjectMember _ _) callParams) = 
                            fillFromCallParams callParams

fillFromCallParams :: [Params] -> MA
fillFromCallParams [] = return ()
fillFromCallParams ((ParamsExpression exp) : params) = do
                                                        fillFromExpression exp
                                                        fillFromCallParams params
                                                        
        
                
prepareAddressMapsFromSymbolTable :: String -> MA
prepareAddressMapsFromSymbolTable fromModule = 
                            do 
                                env <-  ask
                                let symTabList = (Map.toList (symTabMem env))
                                fillIdentifierAddressMap symTabList fromModule

fillIdentifierAddressMap :: [(Identifier,Symbol)] -> String -> MemoryAllocator ()
fillIdentifierAddressMap [] _ = return ()
fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim []) _ _)) : rest ) fromModule =
                        do
                            memState <- get
                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem memState)
                            let identifierAddressMap = (idAddressMapMem memState)
                            case prim of
                                PrimitiveBool -> 
                                                do 
                                                    let newIdMap = (Map.insert identifier boolGC identifierAddressMap)
                                                    modify $ \s -> (s { varCountersMem = (intGC,decGC,strGC,boolGC + 1,objGC) }) 
                                                    modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                    fillIdentifierAddressMap rest fromModule
                                PrimitiveInt ->
                                                do 
                                                    let newIdMap = (Map.insert identifier intGC identifierAddressMap)
                                                    modify $ \s -> (s { varCountersMem = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                    fillIdentifierAddressMap rest fromModule

                                PrimitiveInteger -> do 
                                                        let newIdMap = (Map.insert identifier intGC identifierAddressMap)
                                                        modify $ \s -> (s { varCountersMem = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                        modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                        fillIdentifierAddressMap rest fromModule
                                PrimitiveString -> do 
                                                    let newIdMap = (Map.insert identifier strGC identifierAddressMap)
                                                    modify $ \s -> (s { varCountersMem = (intGC,decGC,strGC + 1,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                    fillIdentifierAddressMap rest fromModule
                                PrimitiveMoney -> do 
                                                    let newIdMap = (Map.insert identifier decGC identifierAddressMap)
                                                    modify $ \s -> (s { varCountersMem = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                    fillIdentifierAddressMap rest fromModule
                                PrimitiveDouble -> do 
                                                    let newIdMap = (Map.insert identifier decGC identifierAddressMap)
                                                    modify $ \s -> (s { varCountersMem = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                    fillIdentifierAddressMap rest fromModule
fillIdentifierAddressMap ( (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)) : rest ) fromModule = 
                            do
                                -- Aqui podemos añadir un check de que si ese classId es igual al modulo actual, se pueden parar en la 10000 iteracion
                                memState <- get
                                let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem memState)
                                let identifierAddressMap = (idAddressMapMem memState)
                                case (arrayAccess) of 
                                    [] -> 
                                        do 
                                            env <- ask
                                            currentMemState <- get
                                            (idMapFromObject,newMemState, _) <- liftIO $ runRWST (insertObjectInObjectAddressMap (TypeClassId classId arrayAccess) ("_" ++ classId ++ "_") ) env currentMemState 
                                            modify $ \s -> newMemState 
                                            let newObjAddressMap = (objAddressMapMem newMemState)
                                            let identifierAddressMap = (idAddressMapMem newMemState)
                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem newMemState)
                                            let newIdMap = (Map.insert identifier objGC identifierAddressMap) 
                                            let newObjMap = (Map.insert objGC idMapFromObject newObjAddressMap)
                                            modify $ \s -> (s { varCountersMem = (intGC,decGC,strGC,boolGC,objGC + 1) }) 
                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                            modify $ \s -> (s { objAddressMapMem = newObjMap })
                                            fillIdentifierAddressMap rest fromModule
                                    (("[",size,"]") : []) -> 
                                         do 
                                            fillArray size size "" (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))
                                            updateArrayClasses size size "" (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)) fromModule
                                            fillIdentifierAddressMap rest fromModule

                                    (("[",rows,"]") : ("[",cols,"]")  : [] ) ->
                                          do 
                                            fillMatrix rows cols rows (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))
                                            updateMatrixClasses rows cols rows (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)) fromModule
                                            fillIdentifierAddressMap rest fromModule

fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic)) : rest ) fromModule =
                                                    do 
                                                        fillArray size size "" (identifier,(SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic))
                                                        fillIdentifierAddressMap rest fromModule
                                                        

fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic)) : rest ) fromModule =
                                                    do 
                                                        let dt = (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : []))
                                                        fillMatrix rows cols rows (identifier,(SymbolVar dt scp isPublic))
                                                        fillIdentifierAddressMap rest fromModule

fillIdentifierAddressMap ((identifier,(SymbolFunction params p1 (Block statements) p2 p3 p4 symTabFunc)) : rest) fromModule =
                                                    do 
                                                        env <- ask
                                                        memState <- get
                                                        let funcMap = (funcMapMem memState)
                                                        if (Map.member (fromModule ++ identifier) funcMap) then fillIdentifierAddressMap rest fromModule
                                                            else do 
  
                                                                    (stateAfterFuncConstants,_) <-  liftIO $ execRWST (prepareConstantAddressMap statements) (setEnvironment symTabFunc (classTabMem env)) 
                                                                                                                                                    (setMemoryState (Map.empty) (constTable memState) 
                                                                                                                                                                 (Map.empty) funcMap (startIntLocalMemory,startDecimalLocalMemory,startStringLocalMemory,startBoolLocalMemory, startObjectLocalMemory)
                                                                                                                                                                 (literalCounters memState))
                                                                    
                                                                    -- Obtenemos las dependencias de la funcion, para primero realizar los metodos de las funciones que utiliza la funcion
                                                                    let funcs1 = (Map.filter getFuncs symTabFunc)
                                                                    let functionDependenciesMap = updateDependenciesForChildClasses fromModule funcs1 (classTabMem env)
                                                                    
                                                                    -- liftIO $ putStrLn.ppShow $ (functionDependenciesMap)
                                                                    -- let functionDependenciesMapNoSelf = (Map.filter )
                                                                    -- liftIO $ putStrLn.ppShow $ (fromModule ++ identifier)
                                                                    let functionSymTabNoFuncs = (Map.filter filterFuncs symTabFunc)
                                                                    -- Para herencia, tenemos que meter los atritibutos de la clase de la funcion hijo
                                                                    let attributesWithSymbols = getAttributesOfCurrentClassWithSymbol fromModule (classTabMem env)
                                                                    let functionSymTabNoFuncsList = (Map.toList functionSymTabNoFuncs)
                                                                    let functionSymTabWithNewAttributes = (Map.fromList (functionSymTabNoFuncsList ++ attributesWithSymbols))
                                                                    (stateAfterDependencies,_) <- liftIO $ execRWST (prepareAddressMapsFromSymbolTable (fromModule)) (setEnvironment functionDependenciesMap (classTabMem env)) stateAfterFuncConstants
                                                                    
                                                                    (stateAfterVariablesFromFunc,_) <- liftIO $ execRWST (prepareAddressMapsFromSymbolTable (fromModule)) (setEnvironment functionSymTabWithNewAttributes (classTabMem env)) stateAfterDependencies
                                                                    let (idMapFromFunc, newConstTable, funcObjMap,funcMap,varCountersFromFunc,newLiteralCounters) = getCurrentMemoryState stateAfterVariablesFromFunc
                                                                    let paramsAddresses = fillParamsFromFunction params idMapFromFunc
                                                                    -- Se inserta un funcData vacio para funciones recursivas
                                                                    let funcData = FunctionData [] paramsAddresses idMapFromFunc funcObjMap
                                                                    let newFuncMap = (Map.insert (fromModule ++ identifier) funcData funcMap)
                                                                    -- Para funciones recursivas
                                                                    let symTabFuncWithOwnFunc = (Map.insert identifier (SymbolFunction params p1 (Block statements) p2 p3 p4 symTabFunc) symTabFunc)
                                                                    let cgState = (setCGState symTabFuncWithOwnFunc varCountersFromFunc 0)
                                                                    let cgEnv = setCGEnvironment (classTabMem env) funcObjMap idMapFromFunc newConstTable newFuncMap (fromModule)
                                                                    (_,quads) <-  liftIO $ execRWST (generateCodeFromStatements statements) cgEnv cgState
                                                                    -- Se actualiza el que se acaba de insertar, ahora con los quads recien generados
                                                                    let funcData = FunctionData quads paramsAddresses idMapFromFunc funcObjMap
                                                                    let newFuncMap = (Map.insert (fromModule ++ identifier) funcData funcMap)
                                                                    modify $ \s -> (s { literalCounters = newLiteralCounters }) 
                                                                    modify $ \s -> (s { constTable = newConstTable })
                                                                    modify $ \s -> (s { funcMapMem = newFuncMap })
                                                                   
                                                                    -- liftIO $ putStrLn $ (fromModule ++ identifier)
                                                                    -- liftIO $ putStrLn.ppShow $ funcData
                                                                    fillIdentifierAddressMap rest fromModule



updateDependenciesForChildClasses :: String -> SymbolTable -> ClassSymbolTable -> SymbolTable
updateDependenciesForChildClasses "_main_" funcs1 classSymTab = funcs1
updateDependenciesForChildClasses fromModule funcs1 classSymTab = 
                                     let functionsClass = (fromJust (Map.lookup (getClassNameFromCurrentModule fromModule) classSymTab))
                                     in (Map.filter getFuncs (Map.intersection functionsClass funcs1))


getFuncs :: Symbol -> Bool
getFuncs (SymbolFunction _ _ _ _ _ _ _) = True
getFuncs _ = False

filterFuncs :: Symbol -> Bool
filterFuncs (SymbolFunction _ _ _ _ _ _ _) = False
filterFuncs _ = True

fillParamsFromFunction :: [(Type,Identifier)] -> IdentifierAddressMap -> [Address]
fillParamsFromFunction [] _ = []
fillParamsFromFunction ((dt,id) : ids) idMapFunc = 
                                                    case dt of
                                                        (TypePrimitive prim []) ->
                                                            case (Map.lookup id idMapFunc) of 
                                                                Just address -> [address] ++ (fillParamsFromFunction ids idMapFunc)
                                                        (TypePrimitive prim (("[",size,"]") : [])) ->
                                                            case (Map.lookup (id ++ "[0]") idMapFunc) of
                                                                Just addressBase ->
                                                                    ( fillArrayParam  addressBase size) ++ (fillParamsFromFunction ids idMapFunc)
                                                        (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [])) ->
                                                            case (Map.lookup (id ++ "[0][0]") idMapFunc) of
                                                                Just addressBase ->
                                                                    (fillArrayParam  addressBase (rows * cols)) ++ (fillParamsFromFunction ids idMapFunc)
                                                        (TypeClassId _ []) ->
                                                            case (Map.lookup id idMapFunc) of 
                                                                Just address -> [address] ++ (fillParamsFromFunction ids idMapFunc)
                                                        (TypeClassId _ (("[",size,"]") : [])) ->
                                                            case (Map.lookup (id ++ "[0]") idMapFunc) of 
                                                                Just addressBase ->
                                                                    ( fillArrayParam  addressBase size) ++ (fillParamsFromFunction ids idMapFunc)
                                                        (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [])) ->
                                                            case (Map.lookup (id ++ "[0][0]") idMapFunc) of
                                                                Just addressBase ->
                                                                    (fillArrayParam  addressBase (rows * cols)) ++ (fillParamsFromFunction ids idMapFunc)

fillArrayParam :: Address -> Integer -> [Address]
fillArrayParam address size = let addresses = [address..] 
                                in take (fromIntegral size) addresses

-- MARK TODO: Funciones
updateArrayClasses  :: Integer -> Integer -> String -> (Identifier,Symbol) -> String -> MA
updateArrayClasses 0 size _ _ _  = return ()
updateArrayClasses limit size strToAppend ( (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))) _ =
                            do 
                                memState <- get
                                let identifierAddressMap = (idAddressMapMem memState)
                                case (Map.lookup (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) identifierAddressMap) of
                                    Just address -> 
                                        do
                                            env <- ask
                                            currentMemState <- get
                                            (idMapFromObject,newMemState, _) <- liftIO $ runRWST (insertObjectInObjectAddressMap (TypeClassId classId arrayAccess) ("_"++ classId ++ "_")) env currentMemState 
                                            modify $ \s -> newMemState 
                                            let (idMap,constMap, newObjAddressMap, funcMap, varCounters, litCounters) = getCurrentMemoryState newMemState

                                            let newObjMap = (Map.insert address idMapFromObject newObjAddressMap)
                                            modify $ \s -> (s { varCountersMem = varCounters }) 
                                            modify $ \s -> (s { idAddressMapMem = idMap })
                                            modify $ \s -> (s { objAddressMapMem = newObjMap })
                                            modify $ \s -> (s { funcMapMem = funcMap })
                                            updateArrayClasses (limit - 1) size strToAppend ((identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))) ("_"++ classId ++ "_")
            

updateMatrixClasses  :: Integer -> Integer -> Integer -> (Identifier,Symbol) -> String  -> MA
updateMatrixClasses  0  _ _ _ _ = return ()
updateMatrixClasses rows columns fixedRows idAndSymbol fromModule =
        do 
            updateArrayClasses columns columns ("[" ++ (show (fixedRows - rows)) ++ "]")  idAndSymbol fromModule
            updateMatrixClasses (rows - 1) columns fixedRows idAndSymbol fromModule

fillArray  :: Integer -> Integer -> String -> (Identifier,Symbol)  -> MA
fillArray 0 size _ _  = return ()
fillArray limit size strToAppend ( (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))) = 
                                do 
                                    currentMemState <- get
                                    let currentObjAddressMap = (objAddressMapMem currentMemState)
                                    let identifierAddressMap = (idAddressMapMem currentMemState)
                                    let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                    let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) objGC identifierAddressMap) 
                                    -- Como es un arreglo de objetos, es importante asignarle su propio identifier address map a cada
                                    -- celda, y por el momento, estan vacios. Esto es necesario para asegurar que se guarden de manera contigua.
                                    let newObjMap = (Map.insert objGC (Map.empty) currentObjAddressMap)
                                    modify $ \s -> (s { varCountersMem = (intGC,decGC,strGC,boolGC,objGC + 1) }) 
                                    modify $ \s -> (s { idAddressMapMem = newIdMap })
                                    modify $ \s -> (s { objAddressMapMem = newObjMap }) 
                                    fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)))

fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveBool arrayAccess) scp isPublic))) = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMapMem currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) boolGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCountersMem = (intGC,decGC,strGC,boolGC + 1,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveBool arrayAccess) scp isPublic))) 

fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveString arrayAccess) scp isPublic))) =
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMapMem currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) strGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCountersMem = (intGC,decGC,strGC + 1,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveString arrayAccess) scp isPublic))) 
fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInteger arrayAccess) scp isPublic)))  = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMapMem currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) intGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCountersMem = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInteger arrayAccess) scp isPublic))) 
fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInt arrayAccess) scp isPublic))) =
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMapMem currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) intGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCountersMem = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInt arrayAccess) scp isPublic)))

fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveMoney arrayAccess) scp isPublic))) = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMapMem currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) decGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCountersMem = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveMoney arrayAccess) scp isPublic)))
fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveDouble arrayAccess) scp isPublic))) = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMapMem currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCountersMem currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) decGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCountersMem = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMapMem = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveDouble arrayAccess) scp isPublic)))


fillMatrix :: Integer -> Integer -> Integer -> (Identifier,Symbol)  -> MA
fillMatrix  0 _ _ _  = return ()
fillMatrix rows columns fixedRows idAndSymbol = 
                                do 
                                    fillArray columns columns ("[" ++ (show (fixedRows - rows)) ++ "]") idAndSymbol
                                    fillMatrix (rows - 1) columns fixedRows idAndSymbol

insertObjectInObjectAddressMap ::  Type -> String -> MemoryAllocator IdentifierAddressMap 
insertObjectInObjectAddressMap (TypeClassId classId arrayAccess) fromModule =
                                do 
                                    env <- ask
                                    let classSymTab = (classTabMem env)
                                    case (Map.lookup classId classSymTab) of
                                        Just symbolTableOfClass -> 
                                            do 
                                                currentMemState <- get
                                                let (idMap,constMap, objMap, funcMap, varCounters, litCounters) = getCurrentMemoryState currentMemState
                                                -- Cambiamos el estado por el momento para que ahora analice los atributos dentro de la symbol table de la clase
                                                (stateAfterAttributesInserted,_) <- liftIO $ execRWST (prepareAddressMapsFromSymbolTable ("_" ++ classId ++ "_")) 
                                                                                                      (setEnvironment symbolTableOfClass classSymTab) 
                                                                                                      -- La mandamos vacia porque lo que obtendremos es una IDMap llena con los
                                                                                                      -- atributos de esa clase!
                                                                                                      (setMemoryState (Map.empty) constMap objMap funcMap varCounters litCounters)
                                                -- Tenemos que obtener el IdentifierAddressMap de los atributos de esta clase
                                                let (idMapObject,constMap2, objMap2, funcMap2, varCounters2, litCounters2) = getCurrentMemoryState stateAfterAttributesInserted
                                                let updatedState = (setMemoryState idMap constMap2 objMap2 funcMap2 varCounters2 litCounters2)
                                                modify $ \s -> updatedState -- Usamos el nuevo estado con los nuevos contadores!!!
                                                return idMapObject


                            
fillFromExpression :: Expression -> MA
fillFromExpression expression = fillFromExpressionAdaptee (reduceExpression expression)

fillFromExpressionAdaptee :: Expression -> MA
fillFromExpressionAdaptee (ExpressionMult exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionPlus exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionPow exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionDiv exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionMinus exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionPars exp) = fillFromExpression exp
fillFromExpressionAdaptee (ExpressionNeg exp) = fillFromExpression  exp
fillFromExpressionAdaptee (ExpressionMod exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionGreater exp1 exp2) = fillFromTwoExpressions  exp1 exp2
fillFromExpressionAdaptee (ExpressionLower exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionGreaterEq exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionLowerEq exp1 exp2) = fillFromTwoExpressions  exp1 exp2
fillFromExpressionAdaptee (ExpressionEqEq exp1 exp2) = fillFromTwoExpressions exp1 exp2
fillFromExpressionAdaptee (ExpressionNotEq exp1 exp2) = fillFromTwoExpressions  exp1 exp2
fillFromExpressionAdaptee (ExpressionAnd exp1 exp2) = fillFromTwoExpressions  exp1 exp2
fillFromExpressionAdaptee (ExpressionOr exp1 exp2) = fillFromTwoExpressions  exp1 exp2
fillFromExpressionAdaptee (ExpressionNot exp) = fillFromExpression  exp
fillFromExpressionAdaptee (ExpressionLitVar litOrVar) = 
                do 
                    memState <- get
                    let (intLitC,decLitC,strLitC,boolLitC) = (literalCounters memState)
                    let constantAddressMap = (constTable memState)
                    case litOrVar of
                         IntegerLiteral int -> case (Map.lookup ("<int>" ++ (show int)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ ->  do 
                                                            let newConsAddressMap = (Map.insert ("<int>" ++ (show int)) intLitC constantAddressMap)
                                                            modify $ \s -> (s { literalCounters = (intLitC + 1, decLitC,strLitC,boolLitC) }) 
                                                            modify $ \s -> (s { constTable = newConsAddressMap }) 
                         DecimalLiteral dec -> case (Map.lookup ("<dec>" ++ (show dec)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ -> do
                                                            let newConsAddressMap = (Map.insert ("<dec>" ++ (show dec)) decLitC constantAddressMap)
                                                            modify $ \s -> (s { literalCounters = (intLitC, decLitC + 1,strLitC,boolLitC) }) 
                                                            modify $ \s -> (s { constTable = newConsAddressMap }) 
                         StringLiteral str -> case (Map.lookup ("<str>" ++ (show str)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ -> do 
                                                            let newConsAddressMap = (Map.insert ("<str>" ++ str) strLitC constantAddressMap)
                                                            modify $ \s -> (s { literalCounters = (intLitC, decLitC,strLitC + 1,boolLitC) }) 
                                                            modify $ \s -> (s { constTable = newConsAddressMap }) 
                         BoolLiteral bool -> case (Map.lookup ("<bool>" ++ (show bool)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ -> do 
                                                         let newConsAddressMap = (Map.insert ("<bool>" ++ (show bool)) boolLitC constantAddressMap)
                                                         modify $ \s -> (s { literalCounters = (intLitC, decLitC,strLitC,boolLitC + 1) }) 
                                                         modify $ \s -> (s { constTable = newConsAddressMap }) 
                         _ -> return ()
fillFromExpressionAdaptee  (ExpressionVarArray _ ((ArrayAccessExpression expression) : []))  = 
                            fillFromExpression expression
fillFromExpressionAdaptee (ExpressionVarArray _ ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[])) =
                            fillFromTwoExpressions expression1 expression2
fillFromExpressionAdaptee (ExpressionFuncCall funcCall) =
                            fillFromFunctionCall funcCall 

fillFromTwoExpressions :: Expression -> Expression -> MA
fillFromTwoExpressions exp1 exp2 = do
                                        memState <- get
                                        fillFromExpression exp1
                                        fillFromExpression exp2
                                        return ()
                                    

