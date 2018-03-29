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


data SymbolEnvironment = SymbolEnvironment
                {   symTab :: SymbolTable, 
                    classTab :: ClassSymbolTable
                }
                deriving (Show)

data MemoryState = MemoryState
                {   idAddressMap :: IdentifierAddressMap, 
                    constAddressMap :: ConstantAddressMap,
                    objAddressMap :: ObjectAddressMap,
                    funcMap :: FunctionMap,
                    varCounters :: VariableCounters,
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
            let constantAddressMap = (constAddressMap stateAfterConstants5)
            (stateAfterVariablesInStatements,_) <- execRWST (prepareAddressMapsFromSymbolTable) env stateAfterConstants5
            let (idMap,constMap, objMap, funcMap, varCounters, litCounters) = getCurrentMemoryState stateAfterVariablesInStatements
            -- let (varCounters,newIdMap,objectAddressMap) = (prepareAddressMapsFromSymbolTable symTab classSymTab (startIntGlobalMemory,startDecimalGlobalMemory,startStringGlobalMemory,startBoolGlobalMemory, startObjectGlobalMemory)
            --                                                     (Map.empty) (Map.empty))
        
            startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab varCounters idMap constMap objMap 

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
                                                        
        
                
prepareAddressMapsFromSymbolTable :: MA
prepareAddressMapsFromSymbolTable = 
                            do 
                                env <-  ask
                                let symTabList = (Map.toList (symTab env))
                                fillIdentifierAddressMap symTabList 

fillIdentifierAddressMap :: [(Identifier,Symbol)] -> MA
fillIdentifierAddressMap [] = return ()
fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim []) _ _)) : rest ) =
                        do
                            memState <- get
                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters memState)
                            let identifierAddressMap = (idAddressMap memState)
                            case prim of
                                PrimitiveBool -> 
                                                do 
                                                    let newIdMap = (Map.insert identifier boolGC identifierAddressMap)
                                                    modify $ \s -> (s { varCounters = (intGC,decGC,strGC,boolGC + 1,objGC) }) 
                                                    modify $ \s -> (s { idAddressMap = newIdMap })
                                                    fillIdentifierAddressMap rest
                                PrimitiveInt ->
                                                do 
                                                    let newIdMap = (Map.insert identifier intGC identifierAddressMap)
                                                    modify $ \s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMap = newIdMap })
                                                    fillIdentifierAddressMap rest

                                PrimitiveInteger -> do 
                                                        let newIdMap = (Map.insert identifier intGC identifierAddressMap)
                                                        modify $ \s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                        modify $ \s -> (s { idAddressMap = newIdMap })
                                                        fillIdentifierAddressMap rest 
                                PrimitiveString -> do 
                                                    let newIdMap = (Map.insert identifier strGC identifierAddressMap)
                                                    modify $ \s -> (s { varCounters = (intGC,decGC,strGC + 1,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMap = newIdMap })
                                                    fillIdentifierAddressMap rest
                                PrimitiveMoney -> do 
                                                    let newIdMap = (Map.insert identifier decGC identifierAddressMap)
                                                    modify $ \s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMap = newIdMap })
                                                    fillIdentifierAddressMap rest
                                PrimitiveDouble -> do 
                                                    let newIdMap = (Map.insert identifier decGC identifierAddressMap)
                                                    modify $ \s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                    modify $ \s -> (s { idAddressMap = newIdMap })
                                                    fillIdentifierAddressMap rest
fillIdentifierAddressMap ( (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)) : rest ) = 
                            do
                                memState <- get
                                let (intGC,decGC,strGC,boolGC,objGC) = (varCounters memState)
                                let identifierAddressMap = (idAddressMap memState)
                                case (arrayAccess) of 
                                    [] -> 
                                        do 
                                            env <- ask
                                            currentMemState <- get
                                            (idMapFromObject,newMemState, _) <- liftIO $ runRWST (insertObjectInObjectAddressMap (TypeClassId classId arrayAccess)) env currentMemState
                                            modify $ \s -> newMemState 
                                            let newObjAddressMap = (objAddressMap newMemState)
                                            let identifierAddressMap = (idAddressMap newMemState)
                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters newMemState)
                                            let newIdMap = (Map.insert identifier objGC identifierAddressMap) 
                                            let newObjMap = (Map.insert objGC idMapFromObject newObjAddressMap)
                                            modify $ \s -> (s { varCounters = (intGC,decGC,strGC,boolGC,objGC + 1) }) 
                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                            modify $ \s -> (s { objAddressMap = newObjMap })
                                            fillIdentifierAddressMap rest 
                                    (("[",size,"]") : []) -> 
                                         do 
                                            fillArray size size "" (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))
                                            updateArrayClasses size size "" (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))
                                            fillIdentifierAddressMap rest

                                    (("[",rows,"]") : ("[",cols,"]")  : [] ) ->
                                          do 
                                            fillMatrix rows cols rows (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))
                                            updateMatrixClasses rows cols rows (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))
                                            fillIdentifierAddressMap rest

fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic)) : rest ) =
                                                    do 
                                                        fillArray size size "" (identifier,(SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic))
                                                        fillIdentifierAddressMap rest
                                                        

fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic)) : rest ) =
                                                    do 
                                                        let dt = (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : []))
                                                        fillMatrix rows cols rows (identifier,(SymbolVar dt scp isPublic))
                                                        fillIdentifierAddressMap rest

-- MARK TODO: Funciones
updateArrayClasses  :: Integer -> Integer -> String -> (Identifier,Symbol) -> MA
updateArrayClasses 0 size _ _   = return ()
updateArrayClasses limit size strToAppend ( (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))) =
                            do 
                                memState <- get
                                let identifierAddressMap = (idAddressMap memState)
                                case (Map.lookup (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) identifierAddressMap) of
                                    Just address -> 
                                        do
                                            env <- ask
                                            currentMemState <- get
                                            (idMapFromObject,newMemState, _) <- liftIO $ runRWST (insertObjectInObjectAddressMap (TypeClassId classId arrayAccess)) env currentMemState
                                            modify $ \s -> newMemState 
                                            let (idMap,constMap, newObjAddressMap, funcMap, varCounters, litCounters) = getCurrentMemoryState newMemState
                                            let newObjMap = (Map.insert address idMapFromObject newObjAddressMap)
                                            modify $ \s -> (s { varCounters = varCounters }) 
                                            modify $ \s -> (s { idAddressMap = idMap })
                                            modify $ \s -> (s { objAddressMap = newObjMap })
                                            updateArrayClasses (limit - 1) size strToAppend ((identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)))
            

updateMatrixClasses  :: Integer -> Integer -> Integer -> (Identifier,Symbol)  -> MA
updateMatrixClasses  0  _ _ _  = return ()
updateMatrixClasses rows columns fixedRows idAndSymbol =
        do 
            updateArrayClasses columns columns ("[" ++ (show (fixedRows - rows)) ++ "]")  idAndSymbol
            updateMatrixClasses (rows - 1) columns fixedRows idAndSymbol

fillArray  :: Integer -> Integer -> String -> (Identifier,Symbol)  -> MA
fillArray 0 size _ _  = return ()
fillArray limit size strToAppend ( (identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic))) = 
                                do 
                                    currentMemState <- get
                                    let currentObjAddressMap = (objAddressMap currentMemState)
                                    let identifierAddressMap = (idAddressMap currentMemState)
                                    let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                    let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) objGC identifierAddressMap) 
                                    -- Como es un arreglo de objetos, es importante asignarle su propio identifier address map a cada
                                    -- celda, y por el momento, estan vacios. Esto es necesario para asegurar que se guarden de manera contigua.
                                    let newObjMap = (Map.insert objGC (Map.empty) currentObjAddressMap)
                                    modify $ \s -> (s { varCounters = (intGC,decGC,strGC,boolGC,objGC + 1) }) 
                                    modify $ \s -> (s { idAddressMap = newIdMap })
                                    modify $ \s -> (s { objAddressMap = newObjMap }) 
                                    fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypeClassId classId arrayAccess) scp isPublic)))

fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveBool arrayAccess) scp isPublic))) = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMap currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) boolGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCounters = (intGC,decGC,strGC,boolGC + 1,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveBool arrayAccess) scp isPublic))) 

fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveString arrayAccess) scp isPublic))) =
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMap currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) strGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCounters = (intGC,decGC,strGC + 1,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveString arrayAccess) scp isPublic))) 
fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInteger arrayAccess) scp isPublic)))  = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMap currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) intGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInteger arrayAccess) scp isPublic))) 
fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInt arrayAccess) scp isPublic))) =
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMap currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) intGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveInt arrayAccess) scp isPublic)))

fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveMoney arrayAccess) scp isPublic))) = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMap currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) decGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveMoney arrayAccess) scp isPublic)))
fillArray limit size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveDouble arrayAccess) scp isPublic))) = 
                                                                        do 
                                                                            currentMemState <- get
                                                                            let identifierAddressMap = (idAddressMap currentMemState)
                                                                            let (intGC,decGC,strGC,boolGC,objGC) = (varCounters currentMemState)
                                                                            let newIdMap = (Map.insert (identifier ++ strToAppend ++ "[" ++ (show (size - limit) ++ "]")) decGC identifierAddressMap)
                                                                            modify $ \s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC) }) 
                                                                            modify $ \s -> (s { idAddressMap = newIdMap })
                                                                            fillArray (limit - 1) size strToAppend ((identifier,(SymbolVar (TypePrimitive PrimitiveDouble arrayAccess) scp isPublic)))


fillMatrix :: Integer -> Integer -> Integer -> (Identifier,Symbol)  -> MA
fillMatrix  0 _ _ _  = return ()
fillMatrix rows columns fixedRows idAndSymbol = 
                                do 
                                    fillArray columns columns ("[" ++ (show (fixedRows - rows)) ++ "]") idAndSymbol
                                    fillMatrix (rows - 1) columns fixedRows idAndSymbol

insertObjectInObjectAddressMap ::  Type -> MemoryAllocator IdentifierAddressMap 
insertObjectInObjectAddressMap (TypeClassId classId arrayAccess) =
                                do 
                                    env <- ask
                                    let classSymTab = (classTab env)
                                    case (Map.lookup classId classSymTab) of
                                        Just symbolTableOfClass -> 
                                            do 
                                                currentMemState <- get
                                                let (idMap,constMap, objMap, funcMap, varCounters, litCounters) = getCurrentMemoryState currentMemState
                                                -- Cambiamos el estado por el momento para que ahora analice los atributos dentro de la symbol table de la clase
                                                (stateAfterAttributesInserted,_) <- liftIO $ execRWST (prepareAddressMapsFromSymbolTable) 
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
                    let constantAddressMap = (constAddressMap memState)
                    case litOrVar of
                         IntegerLiteral int -> case (Map.lookup ("<int>" ++ (show int)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ ->  do 
                                                            let newConsAddressMap = (Map.insert ("<int>" ++ (show int)) intLitC constantAddressMap)
                                                            modify $ \s -> (s { literalCounters = (intLitC + 1, decLitC,strLitC,boolLitC) }) 
                                                            modify $ \s -> (s { constAddressMap = newConsAddressMap }) 
                         DecimalLiteral dec -> case (Map.lookup ("<dec>" ++ (show dec)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ -> do
                                                            let newConsAddressMap = (Map.insert ("<dec>" ++ (show dec)) decLitC constantAddressMap)
                                                            modify $ \s -> (s { literalCounters = (intLitC, decLitC + 1,strLitC,boolLitC) }) 
                                                            modify $ \s -> (s { constAddressMap = newConsAddressMap }) 
                         StringLiteral str -> case (Map.lookup ("<str>" ++ (show str)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ -> do 
                                                            let newConsAddressMap = (Map.insert ("<str>" ++ str) strLitC constantAddressMap)
                                                            modify $ \s -> (s { literalCounters = (intLitC, decLitC,strLitC + 1,boolLitC) }) 
                                                            modify $ \s -> (s { constAddressMap = newConsAddressMap }) 
                         BoolLiteral bool -> case (Map.lookup ("<bool>" ++ (show bool)) constantAddressMap) of
                                                    Just _ -> return ()
                                                    _ -> do 
                                                         let newConsAddressMap = (Map.insert ("<bool>" ++ (show bool)) boolLitC constantAddressMap)
                                                         modify $ \s -> (s { literalCounters = (intLitC, decLitC,strLitC,boolLitC + 1) }) 
                                                         modify $ \s -> (s { constAddressMap = newConsAddressMap }) 
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
                                    

