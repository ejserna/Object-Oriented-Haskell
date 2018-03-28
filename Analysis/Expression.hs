module Expression where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty
import SymbolTable
import ClassSymbolTable
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

data ExpResult = ResDecimal Decimal
                 | ResBool Bool
                 

expressionProcess :: Scope -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive  
expressionProcess scp (ExpressionMult exp1 exp2) symTab classSymTab = expressionCheckOp scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionDiv exp1 exp2) symTab classSymTab = expressionCheckOp scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionPow exp1 exp2) symTab classSymTab = expressionCheckOp scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionPars exp) symTab classSymTab = expressionProcess scp exp symTab classSymTab
expressionProcess scp (ExpressionGreater exp1 exp2) symTab classSymTab = expressionCheckRel2 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionLower exp1 exp2) symTab classSymTab = expressionCheckRel2 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionGreaterEq exp1 exp2) symTab classSymTab = expressionCheckRel2 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionLowerEq exp1 exp2) symTab classSymTab = expressionCheckRel2 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionEqEq exp1 exp2) symTab classSymTab = expressionCheckRel1 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionNotEq exp1 exp2) symTab classSymTab = expressionCheckRel1 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionAnd exp1 exp2) symTab classSymTab = expressionCheckRel3 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionOr exp1 exp2) symTab classSymTab = expressionCheckRel3 scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionPlus exp1 exp2) symTab classSymTab = expressionCheckOp scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionMinus exp1 exp2) symTab classSymTab = expressionCheckOp scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionMod exp1 exp2) symTab classSymTab = expressionCheckMOD scp exp1 exp2 symTab classSymTab
expressionProcess scp (ExpressionNot exp) symTab classSymTab = expressionCheckNOT scp exp symTab classSymTab
expressionProcess scp (ExpressionLitVar litVar) symTab classSymTab = checkDataTypeOfLitVar scp litVar symTab
expressionProcess scp (ExpressionFuncCall funcCall) symTab classSymTab = case functionCallType funcCall scp symTab classSymTab of
                                                                          Just (TypePrimitive prim []) -> Just prim
                                                                          _ -> Nothing   

expressionProcess scp (ExpressionVarArray identifier ((ArrayAccessExpression expression) : [])) symTab classSymTab = case (expressionProcess scp expression symTab classSymTab) of 
                                                                                                        Just PrimitiveInt -> checkArrayID scp identifier symTab 1
                                                                                                        Just PrimitiveInteger -> checkArrayID scp identifier symTab 1
                                                                                                        _ -> Nothing
expressionProcess scp (ExpressionVarArray identifier ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[])) symTab classSymTab = 
                                                  case (expressionProcess scp expression1 symTab classSymTab) of 
                                                     Just PrimitiveInt -> 
                                                            case (expressionProcess scp expression2 symTab classSymTab) of
                                                              Just PrimitiveInt -> checkArrayID scp identifier symTab 2
                                                              Just PrimitiveInteger -> checkArrayID scp identifier symTab 2
                                                              _ -> Nothing  
                                                     Just PrimitiveInteger -> 
                                                            case (expressionProcess scp expression2 symTab classSymTab) of
                                                              Just PrimitiveInteger -> checkArrayID scp identifier symTab 2
                                                              Just PrimitiveInt -> checkArrayID scp identifier symTab 2
                                                              _ -> Nothing
                                                     _ -> Nothing
expressionProcess scp (ExpressionNeg exp) symTab classSymTab = expressionCheckNEG scp exp symTab classSymTab


checkArrayID :: Scope -> Identifier -> SymbolTable -> Int -> Maybe Primitive
checkArrayID scp identifier symTab dimension= case (Map.lookup identifier symTab) of
                             Just (SymbolVar dataType varScp _) 
                               | varScp >= scp -> 
                                   case dataType of 
                                     TypePrimitive prim arrayDeclaration | (length arrayDeclaration) == dimension -> Just prim
                                                       | otherwise -> Nothing
                                     _ -> Nothing
                               | otherwise -> Nothing
                             _ -> Nothing



checkDataTypeOfLitVar :: Scope -> LiteralOrVariable -> SymbolTable -> Maybe Primitive
checkDataTypeOfLitVar scp (VarIdentifier identifier) symTab = 
                                  case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim []) varScp _)
                                      | varScp >= scp -> (Just prim)
                                      | otherwise -> Nothing
                                    _ -> Nothing
checkDataTypeOfLitVar scp (IntegerLiteral int) symTab = (Just PrimitiveInt)
checkDataTypeOfLitVar scp (DecimalLiteral dec) symTab = (Just PrimitiveDouble)
checkDataTypeOfLitVar scp (StringLiteral int) symTab = (Just PrimitiveString)
checkDataTypeOfLitVar scp (BoolLiteral _) symTab = (Just PrimitiveBool)


expressionCheckOp :: Scope -> Expression -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckOp scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab classSymTab = checkDataTypesMult (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckOp scp (ExpressionLitVar litVar1) exp symTab classSymTab = checkDataTypesMult (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab classSymTab) symTab
expressionCheckOp scp exp (ExpressionLitVar litVar1) symTab classSymTab = expressionCheckOp scp (ExpressionLitVar litVar1) exp symTab classSymTab
expressionCheckOp scp exp1 exp2 symTab classSymTab = checkDataTypesMult (expressionProcess scp exp1 symTab classSymTab) (expressionProcess scp exp2 symTab classSymTab) symTab

expressionCheckMOD :: Scope -> Expression -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckMOD scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab classSymTab = checkDataTypesMOD  (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckMOD scp (ExpressionLitVar litVar1) exp symTab classSymTab = checkDataTypesMOD (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab classSymTab) symTab
expressionCheckMOD scp exp (ExpressionLitVar litVar1) symTab classSymTab = expressionCheckMOD scp (ExpressionLitVar litVar1) exp symTab classSymTab
expressionCheckMOD scp exp1 exp2 symTab classSymTab = checkDataTypesMOD (expressionProcess scp exp1 symTab classSymTab) (expressionProcess scp exp2 symTab classSymTab) symTab

expressionCheckNOT :: Scope -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckNOT scp (ExpressionLitVar litVar1) symTab classSymTab = checkDataTypesNOT (checkDataTypeOfLitVar scp litVar1 symTab) symTab
expressionCheckNOT scp exp symTab classSymTab = checkDataTypesNOT  (expressionProcess scp exp symTab classSymTab) symTab

expressionCheckNEG :: Scope -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckNEG scp (ExpressionLitVar litVar1) symTab classSymTab = checkDataTypesNEG (checkDataTypeOfLitVar scp litVar1 symTab) symTab
expressionCheckNEG scp exp symTab classSymTab = checkDataTypesNEG (expressionProcess scp exp symTab classSymTab) symTab

expressionCheckRel1 :: Scope -> Expression -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckRel1 scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab classSymTab = checkDataTypesRel1 (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckRel1 scp (ExpressionLitVar litVar1) exp symTab classSymTab = checkDataTypesRel1 (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab classSymTab) symTab
expressionCheckRel1 scp exp (ExpressionLitVar litVar1) symTab classSymTab = expressionCheckRel1 scp (ExpressionLitVar litVar1) exp symTab classSymTab
expressionCheckRel1 scp exp1 exp2 symTab classSymTab = checkDataTypesRel1 (expressionProcess scp exp1 symTab classSymTab) (expressionProcess scp exp2 symTab classSymTab) symTab

expressionCheckRel2 :: Scope -> Expression -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckRel2 scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab classSymTab = checkDataTypesRel2 (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckRel2 scp (ExpressionLitVar litVar1) exp symTab classSymTab = checkDataTypesRel2 (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab classSymTab) symTab
expressionCheckRel2 scp exp (ExpressionLitVar litVar1) symTab classSymTab = expressionCheckRel2 scp (ExpressionLitVar litVar1) exp symTab classSymTab
expressionCheckRel2 scp exp1 exp2 symTab classSymTab = checkDataTypesRel2 (expressionProcess scp exp1 symTab classSymTab) (expressionProcess scp exp2 symTab classSymTab) symTab

expressionCheckRel3 :: Scope -> Expression -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Primitive
expressionCheckRel3 scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab classSymTab = checkDataTypesRel3 (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckRel3 scp (ExpressionLitVar litVar1) exp symTab classSymTab = checkDataTypesRel3 (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab classSymTab) symTab
expressionCheckRel3 scp exp (ExpressionLitVar litVar1) symTab classSymTab = expressionCheckRel3 scp (ExpressionLitVar litVar1) exp symTab classSymTab
expressionCheckRel3 scp exp1 exp2 symTab classSymTab = checkDataTypesRel3 (expressionProcess scp exp1 symTab classSymTab) (expressionProcess scp exp2 symTab classSymTab) symTab

checkDataTypesNOT :: Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesNOT (Just PrimitiveBool) symTab = (Just PrimitiveBool)
checkDataTypesNOT _ _ = Nothing

checkDataTypesNEG :: Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesNEG (Just PrimitiveBool) symTab = Nothing
checkDataTypesNEG (Just PrimitiveString) symTab = Nothing
checkDataTypesNEG (Just prim) symTab = (Just prim)
checkDataTypesNEG Nothing symTab = Nothing

checkDataTypesMult :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesMult Nothing Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesMult _ Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesMult Nothing _ _ = Nothing -- Todo lo demas, falso
checkDataTypesMult (Just PrimitiveBool) _ _ = Nothing
checkDataTypesMult _ (Just PrimitiveBool) _ = Nothing 
checkDataTypesMult (Just PrimitiveInt) (Just PrimitiveInt) _  = (Just PrimitiveInt)
checkDataTypesMult (Just PrimitiveInteger) (Just PrimitiveInteger) _ = (Just PrimitiveInteger)
checkDataTypesMult (Just PrimitiveInt) (Just PrimitiveInteger) _ = (Just PrimitiveInteger)
checkDataTypesMult (Just PrimitiveInteger) (Just PrimitiveInt) _ = (Just PrimitiveInteger)
checkDataTypesMult (Just PrimitiveMoney) (Just PrimitiveDouble) _ = (Just PrimitiveMoney)
checkDataTypesMult (Just PrimitiveDouble) (Just PrimitiveMoney) _ = (Just PrimitiveMoney)
checkDataTypesMult (Just PrimitiveMoney) _ _ = (Just PrimitiveMoney)
checkDataTypesMult _ (Just PrimitiveMoney) _ = (Just PrimitiveMoney)
checkDataTypesMult (Just PrimitiveDouble) _ _ = (Just PrimitiveDouble)
checkDataTypesMult _ (Just PrimitiveDouble) _ = (Just PrimitiveDouble)
checkDataTypesMult (Just PrimitiveString) (Just PrimitiveString) _ = Just PrimitiveString 
checkDataTypesMult _ _ _ = Nothing -- Todo lo demas, falso


checkDataTypesRel1 :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive 
checkDataTypesRel1 Nothing Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesRel1 _ Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesRel1 Nothing _ _ = Nothing -- Todo lo demas, falso
checkDataTypesRel1 (Just PrimitiveBool) (Just PrimitiveBool) _ = (Just PrimitiveBool)
checkDataTypesRel1 (Just PrimitiveString) (Just PrimitiveString) _ = (Just PrimitiveBool)
checkDataTypesRel1 (Just PrimitiveBool) _ _  = Nothing
checkDataTypesRel1 _ (Just PrimitiveBool) _ = Nothing
checkDataTypesRel1 (Just PrimitiveMoney) (Just PrimitiveInteger) _ = Nothing
checkDataTypesRel1 (Just PrimitiveDouble) (Just PrimitiveInteger) _ = Nothing
checkDataTypesRel1 (Just PrimitiveMoney) (Just PrimitiveInt) _ = Nothing
checkDataTypesRel1 (Just PrimitiveDouble) (Just PrimitiveInt) _ = Nothing
checkDataTypesRel1 (Just PrimitiveInteger) (Just PrimitiveMoney) _ = Nothing
checkDataTypesRel1 (Just PrimitiveInteger) (Just PrimitiveDouble) _ = Nothing
checkDataTypesRel1 (Just PrimitiveInt) (Just PrimitiveMoney) _ = Nothing
checkDataTypesRel1 (Just PrimitiveInt) (Just PrimitiveDouble) _ = Nothing
checkDataTypesRel1 _ _ _ = (Just PrimitiveBool)

checkDataTypesRel2 :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive 
checkDataTypesRel2 Nothing Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesRel2 _ Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesRel2 Nothing _ _ = Nothing -- Todo lo demas, falso
checkDataTypesRel2 (Just PrimitiveBool) _ _  = Nothing
checkDataTypesRel2 _ (Just PrimitiveBool) _ = Nothing
checkDataTypesRel2 (Just PrimitiveString) _ _ = Nothing
checkDataTypesRel2 _ (Just PrimitiveString) _ = Nothing
checkDataTypesRel2 (Just PrimitiveMoney) (Just PrimitiveInteger) _ = Nothing
checkDataTypesRel2 (Just PrimitiveDouble) (Just PrimitiveInteger) _ = Nothing
checkDataTypesRel2 (Just PrimitiveMoney) (Just PrimitiveInt) _ = Nothing
checkDataTypesRel2 (Just PrimitiveDouble) (Just PrimitiveInt) _ = Nothing
checkDataTypesRel2 (Just PrimitiveInteger) (Just PrimitiveMoney) _ = Nothing
checkDataTypesRel2 (Just PrimitiveInteger) (Just PrimitiveDouble) _ = Nothing
checkDataTypesRel2 (Just PrimitiveInt) (Just PrimitiveMoney) _ = Nothing
checkDataTypesRel2 (Just PrimitiveInt) (Just PrimitiveDouble) _ = Nothing
checkDataTypesRel2 _ _ _ = (Just PrimitiveBool)

checkDataTypesRel3 :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesRel3 Nothing Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesRel3 _ Nothing _ = Nothing -- Todo lo demas, falso
checkDataTypesRel3 Nothing _ _ = Nothing -- Todo lo demas, falso
checkDataTypesRel3 (Just PrimitiveBool) (Just PrimitiveBool) _ = (Just PrimitiveBool)
checkDataTypesRel3 _ _ _ = Nothing

checkDataTypesMOD :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive 
checkDataTypesMOD (Just PrimitiveInt) (Just PrimitiveInt) _  = (Just PrimitiveInt)
checkDataTypesMOD (Just PrimitiveInteger) (Just PrimitiveInteger) _ = (Just PrimitiveInteger)
checkDataTypesMOD (Just PrimitiveInteger) (Just PrimitiveInt) _ = (Just PrimitiveInteger)
checkDataTypesMOD (Just PrimitiveInt) (Just PrimitiveInteger) _ = (Just PrimitiveInteger)
checkDataTypesMOD _ _ _ = Nothing -- Todo lo demas, falso


compareListOfTypesWithFuncCall :: Scope -> [Type] -> [Params] -> SymbolTable -> ClassSymbolTable -> Bool
compareListOfTypesWithFuncCall _ [] [] _ _ = True
compareListOfTypesWithFuncCall _ [] (sp : sps) _ _ = False
compareListOfTypesWithFuncCall _ (rpType : rps) [] _ _ = False
compareListOfTypesWithFuncCall scp (rpType : rps) (sp : sps) symTab classSymTab = 
                        case sp of 
                            (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression innerExp) : []))) ->
                                -- Checamos que sea un arreglo
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) varScp _) 
                                       | varScp >= scp -> 
                                            case (expressionProcess scp innerExp symTab classSymTab) of 
                                                Just PrimitiveInt ->  (TypePrimitive prim []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                Just PrimitiveInteger ->  (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                _ -> False
                                            
                                       | otherwise -> False
                                    Just (SymbolVar (TypeClassId classId (("[",size,"]") : []) ) varScp _) 
                                       | varScp >= scp -> 
                                                case (expressionProcess scp innerExp symTab classSymTab) of 
                                                    Just PrimitiveInt -> (TypeClassId classId []) == rpType 
                                                        && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    Just PrimitiveInteger -> (TypeClassId classId []) == rpType 
                                                        && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    _ -> False
                                       | otherwise -> False
                                    _ -> False
                            (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : []))) ->
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [])) varScp _)
                                        | varScp >= scp -> 
                                            let rowExpType = expressionProcess scp rowExp symTab classSymTab
                                                colExpType = expressionProcess scp colExp symTab classSymTab
                                            in if(rowExpType == colExpType) 
                                                then case rowExpType of 
                                                    Just PrimitiveInt -> (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    Just PrimitiveInteger -> (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    _ -> False
                                                else False
                                        | otherwise -> False
                                    Just (SymbolVar (TypeClassId classId (("[",rows,"]") : ("[",cols,"]") : [])) varScp _)
                                        | varScp >= scp -> 
                                            let rowExpType = expressionProcess scp rowExp symTab classSymTab
                                                colExpType = expressionProcess scp colExp symTab classSymTab
                                            in if(rowExpType == colExpType) 
                                                then case rowExpType of 
                                                    Just PrimitiveInt -> (TypeClassId classId []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    Just PrimitiveInteger -> (TypeClassId classId []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    _ -> False
                                                else False
                                        | otherwise -> False
                                    _ -> False
                            (ParamsExpression (ExpressionLitVar ((VarIdentifier identifier)))) ->
                                            checkDataTypes scp rpType (VarIdentifier identifier) symTab 
                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                            (ParamsExpression expression) -> 
                                case (expressionProcess scp expression symTab classSymTab) of
                                    Just expType -> (TypePrimitive expType []) == rpType && (compareListOfTypesWithFuncCall scp rps sps symTab classSymTab)
                                    Nothing -> False 

analyzeFunctionCall :: FunctionCall -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
analyzeFunctionCall (FunctionCallVar funcIdentifier callParams) scp symTab classTab = 
                            case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    let funcParamTypes = map (\p -> fst p) params
                                                    in (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab classTab)         
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
                                                                in (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab classTab)  
                                                            _ -> False   
                                                _ -> False
                                        else False
                                    _ -> False

functionCallType :: FunctionCall -> Scope -> SymbolTable -> ClassSymbolTable -> Maybe Type
functionCallType (FunctionCallVar funcIdentifier callParams) scp symTab classTab = 
                            case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    returnTypeFunc         
                                    _ -> Nothing
functionCallType (FunctionCallObjMem (ObjectMember objectIdentifier functionIdentifier) callParams) scp symTab classTab = 
                           case (Map.lookup objectIdentifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _ ) -> 
                                        if objScp >= scp 
                                            then case (Map.lookup classIdentifier classTab) of
                                                Just symbolTableOfClass ->
                                                        case (Map.lookup functionIdentifier symbolTableOfClass) of
                                                            -- Si y solo si es publica la funcion, la accedemos
                                                            Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                returnTypeFunc 
                                                            _ -> Nothing   
                                                _ -> Nothing
                                        else Nothing
                                    _ -> Nothing

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
