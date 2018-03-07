module Expression where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

import SymbolTable
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

expressionProcess :: Scope -> Expression -> SymbolTable -> Maybe Primitive  
expressionProcess scp (ExpressionMult exp1 exp2) symTab = expressionCheckOp scp exp1 exp2 symTab
expressionProcess scp (ExpressionDiv exp1 exp2) symTab = expressionCheckOp scp exp1 exp2 symTab
expressionProcess scp (ExpressionPow exp1 exp2) symTab = expressionCheckOp scp exp1 exp2 symTab
expressionProcess scp (ExpressionPars exp) symTab = expressionProcess scp exp symTab
expressionProcess scp (ExpressionGreater exp1 exp2) symTab = expressionCheckRel2 scp exp1 exp2 symTab
expressionProcess scp (ExpressionLower exp1 exp2) symTab = expressionCheckRel2 scp exp1 exp2 symTab
expressionProcess scp (ExpressionGreaterEq exp1 exp2) symTab = expressionCheckRel2 scp exp1 exp2 symTab
expressionProcess scp (ExpressionLowerEq exp1 exp2) symTab = expressionCheckRel2 scp exp1 exp2 symTab
expressionProcess scp (ExpressionEqEq exp1 exp2) symTab = expressionCheckRel1 scp exp1 exp2 symTab
expressionProcess scp (ExpressionNotEq exp1 exp2) symTab = expressionCheckRel1 scp exp1 exp2 symTab
expressionProcess scp (ExpressionAnd exp1 exp2) symTab = expressionCheckRel3 scp exp1 exp2 symTab
expressionProcess scp (ExpressionOr exp1 exp2) symTab = expressionCheckRel3 scp exp1 exp2 symTab
expressionProcess scp (ExpressionPlus exp1 exp2) symTab = expressionCheckOp scp exp1 exp2 symTab
expressionProcess scp (ExpressionMinus exp1 exp2) symTab = expressionCheckOp scp exp1 exp2 symTab
expressionProcess scp (ExpressionMod exp1 exp2) symTab = expressionCheckMOD scp exp1 exp2 symTab
expressionProcess scp (ExpressionNot exp) symTab = expressionCheckNOT scp exp symTab
expressionProcess scp (ExpressionLitVar litVar) symTab = checkDataTypeOfLitVar scp litVar symTab

expressionProcess scp (ExpressionVarArray identifier ((ArrayAccessExpression expression) : [])) symTab = case (expressionProcess scp expression symTab) of 
                                                                                                        Just PrimitiveInt -> checkArrayID scp identifier symTab 1
                                                                                                        Just PrimitiveInteger -> checkArrayID scp identifier symTab 1
                                                                                                        _ -> Nothing
expressionProcess scp (ExpressionVarArray identifier ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[])) symTab = 
                                                  case (expressionProcess scp expression1 symTab) of 
                                                     Just PrimitiveInt -> 
                                                            case (expressionProcess scp expression2 symTab) of
                                                              Just PrimitiveInt -> checkArrayID scp identifier symTab 2
                                                              _ -> Nothing  
                                                     Just PrimitiveInteger -> 
                                                            case (expressionProcess scp expression2 symTab) of
                                                              Just PrimitiveInteger -> checkArrayID scp identifier symTab 2
                                                              _ -> Nothing
                                                     _ -> Nothing
expressionProcess scp (ExpressionNeg exp) symTab = expressionCheckNEG scp exp symTab


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


expressionCheckOp :: Scope -> Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckOp scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesMult (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckOp scp (ExpressionLitVar litVar1) exp symTab = checkDataTypesMult (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab) symTab
expressionCheckOp scp exp (ExpressionLitVar litVar1) symTab = expressionCheckOp scp (ExpressionLitVar litVar1) exp symTab
expressionCheckOp scp exp1 exp2 symTab = checkDataTypesMult (expressionProcess scp exp1 symTab) (expressionProcess scp exp2 symTab) symTab

expressionCheckMOD :: Scope -> Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckMOD scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesMOD  (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckMOD scp (ExpressionLitVar litVar1) exp symTab = checkDataTypesMOD (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab) symTab
expressionCheckMOD scp exp (ExpressionLitVar litVar1) symTab = expressionCheckMOD scp (ExpressionLitVar litVar1) exp symTab
expressionCheckMOD scp exp1 exp2 symTab = checkDataTypesMOD (expressionProcess scp exp1 symTab) (expressionProcess scp exp2 symTab) symTab

expressionCheckNOT :: Scope -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckNOT scp (ExpressionLitVar litVar1) symTab = checkDataTypesNOT (checkDataTypeOfLitVar scp litVar1 symTab) symTab
expressionCheckNOT scp exp symTab = checkDataTypesNOT  (expressionProcess scp exp symTab) symTab

expressionCheckNEG :: Scope -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckNEG scp (ExpressionLitVar litVar1) symTab = checkDataTypesNEG (checkDataTypeOfLitVar scp litVar1 symTab) symTab
expressionCheckNEG scp exp symTab = checkDataTypesNEG (expressionProcess scp exp symTab) symTab

expressionCheckRel1 :: Scope -> Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckRel1 scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesRel1 (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckRel1 scp (ExpressionLitVar litVar1) exp symTab = checkDataTypesRel1 (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab) symTab
expressionCheckRel1 scp exp (ExpressionLitVar litVar1) symTab = expressionCheckRel1 scp (ExpressionLitVar litVar1) exp symTab
expressionCheckRel1 scp exp1 exp2 symTab = checkDataTypesRel1 (expressionProcess scp exp1 symTab) (expressionProcess scp exp2 symTab) symTab

expressionCheckRel2 :: Scope -> Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckRel2 scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesRel2 (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckRel2 scp (ExpressionLitVar litVar1) exp symTab = checkDataTypesRel2 (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab) symTab
expressionCheckRel2 scp exp (ExpressionLitVar litVar1) symTab = expressionCheckRel2 scp (ExpressionLitVar litVar1) exp symTab
expressionCheckRel2 scp exp1 exp2 symTab = checkDataTypesRel2 (expressionProcess scp exp1 symTab) (expressionProcess scp exp2 symTab) symTab

expressionCheckRel3 :: Scope -> Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckRel3 scp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesRel3 (checkDataTypeOfLitVar scp litVar1 symTab) (checkDataTypeOfLitVar scp litVar2 symTab) symTab
expressionCheckRel3 scp (ExpressionLitVar litVar1) exp symTab = checkDataTypesRel3 (checkDataTypeOfLitVar scp litVar1 symTab) (expressionProcess scp exp symTab) symTab
expressionCheckRel3 scp exp (ExpressionLitVar litVar1) symTab = expressionCheckRel3 scp (ExpressionLitVar litVar1) exp symTab
expressionCheckRel3 scp exp1 exp2 symTab = checkDataTypesRel3 (expressionProcess scp exp1 symTab) (expressionProcess scp exp2 symTab) symTab

checkDataTypesNOT :: Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesNOT (Just PrimitiveBool) symTab = (Just PrimitiveBool)
checkDataTypesNOT _ _ = Nothing

checkDataTypesNEG :: Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesNEG (Just PrimitiveBool) symTab = Nothing
checkDataTypesNEG (Just PrimitiveString) symTab = Nothing
checkDataTypesNEG (Just prim) symTab = (Just prim)

checkDataTypesMult :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive
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
checkDataTypesMult _ _ _ = Nothing -- Todo lo demas, falso  
                               

checkDataTypesRel1 :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive 
checkDataTypesRel1 (Just PrimitiveBool) (Just PrimitiveBool) _ = (Just PrimitiveBool)
checkDataTypesRel1 (Just PrimitiveBool) _ _  = Nothing
checkDataTypesRel1 _ (Just PrimitiveBool) _ = Nothing
checkDataTypesRel1 (Just PrimitiveString) _ _ = Nothing
checkDataTypesRel1 _ (Just PrimitiveString) _ = Nothing
checkDataTypesRel1 (Just PrimitiveInt) (Just PrimitiveDouble) _ = Nothing
checkDataTypesRel1 (Just PrimitiveDouble) (Just PrimitiveInt) _ = Nothing
checkDataTypesRel1 (Just PrimitiveInteger) (Just PrimitiveDouble) _ = Nothing
checkDataTypesRel1 (Just PrimitiveDouble) (Just PrimitiveInteger) _ = Nothing
checkDataTypesRel1 _ _ _ = (Just PrimitiveBool)

checkDataTypesRel2 :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive 
checkDataTypesRel2 (Just PrimitiveBool) _ _  = Nothing
checkDataTypesRel2 _ (Just PrimitiveBool) _ = Nothing
checkDataTypesRel2 (Just PrimitiveString) _ _ = Nothing
checkDataTypesRel2 _ (Just PrimitiveString) _ = Nothing
checkDataTypesRel2 _ _ _ = (Just PrimitiveBool)

checkDataTypesRel3 :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive
checkDataTypesRel3 (Just PrimitiveBool) (Just PrimitiveBool) _ = (Just PrimitiveBool)
checkDataTypesRel3 _ _ _ = Nothing

checkDataTypesMOD :: Maybe Primitive -> Maybe Primitive -> SymbolTable -> Maybe Primitive 
checkDataTypesMOD (Just PrimitiveInt) (Just PrimitiveInt) _  = (Just PrimitiveInt)
checkDataTypesMOD (Just PrimitiveInteger) (Just PrimitiveInteger) _ = (Just PrimitiveInteger)
checkDataTypesMOD (Just PrimitiveInteger) (Just PrimitiveInt) _ = (Just PrimitiveInteger)
checkDataTypesMOD (Just PrimitiveInt) (Just PrimitiveInteger) _ = (Just PrimitiveInteger)
checkDataTypesMOD _ _ _ = Nothing -- Todo lo demas, falso
