module ExpressionAnalysis where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty

import SymbolTable
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

expressionProcess :: Expression -> SymbolTable -> Maybe Primitive  
expressionProcess (ExpressionMult exp1 exp2) symTab = expressionCheckOp exp1 exp2 symTab
expressionProcess (ExpressionDiv exp1 exp2) symTab = expressionCheckOp exp1 exp2 symTab
expressionProcess (ExpressionPow exp1 exp2) symTab = expressionCheckOp exp1 exp2 symTab
expressionProcess (ExpressionPars exp) symTab = expressionProcess exp symTab
expressionProcess (ExpressionGreater exp1 exp2) symTab = expressionCheckRel2 exp1 exp2 symTab
expressionProcess (ExpressionLower exp1 exp2) symTab = expressionCheckRel2 exp1 exp2 symTab
expressionProcess (ExpressionGreaterEq exp1 exp2) symTab = expressionCheckRel2 exp1 exp2 symTab
expressionProcess (ExpressionLowerEq exp1 exp2) symTab = expressionCheckRel2 exp1 exp2 symTab
expressionProcess (ExpressionEqEq exp1 exp2) symTab = expressionCheckRel1 exp1 exp2 symTab
expressionProcess (ExpressionNotEq exp1 exp2) symTab = expressionCheckRel1 exp1 exp2 symTab
expressionProcess (ExpressionAnd exp1 exp2) symTab = expressionCheckRel3 exp1 exp2 symTab
expressionProcess (ExpressionOr exp1 exp2) symTab = expressionCheckRel3 exp1 exp2 symTab
expressionProcess (ExpressionPlus exp1 exp2) symTab = expressionCheckOp exp1 exp2 symTab
expressionProcess (ExpressionMinus exp1 exp2) symTab = expressionCheckOp exp1 exp2 symTab
expressionProcess (ExpressionMod exp1 exp2) symTab = expressionCheckMOD exp1 exp2 symTab
expressionProcess (ExpressionNot exp) symTab = expressionCheckNOT exp symTab
expressionProcess (ExpressionLitVar litVar) symTab = checkDataTypeOfLitVar litVar symTab

expressionProcess (ExpressionVarArray identifier ((ArrayAccessExpression expression) : [])) symTab = case (expressionProcess expression symTab) of 
                                                                                                        Just PrimitiveInt -> checkArrayID identifier symTab 1
                                                                                                        Just PrimitiveInteger -> checkArrayID identifier symTab 1
                                                                                                        _ -> Nothing
expressionProcess (ExpressionVarArray identifier ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[])) symTab = 
                                                  case (expressionProcess expression1 symTab) of 
                                                     Just PrimitiveInt -> 
                                                            case (expressionProcess expression2 symTab) of
                                                              Just PrimitiveInt -> checkArrayID identifier symTab 2
                                                              _ -> Nothing  
                                                     Just PrimitiveInteger -> 
                                                            case (expressionProcess expression2 symTab) of
                                                              Just PrimitiveInteger -> checkArrayID identifier symTab 2
                                                              _ -> Nothing
                                                     _ -> Nothing
expressionProcess (ExpressionNeg exp) symTab = expressionCheckNEG exp symTab


checkArrayID :: Identifier -> SymbolTable -> Int -> Maybe Primitive
checkArrayID identifier symTab dimension= case (Map.lookup identifier symTab) of
                             Just (SymbolVar dataType _ _) -> 
                               case dataType of 
                                 TypePrimitive prim arrayDeclaration | (length arrayDeclaration) == dimension -> Just prim
                                                   | otherwise -> Nothing
                                 _ -> Nothing
                             _ -> Nothing



checkDataTypeOfLitVar :: LiteralOrVariable -> SymbolTable -> Maybe Primitive
checkDataTypeOfLitVar (VarIdentifier identifier) symTab = case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim _) _ _) -> (Just prim)
checkDataTypeOfLitVar (IntegerLiteral int) symTab = (Just PrimitiveInt)
checkDataTypeOfLitVar (DecimalLiteral dec) symTab = (Just PrimitiveDouble)
checkDataTypeOfLitVar (StringLiteral int) symTab = (Just PrimitiveString)
checkDataTypeOfLitVar (BoolLiteral _) symTab = (Just PrimitiveBool)






expressionCheckOp :: Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckOp (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesMult (checkDataTypeOfLitVar litVar1 symTab) (checkDataTypeOfLitVar litVar2 symTab) symTab
expressionCheckOp (ExpressionLitVar litVar1) exp symTab = checkDataTypesMult (checkDataTypeOfLitVar litVar1 symTab) (expressionProcess exp symTab) symTab
expressionCheckOp exp (ExpressionLitVar litVar1) symTab = expressionCheckOp (ExpressionLitVar litVar1) exp symTab
expressionCheckOp exp1 exp2 symTab = checkDataTypesMult (expressionProcess exp1 symTab) (expressionProcess exp2 symTab) symTab

expressionCheckMOD :: Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckMOD (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesMOD (checkDataTypeOfLitVar litVar1 symTab) (checkDataTypeOfLitVar litVar2 symTab) symTab
expressionCheckMOD (ExpressionLitVar litVar1) exp symTab = checkDataTypesMOD (checkDataTypeOfLitVar litVar1 symTab) (expressionProcess exp symTab) symTab
expressionCheckMOD exp (ExpressionLitVar litVar1) symTab = expressionCheckMOD (ExpressionLitVar litVar1) exp symTab
expressionCheckMOD exp1 exp2 symTab = checkDataTypesMOD (expressionProcess exp1 symTab) (expressionProcess exp2 symTab) symTab

expressionCheckNOT :: Expression -> SymbolTable -> Maybe Primitive
expressionCheckNOT (ExpressionLitVar litVar1) symTab = checkDataTypesNOT (checkDataTypeOfLitVar litVar1 symTab) symTab
expressionCheckNOT exp symTab = checkDataTypesNOT (expressionProcess exp symTab) symTab

expressionCheckNEG :: Expression -> SymbolTable -> Maybe Primitive
expressionCheckNEG (ExpressionLitVar litVar1) symTab = checkDataTypesNEG (checkDataTypeOfLitVar litVar1 symTab) symTab
expressionCheckNEG exp symTab = checkDataTypesNEG (expressionProcess exp symTab) symTab

expressionCheckRel1 :: Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckRel1 (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesRel1 (checkDataTypeOfLitVar litVar1 symTab) (checkDataTypeOfLitVar litVar2 symTab) symTab
expressionCheckRel1 (ExpressionLitVar litVar1) exp symTab = checkDataTypesRel1 (checkDataTypeOfLitVar litVar1 symTab) (expressionProcess exp symTab) symTab
expressionCheckRel1 exp (ExpressionLitVar litVar1) symTab = expressionCheckRel1 (ExpressionLitVar litVar1) exp symTab
expressionCheckRel1 exp1 exp2 symTab = checkDataTypesRel1 (expressionProcess exp1 symTab) (expressionProcess exp2 symTab) symTab

expressionCheckRel2 :: Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckRel2 (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesRel2 (checkDataTypeOfLitVar litVar1 symTab) (checkDataTypeOfLitVar litVar2 symTab) symTab
expressionCheckRel2 (ExpressionLitVar litVar1) exp symTab = checkDataTypesRel2 (checkDataTypeOfLitVar litVar1 symTab) (expressionProcess exp symTab) symTab
expressionCheckRel2 exp (ExpressionLitVar litVar1) symTab = expressionCheckRel2 (ExpressionLitVar litVar1) exp symTab
expressionCheckRel2 exp1 exp2 symTab = checkDataTypesRel2 (expressionProcess exp1 symTab) (expressionProcess exp2 symTab) symTab

expressionCheckRel3 :: Expression -> Expression -> SymbolTable -> Maybe Primitive
expressionCheckRel3 (ExpressionLitVar litVar1) (ExpressionLitVar litVar2) symTab = checkDataTypesRel3 (checkDataTypeOfLitVar litVar1 symTab) (checkDataTypeOfLitVar litVar2 symTab) symTab
expressionCheckRel3 (ExpressionLitVar litVar1) exp symTab = checkDataTypesRel3 (checkDataTypeOfLitVar litVar1 symTab) (expressionProcess exp symTab) symTab
expressionCheckRel3 exp (ExpressionLitVar litVar1) symTab = expressionCheckRel3 (ExpressionLitVar litVar1) exp symTab
expressionCheckRel3 exp1 exp2 symTab = checkDataTypesRel3 (expressionProcess exp1 symTab) (expressionProcess exp2 symTab) symTab

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
