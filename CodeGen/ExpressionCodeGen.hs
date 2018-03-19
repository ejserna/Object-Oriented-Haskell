module ExpressionCodeGen where
import Data.Decimal
import DataTypes
import Quadruple
import SymbolTable
import Expression
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

expCodeGen :: SymbolTable -> ConstantAddressMap -> IdentifierAddressMap -> VariableCounters -> QuadNum -> Expression -> (VariableCounters,[Quadruple],QuadNum)
expCodeGen symTab constaddressMap idaddressMap (intLitC, decLitC, strLitC, boolLitC) quadNum (ExpressionLitVar (DecimalLiteral dec)) = case (Map.lookup ("<dec>" ++ (show (dec))) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<dec>0") constaddressMap) of
                                                                                                                                                                    Just address -> ((intLitC, decLitC + 1, strLitC, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decLitC)))], quadNum + 1)  
expCodeGen symTab constaddressMap idaddressMap (intLitC, decLitC, strLitC, boolLitC) quadNum (ExpressionLitVar (IntegerLiteral int)) = case (Map.lookup ("<int>" ++ (show (int))) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<int>0") constaddressMap) of
                                                                                                                                                                    Just address -> ((intLitC + 1, decLitC, strLitC, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intLitC)))], quadNum + 1) 
expCodeGen symTab constaddressMap idaddressMap (intLitC, decLitC, strLitC, boolLitC) quadNum (ExpressionLitVar (StringLiteral str)) = case (Map.lookup ("<str>" ++ (str)) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<str>") constaddressMap) of
                                                                                                                                                                    Just address -> ((intLitC, decLitC, strLitC + 1, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (strLitC)))], quadNum + 1) 
expCodeGen symTab constaddressMap idaddressMap (intLitC, decLitC, strLitC, boolLitC) quadNum (ExpressionLitVar (BoolLiteral bool)) = case (Map.lookup ("<bool>" ++ (show (bool))) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<bool>True") constaddressMap) of
                                                                                                                                                                    Just address -> ((intLitC, decLitC, strLitC, boolLitC + 1), [(buildQuadrupleThreeAddresses quadNum EQ_ (address, addressCons, (boolLitC)))], quadNum + 1)
expCodeGen symTab constaddressMap idaddressMap (intLitC, decLitC, strLitC, boolLitC) quadNum (ExpressionLitVar (VarIdentifier id)) = case (checkDataTypeOfLitVar (-100000000000000) (VarIdentifier id) symTab) of
                                                                                                                                            Just PrimitiveDouble -> case (Map.lookup id idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<dec>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intLitC, decLitC + 1, strLitC, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decLitC)))], quadNum + 1) 
                                                                                                                                            Just PrimitiveMoney -> case (Map.lookup id idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<dec>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intLitC, decLitC + 1, strLitC, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decLitC)))], quadNum + 1)  
                                                                                                                            
                                                                                                                                            Just PrimitiveInt -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<int>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intLitC + 1, decLitC, strLitC, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intLitC)))], quadNum + 1) 
                                                                                                                                            Just PrimitiveInteger -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<int>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intLitC + 1, decLitC, strLitC, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intLitC)))], quadNum + 1)
                                                                                                                                            Just PrimitiveString -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<str>") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intLitC, decLitC, strLitC + 1, boolLitC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (strLitC)))], quadNum + 1)
                                                                                                                                            Just PrimitiveBool -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<bool>True") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intLitC, decLitC, strLitC, boolLitC + 1), [(buildQuadrupleThreeAddresses quadNum EQ_ (address, addressCons, (boolLitC)))], quadNum + 1)
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionMult exp1 exp2) = genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 MULTIPLY_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionDiv exp1 exp2) = genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 DIVIDE_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionPow exp1 exp2) = genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 POWER_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionPlus exp1 exp2) = genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 ADD_                                                                                                                 
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionMinus exp1 exp2) = genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 SUB_  
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionPars exp1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionGreater exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 GT_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionLower exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 LT_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionGreaterEq exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 GTEQ_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionLowerEq exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 LTEQ_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionEqEq exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 EQ_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionNotEq exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 NOTEQ_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionAnd exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 AND_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionOr exp1 exp2) = genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 OR_
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionMod exp1 exp2) = genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 MOD_  
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionNot exp1) = let ((intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                                                in ((intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate + 1), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NOT_ (boolLitCUpdate, boolLitCUpdate))], quadNum1 + 1)
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionNeg exp1) = let typeExp1 = (expressionProcess (-100000000000000) exp1 symTab (Map.empty))
                                                                                ((intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                                    in case (typeExp1) of
                                                                                                        Just PrimitiveDouble -> ((intLitCUpdate, decLitCUpdate + 1, strLitCUpdate, boolLitCUpdate), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (decLitCUpdate - 1, (decLitCUpdate)))], quadNum1 + 1)
                                                                                                        Just PrimitiveMoney -> ((intLitCUpdate, decLitCUpdate + 1, strLitCUpdate, boolLitCUpdate), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (decLitCUpdate - 1, (decLitCUpdate)))], quadNum1 + 1)
                                                                                                        Just PrimitiveInt ->  ((intLitCUpdate + 1, decLitCUpdate, strLitCUpdate, boolLitCUpdate), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (intLitCUpdate - 1, (intLitCUpdate)))], quadNum1 + 1)
                                                                                                        Just PrimitiveInteger -> ((intLitCUpdate + 1, decLitCUpdate, strLitCUpdate, boolLitCUpdate), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (intLitCUpdate - 1, (intLitCUpdate)))], quadNum1 + 1) 


genQuadrupleArithmetic :: SymbolTable -> ConstantAddressMap -> IdentifierAddressMap -> VariableCounters -> QuadNum -> Expression -> Expression -> Operation -> (VariableCounters,[Quadruple],QuadNum)
genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 op = let typeExp1 = (expressionProcess (-100000000000000) exp1 symTab (Map.empty))
                                                                                in let typeExp2 = (expressionProcess (-100000000000000) exp2 symTab (Map.empty))
                                                                                in let ((intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                in let ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2), quad2, quadNum2) = expCodeGen symTab constMap idMap (intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate) quadNum1 exp2
                                                                                in if (typeExp2 == typeExp1) then
                                                                                    case (typeExp1) of 
                                                                                        Just PrimitiveDouble -> ((intLitCUpdate2, decLitCUpdate2 + 1, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decLitCUpdate - 1, decLitCUpdate2 - 1, (decLitCUpdate2)))], quadNum2 + 1)
                                                                                        Just PrimitiveMoney -> ((intLitCUpdate2, decLitCUpdate2 + 1, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decLitCUpdate - 1, decLitCUpdate2 - 1, (decLitCUpdate2)))], quadNum2 + 1)

                                                                                        Just PrimitiveInt -> ((intLitCUpdate2 + 1, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intLitCUpdate - 1, intLitCUpdate2 - 1, (intLitCUpdate2)))], quadNum2 + 1)
                                                                                        Just PrimitiveInteger -> ((intLitCUpdate2 + 1, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intLitCUpdate - 1, intLitCUpdate2 - 1, (intLitCUpdate2)))], quadNum2 + 1)
                                                                                        Just PrimitiveString -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2 + 1, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (strLitCUpdate - 1, strLitCUpdate2 - 1, (strLitCUpdate2)))], quadNum2 + 1)
                                                                                    -- Aqui hay un else porque nuestro lenguaje permite hacer mezclas de tipos enteros y decimales en operaciones
                                                                                    else 
                                                                                        case (typeExp1) of 
                                                                                        Just PrimitiveDouble -> ((intLitCUpdate2, decLitCUpdate2 + 1, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decLitCUpdate - 1, intLitCUpdate2 - 1, (decLitCUpdate2)))], quadNum2 + 1)
                                                                                        Just PrimitiveMoney -> ((intLitCUpdate2, decLitCUpdate2 + 1, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decLitCUpdate - 1, intLitCUpdate2 - 1, (decLitCUpdate2)))], quadNum2 + 1)
                                                                                        Just PrimitiveInt -> ((intLitCUpdate2, decLitCUpdate2 + 1, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intLitCUpdate - 1, decLitCUpdate2 - 1, (decLitCUpdate2)))], quadNum2 + 1)
                                                                                        Just PrimitiveInteger -> ((intLitCUpdate2, decLitCUpdate2 + 1, strLitCUpdate2, boolLitCUpdate2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intLitCUpdate - 1, decLitCUpdate2 - 1, (decLitCUpdate2)))], quadNum2 + 1)



genQuadrupleRelational :: SymbolTable -> ConstantAddressMap -> IdentifierAddressMap -> VariableCounters -> QuadNum -> Expression -> Expression -> Operation -> (VariableCounters,[Quadruple],QuadNum)
genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 op = let typeExp1 = (expressionProcess (-100000000000000) exp1 symTab (Map.empty))
                                                                                in let typeExp2 = (expressionProcess (-100000000000000) exp2 symTab (Map.empty))
                                                                                in let ((intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                in let ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2), quad2, quadNum2) = expCodeGen symTab constMap idMap (intLitCUpdate, decLitCUpdate, strLitCUpdate, boolLitCUpdate) quadNum1 exp2
                                                                                in if (typeExp2 == typeExp1) then
                                                                                    case (typeExp1) of 
                                                                                        Just PrimitiveDouble -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2 + 1), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decLitCUpdate - 1, decLitCUpdate2 - 1, boolLitCUpdate2))], quadNum2 + 1)
                                                                                        Just PrimitiveMoney -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2 + 1), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decLitCUpdate - 1, decLitCUpdate2 - 1, boolLitCUpdate2))], quadNum2 + 1)
                                                                                        Just PrimitiveInt -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2 + 1), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intLitCUpdate - 1, intLitCUpdate2 - 1, boolLitCUpdate2))], quadNum2 + 1)
                                                                                        Just PrimitiveInteger -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2 + 1), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intLitCUpdate - 1, intLitCUpdate2 - 1, boolLitCUpdate2))], quadNum2 + 1)
                                                                                        Just PrimitiveString -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2 + 1), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (strLitCUpdate - 1, strLitCUpdate2 - 1, boolLitCUpdate2))], quadNum2 + 1)
                                                                                        Just PrimitiveBool -> ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2 + 1), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (boolLitCUpdate - 1, boolLitCUpdate2 - 1, boolLitCUpdate2))], quadNum2 + 1)
                                                                                    else ((intLitCUpdate2, decLitCUpdate2, strLitCUpdate2, boolLitCUpdate2), quad2, quadNum2)