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
expCodeGen symTab constaddressMap idaddressMap (intGC, decGC, strGC, boolGC,objGC) quadNum (ExpressionLitVar (DecimalLiteral dec)) = case (Map.lookup ("<dec>" ++ (show (dec))) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<dec>0") constaddressMap) of
                                                                                                                                                                    Just address -> ((intGC, decGC + 1, strGC, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decGC)))], quadNum + 1)  
expCodeGen symTab constaddressMap idaddressMap (intGC, decGC, strGC, boolGC,objGC) quadNum (ExpressionLitVar (IntegerLiteral int)) = case (Map.lookup ("<int>" ++ (show (int))) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<int>0") constaddressMap) of
                                                                                                                                                                    Just address -> ((intGC + 1, decGC, strGC, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intGC)))], quadNum + 1) 
expCodeGen symTab constaddressMap idaddressMap (intGC, decGC, strGC, boolGC,objGC) quadNum (ExpressionLitVar (StringLiteral str)) = case (Map.lookup ("<str>" ++ (str)) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<str>") constaddressMap) of
                                                                                                                                                                    Just address -> ((intGC, decGC, strGC + 1, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (strGC)))], quadNum + 1) 
expCodeGen symTab constaddressMap idaddressMap (intGC, decGC, strGC, boolGC,objGC) quadNum (ExpressionLitVar (BoolLiteral bool)) = case (Map.lookup ("<bool>" ++ (show (bool))) constaddressMap) of
                                                                                                                                            Just addressCons -> case (Map.lookup ("<bool>True") constaddressMap) of
                                                                                                                                                                    Just address -> ((intGC, decGC, strGC, boolGC + 1,objGC), [(buildQuadrupleThreeAddresses quadNum EQ_ (address, addressCons, (boolGC)))], quadNum + 1)
expCodeGen symTab constaddressMap idaddressMap (intGC, decGC, strGC, boolGC,objGC) quadNum (ExpressionLitVar (VarIdentifier id)) = case (checkDataTypeOfLitVar (-100000000000000) (VarIdentifier id) symTab) of
                                                                                                                                            Just PrimitiveDouble -> case (Map.lookup id idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<dec>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intGC, decGC + 1, strGC, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decGC)))], quadNum + 1) 
                                                                                                                                            Just PrimitiveMoney -> case (Map.lookup id idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<dec>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intGC, decGC + 1, strGC, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decGC)))], quadNum + 1)  
                                                                                                                            
                                                                                                                                            Just PrimitiveInt -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<int>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intGC + 1, decGC, strGC, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intGC)))], quadNum + 1) 
                                                                                                                                            Just PrimitiveInteger -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<int>0") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intGC + 1, decGC, strGC, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intGC)))], quadNum + 1)
                                                                                                                                            Just PrimitiveString -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<str>") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intGC, decGC, strGC + 1, boolGC,objGC), [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (strGC)))], quadNum + 1)
                                                                                                                                            Just PrimitiveBool -> case (Map.lookup (id) idaddressMap) of
                                                                                                                                                                        Just addressCons -> case (Map.lookup ("<bool>True") constaddressMap) of
                                                                                                                                                                                                Just address -> ((intGC, decGC, strGC, boolGC + 1,objGC), [(buildQuadrupleThreeAddresses quadNum EQ_ (address, addressCons, (boolGC)))], quadNum + 1)
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
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionNot exp1) = let ((intGC, decGC, strGC, boolGC,objGC),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                                                in ((intGC, decGC, strGC, boolGC + 1,objGC), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NOT_ (boolGC, boolGC))], quadNum1 + 1)
expCodeGen symTab constMap idMap varCounters quadNum (ExpressionNeg exp1) = let typeExp1 = (expressionProcess (-100000000000000) exp1 symTab (Map.empty))
                                                                                ((intGC, decGC, strGC, boolGC,objGC),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                                    in case (typeExp1) of
                                                                                                        Just PrimitiveDouble -> ((intGC, decGC + 1, strGC, boolGC,objGC), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (decGC - 1, (decGC)))], quadNum1 + 1)
                                                                                                        Just PrimitiveMoney -> ((intGC, decGC + 1, strGC, boolGC,objGC), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (decGC - 1, (decGC)))], quadNum1 + 1)
                                                                                                        Just PrimitiveInt ->  ((intGC + 1, decGC, strGC, boolGC,objGC), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (intGC - 1, (intGC)))], quadNum1 + 1)
                                                                                                        Just PrimitiveInteger -> ((intGC + 1, decGC, strGC, boolGC,objGC), quad1 ++ [(buildQuadrupleTwoAddresses quadNum1 NEG_ (intGC - 1, (intGC)))], quadNum1 + 1) 
expCodeGen symTab constTable idTable varCounters quadNum (ExpressionVarArray identifier ((ArrayAccessExpression exp1) : [])) = 
                                                                                                let ((intGC, decGC, strGC, boolGC,objGC),quadsExp, quadNum1) = expCodeGen symTab constTable idTable varCounters quadNum exp1 
                                                                                                in case (Map.lookup identifier symTab) of
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveString (("[",size,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                            Just address ->
                                                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                                    Just addressBase -> 
                                                                                                                                        let boundQuad = ([(buildQuadrupleTwoAddresses quadNum1 BOUNDS ((getLastAddress $ last $ quadsExp), address ))])
                                                                                                                                            in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum1 + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsExp), intGC))]
                                                                                                                                            in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum1 + 2) ACCESS_INDEX (intGC,strGC))])
                                                                                                                                            in ((intGC + 1,decGC,strGC + 1,boolGC,objGC), (quadsExp ++ boundQuad ++ baseAddQuad ++ quadAssignment), (quadNum1 + 3))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveMoney (("[",size,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                            Just address ->
                                                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                                    Just addressBase -> 
                                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum1 BOUNDS ((getLastAddress $ last $ quadsExp), address ))])
                                                                                                                                                    in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum1 + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsExp), intGC))]
                                                                                                                                                    in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum1 + 2) ACCESS_INDEX (intGC,decGC))])
                                                                                                                                                    in ((intGC + 1,decGC + 1,strGC,boolGC,objGC), (quadsExp ++ boundQuad ++ baseAddQuad ++ quadAssignment), (quadNum1 + 3))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveDouble (("[",size,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                            Just address ->
                                                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                                    Just addressBase -> 
                                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum1 BOUNDS ((getLastAddress $ last $ quadsExp), address ))])
                                                                                                                                                    in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum1 + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsExp), intGC))]
                                                                                                                                                    in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum1 + 2) ACCESS_INDEX (intGC,decGC))])
                                                                                                                                                    in ((intGC + 1,decGC + 1,strGC,boolGC,objGC), (quadsExp ++ boundQuad ++ baseAddQuad ++ quadAssignment), (quadNum1 + 3))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveInteger (("[",size,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                            Just address ->
                                                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                                    Just addressBase -> 
                                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum1 BOUNDS ((getLastAddress $ last $ quadsExp), address ))])
                                                                                                                                                    in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum1 + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsExp), intGC))]
                                                                                                                                                    in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum1 + 2) ACCESS_INDEX (intGC,intGC + 1))])
                                                                                                                                                    in ((intGC + 2,decGC,strGC,boolGC,objGC), (quadsExp ++ boundQuad ++ baseAddQuad ++ quadAssignment), (quadNum1 + 3))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveInt (("[",size,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                            Just address ->
                                                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                                    Just addressBase -> 
                                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum1 BOUNDS ((getLastAddress $ last $ quadsExp), address ))])
                                                                                                                                                    in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum1 + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsExp), intGC))]
                                                                                                                                                    in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum1 + 2) ACCESS_INDEX (intGC,intGC + 1))])
                                                                                                                                                    in ((intGC + 2,decGC,strGC,boolGC,objGC), (quadsExp ++ boundQuad ++ baseAddQuad ++ quadAssignment), (quadNum1 + 3))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveBool (("[",size,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                            Just address ->
                                                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                                    Just addressBase -> 
                                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum1 BOUNDS ((getLastAddress $ last $ quadsExp), address ))])
                                                                                                                                                    in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum1 + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsExp), intGC))]
                                                                                                                                                    in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum1 + 2) ACCESS_INDEX (intGC,boolGC))])
                                                                                                                                                    in ((intGC + 1,decGC,strGC,boolGC + 1,objGC), (quadsExp ++ boundQuad ++ baseAddQuad ++ quadAssignment), (quadNum1 + 3))
expCodeGen symTab constTable idTable varCounters quadNum (ExpressionVarArray identifier ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp) : [])) = 
                                                                                                let (varCounters1,quadsRowExp, quadNum1) = expCodeGen symTab constTable idTable varCounters quadNum rowsIndexExp 
                                                                                                in let ((intGC, decGC, strGC, boolGC,objGC),quadsColExp, quadNum2) = expCodeGen symTab constTable idTable varCounters1 quadNum1 colsIndexExp 
                                                                                                in case (Map.lookup identifier symTab) of
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveString (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum2 BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        in let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum2 + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        in let positionRow = [(buildQuadrupleThreeAddresses (quadNum2 + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        in let positionCol = [(buildQuadrupleThreeAddresses (quadNum2 + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum2 + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum2 + 5) ACCESS_INDEX (intGC + 2,strGC))])
                                                                                                                                        in ((intGC + 3,decGC,strGC + 1,boolGC,objGC), (quadsRowExp ++ quadsColExp ++ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment), (quadNum2 + 6))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveMoney (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum2 BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        in let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum2 + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        in let positionRow = [(buildQuadrupleThreeAddresses (quadNum2 + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        in let positionCol = [(buildQuadrupleThreeAddresses (quadNum2 + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum2 + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum2 + 5) ACCESS_INDEX (intGC + 2,decGC))])
                                                                                                                                        in ((intGC + 3,decGC + 1,strGC,boolGC,objGC), (quadsRowExp ++ quadsColExp ++ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment), (quadNum2 + 6))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveDouble (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum2 BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        in let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum2 + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        in let positionRow = [(buildQuadrupleThreeAddresses (quadNum2 + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        in let positionCol = [(buildQuadrupleThreeAddresses (quadNum2 + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum2 + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum2 + 5) ACCESS_INDEX (intGC + 2,decGC))])
                                                                                                                                        in ((intGC + 3,decGC + 1,strGC,boolGC,objGC), (quadsRowExp ++ quadsColExp ++ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment), (quadNum2 + 6))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveInteger (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum2 BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        in let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum2 + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        in let positionRow = [(buildQuadrupleThreeAddresses (quadNum2 + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        in let positionCol = [(buildQuadrupleThreeAddresses (quadNum2 + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum2 + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum2 + 5) ACCESS_INDEX (intGC + 2,intGC + 3))])
                                                                                                                                        in ((intGC + 4,decGC,strGC,boolGC,objGC), (quadsRowExp ++ quadsColExp ++ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment), (quadNum2 + 6))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveInt (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum2 BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        in let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum2 + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        in let positionRow = [(buildQuadrupleThreeAddresses (quadNum2 + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        in let positionCol = [(buildQuadrupleThreeAddresses (quadNum2 + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum2 + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum2 + 5) ACCESS_INDEX (intGC + 2,intGC + 3))])
                                                                                                                                        in ((intGC + 4,decGC,strGC,boolGC,objGC), (quadsRowExp ++ quadsColExp ++ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment), (quadNum2 + 6))
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveBool (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum2 BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        in let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum2 + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        in let positionRow = [(buildQuadrupleThreeAddresses (quadNum2 + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        in let positionCol = [(buildQuadrupleThreeAddresses (quadNum2 + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        in let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum2 + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        in let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum2 + 5) ACCESS_INDEX (intGC + 2,boolGC))])
                                                                                                                                        in ((intGC + 3,decGC,strGC,boolGC + 1,objGC), (quadsRowExp ++ quadsColExp ++ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment), (quadNum2 + 6))

expCodeGen symTab constMap idMap varCounters quadNum (ExpressionVarArray identifier ((ArrayAccessExpression exp1) : (ArrayAccessExpression exp2) :[])) = let    ((intGC, decGC, strGC, boolGC,objGC),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                                                                                                ((intGC2, decGC2, strGC2, boolGC2,objGC2),quad2, quadNum2) = expCodeGen symTab constMap idMap (intGC, decGC, strGC, boolGC,objGC) quadNum1 exp2
                                                                                                                                                                in ((intGC2, decGC2, strGC2, boolGC2,objGC2),quad1 ++ quad2, quadNum2)

genQuadrupleArithmetic :: SymbolTable -> ConstantAddressMap -> IdentifierAddressMap -> VariableCounters -> QuadNum -> Expression -> Expression -> Operation -> (VariableCounters,[Quadruple],QuadNum)
genQuadrupleArithmetic symTab constMap idMap varCounters quadNum exp1 exp2 op = let typeExp1 = (expressionProcess (-100000000000000) exp1 symTab (Map.empty))
                                                                                in let typeExp2 = (expressionProcess (-100000000000000) exp2 symTab (Map.empty))
                                                                                in let ((intGC, decGC, strGC, boolGC,objGC),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                in let ((intGC2, decGC2, strGC2, boolGC2,objGC2), quad2, quadNum2) = expCodeGen symTab constMap idMap (intGC, decGC, strGC, boolGC,objGC) quadNum1 exp2
                                                                                in if (typeExp2 == typeExp1) then
                                                                                    case (typeExp1) of 
                                                                                        Just PrimitiveDouble -> ((intGC2, decGC2 + 1, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, (decGC2)))], quadNum2 + 1)
                                                                                        Just PrimitiveMoney -> ((intGC2, decGC2 + 1, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, (decGC2)))], quadNum2 + 1)

                                                                                        Just PrimitiveInt -> ((intGC2 + 1, decGC2, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, (intGC2)))], quadNum2 + 1)
                                                                                        Just PrimitiveInteger -> ((intGC2 + 1, decGC2, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, (intGC2)))], quadNum2 + 1)
                                                                                        Just PrimitiveString -> ((intGC2, decGC2, strGC2 + 1, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (strGC - 1, strGC2 - 1, (strGC2)))], quadNum2 + 1)
                                                                                    -- Aqui hay un else porque nuestro lenguaje permite hacer mezclas de tipos enteros y decimales en operaciones
                                                                                    else 
                                                                                        case (typeExp1) of 
                                                                                        Just PrimitiveDouble -> ((intGC2, decGC2 + 1, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, intGC2 - 1, (decGC2)))], quadNum2 + 1)
                                                                                        Just PrimitiveMoney -> ((intGC2, decGC2 + 1, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, intGC2 - 1, (decGC2)))], quadNum2 + 1)
                                                                                        Just PrimitiveInt -> ((intGC2, decGC2 + 1, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, decGC2 - 1, (decGC2)))], quadNum2 + 1)
                                                                                        Just PrimitiveInteger -> ((intGC2, decGC2 + 1, strGC2, boolGC2,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, decGC2 - 1, (decGC2)))], quadNum2 + 1)



genQuadrupleRelational :: SymbolTable -> ConstantAddressMap -> IdentifierAddressMap -> VariableCounters -> QuadNum -> Expression -> Expression -> Operation -> (VariableCounters,[Quadruple],QuadNum)
genQuadrupleRelational symTab constMap idMap varCounters quadNum exp1 exp2 op = let typeExp1 = (expressionProcess (-100000000000000) exp1 symTab (Map.empty))
                                                                                in let typeExp2 = (expressionProcess (-100000000000000) exp2 symTab (Map.empty))
                                                                                in let ((intGC, decGC, strGC, boolGC,objGC),quad1, quadNum1) = expCodeGen symTab constMap idMap varCounters quadNum exp1
                                                                                in let ((intGC2, decGC2, strGC2, boolGC2,objGC2), quad2, quadNum2) = expCodeGen symTab constMap idMap (intGC, decGC, strGC, boolGC,objGC) quadNum1 exp2
                                                                                in if (typeExp2 == typeExp1) then
                                                                                    case (typeExp1) of 
                                                                                        Just PrimitiveDouble -> ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, boolGC2))], quadNum2 + 1)
                                                                                        Just PrimitiveMoney -> ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, boolGC2))], quadNum2 + 1)
                                                                                        Just PrimitiveInt -> ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, boolGC2))], quadNum2 + 1)
                                                                                        Just PrimitiveInteger -> ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, boolGC2))], quadNum2 + 1)
                                                                                        Just PrimitiveString -> ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (strGC - 1, strGC2 - 1, boolGC2))], quadNum2 + 1)
                                                                                        Just PrimitiveBool -> ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (boolGC - 1, boolGC2 - 1, boolGC2))], quadNum2 + 1)
                                                                                    else ((intGC2, decGC2, strGC2, boolGC2,objGC2), quad2, quadNum2)

-- generate1DAccessIndex :: QuadNum -> Expression -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum)
-- generate1DAccessIndex quadNum (ExpressionLitVar (VarIdentifier id)) symTab classSymTab varCounters idMap constTable objMap = 
--                 case (Map.lookup id symTab) of 
--                     Just (SymbolVar (TypePrimitive _ []) _ _) ->
--                         let (varCounters2,quadsExp,)










