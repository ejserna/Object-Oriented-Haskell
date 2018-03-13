module CodeGen where 
import Data.Decimal
import DataTypes
import Quadruple
import SymbolTable
import ClassSymbolTable
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map

type TypeIdentifier = String -- Integer, Decimal, String, Bool

-- Estos tipos le sirven a ExpressionCodeGen saber qué Identifiador/Constante están mappeados en memorias con qué dirección
type IdentifierAddressMap = Map.HashMap Identifier Address
type ConstantAddressMap = Map.HashMap String Address

startIntGlobalMemory :: Address
startIntGlobalMemory = 1

endIntGlobalMemory :: Address
endIntGlobalMemory = 4000

startDecimalGlobalMemory :: Address
startDecimalGlobalMemory = 4001

endDecimalGlobalMemory :: Address
endDecimalGlobalMemory = 8000

startStringGlobalMemory :: Address
startStringGlobalMemory = 8001

endStringGlobalMemory :: Address
endStringGlobalMemory = 12000

startBoolGlobalMemory :: Address
startBoolGlobalMemory = 12001

endBoolGlobalMemory :: Address
endBoolGlobalMemory = 16000

startIntLocalMemory :: Address
startIntLocalMemory = 16001

endIntLocalMemory :: Address
endIntLocalMemory = 20000

startDecimalLocalMemory :: Address
startDecimalLocalMemory = 20001

endDecimaLocalMemory :: Address
endDecimaLocalMemory = 24000

startStringLocalMemory :: Address
startStringLocalMemory = 24001

endStringLocalMemory :: Address
endStringLocalMemory = 26000

startBoolLocalMemory :: Address
startBoolLocalMemory = 26001

endBoolLocalMemory :: Address
endBoolLocalMemory = 30000

startIntLiteralMemory :: Address
startIntLiteralMemory = 64001

endIntLiteralMemory :: Address
endIntLiteralMemory = 68000

startDecimalLiteralMemory :: Address
startDecimalLiteralMemory = 68001

endDecimalLiteralMemory :: Address
endDecimalLiteralMemory = 72000

startStringLiteralMemory :: Address
startStringLiteralMemory = 76001

endStringLiteralMemory :: Address
endStringLiteralMemory = 80000

startBoolLiteralMemory :: Address
startBoolLiteralMemory = 80001

endBoolLiteralMemory :: Address
endBoolLiteralMemory = 84000

-- Los primeros 4 son los contadores globales de Integers,Decimales,Strings,Bool y los ultimos son locales, en el mismo orden
type VariableCounters = (Address,Address,Address,Address,Address,Address,Address,Address) 

type LiteralCounters = (Address,Address,Address,Address) 

startCodeGen :: Program -> SymbolTable -> ClassSymbolTable -> IO()
startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab = 
            do putStrLn $ ppShow $ (prepareAddressMapsFromSymbolTable symTab (startIntGlobalMemory,startDecimalGlobalMemory,startStringGlobalMemory,startBoolGlobalMemory,
                                                                startIntLocalMemory,startDecimalLocalMemory,startStringLocalMemory,startBoolLocalMemory)
                                                                (Map.empty))
               putStrLn $ ppShow $ (prepareConstantAddressMap statements (startIntLiteralMemory,startDecimalLiteralMemory,startStringLiteralMemory,startBoolLiteralMemory)
                                                                (Map.empty))

prepareConstantAddressMap :: [Statement] -> LiteralCounters -> ConstantAddressMap -> (LiteralCounters, ConstantAddressMap)
prepareConstantAddressMap [] literalCounters constantAddressMap = (literalCounters,constantAddressMap)
prepareConstantAddressMap (st : sts) literalCounters constantAddressMap = 
            let (newLiteralCounters, newConsAddressMap) = fillFromStatement st literalCounters constantAddressMap
            in let (newLiteralCounters2, newConsAddressMap2) = (prepareConstantAddressMap sts newLiteralCounters newConsAddressMap)
                in (newLiteralCounters2, newConsAddressMap2)

fillFromAssignment :: Assignment -> LiteralCounters -> ConstantAddressMap -> (LiteralCounters, ConstantAddressMap)
fillFromAssignment (AssignmentExpression identifier expression) literalCounters constantAddressMap = fillFromExpression literalCounters constantAddressMap expression
fillFromAssignment  (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) literalCounters constantAddressMap =  fillFromExpression literalCounters constantAddressMap expression

fillFromAssignment  (AssignmentArrayExpression _ ((ArrayAccessExpression innerExp) : []) expression) literalCounters constantAddressMap =  -- MARK TODO: Llenar literalCounter de expresiones
                                                                                        let (newLiteralCounters,newConsAddressMap) = fillFromExpression literalCounters constantAddressMap innerExp
                                                                                        in fillFromExpression newLiteralCounters newConsAddressMap expression
fillFromAssignment  (AssignmentArrayExpression _ ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) expression) literalCounters constantAddressMap =  -- MARK TODO: Llenar literalCounter de expresiones
                                                                                                                (literalCounters,constantAddressMap)
fillFromAssignment _ literalCounters constantAddressMap  = (literalCounters,constantAddressMap)

fillFromStatement :: Statement -> LiteralCounters -> ConstantAddressMap -> (LiteralCounters, ConstantAddressMap)
fillFromStatement (AssignStatement assignment) literalCounters constantAddressMap = fillFromAssignment assignment literalCounters constantAddressMap
fillFromStatement (ConditionStatement (If expression (Block statements))) literalCounters constantAddressMap = 
                                                let (newLiteralCounters, newConsAddressMap) = fillFromExpression literalCounters constantAddressMap expression
                                                    in let (newLiteralCounters2, newConsAddressMap2) = prepareConstantAddressMap statements newLiteralCounters newConsAddressMap
                                                        in (newLiteralCounters2, newConsAddressMap2)  
fillFromStatement (ConditionStatement (IfElse expression (Block statements) (Block statements2)))literalCounters constantAddressMap = 
                                                let (newLiteralCounters, newConsAddressMap) = fillFromExpression literalCounters constantAddressMap expression
                                                    in let (newLiteralCounters2, newConsAddressMap2) = prepareConstantAddressMap statements newLiteralCounters newConsAddressMap
                                                        in prepareConstantAddressMap statements2 newLiteralCounters2 newConsAddressMap2  
fillFromStatement (CycleStatement (CycleWhile (While expression (Block statements)))) literalCounters constantAddressMap = 
                let (newLiteralCounters, newConsAddressMap) = fillFromExpression literalCounters constantAddressMap expression
                    in let (newLiteralCounters2, newConsAddressMap2) = prepareConstantAddressMap statements newLiteralCounters newConsAddressMap
                        in (newLiteralCounters2, newConsAddressMap2)
fillFromStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) (intLitC,decLitC, strLitC, boolLitC) constantAddressMap =
                                                             case (Map.lookup ("<int>" ++ (show lowerRange)) constantAddressMap) of
                                                                Just _ -> 
                                                                        case (Map.lookup ("<int>" ++ (show greaterRange)) constantAddressMap) of
                                                                            Just _ -> ((intLitC, decLitC, strLitC, boolLitC), constantAddressMap)
                                                                            _ -> let newConsAddressMap2 = (Map.insert ("<int>" ++ (show greaterRange)) intLitC constantAddressMap)
                                                                                    in ( prepareConstantAddressMap statements (intLitC + 1, decLitC, strLitC, boolLitC) newConsAddressMap2 )
                                                                _ -> let newConsAddressMap = (Map.insert ("<int>" ++ (show lowerRange)) intLitC constantAddressMap)
                                                                        in case (Map.lookup ("<int>" ++ (show greaterRange)) constantAddressMap) of
                                                                            Just _ -> ((intLitC + 1, decLitC, strLitC, boolLitC), newConsAddressMap)
                                                                            _ -> let newConsAddressMap2 = (Map.insert ("<int>" ++ (show greaterRange)) (intLitC + 1) newConsAddressMap)
                                                                                    in ( prepareConstantAddressMap statements (intLitC + 2, decLitC, strLitC, boolLitC) newConsAddressMap2 )
fillFromStatement (CycleStatement (CycleForVar statements)) literalCounters constantAddressMap = prepareConstantAddressMap statements literalCounters constantAddressMap
fillFromStatement (DPMStatement assignment) literalCounters constantAddressMap = fillFromStatement (AssignStatement assignment) literalCounters constantAddressMap
fillFromStatement (FunctionCallStatement functionCall) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
fillFromStatement (ReturnStatement (ReturnExp expression)) literalCounters constantAddressMap = fillFromExpression literalCounters constantAddressMap expression
fillFromStatement (ReturnStatement (ReturnFunctionCall functionCall)) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
fillFromStatement (DisplayStatement displays) literalCounters constantAddressMap = fillFromDisplays displays literalCounters constantAddressMap
                                                                where 
                                                                    fillFromDisplays :: [Display] -> LiteralCounters -> ConstantAddressMap -> (LiteralCounters,ConstantAddressMap)
                                                                    fillFromDisplays [] literalCounters constantAddressMap = (literalCounters,constantAddressMap)
                                                                    fillFromDisplays (disp : disps) literalCounters constantAddressMap = 
                                                                            let (newLiteralCounters2,newConsAddressMap) = fillFromDisplay disp literalCounters constantAddressMap
                                                                            in fillFromDisplays disps newLiteralCounters2 newConsAddressMap

                                                                    fillFromDisplay :: Display -> LiteralCounters -> ConstantAddressMap -> (LiteralCounters,ConstantAddressMap)
                                                                    fillFromDisplay (DisplayLiteralOrVariable litOrVar) literalCounters constantAddressMap =
                                                                        fillFromExpression literalCounters constantAddressMap (ExpressionLitVar litOrVar) 
                                                                    fillFromDisplay (DisplayFunctionCall funcCall) literalCounters constantAddressMap =
                                                                        fillFromExpression literalCounters constantAddressMap (ExpressionFuncCall funcCall)
                                                                    fillFromDisplay (DisplayVarArrayAccess identifier arrayAccess) literalCounters constantAddressMap =
                                                                        fillFromExpression literalCounters constantAddressMap (ExpressionVarArray identifier arrayAccess)  
                                                                    fillFromDisplay _ literalCounters constantAddressMap =
                                                                        (literalCounters,constantAddressMap)  
fillFromStatement _ literalCounters constantAddressMap = (literalCounters,constantAddressMap)


fillFromFunctionCall :: FunctionCall -> LiteralCounters  -> ConstantAddressMap -> (LiteralCounters,ConstantAddressMap)
fillFromFunctionCall (FunctionCallVar _ callParams) literalCounters constantAddressMap = 
                            fillFromCallParams literalCounters constantAddressMap callParams
fillFromFunctionCall (FunctionCallObjMem (ObjectMember _ _) callParams) literalCounters constantAddressMap = 
                            fillFromCallParams literalCounters constantAddressMap callParams


fillFromCallParams :: LiteralCounters -> ConstantAddressMap -> [Params] -> (LiteralCounters,ConstantAddressMap)
fillFromCallParams literalCounters constantAddressMap [] = (literalCounters,constantAddressMap)
fillFromCallParams literalCounters constantAddressMap ((ParamsExpression exp) : params) =
        let (newLiteralCounters,newConsAddressMap) = fillFromExpression literalCounters constantAddressMap exp
            in fillFromCallParams newLiteralCounters constantAddressMap params
                


-- analyzeStatement (ReadStatement (Reading identifier)) scp symTab classTab = 
--                                     case (Map.lookup identifier symTab) of
--                                         Just (SymbolVar (TypePrimitive _ []) varScp _) ->
--                                             if varScp >= scp then
--                                                 (symTab,False)
--                                             else (emptySymbolTable, True)
--                                         _ -> (emptySymbolTable, True)


-- analyzeStatement (VariableStatement var) scp symTab classTab = analyzeVariable var scp Nothing symTab classTab







prepareAddressMapsFromSymbolTable :: SymbolTable -> VariableCounters -> IdentifierAddressMap -> IdentifierAddressMap
prepareAddressMapsFromSymbolTable symTab counters identifierAddressMap = 
                            let symTabList = (Map.toList symTab)
                            in fillIdentifierAddressMap symTabList identifierAddressMap counters  

fillIdentifierAddressMap :: [(Identifier,Symbol)] -> IdentifierAddressMap -> VariableCounters -> IdentifierAddressMap 
fillIdentifierAddressMap [] identifierAddressMap _ = identifierAddressMap
fillIdentifierAddressMap ( (identifier,(SymbolVar (TypePrimitive prim _) _ _)) : rest ) identifierAddressMap
                                                                    (intGC,decGC,strGC,boolGC,intLC,decLC,strLC,boolLC)  |
                                                                    intGC <= endIntGlobalMemory
                                                                    && decGC <= endDecimalGlobalMemory
                                                                    && strGC <= endStringGlobalMemory 
                                                                    && boolGC <= endBoolGlobalMemory =
                            case prim of
                                PrimitiveBool -> (Map.union (Map.insert identifier boolGC identifierAddressMap)
                                                            (fillIdentifierAddressMap rest identifierAddressMap
                                                            (intGC,decGC,strGC,boolGC + 1,intLC,decLC,strLC,boolLC)))
                                PrimitiveInt -> (Map.union (Map.insert identifier intGC identifierAddressMap)
                                                            (fillIdentifierAddressMap rest identifierAddressMap
                                                            (intGC + 1,decGC,strGC,boolGC,intLC,decLC,strLC,boolLC)))
                                PrimitiveInteger -> (Map.union (Map.insert identifier intGC identifierAddressMap)
                                                            (fillIdentifierAddressMap rest identifierAddressMap
                                                            (intGC + 1,decGC,strGC,boolGC,intLC,decLC,strLC,boolLC)))
                                PrimitiveString -> (Map.union (Map.insert identifier strGC identifierAddressMap)
                                                            (fillIdentifierAddressMap rest identifierAddressMap
                                                            (intGC,decGC,strGC + 1,boolGC,intLC,decLC,strLC,boolLC)))
                                PrimitiveMoney -> (Map.union (Map.insert identifier decGC identifierAddressMap)
                                                            (fillIdentifierAddressMap rest identifierAddressMap
                                                            (intGC,decGC + 1,strGC,boolGC,intLC,decLC,strLC,boolLC)))
                                PrimitiveDouble -> (Map.union (Map.insert identifier decGC identifierAddressMap)
                                                            (fillIdentifierAddressMap rest identifierAddressMap
                                                            (intGC,decGC + 1,strGC,boolGC,intLC,decLC,strLC,boolLC)))
-- MARK TODO: Clases, funciones
fillIdentifierAddressMap (x : xs) identifierAddressMap (intGC,decGC,strGC,boolGC,intLC,decLC,strLC,boolLC) = 
            (fillIdentifierAddressMap xs identifierAddressMap
                                                            (intGC,decGC,strGC,boolGC,intLC,decLC,strLC,boolLC))
                            


fillFromExpression :: LiteralCounters -> ConstantAddressMap -> Expression -> (LiteralCounters,ConstantAddressMap)
fillFromExpression literalCounters constantAddressMap (ExpressionMult exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionPlus exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionPow exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionDiv exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionMinus exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionPars exp) = fillFromExpression literalCounters constantAddressMap exp
fillFromExpression literalCounters constantAddressMap (ExpressionNeg exp) = fillFromExpression literalCounters constantAddressMap exp
fillFromExpression literalCounters constantAddressMap (ExpressionMod exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionGreater exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionLower exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionGreaterEq exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionLowerEq exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionEqEq exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionNotEq exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionAnd exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionOr exp1 exp2) = fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2
fillFromExpression literalCounters constantAddressMap (ExpressionNot exp) = fillFromExpression literalCounters constantAddressMap exp
fillFromExpression (intLitC,decLitC, strLitC, boolLitC) constantAddressMap (ExpressionLitVar litOrVar)
                    | intLitC <= endIntLiteralMemory
                     && decLitC <= endDecimalLiteralMemory
                     && strLitC <= endStringLiteralMemory
                     && boolLitC <= endBoolLiteralMemory = 
                    case litOrVar of
                         IntegerLiteral int -> case (Map.lookup ("<int>" ++ (show int)) constantAddressMap) of
                                                    Just _ -> ((intLitC, decLitC, strLitC, boolLitC), constantAddressMap)
                                                    _ ->  let newConsAddressMap = (Map.insert ("<int>" ++ (show int)) intLitC constantAddressMap)
                                                                in ((intLitC + 1, decLitC, strLitC, boolLitC), newConsAddressMap)
                         DecimalLiteral dec -> case (Map.lookup ("<dec>" ++ (show dec)) constantAddressMap) of
                                                    Just _ -> ((intLitC, decLitC, strLitC, boolLitC), constantAddressMap)
                                                    _ -> let newConsAddressMap = (Map.insert ("<dec>" ++ (show dec)) decLitC constantAddressMap)
                                                            in ((intLitC, decLitC + 1, strLitC, boolLitC), newConsAddressMap)
                         StringLiteral str -> case (Map.lookup ("<str>" ++ (show str)) constantAddressMap) of
                                                    Just _ -> ((intLitC, decLitC, strLitC, boolLitC), constantAddressMap)
                                                    _ -> let newConsAddressMap = (Map.insert ("<str>" ++ str) strLitC constantAddressMap)
                                                            in ((intLitC, decLitC, strLitC + 1, boolLitC), newConsAddressMap)
                         BoolLiteral bool -> case (Map.lookup ("<bool>" ++ (show bool)) constantAddressMap) of
                                                    Just _ -> ((intLitC, decLitC, strLitC, boolLitC), constantAddressMap)
                                                    _ -> let newConsAddressMap = (Map.insert ("<bool>" ++ (show bool)) boolLitC constantAddressMap)
                                                            in ((intLitC, decLitC, strLitC, boolLitC + 1), newConsAddressMap)
                         _ -> ((intLitC,decLitC, strLitC, boolLitC), constantAddressMap)
fillFromExpression literalCounters constantAddressMap (ExpressionVarArray _ ((ArrayAccessExpression expression) : []))  = 
                            fillFromExpression literalCounters constantAddressMap expression
fillFromExpression literalCounters constantAddressMap (ExpressionVarArray _ ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[])) =
                            fillFromTwoExpressions literalCounters constantAddressMap expression1 expression2
fillFromExpression literalCounters constantAddressMap (ExpressionFuncCall funcCall) =
                            fillFromFunctionCall funcCall literalCounters constantAddressMap

--Mark TODO: CallParams from function
fillFromTwoExpressions :: LiteralCounters -> ConstantAddressMap -> Expression -> Expression -> (LiteralCounters,ConstantAddressMap)
fillFromTwoExpressions literalCounters constantAddressMap exp1 exp2 = let (newLiteralCounters1,constAddressMap1) =
                                                                            fillFromExpression literalCounters constantAddressMap exp1
                                                                      in let (newLiteralCounters2,constAddressMap2) = fillFromExpression newLiteralCounters1 constAddressMap1 exp2
                                                                            in (newLiteralCounters2,(Map.union constAddressMap1 constAddressMap2))

-- analyzeStatements :: [Statement] -> IdentifierAddressMap -> ConstantAddressMap -> [Quadruple]
-- analyzeStatements [] _ symTab _ = []
-- analyzeStatements (st : sts) scp symTab classTab = 
--                                 let (newSymTab, hasErrors) = analyzeStatement st scp symTab classTab
--                                 in if (hasErrors) then (emptySymbolTable, True)
--                                     else let (newSymTab2,hasErrors2) = analyzeStatements sts scp newSymTab classTab
--                                          in if (hasErrors2) then (emptySymbolTable,True)
--                                             else ((Map.union newSymTab newSymTab2), False)

-- analyzeStatement :: Statement -> Scope -> SymbolTable -> ClassSymbolTable -> (SymbolTable, Bool)
-- analyzeStatement (AssignStatement assignment) scp symTab classTab = if (isAssignmentOk assignment scp symTab classTab)
--                                                                         then (symTab, False)
--                                                                         else (emptySymbolTable, True)
-- analyzeStatement (DisplayStatement displays) scp symTab classTab = if analyzeDisplays displays 
--                                                                     then (symTab, False)
--                                                                     else (emptySymbolTable, True)
--                                                                 where 
--                                                                     analyzeDisplays :: [Display] -> Bool
--                                                                     analyzeDisplays [] = True
--                                                                     analyzeDisplays (disp : disps) = 
--                                                                             analyzeDisplay disp scp symTab classTab
--                                                                             && analyzeDisplays disps

-- analyzeStatement (ReadStatement (Reading identifier)) scp symTab classTab = 
--                                     case (Map.lookup identifier symTab) of
--                                         Just (SymbolVar (TypePrimitive _ []) varScp _) ->
--                                             if varScp >= scp then
--                                                 (symTab,False)
--                                             else (emptySymbolTable, True)
--                                         _ -> (emptySymbolTable, True)

-- analyzeStatement (DPMStatement assignment) scp symTab classTab = analyzeStatement (AssignStatement assignment) scp symTab classTab
-- analyzeStatement (FunctionCallStatement functionCall) scp symTab classTab = if (analyzeFunctionCall functionCall scp symTab classTab) 
--                                                                                 then (symTab, False)
--                                                                                 else (emptySymbolTable, True) 
-- analyzeStatement (VariableStatement var) scp symTab classTab = analyzeVariable var scp Nothing symTab classTab
-- analyzeStatement (ConditionStatement (If expression (Block statements))) scp symTab classTab = 
--                                                 case (expressionProcess scp expression symTab classTab) of
--                                                     -- Si la expresión del if regresa booleano, entonces está bien
--                                                     Just (PrimitiveBool) -> analyzeStatements statements (scp - 1) symTab classTab
--                                                    -- De lo contrario, no se puede tener esa expresión en el if
--                                                     _ -> (emptySymbolTable, True)  
-- analyzeStatement (CycleStatement (CycleWhile (While expression (Block statements)))) scp symTab classTab = 
--                 case (expressionProcess scp expression symTab classTab) of
--                     Just PrimitiveBool -> analyzeStatements statements (scp - 1) symTab classTab
--                     _ -> (emptySymbolTable, True) 
-- analyzeStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) scp symTab classTab = 
--                                                                             if (greaterRange > lowerRange) 
--                                                                                 then analyzeStatements statements (scp - 1) symTab classTab
--                                                                                 else (emptySymbolTable,True)
-- analyzeStatement (CycleStatement (CycleForVar statements)) scp symTab classTab = analyzeStatements statements scp symTab classTab
-- analyzeStatement (ReturnStatement (ReturnFunctionCall functionCall)) scp symTab classTab =  
--             let isFuncCallOk = analyzeFunctionCall functionCall scp symTab classTab
--             in if (isFuncCallOk) then (symTab,False)
--                 else (emptySymbolTable, True)
-- analyzeStatement (ReturnStatement (ReturnExp expression)) scp symTab classTab =  
--             case (preProcessExpression scp expression symTab classTab) of
--                 Just expType -> (symTab,False)
--                 Nothing -> (emptySymbolTable, True)
