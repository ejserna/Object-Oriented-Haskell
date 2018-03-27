module CodeGen where 
import DataTypes
import Quadruple
import SymbolTable
import ClassSymbolTable
import ExpressionCodeGen
import ExpressionOptimizer
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import VirtualMachine
import Data.Decimal
import Data.List(isInfixOf)
import Data.List (sortBy,sort,intercalate)
import Data.Ord (comparing)
import Data.Function (on)
import  System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)

startCodeGen :: Program -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> IO()
startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab varCounters idTable constTable objMap =
            let ((int,dec,str,bool,obj),quads,_,newObjMap) =  generateCodeFromStatements statements 0 symTab classSymTab varCounters idTable constTable objMap
                in 
            do  
                mapM_ (putStrLn.show) quads
                putStrLn $ (style Bold $ "# Ints: ") ++ (color Magenta $ show $ int)
                putStrLn $ (style Bold $ "# Decimals: ") ++ (color Magenta $ show $ (dec - endIntGlobalMemory))
                putStrLn $ (style Bold $ "# Strings: ") ++ (color Magenta $ show $ (str - endDecimalGlobalMemory))
                putStrLn $ (style Bold $ "# Booleans: ") ++ (color Magenta $ show $ (bool - endStringGlobalMemory))
                putStrLn $ (style Bold $ "# Objects: ") ++ (color Magenta $ show $ (obj - endObjectLocalMemory))
                -- mapM_ (putStrLn.ppShow) $ intercalate " , " [(color White . show $ (int - endIntGlobalMemory)), (color White . show $ (dec - endDecimalGlobalMemory)), (color White . show $ (str - endStringGlobalMemory)), (color White . show $ (obj - endObjectGlobalMemory))]
                let (objMem,memoryFromAttributes) = prepareMemoryFromObjects (Map.toList objMap) Map.empty Map.empty
                -- putStrLn $ ppShow $ (sortBy (compare `on` snd) (Map.toList idTable) )
                -- putStrLn $ ppShow $ (sortBy (compare `on` fst) (Map.toList objMem) )
                startVM quads (Map.union  memoryFromAttributes (prepareMemory idTable constTable)) (Map.empty) objMem


prepareMemory :: IdentifierAddressMap -> ConstantAddressMap -> Memory
prepareMemory idTable constTable = (Map.union 
                                        (makeMemory (Map.toList idTable) (Map.empty))
                                        (makeMemory (Map.toList constTable) (Map.empty))
                                    )
prepareMemoryFromObjects :: [(Address,IdentifierAddressMap)] -> ObjectMemory -> Memory -> (ObjectMemory,Memory)
prepareMemoryFromObjects [] objMem mem = (objMem,mem)
prepareMemoryFromObjects ((objAddress,idMap) : idMaps) objMem mem = 
                                        let mem1 = prepareMemory idMap (Map.empty)
                                        in let objMem1 = (Map.insert objAddress (sort (Map.keys mem1)) objMem)
                                        in let (objMem2,mem2) = prepareMemoryFromObjects idMaps objMem1 mem1 
                                        in (objMem2, (Map.union mem2 mem))

makeMemory :: [(String,Address)] -> Memory -> Memory
makeMemory [] mem = mem
makeMemory ((str,address) : addresses ) mem =
                    if (isInfixOf "<int>" str) then 
                        let mem1 = (Map.insert address (VMInteger (read (drop 5 str) :: Integer)) mem)
                        in (makeMemory addresses mem1)
                    else if (isInfixOf "<dec>" str) then 
                        let mem1 = (Map.insert address (VMDecimal (read (drop 5 str) :: Decimal)) mem)
                        in (makeMemory addresses mem1)
                    else if (isInfixOf "<bool>" str) then 
                        let mem1 = (Map.insert address (VMBool (read (drop 6 str) :: Bool)) mem)
                        in (makeMemory addresses mem1)
                    else if (isInfixOf "<str>" str) then 
                        let mem1 = (Map.insert address (VMString ((drop 5 str))) mem)
                        in (makeMemory addresses mem1)
                    else let mem1 = (Map.insert address VMEmpty mem)
                        in (makeMemory addresses mem1)


generateCodeFromStatements :: [Statement] -> QuadNum ->SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateCodeFromStatements [] quadNum symTab classSymTab varCounters idTable constTable objectAddressMap  = (varCounters,[(buildNoOpQuad quadNum)],quadNum + 1,objectAddressMap)
generateCodeFromStatements ((ConditionStatement (If expression (Block innerSts))) : sts) quadNum symTab classSymTab varCounters idTable constTable objMap  = 
            -- Le sumamos uno porque el de antes va a ser el goto en falso
            let ((intC,decC,strC,boolC,objC), quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression expression) 
            in let (varCounters2, quadsInnerStatements, lastQuadNum2,objectAddressMap2) = generateCodeFromStatements innerSts (lastQuadNum1 + 1) symTab classSymTab (intC,decC,strC,boolC,objC) idTable constTable objMap
            in let (varCounters3, quadsStatements, lastQuadNum3,objectAddressMap3) = generateCodeFromStatements sts lastQuadNum2 symTab classSymTab varCounters2 idTable constTable objectAddressMap2
            in let gotoFQuad = buildQuadForConditions lastQuadNum1 (GOTO_IF_FALSE) (boolC - 1) (getQuadNum $ head $ quadsStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
            in (varCounters3, (quadsExp ++ [gotoFQuad] ++ quadsInnerStatements ++ quadsStatements), lastQuadNum3, objectAddressMap3)
generateCodeFromStatements ((ConditionStatement (IfElse expression (Block trueStatements) (Block elseStatements))) : sts) quadNum symTab classSymTab varCounters idTable constTable objMap  = 
            -- Le sumamos uno porque el de antes va a ser el goto en falso
            let ((intC,decC,strC,boolC,objC), quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression expression) 
            in let (varCounters2, trueQuadStatements, lastQuadNum2,objMap2) = generateCodeFromStatements trueStatements (lastQuadNum1 + 1) symTab classSymTab (intC,decC,strC,boolC,objC) idTable constTable objMap
            -- Aqui tambien le sumamos mas uno porque el anterior sera un GOTO a los quadsStatements!
            in let (varCounters3, elseQuadStatements, lastQuadNum3,objMap3) = generateCodeFromStatements elseStatements (lastQuadNum2 + 1) symTab classSymTab varCounters2 idTable constTable objMap2
            in let (varCounters4, quadsStatements, lastQuadNum4,objMap4) = generateCodeFromStatements sts lastQuadNum3 symTab classSymTab varCounters3 idTable constTable objMap3
            in let gotoFQuad = buildQuadForConditions lastQuadNum1 (GOTO_IF_FALSE) (boolC - 1) (getQuadNum $ head $ elseQuadStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
            in let gotoOutOfTrue = buildGoto lastQuadNum2 (getQuadNum $ head $ quadsStatements)
            in (varCounters4, (quadsExp ++ [gotoFQuad] ++ trueQuadStatements ++ [gotoOutOfTrue] ++ elseQuadStatements ++ quadsStatements), lastQuadNum4,objMap4)
generateCodeFromStatements ((CycleStatement (CycleWhile (While expression (Block innerSts)))) : sts) quadNum symTab classSymTab varCounters idTable constTable objMap = 
            -- Le sumamos uno porque el de antes va a ser el goto en falso
            let ((intC,decC,strC,boolC,objC), quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression expression) 
            in let (varCounters2, quadsInnerStatements, lastQuadNum2, objMap2) = generateCodeFromStatements innerSts (lastQuadNum1 + 1) symTab classSymTab (intC,decC,strC,boolC,objC) idTable constTable objMap
            in let (varCounters3, quadsStatements, lastQuadNum3, objMap3) = generateCodeFromStatements sts (lastQuadNum2 + 1) symTab classSymTab varCounters2 idTable constTable objMap3
            in let gotoFQuad = buildQuadForConditions lastQuadNum1 (GOTO_IF_FALSE) (boolC - 1) (getQuadNum $ head $ quadsStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
            in let gotoCondition = buildGoto lastQuadNum2 (getQuadNum $ head $ quadsExp)
            in (varCounters3, (quadsExp ++ [gotoFQuad] ++ quadsInnerStatements ++ [gotoCondition]  ++  quadsStatements), lastQuadNum3, objMap3)
generateCodeFromStatements ((CycleStatement (CycleFor (For lowerBound upperBound (Block innerSts)))) : sts) quadNum symTab classSymTab varCounters idTable constTable objMap =
            case (Map.lookup ("<int>" ++ (show lowerBound)) constTable) of
                Just addressLB ->
                    case (Map.lookup ("<int>" ++ (show upperBound)) constTable) of 
                        Just addressUB -> let (varCounters2, quadsLoops, lastQuadNum2,objMap2) = generateCodeFromLoop innerSts quadNum symTab classSymTab varCounters idTable constTable objMap lowerBound upperBound lowerBound 
                                          in let (varCounters3, quadsStatements, lastQuadNum3,objMap3) = generateCodeFromStatements sts lastQuadNum2 symTab classSymTab varCounters2 idTable constTable objMap2
                                          -- in let forLoopQuad = buildForLoopQuad quadNum (addressLB,addressUB) (getQuadNum $ head $ quadsStatements)
                                          in (varCounters3, quadsLoops ++ quadsStatements,lastQuadNum3,objMap3)

generateCodeFromStatements ((CycleStatement (CycleForVar forVarSts ) : sts)) quadNum symTab classSymTab varCounters idTable constTable objMap =
                let (varCounters2, quadsForVarSts, lastQuadNum2,objMap2) = generateCodeFromStatements forVarSts quadNum symTab classSymTab varCounters idTable constTable objMap
                    in let (varCounters3, quadsSts, lastQuadNum3,objMap3) = generateCodeFromStatements sts lastQuadNum2 symTab classSymTab varCounters idTable constTable objMap2
                        in (varCounters3, quadsForVarSts ++ quadsSts, lastQuadNum3,objMap3)
generateCodeFromStatements (st : sts) quadNum symTab classSymTab varCounters idTable constTable objMap = 
            let (varCounters1, quads,lastQuadNum,objMap2) = generateCodeFromStatement st quadNum symTab classSymTab varCounters idTable constTable objMap
            in let (varCounters2, quadsStatements, lastQuadNum2,objMap3) = generateCodeFromStatements sts lastQuadNum symTab classSymTab varCounters1 idTable constTable objMap2
                in (varCounters2, (quads ++ quadsStatements), lastQuadNum2,objMap3)


generateCodeFromLoop :: [Statement] -> QuadNum ->SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> Integer -> Integer -> Integer -> (VariableCounters,[Quadruple],QuadNum, ObjectAddressMap)
generateCodeFromLoop sts quadNum symTab classSymTab varCounters idAddressMap consAddressMap objMap lowerBound upperBound currentIteration
            | currentIteration <= upperBound =
                        let (varCounters1, quads, quadNum1, objMap2) = generateCodeFromStatements sts quadNum symTab classSymTab varCounters idAddressMap consAddressMap objMap
                        in let (varCounters2, quadsLoop, quadNum2,objMap3) = generateCodeFromLoop sts quadNum1 symTab classSymTab varCounters1 idAddressMap consAddressMap objMap2 lowerBound upperBound (currentIteration + 1)
                        in (varCounters2, quads ++ quadsLoop, quadNum2, objMap3)
            | otherwise = (varCounters,[],quadNum, objMap)

generateCodeFromStatement :: Statement -> QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum, ObjectAddressMap)
generateCodeFromStatement (AssignStatement assignment) quadNum symTab classSymTab varCounters idTable constTable objMap = generateCodeFromAssignment assignment quadNum symTab classSymTab varCounters idTable constTable objMap
generateCodeFromStatement (VariableStatement varStatement) quadNum symTab classSymTab varCounters idTable constTable objMap = generateCodeFromVariableStatement varStatement quadNum symTab classSymTab varCounters idTable constTable objMap
generateCodeFromStatement (DPMStatement assignment) quadNum symTab classSymTab varCounters idTable constTable objMap = generateCodeFromStatement (AssignStatement assignment) quadNum symTab classSymTab varCounters idTable constTable objMap
-- MARK TODO: Hacer cuadruplos de cuando se hace una llamada a funcion
-- generateCodeFromStatement (FunctionCallStatement functionCall) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
-- generateCodeFromStatement (ReturnStatement (ReturnExp expression)) literalCounters constantAddressMap = fillFromExpression literalCounters constantAddressMap expression
-- generateCodeFromStatement (ReturnStatement (ReturnFunctionCall functionCall)) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
generateCodeFromStatement (ReadStatement (Reading identifier)) quadNumInit symTab _ varCounters idTable constTable objMap = 
    case (Map.lookup identifier idTable) of
        Just address ->  
                                    case (Map.lookup identifier idTable) of
                                        Just address ->
                                            case (Map.lookup identifier symTab) of
                                                Just (SymbolVar (TypePrimitive PrimitiveDouble _) _ _) -> 
                                                    let newQuad = [(buildQuadOneAddress (quadNumInit + 1) (DOUBLE) address)]
                                                    in (varCounters,([(buildQuadOneAddress (quadNumInit) (READ) address)]) ++ newQuad,quadNumInit + 2, objMap)
                                                Just (SymbolVar (TypePrimitive PrimitiveInt _) _ _) -> 
                                                    let newQuad = [(buildQuadOneAddress (quadNumInit + 1) (INT_64) address)]
                                                    in (varCounters,([(buildQuadOneAddress (quadNumInit) (READ) address)]) ++ newQuad,quadNumInit + 2, objMap)
                                                _ -> (varCounters,[(buildQuadOneAddress quadNumInit  (READ) address)],quadNumInit, objMap)
generateCodeFromStatement (DisplayStatement displays) quadNumInit symTab classSymTab varCounters idTable constTable objMap = 
                                                        let (quads,lastQuadNum) = (genFromDisplays displays quadNumInit idTable constTable)
                                                         in (varCounters,quads,lastQuadNum, objMap)
                                                                where 
                                                                    genFromDisplays :: [Display] -> QuadNum -> IdentifierAddressMap -> ConstantAddressMap -> ([Quadruple],QuadNum)
                                                                    genFromDisplays [] quadNum _ _  = ([],quadNum)
                                                                    genFromDisplays (disp : disps) quadNum idTable constTable = 
                                                                            let (quads,lastQuadNum) = genFromDisplay disp quadNum idTable constTable
                                                                            in let (quads2,lastQuadNum2) = (genFromDisplays disps lastQuadNum idTable constTable)
                                                                                in ((quads ++ quads2),lastQuadNum2)
                                                                    genFromDisplay :: Display -> QuadNum -> IdentifierAddressMap -> ConstantAddressMap -> ([Quadruple],QuadNum)
                                                                    genFromDisplay (DisplayLiteralOrVariable (VarIdentifier var)) quadNum idTable constTable =
                                                                        case (Map.lookup var symTab) of 
                                                                            Just (SymbolVar (TypePrimitive prim accessExpression) _ _) ->
                                                                                case accessExpression of 
                                                                                    [] -> case ((Map.lookup var idTable)) of
                                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)
                                                                                    (("[",size,"]") : []) -> case ((Map.lookup (var ++ "[0]") idTable)) of
                                                                                                                Just address -> genLoopArray address size quadNum

                                                                                    (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup (var ++ "[0][0]") idTable)) of
                                                                                                                Just address -> genLoopMatrix address rows cols 1 quadNum
                                                                            Just (SymbolVar (TypeClassId prim accessExpression) _ _) ->
                                                                                case accessExpression of 
                                                                                    [] -> case ((Map.lookup var idTable)) of
                                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)
                                                                                    (("[",size,"]") : []) -> case ((Map.lookup (var ++ "[0]") idTable)) of
                                                                                                                Just address -> genLoopArray address size quadNum

                                                                                    (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup (var ++ "[0][0]") idTable)) of
                                                                                                                Just address -> genLoopMatrix address rows cols 1 quadNum
                                                                    genFromDisplay (DisplayObjMem (ObjectMember object attribute)) quadNum idTable constTable =
                                                                        case (Map.lookup object symTab) of 
                                                                            Just (SymbolVar (TypeClassId classId _) _ _) ->
                                                                                case (Map.lookup classId classSymTab) of
                                                                                    Just symTabOfClass -> 
                                                                                        case (Map.lookup attribute symTabOfClass) of
                                                                                            Just (SymbolVar (TypeClassId prim accessExpression) _ _) ->
                                                                                                case accessExpression of 
                                                                                                    [] -> case ((Map.lookup object idTable)) of
                                                                                                            Just addressObj -> 
                                                                                                                case (Map.lookup addressObj objMap) of
                                                                                                                    Just objTable -> case (Map.lookup attribute objTable) of
                                                                                                                                        Just addressAttr -> ([(buildQuadOneAddress quadNum (DISPLAY) addressAttr)],quadNum + 1)
                                                                                                    (("[",size,"]") : []) -> case ((Map.lookup object idTable)) of
                                                                                                                                Just addressObj -> 
                                                                                                                                    case (Map.lookup addressObj objMap) of
                                                                                                                                        Just objTable -> 
                                                                                                                                            case (Map.lookup (attribute ++ "[0]") objTable) of
                                                                                                                                                Just addressAttr -> genLoopArray addressAttr size quadNum
                                                                                                    (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup object idTable)) of
                                                                                                                                                Just addressObj -> 
                                                                                                                                                    case (Map.lookup addressObj objMap) of
                                                                                                                                                        Just objTable -> 
                                                                                                                                                            case (Map.lookup (attribute ++ "[0][0]") objTable) of
                                                                                                                                                                Just addressAttr -> genLoopMatrix addressAttr rows cols 1 quadNum

                                                                            
                                                                    genFromDisplay (DisplayLiteralOrVariable (StringLiteral str)) quadNum idTable constTable =
                                                                        case ((Map.lookup ("<str>" ++ str) constTable)) of
                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)
                                                                    genFromDisplay (DisplayLiteralOrVariable (IntegerLiteral int)) quadNum idTable constTable =
                                                                        case ((Map.lookup ("<int>" ++ show(int)) constTable)) of
                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)
                                                                    genFromDisplay (DisplayLiteralOrVariable (DecimalLiteral dec)) quadNum idTable constTable =
                                                                        case ((Map.lookup ("<dec>" ++ show(dec)) constTable)) of
                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)
                                                                    genFromDisplay (DisplayLiteralOrVariable (BoolLiteral bool)) quadNum idTable constTable =
                                                                        case ((Map.lookup ("<bool>" ++ show(bool)) constTable)) of
                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)

                                                                    genFromDisplay _ quadNum idTable constTable = ([],quadNum)

                                                                    genLoopArray :: Address -> Integer -> QuadNum -> ([Quadruple],QuadNum)
                                                                    genLoopArray address 0 quadNum = ([],quadNum)
                                                                    genLoopArray address limit quadNum = 
                                                                        let newQuadAssignment = ([(buildQuadOneAddress quadNum (DISPLAY) address)])
                                                                        in let (newQuads2,lastQuadNum2) = genLoopArray (address + 1) (limit - 1) (quadNum + 1)
                                                                        in (newQuadAssignment ++ newQuads2,lastQuadNum2)

                                                                    genLoopMatrix :: Address -> Integer -> Integer -> Integer -> QuadNum -> ([Quadruple],QuadNum)
                                                                    genLoopMatrix address 0 cols _ quadNum = ([],quadNum)
                                                                    genLoopMatrix address rows cols offset quadNum = 
                                                                        let (newQuads2,lastQuadNum2) = genLoopArray address cols quadNum
                                                                        in let (newQuads3,lastQuadNum3) = genLoopMatrix (address + cols) (rows - 1) cols (offset + 1) lastQuadNum2  
                                                                        in (newQuads2 ++ newQuads3,lastQuadNum3)

                                                                    -- TODO MARK: Hacer cuadruplos de funciones y de acceso a arreglo, y tambien sustituir lo anterior por expresion!
                                                                    -- fillFromDisplay (DisplayFunctionCall funcCall) literalCounters constantAddressMap =
                                                                    --     fillFromExpression literalCounters constantAddressMap (ExpressionFuncCall funcCall)
                                                                    -- fillFromDisplay (DisplayVarArrayAccess identifier arrayAccess) literalCounters constantAddressMap =
                                                                    --     fillFromExpression literalCounters constantAddressMap (ExpressionVarArray identifier arrayAccess)  
generateCodeFromStatement _ quadNum _ _ varCounters idTable _ objMap = (varCounters,[],quadNum,objMap)

generateCodeFromAssignment :: Assignment -> QuadNum ->SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateCodeFromAssignment (AssignmentExpression identifier (ExpressionLitVar (VarIdentifier identifier2))) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup identifier symTab) of 
            Just (SymbolVar (TypeClassId classIdentifier []) _ _) -> 
                generateQuadruplesAssignmentClasses identifier identifier2 quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypeClassId _ (("[",size,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup (identifier2 ++ "[0]") idTable) of 
                                    Just address2 ->  
                                        let (v1,quads,lastQuadNum,objMap2) = assignTwoArrays address1 address2 size quadNum symTab classSymTab varCounters idTable constTable objMap
                                        in (v1,quads,lastQuadNum,objMap2)

            Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup (identifier2 ++ "[0][0]") idTable) of 
                                    Just address2 ->  
                                        let (v1,quads,lastQuadNum,objMap2) = assignTwoMatrices address1 address2 rows columns quadNum symTab classSymTab varCounters idTable constTable objMap
                                        in (v1,quads,lastQuadNum,objMap2)
            Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup (identifier2 ++ "[0]") idTable) of 
                                    Just address2 ->  
                                        let (v1,quads,lastQuadNum,objMap2) = assignTwoArrays address1 address2 size quadNum symTab classSymTab varCounters idTable constTable objMap
                                        in (v1,quads,lastQuadNum,objMap2)
            Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup (identifier2 ++ "[0][0]") idTable) of 
                                    Just address2 ->  
                                        let (v1,quads,lastQuadNum,objMap2) = assignTwoMatrices address1 address2 rows columns quadNum symTab classSymTab varCounters idTable constTable objMap
                                        in (v1,quads,lastQuadNum,objMap2)
            _ ->  
                let (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression (ExpressionLitVar (VarIdentifier identifier2))) 
                in case (Map.lookup identifier idTable) of
                    Just address -> (varCounters1,(quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]),lastQuadNum1 + 1, objMap)
generateCodeFromAssignment (AssignmentExpression identifier expression) quadNum symTab classSymTab varCounters idTable constTable objMap = 
    let (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression expression) 
    in case (Map.lookup identifier idTable) of
        Just address ->  
            case (Map.lookup identifier symTab) of
                Just (SymbolVar (TypePrimitive PrimitiveDouble _) _ _) -> 
                    let newQuad = ([(buildQuadOneAddress lastQuadNum1 (DOUBLE) (getLastAddress $ last $ quadsExp))])
                    in (varCounters1,(quadsExp ++ newQuad ++ [(buildQuadrupleTwoAddresses (lastQuadNum1 + 1) ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]),lastQuadNum1 + 2, objMap)
                Just (SymbolVar (TypePrimitive PrimitiveInt _) _ _) -> 
                    let newQuad = ([(buildQuadOneAddress lastQuadNum1 (INT_64) (getLastAddress $ last $ quadsExp))])
                    in (varCounters1,(quadsExp ++ newQuad ++ [(buildQuadrupleTwoAddresses (lastQuadNum1 + 1) ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]),lastQuadNum1 + 2, objMap)
                _ -> (varCounters1,(quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]),lastQuadNum1 + 1, objMap)
generateCodeFromAssignment (AssignmentObjectMember identifier (ObjectMember objectIdentifier attrIdentifier)) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup identifier symTab) of
            Just (SymbolVar (TypeClassId classIdentifier []) _ _) ->
                case (Map.lookup objectIdentifier idTable) of 
                    Just addressObject -> 
                            case (Map.lookup addressObject objMap) of 
                                Just idTableObj -> case (Map.lookup attrIdentifier idTableObj) of 
                                                        Just addressAttribute -> 
                                                            let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) addressAttribute idTable)
                                                            in generateCodeFromAssignment (AssignmentExpression identifier (ExpressionLitVar (VarIdentifier (objectIdentifier ++ "." ++ attrIdentifier)))) quadNum symTab classSymTab varCounters tempIdTable constTable objMap 
                -- generateQuadruplesAssignmentClasses identifier identifier2 quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypePrimitive prim  []) _ _) -> 
                case (Map.lookup identifier idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup attrIdentifier idTableObject) of
                                                    Just addressAttribute -> assignTwoArrays address1 addressAttribute 1 quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup (attrIdentifier ++ "[0]") idTableObject) of
                                                    Just addressAttribute -> assignTwoArrays address1 addressAttribute size quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup (attrIdentifier ++ "[0][0]") idTableObject) of
                                                    Just addressAttribute -> assignTwoMatrices address1 addressAttribute rows columns quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypeClassId _ (("[",size,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup (attrIdentifier ++ "[0]") idTableObject) of
                                                    Just addressAttribute -> assignTwoArrays address1 addressAttribute size quadNum symTab classSymTab varCounters idTable constTable objMap

            Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup (attrIdentifier ++ "[0][0]") idTableObject) of
                                                    Just addressAttribute -> assignTwoMatrices address1 addressAttribute rows columns quadNum symTab classSymTab varCounters idTable constTable objMap       
generateCodeFromAssignment  (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) quadNum symTab classSymTab varCounters idTable constTable objMap =  
                                -- let 

                                --     (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression expression) 
                                -- in 
                                case (Map.lookup objectIdentifier idTable) of 
                                    Just objectAddress -> 
                                        case (Map.lookup objectAddress objMap) of 
                                            Just objTable -> 
                                                case (Map.lookup objectIdentifier symTab) of 
                                                                Just (SymbolVar (TypeClassId classIdentifier []) _ _) ->
                                                                    case (Map.lookup classIdentifier classSymTab) of 
                                                                          Just symTabOfClass -> case (Map.lookup attrIdentifier symTabOfClass) of 
                                                                                                    Just (SymbolVar (TypePrimitive prim []) scp isPublic) ->
                                                                                                         case (Map.lookup attrIdentifier objTable) of 
                                                                                                               Just attrAddress -> 
                                                                                                                   let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) attrAddress idTable)
                                                                                                                   in let symbolVarAttr = (SymbolVar (TypePrimitive prim []) scp isPublic)
                                                                                                                   in let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                                   in generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression)  quadNum newSymTab classSymTab varCounters tempIdTable constTable objMap
                                                                                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic) ->
                                                                                                         case (Map.lookup (attrIdentifier ++ "[0]")  objTable) of 
                                                                                                               Just attrAddress -> 
                                                                                                                   let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0]") attrAddress idTable)
                                                                                                                   in let symbolVarAttr = (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic)
                                                                                                                   in let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                                   in generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression)  quadNum newSymTab classSymTab varCounters tempIdTable constTable objMap
                                                                                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic) ->
                                                                                                         case (Map.lookup (attrIdentifier ++ "[0][0]")  objTable) of 
                                                                                                               Just attrAddress -> 
                                                                                                                   let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0][0]") attrAddress idTable)
                                                                                                                   in let symbolVarAttr = (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic)
                                                                                                                   in let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                                   in generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression)  quadNum newSymTab classSymTab varCounters tempIdTable constTable objMap
                                                                                                    Just (SymbolVar (TypeClassId c []) scp isPublic) ->
                                                                                                         case (Map.lookup attrIdentifier objTable) of 
                                                                                                               Just attrAddress -> 
                                                                                                                   let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) attrAddress idTable)
                                                                                                                   in let symbolVarAttr = (SymbolVar (TypeClassId c []) scp isPublic)
                                                                                                                   in let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                                   in generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression)  quadNum newSymTab classSymTab varCounters tempIdTable constTable objMap
                                                                                                    Just (SymbolVar (TypeClassId c (("[",size,"]") : [])) scp isPublic) ->
                                                                                                         case (Map.lookup (attrIdentifier ++ "[0]")  objTable) of 
                                                                                                               Just attrAddress -> 
                                                                                                                   let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0]") attrAddress idTable)
                                                                                                                   in let symbolVarAttr = (SymbolVar (TypeClassId c (("[",size,"]") : [])) scp isPublic)
                                                                                                                   in let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                                   in generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression)  quadNum newSymTab classSymTab varCounters tempIdTable constTable objMap
                                                                                                    Just (SymbolVar (TypeClassId c (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic) ->
                                                                                                         case (Map.lookup (attrIdentifier ++ "[0][0]")  objTable) of 
                                                                                                               Just attrAddress -> 
                                                                                                                   let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0][0]") attrAddress idTable)
                                                                                                                   in let symbolVarAttr = (SymbolVar (TypeClassId c (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic)
                                                                                                                   in let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                                   in generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression)  quadNum newSymTab classSymTab varCounters tempIdTable constTable objMap
                                                        
                                                                                                    
                                                

generateCodeFromAssignment _ quadNum symTab classSymTab varCounters idTable constTable objMap = (varCounters,[],quadNum,objMap)


generateQuadruplesAssignmentClasses :: Identifier -> Identifier ->  QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateQuadruplesAssignmentClasses identifier1 identifier2 quadNum symTab classSymTab varCounters idTable constMap objMap =
                                             case (Map.lookup identifier1 idTable) of 
                                                Just addressReceiver -> 
                                                    case (Map.lookup identifier2 idTable) of
                                                        Just addressGiver -> 
                                                            let newQuad = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressGiver , addressReceiver ))])
                                                            in (varCounters,newQuad,quadNum + 1,objMap)
                                                    

assignTwoArrays :: Address -> Address -> Integer -> QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
assignTwoArrays addressReceiver addressGiver 0 quadNum symTab classSymTab varCounters idMap constantMap objMap = (varCounters,[],quadNum,objMap)
assignTwoArrays addressReceiver addressGiver limit quadNum symTab classSymTab varCounters idMap constantMap objMap =
        let newQuadAssignment = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressGiver , addressReceiver))])
        in let (varCounters2, newQuads2,lastQuadNum2,objMap2) = assignTwoArrays (addressReceiver + 1) (addressGiver + 1) (limit - 1) (quadNum + 1)  symTab classSymTab varCounters idMap constantMap objMap
        in (varCounters2,newQuadAssignment ++ newQuads2,lastQuadNum2,objMap2)

assignTwoMatrices :: Address -> Address -> Integer -> Integer -> QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
assignTwoMatrices addressReceiver addressGiver 0 _ quadNum symTab classSymTab varCounters idMap constantMap objMap = (varCounters,[],quadNum,objMap)
assignTwoMatrices addressReceiver addressGiver  rows columns quadNum symTab classSymTab varCounters idMap constantMap objMap =
        let (varCounters2,quads,lastQuadNum2,objMap2) = assignTwoArrays addressReceiver addressGiver columns quadNum symTab classSymTab varCounters idMap constantMap objMap
        in let (varCounters3, newQuads3,lastQuadNum3,objMap3) = assignTwoMatrices (addressReceiver + columns) (addressGiver + columns) (rows - 1) columns lastQuadNum2  symTab classSymTab varCounters2 idMap constantMap objMap2
        in (varCounters3,quads ++ newQuads3,lastQuadNum3,objMap3)

-- assignTwoMatrices :: Integer -> Integer -> QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
-- assignTwoMatrices 

-- generateCodeFromAssignment  (AssignmentArrayExpression _ ((ArrayAccessExpression innerExp) : []) expression) literalCounters constantAddressMap =  
--                                                                                         let (newLiteralCounters,newConsAddressMap) = fillFromExpression literalCounters constantAddressMap innerExp
--                                                                                         in fillFromExpression newLiteralCounters newConsAddressMap expression
-- generateCodeFromAssignment  (AssignmentArrayExpression _ ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) expression) literalCounters constantAddressMap = 
--                                                                                                                 let (newLiteralCounters,newConsAddressMap) = fillFromExpression literalCounters constantAddressMap innerExpRow
--                                                                                                                     in let (newLiteralCounters2,newConsAddressMap2) = fillFromExpression newLiteralCounters newConsAddressMap innerExpCol
--                                                                                                                         in fillFromExpression newLiteralCounters2 newConsAddressMap2 expression

generateCodeFromVariableStatement :: Variable -> QuadNum ->SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateCodeFromVariableStatement (VariableAssignmentLiteralOrVariable _ identifier literalOrVariable) quadNum symTab classSymTab varCounters idTable constTable objMap = 
                generateCodeFromAssignment ((AssignmentExpression identifier (ExpressionLitVar literalOrVariable))) quadNum symTab classSymTab varCounters idTable constTable objMap

generateCodeFromVariableStatement (VariableAssignment1D _ identifier literalOrVariables) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup (identifier ++ "[0]") idTable) of
            Just address -> generateAssignmentArray1D quadNum literalOrVariables address symTab classSymTab varCounters idTable constTable objMap 
generateCodeFromVariableStatement (VariableAssignment2D _ identifier listLiteralOrVariables) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup (identifier ++ "[0][0]") idTable) of
            Just address ->
                case (Map.lookup identifier symTab) of
                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                            generateAssignmentArray2D quadNum listLiteralOrVariables address symTab classSymTab varCounters idTable constTable objMap cols
                    Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) -> 
                            generateAssignmentArray2D quadNum listLiteralOrVariables address symTab classSymTab varCounters idTable constTable objMap cols
generateCodeFromVariableStatement _ quadNum symTab classSymTab varCounters idTable constTable objMap = (varCounters,[],quadNum, objMap)


generateAssignmentArray1D :: QuadNum -> [LiteralOrVariable] -> Address -> SymbolTable  -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum, ObjectAddressMap)
generateAssignmentArray1D quadNum [] initialAddress _ _ varCounters _ _ objMap  = (varCounters,[],quadNum,objMap)
generateAssignmentArray1D quadNum (litOrVar : litOrVars) address symTab classSymTab varCounters idTable constTable objMap = 
        case litOrVar of
            (VarIdentifier identifier) ->
                 case (Map.lookup identifier symTab) of
                    Just (SymbolVar (TypeClassId _ _) _ _) -> 
                             case (Map.lookup identifier idTable) of 
                                Just addressGiver -> 
                                    let newQuad = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressGiver , address ))])
                                    in let (varCounters2, newQuads2,lastQuadNum2,objMap2) = generateAssignmentArray1D (quadNum + 1) litOrVars (address + 1) symTab classSymTab varCounters idTable constTable objMap
                                    in (varCounters2,newQuad ++ newQuads2,lastQuadNum2,objMap2)
       
            _ -> let (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (ExpressionLitVar litOrVar)
                in let newQuads = (quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))])
                in let (varCounters2, newQuads2,lastQuadNum2,objMap2) = generateAssignmentArray1D (lastQuadNum1 + 1) litOrVars (address + 1) symTab classSymTab varCounters1 idTable constTable objMap
                in (varCounters2,newQuads ++ newQuads2,lastQuadNum2,objMap2)
        

generateAssignmentArray2D :: QuadNum -> [[LiteralOrVariable]] -> Address -> SymbolTable  -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> Integer -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateAssignmentArray2D quadNum [] initialAddress _ _ varCounters _ _ objMap _ = (varCounters,[],quadNum,objMap)
generateAssignmentArray2D quadNum (litOrVars : listLitOrVars) address symTab classSymTab varCounters idTable constTable objMap cols = 
        let (varCounters1, quadsRow, lastQuadNum1,objMap1) = generateAssignmentArray1D quadNum litOrVars address symTab classSymTab varCounters idTable constTable objMap
        in let (varCounters2, quadsRows,lastQuadNum2,objMap2) = generateAssignmentArray2D lastQuadNum1 listLitOrVars (address + cols) symTab classSymTab varCounters1 idTable constTable objMap1 cols
        in (varCounters2,quadsRow ++ quadsRows,lastQuadNum2,objMap2)












