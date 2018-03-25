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
-- type JumpsStack = Stack Quadruple

startCodeGen :: Program -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> IO()
startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab varCounters idTable constTable objMap =
            let (_,quads,_,newObjMap) =  generateCodeFromStatements statements 0 symTab classSymTab varCounters idTable constTable objMap
                in 
            do  
                mapM_ (putStrLn.show) quads
                startVM quads (Map.union  (prepareMemoryFromObjects (Map.elems objMap) Map.empty) (prepareMemory idTable constTable)) (Map.empty)


prepareMemory :: IdentifierAddressMap -> ConstantAddressMap -> Memory
prepareMemory idTable constTable = (Map.union 
                                        (makeMemory (Map.toList idTable) (Map.empty))
                                        (makeMemory (Map.toList constTable) (Map.empty))
                                    )
prepareMemoryFromObjects :: [IdentifierAddressMap] -> Memory -> Memory
prepareMemoryFromObjects [] mem = mem
prepareMemoryFromObjects (idMap : idMaps) mem = 
                                        let mem1 = prepareMemory idMap (Map.empty)
                                        in let otherMems = prepareMemoryFromObjects idMaps mem1
                                        in (Map.union otherMems mem)

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
generateCodeFromStatement (ReadStatement (Reading identifier)) quadNumInit _ _ varCounters idTable constTable objMap = 
                                    case (Map.lookup identifier idTable) of
                                        Just address ->
                                            (varCounters,[(buildQuadOneAddress quadNumInit (READ) address)],quadNumInit + 1, objMap)
generateCodeFromStatement (DisplayStatement displays) quadNumInit _ _ varCounters idTable constTable objMap = 
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
                                                                        case ((Map.lookup var idTable)) of
                                                                            Just address -> ([(buildQuadOneAddress quadNum (DISPLAY) address)],quadNum + 1)
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

                                                                    -- TODO MARK: Hacer cuadruplos de funciones y de acceso a arreglo, y tambien sustituir lo anterior por expresion!
                                                                    -- fillFromDisplay (DisplayFunctionCall funcCall) literalCounters constantAddressMap =
                                                                    --     fillFromExpression literalCounters constantAddressMap (ExpressionFuncCall funcCall)
                                                                    -- fillFromDisplay (DisplayVarArrayAccess identifier arrayAccess) literalCounters constantAddressMap =
                                                                    --     fillFromExpression literalCounters constantAddressMap (ExpressionVarArray identifier arrayAccess)  
generateCodeFromStatement _ quadNum _ _ varCounters idTable _ objMap = (varCounters,[],quadNum,objMap)

generateCodeFromAssignment :: Assignment -> QuadNum ->SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateCodeFromAssignment (AssignmentExpression identifier (ExpressionLitVar (VarIdentifier identifier2))) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup identifier symTab) of 
            Just (SymbolVar (TypeClassId classIdentifier _) _ _) -> 
                generateQuadruplesAssignmentClasses identifier identifier2 quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) _ _) -> 
                case (Map.lookup identifier idTable) of 
                            Just address1 ->  
                                 case (Map.lookup identifier2 idTable) of 
                                    Just address2 ->  
                                        let (v1,quads,lastQuadNum,objMap2) = assignTwoArrays address1 address2 size quadNum symTab classSymTab varCounters idTable constTable objMap
                                        in (v1,quads,lastQuadNum,objMap2)
            Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                case (Map.lookup identifier idTable) of 
                            Just address1 ->  
                                 case (Map.lookup identifier2 idTable) of 
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
        Just address ->   (varCounters1,(quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]),lastQuadNum1 + 1, objMap)
generateCodeFromAssignment (AssignmentObjectMember identifier (ObjectMember objectIdentifier attrIdentifier)) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup identifier symTab) of
            -- Just (SymbolVar (TypeClassId classIdentifier _) _ _) -> 
            --     generateQuadruplesAssignmentClasses identifier identifier2 quadNum symTab classSymTab varCounters idTable constTable objMap
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
                case (Map.lookup identifier idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup attrIdentifier idTableObject) of
                                                    Just addressAttribute -> assignTwoArrays address1 addressAttribute size quadNum symTab classSymTab varCounters idTable constTable objMap
            Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                case (Map.lookup identifier idTable) of 
                            Just address1 ->  
                                 case (Map.lookup objectIdentifier idTable) of 
                                    Just objAddress2 -> 
                                        case (Map.lookup objAddress2 objMap) of
                                            Just idTableObject ->
                                                case (Map.lookup attrIdentifier idTableObject) of
                                                    Just addressAttribute -> assignTwoMatrices address1 addressAttribute rows columns quadNum symTab classSymTab varCounters idTable constTable objMap        
generateCodeFromAssignment  (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) quadNum symTab classSymTab varCounters idTable constTable objMap =  
                                let (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (reduceExpression expression) 
                                in case (Map.lookup objectIdentifier idTable) of 
                                    Just objectAddress -> 
                                        case (Map.lookup objectAddress objMap) of 
                                            Just objTable -> 
                                                case (Map.lookup attrIdentifier objTable) of 
                                                    Just attrAddress -> (varCounters1,(quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , attrAddress ))]),lastQuadNum1 + 1, objMap)


generateCodeFromAssignment _ quadNum symTab classSymTab varCounters idTable constTable objMap = (varCounters,[],quadNum,objMap)


generateQuadruplesAssignmentClasses :: Identifier -> Identifier ->  QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateQuadruplesAssignmentClasses identifier1 identifier2 quadNum symTab classSymTab varCounters idMap constMap objMap =
                                case (Map.lookup identifier1 symTab) of 
                                    Just (SymbolVar (TypeClassId classIdentifier []) _ _) -> 
                                        case (Map.lookup classIdentifier classSymTab) of
                                            Just symTabOfClass -> 
                                                let classAttributes = (Map.toList symTabOfClass)
                                                in assignmentTwoObjects classAttributes identifier1 identifier2 quadNum symTab classSymTab varCounters idMap constMap objMap
                                    


assignmentTwoObjects :: [(Identifier,Symbol)] -> Identifier -> Identifier -> QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
assignmentTwoObjects [] _ _ quadNum symTab classSymTab varCounters idMap constantMap objMap = (varCounters,[],quadNum,objMap)
assignmentTwoObjects ((attr,(SymbolVar ((TypePrimitive prim arrayAccess)) _ _)) : symbols) identifier1 identifier2 quadNum symTab classSymTab varCounters idMap constantMap objMap =
                                    case arrayAccess of 
                                        [] -> 
                                            case (Map.lookup identifier1 idMap) of 
                                                Just address ->  
                                                    case (Map.lookup address objMap) of 
                                                        Just idTableObj -> 
                                                            case (Map.lookup attr idTableObj) of 
                                                                Just addressAttr1 ->
                                                                     case (Map.lookup identifier2 idMap) of 
                                                                        Just address2 ->  
                                                                            case (Map.lookup address2 objMap) of 
                                                                                Just idTableObj2 -> 
                                                                                    case (Map.lookup attr idTableObj2) of
                                                                                        Just addressAttr2 -> 
                                                                                            let (v1,quads,lastQuadNum,objMap2) = assignTwoArrays addressAttr1 addressAttr2 1 quadNum symTab classSymTab varCounters idMap constantMap objMap
                                                                                            in let (v2,quads2,lastQuadNum2,objMap3) = assignmentTwoObjects symbols identifier1 identifier2 lastQuadNum symTab classSymTab v1 idMap constantMap objMap2
                                                                                            in (v2,quads ++ quads2,lastQuadNum2,objMap3)
                                        (("[",size,"]") : []) ->                    
                                            case (Map.lookup identifier1 idMap) of 
                                                Just address ->  
                                                    case (Map.lookup address objMap) of 
                                                        Just idTableObj -> 
                                                            case (Map.lookup attr idTableObj) of 
                                                                Just addressAttr1 ->
                                                                     case (Map.lookup identifier2 idMap) of 
                                                                        Just address2 ->  
                                                                            case (Map.lookup address2 objMap) of 
                                                                                Just idTableObj2 -> 
                                                                                    case (Map.lookup attr idTableObj2) of 
                                                                                        Just addressAttr2 -> 
                                                                                           let (v1,quads,lastQuadNum,objMap2) = assignTwoArrays addressAttr1 addressAttr2 size quadNum symTab classSymTab varCounters idMap constantMap objMap
                                                                                            in let (v2,quads2,lastQuadNum2,objMap3) = assignmentTwoObjects symbols identifier1 identifier2 lastQuadNum symTab classSymTab v1 idMap constantMap objMap2
                                                                                            in (v2,quads ++ quads2,lastQuadNum2,objMap3)
                                        (("[",rows,"]") : ("[",columns,"]")  : []) ->  
                                            case (Map.lookup identifier1 idMap) of 
                                                Just address ->  
                                                    case (Map.lookup address objMap) of 
                                                        Just idTableObj -> 
                                                            case (Map.lookup attr idTableObj) of 
                                                                Just addressAttr1 ->
                                                                     case (Map.lookup identifier2 idMap) of 
                                                                        Just address2 ->  
                                                                            case (Map.lookup address2 objMap) of 
                                                                                Just idTableObj2 -> 
                                                                                    case (Map.lookup attr idTableObj2) of 
                                                                                        Just addressAttr2 -> 
                                                                                            let (v1,quads,lastQuadNum,objMap2) = assignTwoMatrices addressAttr1 addressAttr2 rows columns quadNum symTab classSymTab varCounters idMap constantMap objMap
                                                                                                in let (v2,quads2,lastQuadNum2,objMap3) = assignmentTwoObjects symbols identifier1 identifier2 lastQuadNum symTab classSymTab v1 idMap constantMap objMap2
                                                                                                in (v2,quads ++ quads2,lastQuadNum2,objMap3)
assignmentTwoObjects ((attr,(SymbolVar ((TypeClassId classID arrayAccess)) _ _)) : symbols) identifier1 identifier2 quadNum symTab classSymTab varCounters idMap constantMap objMap =
                                    case arrayAccess of 
                                        [] -> 
                                            case (Map.lookup identifier1 idMap) of 
                                                Just address ->  
                                                    case (Map.lookup address objMap) of 
                                                        Just idTableObj -> 
                                                            case (Map.lookup attr idTableObj) of 
                                                                Just addressAttr1 ->
                                                                     case (Map.lookup identifier2 idMap) of 
                                                                        Just address2 ->  
                                                                            case (Map.lookup address2 objMap) of 
                                                                                Just idTableObj2 -> 
                                                                                    case (Map.lookup attr idTableObj2) of
                                                                                        Just addressAttr2 ->  
                                                                                            let temporalIdTableObj = (Map.insert ("_2" ++ attr) addressAttr2 idMap)
                                                                                            in let queryTable = (Map.insert attr addressAttr1 temporalIdTableObj)
                                                                                            in case (Map.lookup identifier1 symTab) of
                                                                                                Just (SymbolVar (TypeClassId classId _) _ _) ->
                                                                                                    case (Map.lookup classId classSymTab) of 
                                                                                                        Just symTabClass -> 
                                                                                                            let (v1,quads,lastQuadNum,objMap2) = generateQuadruplesAssignmentClasses (attr) ("_2" ++ attr) quadNum symTabClass classSymTab varCounters queryTable constantMap objMap
                                                                                                            in let (v2,quads2,lastQuadNum2,objMap3) = assignmentTwoObjects symbols identifier1 identifier2 lastQuadNum symTab classSymTab v1 idMap constantMap objMap
                                                                                                            in (v2,quads ++ quads2,lastQuadNum2,objMap3)
                                        -- (("[",size,"]") : []) ->                    
                                        --     case (Map.lookup identifier1 idMap) of 
                                        --         Just address ->  
                                        --             case (Map.lookup address objMap) of 
                                        --                 Just idTableObj -> 
                                        --                     case (Map.lookup attr idTableObj) of 
                                        --                         Just addressAttr1 ->
                                        --                              case (Map.lookup identifier2 idMap) of 
                                        --                                 Just address2 ->  
                                        --                                     case (Map.lookup address2 objMap) of 
                                        --                                         Just idTableObj2 -> 
                                        --                                             case (Map.lookup attr idTableObj2) of 
                                        --                                                 Just addressAttr2 -> 
                                        --                                                    let (v1,quads,lastQuadNum,objMap2) = assignTwoArrays addressAttr1 addressAttr2 size quadNum symTab classSymTab varCounters idMap constantMap objMap
                                        --                                                     in let (v2,quads2,lastQuadNum2,objMap3) = assignmentTwoObjects symbols identifier1 identifier2 lastQuadNum symTab classSymTab v1 idMap constantMap objMap2
                                        --                                                     in (v2,quads ++ quads2,lastQuadNum2,objMap3)
assignmentTwoObjects (_ : symbols) identifier1 identifier2 quadNum symTab classSymTab varCounters idMap constantMap objMap = assignmentTwoObjects symbols identifier1 identifier2 quadNum symTab classSymTab varCounters idMap constantMap objMap


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
    -- let (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (ExpressionLitVar literalOrVariable)
    -- in case (Map.lookup identifier idTable) of
    --     Just address -> (varCounters1,(quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]),lastQuadNum1 + 1,objMap)
generateCodeFromVariableStatement (VariableAssignment1D _ identifier literalOrVariables) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup identifier idTable) of
            Just address -> generateAssignmentArray1D quadNum literalOrVariables address symTab classSymTab varCounters idTable constTable objMap 
generateCodeFromVariableStatement (VariableAssignment2D _ identifier listLiteralOrVariables) quadNum symTab classSymTab varCounters idTable constTable objMap = 
        case (Map.lookup identifier idTable) of
            Just address -> generateAssignmentArray2D quadNum listLiteralOrVariables address symTab classSymTab varCounters idTable constTable objMap 1 
generateCodeFromVariableStatement _ quadNum symTab classSymTab varCounters idTable constTable objMap = (varCounters,[],quadNum, objMap)


generateAssignmentArray1D :: QuadNum -> [LiteralOrVariable] -> Address -> SymbolTable  -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> (VariableCounters,[Quadruple],QuadNum, ObjectAddressMap)
generateAssignmentArray1D quadNum [] initialAddress _ _ varCounters _ _ objMap  = (varCounters,[],quadNum,objMap)
generateAssignmentArray1D quadNum (litOrVar : litOrVars) address symTab classSymTab varCounters idTable constTable objMap = 
        let (varCounters1, quadsExp, lastQuadNum1) = expCodeGen symTab constTable idTable varCounters quadNum (ExpressionLitVar litOrVar)
        in let newQuads = (quadsExp ++ [(buildQuadrupleTwoAddresses lastQuadNum1 ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))])
        in let (varCounters2, newQuads2,lastQuadNum2,objMap2) = generateAssignmentArray1D (lastQuadNum1 + 1) litOrVars (address + 1) symTab classSymTab varCounters1 idTable constTable objMap
        in (varCounters2,newQuads ++ newQuads2,lastQuadNum2,objMap2)

generateAssignmentArray2D :: QuadNum -> [[LiteralOrVariable]] -> Address -> SymbolTable  -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> Integer -> (VariableCounters,[Quadruple],QuadNum,ObjectAddressMap)
generateAssignmentArray2D quadNum [] initialAddress _ _ varCounters _ _ objMap _ = (varCounters,[],quadNum,objMap)
generateAssignmentArray2D quadNum (litOrVars : listLitOrVars) address symTab classSymTab varCounters idTable constTable objMap offset = 
        let (varCounters1, quadsRow, lastQuadNum1,objMap1) = generateAssignmentArray1D quadNum litOrVars (address * offset) symTab classSymTab varCounters idTable constTable objMap
        in let (varCounters2, quadsRows,lastQuadNum2,objMap2) = generateAssignmentArray2D lastQuadNum1 listLitOrVars (address) symTab classSymTab varCounters1 idTable constTable objMap1 (offset + 1)
        in (varCounters2,quadsRow ++ quadsRows,lastQuadNum2,objMap2)












