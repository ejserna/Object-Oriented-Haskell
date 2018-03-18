module CodeGen where 
import DataTypes
import Quadruple
import SymbolTable
import ClassSymbolTable
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
-- import Data.Stack as Stack

-- type JumpsStack = Stack Quadruple

startCodeGen :: Program -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> IO()
startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab varCounters idTable constTable =
            let (_,quads,_) =  generateCodeFromStatements statements 1 symTab classSymTab varCounters idTable constTable
                in 
            do  mapM_ (putStrLn.show) quads


generateCodeFromStatements :: [Statement] -> QuadNum ->SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> (VariableCounters,[Quadruple],QuadNum)
generateCodeFromStatements [] quadNum symTab classSymTab varCounters idTable constTable = (varCounters,[(buildNoOpQuad quadNum)],quadNum + 1)
generateCodeFromStatements ((ConditionStatement (If expression (Block innerSts))) : sts) quadNum symTab classSymTab varCounters idTable constTable  = 
            -- Le sumamos uno porque el de antes va a ser el goto en falso
            let (varCounters2, quadsInnerStatements, lastQuadNum2) = generateCodeFromStatements innerSts (quadNum + 1) symTab classSymTab varCounters idTable constTable
            in let (varCounters3, quadsStatements, lastQuadNum3) = generateCodeFromStatements sts lastQuadNum2 symTab classSymTab varCounters2 idTable constTable
            in let gotoFQuad = buildQuadForConditions quadNum (GOTO_IF_FALSE) (0) (getQuadNum $ head $ quadsStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
            in (varCounters3, ([gotoFQuad] ++ quadsInnerStatements ++ quadsStatements), lastQuadNum3)
generateCodeFromStatements ((ConditionStatement (IfElse expression (Block trueStatements) (Block elseStatements))) : sts) quadNum symTab classSymTab varCounters idTable constTable  = 
            -- Le sumamos uno porque el de antes va a ser el goto en falso
            let (varCounters2, trueQuadStatements, lastQuadNum2) = generateCodeFromStatements trueStatements (quadNum + 1) symTab classSymTab varCounters idTable constTable
            -- Aqui tambien le sumamos mas uno porque el anterior sera un GOTO a los quadsStatements!
            in let (varCounters3, elseQuadStatements, lastQuadNum3) = generateCodeFromStatements elseStatements (lastQuadNum2 + 1) symTab classSymTab varCounters2 idTable constTable
            in let (varCounters4, quadsStatements, lastQuadNum4) = generateCodeFromStatements sts lastQuadNum3 symTab classSymTab varCounters3 idTable constTable
            in let gotoFQuad = buildQuadForConditions quadNum (GOTO_IF_FALSE) (0) (getQuadNum $ head $ elseQuadStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
            in let gotoOutOfTrue = buildGoto lastQuadNum2 (getQuadNum $ head $ quadsStatements)
            in (varCounters4, ([gotoFQuad] ++ trueQuadStatements ++ [gotoOutOfTrue] ++ elseQuadStatements ++ quadsStatements), lastQuadNum4)
generateCodeFromStatements ((CycleStatement (CycleWhile (While expression (Block innerSts)))) : sts) quadNum symTab classSymTab varCounters idTable constTable  = 
            -- Le sumamos uno porque el de antes va a ser el goto en falso
            let quadNumExp = 10000
            in let (varCounters2, quadsInnerStatements, lastQuadNum2) = generateCodeFromStatements innerSts (quadNum + 1) symTab classSymTab varCounters idTable constTable
            in let (varCounters3, quadsStatements, lastQuadNum3) = generateCodeFromStatements sts (lastQuadNum2 + 1) symTab classSymTab varCounters2 idTable constTable
            in let gotoFQuad = buildQuadForConditions quadNum (GOTO_IF_FALSE) (0) (getQuadNum $ head $ quadsStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
            in let gotoCondition = buildGoto lastQuadNum2 quadNumExp
            in (varCounters3, ([gotoFQuad] ++ quadsInnerStatements ++ [gotoCondition]  ++  quadsStatements), lastQuadNum3)
    


generateCodeFromStatements (st : sts) quadNum symTab classSymTab varCounters idTable constTable = 
            let (varCounters1, quads,lastQuadNum) = generateCodeFromStatement st quadNum symTab classSymTab varCounters idTable constTable 
            in let (varCounters2, quadsStatements, lastQuadNum2) = generateCodeFromStatements sts lastQuadNum symTab classSymTab varCounters1 idTable constTable
                in (varCounters2, (quads ++ quadsStatements), lastQuadNum2)

generateCodeFromStatement :: Statement -> QuadNum -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> (VariableCounters,[Quadruple],QuadNum)

-- generateCodeFromStatement (AssignStatement assignment) symTab classSymTab varCounters idTable constTable = fillFromAssignment assignment literalCounters constantAddressMap
-- generateCodeFromStatement (VariableStatement var) symTab classSymTab varCounters idTable constTable = fillFromVariable var literalCounters constantAddressMap

-- generateCodeFromStatement (ConditionStatement (If expression (Block statements))) quadNumInit _ _ varCounters idTable constTable = 
--                                                 -- # MARK TODO: Generar cuadruplos de expresion
--                                                 let (newLiteralCounters, newConsAddressMap) = fillFromExpression literalCounters constantAddressMap expression
--                                                     in let (newLiteralCounters2, newConsAddressMap2) = prepareConstantAddressMap statements newLiteralCounters newConsAddressMap
--                                                         in (newLiteralCounters2, newConsAddressMap2)  
-- generateCodeFromStatement (ConditionStatement (IfElse expression (Block statements) (Block statements2)))literalCounters constantAddressMap = 
--                                                 let (newLiteralCounters, newConsAddressMap) = fillFromExpression literalCounters constantAddressMap expression
--                                                     in let (newLiteralCounters2, newConsAddressMap2) = prepareConstantAddressMap statements newLiteralCounters newConsAddressMap
--                                                         in prepareConstantAddressMap statements2 newLiteralCounters2 newConsAddressMap2  
-- generateCodeFromStatement (CycleStatement (CycleWhile (While expression (Block statements)))) literalCounters constantAddressMap = 
--                 let (newLiteralCounters, newConsAddressMap) = fillFromExpression literalCounters constantAddressMap expression
--                     in let (newLiteralCounters2, newConsAddressMap2) = prepareConstantAddressMap statements newLiteralCounters newConsAddressMap
--                         in (newLiteralCounters2, newConsAddressMap2)
-- generateCodeFromStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) (intLitC,decLitC, strLitC, boolLitC) constantAddressMap =
--                                                              case (Map.lookup ("<int>" ++ (show lowerRange)) constantAddressMap) of
--                                                                 Just _ -> 
--                                                                         case (Map.lookup ("<int>" ++ (show greaterRange)) constantAddressMap) of
--                                                                             Just _ -> ((intLitC, decLitC, strLitC, boolLitC), constantAddressMap)
--                                                                             _ -> let newConsAddressMap2 = (Map.insert ("<int>" ++ (show greaterRange)) intLitC constantAddressMap)
--                                                                                     in ( prepareConstantAddressMap statements (intLitC + 1, decLitC, strLitC, boolLitC) newConsAddressMap2 )
--                                                                 _ -> let newConsAddressMap = (Map.insert ("<int>" ++ (show lowerRange)) intLitC constantAddressMap)
--                                                                         in case (Map.lookup ("<int>" ++ (show greaterRange)) constantAddressMap) of
--                                                                             Just _ -> ((intLitC + 1, decLitC, strLitC, boolLitC), newConsAddressMap)
--                                                                             _ -> let newConsAddressMap2 = (Map.insert ("<int>" ++ (show greaterRange)) (intLitC + 1) newConsAddressMap)
--                                                                                     in ( prepareConstantAddressMap statements (intLitC + 2, decLitC, strLitC, boolLitC) newConsAddressMap2 )
-- generateCodeFromStatement (CycleStatement (CycleForVar statements)) literalCounters constantAddressMap = prepareConstantAddressMap statements literalCounters constantAddressMap
-- generateCodeFromStatement (DPMStatement assignment) literalCounters constantAddressMap = fillFromStatement (AssignStatement assignment) literalCounters constantAddressMap
-- generateCodeFromStatement (FunctionCallStatement functionCall) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
-- generateCodeFromStatement (ReturnStatement (ReturnExp expression)) literalCounters constantAddressMap = fillFromExpression literalCounters constantAddressMap expression
-- generateCodeFromStatement (ReturnStatement (ReturnFunctionCall functionCall)) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
generateCodeFromStatement (ReadStatement (Reading identifier)) quadNumInit _ _ varCounters idTable constTable = 
                                    case (Map.lookup identifier idTable) of
                                        Just address ->
                                            (varCounters,[(buildQuadOneAddress quadNumInit (READ) address)],quadNumInit + 1)
generateCodeFromStatement (DisplayStatement displays) quadNumInit _ _ varCounters idTable constTable = 
                                                        let (quads,lastQuadNum) = (genFromDisplays displays quadNumInit idTable constTable)
                                                         in (varCounters,quads,lastQuadNum)
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
generateCodeFromStatement _ quadNum _ _ varCounters idTable _ = (varCounters,[],quadNum)



