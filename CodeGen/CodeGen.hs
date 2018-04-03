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
import CodeGenDataTypes
import Data.List(isInfixOf)
import Data.List (sortBy,sort,intercalate)
import Data.Ord (comparing)
import Data.Function (on)
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import  System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)


startCodeGen :: Program -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> FunctionMap -> String -> IO()
startCodeGen (Program classes functions variables (Block statements)) symTab classSymTab varCounters1 idTable constTable objMap funcMap currModule =
            do 
            let cgState = (setCGState symTab varCounters1 0)
            -- putStrLn.ppShow $ funcMap
            let cgEnv = setCGEnvironment classSymTab objMap idTable constTable funcMap currModule
            (newCgState,quads) <-  execRWST (generateCodeFromStatements statements) cgEnv cgState
            let (int,dec,str,bool,obj) = (varCounters newCgState)
            -- let ((int,dec,str,bool,obj),quads,_,newObjMap) =  generateCodeFromStatements statements 0 symTab classSymTab varCounters idTable constTable objMap
            --     in 
            -- do  
            -- mapM_ (putStrLn.show) quads
            putStrLn $ (style Bold $ "# Ints: ") ++ (color Magenta $ show $ (int - 1))
            putStrLn $ (style Bold $ "# Decimals: ") ++ (color Magenta $ show $ (dec - endIntGlobalMemory - 1))
            putStrLn $ (style Bold $ "# Strings: ") ++ (color Magenta $ show $ (str - endDecimalGlobalMemory - 1))
            putStrLn $ (style Bold $ "# Booleans: ") ++ (color Magenta $ show $ (bool - endStringGlobalMemory - 1))
            putStrLn $ (style Bold $ "# Objects: ") ++ (color Magenta $ show $ (obj - endObjectLocalMemory - 1))
            -- mapM_ (putStrLn.ppShow) $ intercalate " , " [(color White . show $ (int - endIntGlobalMemory)), (color White . show $ (dec - endDecimalGlobalMemory)), (color White . show $ (str - endStringGlobalMemory)), (color White . show $ (obj - endObjectGlobalMemory))]
            let (objMem,memoryFromAttributes) = prepareMemoryFromObjects (Map.toList objMap) Map.empty Map.empty
            mapM_ (putStrLn.show) quads
            let funcMem = prepareMemoryFromFunctions (Map.toList funcMap) (Map.empty) 
            -- mapM_ (putStrLn.show) $ (sortBy (compare `on` fst) (Map.toList funcMem) )
            -- putStrLn $ ppShow $ (sortBy (compare `on` snd) (Map.toList constTable) )

            -- putStrLn $ ppShow $ (sortBy (compare `on` fst) (Map.toList idTable) )
            -- putStrLn $ ppShow $ (Map.union  memoryFromAttributes (prepareMemory idTable constTable))
            startVM quads (Map.union  memoryFromAttributes (prepareMemory idTable constTable)) (Map.empty) objMem funcMem

-- generateQuadruplesFromInput :: [Statement] -> SymbolTable -> ClassSymbolTable -> VariableCounters -> IdentifierAddressMap -> ConstantAddressMap -> ObjectAddressMap -> FunctionMap -> String -> CodeGen [Quadruple]
-- generateQuadruplesFromInput  statements symTab classSymTab varCounters1 idTable constTable objMap funcMap currModule =
--             do 
--                 let cgState = (setCGState symTab varCounters1 0)
--                 let cgEnv = setCGEnvironment classSymTab objMap idTable constTable funcMap currModule
--                 (_,quads) <-  liftIO $ execRWST (generateCodeFromStatements statements) cgEnv cgState
--                 return quads

prepareMemory :: IdentifierAddressMap -> ConstantAddressMap -> Memory
prepareMemory idTable constTable = (Map.union 
                                        (makeMemory (Map.toList idTable) (Map.empty))
                                        (makeMemory (Map.toList constTable) (Map.empty))
                                    )

prepareMemoryFromFunctions :: [(String,FunctionData)] -> FunctionMemoryMap -> FunctionMemoryMap
prepareMemoryFromFunctions []  fmem = fmem
prepareMemoryFromFunctions ((funcID,funcData) : funcs) fmem =
                                        let (FunctionData instructions _ fIdMap fObjMap) = funcData
                                        in let memFunc = prepareMemory fIdMap (Map.empty)
                                        in let (objMemFunc,objMemAttributes) = prepareMemoryFromObjects (Map.toList fObjMap) Map.empty Map.empty
                                        in let funcMem = (FunctionMemory instructions (Map.union memFunc objMemAttributes) objMemFunc)
                                        in let fMemNew = (Map.insert funcID funcMem fmem)
                                        in let fMems = prepareMemoryFromFunctions funcs fMemNew 
                                        in fMems

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


generateCodeFromStatements :: [Statement] -> CG
generateCodeFromStatements []  = do 
                                    cgState <- get
                                    let (_,_,quadNum) = getCGState cgState
                                    modify $ \s -> (s { currentQuadNum = quadNum + 1 })
                                    tell $ [(buildNoOpQuad quadNum)] 
generateCodeFromStatements ((ConditionStatement (If expression (Block innerSts))) : sts)  =
            do 
                cgEnv <- ask
                cgState <- get
                let (symTab,varCounters,quadNumBeforeExp) = getCGState cgState
                (stateQuadsExp,quadsExp) <- liftIO $ execRWST (expCodeGen (reduceExpression expression))
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters quadNumBeforeExp)
                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNumAfterExp) = getCGState stateQuadsExp
                (stateQuadsTrue,quadsInnerStatements) <- liftIO $ execRWST (generateCodeFromStatements innerSts)
                                                                               cgEnv 
                                                                             (setCGState symTab (intGC, decGC, strGC, boolGC,objGC) (quadNumAfterExp + 1))
                let (symTab,varCounters,quadNumAfterInnerSts) = getCGState stateQuadsTrue
                (stateQuadsSts,quadsStatements) <- liftIO $ execRWST (generateCodeFromStatements sts)
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters (quadNumAfterInnerSts))
                let (symTab,varCounters,quadNumEndSts) = getCGState stateQuadsSts 
                let gotoFQuad = buildQuadForConditions quadNumAfterExp (GOTO_IF_FALSE) (boolGC - 1) (getQuadNum $ head $ quadsStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
                modify $ \s -> (s { varCounters = varCounters})
                modify $ \s -> (s { currentQuadNum = quadNumEndSts})
                tell $ quadsExp ++ [gotoFQuad] ++ quadsInnerStatements ++ quadsStatements 
                                  
generateCodeFromStatements ((ConditionStatement (IfElse expression (Block trueStatements) (Block elseStatements))) : sts) = 
             do 
                cgEnv <- ask
                cgState <- get
                let (symTab,varCounters,quadNumBeforeExp) = getCGState cgState
                (stateQuadsExp,quadsExp) <- liftIO $ execRWST (expCodeGen (reduceExpression expression))
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters quadNumBeforeExp)
                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNumAfterExp) = getCGState stateQuadsExp
                (stateQuadsTrue,quadsTrueStatements) <- liftIO $ execRWST (generateCodeFromStatements trueStatements)
                                                                               cgEnv 
                                                                             (setCGState symTab (intGC, decGC, strGC, boolGC,objGC) (quadNumAfterExp + 1))
                let (symTab,varCounters,quadNumEndTrue) = getCGState stateQuadsTrue
                (stateQuadsElse,quadsElseStatements) <- liftIO $ execRWST (generateCodeFromStatements elseStatements)
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters (quadNumEndTrue + 1))
                let (symTab,varCounters5,quadNumEndElse) = getCGState stateQuadsElse 
                (stateQuadsSts,quadsStatements) <- liftIO $ execRWST (generateCodeFromStatements sts)
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters (quadNumEndElse))
                let (symTab,varCounters,quadNumEndSts) = getCGState stateQuadsSts 
                let gotoFQuad = buildQuadForConditions quadNumAfterExp (GOTO_IF_FALSE) (boolGC - 1) (getQuadNum $ head $ quadsElseStatements)
                let gotoOutOfTrue = buildGoto quadNumEndTrue (getQuadNum $ head $ quadsStatements)
                modify $ \s -> (s { varCounters = varCounters})
                modify $ \s -> (s { currentQuadNum = quadNumEndSts})
                tell $ quadsExp ++ [gotoFQuad] ++ quadsTrueStatements ++ [gotoOutOfTrue] ++ quadsElseStatements ++ quadsStatements

generateCodeFromStatements ((CycleStatement (CycleWhile (While expression (Block innerSts)))) : sts) =

            do 
                cgEnv <- ask
                cgState <- get
                let (symTab,varCounters,quadNumBeforeExp) = getCGState cgState
                (stateQuadsExp,quadsExp) <- liftIO $ execRWST (expCodeGen (reduceExpression expression))
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters quadNumBeforeExp)
                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNumAfterExp) = getCGState stateQuadsExp
                (stateQuadsInner,quadsInnerStatements) <- liftIO $ execRWST (generateCodeFromStatements innerSts)
                                                                               cgEnv 
                                                                             (setCGState symTab (intGC, decGC, strGC, boolGC,objGC) (quadNumAfterExp + 1))
                let (symTab,varCounters,quadNumEndTrue) = getCGState stateQuadsInner
                (stateQuadsSts,quadsStatements) <- liftIO $ execRWST (generateCodeFromStatements sts)
                                                                               cgEnv 
                                                                             (setCGState symTab varCounters (quadNumEndTrue + 1))
                let (symTab,varCounters,quadNumEndSts) = getCGState stateQuadsSts 
                let gotoFQuad = buildQuadForConditions quadNumAfterExp (GOTO_IF_FALSE) (boolGC - 1) (getQuadNum $ head $ quadsStatements) -- MARK TODO: Cambiar 0 por lo que de expresion
                let gotoCondition = buildGoto quadNumEndTrue (getQuadNum $ head $ quadsExp)
                modify $ \s -> (s { varCounters = varCounters})
                modify $ \s -> (s { currentQuadNum = quadNumEndSts})
                tell $ quadsExp ++ [gotoFQuad] ++ quadsInnerStatements ++ [gotoCondition]  ++  quadsStatements   
                
generateCodeFromStatements ((CycleStatement (CycleFor (For lowerBound upperBound (Block innerSts)))) : sts)  =
            do 
                generateCodeFromLoop innerSts lowerBound upperBound lowerBound 
                generateCodeFromStatements sts


generateCodeFromStatements ((CycleStatement (CycleForVar forVarSts ) : sts)) =
            do  
                generateCodeFromStatements forVarSts
                generateCodeFromStatements sts

generateCodeFromStatements (st : sts)  = 
            do 
                generateCodeFromStatement st 
                generateCodeFromStatements sts 


generateCodeFromLoop :: [Statement] -> Integer -> Integer -> Integer -> CG
generateCodeFromLoop sts lowerBound upperBound currentIteration
            | currentIteration <= upperBound =
                        do 
                            generateCodeFromStatements sts
                            generateCodeFromLoop sts lowerBound upperBound (currentIteration + 1)
            | otherwise = return ()



generateCodeReturnFromFunction :: Expression -> CG
generateCodeReturnFromFunction (ExpressionLitVar (VarIdentifier identifierExp)) = 
                                                 do 
                                                    cgEnv <- ask
                                                    cgState <- get
                                                    let (_,_,idTable,_,funcMap,currentModule) = getCGEnvironment cgEnv
                                                    let (symTab,_,quadNum) = getCGState cgState 
                                                    case (Map.lookup identifierExp symTab) of 
                                                        Just (SymbolVar (TypePrimitive prim accessExpression) _ _) ->
                                                            case accessExpression of 
                                                                [] -> case ((Map.lookup identifierExp idTable)) of
                                                                        Just address -> do 
                                                                                            tell $ [(buildReturnFromFunction quadNum [address])]
                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                (("[",size,"]") : []) -> case ((Map.lookup (identifierExp ++ "[0]") idTable)) of
                                                                                            Just address -> do 
                                                                                                                let addressesArray = take (fromIntegral size) [address..]
                                                                                                                tell $ [(buildReturnFromFunction quadNum addressesArray)]
                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})

                                                                (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup (identifierExp ++ "[0][0]") idTable)) of
                                                                                    Just address -> do 
                                                                                                        let addressesArray = take (fromIntegral $ rows * cols) [address..]
                                                                                                        tell $ [(buildReturnFromFunction quadNum addressesArray)]
                                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                        Just (SymbolVar (TypeClassId prim accessExpression) _ _) ->
                                                            case accessExpression of 
                                                                [] -> case ((Map.lookup identifierExp idTable)) of
                                                                        Just address -> do 
                                                                                            tell $ [(buildReturnFromFunction quadNum [address])]
                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                (("[",size,"]") : []) -> case ((Map.lookup (identifierExp ++ "[0]") idTable)) of
                                                                                            Just address -> do 
                                                                                                                let addressesArray = take (fromIntegral size) [address..]
                                                                                                                tell $ [(buildReturnFromFunction quadNum addressesArray)]
                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})

                                                                (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup (identifierExp ++ "[0][0]") idTable)) of
                                                                                    Just address -> do 
                                                                                                        let addressesArray = take (fromIntegral $ rows * cols) [address..]
                                                                                                        tell $ [(buildReturnFromFunction quadNum  addressesArray)]
                                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})
generateCodeReturnFromFunction expression =  do 
                                                (_,quads) <- listen $ expCodeGen expression
                                                cgState <- get
                                                let (_,_,quadNum) = getCGState cgState
                                                tell $ [(buildReturnFromFunction quadNum [(getLastAddress $ last $ quads)])]
                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})

                                                            
generateCodeFromStatement :: Statement -> CG
generateCodeFromStatement (AssignStatement assignment)  = generateCodeFromAssignment assignment
generateCodeFromStatement (VariableStatement varStatement) = generateCodeFromVariableStatement varStatement 
generateCodeFromStatement (DPMStatement assignment)  = generateCodeFromStatement (AssignStatement assignment)
-- MARK TODO: Hacer cuadruplos de cuando se hace una llamada a funcion
generateCodeFromStatement (FunctionCallStatement functionCall)  = generateCodeFuncCall functionCall []
generateCodeFromStatement (ReturnStatement (ReturnExp expression)) = generateCodeReturnFromFunction expression
-- generateCodeFromStatement (ReturnStatement (ReturnFunctionCall functionCall)) literalCounters constantAddressMap = fillFromFunctionCall functionCall literalCounters constantAddressMap
generateCodeFromStatement (ReadStatement (Reading identifier))  = 
    do 
        cgEnv <- ask
        cgState <- get
        let (_,_,idTable,_,_,_) = getCGEnvironment cgEnv
        let (symTab,_,quadNum) = getCGState cgState
        case (Map.lookup identifier idTable) of
            Just address ->
                case (Map.lookup identifier symTab) of
                    Just (SymbolVar (TypePrimitive PrimitiveDouble _) _ _) -> 
                        do 
                            let quadDouble = [(buildQuadOneAddress (quadNum + 1) (DOUBLE) address)]
                            tell $ ([(buildQuadOneAddress (quadNum) (READ) address)]) ++ quadDouble
                            modify $ \s -> (s { currentQuadNum = quadNum + 2})
                    Just (SymbolVar (TypePrimitive PrimitiveInt _) _ _) -> 
                        do
                            let quadInt64 = [(buildQuadOneAddress (quadNum + 1) (INT_64) address)]
                            tell $ ([(buildQuadOneAddress (quadNum) (READ) address)]) ++ quadInt64
                            modify $ \s -> (s { currentQuadNum = quadNum + 2})
                    _ -> do 
                            tell $ [(buildQuadOneAddress (quadNum) (READ) address)]
                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                        
generateCodeFromStatement (DisplayStatement displays)  =  genFromDisplays displays
                                                                where 
                                                                    genFromDisplays :: [Display] -> CG
                                                                    genFromDisplays [] = return ()
                                                                    genFromDisplays (disp : disps) = 
                                                                                                    do 
                                                                                                        genFromDisplay disp
                                                                                                        genFromDisplays disps
                                                                            
                                                                    genFromDisplay :: Display-> CG
                                                                    genFromDisplay (DisplayLiteralOrVariable (VarIdentifier var) op) =
                                                                            do 
                                                                                cgEnv <- ask
                                                                                cgState <- get
                                                                                let (_,_,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                                let (symTab,_,quadNum) = getCGState cgState
                                                                                case (Map.lookup var symTab) of 
                                                                                    Just (SymbolVar (TypePrimitive prim accessExpression) _ _) ->
                                                                                        case accessExpression of 
                                                                                            [] -> case ((Map.lookup var idTable)) of
                                                                                                    Just address -> do 
                                                                                                                        tell $ [(buildQuadOneAddress quadNum op address)]
                                                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                                            (("[",size,"]") : []) -> case ((Map.lookup (var ++ "[0]") idTable)) of
                                                                                                                        Just address -> genLoopArray address size op

                                                                                            (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup (var ++ "[0][0]") idTable)) of
                                                                                                                Just address -> genLoopMatrix address rows cols 1  op
                                                                                    Just (SymbolVar (TypeClassId prim accessExpression) _ _) ->
                                                                                        case accessExpression of 
                                                                                            [] -> case ((Map.lookup var idTable)) of
                                                                                                    Just address ->
                                                                                                        do 
                                                                                                            tell $ [(buildQuadOneAddress quadNum op address)]
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1}) 
                                                                                            (("[",size,"]") : []) -> case ((Map.lookup (var ++ "[0]") idTable)) of
                                                                                                                        Just address -> genLoopArray address size  op

                                                                                            (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup (var ++ "[0][0]") idTable)) of
                                                                                                                        Just address -> genLoopMatrix address rows cols 1  op
                                                                    genFromDisplay (DisplayObjMem (ObjectMember object attribute) op)  =
                                                                            do 
                                                                                cgEnv <- ask
                                                                                cgState <- get
                                                                                let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                                let (symTab,_,quadNum) = getCGState cgState
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
                                                                                                                                                Just addressAttr ->
                                                                                                                                                        do 
                                                                                                                                                            tell $ [(buildQuadOneAddress quadNum op addressAttr)]
                                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1}) 
                                                                                                            (("[",size,"]") : []) -> case ((Map.lookup object idTable)) of
                                                                                                                                        Just addressObj -> 
                                                                                                                                            case (Map.lookup addressObj objMap) of
                                                                                                                                                Just objTable -> 
                                                                                                                                                    case (Map.lookup (attribute ++ "[0]") objTable) of
                                                                                                                                                        Just addressAttr -> genLoopArray addressAttr size op
                                                                                                            (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup object idTable)) of
                                                                                                                                                        Just addressObj -> 
                                                                                                                                                            case (Map.lookup addressObj objMap) of
                                                                                                                                                                Just objTable -> 
                                                                                                                                                                    case (Map.lookup (attribute ++ "[0][0]") objTable) of
                                                                                                                                                                        Just addressAttr -> genLoopMatrix addressAttr rows cols 1 op
                                                                                                    Just (SymbolVar (TypePrimitive prim accessExpression) _ _) ->
                                                                                                        case accessExpression of 
                                                                                                            [] -> case ((Map.lookup object idTable)) of
                                                                                                                    Just addressObj -> 
                                                                                                                        case (Map.lookup addressObj objMap) of
                                                                                                                            Just objTable -> case (Map.lookup attribute objTable) of
                                                                                                                                                Just addressAttr -> 
                                                                                                                                                        do 
                                                                                                                                                            tell $ [(buildQuadOneAddress quadNum (op) addressAttr)]
                                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                                                            (("[",size,"]") : []) -> case ((Map.lookup object idTable)) of
                                                                                                                                        Just addressObj -> 
                                                                                                                                            case (Map.lookup addressObj objMap) of
                                                                                                                                                Just objTable -> 
                                                                                                                                                    case (Map.lookup (attribute ++ "[0]") objTable) of
                                                                                                                                                        Just addressAttr -> genLoopArray addressAttr size  op
                                                                                                            (("[",rows,"]") : ("[",cols,"]") : []) -> case ((Map.lookup object idTable)) of
                                                                                                                                                        Just addressObj -> 
                                                                                                                                                            case (Map.lookup addressObj objMap) of
                                                                                                                                                                Just objTable -> 
                                                                                                                                                                    case (Map.lookup (attribute ++ "[0][0]") objTable) of
                                                                                                                                                                        Just addressAttr -> genLoopMatrix addressAttr rows cols 1 op
                                                                    genFromDisplay (DisplayVarArrayAccess identifierArray accessExpression op)  =
                                                                        do 
                                                                            cgEnv <- ask
                                                                            cgState <- get
                                                                            let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                            let (symTab,_,quadNum) = getCGState cgState
                                                                            case (Map.lookup identifierArray symTab) of 
                                                                                Just (SymbolVar (TypeClassId classId (("[",size,"]") : []) ) _ _) ->
                                                                                                    case accessExpression of 
                                                                                                        ((ArrayAccessExpression arrayIndexExp) : []) -> 
                                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                                                    Just address ->
                                                                                                                                        case (Map.lookup (identifierArray ++ "[0]") idTable) of
                                                                                                                                            Just addressBase -> 
                                                                                                                                                do 
                                                                                                                                                    (_,quadsIndexAccess) <- listen $ expCodeGen (reduceExpression arrayIndexExp)
                                                                                                                                                    cgState <- get
                                                                                                                                                    let (_,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                                                    tell $ quadsIndexAccess ++ [(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsIndexAccess), address ))] 
                                                                                                                                                    let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsIndexAccess), intGC))]
                                                                                                                                                    let displayObject = ([(buildQuadOneAddress (quadNum + 2) DISPLAY_VALUE_IN_INDEX intGC)])
                                                                                                                                                    let displayOp = ([(buildQuadOneAddress (quadNum + 3) op (-1))])
                                                                                                                                                    tell $ baseAddQuad ++ displayObject ++ displayOp
                                                                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 4}) 

                                                                                Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                    case accessExpression of 
                                                                                        ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp)   : []) -> 
                                                                                            case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                Just addressRowsSize ->
                                                                                                    case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                        Just addressColsSize -> 
                                                                                                                case (Map.lookup (identifierArray ++ "[0][0]") idTable) of
                                                                                                                    Just addressBase ->
                                                                                                                            do 
                                                                                                                                (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression rowsIndexExp)
                                                                                                                                (_,quadsColExp) <- listen $ expCodeGen (reduceExpression colsIndexExp)
                                                                                                                                cgState <- get
                                                                                                                                let (_,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                                let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize))])
                                                                                                                                let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))]
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                let displayObject = ([(buildQuadOneAddress (quadNum + 5) DISPLAY_VALUE_IN_INDEX  (intGC + 2) )])
                                                                                                                                let displayOp = ([(buildQuadOneAddress (quadNum + 6) (op) (-1) )])
                                                                                                                                tell $  boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ displayObject ++ displayOp
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 7}) 
                                                                                Just (SymbolVar (TypePrimitive _ (("[",size,"]") : []) ) _ _) ->
                                                                                                    case accessExpression of 
                                                                                                        ((ArrayAccessExpression arrayIndexExp) : []) -> 
                                                                                                                        case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                                                    Just address ->
                                                                                                                                        case (Map.lookup (identifierArray ++ "[0]") idTable) of
                                                                                                                                            Just addressBase -> 
                                                                                                                                                do 
                                                                                                                                                    (_,quadsIndexAccess) <- listen $ expCodeGen (reduceExpression arrayIndexExp)
                                                                                                                                                    cgState <- get
                                                                                                                                                    let (_,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                                                    tell $ quadsIndexAccess ++ [(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsIndexAccess), address ))] 
                                                                                                                                                    let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsIndexAccess), intGC))]
                                                                                                                                                    let displayObject = ([(buildQuadOneAddress (quadNum + 2) DISPLAY_VALUE_IN_INDEX intGC)])
                                                                                                                                                    let displayOp = ([(buildQuadOneAddress (quadNum + 3) op (-1))])
                                                                                                                                                    tell $ baseAddQuad ++ displayObject ++ displayOp
                                                                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 4}) 

                                                                                Just (SymbolVar (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                    case accessExpression of 
                                                                                        ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp)   : []) -> 
                                                                                            case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                Just addressRowsSize ->
                                                                                                    case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                        Just addressColsSize -> 
                                                                                                                case (Map.lookup (identifierArray ++ "[0][0]") idTable) of
                                                                                                                    Just addressBase ->
                                                                                                                            do
                                                                                                                                (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression rowsIndexExp)
                                                                                                                                (_,quadsColExp) <- listen $ expCodeGen (reduceExpression colsIndexExp)
                                                                                                                                cgState <- get
                                                                                                                                let (_,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                                let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize))])
                                                                                                                                let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))]
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                let displayObject = ([(buildQuadOneAddress (quadNum + 5) DISPLAY_VALUE_IN_INDEX  (intGC + 2) )])
                                                                                                                                let displayOp = ([(buildQuadOneAddress (quadNum + 6) (op) (-1) )])
                                                                                                                                tell $  boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ displayObject ++ displayOp
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 7})
                
                                                                    genFromDisplay (DisplayFunctionCall funcCall op) =
                                                                        do 
                                                                            (_,quadsFuncExp) <- listen $ expCodeGen (reduceExpression (ExpressionFuncCall funcCall))
                                                                            cgState <- get
                                                                            let (_,_,quadNum) = getCGState cgState
                                                                            tell $ [(buildQuadOneAddress quadNum (op) (getLastAddress $ last $ quadsFuncExp))]
                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})

                                                                    genFromDisplay (DisplayLiteralOrVariable (StringLiteral str) op) =
                                                                        do 
                                                                            cgEnv <- ask
                                                                            cgState <- get
                                                                            let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                            let (symTab,_,quadNum) = getCGState cgState
                                                                            case (Map.lookup ("<str>" ++ str) constTable) of
                                                                                Just address -> do
                                                                                                  tell $ [(buildQuadOneAddress quadNum (op) address)]
                                                                                                  modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                    genFromDisplay (DisplayLiteralOrVariable (IntegerLiteral int) op) =
                                                                        do 
                                                                            cgEnv <- ask
                                                                            cgState <- get
                                                                            let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                            let (symTab,_,quadNum) = getCGState cgState
                                                                            case ((Map.lookup ("<int>" ++ show(int)) constTable)) of
                                                                                Just address -> do
                                                                                                  tell $ [(buildQuadOneAddress quadNum (op) address)]
                                                                                                  modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                    genFromDisplay (DisplayLiteralOrVariable (DecimalLiteral dec) op) =
                                                                        do 
                                                                            cgEnv <- ask
                                                                            cgState <- get
                                                                            let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                            let (symTab,_,quadNum) = getCGState cgState
                                                                            case ((Map.lookup ("<dec>" ++ show(dec)) constTable)) of
                                                                                Just address -> do 
                                                                                                    tell $ [(buildQuadOneAddress quadNum (op) address)]
                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                    genFromDisplay (DisplayLiteralOrVariable (BoolLiteral bool) op)  =
                                                                        do 
                                                                            cgEnv <- ask
                                                                            cgState <- get
                                                                            let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                            let (symTab,_,quadNum) = getCGState cgState
                                                                            case ((Map.lookup ("<bool>" ++ show(bool)) constTable)) of
                                                                                Just address -> do 
                                                                                                    tell $ [(buildQuadOneAddress quadNum (op) address)]
                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})

                                                                    

                                                                    genLoopArray :: Address -> Integer  -> Operation -> CG
                                                                    genLoopArray address 0  _ = return ()
                                                                    genLoopArray address limit op = 
                                                                        do 
                                                                            cgState <- get
                                                                            let (_,_,quadNum) = getCGState cgState
                                                                            tell $ ([(buildQuadOneAddress quadNum (op) address)])
                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                            genLoopArray (address + 1) (limit - 1) op

                                                                    genLoopMatrix :: Address -> Integer -> Integer -> Integer -> Operation -> CG
                                                                    genLoopMatrix address 0 cols _ _ = return ()
                                                                    genLoopMatrix address rows cols offset op = 
                                                                         do 
                                                                            genLoopArray address cols (DISPLAY)
                                                                            cgState <- get
                                                                            let (_,_,quadNum) = getCGState cgState
                                                                            tell $ ([(buildQuadOneAddress quadNum (DISPLAY_LINE) (-1))])
                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                            genLoopMatrix (address + cols) (rows - 1) cols (offset + 1) op 

                                                                    -- TODO MARK: Hacer cuadruplos de funciones, y tambien sustituir lo anterior por expresion!
                                                                    -- 
                                                                    -- fillFromDisplay (DisplayVarArrayAccess identifier arrayAccess) literalCounters constantAddressMap =
                                                                    --     fillFromExpression literalCounters constantAddressMap (ExpressionVarArray identifier arrayAccess)  
generateCodeFromStatement _  = return ()

generateCodeFromAssignment :: Assignment -> CG
generateCodeFromAssignment (AssignmentExpression identifier (ExpressionLitVar (VarIdentifier identifier2))) =
        do 
            cgEnv <- ask
            cgState <- get
            let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
            let (symTab,_,quadNum) = getCGState cgState
            case (Map.lookup identifier symTab) of 
                Just (SymbolVar (TypeClassId classIdentifier []) _ _) -> 
                    generateQuadruplesAssignmentClasses identifier identifier2
                Just (SymbolVar (TypeClassId _ (("[",size,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup (identifier2 ++ "[0]") idTable) of 
                                        Just address2 ->  
                                            assignTwoArrays address1 address2 size 

                Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup (identifier2 ++ "[0][0]") idTable) of 
                                        Just address2 -> assignTwoMatrices address1 address2 rows columns
                Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup (identifier2 ++ "[0]") idTable) of 
                                        Just address2 ->  
                                            assignTwoArrays address1 address2 size
                Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup (identifier2 ++ "[0][0]") idTable) of 
                                        Just address2 ->  
                                            assignTwoMatrices address1 address2 rows columns
                _ ->  do
                        (_,quadsExp) <- listen $ expCodeGen (reduceExpression (ExpressionLitVar (VarIdentifier identifier2))) 
                        cgEnv <- ask
                        cgState <- get
                        let (_,_,quadNum) = getCGState cgState
                        -- liftIO $ putStrLn.show $ idTable
                        case (Map.lookup identifier idTable) of
                            Just address -> do 
                                                tell $ ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))])
                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})
generateCodeFromAssignment (AssignmentExpression identifier (ExpressionVarArray identifierArray ((ArrayAccessExpression arrayIndexExp) : [])))  = 
                do 
                    cgEnv <- ask
                    cgState <- get
                    let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
                    let (symTab,_,quadNum) = getCGState cgState
                    case (Map.lookup identifierArray symTab) of 
                        Just (SymbolVar (TypeClassId _ (("[",size,"]") : [] )) _ _) ->
                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                Just address ->
                                    case (Map.lookup (identifierArray ++ "[0]") idTable) of
                                        Just addressBase -> case (Map.lookup identifier idTable) of
                                                                Just addressIdentifier -> 
                                                                    do 
                                                                        (_,quads) <- listen $ expCodeGen arrayIndexExp
                                                                        cgEnvironment <- ask
                                                                        cgState <- get
                                                                        let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnvironment
                                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState 
                                                                        let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                        let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                        let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,addressIdentifier))])
                                                                        tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                        modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                
                        _ -> 
                            do
                                 case (Map.lookup identifier idTable) of
                                        Just addressIdentifier -> 
                                            do 
                                                (_,quadsExp) <- listen $ expCodeGen (reduceExpression (ExpressionVarArray identifierArray ((ArrayAccessExpression arrayIndexExp) : []))) 
                                                cgEnv <- ask
                                                cgState <- get
                                                let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                let (symTab,_,quadNum) = getCGState cgState
                                                let quadAssignment = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT ((getLastAddress $ last $ quadsExp),addressIdentifier))])
                                                tell $ quadAssignment
                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})

generateCodeFromAssignment (AssignmentExpression identifier (ExpressionVarArray identifierArray ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp) : [])))  = 
                do 
                    cgEnv <- ask
                    cgState <- get
                    let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
                    let (symTab,_,quadNum) = getCGState cgState
                    case (Map.lookup identifierArray symTab) of
                        Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                            case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                Just addressRowsSize ->
                                    case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                        Just addressColsSize -> 
                                                case (Map.lookup (identifierArray ++ "[0][0]") idTable) of
                                                    Just addressBase -> 
                                                        case (Map.lookup identifier idTable) of
                                                            Just addressIdentifier -> 
                                                                do 
                                                                    (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression rowsIndexExp)
                                                                    (_,quadsColExp) <- listen $ expCodeGen (reduceExpression colsIndexExp)  
                                                                    cgEnvironment <- ask
                                                                    cgState <- get
                                                                    let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnvironment
                                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                    let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                    let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                    let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                    let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                    let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                    let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,addressIdentifier))])
                                                                    tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                    modify $ \s -> (s { varCounters = (intGC + 3, decGC, strGC, boolGC + 1,objGC)})
                        _ -> 
                            do
                             case (Map.lookup identifier idTable) of
                                    Just addressIdentifier -> 
                                        do 
                                            (_,quadsExp) <- listen $ expCodeGen (reduceExpression (ExpressionVarArray identifierArray ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp) : []))) 
                                            cgEnv <- ask
                                            cgState <- get
                                            let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                            let (symTab,_,quadNum) = getCGState cgState
                                            let quadAssignment = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT ((getLastAddress $ last $ quadsExp),addressIdentifier))])
                                            tell $ quadAssignment
                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})

generateCodeFromAssignment (AssignmentExpression identifier (ExpressionFuncCall functionCall))  = 
                                                            -- Todo mark, a;adir que a un identificador de clase se le pueda asignar una direccion de algun arreglo o matriz
                                                            do 
                                                                cgEnv <- ask
                                                                cgState <- get
                                                                let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
                                                                let (symTab,_,quadNum) = getCGState cgState
                                                                case (Map.lookup identifier symTab) of 
                                                                    Just (SymbolVar (TypeClassId classIdentifier []) _ _) -> 
                                                                        case (Map.lookup (identifier) idTable) of 
                                                                                    Just address -> generateCodeFuncCall functionCall [address]
                                    
                                                                    Just (SymbolVar (TypeClassId _ (("[",size,"]") : []) ) _ _) -> 
                                                                        case (Map.lookup (identifier ++ "[0]") idTable) of 
                                                                                    Just address ->  
                                                                                        do 
                                                                                            let addressesArray = take (fromIntegral $ size) [address..]
                                                                                            generateCodeFuncCall functionCall addressesArray

                                                                    Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                                                                        case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                                                                                    Just address ->  
                                                                                         do 
                                                                                            let addressesArray = take (fromIntegral $ rows * columns) [address..]
                                                                                            generateCodeFuncCall functionCall addressesArray
                                                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) _ _) -> 
                                                                        case (Map.lookup (identifier ++ "[0]") idTable) of 
                                                                                    Just address ->  
                                                                                         do 
                                                                                            let addressesArray = take (fromIntegral $ size) [address..]
                                                                                            generateCodeFuncCall functionCall addressesArray
                                                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                                                                        case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                                                                                    Just address ->  
                                                                                         do 
                                                                                            let addressesArray = take (fromIntegral $ rows * columns) [address..]
                                                                                            generateCodeFuncCall functionCall addressesArray
                                                                    _ ->  do
                                                                            case (Map.lookup (identifier) idTable) of 
                                                                                    Just address -> generateCodeFuncCall functionCall [address]

generateCodeFromAssignment (AssignmentExpression identifier expression)  = 
    -- Todo mark, a;adir que a un identificador de clase se le pueda asignar una direccion de algun arreglo o matriz
    do
        (_,quadsExp) <- listen $ expCodeGen (reduceExpression expression) 
        cgEnv <- ask
        cgState <- get
        let (classSymTab,objMap,idTable,constTable,_,_) = getCGEnvironment cgEnv
        let (symTab,_,quadNum) = getCGState cgState
        case (Map.lookup identifier idTable) of
            Just address -> case (Map.lookup identifier symTab) of
                                Just (SymbolVar (TypePrimitive PrimitiveDouble _) _ _) -> 
                                    do 
                                        let roundQuad = ([(buildQuadOneAddress quadNum (DOUBLE) (getLastAddress $ last $ quadsExp))])
                                        tell $ (roundQuad ++ [(buildQuadrupleTwoAddresses (quadNum + 1) ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))])
                                        modify $ \s -> (s { currentQuadNum = quadNum + 2})
                                Just (SymbolVar (TypePrimitive PrimitiveInt _) _ _) -> 
                                    do 
                                        let int64 = ([(buildQuadOneAddress quadNum (INT_64) (getLastAddress $ last $ quadsExp))])
                                        tell $ (int64 ++ [(buildQuadrupleTwoAddresses (quadNum + 1) ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))])
                                        modify $ \s -> (s { currentQuadNum = quadNum + 2})

                                _ -> do 
                                        tell $ [(buildQuadrupleTwoAddresses quadNum ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))]
                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})


                                              

generateCodeFromAssignment (AssignmentObjectMember identifier (ObjectMember objectIdentifier attrIdentifier))  = 
        do 
            cgEnv <- ask
            cgState <- get
            let (symTab,varCounters,quadNum) = getCGState cgState
            let (classSymTab,objMap,idTable,funcMap,currentModule,_) = getCGEnvironment cgEnv
            case (Map.lookup identifier symTab) of
                Just (SymbolVar (TypeClassId classIdentifier []) _ _) ->
                    case (Map.lookup objectIdentifier idTable) of 
                        Just addressObject -> 
                                case (Map.lookup addressObject objMap) of 
                                    Just idTableObj -> case (Map.lookup attrIdentifier idTableObj) of 
                                                            Just addressAttribute -> 
                                                                case (Map.lookup identifier idTable) of
                                                                    Just address -> 
                                                                        do 
                                                                            tell $ [(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressAttribute , address ))]
                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                    -- generateQuadruplesAssignmentClasses identifier identifier2 quadNum symTab classSymTab varCounters idTable constTable objMap
                Just (SymbolVar (TypePrimitive prim  []) _ _) -> 
                    case (Map.lookup identifier idTable) of 
                                Just address1 ->  
                                     case (Map.lookup objectIdentifier idTable) of 
                                        Just objAddress2 -> 
                                            case (Map.lookup objAddress2 objMap) of
                                                Just idTableObject ->
                                                    case (Map.lookup attrIdentifier idTableObject) of
                                                        Just addressAttribute -> assignTwoArrays address1 addressAttribute 1
                Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup objectIdentifier idTable) of 
                                        Just objAddress2 -> 
                                            case (Map.lookup objAddress2 objMap) of
                                                Just idTableObject ->
                                                    case (Map.lookup (attrIdentifier ++ "[0]") idTableObject) of
                                                        Just addressAttribute -> assignTwoArrays address1 addressAttribute size
                Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup objectIdentifier idTable) of 
                                        Just objAddress2 -> 
                                            case (Map.lookup objAddress2 objMap) of
                                                Just idTableObject ->
                                                    case (Map.lookup (attrIdentifier ++ "[0][0]") idTableObject) of
                                                        Just addressAttribute -> assignTwoMatrices address1 addressAttribute rows columns
                Just (SymbolVar (TypeClassId _ (("[",size,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup objectIdentifier idTable) of 
                                        Just objAddress2 -> 
                                            case (Map.lookup objAddress2 objMap) of
                                                Just idTableObject ->
                                                    case (Map.lookup (attrIdentifier ++ "[0]") idTableObject) of
                                                        Just addressAttribute -> assignTwoArrays address1 addressAttribute size 

                Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",columns,"]") : []) ) _ _) -> 
                    case (Map.lookup (identifier ++ "[0][0]") idTable) of 
                                Just address1 ->  
                                     case (Map.lookup objectIdentifier idTable) of 
                                        Just objAddress2 -> 
                                            case (Map.lookup objAddress2 objMap) of
                                                Just idTableObject ->
                                                    case (Map.lookup (attrIdentifier ++ "[0][0]") idTableObject) of
                                                        Just addressAttribute -> assignTwoMatrices address1 addressAttribute rows columns   
generateCodeFromAssignment (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) =  
                             do 
                                cgEnv <- ask
                                cgState <- get
                                let (symTab,varCounters,quadNum) = getCGState cgState
                                let (classSymTab,objMap,idTable,constTable,funcMap,currentModule) = getCGEnvironment cgEnv  
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
                                                                                                        do 
                                                                                                            (_,quadsExp) <- listen $ expCodeGen (reduceExpression expression)
                                                                                                            cgEnvironment <- ask
                                                                                                            cgState <- get
                                                                                                            let (symTab,varCounters,quadNum) = getCGState cgState  
                                                                                                            tell $ [(buildQuadrupleTwoAddresses quadNum ASSIGNMENT ((getLastAddress $ last $ quadsExp) , attrAddress ))]
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                                        Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic) ->
                                                                                             case (Map.lookup (attrIdentifier ++ "[0]")  objTable) of 
                                                                                                   Just attrAddress -> 
                                                                                                       do 
                                                                                                        let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0]") attrAddress idTable)
                                                                                                        let symbolVarAttr = (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) scp isPublic)
                                                                                                        let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                        (stateAfterAttributesInserted,quadruples) <- liftIO $ execRWST (generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression))
                                                                                                                                                             (setCGEnvironment classSymTab objMap tempIdTable constTable funcMap currentModule)
                                                                                                                                                             (setCGState newSymTab varCounters quadNum)
                                                                                                        let (_,newVarCounters,newQuadNum) = getCGState stateAfterAttributesInserted
                                                                                                        modify $ \s -> (s { currentQuadNum = newQuadNum})
                                                                                                        modify $ \s -> (s { varCounters = newVarCounters})
                                                                                                        tell $ quadruples
                                                                                                       
                                                                                        Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic) ->
                                                                                             case (Map.lookup (attrIdentifier ++ "[0][0]")  objTable) of 
                                                                                                   Just attrAddress -> 
                                                                                                       do 
                                                                                                        let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0][0]") attrAddress idTable)
                                                                                                        let symbolVarAttr = (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic)
                                                                                                        let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                        (stateAfterAttributesInserted,quadruples) <- liftIO $ execRWST (generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression))
                                                                                                                                                             (setCGEnvironment classSymTab objMap tempIdTable constTable funcMap currentModule)
                                                                                                                                                             (setCGState newSymTab varCounters quadNum)
                                                                                                        let (_,newVarCounters,newQuadNum) = getCGState stateAfterAttributesInserted
                                                                                                        modify $ \s -> (s { currentQuadNum = newQuadNum})
                                                                                                        modify $ \s -> (s { varCounters = newVarCounters})
                                                                                                        tell $ quadruples

                                                                                        Just (SymbolVar (TypeClassId c []) scp isPublic) ->
                                                                                             case (Map.lookup attrIdentifier objTable) of 
                                                                                                   Just attrAddress -> 
                                                                                                       do 
                                                                                                            (_,quadsExp) <- listen $ expCodeGen (reduceExpression expression)
                                                                                                            cgEnvironment <- ask
                                                                                                            cgState <- get
                                                                                                            let (symTab,varCounters,quadNum) = getCGState cgState  
                                                                                                            tell $ [(buildQuadrupleTwoAddresses quadNum ASSIGNMENT ((getLastAddress $ last $ quadsExp) , attrAddress ))]
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                                        Just (SymbolVar (TypeClassId c (("[",size,"]") : [])) scp isPublic) ->
                                                                                             case (Map.lookup (attrIdentifier ++ "[0]")  objTable) of 
                                                                                                   Just attrAddress -> 
                                                                                                        do 
                                                                                                            let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0]") attrAddress idTable)
                                                                                                            let symbolVarAttr = (SymbolVar (TypeClassId c (("[",size,"]") : [])) scp isPublic)
                                                                                                            let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                            (stateAfterAttributesInserted,quadruples) <- liftIO $ execRWST (generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression))
                                                                                                                                                                 (setCGEnvironment classSymTab objMap tempIdTable constTable funcMap currentModule)
                                                                                                                                                                 (setCGState newSymTab varCounters quadNum)
                                                                                                            let (_,newVarCounters,newQuadNum) = getCGState stateAfterAttributesInserted
                                                                                                            modify $ \s -> (s { currentQuadNum = newQuadNum})
                                                                                                            modify $ \s -> (s { varCounters = newVarCounters})
                                                                                                            tell $ quadruples
                                                                      
                                                                                        Just (SymbolVar (TypeClassId c (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic) ->
                                                                                             case (Map.lookup (attrIdentifier ++ "[0][0]")  objTable) of 
                                                                                                   Just attrAddress -> 
                                                                                                       do 
                                                                                                        let tempIdTable = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier ++ "[0][0]") attrAddress idTable)
                                                                                                        let symbolVarAttr = (SymbolVar (TypeClassId c (("[",rows,"]") : ("[",cols,"]")  : [])) scp isPublic)
                                                                                                        let newSymTab = (Map.insert (objectIdentifier ++ "." ++ attrIdentifier) symbolVarAttr symTab)
                                                                                                        (stateAfterAttributesInserted,quadruples) <- liftIO $ execRWST (generateCodeFromAssignment (AssignmentExpression (objectIdentifier ++ "." ++ attrIdentifier) expression))
                                                                                                                                                             (setCGEnvironment classSymTab objMap tempIdTable constTable funcMap currentModule)
                                                                                                                                                             (setCGState newSymTab varCounters quadNum)
                                                                                                        let (_,newVarCounters,newQuadNum) = getCGState stateAfterAttributesInserted
                                                                                                        modify $ \s -> (s { currentQuadNum = newQuadNum})
                                                                                                        modify $ \s -> (s { varCounters = newVarCounters})
                                                                                                        tell $ quadruples

generateCodeFromAssignment  (AssignmentArrayExpression identifier ((ArrayAccessExpression arrayIndexExp) : []) (ExpressionLitVar (VarIdentifier id)))  =  
                                                do 
                                                    cgEnv <- ask
                                                    cgState <- get
                                                    let (symTab,varCounters,quadNum) = getCGState cgState
                                                    let (classSymTab,objMap,idTable,constTable,funcMap,currentModule) = getCGEnvironment cgEnv 
                                                    case (Map.lookup identifier symTab) of
                                                        Just (SymbolVar (TypeClassId _ (("[",size,"]") : [] )) _ _) ->
                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                Just address ->
                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                        Just addressBase -> case (Map.lookup id idTable) of
                                                                                                Just addressObject -> 
                                                                                                    do 
                                                                                                        (_,quads) <- listen $ expCodeGen arrayIndexExp
                                                                                                        cgEnvironment <- ask
                                                                                                        cgState <- get
                                                                                                        let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnvironment
                                                                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState 
                                                                                                        let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                        let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                        let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) PUT_INDEX (addressObject,intGC))])
                                                                                                        tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                        modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                        Just (SymbolVar (TypePrimitive _ (("[",size,"]") : [] )) _ _) ->
                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                Just address ->
                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                        Just addressBase -> case (Map.lookup id idTable) of
                                                                                                Just addressIdentifier -> 
                                                                                                    do 
                                                                                                        (_,quads) <- listen $ expCodeGen arrayIndexExp
                                                                                                        cgEnvironment <- ask
                                                                                                        cgState <- get
                                                                                                        let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnvironment
                                                                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState 
                                                                                                        let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                        let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                        let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) PUT_INDEX (addressIdentifier,intGC))])
                                                                                                        tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                        modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
generateCodeFromAssignment  (AssignmentArrayExpression identifier ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp) : []) (ExpressionLitVar (VarIdentifier id))) =  
                                                 do 
                                                    cgEnv <- ask
                                                    cgState <- get
                                                    let (symTab,varCounters,quadNum) = getCGState cgState
                                                    let (classSymTab,objMap,idTable,constTable,funcMap,currentModule) = getCGEnvironment cgEnv 
                                                    case (Map.lookup identifier symTab) of
                                                        Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]")  : [] )) _ _) ->
                                                            case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                Just addressRowsSize ->
                                                                    case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                        Just addressColsSize -> 
                                                                                case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                    Just addressBase ->
                                                                                        case (Map.lookup id idTable) of
                                                                                            Just addressIdentifier ->
                                                                                                do 
                                                                                                    (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression rowsIndexExp)
                                                                                                    (_,quadsColExp) <- listen $ expCodeGen (reduceExpression colsIndexExp)  
                                                                                                    cgEnvironment <- ask
                                                                                                    cgState <- get
                                                                                                    let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnvironment
                                                                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                    let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                    let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                    let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                    let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                    let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                    let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) PUT_INDEX (addressIdentifier,intGC + 2))])
                                                                                                    tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                    modify $ \s -> (s { varCounters = (intGC + 3, decGC, strGC, boolGC,objGC)})
                                                                                                
                                                        _ ->  case (Map.lookup identifier symTab) of
                                                                Just (SymbolVar (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]")  : [] )) _ _) ->
                                                                    case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                        Just addressRowsSize ->
                                                                            case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                Just addressColsSize -> 
                                                                                        case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                            Just addressBase ->
                                                                                                 case (Map.lookup id idTable) of
                                                                                                    Just addressIdentifier ->
                                                                                                        do 
                                                                                                            (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression rowsIndexExp)
                                                                                                            (_,quadsColExp) <- listen $ expCodeGen (reduceExpression colsIndexExp)  
                                                                                                            cgEnvironment <- ask
                                                                                                            cgState <- get
                                                                                                            let (classSymTab,_,idTable,constTable,_,_) = getCGEnvironment cgEnvironment
                                                                                                            let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                            let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                            let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                            let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                            let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                            let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) PUT_INDEX (addressIdentifier,intGC + 2))])
                                                                                                            tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                            modify $ \s -> (s { varCounters = (intGC + 3, decGC, strGC, boolGC,objGC)})                                                  

generateCodeFromAssignment  (AssignmentArrayExpression identifier ((ArrayAccessExpression arrayIndexExp) : []) expression)  =  
                                                    do
                                                        (_,quadsIndexAccess) <- listen $ expCodeGen (reduceExpression arrayIndexExp)
                                                        cgEnv <- ask
                                                        cgState <- get
                                                        let (symTab,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                        let (classSymTab,objMap,idTable,constTable,funcMap,currentModule) = getCGEnvironment cgEnv
                                                        case (Map.lookup identifier symTab) of 
                                                                Just (SymbolVar (TypeClassId classId (("[",size,"]") : []) ) _ _) ->
                                                                    case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                            Just address ->
                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                    Just addressBase -> 
                                                                                                        do 
                                                                                                            let boundQuad = [(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsIndexAccess), address ))]
                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsIndexAccess), intGC))]
                                                                                                            tell $ boundQuad ++ baseAddQuad
                                                                                                            modify $ \s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC)})
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 2})
                                                                                                            (_,quadsExpRight) <- listen $ expCodeGen (reduceExpression expression)
                                                                                                            cgState <- get
                                                                                                            let (symTab,(intGC2,decGC2,strGC2,boolGC2,objGC2),quadNum) = getCGState cgState
                                                                                                            let quadAssignment = [(buildQuadrupleTwoAddresses quadNum PUT_INDEX ((getLastAddress $ last $ quadsExpRight),intGC))]
                                                                                                            tell $ quadAssignment
                                                                                                            modify $ \s -> (s { varCounters = (intGC2,decGC2,strGC2,boolGC2,objGC2)})
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                                Just (SymbolVar (TypePrimitive _ (("[",size,"]") : []) ) _ _) ->
                                                                    case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                            Just address ->
                                                                                case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                    Just addressBase -> 
                                                                                                        do 
                                                                                                            let boundQuad = [(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsIndexAccess), address ))]
                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quadsIndexAccess), intGC))]
                                                                                                            tell $ boundQuad ++ baseAddQuad
                                                                                                            modify $ \s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC)})
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 2})
                                                                                                            (_,quadsExpRight) <- listen $ expCodeGen (reduceExpression expression)
                                                                                                            cgState <- get
                                                                                                            let (symTab,(intGC2,decGC2,strGC2,boolGC2,objGC2),quadNum) = getCGState cgState
                                                                                                            let quadAssignment = [(buildQuadrupleTwoAddresses quadNum PUT_INDEX ((getLastAddress $ last $ quadsExpRight),intGC))]
                                                                                                            tell $ quadAssignment
                                                                                                            modify $ \s -> (s { varCounters = (intGC2,decGC2,strGC2,boolGC2,objGC2)})
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
generateCodeFromAssignment (AssignmentArrayExpression identifier ((ArrayAccessExpression arrayRowsExp) : (ArrayAccessExpression arrayColsExp) : []) expression) =  
                                                    do 
                                                        cgEnv <- ask
                                                        cgState <- get
                                                        let (symTab,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                        let (classSymTab,objMap,idTable,constTable,funcMap,currentModule) = getCGEnvironment cgEnv
                                                        case (Map.lookup identifier symTab) of
                                                            Just (SymbolVar (TypePrimitive _ (("[",rows,"]") : ("[",cols,"]")  : [] )) _ _) ->
                                                                case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                    Just addressRowsSize ->
                                                                        case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                            Just addressColsSize -> 
                                                                                    case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                        Just addressBase -> 
                                                                                            do
                                                                                                (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression arrayRowsExp)
                                                                                                (_,quadsColExp) <- listen $ expCodeGen (reduceExpression arrayColsExp)
                                                                                                cgState <- get
                                                                                                let (_,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                                                let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize))])
                                                                                                let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))]
                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                modify $ \s -> (s { varCounters = (intGC + 3,decGC,strGC,boolGC,objGC)})
                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 5})
                                                                                                tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad
                                                                                                (_,quadsExpRight) <- listen $ expCodeGen (reduceExpression expression)
                                                                                                cgState <- get
                                                                                                let (_,(intGC2,decGC2,strGC2,boolGC2,objGC2),quadNum) = getCGState cgState
                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses quadNum PUT_INDEX ((getLastAddress $ last $ quadsExpRight),intGC + 2))])
                                                                                                tell $ quadAssignment
                                                                                                modify $ \s -> (s { varCounters = (intGC2,decGC2,strGC2,boolGC2,objGC2)})
                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                            Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]")  : [] )) _ _) ->
                                                                case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                    Just addressRowsSize ->
                                                                        case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                            Just addressColsSize -> 
                                                                                    case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                        Just addressBase -> 
                                                                                            do
                                                                                                (_,quadsRowExp) <- listen $ expCodeGen (reduceExpression arrayRowsExp)
                                                                                                (_,quadsColExp) <- listen $ expCodeGen (reduceExpression arrayColsExp)
                                                                                                cgState <- get
                                                                                                let (_,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                                                let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize))])
                                                                                                let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))]
                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                modify $ \s -> (s { varCounters = (intGC + 3,decGC,strGC,boolGC,objGC)})
                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 5})
                                                                                                tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad
                                                                                                (_,quadsExpRight) <- listen $ expCodeGen (reduceExpression expression)
                                                                                                cgState <- get
                                                                                                let (_,(intGC2,decGC2,strGC2,boolGC2,objGC2),quadNum) = getCGState cgState
                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses quadNum PUT_INDEX ((getLastAddress $ last $ quadsExpRight),intGC + 2))])
                                                                                                tell $ quadAssignment
                                                                                                modify $ \s -> (s { varCounters = (intGC2,decGC2,strGC2,boolGC2,objGC2)})
                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})
                             

generateCodeFromAssignment _  = return ()


generateQuadruplesAssignmentClasses :: Identifier -> Identifier ->  CG
generateQuadruplesAssignmentClasses identifier1 identifier2  =
                                        do 
                                            cgEnv <- ask
                                            cgState <- get
                                            let (symTab,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                            let (classSymTab,objMap,idTable,constTable,funcMap,currentModule) = getCGEnvironment cgEnv
                                            case (Map.lookup identifier1 idTable) of 
                                                Just addressReceiver -> 
                                                    case (Map.lookup identifier2 idTable) of
                                                        Just addressGiver -> 
                                                            do 
                                                                let newQuad = [(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressGiver , addressReceiver))]
                                                                tell $ newQuad
                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})

                                                    

assignTwoArrays :: Address -> Address -> Integer -> CG
assignTwoArrays addressReceiver addressGiver 0 = return ()
assignTwoArrays addressReceiver addressGiver limit  =
        do 
            cgState <- get
            let (_,_,quadNum) = getCGState cgState
            let newQuadAssignment = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressGiver , addressReceiver))])
            tell $ newQuadAssignment
            modify $ \s -> (s { currentQuadNum = quadNum + 1})
            assignTwoArrays (addressReceiver + 1) (addressGiver + 1) (limit - 1)

assignTwoMatrices :: Address -> Address -> Integer -> Integer -> CG
assignTwoMatrices addressReceiver addressGiver 0 _  = return ()
assignTwoMatrices addressReceiver addressGiver rows columns  =
        do 
            assignTwoArrays addressReceiver addressGiver columns
            assignTwoMatrices (addressReceiver + columns) (addressGiver + columns) (rows - 1) columns


generateCodeFromVariableStatement :: Variable -> CG
generateCodeFromVariableStatement (VariableAssignmentLiteralOrVariable _ identifier literalOrVariable) = 
                generateCodeFromAssignment ((AssignmentExpression identifier (ExpressionLitVar literalOrVariable)))

generateCodeFromVariableStatement (VariableAssignment1D _ identifier literalOrVariables) = 
        do 
            cgEnv <- ask
            let (_,_,idTable,_,_,_) = getCGEnvironment cgEnv
            case (Map.lookup (identifier ++ "[0]") idTable) of
                Just address -> generateAssignmentArray1D literalOrVariables address
generateCodeFromVariableStatement (VariableAssignment2D _ identifier listLiteralOrVariables) = 
        do 
            cgEnv <- ask
            cgState <- get
            let (_,_,idTable,_,_,_) = getCGEnvironment cgEnv
            let (symTab,_,_) = getCGState cgState
            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                Just address ->
                    case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                generateAssignmentArray2D  listLiteralOrVariables address cols
                        Just (SymbolVar (TypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) -> 
                                generateAssignmentArray2D listLiteralOrVariables address cols
generateCodeFromVariableStatement (VariableAssignmentObject _ identifier (ObjectCreation classIdentifier callParams)) = 
                    generateCodeFromStatement (FunctionCallStatement (FunctionCallObjMem (ObjectMember identifier (classIdentifier ++ "_constructor")) callParams))
generateCodeFromVariableStatement _  = return ()


generateAssignmentArray1D :: [LiteralOrVariable] -> Address -> CG
generateAssignmentArray1D  [] initialAddress   = return ()
generateAssignmentArray1D  (litOrVar : litOrVars) address = 
        do 
            cgEnv <- ask
            cgState <- get
            let (_,_,idTable,_,_,_) = getCGEnvironment cgEnv
            let (symTab,_,quadNum) = getCGState cgState
            case litOrVar of
                (VarIdentifier identifier) ->
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypeClassId _ _) _ _) -> 
                                 case (Map.lookup identifier idTable) of 
                                    Just addressGiver -> 
                                        do 
                                            let newQuad = ([(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressGiver , address ))])
                                            tell $ newQuad
                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                            generateAssignmentArray1D litOrVars (address + 1) 
           
                _ -> do 
                        (_,quadsExp) <- listen $ expCodeGen (reduceExpression (ExpressionLitVar litOrVar))
                        cgState <- get
                        let (_,_,quadNumAfterExp) = getCGState cgState
                        let newQuads = ([(buildQuadrupleTwoAddresses quadNumAfterExp ASSIGNMENT ((getLastAddress $ last $ quadsExp) , address ))])
                        tell $ newQuads
                        modify $ \s -> (s { currentQuadNum = quadNumAfterExp + 1 })
                        generateAssignmentArray1D litOrVars (address + 1) 
        

generateAssignmentArray2D ::  [[LiteralOrVariable]] -> Address -> Integer -> CG
generateAssignmentArray2D  [] _ _ = return ()
generateAssignmentArray2D (litOrVars : listLitOrVars) address cols = 
        do 
            generateAssignmentArray1D litOrVars address
            generateAssignmentArray2D listLitOrVars (address + cols) cols






