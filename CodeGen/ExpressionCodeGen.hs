module ExpressionCodeGen where
import Data.Decimal
import Data.Maybe
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import DataTypes
import CodeGenDataTypes
import MemoryLimits
import Quadruple
import SymbolTable
import ExpressionOptimizer
import ClassSymbolTable
import Expression
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy,findIndex)
import Data.Ord (comparing)

expCodeGen :: Expression -> CG 
expCodeGen (ExpressionLitVar (DecimalLiteral dec)) = do
                                                        cgEnvironment <- ask
                                                        cgState <- get
                                                        let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                        case (Map.lookup ("<dec>" ++ (show (dec))) constTable) of
                                                            Just addressCons -> 
                                                                case (Map.lookup ("<dec>0") constTable) of
                                                                    Just address -> do
                                                                                        tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decGC)))]
                                                                                        modify $ \s -> (s { varCounters = (intGC, decGC + 1, strGC, boolGC,objGC)})
                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})
expCodeGen (ExpressionLitVar (IntegerLiteral int)) = do
                                                        cgEnvironment <- ask
                                                        cgState <- get
                                                        let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                        case (Map.lookup ("<int>" ++ (show (int))) constTable) of
                                                            Just addressCons -> 
                                                                case (Map.lookup ("<int>0") constTable) of
                                                                    Just address -> do 
                                                                                        tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intGC)))]
                                                                                        modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})

expCodeGen (ExpressionLitVar (StringLiteral str)) = do 
                                                        cgEnvironment <- ask
                                                        cgState <- get
                                                        let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                        case (Map.lookup ("<str>" ++ (str)) constTable) of
                                                            Just addressCons -> 
                                                                case (Map.lookup ("<str>") constTable) of
                                                                    Just address -> do 
                                                                                        tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (strGC)))]
                                                                                        modify $ \s -> (s { varCounters = (intGC, decGC, strGC + 1, boolGC,objGC)})
                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})  
expCodeGen (ExpressionLitVar (BoolLiteral bool)) = do 
                                                    cgEnvironment <- ask
                                                    cgState <- get
                                                    let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                    case (Map.lookup ("<bool>" ++ (show (bool))) constTable) of
                                                        Just addressCons -> 
                                                            case (Map.lookup ("<bool>True") constTable) of
                                                                Just address -> do 
                                                                                    tell $ [(buildQuadrupleThreeAddresses quadNum EQ_ (address, addressCons, (boolGC)))]
                                                                                    modify $ \s -> (s { varCounters = (intGC, decGC, strGC, boolGC + 1,objGC)})
                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
expCodeGen (ExpressionLitVar (VarIdentifier id)) = 
                                                do 
                                                    cgEnvironment <- ask
                                                    cgState <- get
                                                    let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                    case (checkDataTypeOfVar (VarIdentifier id) symTab) of
                                                        (TypePrimitive PrimitiveDouble []) -> case (Map.lookup id idTable) of
                                                                                    Just addressCons -> case (Map.lookup ("<dec>0") constTable) of
                                                                                                            Just address -> 
                                                                                                                do 
                                                                                                                    cgEnvironment <- ask
                                                                                                                    cgState <- get
                                                                                                                    let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                    tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decGC)))]
                                                                                                                    modify $ \s -> (s { varCounters = (intGC, decGC + 1, strGC, boolGC,objGC)})
                                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                        (TypePrimitive PrimitiveMoney []) -> case (Map.lookup id idTable) of
                                                                                    Just addressCons -> case (Map.lookup ("<dec>0") constTable) of
                                                                                                            Just address -> 
                                                                                                                do 
                                                                                                                    cgEnvironment <- ask
                                                                                                                    cgState <- get
                                                                                                                    let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                    tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (decGC)))]
                                                                                                                    modify $ \s -> (s { varCounters = (intGC, decGC + 1, strGC, boolGC,objGC)})
                                                                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                        
                                                        (TypePrimitive PrimitiveInt [])-> case (Map.lookup (id) idTable) of
                                                                                   Just addressCons ->  case (Map.lookup ("<int>0") constTable) of
                                                                                                            Just address -> do 
                                                                                                                                cgEnvironment <- ask
                                                                                                                                cgState <- get
                                                                                                                                let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                                                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                                tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intGC)))]
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                        (TypePrimitive PrimitiveInteger []) -> case (Map.lookup (id) idTable) of
                                                                                    Just addressCons ->  case (Map.lookup ("<int>0") constTable) of
                                                                                                            Just address -> do 
                                                                                                                                cgEnvironment <- ask
                                                                                                                                cgState <- get
                                                                                                                                let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                                                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                                tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (intGC)))]
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                        (TypePrimitive PrimitiveString [])  -> case (Map.lookup (id) idTable) of
                                                                                    Just addressCons -> 
                                                                                        case (Map.lookup ("<str>") constTable) of
                                                                                            Just address -> do 
                                                                                                                cgEnvironment <- ask
                                                                                                                cgState <- get
                                                                                                                let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                                tell $ [(buildQuadrupleThreeAddresses quadNum ADD_ (address, addressCons, (strGC)))]
                                                                                                                modify $ \s -> (s { varCounters = (intGC, decGC, strGC + 1, boolGC,objGC)})
                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 1}) 
                                                        (TypePrimitive PrimitiveBool [])  -> case (Map.lookup (id) idTable) of
                                                                                Just addressCons -> 
                                                                                    case (Map.lookup ("<bool>True") constTable) of
                                                                                        Just address -> do 
                                                                                                            cgEnvironment <- ask
                                                                                                            cgState <- get
                                                                                                            let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                            let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                                            tell $ [(buildQuadrupleThreeAddresses quadNum EQ_ (address, addressCons, (boolGC)))]
                                                                                                            modify $ \s -> (s { varCounters = (intGC, decGC, strGC, boolGC + 1,objGC)})
                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                                        (TypeClassId _ [])  -> case (Map.lookup (id) idTable) of
                                                                                Just addressCons -> 
                                                                                    do 
                                                                                        cgEnvironment <- ask
                                                                                        cgState <- get
                                                                                        let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                        let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                                                        tell $ [(buildQuadrupleTwoAddresses quadNum ASSIGNMENT (addressCons, objGC ))]
                                                                                        modify $ \s -> (s { varCounters = (intGC, decGC, strGC, boolGC,objGC + 1)})
                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 1})
expCodeGen(ExpressionFuncCall functionCall) = generateCodeFuncCall functionCall []
                                                                                                            
expCodeGen (ExpressionMult exp1 exp2) = genQuadrupleArithmetic exp1 exp2 MULTIPLY_
expCodeGen (ExpressionDiv exp1 exp2) = genQuadrupleArithmetic exp1 exp2 DIVIDE_
expCodeGen (ExpressionPow exp1 exp2) = genQuadrupleArithmetic exp1 exp2 POWER_
expCodeGen (ExpressionPlus exp1 exp2) = genQuadrupleArithmetic exp1 exp2 ADD_                                                                                                                 
expCodeGen (ExpressionMinus exp1 exp2) = genQuadrupleArithmetic exp1 exp2 SUB_  
expCodeGen (ExpressionPars exp1) = expCodeGen exp1
expCodeGen (ExpressionGreater exp1 exp2) = genQuadrupleRelational exp1 exp2 GT_
expCodeGen (ExpressionLower exp1 exp2) = genQuadrupleRelational exp1 exp2 LT_
expCodeGen (ExpressionGreaterEq exp1 exp2) = genQuadrupleRelational exp1 exp2 GTEQ_
expCodeGen (ExpressionLowerEq exp1 exp2) = genQuadrupleRelational exp1 exp2 LTEQ_
expCodeGen (ExpressionEqEq exp1 exp2) = genQuadrupleRelational exp1 exp2 EQ_
expCodeGen (ExpressionNotEq exp1 exp2) = genQuadrupleRelational exp1 exp2 NOTEQ_
expCodeGen (ExpressionAnd exp1 exp2) = genQuadrupleRelational exp1 exp2 AND_
expCodeGen (ExpressionOr exp1 exp2) = genQuadrupleRelational exp1 exp2 OR_
expCodeGen (ExpressionMod exp1 exp2) = genQuadrupleArithmetic exp1 exp2 MOD_  
expCodeGen (ExpressionNot exp1) = 
                               do 
                                expCodeGen exp1
                                cgEnvironment <- ask
                                cgState <- get
                                let (_,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                tell $ [(buildQuadrupleTwoAddresses quadNum NOT_ (boolGC, boolGC))]
                                modify $ \s -> (s { varCounters = (intGC, decGC, strGC, boolGC + 1,objGC)})
                                modify $ \s -> (s { currentQuadNum = quadNum + 1})

expCodeGen (ExpressionNeg exp1) = 
                                do 
                                    cgEnvironment <- ask
                                    cgState <- get
                                    let (classSymTab,_,idTable,constTable,_,_,aMap) = getCGEnvironment cgEnvironment
                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                    let typeExp1 = (expressionTypeChecker (-100000000000000) exp1 symTab classSymTab aMap)
                                    expCodeGen exp1
                                    cgEnvironment <- ask
                                    cgState <- get
                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                    case (typeExp1) of
                                        Right (TypePrimitive PrimitiveDouble []) -> do 
                                                                    tell $ [(buildQuadrupleTwoAddresses quadNum NEG_ (decGC - 1, decGC))]
                                                                    modify $ \s -> (s { varCounters = (intGC, decGC + 1, strGC, boolGC,objGC)})
                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                        Right (TypePrimitive PrimitiveMoney []) -> do 
                                                                    tell $ [(buildQuadrupleTwoAddresses quadNum NEG_ (decGC - 1, decGC))]
                                                                    modify $ \s -> (s { varCounters = (intGC, decGC + 1, strGC, boolGC,objGC)})
                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                        Right (TypePrimitive PrimitiveInt []) ->  do 
                                                                    tell $ [(buildQuadrupleTwoAddresses quadNum NEG_ (intGC - 1, intGC))]
                                                                    modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
                                        Right (TypePrimitive PrimitiveInteger []) -> do 
                                                                    tell $ [(buildQuadrupleTwoAddresses quadNum NEG_ (intGC - 1, intGC))]
                                                                    modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC)})
                                                                    modify $ \s -> (s { currentQuadNum = quadNum + 1})
expCodeGen (ExpressionVarArray identifier ((ArrayAccessExpression exp1) : [])) = do 
                                                                                    (_,quads) <- listen $ expCodeGen exp1
                                                                                    cgEnvironment <- ask
                                                                                    cgState <- get
                                                                                    let (classSymTab,_,idTable,constTable,_,_,aMap) = getCGEnvironment cgEnvironment
                                                                                    let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState 
                                                                                    case (Map.lookup identifier symTab) of
                                                                                        Just (SymbolVar (TypePrimitive PrimitiveString (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,strGC))])
                                                                                                                                tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC + 1, boolGC,objGC)})
                                                                                        Just (SymbolVar (TypePrimitive PrimitiveMoney (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,decGC))])
                                                                                                                                tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC + 1, strGC, boolGC,objGC)})
                                                                                        Just (SymbolVar (TypePrimitive PrimitiveDouble (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,decGC))])
                                                                                                                                tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC + 1, strGC, boolGC,objGC)})
                                                                                        Just (SymbolVar (TypePrimitive PrimitiveInteger (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,intGC + 1))])
                                                                                                                                tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 2, decGC, strGC, boolGC,objGC)})
                                                                                        Just (SymbolVar (TypePrimitive PrimitiveInt (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,intGC + 1))])
                                                                                                                                tell $  boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 2, decGC, strGC, boolGC,objGC)})
                                                                                        Just (SymbolVar (TypePrimitive PrimitiveBool (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,boolGC))])
                                                                                                                                tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC + 1,objGC)})
                                                                                        Just (SymbolVar (TypeClassId _ (("[",size,"]") : [] )) _ _) ->
                                                                                            case (Map.lookup ("<int>" ++ (show $ size)) constTable) of
                                                                                                Just address ->
                                                                                                    case (Map.lookup (identifier ++ "[0]") idTable) of
                                                                                                        Just addressBase -> do 
                                                                                                                                let boundQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quads), address ))])
                                                                                                                                let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 1) ADD_INDEX (addressBase, (getLastAddress $ last $ quads), intGC))]
                                                                                                                                let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 2) ACCESS_INDEX (intGC,objGC))])
                                                                                                                                tell $ boundQuad ++ baseAddQuad ++ quadAssignment
                                                                                                                                modify $ \s -> (s { currentQuadNum = quadNum + 3})
                                                                                                                                modify $ \s -> (s { varCounters = (intGC + 1, decGC, strGC, boolGC,objGC + 1)})

expCodeGen (ExpressionVarArray identifier ((ArrayAccessExpression rowsIndexExp) : (ArrayAccessExpression colsIndexExp) : [])) = 
                                                                                            do
                                                                                                (_,quadsRowExp) <- listen $ expCodeGen rowsIndexExp
                                                                                                (_,quadsColExp) <- listen $ expCodeGen colsIndexExp  
                                                                                                cgEnvironment <- ask
                                                                                                cgState <- get
                                                                                                let (classSymTab,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                                                                let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState 
                                                                                                case (Map.lookup identifier symTab) of
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveString (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                    do
                                                                                                                                        let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                        let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                        let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                        let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                        let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                        let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,strGC))])
                                                                                                                                        tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                                                        modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                                                        modify $ \s -> (s { varCounters = (intGC + 3, decGC, strGC + 1, boolGC,objGC)})
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveMoney (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        do 
                                                                                                                                            let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                            let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                            let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                            let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                            let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,decGC))])
                                                                                                                                            tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                                                            modify $ \s -> (s { varCounters = (intGC + 3, decGC + 1, strGC, boolGC,objGC)})
                                                                                                                                      
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveDouble (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        do
                                                                                                                                            let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                            let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                            let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                            let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                            let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,decGC))])
                                                                                                                                            tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                                                            modify $ \s -> (s { varCounters = (intGC + 3, decGC + 1, strGC, boolGC,objGC)})
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveInteger (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase ->
                                                                                                                                        do 
                                                                                                                                            let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                            let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                            let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                            let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                            let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,intGC + 3))])
                                                                                                                                            tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                                                            modify $ \s -> (s { varCounters = (intGC + 4, decGC, strGC, boolGC,objGC)}) 
                                                                                                      
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveInt (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        do 
                                                                                                                                            let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                            let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                            let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                            let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                            let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,intGC + 3))])
                                                                                                                                            tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                                                            modify $ \s -> (s { varCounters = (intGC + 4, decGC, strGC, boolGC,objGC)})
                                                                                                    Just (SymbolVar (TypePrimitive PrimitiveBool (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                        case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                            Just addressRowsSize ->
                                                                                                                case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                                    Just addressColsSize -> 
                                                                                                                            case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                                                Just addressBase -> 
                                                                                                                                        do 
                                                                                                                                            let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                                                            let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                                                            let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                                                            let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                                                            let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                                                            let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,boolGC))])
                                                                                                                                            tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                                                            modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                                                            modify $ \s -> (s { varCounters = (intGC + 3, decGC, strGC, boolGC + 1,objGC)})
                                                                                                    -- Just (SymbolVar (TypeClassIdTypeClassId _ (("[",rows,"]") : ("[",cols,"]") : [] )) _ _) ->
                                                                                                    --     case (Map.lookup ("<int>" ++ (show $ rows)) constTable) of
                                                                                                    --         Just addressRowsSize ->
                                                                                                    --             case (Map.lookup ("<int>" ++ (show $ cols)) constTable) of
                                                                                                    --                 Just addressColsSize -> 
                                                                                                    --                         case (Map.lookup (identifier ++ "[0][0]") idTable) of
                                                                                                    --                             Just addressBase -> 
                                                                                                    --                                     do 
                                                                                                    --                                         let boundRowQuad = ([(buildQuadrupleTwoAddresses quadNum BOUNDS ((getLastAddress $ last $ quadsRowExp), addressRowsSize ))])
                                                                                                    --                                         let boundColQuad = ([(buildQuadrupleTwoAddresses (quadNum + 1) BOUNDS ((getLastAddress $ last $ quadsColExp), addressColsSize ))])
                                                                                                    --                                         let positionRow = [(buildQuadrupleThreeAddresses (quadNum + 2) MULTIPLY_ ((getLastAddress $ last $ quadsRowExp), addressColsSize, intGC))] 
                                                                                                    --                                         let positionCol = [(buildQuadrupleThreeAddresses (quadNum + 3) ADD_ (intGC, (getLastAddress $ last $ quadsColExp), (intGC + 1) ))] 
                                                                                                    --                                         let baseAddQuad = [(buildQuadrupleThreeAddresses (quadNum + 4) ADD_INDEX (addressBase, (intGC + 1), intGC + 2 ))]
                                                                                                    --                                         let quadAssignment = ([(buildQuadrupleTwoAddresses (quadNum + 5) ACCESS_INDEX (intGC + 2,objGC))])
                                                                                                    --                                         tell $ boundRowQuad ++ boundColQuad ++ positionRow ++ positionCol ++ baseAddQuad ++ quadAssignment
                                                                                                    --                                         modify $ \s -> (s { currentQuadNum = quadNum + 6})
                                                                                                    --                                         modify $ \s -> (s { varCounters = (intGC + 3, decGC, strGC, boolGC,objGC)})

genQuadrupleArithmetic ::  Expression -> Expression -> Operation -> CG
genQuadrupleArithmetic exp1 exp2 op =
                    do 
                        cgEnvironment <- ask
                        cgState <- get
                        let (classSymTab,_,_,_,_,_,aMap) = getCGEnvironment cgEnvironment
                        let (symTab,_,_) = getCGState cgState
                        let typeExp1 = (expressionTypeChecker (-100000000000000) exp1 symTab classSymTab aMap)
                        let typeExp2 = (expressionTypeChecker (-100000000000000) exp2 symTab classSymTab aMap)
                        (_,quadsExp1) <- listen $ expCodeGen exp1
                        cgState <- get
                        let (_,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                        (_,quadsExp2) <- listen $ expCodeGen exp2  
                        cgState <- get
                        let (_,(intGC2, decGC2, strGC2, boolGC2,objGC2),quadNum2) = getCGState cgState
                        if (typeExp2 == typeExp1) then do
                            case (typeExp1) of 
                                Right (TypePrimitive PrimitiveDouble []) -> do 
                                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, (decGC2)))]
                                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                            modify $ \s -> (s { varCounters = (intGC2, decGC2 + 1, strGC2, boolGC2,objGC2)})
                                Right (TypePrimitive PrimitiveMoney []) -> do 
                                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, (decGC2)))]
                                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                            modify $ \s -> (s { varCounters = (intGC2, decGC2 + 1, strGC2, boolGC2,objGC2)})
                                Right (TypePrimitive PrimitiveInt []) -> do 
                                                        tell $ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, (intGC2)))]
                                                        modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                        modify $ \s -> (s { varCounters = (intGC2 + 1, decGC2, strGC2, boolGC2,objGC2)})
                                Right (TypePrimitive PrimitiveInteger []) -> do 
                                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, (intGC2)))]
                                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                            modify $ \s -> (s { varCounters = (intGC2 + 1, decGC2, strGC2, boolGC2,objGC2)})
                                Right (TypePrimitive PrimitiveString []) -> do 
                                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (strGC - 1, strGC2 - 1, (strGC2)))]
                                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2 + 1, boolGC2,objGC2)})
                            -- Aqui hay un else porque nuestro lenguaje permite hacer mezclas de tipos enteros y decimales en operaciones
                            else do
                                case (typeExp1) of 
                                    Right (TypePrimitive PrimitiveDouble []) -> do 
                                                                tell $ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, intGC2 - 1, (decGC2)))]
                                                                modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                                modify $ \s -> (s { varCounters = (intGC2, decGC2 + 1, strGC2, boolGC2,objGC2)})
                                    Right (TypePrimitive PrimitiveMoney []) -> do 
                                                                tell $  [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, intGC2 - 1, (decGC2)))]
                                                                modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                                modify $ \s -> (s { varCounters = (intGC2, decGC2 + 1, strGC2, boolGC2,objGC2)})
                                    Right (TypePrimitive PrimitiveInt []) -> do 
                                                                tell $ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, decGC2 - 1, (decGC2)))]
                                                                modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                                modify $ \s -> (s { varCounters = (intGC2, decGC2 + 1, strGC2, boolGC2,objGC2)})
                                    Right (TypePrimitive PrimitiveInteger []) -> do 
                                                                tell $ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, decGC2 - 1, (decGC2)))]
                                                                modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                                                modify $ \s -> (s { varCounters = (intGC2, decGC2 + 1, strGC2, boolGC2,objGC2)})



genQuadrupleRelational :: Expression -> Expression -> Operation -> CG
genQuadrupleRelational exp1 exp2 op =
    do 
        cgEnvironment <- ask
        cgState <- get
        let (classSymTab,_,_,_,_,_,aMap) = getCGEnvironment cgEnvironment
        let (symTab,_,_) = getCGState cgState
        let typeExp1 = (expressionTypeChecker (-100000000000000) exp1 symTab classSymTab aMap)
        let typeExp2 = (expressionTypeChecker (-100000000000000) exp2 symTab classSymTab aMap)
        (_,quadsExp1) <- listen $ expCodeGen exp1
        cgState <- get
        let (_,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
        (_,quadsExp2) <- listen $ expCodeGen exp2  
        cgState <- get
        let (_,(intGC2, decGC2, strGC2, boolGC2,objGC2),quadNum2) = getCGState cgState
        if (typeExp2 == typeExp1) then do 
            case (typeExp1) of 
                Right (TypePrimitive PrimitiveDouble []) -> do
                                            tell $  [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})
                     -- ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, boolGC2))], quadNum2 + 1)
                Right (TypePrimitive PrimitiveMoney []) -> do
                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (decGC - 1, decGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})
                Right (TypePrimitive PrimitiveInt []) -> do
                                            tell $  [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})

                -- ((intGC2, decGC2, strGC2, boolGC2 + 1,objGC2), quad1 ++ quad2 ++ [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, boolGC2))], quadNum2 + 1)
                Right (TypePrimitive PrimitiveInteger []) -> do
                                            tell $  [(buildQuadrupleThreeAddresses quadNum2 op (intGC - 1, intGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})

                Right (TypePrimitive PrimitiveString []) -> do
                                            tell $  [(buildQuadrupleThreeAddresses quadNum2 op (strGC - 1, strGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})
                Right (TypePrimitive PrimitiveBool []) -> do
                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (boolGC - 1, boolGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})
                _ -> do
                                            tell $ [(buildQuadrupleThreeAddresses quadNum2 op (objGC - 1, objGC2 - 1, (boolGC2)))]
                                            modify $ \s -> (s { currentQuadNum = quadNum2 + 1})
                                            modify $ \s -> (s { varCounters = (intGC2, decGC2, strGC2, boolGC2 + 1,objGC2)})
        else do 
            liftIO $ putStrLn "ERROR Typechecker ERROR WENT THROUGH CODEGEN" 
            return ()


generateCodeFromCallParams :: [Address] -> [Type] -> [Expression] -> CodeGen [(Address,Address)]
generateCodeFromCallParams [] [] [] = return []
generateCodeFromCallParams  addressesInFunction  (t : ts) (e : es) =
                            do 
                                cgEnv <- ask
                                cgState <- get
                                let (_,_,idTable,_,funcMap,currentModule,_) = getCGEnvironment cgEnv
                                let (symTab,_,quadNum) = getCGState cgState 
                                case t of
                                    (TypeClassId classId accessExpression) ->
                                        case accessExpression of 
                                            [] -> 
                                                case e of 
                                                    (ExpressionLitVar (VarIdentifier identifierExp)) ->
                                                        case (Map.lookup identifierExp idTable) of 
                                                            Just attrObject ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [attrObject] [a]
                                                                                    generateCodeFromCallParams as ts es

                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads

                                                                                    return $ paramsZip ++ quadsParams
                                            (("[",size,"]") : []) -> case e of 
                                                                        (ExpressionLitVar (VarIdentifier identifierExp)) ->
                                                                            case (Map.lookup (identifierExp ++ "[0]") idTable) of 
                                                                                Just attrBase ->
                                                                                            do 
                                                                                                let addressesC = [attrBase..] 
                                                                                                let addressesCurrent = take (fromIntegral size) addressesC
                                                                                                let addressesFunction = take (fromIntegral size) addressesInFunction
                                                                                                let paramsZip = zip addressesCurrent addressesFunction

                                                                                                (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams (drop (fromIntegral size) addressesInFunction) ts es) cgEnv cgState
                                                            
                                                                                                modify $ (\s -> newCGState)
                                                                                                tell $ quads

                                                                                                return (paramsZip ++ quadsParams)
                                            (("[",rows,"]") : ("[",cols,"]") : []) -> case e of 
                                                                        (ExpressionLitVar (VarIdentifier identifierExp)) ->
                                                                            case (Map.lookup (identifierExp ++ "[0][0]") idTable) of 
                                                                                Just attrBase -> 
                                                                                            do 

                                                                                                let addressesC = [attrBase..] 
                                                                                                let addressesCurrent = take (fromIntegral $ rows * cols) addressesC
                                                                                                let addressesFunction = take (fromIntegral $ rows * cols) addressesInFunction
                                                                                                let paramsZip = zip addressesCurrent addressesFunction
                                                                                                (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams (drop (fromIntegral $ rows * cols) addressesInFunction) ts es) cgEnv cgState
                                                            
                                                                                                modify $ (\s -> newCGState)
                                                                                                tell $ quads

                                                                                                return (paramsZip ++ quadsParams)
                                    (TypePrimitive prim accessExpression) ->
                                        case accessExpression of 
                                            [] -> case e of 
                                                    (ExpressionLitVar (VarIdentifier identifierExp)) ->
                                                        case (Map.lookup identifierExp idTable) of 
                                                            Just attrObject ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [attrObject] [a]
                                                                                    -- generateCodeFromCallParams as ts es

                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)
                                                    -- A todo lo demas, generamos la expresión resultante
                                                    _ -> do 
                                                            (_,quads) <- listen $ expCodeGen e
                                                            -- tell $ quads
                                                            cgEnvironment <- ask
                                                            cgState <- get
                                                            let (classSymTab,_,idTable,constTable,_,_,_) = getCGEnvironment cgEnvironment
                                                            let (symTab,(intGC, decGC, strGC, boolGC,objGC),quadNum) = getCGState cgState
                                                            case prim of 
                                                                PrimitiveDouble ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [(decGC - 1)] [a]
                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)

                                                                PrimitiveMoney ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [(decGC - 1)] [a]
                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)
                                                                PrimitiveInteger ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [(intGC - 1)] [a]
                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)
                                                                PrimitiveInt ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [(intGC - 1)] [a]
                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)
                                                                PrimitiveString ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [(strGC - 1)] [a]
                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)
                                                                PrimitiveBool ->
                                                                                do 
                                                                                    let (a : as) = addressesInFunction
                                                                                    let paramsZip = zip [(boolGC - 1)] [a]
                                                                                    (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  as ts es) cgEnv cgState
                                                            
                                                                                    modify $ (\s -> newCGState)
                                                                                    tell $ quads
                                                                                    return (paramsZip ++ quadsParams)
                                            (("[",size,"]") : []) -> case e of 
                                                                        (ExpressionLitVar (VarIdentifier identifierExp)) ->
                                                                            case (Map.lookup (identifierExp ++ "[0]") idTable) of 
                                                                                Just attrBase ->
                                                                                            do 
                                                                                                let addressesC = [attrBase..] 
                                                                                                let addressesCurrent = take (fromIntegral size) addressesC
                                                                                                let addressesFunction = take (fromIntegral size) addressesInFunction
                                                                                                let paramsZip = zip addressesCurrent addressesFunction

                                                                                                (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams (drop (fromIntegral size) addressesInFunction) ts es) cgEnv cgState
                                                            
                                                                                                modify $ (\s -> newCGState)
                                                                                                tell $ quads
                                                                                                return (paramsZip ++ quadsParams)
                                            (("[",rows,"]") : ("[",cols,"]") : []) -> case e of 
                                                                                        (ExpressionLitVar (VarIdentifier identifierExp)) ->
                                                                                            case (Map.lookup (identifierExp ++ "[0][0]") idTable) of 
                                                                                                Just attrBase -> 
                                                                                                            do 
                                                                                                                let addressesC = [attrBase..] 
                                                                                                                let addressesCurrent = take (fromIntegral $ rows * cols) addressesC
                                                                                                                let addressesFunction = take (fromIntegral $ rows * cols) addressesInFunction
                                                                                                                let paramsZip = zip addressesCurrent addressesFunction
                                                                                                                (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams (drop (fromIntegral $ rows * cols) addressesInFunction) ts es) cgEnv cgState
                                                                            
                                                                                                                modify $ (\s -> newCGState)
                                                                                                                tell $ quads
                                                                                                                return (paramsZip ++ quadsParams)
                                    






getAddressesOfAttributesInClassFunction :: String -> IdentifierAddressMap -> ClassSymbolTable -> [Address]
getAddressesOfAttributesInClassFunction currentModule idMapOfFunc classSymTab = 
                                                                    let identifierAttributes = getAttributesOfCurrentClass currentModule classSymTab
                                                                    in (map (\f -> case (Map.lookup f idMapOfFunc) of 
                                                                                    Just addressIdentifier -> addressIdentifier) identifierAttributes)
getAddressesOfAttributesInObjMap :: Identifier -> ClassIdentifier  -> IdentifierAddressMap -> ObjectAddressMap -> ClassSymbolTable -> [Address]
getAddressesOfAttributesInObjMap objIdentifier classIdentifier idMap objMap classSymTab = 
                                                                    let identifierAttributes = getAttributesOfCurrentClass classIdentifier classSymTab
                                                                    in case (Map.lookup objIdentifier idMap) of 
                                                                        Just addressObj -> case (Map.lookup addressObj objMap) of 
                                                                                                Just idTableObj -> 
                                                                                                    (map (\f -> case (Map.lookup f idTableObj) of 
                                                                                                                    Just addressIdentifier -> addressIdentifier) 
                                                                                                    identifierAttributes)

                                                                    






generateCodeFuncCall :: FunctionCall -> [Address] -> CG
generateCodeFuncCall (FunctionCallVar funcIdentifier callParams) addressesToSet =
                            do 
                                cgEnv <- ask
                                cgState <- get
                                let (classSymTab,_,idTable,_,funcMap,currentModule,_) = getCGEnvironment cgEnv
                                let (symTab,_,_) = getCGState cgState
                                -- liftIO $ putStrLn.ppShow $ funcMap
                                -- liftIO $ putStrLn.ppShow $ currentModule ++ funcIdentifier

                                case (Map.lookup (currentModule ++ funcIdentifier) funcMap) of 
                                    Just (FunctionData _ paramsAddressesFunc idMapFunc objMapFunc) -> 
                                            do 
                                                case (Map.lookup funcIdentifier symTab) of 
                                                    Just (SymbolFunction params returnType _ _ _ _ _) ->
                                                        do 
                                                            let typesParams = (map (\f -> fst f) params)
                                                            let expressionsParams = (map (\f -> 
                                                                                                let (ParamsExpression exp) = f 
                                                                                                 in (reduceExpression exp)
                                                                                          ) 
                                                                                    callParams)
                                                            (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  paramsAddressesFunc typesParams expressionsParams) cgEnv cgState
                                                            tell $ quads
                                                            
                                                            -- Actualizamos el estado
                                                            modify $ (\s -> newCGState)
                                                            cgState <- get
                                                            let (symTab,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                            -- Mark todo: aqui debemos sacar en que módulo se está actualmente
                                                            -- liftIO $ putStrLn.ppShow $ funcMap
                                                            -- liftIO $ putStrLn.show $ currentModule
                                                            -- liftIO $ putStrLn.show $ symTab
                                                            let addressesAttributesOfCaller = getAddressesOfAttributesInClassFunction currentModule idTable classSymTab
                                                            let addressesAttributesOfCallingFunction = getAddressesOfAttributesInClassFunction currentModule idMapFunc classSymTab
                                                            let zippedAddresses = zip addressesAttributesOfCaller addressesAttributesOfCallingFunction
                                                            let funcCallQuad = buildFuncCall quadNum zippedAddresses quadsParams (currentModule ++ funcIdentifier)
                                                            tell $ [funcCallQuad]
                                                            case addressesToSet of 
                                                                [] -> do 
                                

                                                                    case returnType of 
                                                                        Just (TypePrimitive PrimitiveMoney []) -> 
                                                                                       do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) [decGC]
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC)})) 
                                                                        Just (TypePrimitive PrimitiveDouble []) -> 
                                                                                       do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) [decGC]
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC)})) 
                                                                        Just (TypePrimitive PrimitiveInteger []) -> 
                                                                                       do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) [intGC]
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                        modify $ (\s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC)}))
                                                                        Just (TypePrimitive PrimitiveInt []) -> 
                                                                                       do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) [intGC]
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                        modify $ (\s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC)})) 
                                                                        Just (TypePrimitive PrimitiveString []) -> 
                                                                                       do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) [strGC]
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC,strGC + 1,boolGC,objGC)})) 
                                                                        Just (TypePrimitive PrimitiveBool []) -> 
                                                                                       do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) [strGC]
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC,strGC,boolGC + 1,objGC)}))
                                                                        _ -> do 
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 1}))

                                                                _ -> 
                                                                    do 
                                                                        let ret = buildReturnAssignment (quadNum + 1) addressesToSet
                                                                        tell $ [ret]
                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                            

generateCodeFuncCall (FunctionCallObjMem (ObjectMember identifier funcIdentifier) callParams) addressesToSet =
                            do 
                                cgEnv <- ask
                                cgState <- get
                                let (classSymTab,objMap,idTable,_,funcMap,_,_) = getCGEnvironment cgEnv
                                let (symTab,_,_) = getCGState cgState
                                
                                -- liftIO $ putStrLn.ppShow $ currentModule ++ funcIdentifier

                                case (Map.lookup identifier symTab) of 
                                    Just (SymbolVar (TypeClassId classId []) _ _) ->
                                        case (Map.lookup (("_" ++ classId ++ "_") ++ funcIdentifier) funcMap) of 
                                            Just (FunctionData _ paramsAddressesFunc idMapFunc objMapFunc) -> 
                                                    do 
                                                        case (Map.lookup classId classSymTab) of 
                                                            Just symTabOfClass ->
                                                                case (Map.lookup funcIdentifier symTabOfClass) of 
                                                                    Just (SymbolFunction params returnType _ _ _ _ _) ->
                                                                        do 
                                                                            let typesParams = (map (\f -> fst f) params)
                                                                            let expressionsParams = (map (\f -> 
                                                                                                                let (ParamsExpression exp) = f 
                                                                                                                 in exp) 
                                                                                                    callParams)
                                                                            (quadsParams,newCGState, quads) <- liftIO $ runRWST (generateCodeFromCallParams  paramsAddressesFunc typesParams expressionsParams) cgEnv cgState
                                                                            tell $ quads
                                                                            
                                                                            -- Actualizamos el estado
                                                                            modify $ (\s -> newCGState)
                                                                            cgState <- get
                                                                            let (symTab,(intGC,decGC,strGC,boolGC,objGC),quadNum) = getCGState cgState
                                                                            
                                                                            let addressesAttributesOfCaller = getAddressesOfAttributesInObjMap identifier ("_" ++ classId ++ "_") idTable objMap classSymTab
                                                                            let addressesAttributesOfCallingFunction = getAddressesOfAttributesInClassFunction ("_" ++ classId ++ "_") idMapFunc classSymTab
                                                                            let zippedAddresses = zip addressesAttributesOfCaller addressesAttributesOfCallingFunction
                                                                            let funcCallQuad = buildFuncCall quadNum zippedAddresses quadsParams (("_" ++ classId ++ "_") ++ funcIdentifier)
                                                                            tell $ [funcCallQuad]
                                                                            case addressesToSet of 
                                                                                [] -> do 
                                                                                    case returnType of 
                                                                                        Just (TypePrimitive PrimitiveMoney []) -> 
                                                                                                       do 
                                                                                                        let ret = buildReturnAssignment (quadNum + 1) [decGC]
                                                                                                        tell $ [ret]
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC)})) 
                                                                                        Just (TypePrimitive PrimitiveDouble []) -> 
                                                                                                       do 
                                                                                                        let ret = buildReturnAssignment (quadNum + 1) [decGC]
                                                                                                        tell $ [ret]
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC + 1,strGC,boolGC,objGC)})) 
                                                                                        Just (TypePrimitive PrimitiveInteger []) -> 
                                                                                                       do 
                                                                                                        let ret = buildReturnAssignment (quadNum + 1) [intGC]
                                                                                                        tell $ [ret]
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                                        modify $ (\s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC)}))
                                                                                        Just (TypePrimitive PrimitiveInt []) -> 
                                                                                                       do 
                                                                                                        let ret = buildReturnAssignment (quadNum + 1) [intGC]
                                                                                                        tell $ [ret]
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                                        modify $ (\s -> (s { varCounters = (intGC + 1,decGC,strGC,boolGC,objGC)})) 
                                                                                        Just (TypePrimitive PrimitiveString []) -> 
                                                                                                       do 
                                                                                                        let ret = buildReturnAssignment (quadNum + 1) [strGC]
                                                                                                        tell $ [ret]
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC,strGC + 1,boolGC,objGC)})) 
                                                                                        Just (TypePrimitive PrimitiveBool []) -> 
                                                                                                       do 
                                                                                                        let ret = buildReturnAssignment (quadNum + 1) [strGC]
                                                                                                        tell $ [ret]
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))
                                                                                                        modify $ (\s -> (s { varCounters = (intGC,decGC,strGC,boolGC + 1,objGC)}))
                                                                                        _ -> do 
                                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 1}))

                                                                                _ -> 
                                                                                    do 
                                                                                        let ret = buildReturnAssignment (quadNum + 1) addressesToSet
                                                                                        tell $ [ret]
                                                                                        modify $ (\s -> (s { currentQuadNum = quadNum + 2}))


                                    





