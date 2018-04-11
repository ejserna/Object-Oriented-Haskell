module TypeChecker where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Control.Monad.Except
import SymbolTable
import ClassSymbolTable
import Expression
import MemoryAllocator
import Data.Either.Combinators
import System.Exit
-- import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as Map
import  System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)
import Data.List (intercalate, maximumBy, union)
import Data.Ord (comparing)

data TCError a = IdentifierDeclared Identifier a
             | IdentifierNotDeclared Identifier
             | ParentClassNotFound ClassIdentifier a
             | TypeNotFound Type a
             | BadConstructor ClassIdentifier ClassIdentifier a
             | ClassDeclared ClassIdentifier a
             | InvalidArrayAssignment a
             | ConstructorOnlyClasses a
             | GeneralError a
             | BadReturns a
             | ErrorInStatement a
             | ScopeError a
             | ErrorInFunctionCall a
             | ExpressionNotBoolean a
             | BadForRange Integer Integer
             | BadExpression a
             -- Añadir más aquí

type AncestorsMap = Map.HashMap ClassIdentifier [ClassIdentifier]

instance (Show a) => Show (TCError a) where
    show err = case err of 
                 (IdentifierDeclared id a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Identifier " 
                                               ++ (color Red . style Bold  $ id ) 
                                               ++ (color White . style Bold $ " was already declared \n")))
                 (IdentifierNotDeclared id) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Identifier " 
                                               ++ (color Red . style Bold  $ id ) 
                                               ++ (color White . style Bold $ " not declared in \n")))
                 (ParentClassNotFound classId a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Parent class was never declared " 
                                               ++ (color Red . style Bold  $ classId ) 
                                               ++ (color White . style Bold $ " in \n"))
                                               ++ (color Red . style Bold  $ show $ a )) 

                 (TypeNotFound typ a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Type not found " 
                                               ++ (color Red . style Bold  $ show $ typ ) 
                                               ++ (color White . style Bold $ " in \n"))
                                               ++ (color Red . style Bold  $ show $ a )) 
                 (BadConstructor classId constructor a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Constructor name " 
                                               ++ (color Red . style Bold  $ constructor ) 
                                               ++ (color White . style Bold $ " should be the same as class name \n"))
                                               ++ (color Red . style Bold  $ show $ classId )) 
                 (ClassDeclared classId a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Class " 
                                               ++ (color Red . style Bold  $ classId ) 
                                               ++ (color White . style Bold $ " was already declared \n")))
                 (InvalidArrayAssignment a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Invalid array assignment in \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (ConstructorOnlyClasses a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Constructors can only be used in classes in \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (GeneralError a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "in \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (BadReturns a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "There are some bad return statements in function " 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (ErrorInStatement a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "in statement \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (ScopeError a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Out of scope in \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (ErrorInFunctionCall a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "in function call  \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (ExpressionNotBoolean a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Was expecting a boolean expression in \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                 (BadForRange lower upper) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Invalid for range \n" 
                                               ++ (color Red . style Bold  $ show $ lower )
                                               ++ (color White . style Bold  $ ".." )
                                               ++ (color Red . style Bold  $ show $ upper )))
                 (BadExpression a) -> (color Red . style Bold $ "error \n" 
                                               ++ (color White . style Bold  $ "Bad expression in \n" 
                                               ++ (color Red . style Bold  $ show $ a )))
                                               

                 

data TCState = TCState
                {   tcSymTab :: SymbolTable, 
                    tcClassSymTab :: ClassSymbolTable,
                    tcAncestors :: AncestorsMap
                }
                deriving (Show) 


setTCState :: SymbolTable -> ClassSymbolTable -> AncestorsMap -> TCState
setTCState s c i = (TCState s c i) 

getTCState :: TCState -> (SymbolTable,ClassSymbolTable,AncestorsMap)
getTCState (TCState s c i) = (s,c,i) 

modifyTCState (val,newTCState) = do 
                                  modify $ \s -> newTCState

type TypeChecker α = StateT TCState (Except (TCError String)) α

type TC = TypeChecker ()

runTypeChecker f = runExcept.runStateT f


exitIfFailure state = do 
                        whenLeft state (\e -> 
                                            do 
                                                putStrLn.show $ e
                                                exitFailure
                                        )

updatedSymTabOfClass  :: ClassIdentifier -> TC
updatedSymTabOfClass classIdentifier = 
                          do
                            tcState <- get
                            let (symTab,classSymTab,aMap) = getTCState tcState
                            let updatedSymTabOfClass = (Map.insert classIdentifier symTab classSymTab) 
                            modify $ \s -> (s { tcClassSymTab = updatedSymTabOfClass})

startSemanticAnalysis :: Program -> IO ()
startSemanticAnalysis (Program classList functionList varsList (Block statements)) =  do 
            do 
                let stateAfterClasses =  runTypeChecker (analyzeClasses classList) (setTCState emptySymbolTable emptyClassSymbolTable (Map.empty))
                exitIfFailure stateAfterClasses
                let (_,classSymbolTable,aMap) = getTCState (snd (fromRight' stateAfterClasses))
                let stateAfterFunctions = runTypeChecker (analyzeFunctions functionList globalScope Nothing) (setTCState emptySymbolTable classSymbolTable aMap)
                exitIfFailure stateAfterFunctions
                let (symbolTableWithFuncs,_,_) = getTCState (snd (fromRight' stateAfterFunctions))
                let stateAfterVariables = runTypeChecker (analyzeVariables varsList globalScope Nothing) (setTCState symbolTableWithFuncs classSymbolTable aMap)
                exitIfFailure stateAfterVariables
                let (symbolTableWithFuncsVars,_,_) = getTCState (snd (fromRight' stateAfterVariables))
                let returnListInMain = getReturnStatements statements
                if (length returnListInMain > 0) then do 
                                                        putStrLn $ "There shouldn't be returns in main"
                else 
                    do 
                        let stateAfterMain = runTypeChecker (analyzeStatements statements defScope) (setTCState symbolTableWithFuncsVars classSymbolTable aMap)
                        exitIfFailure stateAfterMain
                        let (symbolTableStatements,classSymbolTable,aMap) = getTCState (snd (fromRight' stateAfterMain))
                        putStrLn.ppShow $ symbolTableStatements
                        putStrLn.ppShow $ classSymbolTable
                        putStrLn.ppShow $ aMap
                        startMemoryAllocation (Program classList functionList varsList (Block statements)) symbolTableStatements classSymbolTable

                -- if (classErrors) 
                --     then putStrLn $ show "[SEMANTIC ANALYSIS 1] ERROR: Semantic Error in Class Checking."
                --     else do putStrLn $ ppShow $ "[SEMANTIC ANALYSIS 1]: Semantic Class Analysis Passed."
                --             putStrLn $ ppShow $ classSymbolTable
                
                -- let (symbolTableWithFuncsVars,semanticErrorVars) = analyzeVariables varsList globalScope Nothing symbolTableWithFuncs classSymbolTable
                
                -- let (symbolTableStatements,semanticErrorBlock) = 
                -- if (semanticErrorFuncs || semanticErrorVars || semanticErrorBlock || (length returnListInMain) > 0) 
                --     then putStrLn $ show "[SEMANTIC ANALYSIS 2] ERROR: Semantic Error in Variable Checking."
                --     else do putStrLn $ ppShow $ "[SEMANTIC ANALYSIS 2]: Semantic Variable Analysis Passed."
                --             putStrLn $ ppShow $  symbolTableStatements
                --             putStrLn $ show "[CODEGEN 1] Starting Memory Allocation for CodeGen"
                        -- 

deepFilter :: [(Identifier,Symbol)] -> Identifier -> [(Identifier,Symbol)]
deepFilter [] _  = []
deepFilter ((identifier,(SymbolFunction params p1 (Block statements) p2 p3 p4 symTabFunc)) : rest) identifierParent
    | identifier == identifierParent = ( deepFilter rest identifierParent )
    | otherwise = let filteredSym = (Map.fromList ( deepFilter (Map.toList symTabFunc) identifierParent ) )   
                  in let newSymTab = [(identifier,(SymbolFunction params p1 (Block statements) p2 p3 p4 filteredSym))]
                  in newSymTab ++ ( deepFilter rest identifierParent )
    

deepFilter (sym : rest) identifierParent = [sym] ++ (deepFilter rest identifierParent)

getClassName :: Class -> ClassIdentifier
getClassName (ClassInheritance classIdentifier _ _) = classIdentifier
getClassName (ClassNormal classIdentifier _) = classIdentifier

-- Analyze classes regresa una tabla de simbolos de clase y un booleano. Si es true, significa que hubo errores, si es false, no hubo errores
analyzeClasses :: [Class] -> TC 
analyzeClasses [] = do return ()
analyzeClasses (cl : classes)  =
                                do 
                                    tcState <- get
                                    let (_,classSymTab,aMap) = getTCState tcState
                                    let classIdentifier = getClassName cl
                                    if Map.member classIdentifier classSymTab
                                        then lift $ throwError (ClassDeclared classIdentifier "")
                                    else do 
                                      let classSymTabWithOwnClass = (Map.insert classIdentifier Map.empty classSymTab)
                                      let classStateAfterBlock = runTypeChecker (analyzeClassBlock cl) (setTCState emptySymbolTable classSymTabWithOwnClass aMap)
                                      whenLeft classStateAfterBlock (lift.throwError)
                                      whenRight classStateAfterBlock (modifyTCState)
                                      analyzeClass cl
                                      analyzeClasses classes
                                     


analyzeClassBlock :: Class -> TC 
-- Debido a que se está heredando, hay que meter la symbol table de la clase padre en la hijo
analyzeClassBlock (ClassInheritance classIdentifier parentClass classBlock) = 
                do 
                    tcState <- get
                    let (symTabOfCurrentClass,classSymTab,aMap) = getTCState tcState
                    case (Map.lookup parentClass classSymTab) of
                        Just symTabOfClass -> 
                                do 
                                    let newSymTabOfCurrentClass = (Map.union symTabOfCurrentClass symTabOfClass)
                                    modify $ \s -> (s { tcSymTab = newSymTabOfCurrentClass})
                                    case (Map.lookup parentClass aMap) of
                                        Just classAncestors -> do 
                                                                let ancestorsForCurrentClass = [parentClass] ++ classAncestors
                                                                let newAncestorMap = (Map.insert classIdentifier ancestorsForCurrentClass aMap) 
                                                                modify $ \s -> (s { tcAncestors = newAncestorMap}) 
                                                                analyzeMembersOfClassBlock classBlock classIdentifier globalScope
                                                                updatedSymTabOfClass classIdentifier
                                    

                                
                                
                        Nothing -> do
                                    let s =  (ClassInheritance classIdentifier parentClass classBlock)
                                    lift $ throwError (ParentClassNotFound parentClass (show s))


analyzeClassBlock (ClassNormal classIdentifier classBlock) = do 
                                                                tcState <- get
                                                                let (symTabOfCurrentClass,classSymTab,aMap) = getTCState tcState
                                                                let newAncestorMap = (Map.insert classIdentifier [] aMap) 
                                                                modify $ \s -> (s { tcAncestors = newAncestorMap}) 
                                                                analyzeMembersOfClassBlock classBlock classIdentifier globalScope
                                                                updatedSymTabOfClass classIdentifier

analyzeMembersOfClassBlock :: ClassBlock -> ClassIdentifier -> Scope -> TC
analyzeMembersOfClassBlock (ClassBlockNoConstructor classMembers) classIdentifier scp = do 
                                                                                          analyzeClassMembers classMembers classIdentifier scp
                                                                                          updatedSymTabOfClass classIdentifier
analyzeMembersOfClassBlock (ClassBlock classMembers (ClassConstructor classIdConstructor params block)) classIdentifier scp  = 
                                        do 
                                            if (classIdConstructor /= classIdentifier) 
                                                then do 
                                                        lift $ throwError (BadConstructor classIdentifier classIdConstructor "")
                                            else 
                                                do 
                                                    analyzeClassMembers classMembers classIdentifier scp
                                                    analyzeFunction (Function (classIdentifier ++ "_constructor") TypeFuncReturnNothing params block) scp (Just True)
                                                    updatedSymTabOfClass classIdentifier

analyzeClassMembers :: [ClassMember] -> ClassIdentifier -> Scope -> TC
analyzeClassMembers [] _ _ = return ()
analyzeClassMembers (cm : cms) classIdentifier scp = 
                                                    do 
                                                        analyzeClassMember cm classIdentifier scp
                                                        updatedSymTabOfClass classIdentifier
                                                        analyzeClassMembers cms classIdentifier scp
                                                        updatedSymTabOfClass classIdentifier


analyzeClassMember :: ClassMember -> ClassIdentifier -> Scope -> TC
analyzeClassMember (ClassMemberAttribute (ClassAttributePublic variable)) classIdentifier scp = analyzeVariable variable scp  (Just True)
analyzeClassMember (ClassMemberAttribute (ClassAttributePrivate variable)) classIdentifier scp  = analyzeVariable variable scp  (Just False) 
analyzeClassMember (ClassMemberFunction (ClassFunctionPublic function)) classIdentifier scp = analyzeFunction function scp (Just True)
analyzeClassMember (ClassMemberFunction (ClassFunctionPrivate function)) classIdentifier scp = analyzeFunction function scp (Just False)

analyzeClass :: Class -> TC
analyzeClass (ClassInheritance subClass parentClass classBlock)  = 
                                                                    do 
                                                                        tcState <- get
                                                                        let (symTab,classSymTab,aMap) = getTCState tcState
                                                                        let s =  (ClassInheritance subClass parentClass classBlock)
                                                                        if Map.member parentClass classSymTab  
                                                                            then do 
                                                                                    let newClassSymTable = Map.insert subClass symTab classSymTab -- Si si es miembro, entonces si se puede heredar
                                                                                    modify $ \s -> (s { tcClassSymTab = newClassSymTable}) 
                                                                            else lift $ throwError (ParentClassNotFound parentClass (show s)) -- Si el parent class no es miembro, entonces error, no puedes heredar de una clase no declarada

analyzeClass (ClassNormal classIdentifier classBlock)  = do 
                                                            tcState <- get
                                                            let (symTab,classSymTab,aMap) = getTCState tcState
                                                            let s =  (ClassNormal classIdentifier classBlock)
                                                            let newClassSymTable = Map.insert classIdentifier symTab classSymTab
                                                            modify $ \s -> (s { tcClassSymTab = newClassSymTable}) 
                                                            -- if Map.member classIdentifier classSymTab
                                                            --         then lift $ throwError (ClassDeclared classIdentifier (show s))
                                                            --         else 
                                                            --             do 
                                                            --                 let newClassSymTable = Map.insert classIdentifier symTab classSymTab
                                                            --                 modify $ \s -> (s { tcClassSymTab = newClassSymTable}) 

analyzeFunctions :: [Function] -> Scope -> Maybe Bool -> TC
analyzeFunctions [] _ _  = return ()
analyzeFunctions (func : funcs) scp isFuncPublic  = do 
                                                        analyzeFunction func scp isFuncPublic
                                                        analyzeFunctions funcs scp isFuncPublic
                                                        

analyzeVariables :: [Variable] -> Scope -> Maybe Bool -> TC
analyzeVariables [] _ _  = return ()
analyzeVariables (var : vars) scp isVarPublic = do  
                                                    analyzeVariable var scp isVarPublic
                                                    analyzeVariables vars scp isVarPublic                     


analyzeVariable :: Variable -> Scope -> Maybe Bool -> TC
analyzeVariable (VariableNoAssignment dataType identifiers) scp isVarPublic = 
                                                         do 
                                                            tcState <- get
                                                            let (symTab,classSymTab,aMap) = getTCState tcState
                                                            let s =  (VariableNoAssignment dataType identifiers)
                                                            -- Checamos si existe ese tipo
                                                            if (checkTypeExistance dataType classSymTab) 
                                                                then insertIdentifiers identifiers (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic})
                                                                else lift $ throwError (TypeNotFound dataType (show s)) -- Tipo no existe
analyzeVariable (VariableAssignmentLiteralOrVariable dataType identifier literalOrVariable) scp isVarPublic =
                                        -- En esta parte nos aseguramos que el tipo este declarado, el literal or variable exista y que la asignacion de tipos de datos sea correcta
                                        do 
                                            tcState <-  get
                                            let (symTab,classSymTab,aMap) = getTCState tcState
                                            let s =  (VariableAssignmentLiteralOrVariable dataType identifier literalOrVariable)
                                            -- Checamos si existe ese tipo
                                            if (checkTypeExistance dataType classSymTab) &&  (checkLiteralOrVariableInSymbolTable scp literalOrVariable symTab) && (checkDataTypes scp dataType literalOrVariable symTab)
                                                then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic})
                                                else lift $ throwError (GeneralError (show s)) 
analyzeVariable (VariableAssignment1D dataType identifier literalOrVariables) scp isVarPublic = 
                                        do -- En esta parte nos aseguramos que la lista de asignaciones concuerde con el tipo de dato declarado
                                            tcState <-  get
                                            let (symTab,classSymTab,aMap) = getTCState tcState
                                            let s =  (VariableAssignment1D dataType identifier literalOrVariables)
                                            case dataType of
                                                TypePrimitive _ (("[",size,"]") : []) ->  
                                                    makeCheckFor1DAssignment size symTab classSymTab s
                                                TypeClassId _ (("[",size,"]") : []) ->  
                                                    makeCheckFor1DAssignment size symTab classSymTab s
                                                _ -> lift $ throwError (InvalidArrayAssignment (show s))
                                            where
                                                makeCheckFor1DAssignment size symTab classSymTab s = if (checkTypeExistance dataType classSymTab) 
                                                                                    && (checkLiteralOrVariablesAndDataTypes scp dataType literalOrVariables symTab) 
                                                                                    && ((length literalOrVariables) <= fromIntegral size)
                                                        then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic})
                                                        else lift $ throwError (GeneralError (show s))
                                        
analyzeVariable (VariableAssignment2D dataType identifier listOfLiteralOrVariables) scp isVarPublic =
                                        do -- En esta parte nos aseguramos que la lista de asignaciones concuerde con el tipo de dato declarado
                                            tcState <-  get
                                            let (symTab,classSymTab,aMap) = getTCState tcState
                                            let s =  (VariableAssignment2D dataType identifier listOfLiteralOrVariables) 
                                            case dataType of
                                                TypePrimitive _ (("[",sizeRows,"]") : ("[",sizeCols,"]") : []) ->  
                                                    makeCheckFor2DAssignment sizeRows sizeCols symTab classSymTab s
                                                TypeClassId _ (("[",sizeRows,"]") : ("[",sizeCols,"]") : []) ->  
                                                    makeCheckFor2DAssignment sizeRows sizeCols symTab classSymTab s
                                                _ -> lift $ throwError (InvalidArrayAssignment (show s))
                                            where
                                                makeCheckFor2DAssignment sizeRows sizeCols symTab classSymTab s= if (checkTypeExistance dataType classSymTab) && 
                                                                                   (checkLiteralOrVariablesAndDataTypes2D scp dataType listOfLiteralOrVariables symTab)
                                                                                   && ((length listOfLiteralOrVariables) <= fromIntegral sizeRows) -- checamos que sea el numero correcto de renglones
                                                                                   && ((getLongestList listOfLiteralOrVariables) <= fromIntegral sizeCols)
                                                        then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic})
                                                        else lift $ throwError (GeneralError (show s))
                                                getLongestList :: [[LiteralOrVariable]] -> Int
                                                getLongestList [] = 0
                                                getLongestList (x : xs) = max (length x) (getLongestList xs) 
analyzeVariable (VariableAssignmentObject dataType identifier (ObjectCreation classIdentifier params)) scp isVarPublic =
                                    do -- En esta parte nos aseguramos que la lista de asignaciones concuerde con el tipo de dato declarado
                                        tcState <- get
                                        let (symTab,classSymTab,aMap) = getTCState tcState
                                        let s =  (VariableAssignmentObject dataType identifier (ObjectCreation classIdentifier params)) 
                                        case dataType of
                                            TypePrimitive _ _ -> lift $ throwError (ConstructorOnlyClasses (show s))
                                            -- Checamos si el constructor es del mismo tipo que la clase
                                            TypeClassId classIdentifierDecl [] -> do 
                                                                                    if (classIdentifierDecl == classIdentifier)
                                                                                     -- Checamos los parametros que se mandan con los del constructor
                                                                                     && (checkIfParamsAreCorrect scp params classIdentifier symTab classSymTab) 
                                                                                     then insertInSymbolTable identifier (SymbolVar {dataType = dataType, scope = scp, isPublic = isVarPublic})
                                                                                     else lift $ throwError (GeneralError (show s))
analyzeVariable var _ _  = lift $ throwError (GeneralError (show var))

filterFuncs :: Symbol -> Bool
filterFuncs (SymbolFunction _ _ _ _ _ _ _) = False
filterFuncs _ = False


analyzeFunction :: Function -> Scope -> Maybe Bool -> TC
analyzeFunction (Function identifier (TypeFuncReturnPrimitive primitive arrayDimension) params (Block statements)) scp isPublic = 
                    do 
                        tcState <- get
                        let (symTab',classSymTab,aMap) = getTCState tcState
                        let symTabWithParamsState = runTypeChecker (analyzeFuncParams params) (setTCState emptySymbolTable classSymTab aMap)
                        whenLeft symTabWithParamsState (lift.throwError)
                        -- whenRight classStateAfterBlock modifyTCState
                        let (newFuncSymTab,_,_) = getTCState (snd (fromRight' symTabWithParamsState))
                        -- let (symTab',classSymTab,aMap) = getTCState tcState
                        let s =  (Function identifier (TypeFuncReturnPrimitive primitive arrayDimension) params (Block statements))
                        -- if  not (Map.member identifier symTab) then  -- Esto se comenta para que se pueda hacer override de las funciones
                        if ((Map.size (Map.intersection symTab' newFuncSymTab)) /= 0) then lift $ throwError (GeneralError (show s))
                        else do 
                                let symTab = (Map.fromList (deepFilter (Map.toList symTab') identifier))
                                let symTabFuncWithOwnFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypePrimitive primitive arrayDimension)), scope = scp, body = (Block []), shouldReturn = True ,isPublic = isPublic, symbolTable = newFuncSymTab, params = params}) symTab
                                let newSymTabForFunc = (Map.union (Map.union symTabFuncWithOwnFunc symTab) newFuncSymTab)
                                -- modify $ \s -> (s { tcSymTab = newSymTabForFunc}) 
                                let symTabWithStatementsState = runTypeChecker (analyzeStatements statements scp) (setTCState newSymTabForFunc classSymTab aMap)
                                whenLeft symTabWithStatementsState (lift.throwError)
                                let (symTabFuncFinished,_,_) = getTCState (snd (fromRight' symTabWithStatementsState))
                                -- let (symTabWithStatements,_,_) = getTCState tcState
                                let updatedSymTabFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypePrimitive primitive arrayDimension)), scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.filterWithKey (\k _ -> k /= identifier) symTabFuncFinished), params = params}) symTab
                                if (areReturnTypesOk scp (TypePrimitive primitive arrayDimension) statements updatedSymTabFunc symTabFuncFinished classSymTab)
                                    then 
                                        modify $ \s -> (s { tcSymTab = updatedSymTabFunc } )
                                    else lift $ throwError (BadReturns identifier)
analyzeFunction (Function identifier (TypeFuncReturnClassId classIdentifier arrayDimension) params (Block statements)) scp isPublic = 
                    do 
                        tcState <-  get
                        let (symTab',classSymTab,aMap) = getTCState tcState
                        let symTabWithParamsState = runTypeChecker (analyzeFuncParams params) (setTCState emptySymbolTable classSymTab aMap)
                        whenLeft symTabWithParamsState (lift.throwError)
                        -- whenRight classStateAfterBlock modifyTCState
                        let (newFuncSymTab,_,_) = getTCState (snd (fromRight' symTabWithParamsState))
                        -- let (symTab',classSymTab,aMap) = getTCState tcState
                        let s =  (Function identifier (TypeFuncReturnClassId classIdentifier arrayDimension) params (Block statements))
                        -- if  not (Map.member identifier symTab) then  -- Esto se comenta para que se pueda hacer override de las funciones
                        if ((Map.size (Map.intersection symTab' newFuncSymTab)) /= 0) then lift $ throwError (GeneralError (show s))
                        else do 
                                let symTab = (Map.fromList (deepFilter (Map.toList symTab') identifier))
                                let symTabFuncWithOwnFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypeClassId classIdentifier arrayDimension)), scope = scp, body = (Block []), shouldReturn = True ,isPublic = isPublic, symbolTable = newFuncSymTab, params = params}) symTab
                                let newSymTabForFunc = (Map.union (Map.union symTabFuncWithOwnFunc symTab) newFuncSymTab)
                                -- modify $ \s -> (s { tcSymTab = newSymTabForFunc}) 
                                let symTabWithStatementsState = runTypeChecker (analyzeStatements statements scp) (setTCState newSymTabForFunc classSymTab aMap)
                                whenLeft symTabWithStatementsState (lift.throwError)
                                let (symTabFuncFinished,_,_) = getTCState (snd (fromRight' symTabWithStatementsState))
                                -- let (symTabWithStatements,_,_) = getTCState tcState
                                let updatedSymTabFunc = Map.insert identifier (SymbolFunction {returnType = (Just (TypeClassId classIdentifier arrayDimension)), scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.filterWithKey (\k _ -> k /= identifier) symTabFuncFinished), params = params}) symTab
                                if (areReturnTypesOk scp (TypeClassId classIdentifier arrayDimension) statements updatedSymTabFunc symTabFuncFinished classSymTab)
                                    then 
                                        modify $ \s -> (s { tcSymTab = updatedSymTabFunc } )
                                    else lift $ throwError (BadReturns identifier)

    -- Como no regresa nada, no hay que buscar que regrese algo el bloque
analyzeFunction (Function identifier (TypeFuncReturnNothing) params (Block statements)) scp isPublic =  
                  -- if  not (Map.member identifier symTab) -- Esto se comenta para que se pueda hacer override de las funciones
                     do 
                        tcState <-  get
                        let (symTab',classSymTab,aMap) = getTCState tcState
                        let symTabWithParamsState = runTypeChecker (analyzeFuncParams params) (setTCState emptySymbolTable classSymTab aMap)
                        whenLeft symTabWithParamsState (lift.throwError)
                        -- whenRight classStateAfterBlock modifyTCState
                        let (newFuncSymTab,_,_) = getTCState (snd (fromRight' symTabWithParamsState))
                        -- let (symTab',classSymTab,aMap) = getTCState tcState
                        let s =  (Function identifier (TypeFuncReturnNothing) params (Block statements))
                        -- if  not (Map.member identifier symTab) then  -- Esto se comenta para que se pueda hacer override de las funciones
                        if ( ((Map.size (Map.intersection symTab' newFuncSymTab)) /= 0) || (length (getReturnStatements statements)) > 0) then lift $ throwError (GeneralError (show s))
                        else do
                                let symTab = (Map.fromList (deepFilter (Map.toList symTab') identifier))
                                let symTabFuncWithOwnFunc = Map.insert identifier (SymbolFunction {returnType = Nothing, scope = scp, body = (Block []), shouldReturn = True ,isPublic = isPublic, symbolTable = newFuncSymTab, params = params}) symTab
                                let newSymTabForFunc = (Map.union (Map.union symTabFuncWithOwnFunc symTab) newFuncSymTab)
                                -- modify $ \s -> (s { tcSymTab = newSymTabForFunc}) 
                                let symTabWithStatementsState = runTypeChecker (analyzeStatements statements scp) (setTCState newSymTabForFunc classSymTab aMap)
                                whenLeft symTabWithStatementsState (lift.throwError)
                                let (symTabFuncFinished,_,_) = getTCState (snd (fromRight' symTabWithStatementsState))
                                -- let (symTabWithStatements,_,_) = getTCState tcState
                                let updatedSymTabFunc = Map.insert identifier (SymbolFunction {returnType = Nothing, scope = scp, body = (Block statements), shouldReturn = True ,isPublic = isPublic, symbolTable = (Map.filterWithKey (\k _ -> k /= identifier) symTabFuncFinished), params = params}) symTab
                                modify $ \s -> (s { tcSymTab = updatedSymTabFunc } )

areReturnTypesOk :: Scope -> Type -> [Statement] -> SymbolTable -> SymbolTable -> ClassSymbolTable -> Bool
areReturnTypesOk _ _ [] _ _ _  = False 
areReturnTypesOk scp funcRetType ((ReturnStatement ret) : []) symTab ownFuncSymTab classTab = 
                                                checkCorrectReturnTypes scp funcRetType ret symTab ownFuncSymTab classTab
areReturnTypesOk scp funcRetType ((ConditionStatement (If _ (Block statements))) : []) symTab ownFuncSymTab classTab = 
                                                areReturnTypesOk (scp - 1) funcRetType statements symTab ownFuncSymTab classTab
areReturnTypesOk scp funcRetType ((ConditionStatement (IfElse _ (Block statements) (Block statementsElse))) : []) symTab ownFuncSymTab classTab = 
                                                areReturnTypesOk (scp - 1) funcRetType statements symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType statementsElse symTab ownFuncSymTab classTab 
areReturnTypesOk scp funcRetType ((CaseStatement (Case expressionToMatch expAndBlock otherwiseStatements)) : []) symTab ownFuncSymTab classTab = 
                                                (foldl (\bool f -> (bool && (areReturnTypesOk (scp - 1) funcRetType (snd f) symTab ownFuncSymTab classTab))) True expAndBlock ) &&
                                                (areReturnTypesOk (scp - 1) funcRetType otherwiseStatements symTab ownFuncSymTab classTab)
areReturnTypesOk scp funcRetType ((ConditionStatement (If _ (Block statements))) : sts) symTab ownFuncSymTab classTab = 
                                                areReturnTypesOk (scp - 1) funcRetType statements symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab 
areReturnTypesOk scp funcRetType ((ConditionStatement (IfElse _ (Block statements) (Block statementsElse))) : sts) symTab ownFuncSymTab classTab = 
                                                areReturnTypesOk (scp - 1) funcRetType statements symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType statementsElse symTab ownFuncSymTab classTab &&
                                                areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab 
areReturnTypesOk scp funcRetType ((CaseStatement (Case expressionToMatch expAndBlock otherwiseStatements)) : sts) symTab ownFuncSymTab classTab = 
                                                (foldl (\bool f -> (bool && (areReturnTypesOk (scp - 1) funcRetType (snd f) symTab ownFuncSymTab classTab))) True expAndBlock ) &&
                                                (areReturnTypesOk (scp - 1) funcRetType otherwiseStatements symTab ownFuncSymTab classTab)
                                                && areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab  
areReturnTypesOk scp funcRetType (_ : sts) symTab ownFuncSymTab classTab =  areReturnTypesOk (scp - 1) funcRetType sts symTab ownFuncSymTab classTab 

-- Aqui sacamos todos los returns que pueda haber, inclusive si estan en statements anidados
getReturnStatements :: [Statement]  -> [Return]
getReturnStatements [] = []
getReturnStatements ((ReturnStatement returnExp) : sts) = (returnExp) : (getReturnStatements sts)
getReturnStatements ((ConditionStatement (If _ (Block statements))) : sts) = (getReturnStatements statements) ++ (getReturnStatements sts)
getReturnStatements ((ConditionStatement (IfElse _ (Block statements) (Block statementsElse))) : sts) = (getReturnStatements statements) ++ (getReturnStatements statementsElse) ++ (getReturnStatements sts)
getReturnStatements ((CaseStatement (Case expressionToMatch expAndBlock otherwiseStatements)) : sts) = (foldl (\rets f -> (rets ++ (getReturnStatements (snd f)))) [] expAndBlock ) ++ (getReturnStatements otherwiseStatements) ++ (getReturnStatements sts)
getReturnStatements ((CycleStatement (CycleWhile (While _ (Block statements)))) : sts) = (getReturnStatements statements) ++ (getReturnStatements sts)
getReturnStatements ((CycleStatement (CycleFor (For _ _ (Block statements)))) : sts) = (getReturnStatements statements) ++ (getReturnStatements sts)
getReturnStatements (_ : sts) =  (getReturnStatements sts)

analyzeFuncParams :: [(Type,Identifier)] -> TC
analyzeFuncParams []  = return ()
analyzeFuncParams ((dataType,identifier) : rest) = 
        do 
            analyzeVariable ((VariableNoAssignment dataType [identifier])) globalScope Nothing
            analyzeFuncParams rest


-- El tipo de regreso de una funcion solo puede ser un elemento atomico!
checkCorrectReturnTypes :: Scope -> Type -> Return -> SymbolTable -> SymbolTable -> ClassSymbolTable -> Bool
checkCorrectReturnTypes scp dataType (ReturnExp expression) symTab ownFuncSymTab classTab =  
                                            case (preProcessExpression scp expression (Map.union symTab ownFuncSymTab) classTab) of
                                                Just expType -> dataType == expType
                                                _ -> False   

-- Checamos aqui que la llamada al constructor sea correcta
checkIfParamsAreCorrect :: Scope -> [Params] -> ClassIdentifier -> SymbolTable -> ClassSymbolTable -> Bool
checkIfParamsAreCorrect scp sendingParams classIdentifier symTab classTab = 
                                    case (Map.lookup classIdentifier classTab) of
                                        Just symbolTableOfClass -> 
                                                case (Map.lookup (classIdentifier ++ "_constructor") symbolTableOfClass) of
                                                    Just (symbolFunc) -> (compareListOfTypesWithFuncCall scp (map (\f -> fst f) (params symbolFunc)) sendingParams symTab classTab)
                                                    Nothing -> False 
                                        Nothing -> False 

analyzeStatements :: [Statement] -> Scope -> TC
analyzeStatements [] _ = return ()
analyzeStatements (st : sts) scp = do 
                                    analyzeStatement st scp
                                    analyzeStatements sts scp

analyzeStatement :: Statement -> Scope -> TC
analyzeStatement (AssignStatement assignment) scp = 
                                                    do
                                                        tcState <- get
                                                        let (symTab,classSymTab,aMap) = getTCState tcState
                                                        if (isAssignmentOk assignment scp symTab classSymTab)
                                                            then return ()
                                                        else lift $ throwError (ErrorInStatement (show assignment))
analyzeStatement (DisplayStatement displays) scp = 
                                                                do
                                                                    tcState <- get
                                                                    let (symTab,classSymTab,aMap) = getTCState tcState
                                                                    if (analyzeDisplays displays symTab classSymTab)
                                                                        then return ()
                                                                        else lift $ throwError (ErrorInStatement (show displays))
                                                                    where                                        
                                                                        analyzeDisplays [] _ _ = True
                                                                        analyzeDisplays (disp : disps) symTab classSymTab = 
                                                                                analyzeDisplay disp scp symTab classSymTab
                                                                                && analyzeDisplays disps symTab classSymTab

analyzeStatement (ReadStatement (Reading identifier)) scp = 
                                    do
                                        tcState <- get
                                        let (symTab,classSymTab,aMap) = getTCState tcState
                                        case (Map.lookup identifier symTab) of
                                            Just (SymbolVar (TypePrimitive _ []) varScp _) ->
                                                if varScp >= scp then
                                                    return ()
                                                else lift $ throwError (ScopeError identifier)
                                            _ -> lift $ throwError (IdentifierNotDeclared identifier)

analyzeStatement (DPMStatement assignment) scp = analyzeStatement (AssignStatement assignment) scp
analyzeStatement (FunctionCallStatement functionCall) scp = 
                                                            do
                                                                tcState <- get
                                                                let (symTab,classSymTab,aMap) = getTCState tcState
                                                                if (analyzeFunctionCall functionCall scp symTab classSymTab) 
                                                                            then return ()
                                                                            else lift $ throwError (ErrorInFunctionCall (show functionCall))
analyzeStatement (VariableStatement var) scp = analyzeVariable var scp Nothing
analyzeStatement (ConditionStatement (If expression (Block statements))) scp = 
                                                do
                                                    tcState <- get
                                                    let (symTab,classSymTab,aMap) = getTCState tcState
                                                    case (expressionTypeChecker scp expression symTab classSymTab) of
                                                        -- Si la expresión del if regresa booleano, entonces está bien
                                                        Right (TypePrimitive PrimitiveBool []) -> analyzeStatements statements (scp - 1)
                                                       -- De lo contrario, no se puede tener esa expresión en el if
                                                        _ -> lift $ throwError (ExpressionNotBoolean (show expression))
analyzeStatement (ConditionStatement (IfElse expression (Block statements) (Block statements2))) scp =
                                                do
                                                    tcState <- get
                                                    let (symTab,classSymTab,aMap) = getTCState tcState 
                                                    case (expressionTypeChecker scp expression symTab classSymTab) of
                                                        -- Si la expresión del if regresa booleano, entonces está bien
                                                        Right (TypePrimitive PrimitiveBool []) ->
                                                            do 
                                                                analyzeStatements statements (scp - 1)
                                                                analyzeStatements statements2 (scp - 1) 
                                                       -- De lo contrario, no se puede tener esa expresión en el if
                                                        _ -> lift $ throwError (ExpressionNotBoolean (show expression))

analyzeStatement (CaseStatement (Case expressionToMatch expAndBlock otherwiseStatements)) scp =
                                                do
                                                    tcState <- get
                                                    let (symTab,classSymTab,aMap) = getTCState tcState
                                                    case (expressionTypeChecker scp expressionToMatch symTab classSymTab) of 
                                                      Right dataTypeToMatch -> 
                                                        do 
                                                          analyzeCases scp expAndBlock dataTypeToMatch
                                                          analyzeStatements otherwiseStatements (scp - 1)
                                                      Left err -> lift $ throwError (GeneralError (show err))
                                                
analyzeStatement (CycleStatement (CycleWhile (While expression (Block statements)))) scp = 
                do
                    tcState <- get
                    let (symTab,classSymTab,aMap) = getTCState tcState 
                    case (expressionTypeChecker scp expression symTab classSymTab) of
                        Right (TypePrimitive PrimitiveBool []) -> analyzeStatements statements (scp - 1)
                        _ ->  lift $ throwError (ExpressionNotBoolean (show expression))
analyzeStatement (CycleStatement (CycleFor (For lowerRange greaterRange (Block statements)))) scp = 
                                                                            if (greaterRange >= lowerRange) 
                                                                                then analyzeStatements statements (scp - 1)
                                                                                else lift $ throwError (BadForRange lowerRange greaterRange)
analyzeStatement (CycleStatement (CycleForVar statements)) scp = analyzeStatements statements scp
analyzeStatement (ReturnStatement (ReturnFunctionCall functionCall)) scp = 
                do
                    tcState <- get
                    let (symTab,classSymTab,aMap) = getTCState tcState  
                    let isFuncCallOk = analyzeFunctionCall functionCall scp symTab classSymTab
                    if (isFuncCallOk) then return ()
                        else lift $ throwError (ErrorInFunctionCall (show functionCall))
analyzeStatement (ReturnStatement (ReturnExp expression)) scp =  
            do 
                tcState <- get
                let (symTab,classSymTab,aMap) = getTCState tcState 
                case (preProcessExpression scp expression symTab classSymTab) of
                    Just expType -> return ()
                    Nothing -> lift $ throwError (BadExpression (show expression))

analyzeCases :: Scope -> [(Expression,[Statement])] -> Type -> TC
analyzeCases scp [] _ = return ()
analyzeCases scp ((e,statements) : es) dataTypeToMatch  = 
      do
        tcState <- get
        let (symTab,classSymTab,aMap) = getTCState tcState 
        case (expressionTypeChecker scp e symTab classSymTab) of
          Right dataTypeOfExp -> 
                      if (dataTypeOfExp == dataTypeToMatch) then 
                        do 
                          analyzeStatements statements (scp - 1)
                          analyzeCases scp es dataTypeToMatch
                      else lift $ throwError (GeneralError "Case head expression is different from expressions to match")
          Left _ -> lift $ throwError (GeneralError "Case with a bad formed expression")

analyzeDisplay :: Display -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
analyzeDisplay (DisplayLiteralOrVariable (VarIdentifier identifier) _) scp symTab classTab = 
                            case (Map.lookup identifier symTab) of 
                                Just (SymbolVar (TypePrimitive prim _) varScp _) ->
                                    varScp >= scp
                                Just (SymbolVar (TypeClassId classIdentifier _) varScp _) ->
                                    varScp >= scp
                                _ -> False

-- Podemos desplegar primitivos sin problemas
analyzeDisplay (DisplayLiteralOrVariable _ _) scp symTab classTab = True
analyzeDisplay (DisplayObjMem (ObjectMember objectIdentifier attrIdentifier) _) scp symTab classTab =
                            case (Map.lookup objectIdentifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _) -> 
                                        if (objScp >= scp) 
                                            then
                                            case (Map.lookup classIdentifier classTab) of
                                                Just symbolTableOfClass ->
                                                        case (Map.lookup attrIdentifier symbolTableOfClass) of
                                                            -- Si y solo si es publico el atributo, la accedemos
                                                            Just (SymbolVar attrDataType attrScp (Just True)) ->
                                                                True 
                                                            _ -> False   
                                                _ -> False
                                        else False
                                    _ -> False
analyzeDisplay (DisplayFunctionCall functionCall _) scp symTab classTab =
                            analyzeFunctionCall functionCall scp symTab classTab
analyzeDisplay (DisplayVarArrayAccess identifier ((ArrayAccessExpression expressionIndex) : []) _ ) scp symTab classTab =
                            case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) varScp _ ) -> 
                                        if (varScp >= scp) then
                                         let typeIndexExp = (preProcessExpression scp expressionIndex symTab classTab)
                                            in case typeIndexExp of
                                                    Just (TypePrimitive PrimitiveInteger _) ->  
                                                        True
                                                    Just (TypePrimitive PrimitiveInt _) ->  
                                                        True
                                                    _ -> False
                                         else False
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _ ) -> 
                                        if (varScp >= scp) then
                                         let typeIndexExp = (preProcessExpression scp expressionIndex symTab classTab)
                                                        in case typeIndexExp of
                                                                Just (TypePrimitive PrimitiveInteger _) ->  
                                                                    True
                                                                Just (TypePrimitive PrimitiveInt _) ->  
                                                                    True
                                                                _ -> False
                                         else False 
                                    _ -> False                            
analyzeDisplay (DisplayVarArrayAccess identifier ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) _ ) scp symTab classTab =
                            case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",columns,"]") : [])) varScp _ ) -> 
                                        if (varScp >= scp) 
                                            then
                                                let typeRowExp = (expressionTypeChecker scp innerExpRow symTab classTab)
                                                    typeColExp = (expressionTypeChecker scp innerExpCol symTab classTab)
                                                        in if (typeColExp == typeRowExp) then
                                                            case typeRowExp of
                                                               Right (TypePrimitive PrimitiveInt []) ->  True
                                                               Right (TypePrimitive PrimitiveInteger []) -> True
                                                               _ -> False
                                                            else False
                                            else False

                                    Just (SymbolVar (TypeClassId classIdentifier (("[",rows,"]") : ("[",cols,"]")  : [])) varScp _ ) -> 
                                        if (varScp >= scp)
                                            then
                                                let typeRowExp = (expressionTypeChecker scp innerExpRow symTab classTab)
                                                    typeColExp = (expressionTypeChecker scp innerExpCol symTab classTab)
                                                        in if (typeColExp == typeRowExp) then
                                                            case typeRowExp of
                                                               Right (TypePrimitive PrimitiveInt []) -> True
                                                               Right (TypePrimitive PrimitiveInteger []) -> True
                                                               _ -> False
                                                            else False
                                            else False 
                                    _ -> False 


isAssignmentOk :: Assignment -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
isAssignmentOk (AssignmentExpression identifier expression) scp symTab classSymTab = case (Map.lookup identifier symTab) of
                                                                                        Just (SymbolVar (TypePrimitive prim accessExpression) varScp _) -> 
                                                                                                    -- Si el scope de esta variable es mayor al scope de este assignment, si puedo accederlo
                                                                                                    if (varScp >= scp) 
                                                                                                        then 
                                                                                                            let typeExp = (preProcessExpression scp expression symTab classSymTab)
                                                                                                                in case typeExp of
                                                                                                                       Just typeUnwrapped ->  
                                                                                                                            typeUnwrapped == (TypePrimitive prim accessExpression)
                                                                                                                       _ -> False
                                                                                                        else False
                                                                                        Just (SymbolVar (TypeClassId classIdentifier _) varScp _) -> 
                                                                                                    -- Si el scope de esta variable es mayor al scope de este assignment, si puedo accederlo
                                                                                                    if (varScp >= scp) 
                                                                                                        then let typeExp = (preProcessExpression scp expression symTab classSymTab)
                                                                                                                in case typeExp of
                                                                                                                       Just (TypeClassId classId _) ->  
                                                                                                                            classIdentifier == classId
                                                                                                                       _ -> False
                                                                                                        else False
                                                                                        _ -> False

isAssignmentOk  (AssignmentObjectMember identifier (ObjectMember objectIdentifier attrIdentifier)) scp symTab classSymTab =  
                      case (Map.lookup identifier symTab) of
                        Just (SymbolVar dataType varScp _) ->
                                    if (varScp >= scp)
                                        then 
                                        case (Map.lookup objectIdentifier symTab) of
                                            Just (SymbolVar (TypeClassId classIdentifier []) objScp _) -> 
                                                if (objScp >= scp) 
                                                    then
                                                    case (Map.lookup classIdentifier classSymTab) of
                                                        Just symbolTableOfClass ->
                                                                case (Map.lookup attrIdentifier symbolTableOfClass) of
                                                                    -- Si y solo si es publico el atributo, la accedemos
                                                                    Just (SymbolVar attrDataType attrScp (Just True)) ->
                                                                        dataType == attrDataType  
                                                                    _ -> False   
                                                        _ -> False
                                                else False
                                            _ -> False
                                    else False
                        _ -> False

isAssignmentOk  (AssignmentObjectMemberExpression (ObjectMember objectIdentifier attrIdentifier) expression) scp symTab classSymTab =  
                      case (Map.lookup objectIdentifier symTab) of
                        Just (SymbolVar (TypeClassId classIdentifier _) objScp _) -> 
                            if (objScp >= scp) 
                                then
                                case (Map.lookup classIdentifier classSymTab) of
                                    Just symbolTableOfClass ->
                                            case (Map.lookup attrIdentifier symbolTableOfClass) of
                                                -- Si y solo si es publico el atributo, la accedemos
                                                Just (SymbolVar attrDataType attrScp (Just True)) ->
                                                    let typeExp = (preProcessExpression scp expression symTab classSymTab)
                                                        in case typeExp of
                                                               Just dataTypeExp ->  
                                                                    dataTypeExp == attrDataType
                                                               _ -> False
                                                _ -> False   
                                    _ -> False
                            else False
                        _ -> False

isAssignmentOk  (AssignmentArrayExpression identifier ((ArrayAccessExpression innerExp) : []) expression) scp symTab classSymTab =  
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",size,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                   let typeIndexExp = (expressionTypeChecker scp innerExp symTab classSymTab)
                                            in case typeIndexExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  
                                                        case (preProcessExpression scp expression symTab classSymTab) of
                                                            Just (TypePrimitive primExp _) -> primExp == prim
                                                            _ -> False 
                                                   Right (TypePrimitive PrimitiveInteger []) -> 
                                                         case (preProcessExpression scp expression symTab classSymTab) of
                                                            Just (TypePrimitive primExp _) -> primExp == prim
                                                            _ -> False  
                                                   _ -> False
                            else False
                        Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    let typeIndexExp = (expressionTypeChecker scp innerExp symTab classSymTab)
                                            in case typeIndexExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  
                                                        case (preProcessExpression scp expression symTab classSymTab) of
                                                            Just (TypeClassId classId _) -> classId == classIdentifier
                                                            _ -> False 
                                                   Right (TypePrimitive PrimitiveInteger []) -> 
                                                         case (preProcessExpression scp expression symTab classSymTab) of
                                                            Just (TypeClassId classId _) -> classId == classIdentifier
                                                            _ -> False  
                                                   _ -> False
                            else False
                        _ -> False

isAssignmentOk  (AssignmentArrayExpression identifier ((ArrayAccessExpression innerExpRow) : (ArrayAccessExpression innerExpCol)  : []) expression) scp symTab classSymTab = 
                     case (Map.lookup identifier symTab) of
                        Just (SymbolVar (TypePrimitive prim (("[",sizeRows,"]") : ("[",sizeCols,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    let typeRowExp = (expressionTypeChecker scp innerExpRow symTab classSymTab)
                                        typeColExp = (expressionTypeChecker scp innerExpCol symTab classSymTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  
                                                    case (preProcessExpression scp expression symTab classSymTab) of 
                                                        Just (TypePrimitive primAssignment _) -> 
                                                            primAssignment == prim
                                                        _ -> False
                                                   Right (TypePrimitive PrimitiveInteger [])  ->  
                                                    case (preProcessExpression scp expression symTab classSymTab) of 
                                                        Just (TypePrimitive primAssignment _) -> 
                                                            primAssignment == prim
                                                        _ -> False
                                                   _ -> False
                                                else False
                            else False
                        Just (SymbolVar (TypeClassId classIdentifier (("[",sizeRows,"]") : ("[",sizeCols,"]") : [])) varScp _) -> 
                            if (varScp >= scp) 
                                then
                                    let typeRowExp = (expressionTypeChecker scp innerExpRow symTab classSymTab)
                                        typeColExp = (expressionTypeChecker scp innerExpCol symTab classSymTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  
                                                    case (preProcessExpression scp expression symTab classSymTab) of 
                                                        Just (TypeClassId classIdAssignment _) -> 
                                                            classIdentifier == classIdAssignment
                                                        _ -> False
                                                   Right (TypePrimitive PrimitiveInteger []) ->  
                                                    case (preProcessExpression scp expression symTab classSymTab) of 
                                                        Just (TypeClassId classIdAssignment _) -> 
                                                            classIdentifier == classIdAssignment
                                                        _ -> False
                                                   _ -> False
                                                else False
                            else False
                        _ -> False 

isAssignmentOk _ _ _ _ = False

insertIdentifiers :: [Identifier] -> Symbol -> TC
insertIdentifiers [] _  = return ()
insertIdentifiers (identifier : ids) symbol = 
                                    do 
                                        insertInSymbolTable identifier symbol 
                                        insertIdentifiers ids symbol


insertInSymbolTable :: Identifier -> Symbol -> TC
insertInSymbolTable identifier symbol  = 
                                                -- Si esta ese identificador en la tabla de simbolos, entonces regreso error
                                                do 
                                                    tcState <- get
                                                    let (symTab,classSymTab,aMap) = getTCState tcState 
                                                    if Map.member identifier symTab
                                                      then lift $ throwError (IdentifierDeclared identifier "")
                                                      else do 
                                                              let newSymTab = (Map.insert identifier symbol symTab)
                                                              modify $ \s -> (s {tcSymTab = newSymTab})

-- Aqui checamos que la asignacion de un una lista de literales o variables sea del tipo receptor
checkLiteralOrVariablesAndDataTypes :: Scope -> Type -> [LiteralOrVariable] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes _ _ [] _ = True
checkLiteralOrVariablesAndDataTypes scp dataType (litVar : litVars) symTab =  
                            if (checkLiteralOrVariableInSymbolTable scp litVar symTab) &&  (checkArrayAssignment scp dataType litVar symTab)
                                then checkLiteralOrVariablesAndDataTypes scp dataType litVars symTab
                                else False -- Alguna literal o variable asignada no existe, o bien, el tipo de dato que se esta asignando no concuerda con la declaracion

checkLiteralOrVariableInSymbolTable :: Scope -> LiteralOrVariable -> SymbolTable  -> Bool
checkLiteralOrVariableInSymbolTable scp (VarIdentifier identifier) symTab =  
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar _ varScp _) ->
                                        varScp >= scp
                                    _ -> False
checkLiteralOrVariableInSymbolTable _ _ _  = True -- Si es otra cosa que var identifier, entonces regresamos true, o sea, sin error

checkTypeExistance :: Type -> ClassSymbolTable -> Bool
checkTypeExistance (TypeClassId classIdentifier _) classTab = 
                                                  case (Map.lookup classIdentifier classTab) of
                                                  Just _ -> True -- Si existe esa clase
                                                  _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkTypeExistance _ _ = True -- Todos lo demas regresa true


checkLiteralOrVariablesAndDataTypes2D :: Scope -> Type -> [[LiteralOrVariable]] -> SymbolTable -> Bool
checkLiteralOrVariablesAndDataTypes2D _ _ [] _ = True
checkLiteralOrVariablesAndDataTypes2D scp dataType (listOfLitVars : rest) symTab =  
                            if (checkLiteralOrVariablesAndDataTypes scp dataType listOfLitVars symTab)  
                                then checkLiteralOrVariablesAndDataTypes2D scp dataType rest symTab
                                else False -- Alguna literal o variable asignada no existe, o bien, el tipo de dato que se esta asignando no concuerda con la declaracion

-- Podriamos necesitar preprocesar una expresion en busqueda de un literal or variable que sea un tipo de alguna clase
preProcessExpression :: Scope -> Expression -> SymbolTable -> ClassSymbolTable -> Maybe Type
preProcessExpression scp (ExpressionLitVar (VarIdentifier identifier)) symTab _ =
                         case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier accessExpression) varScp _)  
                                        | varScp >= scp ->
                                            Just (TypeClassId classIdentifier accessExpression)
                                        | otherwise -> Nothing
                                    Just (SymbolVar (TypePrimitive prim accessExpression) varScp _)  
                                        | varScp >= scp ->
                                            Just (TypePrimitive prim accessExpression)
                                        | otherwise -> Nothing
                                    _ -> Nothing

preProcessExpression scp (ExpressionFuncCall functionCall) symTab classSymTab = if (analyzeFunctionCall functionCall scp symTab classSymTab) then 
                                                                                    functionCallType functionCall scp symTab classSymTab
                                                                                else Nothing   
preProcessExpression scp (ExpressionVarArray identifier ((ArrayAccessExpression expressionIndex) : [])) symTab classSymTab =
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) varScp _)  
                                       | varScp >= scp ->
                                            let typeIndexExp = (expressionTypeChecker scp expressionIndex symTab classSymTab )
                                            in case typeIndexExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  Just (TypePrimitive prim [])
                                                   Right (TypePrimitive PrimitiveInteger []) ->  Just (TypePrimitive prim [])
                                                   _ -> Nothing
                                       | otherwise -> Nothing
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",size,"]") : []) ) varScp _)  
                                       | varScp >= scp ->
                                            let typeIndexExp = (expressionTypeChecker scp expressionIndex symTab classSymTab)
                                            in case typeIndexExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  Just (TypeClassId classIdentifier [])
                                                   Right (TypePrimitive PrimitiveInteger []) ->  Just (TypeClassId classIdentifier [])
                                                   _ -> Nothing
                                       | otherwise -> Nothing
                                    _ -> Nothing
preProcessExpression scp (ExpressionVarArray identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : [])) symTab classSymTab =
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [])) varScp _) 
                                       | varScp >= scp ->
                                            let typeRowExp = (expressionTypeChecker scp rowExp symTab classSymTab)
                                                typeColExp = (expressionTypeChecker scp colExp symTab classSymTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  Just (TypePrimitive prim [])
                                                   Right (TypePrimitive PrimitiveInteger []) ->  Just (TypePrimitive prim [])
                                                   _ -> Nothing
                                                else Nothing
                                       | otherwise -> Nothing
                                    Just (SymbolVar (TypeClassId classIdentifier (("[",rows,"]") : ("[",cols,"]") : [])) varScp _) 
                                       | varScp >= scp ->
                                            let typeRowExp = (expressionTypeChecker scp rowExp symTab classSymTab)
                                                typeColExp = (expressionTypeChecker scp colExp symTab classSymTab)
                                            in if (typeColExp == typeRowExp) then
                                                case typeRowExp of
                                                   Right (TypePrimitive PrimitiveInt []) ->  Just (TypeClassId classIdentifier [])
                                                   Right (TypePrimitive PrimitiveInteger []) ->  Just (TypeClassId classIdentifier [])
                                                   _ -> Nothing
                                                else Nothing
                                      | otherwise -> Nothing
                                    _ -> Nothing
preProcessExpression scp expression symTab classSymTab = case (expressionTypeChecker scp expression symTab classSymTab) of
                                                Right dtExp -> (Just dtExp)
                                                _ -> Nothing

