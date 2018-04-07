module Expression where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty
import SymbolTable
import ClassSymbolTable
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

instance TypeSemant Type where
    -- OPERATIONS: +
    (TypePrimitive PrimitiveInt []) `semantadd` (TypePrimitive PrimitiveMoney []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveMoney []) `semantadd` (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveInt []) `semantadd` (TypePrimitive PrimitiveDouble []) = (Right (TypePrimitive PrimitiveDouble []))
    (TypePrimitive PrimitiveDouble []) `semantadd` (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive PrimitiveDouble []))

    (TypePrimitive PrimitiveInteger []) `semantadd` (TypePrimitive PrimitiveMoney []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveMoney []) `semantadd` (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveInteger []) `semantadd` (TypePrimitive PrimitiveDouble []) = (Right (TypePrimitive PrimitiveDouble []))
    (TypePrimitive PrimitiveDouble []) `semantadd` (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive PrimitiveDouble []))
    (TypePrimitive (PrimitiveString) []) `semantadd` (TypePrimitive (PrimitiveString) []) = (Right (TypePrimitive (PrimitiveString) []))
    (TypePrimitive PrimitiveBool []) `semantadd` (TypePrimitive PrimitiveBool []) = (Left (("Addition operation not supported between booleans ")))
    (TypePrimitive prim1 []) `semantadd` (TypePrimitive prim2 []) = if (prim1 /= prim2) then (Left (("Addition operation not supported between primitive ") ++ (show prim1) ++ (" and primitive ") ++ (show prim2)))
                                                                     else (Right (TypePrimitive prim1 []))
    type1 `semantadd` type2 = (Left (("Addition operation not supported between type ") ++ (show type1) ++ (" and type ") ++ (show type2)))

    -- OPERATIONS: / * - ^
    (TypePrimitive PrimitiveInt []) `semantarithmetic` (TypePrimitive PrimitiveMoney []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveMoney []) `semantarithmetic` (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveInt []) `semantarithmetic` (TypePrimitive PrimitiveDouble []) = (Right (TypePrimitive PrimitiveDouble []))
    (TypePrimitive PrimitiveDouble []) `semantarithmetic` (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive PrimitiveDouble []))

    (TypePrimitive PrimitiveInteger []) `semantarithmetic` (TypePrimitive PrimitiveMoney []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveMoney []) `semantarithmetic` (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive PrimitiveMoney []))
    (TypePrimitive PrimitiveInteger []) `semantarithmetic` (TypePrimitive PrimitiveDouble []) = (Right (TypePrimitive PrimitiveDouble []))
    (TypePrimitive PrimitiveDouble []) `semantarithmetic` (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive PrimitiveDouble []))
    (TypePrimitive PrimitiveBool []) `semantarithmetic` (TypePrimitive PrimitiveBool []) = (Left (("Arithmetic operation not supported between booleans ")))
    (TypePrimitive PrimitiveString []) `semantarithmetic` (TypePrimitive PrimitiveString []) = (Left (("Arithmetic operation not supported between strings ")))
    (TypePrimitive prim1 []) `semantarithmetic` (TypePrimitive prim2 []) = if (prim1 /= prim2) then (Left (("Arithmetic operation not supported between primitive ") ++ (show prim1) ++ (" and primitive ") ++ (show prim2)))
                                                                     else (Right (TypePrimitive prim1 []))
    type1 `semantarithmetic` type2 = (Left (("Arithmetic operation not supported between type ") ++ (show type1) ++ (" and type ") ++ (show type2)))


    -- OPERATIONS: == !=
    (TypePrimitive prim1 []) `semantequivalence` (TypePrimitive prim2 []) = if (prim1 /= prim2) then (Left (("Equivalence operation not supported between primitive ") ++ (show prim1) ++ (" and primitive ") ++ (show prim2)))
                                                                     else (Right (TypePrimitive (PrimitiveBool) []))
    (TypeClassId classId1 []) `semantequivalence` (TypeClassId classId2 []) = if (classId1 /= classId2) then (Left (("Object equivalence operation not supported for objects of different classes ") ++ classId1 ++ (" and ") ++ classId2))
                                                                                else (Right (TypePrimitive (PrimitiveBool) []))
    type1 `semantequivalence` type2 = (Left (("Equivalence operation not supported between type ") ++ (show type1) ++ (" and type ") ++ (show type2)))


    -- OPERATIONS: > < <= >=
    (TypePrimitive PrimitiveBool []) `semantrelational` (TypePrimitive PrimitiveBool []) = (Left (("Relational operation not supported between booleans ")))
    (TypePrimitive PrimitiveString []) `semantrelational` (TypePrimitive PrimitiveString []) = (Left (("Relational operation not supported between strings ")))
    (TypePrimitive prim1 []) `semantrelational` (TypePrimitive prim2 []) = if (prim1 /= prim2) then (Left (("Relational operation not valid between ") ++ (show prim1) ++ (" and primitive ") ++ (show prim2)))
                                                                     else (Right (TypePrimitive (PrimitiveBool) []))
    type1 `semantrelational` type2 = (Left (("Relational operation not supported between type ") ++ (show type1) ++ (" and type ") ++ (show type2)))

    -- OPERATIONS: && ||
    (TypePrimitive PrimitiveBool []) `semantbooleanrelational` (TypePrimitive PrimitiveBool []) = (Right (TypePrimitive (PrimitiveBool) [])) 
    type1 `semantbooleanrelational` type2 = (Left (("Boolean relational operation is only supported for boolean primitives ")))

    -- OPERATIONS: not 
    semantnot (TypePrimitive PrimitiveBool []) = (Right (TypePrimitive (PrimitiveBool) [])) 
    semantnot type1 = (Left (("Boolean not operation is only supported for boolean primitives ")))

    -- OPERATIONS: neg
    semantneg (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive (PrimitiveInt) []))
    semantneg (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive (PrimitiveInteger) []))
    semantneg (TypePrimitive PrimitiveMoney []) = (Right (TypePrimitive (PrimitiveMoney) []))
    semantneg (TypePrimitive PrimitiveDouble []) = (Right (TypePrimitive (PrimitiveDouble) [])) 
    semantneg type1 = (Left (("Negation operation is only supported for integer,int,money and double primitives ")))

    -- OPERATIONS: mod
    (TypePrimitive PrimitiveInteger []) `semantmod` (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive PrimitiveInteger []))
    (TypePrimitive PrimitiveInt []) `semantmod` (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive PrimitiveInt []))
    (TypePrimitive PrimitiveInteger []) `semantmod` (TypePrimitive PrimitiveInt []) = (Right (TypePrimitive PrimitiveInteger []))
    (TypePrimitive PrimitiveInt []) `semantmod` (TypePrimitive PrimitiveInteger []) = (Right (TypePrimitive PrimitiveInt []))
    type1 `semantmod` type2 = (Left (("Mod operation not supported between type ") ++ (show type1) ++ (" and type ") ++ (show type2)))


expressionTypeChecker :: Scope  -> Expression -> SymbolTable -> ClassSymbolTable ->  (Either String Type)
expressionTypeChecker scp (ExpressionMult exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                in checkBinaryOperation semantarithmetic typeExp1 typeExp2 
                                                              

expressionTypeChecker scp (ExpressionDiv exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                in checkBinaryOperation semantarithmetic typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionPow exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                in checkBinaryOperation semantarithmetic typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionPars exp) symTab classSymTab = expressionTypeChecker scp exp symTab classSymTab
expressionTypeChecker scp (ExpressionGreater exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantrelational typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionLower exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantrelational typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionGreaterEq exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantrelational typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionLowerEq exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantrelational typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionEqEq exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantequivalence typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionNotEq exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantequivalence typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionAnd exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantbooleanrelational typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionOr exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                    in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                    in checkBinaryOperation semantbooleanrelational typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionPlus exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                in checkBinaryOperation semantadd typeExp1 typeExp2
expressionTypeChecker scp (ExpressionMinus exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                in checkBinaryOperation semantarithmetic typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionMod exp1 exp2) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp1 symTab classSymTab
                                                                in let typeExp2 = expressionTypeChecker scp exp2 symTab classSymTab
                                                                in checkBinaryOperation semantarithmetic typeExp1 typeExp2 
expressionTypeChecker scp (ExpressionNot exp) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp symTab classSymTab
                                                                in checkUnaryOperation semantnot typeExp1 
expressionTypeChecker scp (ExpressionNeg exp) symTab classSymTab = let typeExp1 = expressionTypeChecker scp exp symTab classSymTab
                                                                in checkUnaryOperation semantneg typeExp1 
expressionTypeChecker scp (ExpressionLitVar litVar) symTab classSymTab = checkDataTypeOfLitVar scp litVar symTab
expressionTypeChecker scp (ExpressionFuncCall funcCall) symTab classSymTab = 
                                                                    if (analyzeFunctionCall funcCall scp symTab classSymTab) then 
                                                                        case functionCallType funcCall scp symTab classSymTab of
                                                                          Just (TypePrimitive prim []) -> (Right (TypePrimitive prim []))
                                                                          Just (TypeClassId classId []) -> (Right (TypeClassId classId []))
                                                                          _ -> (Left (("Function call return type in expression not supported "))) 
                                                                    else  (Left (("Function identifier is not known"))) 

expressionTypeChecker scp (ExpressionVarArray identifier ((ArrayAccessExpression expression) : [])) symTab classSymTab = case (expressionTypeChecker scp expression symTab classSymTab) of 
                                                                                                        Right (TypePrimitive PrimitiveInt []) -> checkArrayID scp identifier symTab 1
                                                                                                        Right (TypePrimitive PrimitiveInteger []) -> checkArrayID scp identifier symTab 1
                                                                                                        _ -> (Left (("Was expecting an integer in expression" ++ identifier))) 
expressionTypeChecker scp (ExpressionVarArray identifier ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[])) symTab classSymTab = 
                                                  case (expressionTypeChecker scp expression1 symTab classSymTab) of 
                                                     Right (TypePrimitive PrimitiveInt []) -> 
                                                            case (expressionTypeChecker scp expression2 symTab classSymTab) of
                                                              Right (TypePrimitive PrimitiveInt []) -> checkArrayID scp identifier symTab 2
                                                              Right (TypePrimitive PrimitiveInteger []) -> checkArrayID scp identifier symTab 2
                                                              _ -> (Left (("Was expecting an integer in column expression in " ++ identifier))) 
                                                     Right (TypePrimitive PrimitiveInteger []) -> 
                                                            case (expressionTypeChecker scp expression2 symTab classSymTab) of
                                                              Right (TypePrimitive PrimitiveInteger []) -> checkArrayID scp identifier symTab 2
                                                              Right (TypePrimitive PrimitiveInt []) -> checkArrayID scp identifier symTab 2
                                                              _ -> (Left (("Was expecting an integer in column expression" ++ identifier) )) 
                                                     _ -> (Left (("Was expecting an integer in row expression" ++ identifier))) 


checkBinaryOperation :: (Type -> Type -> (Either String Type)) -> (Either String Type) -> (Either String Type) -> (Either String Type)
checkBinaryOperation f dt1 dt2 = case dt1 of 
                                            Left err -> dt1
                                            Right type1 -> case dt2 of 
                                                              Left err2 -> dt2
                                                              Right type2 -> (f type1 type2)

checkUnaryOperation :: (Type -> (Either String Type)) -> (Either String Type) -> (Either String Type)
checkUnaryOperation f dt1 = case dt1 of 
                                            Left err -> dt1
                                            Right type1 -> (f type1)


checkArrayID :: Scope -> Identifier -> SymbolTable -> Int -> (Either String Type)
checkArrayID scp identifier symTab dimension= case (Map.lookup identifier symTab) of
                             Just (SymbolVar dataType varScp _) 
                               | varScp >= scp -> 
                                   case dataType of 
                                     TypePrimitive prim arrayDeclaration | (length arrayDeclaration) == dimension -> (Right (TypePrimitive prim []))
                                                                         | otherwise -> (Left (("Wrong dimension for array" ++ identifier) )) 
                                     TypeClassId classId arrayDeclaration | (length arrayDeclaration) == dimension -> (Right (TypeClassId classId []))
                                                                         | otherwise -> (Left (("Wrong dimension for array" ++ identifier) )) 
                                     
                               | otherwise -> (Left (("Out of scope " ++ identifier) )) 
                             _ -> (Left (("Variable not declared" ++ identifier) )) 



checkDataTypeOfLitVar :: Scope -> LiteralOrVariable -> SymbolTable -> (Either String Type)
checkDataTypeOfLitVar scp (VarIdentifier identifier) symTab = 
                                  case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim []) varScp _)
                                      | varScp >= scp -> (Right (TypePrimitive prim []))
                                      | otherwise -> (Left (("Variable " ++ identifier) ++ "Out of scope"))
                                    Just (SymbolVar (TypeClassId classId []) varScp _)
                                      | varScp >= scp -> (Right (TypeClassId classId []))
                                      | otherwise -> (Left (("Variable " ++ identifier) ++ "Out of scope"))
                                    _ -> (Left ("Variable not found " ++ identifier))

checkDataTypeOfLitVar scp (IntegerLiteral int) symTab = (Right (TypePrimitive PrimitiveInteger []))
checkDataTypeOfLitVar scp (DecimalLiteral dec) symTab = (Right (TypePrimitive PrimitiveMoney []))
checkDataTypeOfLitVar scp (StringLiteral int) symTab = (Right (TypePrimitive PrimitiveString []))
checkDataTypeOfLitVar scp (BoolLiteral _) symTab = (Right (TypePrimitive PrimitiveBool []))

checkDataTypeOfVar ::  LiteralOrVariable -> SymbolTable -> Type
checkDataTypeOfVar  (VarIdentifier identifier) symTab = 
                                  case (Map.lookup identifier symTab) of
                                    Just (SymbolVar dataType varScp _) -> dataType


compareListOfTypesWithFuncCall :: Scope -> [Type] -> [Params] -> SymbolTable -> ClassSymbolTable -> Bool
compareListOfTypesWithFuncCall _ [] [] _ _ = True
compareListOfTypesWithFuncCall _ [] (sp : sps) _ _ = False
compareListOfTypesWithFuncCall _ (rpType : rps) [] _ _ = False
compareListOfTypesWithFuncCall scp (rpType : rps) (sp : sps) symTab classSymTab = 
                        case sp of 
                            (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression innerExp) : []))) ->
                                -- Checamos que sea un arreglo
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",size,"]") : []) ) varScp _) 
                                       | varScp >= scp -> 
                                            case (expressionTypeChecker scp innerExp symTab classSymTab) of 
                                                Right (TypePrimitive PrimitiveInt []) ->  (TypePrimitive prim []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                Right (TypePrimitive PrimitiveInteger []) ->  (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                _ -> False
                                            
                                       | otherwise -> False
                                    Just (SymbolVar (TypeClassId classId (("[",size,"]") : []) ) varScp _) 
                                       | varScp >= scp -> 
                                                case (expressionTypeChecker scp innerExp symTab classSymTab) of 
                                                    Right (TypePrimitive PrimitiveInt []) -> (TypeClassId classId []) == rpType 
                                                        && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    Right (TypePrimitive PrimitiveInteger []) -> (TypeClassId classId []) == rpType 
                                                        && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    _ -> False
                                       | otherwise -> False
                                    _ -> False
                            (ParamsExpression (ExpressionVarArray identifier ((ArrayAccessExpression rowExp) : (ArrayAccessExpression colExp)  : []))) ->
                                -- Checamos que sea una matriz ese identificador
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive prim (("[",rows,"]") : ("[",cols,"]") : [])) varScp _)
                                        | varScp >= scp -> 
                                            let rowExpType = expressionTypeChecker scp rowExp symTab classSymTab
                                                colExpType = expressionTypeChecker scp colExp symTab classSymTab
                                            in if(rowExpType == colExpType) 
                                                then case rowExpType of 
                                                    Right (TypePrimitive PrimitiveInt []) -> (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    Right (TypePrimitive PrimitiveInteger []) -> (TypePrimitive prim []) == rpType
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    _ -> False
                                                else False
                                        | otherwise -> False
                                    Just (SymbolVar (TypeClassId classId (("[",rows,"]") : ("[",cols,"]") : [])) varScp _)
                                        | varScp >= scp -> 
                                            let rowExpType = expressionTypeChecker scp rowExp symTab classSymTab
                                                colExpType = expressionTypeChecker scp colExp symTab classSymTab
                                            in if(rowExpType == colExpType) 
                                                then case rowExpType of 
                                                    Right (TypePrimitive PrimitiveInt []) -> (TypeClassId classId []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    Right (TypePrimitive PrimitiveInteger []) -> (TypeClassId classId []) == rpType 
                                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                                                    _ -> False
                                                else False
                                        | otherwise -> False
                                    _ -> False
                            (ParamsExpression (ExpressionLitVar ((VarIdentifier identifier)))) ->
                                            checkDataTypes scp rpType (VarIdentifier identifier) symTab 
                                            && compareListOfTypesWithFuncCall scp rps sps symTab classSymTab
                            (ParamsExpression expression) -> 
                                case (expressionTypeChecker scp expression symTab classSymTab) of
                                    (Right expType) -> expType == rpType && (compareListOfTypesWithFuncCall scp rps sps symTab classSymTab)
                                    Left err -> False

analyzeFunctionCall :: FunctionCall -> Scope -> SymbolTable -> ClassSymbolTable -> Bool
analyzeFunctionCall (FunctionCallVar funcIdentifier callParams) scp symTab classTab = 
                            case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    let funcParamTypes = map (\p -> fst p) params
                                                    in (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab classTab)         
                                    _ -> False
analyzeFunctionCall (FunctionCallObjMem (ObjectMember objectIdentifier functionIdentifier) callParams) scp symTab classTab = 
                           case (Map.lookup objectIdentifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _ ) -> 
                                        if objScp >= scp 
                                            then case (Map.lookup classIdentifier classTab) of
                                                Just symbolTableOfClass ->
                                                        case (Map.lookup functionIdentifier symbolTableOfClass) of
                                                            -- Si y solo si es publica la funcion, la accedemos
                                                            Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                let funcParamTypes = map (\p -> fst p) params
                                                                in (compareListOfTypesWithFuncCall scp funcParamTypes callParams symTab classTab)  
                                                            _ -> False   
                                                _ -> False
                                        else False
                                    _ -> False

functionCallType :: FunctionCall -> Scope -> SymbolTable -> ClassSymbolTable -> Maybe Type
functionCallType (FunctionCallVar funcIdentifier callParams) scp symTab classTab = 
                            case (Map.lookup funcIdentifier symTab) of
                                    Just (SymbolFunction params returnTypeFunc _ _ _ _ _) -> 
                                                    returnTypeFunc         
                                    _ -> Nothing
functionCallType (FunctionCallObjMem (ObjectMember objectIdentifier functionIdentifier) callParams) scp symTab classTab = 
                           case (Map.lookup objectIdentifier symTab) of
                                    Just (SymbolVar (TypeClassId classIdentifier _) objScp _ ) -> 
                                        if objScp >= scp 
                                            then case (Map.lookup classIdentifier classTab) of
                                                Just symbolTableOfClass ->
                                                        case (Map.lookup functionIdentifier symbolTableOfClass) of
                                                            -- Si y solo si es publica la funcion, la accedemos
                                                            Just (SymbolFunction params returnTypeFunc _ _ _ (Just True) _) ->
                                                                returnTypeFunc 
                                                            _ -> Nothing   
                                                _ -> Nothing
                                        else Nothing
                                    _ -> Nothing

-- Aqui checamos si el literal or variable que se esta asignando al arreglo sea del tipo indicado
-- es decir, en Humano [10] humanos = [h1,h2,h3,h4] checa que h1,h2,h3 y h4 sean del tipo humano
checkArrayAssignment :: Scope -> Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkArrayAssignment scp (TypePrimitive prim arrayDeclaration) (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypePrimitive primVar []) varScp _) 
                                        | varScp >= scp -> 
                                                primVar == prim
                                        | otherwise -> False
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkArrayAssignment scp (TypeClassId classIdentifier arrayDeclaration) (VarIdentifier identifier) symTab = 
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar (TypeClassId classId []) varScp _)  
                                        |  varScp >= scp ->  
                                                classId == classIdentifier
                                        | otherwise -> False                                        
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkArrayAssignment scp dataType litOrVar symTab  = checkDataTypes scp dataType litOrVar symTab

-- Aqui checamos si el literal or variable que se esta dando esta de acuerdo al que se esta asignando! O sea,
-- no es valido decir Int i = 1; Money m = i; 
checkDataTypes :: Scope -> Type -> LiteralOrVariable -> SymbolTable -> Bool 
checkDataTypes scp dType (VarIdentifier identifier) symTab =  
                                case (Map.lookup identifier symTab) of
                                    Just (SymbolVar dataType varScp _) 
                                        | varScp >= scp -> dataType == dType -- Si son iguales, regresamos true
                                        | otherwise -> False
                                    _ -> False -- El identificador que se esta asignando no esta en ningun lado
checkDataTypes _ (TypePrimitive (PrimitiveInt) _) (IntegerLiteral _) _  = True
checkDataTypes _ (TypePrimitive (PrimitiveDouble) _) (DecimalLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveMoney) _) (DecimalLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveString) _) (StringLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveInteger) _) (IntegerLiteral _) _ = True
checkDataTypes _ (TypePrimitive (PrimitiveBool) _) (BoolLiteral _) _ = True
checkDataTypes _ _ _ _ = False -- Todo lo demas, falso
