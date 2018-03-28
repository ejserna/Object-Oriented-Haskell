module ExpressionOptimizer where 
import Data.Decimal
import DataTypes
import DataTypes
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)


checkIfString :: Expression -> Bool
checkIfString (ExpressionLitVar (StringLiteral str)) = True
checkIfString (ExpressionLitVar (DecimalLiteral dec)) = False
checkIfString (ExpressionLitVar (IntegerLiteral int)) = False
checkIfString (ExpressionPlus exp1 exp2) = checkIfString exp1

-- En caso que se pueda reducir la expresion, se hace
reduceExpression :: Expression -> Expression
reduceExpression exp = let optimizedExp = optimizeExpression exp
                        in if exp == optimizedExp
                            then exp
                        else reduceExpression optimizedExp

optimizeExpression :: Expression -> Expression 
optimizeExpression (ExpressionMult exp1 exp2) = arithmeticFunctionExpression (|*|) (*) exp1 exp2
optimizeExpression (ExpressionPlus exp1 exp2) = if (isExpressionString exp1 || isExpressionString exp2)
                                                 then functionConcatString (|+|) (++) exp1 exp2
                                                 else arithmeticFunctionExpression (|+|) (+) exp1 exp2
optimizeExpression (ExpressionPow exp1 exp2) = powFunction (|^|) (**)  exp1 exp2
optimizeExpression (ExpressionDiv exp1 exp2) = arithmeticFunctionExpression (|/|) (/) exp1 exp2
optimizeExpression (ExpressionMinus exp1 exp2) = arithmeticFunctionExpression (|-|) (-) exp1 exp2
optimizeExpression (ExpressionPars exp) = optimizeExpression exp
optimizeExpression (ExpressionNeg exp) = arithmeticFunctionExpression (|-|) (-) (ExpressionLitVar (IntegerLiteral 0)) exp
optimizeExpression (ExpressionMod exp1 exp2) = modFunction (|%|) (mod) exp1 exp2
optimizeExpression (ExpressionGreater exp1 exp2) = arithmeticRelationalFunctionExpression (|>|) (>) exp1 exp2
optimizeExpression (ExpressionLower exp1 exp2) = arithmeticRelationalFunctionExpression (|<|) (<) exp1 exp2
optimizeExpression (ExpressionGreaterEq exp1 exp2) = arithmeticRelationalFunctionExpression (|>=|) (>=) exp1 exp2
optimizeExpression (ExpressionLowerEq exp1 exp2) = arithmeticRelationalFunctionExpression (|<=|) (<=) exp1 exp2
optimizeExpression (ExpressionEqEq exp1 exp2) = arithmeticRelationalFunctionExpression (|==|) (==) exp1 exp2
optimizeExpression (ExpressionNotEq exp1 exp2) = arithmeticRelationalFunctionExpression (|!=|) (/=) exp1 exp2
optimizeExpression (ExpressionAnd exp1 exp2) = booleanRelationalFunctionExpression (|&&|) (&&) exp1 exp2
optimizeExpression (ExpressionOr exp1 exp2) = booleanRelationalFunctionExpression (|-||-|) (||) exp1 exp2
optimizeExpression (ExpressionNot exp) = booleanUnaryFunctionExpression (|!|) (not) exp
optimizeExpression (ExpressionLitVar litOrVar) = (ExpressionLitVar litOrVar)
optimizeExpression (ExpressionFuncCall funcCall)  =  (ExpressionFuncCall (optimizeExpsInFunctionCall funcCall))
optimizeExpression (ExpressionVarArray identifier ((ArrayAccessExpression expression) : [])) = 
                                let optimizedExp = reduceExpression expression
                                in (ExpressionVarArray identifier ((ArrayAccessExpression optimizedExp) : []))
optimizeExpression (ExpressionVarArray identifier ((ArrayAccessExpression expression1) : (ArrayAccessExpression expression2) :[]))  = 
                                let optimizedExp1 = reduceExpression expression1
                                    optimizedExp2 = reduceExpression expression2 
                                in (ExpressionVarArray identifier ((ArrayAccessExpression optimizedExp1) : (ArrayAccessExpression optimizedExp2) : []))


optimizeExpsInFunctionCall :: FunctionCall -> FunctionCall 
optimizeExpsInFunctionCall (FunctionCallVar identifier callParams) = 
                            let optimizedParams = optimizeExpsInParams callParams
                              in (FunctionCallVar identifier optimizedParams)
optimizeExpsInFunctionCall (FunctionCallObjMem (ObjectMember obj attr) callParams) = 
                            let optimizedParams = optimizeExpsInParams callParams
                              in (FunctionCallObjMem (ObjectMember obj attr) optimizedParams)

optimizeExpsInParams :: [Params] -> [Params]
optimizeExpsInParams []  = []
optimizeExpsInParams ((ParamsExpression exp) : params) =
        let reducedExp = (ParamsExpression (reduceExpression exp))
            in reducedExp : (optimizeExpsInParams params)


arithmeticRelationalFunctionExpression :: (Expression -> Expression -> Expression) -> (Decimal -> Decimal -> Bool) -> Expression -> Expression -> Expression
arithmeticRelationalFunctionExpression _ f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = (ExpressionLitVar (BoolLiteral (f (intToDecimal int1) (intToDecimal int2))))
arithmeticRelationalFunctionExpression _ f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = (ExpressionLitVar (BoolLiteral (f dec1 dec2)))
arithmeticRelationalFunctionExpression f _ (ExpressionLitVar litOrVar) exp2 = (f (ExpressionLitVar litOrVar) (optimizeExpression exp2))
arithmeticRelationalFunctionExpression f _ exp1 (ExpressionLitVar litOrVar) = (f (optimizeExpression exp1) (ExpressionLitVar litOrVar) )
arithmeticRelationalFunctionExpression f _ exp1 exp2 =  (f (optimizeExpression exp1) (optimizeExpression exp2))

booleanRelationalFunctionExpression :: (Expression -> Expression -> Expression) -> (Bool -> Bool -> Bool) -> Expression -> Expression -> Expression
booleanRelationalFunctionExpression _ f (ExpressionLitVar (BoolLiteral bool1)) (ExpressionLitVar (BoolLiteral bool2)) = (ExpressionLitVar (BoolLiteral (f bool1 bool2)))
booleanRelationalFunctionExpression f _ (ExpressionLitVar litOrVar) exp2 = (f (ExpressionLitVar litOrVar) (optimizeExpression exp2))
booleanRelationalFunctionExpression f _ exp1 (ExpressionLitVar litOrVar) = (f (optimizeExpression exp1) (ExpressionLitVar litOrVar) )
booleanRelationalFunctionExpression f _ exp1 exp2 =  (f (optimizeExpression exp1) (optimizeExpression exp2))

booleanUnaryFunctionExpression :: (Expression -> Expression) -> (Bool -> Bool) -> Expression -> Expression
booleanUnaryFunctionExpression _ f (ExpressionLitVar (BoolLiteral bool1))  = (ExpressionLitVar (BoolLiteral (f bool1)))
booleanUnaryFunctionExpression f _ (ExpressionLitVar litOrVar) = (f (ExpressionLitVar litOrVar))
booleanUnaryFunctionExpression f _ exp1  =  (f (optimizeExpression exp1))

arithmeticFunctionExpression :: (Expression -> Expression -> Expression) -> (Decimal -> Decimal -> Decimal) -> Expression -> Expression -> Expression
arithmeticFunctionExpression _ f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = (ExpressionLitVar (IntegerLiteral (decToInt (f (intToDecimal int1) (intToDecimal int2)))))
arithmeticFunctionExpression _ f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = (ExpressionLitVar (DecimalLiteral (f dec1 (intToDecimal int1))))
arithmeticFunctionExpression _ f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = (ExpressionLitVar (DecimalLiteral (f (intToDecimal int1) dec1))) 
arithmeticFunctionExpression _ f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = (ExpressionLitVar (DecimalLiteral (f dec1 dec2)))
arithmeticFunctionExpression f _ (ExpressionLitVar litOrVar) exp2 = (f (ExpressionLitVar litOrVar) (optimizeExpression exp2))
arithmeticFunctionExpression f _ exp1 (ExpressionLitVar litOrVar) = (f (optimizeExpression exp1) (ExpressionLitVar litOrVar) )
arithmeticFunctionExpression f _ exp1 exp2 =  (f (optimizeExpression exp1) (optimizeExpression exp2))


powFunction :: (Expression -> Expression -> Expression) -> (Double -> Double -> Double) -> Expression -> Expression -> Expression
powFunction _ f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = (ExpressionLitVar (IntegerLiteral ((int1 ^ int2))))
powFunction _ f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = (ExpressionLitVar (DecimalLiteral (doubleToDecimal (f (decToDouble dec1) (intToDouble int1)))))
powFunction _ f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = (ExpressionLitVar (DecimalLiteral (doubleToDecimal (f (intToDouble int1) (decToDouble dec1)))))
powFunction _ f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = (ExpressionLitVar (DecimalLiteral (doubleToDecimal (f (decToDouble dec1) (decToDouble dec2)))))
powFunction f _ (ExpressionLitVar litOrVar) exp2 = (f (ExpressionLitVar litOrVar) (optimizeExpression exp2))
powFunction f _ exp1 (ExpressionLitVar litOrVar) = (f (optimizeExpression exp1) (ExpressionLitVar litOrVar) )
powFunction f _ exp1 exp2 =  (f (optimizeExpression exp1) (optimizeExpression exp2))

modFunction :: (Expression -> Expression -> Expression) -> (Integer -> Integer -> Integer) -> Expression -> Expression -> Expression
modFunction _ f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = (ExpressionLitVar (IntegerLiteral (f int1 int2)))
modFunction f _ (ExpressionLitVar litOrVar) exp2 = (f (ExpressionLitVar litOrVar) (optimizeExpression exp2))
modFunction f _ exp1 (ExpressionLitVar litOrVar) = (f (optimizeExpression exp1) (ExpressionLitVar litOrVar) )
modFunction f _ exp1 exp2 =  (f (optimizeExpression exp1) (optimizeExpression exp2))


isExpressionString :: Expression -> Bool
isExpressionString (ExpressionLitVar (StringLiteral str)) = True
isExpressionString (ExpressionLitVar litOrVar) = False
isExpressionString (ExpressionPlus exp1 exp2) = isExpressionString exp1 || 
                                                isExpressionString exp2
isExpressionString _ = False


functionConcatString :: (Expression -> Expression -> Expression) -> ([Char] -> [Char] -> [Char]) -> Expression -> Expression -> Expression
functionConcatString _ f  (ExpressionLitVar (StringLiteral str1)) (ExpressionLitVar (StringLiteral str2)) = (ExpressionLitVar (StringLiteral (f str1 str2)))
functionConcatString f _ (ExpressionLitVar litOrVar) exp2 = (f (ExpressionLitVar litOrVar) (optimizeExpression exp2))
functionConcatString f _ exp1 (ExpressionLitVar litOrVar) = (f (optimizeExpression exp1) (ExpressionLitVar litOrVar) )
functionConcatString f _ exp1 exp2 = optimizeExpression (f (optimizeExpression exp1) (optimizeExpression exp2))
