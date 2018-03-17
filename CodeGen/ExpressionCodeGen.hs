module ExpressionCodeGen where
import Data.Decimal
import DataTypes
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

data ExpResult
    = ResultDecimal Decimal
    | ResultBool Bool
    | ResultString String
    | ResultInteger Integer
    deriving (Show,Eq)

processStart :: Expression -> ExpResult
processStart (ExpressionMult exp1 exp2) = case (checkIfDecimal exp1) of
                                            True -> (ResultDecimal (expressionEvaluationDec (ExpressionMult exp1 exp2)))
                                            False -> case (checkIfDecimal exp2) of
                                                      True -> (ResultDecimal (expressionEvaluationDec (ExpressionMult exp1 exp2)))
                                                      False -> (ResultInteger (expressionEvaluationInt (ExpressionMult exp1 exp2)))
processStart (ExpressionPlus exp1 exp2) = case (checkIfString exp1) of
                                          True -> (ResultString (expressionConcatString (ExpressionPlus exp1 exp2)))
                                          False -> case (checkIfDecimal exp1) of
                                                    True -> (ResultDecimal (expressionEvaluationDec (ExpressionPlus exp1 exp2)))
                                                    False -> case (checkIfDecimal exp2) of
                                                              True -> (ResultDecimal (expressionEvaluationDec (ExpressionPlus exp1 exp2)))
                                                              False -> (ResultInteger (expressionEvaluationInt (ExpressionPlus exp1 exp2)))
processStart (ExpressionPow exp1 exp2) = case (checkIfDecimal exp1) of
                                            True -> (ResultDecimal (expressionEvaluationDec (ExpressionPow exp1 exp2)))
                                            False -> case (checkIfDecimal exp2) of
                                                        True -> (ResultDecimal (expressionEvaluationDec (ExpressionPow exp1 exp2)))
                                                        False -> (ResultInteger (expressionEvaluationInt (ExpressionPow exp1 exp2)))
processStart (ExpressionDiv exp1 exp2) = case (checkIfDecimal exp1) of
                                           True -> (ResultDecimal (expressionEvaluationDec (ExpressionDiv exp1 exp2)))
                                           False -> case (checkIfDecimal exp2) of
                                                      True -> (ResultDecimal (expressionEvaluationDec (ExpressionDiv exp1 exp2)))
                                                      False -> (ResultInteger (expressionEvaluationInt (ExpressionDiv exp1 exp2)))
processStart (ExpressionMinus exp1 exp2) = case (checkIfDecimal exp1) of
                                             True -> (ResultDecimal (expressionEvaluationDec (ExpressionMinus exp1 exp2)))
                                             False -> case (checkIfDecimal exp2) of
                                                        True -> (ResultDecimal (expressionEvaluationDec (ExpressionMinus exp1 exp2)))
                                                        False -> (ResultInteger (expressionEvaluationInt (ExpressionMinus exp1 exp2)))
processStart (ExpressionPars exp) = processStart exp
processStart (ExpressionNeg exp) = case (checkIfDecimal exp) of
                                    True -> (ResultDecimal (expressionEvaluationDec (ExpressionNeg exp)))
                                    False -> (ResultInteger (expressionEvaluationInt (ExpressionNeg exp)))
processStart (ExpressionMod exp1 exp2) = case (checkIfDecimal exp1) of
                                          True -> (ResultDecimal (expressionEvaluationDec (ExpressionMod exp1 exp2)))
                                          False -> case (checkIfDecimal exp2) of
                                                    True -> (ResultDecimal (expressionEvaluationDec (ExpressionMod exp1 exp2)))
                                                    False -> (ResultInteger (expressionEvaluationInt (ExpressionMod exp1 exp2)))
processStart (ExpressionGreater exp1 exp2) = (ResultBool (expressionRel (ExpressionGreater exp1 exp2)))
processStart (ExpressionLower exp1 exp2) = (ResultBool (expressionRel (ExpressionLower exp1 exp2)))
processStart (ExpressionGreaterEq exp1 exp2) = (ResultBool (expressionRel (ExpressionGreaterEq exp1 exp2)))
processStart (ExpressionLowerEq exp1 exp2) = (ResultBool (expressionRel (ExpressionLowerEq exp1 exp2)))
processStart (ExpressionEqEq exp1 exp2) = (ResultBool (expressionRel (ExpressionEqEq exp1 exp2)))
processStart (ExpressionNotEq exp1 exp2) = (ResultBool (expressionRel (ExpressionNotEq exp1 exp2)))
processStart (ExpressionAnd exp1 exp2) = (ResultBool (expressionRel (ExpressionAnd exp1 exp2)))
processStart (ExpressionOr exp1 exp2) = (ResultBool (expressionRel (ExpressionOr exp1 exp2)))
processStart (ExpressionNot exp) = (ResultBool (expressionRel (ExpressionNot exp)))

checkIfDecimal :: Expression -> Bool
checkIfDecimal (ExpressionLitVar (DecimalLiteral dec)) = True
checkIfDecimal (ExpressionLitVar (IntegerLiteral int)) = False
checkIfDecimal (ExpressionPlus exp1 exp2) = checkIfDecimal exp1 || checkIfDecimal exp2
checkIfDecimal (ExpressionMult exp1 exp2) = checkIfDecimal exp1 || checkIfDecimal exp2
checkIfDecimal (ExpressionPow exp1 exp2) = checkIfDecimal exp1 || checkIfDecimal exp2
checkIfDecimal (ExpressionDiv exp1 exp2) = checkIfDecimal exp1 || checkIfDecimal exp2
checkIfDecimal (ExpressionMinus exp1 exp2) = checkIfDecimal exp1 || checkIfDecimal exp2
checkIfDecimal (ExpressionPars exp1) = checkIfDecimal exp1
checkIfDecimal (ExpressionNeg exp1) = checkIfDecimal exp1
checkIfDecimal (ExpressionMod exp1 exp2) = checkIfDecimal exp1 || checkIfDecimal exp2

checkIfString :: Expression -> Bool
checkIfString (ExpressionLitVar (StringLiteral str)) = True
checkIfString (ExpressionLitVar (DecimalLiteral dec)) = False
checkIfString (ExpressionLitVar (IntegerLiteral int)) = False
checkIfString (ExpressionPlus exp1 exp2) = checkIfString exp1 || checkIfString exp2
checkIfString (ExpressionMult exp1 exp2) = checkIfString exp1 || checkIfString exp2
checkIfString (ExpressionPow exp1 exp2) = checkIfString exp1 || checkIfString exp2
checkIfString (ExpressionDiv exp1 exp2) = checkIfString exp1 || checkIfString exp2
checkIfString (ExpressionMinus exp1 exp2) = checkIfString exp1 || checkIfString exp2
checkIfString (ExpressionPars exp1) = checkIfString exp1
checkIfString (ExpressionNeg exp1) = checkIfString exp1
checkIfString (ExpressionMod exp1 exp2) = checkIfString exp1 || checkIfString exp2

expressionConcatString :: Expression -> String
expressionConcatString (ExpressionPlus exp1 exp2) = functionConcatString (++) exp1 exp2

expressionRel :: Expression -> Bool
expressionRel (ExpressionGreater exp1 exp2) = (expressionEvaluationDec exp1) > (expressionEvaluationDec exp2)
expressionRel (ExpressionLower exp1 exp2) = (expressionEvaluationDec exp1) < (expressionEvaluationDec exp2)
expressionRel (ExpressionGreaterEq exp1 exp2) = (expressionEvaluationDec exp1) >= (expressionEvaluationDec exp2)
expressionRel (ExpressionLowerEq exp1 exp2) = (expressionEvaluationDec exp1) <= (expressionEvaluationDec exp2)
expressionRel (ExpressionEqEq exp1 exp2) = (expressionEvaluationDec exp1) == (expressionEvaluationDec exp2)
expressionRel (ExpressionNotEq exp1 exp2) = (expressionEvaluationDec exp1) /= (expressionEvaluationDec exp2)
expressionRel (ExpressionAnd exp1 exp2) = (expressionRel exp1) && (expressionRel exp2)
expressionRel (ExpressionOr exp1 exp2) = (expressionRel exp1) || (expressionRel exp2)
expressionRel (ExpressionPars exp) = expressionRel exp
expressionRel (ExpressionLitVar (BoolLiteral b)) = b
expressionRel (ExpressionNot exp1) = case (expressionRel exp1) of
                                      True -> False
                                      False -> True

expressionEvaluationDec :: Expression -> Decimal 
expressionEvaluationDec (ExpressionMult exp1 exp2) = functionExpressionDec (*) exp1 exp2
expressionEvaluationDec (ExpressionPlus exp1 exp2) = functionExpressionDec (+) exp1 exp2
expressionEvaluationDec (ExpressionPow exp1 exp2) = functionPOWDec (**) exp1 exp2
expressionEvaluationDec (ExpressionDiv exp1 exp2) = functionExpressionDec (/) exp1 exp2
expressionEvaluationDec (ExpressionMinus exp1 exp2) = functionExpressionDec (-) exp1 exp2
expressionEvaluationDec (ExpressionPars exp) = expressionEvaluationDec exp
expressionEvaluationDec (ExpressionNeg exp) = functionExpressionDec (-) (ExpressionLitVar (IntegerLiteral 0)) exp
expressionEvaluationDec (ExpressionMod exp1 exp2) = functionMODDec (mod) exp1 exp2
expressionEvaluationDec (ExpressionLitVar (IntegerLiteral int)) = intToDecimal (int)
expressionEvaluationDec (ExpressionLitVar (DecimalLiteral dec)) = dec

expressionEvaluationInt :: Expression -> Integer 
expressionEvaluationInt (ExpressionMult exp1 exp2) = functionExpressionInt (*) exp1 exp2
expressionEvaluationInt (ExpressionPlus exp1 exp2) = functionExpressionInt (+) exp1 exp2
expressionEvaluationInt (ExpressionPow exp1 exp2) = functionPOWInt (^) exp1 exp2
expressionEvaluationInt (ExpressionDiv exp1 exp2) = functionExpressionInt (quot) exp1 exp2
expressionEvaluationInt (ExpressionMinus exp1 exp2) = functionExpressionInt (-) exp1 exp2
expressionEvaluationInt (ExpressionPars exp) = expressionEvaluationInt exp
expressionEvaluationInt (ExpressionNeg exp) = functionExpressionInt (-) (ExpressionLitVar (IntegerLiteral 0)) exp
expressionEvaluationInt (ExpressionMod exp1 exp2) = functionMODInt (mod) exp1 exp2
expressionEvaluationInt (ExpressionLitVar (IntegerLiteral int)) = int

functionConcatString :: ([Char] -> [Char] -> [Char]) -> Expression -> Expression -> String
functionConcatString f (ExpressionLitVar (StringLiteral str1)) (ExpressionLitVar (StringLiteral str2)) = f str1 str2
functionConcatString f (ExpressionLitVar (StringLiteral str1)) exp = f str1 (expressionConcatString exp)
functionConcatString f exp (ExpressionLitVar (StringLiteral str1)) = f (expressionConcatString exp) str1
functionConcatString f exp1 exp2 = f (expressionConcatString exp1) (expressionConcatString exp2)


functionExpressionDec :: (Decimal -> Decimal -> Decimal) -> Expression -> Expression -> Decimal
functionExpressionDec f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = f (intToDecimal int1) (intToDecimal int2)
functionExpressionDec f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = f dec1 (intToDecimal int1)
functionExpressionDec f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = f (intToDecimal int1) dec1 
functionExpressionDec f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = f dec1 dec2
functionExpressionDec f (ExpressionLitVar (IntegerLiteral int1)) exp = f (intToDecimal int1) (expressionEvaluationDec exp)
functionExpressionDec f (ExpressionLitVar (DecimalLiteral dec1)) exp = f dec1 (expressionEvaluationDec exp)
functionExpressionDec f exp (ExpressionLitVar (IntegerLiteral int1)) = f (expressionEvaluationDec exp) (intToDecimal int1)
functionExpressionDec f exp (ExpressionLitVar (DecimalLiteral dec1)) = f (expressionEvaluationDec exp) dec1
functionExpressionDec f exp1 exp2 = f (expressionEvaluationDec exp1) (expressionEvaluationDec exp2)

functionExpressionInt :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Integer
functionExpressionInt f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = f int1 int2
functionExpressionInt f (ExpressionLitVar (IntegerLiteral int1)) exp = f int1 (expressionEvaluationInt exp)
functionExpressionInt f exp (ExpressionLitVar (IntegerLiteral int1)) = f (expressionEvaluationInt exp) int1
functionExpressionInt f exp1 exp2 = f (expressionEvaluationInt exp1) (expressionEvaluationInt exp2)

functionPOWDec :: (Double -> Double -> Double) -> Expression -> Expression -> Decimal
functionPOWDec f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = doubleToDecimal (f (intToDouble int1) (intToDouble int2))
functionPOWDec f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = doubleToDecimal (f (decToDouble dec1) (intToDouble int1))
functionPOWDec f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = doubleToDecimal (f (intToDouble int1) (decToDouble dec1))
functionPOWDec f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = doubleToDecimal (f (decToDouble dec1) (decToDouble dec2))
functionPOWDec f (ExpressionLitVar (IntegerLiteral int1)) exp = doubleToDecimal (f (intToDouble int1) (decToDouble (expressionEvaluationDec exp)))
functionPOWDec f (ExpressionLitVar (DecimalLiteral dec1)) exp = doubleToDecimal (f (decToDouble dec1) (decToDouble (expressionEvaluationDec exp)))
functionPOWDec f exp (ExpressionLitVar (IntegerLiteral int1)) = doubleToDecimal (f (decToDouble (expressionEvaluationDec exp)) (intToDouble int1))
functionPOWDec f exp (ExpressionLitVar (DecimalLiteral dec1)) = doubleToDecimal (f (decToDouble (expressionEvaluationDec exp)) (decToDouble dec1))
functionPOWDec f exp1 exp2 = doubleToDecimal (f (decToDouble (expressionEvaluationDec exp1)) (decToDouble (expressionEvaluationDec exp2)))

functionPOWInt :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Integer
functionPOWInt f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = f int1 int2
functionPOWInt f (ExpressionLitVar (IntegerLiteral int1)) exp = f int1 (expressionEvaluationInt exp)
functionPOWInt f exp (ExpressionLitVar (IntegerLiteral int1)) = f (expressionEvaluationInt exp) int1
functionPOWInt f exp1 exp2 = f (expressionEvaluationInt exp1) (expressionEvaluationInt exp2)
  

functionMODDec :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Decimal
functionMODDec f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = intToDecimal (f int1 int2)
functionMODDec f (ExpressionLitVar (IntegerLiteral int1)) exp = intToDecimal (f int1 (decToInt (expressionEvaluationDec exp)))
functionMODDec f exp (ExpressionLitVar (IntegerLiteral int1)) = intToDecimal (f (decToInt (expressionEvaluationDec exp)) int1)
functionMODDec f exp1 exp2 = intToDecimal (f (decToInt (expressionEvaluationDec exp1)) (decToInt (expressionEvaluationDec exp2)))

functionMODInt :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Integer
functionMODInt f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = f int1 int2
functionMODInt f (ExpressionLitVar (IntegerLiteral int1)) exp = f int1 (expressionEvaluationInt exp)
functionMODInt f exp (ExpressionLitVar (IntegerLiteral int1)) = f (expressionEvaluationInt exp) int1
functionMODInt f exp1 exp2 = f (expressionEvaluationInt exp1) (expressionEvaluationInt exp2)

resultUnwrapperDecimal :: ExpResult -> Decimal
resultUnwrapperDecimal (ResultDecimal dec) = dec

intToDecimal :: Integer -> Decimal
intToDecimal int = let num = show(int)
                    in (strToDecimal num)

strToDecimal :: String -> Decimal
strToDecimal str = read str :: Decimal

decToDouble :: Decimal -> Double 
decToDouble dec = let num = show(dec)
                    in (strToDouble num)

strToDouble :: String -> Double
strToDouble str = read str :: Double

intToDouble :: Integer -> Double
intToDouble int = let num = show(int)
                  in (strToDouble num)

doubleToDecimal :: Double -> Decimal
doubleToDecimal doub = let num = show(doub)
                        in (strToDecimal num)

decToInt :: Decimal -> Integer
decToInt dec = let num = show(dec)
                  in (strToInt num)

strToInt :: String -> Integer
strToInt str = read str :: Integer 


