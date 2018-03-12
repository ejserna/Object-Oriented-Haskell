module Expression where 
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
    deriving (Show,Eq)

processStart :: Expression -> ExpResult
processStart (ExpressionMult exp1 exp2) = (ResultDecimal (expressionProcess (ExpressionMult exp1 exp2)))
processStart (ExpressionPlus exp1 exp2) = case (checkIfString exp1) of
                                          True -> (ResultString (expressionConcatString (ExpressionPlus exp1 exp2)))
                                          False -> (ResultDecimal (expressionProcess (ExpressionPlus exp1 exp2)))
processStart (ExpressionPow exp1 exp2) = (ResultDecimal (expressionProcess (ExpressionPow exp1 exp2)))
processStart (ExpressionDiv exp1 exp2) = (ResultDecimal (expressionProcess (ExpressionDiv exp1 exp2)))
processStart (ExpressionMinus exp1 exp2) = (ResultDecimal (expressionProcess (ExpressionMinus exp1 exp2)))
processStart (ExpressionPars exp) = processStart exp
processStart (ExpressionNeg exp) = (ResultDecimal (expressionProcess (ExpressionNeg exp)))
processStart (ExpressionMod exp1 exp2) = (ResultDecimal (expressionProcess (ExpressionMod exp1 exp2)))
processStart (ExpressionGreater exp1 exp2) = (ResultBool (expressionRel (ExpressionGreater exp1 exp2)))
processStart (ExpressionLower exp1 exp2) = (ResultBool (expressionRel (ExpressionLower exp1 exp2)))
processStart (ExpressionGreaterEq exp1 exp2) = (ResultBool (expressionRel (ExpressionGreaterEq exp1 exp2)))
processStart (ExpressionLowerEq exp1 exp2) = (ResultBool (expressionRel (ExpressionLowerEq exp1 exp2)))
processStart (ExpressionEqEq exp1 exp2) = (ResultBool (expressionRel (ExpressionEqEq exp1 exp2)))
processStart (ExpressionNotEq exp1 exp2) = (ResultBool (expressionRel (ExpressionNotEq exp1 exp2)))
processStart (ExpressionAnd exp1 exp2) = (ResultBool (expressionRel (ExpressionAnd exp1 exp2)))
processStart (ExpressionOr exp1 exp2) = (ResultBool (expressionRel (ExpressionOr exp1 exp2)))
processStart (ExpressionNot exp) = (ResultBool (expressionRel (ExpressionNot exp)))

checkIfString :: Expression -> Bool
checkIfString (ExpressionLitVar (StringLiteral str)) = True
checkIfString (ExpressionLitVar (DecimalLiteral dec)) = False
checkIfString (ExpressionLitVar (IntegerLiteral int)) = False
checkIfString (ExpressionPlus exp1 exp2) = checkIfString exp1

expressionConcatString :: Expression -> String
expressionConcatString (ExpressionPlus exp1 exp2) = functionConcatString (++) exp1 exp2

expressionRel :: Expression -> Bool
expressionRel (ExpressionGreater exp1 exp2) = (expressionProcess exp1) > (expressionProcess exp2)
expressionRel (ExpressionLower exp1 exp2) = (expressionProcess exp1) < (expressionProcess exp2)
expressionRel (ExpressionGreaterEq exp1 exp2) = (expressionProcess exp1) >= (expressionProcess exp2)
expressionRel (ExpressionLowerEq exp1 exp2) = (expressionProcess exp1) <= (expressionProcess exp2)
expressionRel (ExpressionEqEq exp1 exp2) = (expressionProcess exp1) == (expressionProcess exp2)
expressionRel (ExpressionNotEq exp1 exp2) = (expressionProcess exp1) /= (expressionProcess exp2)
expressionRel (ExpressionAnd exp1 exp2) = (expressionRel exp1) && (expressionRel exp2)
expressionRel (ExpressionOr exp1 exp2) = (expressionRel exp1) || (expressionRel exp2)
expressionRel (ExpressionPars exp) = expressionRel exp
expressionRel (ExpressionLitVar (BoolLiteral b)) = b
expressionRel (ExpressionNot exp1) = case (expressionRel exp1) of
                                      True -> False
                                      False -> True

expressionProcess :: Expression -> Decimal 
expressionProcess (ExpressionMult exp1 exp2) = functionExpression (*) exp1 exp2
expressionProcess (ExpressionPlus exp1 exp2) = functionExpression (+) exp1 exp2
expressionProcess (ExpressionPow exp1 exp2) = functionPOW (**) exp1 exp2
expressionProcess (ExpressionDiv exp1 exp2) = functionExpression (/) exp1 exp2
expressionProcess (ExpressionMinus exp1 exp2) = functionExpression (-) exp1 exp2
expressionProcess (ExpressionPars exp) = expressionProcess exp
expressionProcess (ExpressionNeg exp) = functionExpression (-) (ExpressionLitVar (IntegerLiteral 0)) exp
expressionProcess (ExpressionMod exp1 exp2) = functionMOD (mod) exp1 exp2
expressionProcess (ExpressionLitVar (IntegerLiteral int)) = intToDecimal (int)
expressionProcess (ExpressionLitVar (DecimalLiteral dec)) = dec

functionConcatString :: ([Char] -> [Char] -> [Char]) -> Expression -> Expression -> String
functionConcatString f (ExpressionLitVar (StringLiteral str1)) (ExpressionLitVar (StringLiteral str2)) = f str1 str2
functionConcatString f (ExpressionLitVar (StringLiteral str1)) exp = f str1 (expressionConcatString exp)
functionConcatString f exp (ExpressionLitVar (StringLiteral str1)) = f (expressionConcatString exp) str1
functionConcatString f exp1 exp2 = f (expressionConcatString exp1) (expressionConcatString exp2)


functionExpression :: (Decimal -> Decimal -> Decimal) -> Expression -> Expression -> Decimal
functionExpression f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = f (intToDecimal int1) (intToDecimal int2)
functionExpression f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = f dec1 (intToDecimal int1)
functionExpression f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = f (intToDecimal int1) dec1 
functionExpression f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = f dec1 dec2
functionExpression f (ExpressionLitVar (IntegerLiteral int1)) exp = f (intToDecimal int1) (expressionProcess exp)
functionExpression f (ExpressionLitVar (DecimalLiteral dec1)) exp = f dec1 (expressionProcess exp)
functionExpression f exp (ExpressionLitVar (IntegerLiteral int1)) = f (expressionProcess exp) (intToDecimal int1)
functionExpression f exp (ExpressionLitVar (DecimalLiteral dec1)) = f (expressionProcess exp) dec1
functionExpression f exp1 exp2 = f (expressionProcess exp1) (expressionProcess exp2)

functionPOW :: (Double -> Double -> Double) -> Expression -> Expression -> Decimal
functionPOW f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = doubleToDecimal (f (intToDouble int1) (intToDouble int2))
functionPOW f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = doubleToDecimal (f (decToDouble dec1) (intToDouble int1))
functionPOW f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = doubleToDecimal (f (intToDouble int1) (decToDouble dec1))
functionPOW f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = doubleToDecimal (f (decToDouble dec1) (decToDouble dec2))
functionPOW f (ExpressionLitVar (IntegerLiteral int1)) exp = doubleToDecimal (f (intToDouble int1) (decToDouble (expressionProcess exp)))
functionPOW f (ExpressionLitVar (DecimalLiteral dec1)) exp = doubleToDecimal (f (decToDouble dec1) (decToDouble (expressionProcess exp)))
functionPOW f exp (ExpressionLitVar (IntegerLiteral int1)) = doubleToDecimal (f (decToDouble (expressionProcess exp)) (intToDouble int1))
functionPOW f exp (ExpressionLitVar (DecimalLiteral dec1)) = doubleToDecimal (f (decToDouble (expressionProcess exp)) (decToDouble dec1))
functionPOW f exp1 exp2 = doubleToDecimal (f (decToDouble (expressionProcess exp1)) (decToDouble (expressionProcess exp2)))
  

functionMOD :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Decimal
functionMOD f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = intToDecimal (f int1 int2)
functionMOD f (ExpressionLitVar (IntegerLiteral int1)) exp = intToDecimal (f int1 (decToInt (expressionProcess exp)))
functionMOD f exp (ExpressionLitVar (IntegerLiteral int1)) = intToDecimal (f (decToInt (expressionProcess exp)) int1)
functionMOD f exp1 exp2 = intToDecimal (f (decToInt (expressionProcess exp1)) (decToInt (expressionProcess exp2)))

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



