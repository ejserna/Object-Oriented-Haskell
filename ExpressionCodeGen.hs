module Expression where 
import Data.Decimal
import DataTypes
import Text.Show.Pretty
import SymbolTable
import ClassSymbolTable
import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

data ExpResult
    = ResultDecimal Decimal
    | ResltBool Bool
    | ResultString String


expressionProcess :: Expression -> Decimal 
expressionProcess (ExpressionMult exp1 exp2) = functionExpression (*) exp1 exp2
expressionProcess (ExpressionPlus exp1 exp2) = functionExpression (+) exp1 exp2
expressionProcess (ExpressionPow exp1 exp2) = functionPOW (**) exp1 exp2
expressionProcess (ExpressionDiv exp1 exp2) = functionExpression (/) exp1 exp2
expressionProcess (ExpressionMinus exp1 exp2) = functionExpression (-) exp1 exp2
expressionProcess (ExpressionPars exp) = expressionProcess exp
expressionProcess (ExpressionNeg exp) = functionExpression (-) (ExpressionLitVar (IntegerLiteral 0)) exp
expressionProcess (ExpressionMod exp1 exp2) = functionMOD (mod) exp1 exp2



functionExpression :: (Decimal -> Decimal -> Decimal) -> Expression -> Expression -> Decimal
functionExpression f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = f (intToDecimal int1) (intToDecimal int2)
functionExpression f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = f dec1 (intToDecimal int1)
functionExpression f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = f dec1 (intToDecimal int1)
functionExpression f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = f dec1 dec2
functionExpression f (ExpressionLitVar (IntegerLiteral int1)) exp = f (intToDecimal int1) (expressionProcess exp)
functionExpression f (ExpressionLitVar (DecimalLiteral dec1)) exp = f dec1 (expressionProcess exp)
functionExpression f exp (ExpressionLitVar (IntegerLiteral int1)) = f (expressionProcess exp) (intToDecimal int1)
functionExpression f exp (ExpressionLitVar (DecimalLiteral dec1)) = f (expressionProcess exp) dec1
functionExpression f exp1 exp2 = f (expressionProcess exp1) (expressionProcess exp2)

functionPOW :: (Double -> Double -> Double) -> Expression -> Expression -> Decimal
functionPOW f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = doubleToDecimal (f (intToDouble int1) (intToDouble int2))
functionPOW f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (IntegerLiteral int1)) = doubleToDecimal (f (decToDouble dec1) (intToDouble int1))
functionPOW f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (DecimalLiteral dec1)) = doubleToDecimal (f (decToDouble dec1) (intToDouble int1))
functionPOW f (ExpressionLitVar (DecimalLiteral dec1)) (ExpressionLitVar (DecimalLiteral dec2)) = doubleToDecimal (f (decToDouble dec1) (decToDouble dec2))
functionPOW f (ExpressionLitVar (IntegerLiteral int1)) exp = doubleToDecimal (f (intToDouble int1) (decToDouble (expressionProcess exp)))
functionPOW f (ExpressionLitVar (DecimalLiteral dec1)) exp = doubleToDecimal (f (decToDouble dec1) (decToDouble (expressionProcess exp)))
functionPOW f exp (ExpressionLitVar (IntegerLiteral int1)) = doubleToDecimal (f (intToDouble int1) (decToDouble (expressionProcess exp)))
functionPOW f exp (ExpressionLitVar (DecimalLiteral dec1)) = doubleToDecimal (f (decToDouble dec1) (decToDouble (expressionProcess exp)))
functionPOW f exp1 exp2 = doubleToDecimal (f (decToDouble (expressionProcess exp1)) (decToDouble (expressionProcess exp2)))
  

functionMOD :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Decimal
functionMOD f (ExpressionLitVar (IntegerLiteral int1)) (ExpressionLitVar (IntegerLiteral int2)) = intToDecimal (f int1 int2)
functionMOD f (ExpressionLitVar (IntegerLiteral int1)) exp = intToDecimal (f int1 (decToInt (expressionProcess exp)))
functionMOD f exp (ExpressionLitVar (IntegerLiteral int1)) = intToDecimal (f int1 (decToInt (expressionProcess exp)))
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



