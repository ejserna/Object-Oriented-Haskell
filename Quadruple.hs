module Quadruple where
import DataTypes
import Data.List (intercalate)

-- Esta sección tiene las producciones semánticas para producir el árbol abstracto de sintaxis 
type Register = Integer
type QuadNum = Integer

-- Operation dice la operacion que hará el cuádruplo
data Operation = 
        -- Expression Operators
          GT_
        | LT_
        | GTEQ_
        | LTEQ_
        | EQ_
        | NOTEQ_
        | AND_
        | OR_      
        | ADD_
        | SUB_ 
        | MULTIPLY_
        | DIVIDE_
        | NEG_
        | NOT_
        | MOD_
        | POWER_
        -- Assignment Operators
        | ASSIGNMENT
        -- Flow Control Operators
        | GOTO
        | GOTO_IF_FALSE
        | GOTO_IF_TRUE
        | GOTO_NORMAL

instance Show Operation where
    show op = case op of
        GT_  -> id ">"
        LT_  -> id "<"
        GTEQ_  -> id ">="
        LTEQ_  -> id "<=" 
        EQ_  -> id "=="
        NOTEQ_  -> id "!="
        AND_  -> id "&&"
        OR_  -> id "||"
        ADD_  -> id "+" 
        SUB_  -> id "-"
        MULTIPLY_  -> id "*"
        DIVIDE_  -> id "/"
        NEG_  -> id "NEG"
        NOT_  -> id "!" 
        MOD_  -> id "%"
        POWER_  -> id "POW"
        GOTO_NORMAL  -> id "GOTO"
        GOTO_IF_FALSE  -> id "GOTO_F"
        GOTO_IF_TRUE  -> id  "GOTO_T" 
        ASSIGNMENT  -> id  "="

data Result =
      ResultRegister Register
    | ResultQuadruple QuadNum

instance Show Result where
    show (ResultRegister reg) = let register = "t" ++ show reg
                                in id $ register
    show (ResultQuadruple quad) = id $ show quad

data Quadruple = 
    QuadrupleFull QuadNum Operation LiteralOrVariable LiteralOrVariable Result
  | QuadrupleAssignment QuadNum Operation Result Identifier
  | QuadrupleEmpty QuadNum Operation Result

instance Show Quadruple where
    show (QuadrupleFull quadNum op litOrVar litOrVar2 res) = show quadNum ++ ". " ++ intercalate "\t," [show op,show litOrVar, show litOrVar2, show res]
    show (QuadrupleAssignment quadNum op register identifier) = show quadNum ++ ". " ++ intercalate "\t," [show op,show register, id "_", id identifier]
    show (QuadrupleEmpty quadNum op res) = show quadNum ++ ". "  ++ intercalate "\t," [show op, id "_", id "_", show res]

buildAssignmentQuadruple :: QuadNum -> Result -> Identifier -> Quadruple
buildAssignmentQuadruple quadNum resultRegister identifier = (QuadrupleAssignment quadNum (ASSIGNMENT) resultRegister identifier) 

buildFullQuadruple :: QuadNum -> Operation -> LiteralOrVariable -> LiteralOrVariable -> Result -> Quadruple
buildFullQuadruple quadNum op litOrVar1 litOrVar2 result = (QuadrupleFull quadNum op litOrVar1 litOrVar2 result) 

buildEmptyQuadruple :: QuadNum -> Operation -> Result -> Quadruple
buildEmptyQuadruple quadNum op result = (QuadrupleEmpty quadNum op result) 

main = do
  let q1 = buildFullQuadruple 1 (GT_) (VarIdentifier "a") (VarIdentifier "b") (ResultRegister 1)  in putStrLn $ id $ show q1
  let q2 = buildEmptyQuadruple 2 (GOTO_NORMAL) (ResultQuadruple 3)  in putStrLn (show q2)
  let q3 = buildAssignmentQuadruple 3 (ResultRegister 1) "b" in putStrLn (show q3)
  let q4 = buildAssignmentQuadruple 4 (ResultRegister 1) "c"
    in putStrLn (show q4)







