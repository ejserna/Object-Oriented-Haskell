module Quadruple where
import DataTypes
import Data.List (intercalate)


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
        GOTO  -> id "GOTO"
        GOTO_IF_FALSE  -> id "GOTO_F"
        GOTO_IF_TRUE  -> id  "GOTO_T" 
        ASSIGNMENT  -> id  "="

-- data Result =
--       ResultRegister Register
--     | ResultQuadruple QuadNum

-- instance Show Result where
--     show (ResultRegister reg) = let register = "t" ++ show reg
--                                 in id $ register
--     show (ResultQuadruple quad) = id $ show quad

-- 
data Quadruple = 
    QuadrupleThreeAddresses  QuadNum Operation Address Address Address -- + 1002 1003 1004
  | QuadrupleTwoAddresses QuadNum Operation Address Address -- Assignment 1002 _ 1003
  | QuadrupleOneAddressOneQuad QuadNum Operation Address QuadNum -- GOTOF 1002 _ 8
  | QuadrupleOneQuad QuadNum Operation QuadNum -- GOTO _ _ 8


instance Show Quadruple where
    show (QuadrupleThreeAddresses quadNum op address1 address2 address3) = show quadNum ++ ". " ++ intercalate "\t" [show op,show address1, show address2, show address3]
    show (QuadrupleTwoAddresses quadNum op address1 address2) = show quadNum ++ ". "  ++ intercalate "\t," [show op,show address1, id "_" ,show address2]
    show (QuadrupleOneAddressOneQuad quadNum op address1 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show quadNumAssigned]
    show (QuadrupleOneQuad quadNum op quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show quadNumAssigned]

-- buildAssignmentQuadruple :: QuadNum -> Result -> Identifier -> Quadruple
-- buildAssignmentQuadruple quadNum resultRegister identifier = (QuadrupleAssignment quadNum (ASSIGNMENT) resultRegister identifier) 

buildQuadrupleThreeAddresses :: QuadNum -> Operation -> (Address,Address,Address)-> Quadruple
buildQuadrupleThreeAddresses quadNum op (address1,address2,address3) = (QuadrupleThreeAddresses quadNum op address1 address2 address3)

buildQuadrupleTwoAddresses :: QuadNum -> Operation -> (Address,Address)-> Quadruple
buildQuadrupleTwoAddresses quadNum op (address1,address2) = (QuadrupleTwoAddresses quadNum op address1 address2)

buildGoto :: QuadNum -> QuadNum -> Quadruple
buildGoto quadNum quadNumAssigned = (QuadrupleOneQuad quadNum (GOTO) quadNumAssigned) 

buildGotoCondition :: QuadNum -> QuadNum -> Quadruple
buildGotoCondition quadNum quadNumAssigned = (QuadrupleOneQuad quadNum (GOTO) quadNumAssigned) 

-- buildEmptyQuadruple :: QuadNum -> Operation -> Result -> Quadruple
-- buildEmptyQuadruple quadNum op result = (QuadrupleEmpty quadNum op result) 








