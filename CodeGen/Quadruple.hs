{-# LANGUAGE MultiParamTypeClasses #-}
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
        | DISPLAY
        | READ
        | NOP
        | FOR
        | INT_64
        | DOUBLE

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
        DISPLAY -> id "PRINT"
        READ -> id "READ"
        NOP -> id "NOP"
        FOR -> id "FOR"
        INT_64 -> id "INT_64"
        DOUBLE -> id "DOUBLE"

data Quadruple = 
    QuadrupleThreeAddresses  QuadNum Operation Address Address Address -- + 1002 1003 1004
  | QuadrupleTwoAddresses QuadNum Operation Address Address -- Assignment 1002 _ 1003
  | QuadrupleOneAddressOneQuad QuadNum Operation Address QuadNum -- GOTOF 1002 _ 8
  | QuadrupleOneQuad QuadNum Operation QuadNum -- GOTO _ _ 8
  | QuadrupleOneAddress QuadNum Operation Address -- GOTO _ _ 8
  | QuadrupleEmpty QuadNum Operation  -- No Operation
  | QuadrupleTwoAddressesOneQuad QuadNum Operation Address Address QuadNum -- FOR 6002 6003 SALTO

instance Show Quadruple where
    show (QuadrupleThreeAddresses quadNum op address1 address2 address3) = show quadNum ++ ". " ++ intercalate "\t" [show op,show address1, show address2, show address3]
    show (QuadrupleTwoAddresses quadNum op address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show address2]
    show (QuadrupleOneAddressOneQuad quadNum op address1 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show quadNumAssigned]
    show (QuadrupleOneQuad quadNum op quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show quadNumAssigned]
    show (QuadrupleOneAddress quadNum op address) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show address]
    show (QuadrupleEmpty quadNum op) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,id "_"]
    show (QuadrupleTwoAddressesOneQuad quadNum op address1 address2 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, show address2 ,show quadNumAssigned]


buildQuadrupleThreeAddresses :: QuadNum -> Operation -> (Address,Address,Address)-> Quadruple
buildQuadrupleThreeAddresses quadNum op (address1,address2,address3) = (QuadrupleThreeAddresses quadNum op address1 address2 address3)

buildQuadrupleTwoAddresses :: QuadNum -> Operation -> (Address,Address)-> Quadruple
buildQuadrupleTwoAddresses quadNum op (address1,address2) = (QuadrupleTwoAddresses quadNum op address1 address2)

buildQuadForConditions :: QuadNum -> Operation -> Address -> QuadNum-> Quadruple
buildQuadForConditions quadNum op address quadNumAssigned = (QuadrupleOneAddressOneQuad quadNum op address quadNumAssigned)

buildQuadOneAddress :: QuadNum -> Operation -> Address -> Quadruple
buildQuadOneAddress quadNum op address = (QuadrupleOneAddress quadNum op address)

buildNoOpQuad :: QuadNum -> Quadruple
buildNoOpQuad quadNum = (QuadrupleEmpty quadNum (NOP))

buildForLoopQuad :: QuadNum -> (Address,Address) -> QuadNum -> Quadruple
buildForLoopQuad quadNum (address1,address2) quadNumAssigned = (QuadrupleTwoAddressesOneQuad quadNum (FOR) address1 address2 quadNumAssigned)

getQuadNum :: Quadruple -> QuadNum
getQuadNum (QuadrupleThreeAddresses quadNum op address1 address2 address3) = quadNum
getQuadNum (QuadrupleTwoAddresses quadNum op address1 address2) = quadNum
getQuadNum (QuadrupleOneAddressOneQuad quadNum op address1 quadNumAssigned) = quadNum
getQuadNum (QuadrupleOneQuad quadNum op quadNumAssigned) = quadNum
getQuadNum (QuadrupleOneAddress quadNum op address) = quadNum
getQuadNum (QuadrupleEmpty quadNum op) = quadNum
getQuadNum (QuadrupleTwoAddressesOneQuad quadNum op address1 address2 quadNumAssigned) = quadNum

getLastAddress :: Quadruple -> QuadNum
getLastAddress (QuadrupleThreeAddresses quadNum op address1 address2 address3) = address3
getLastAddress (QuadrupleTwoAddresses quadNum op address1 address2) = address2

buildGoto :: QuadNum -> QuadNum -> Quadruple
buildGoto quadNum quadNumAssigned = (QuadrupleOneQuad quadNum (GOTO) quadNumAssigned) 










