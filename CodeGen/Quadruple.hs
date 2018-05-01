{-# LANGUAGE MultiParamTypeClasses #-}
module Quadruple where
import Data.List (intercalate)

type Address = Integer
type QuadNum = Integer

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
        | DISPLAY_LINE
        | READ
        | NOP
        | FOR
        | INT_64
        | DOUBLE
        | BOUNDS
        | ADD_INDEX
        | ACCESS_INDEX -- ACCESS_INDEX (2002) _ ADDRESS Esto sirve para que el valor que haya en 2002 se ponga a dentro de address
        | PUT_INDEX -- PUT_INDEX ADDRESS_VALOR _ (2002) Esto sirve para que el valor se ponga a dentro del 2002
        | DISPLAY_VALUE_IN_INDEX
        -- | PARAMS
        -- | PARAMS_OBJ
        | RETURN_SET
        | GO_SUB
        | RETURN

      deriving(Eq)

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
        DISPLAY_LINE -> id "PRINTLN"
        READ -> id "READ"
        NOP -> id "NOP"
        FOR -> id "FOR"
        INT_64 -> id "INT_64"
        DOUBLE -> id "DOUBLE"
        BOUNDS -> id "BOUNDS"
        ADD_INDEX -> id "ADD_INDEX"
        ACCESS_INDEX -> id "ASSIGNMENT_FROM_INDEX"
        PUT_INDEX -> id "PUT_INDEX"
        DISPLAY_VALUE_IN_INDEX -> id "DISPLAY_VAL_IN_INDEX"
        -- PARAMS -> id "PARAMS" -- _ _ [(ADDRESS_CURRENT,ADDRESS_IN_FUNCTION)]
        -- PARAMS_OBJ -> id "PARAMS_OBJ" -- _ _ [(ADDRESS_CURRENT,ADDRESS_IN_FUNCTION)]
        RETURN_SET -> id "ASSIGN_FROM_RETURN"
        GO_SUB -> id "GO_SUB" -- GO_SUB [(AddressAttrCurrent,AddressAttrFunc)]  [(AddressAttrParamCurrent,AddressAttrParamFunc)] "_main_fibo"
        RETURN -> id "RETURN"



data Quadruple = 
    QuadrupleThreeAddresses  QuadNum Operation Address Address Address -- + 1002 1003 1004
  | QuadrupleTwoAddresses QuadNum Operation Address Address -- Assignment 1002 _ 1003
  | QuadrupleOneAddressOneQuad QuadNum Operation Address QuadNum -- GOTOF 1002 _ 8
  | QuadrupleOneQuad QuadNum Operation QuadNum -- GOTO _ _ 8
  | QuadrupleOneAddress QuadNum Operation Address -- GOTO _ _ 8
  | QuadrupleEmpty QuadNum Operation  -- No Operation
  | QuadrupleTwoAddressesOneQuad QuadNum Operation Address Address QuadNum -- FOR 6002 6003 SALTO
  | QuadrupleFunctionCall QuadNum Operation [(Address,Address)] [(Address,Address)] String -- FOR 6002 6003 SALTO
  | QuadrupleReturnSet QuadNum Operation [Address] 
  | QuadrupleReturn QuadNum Operation [Address] 

instance Show Quadruple where
    show (QuadrupleTwoAddresses quadNum ACCESS_INDEX address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show ACCESS_INDEX,"[" ++ (show address1) ++ "]", id "_" ,show address2]
    show (QuadrupleTwoAddresses quadNum PUT_INDEX address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show PUT_INDEX,show address1, id "_" ,"[" ++ (show address2) ++ "]"]
    show (QuadrupleThreeAddresses quadNum ADD_INDEX address1 address2 address3) = show quadNum ++ ". " ++ intercalate "\t" [show ADD_INDEX, "[" ++ (show address1) ++ "]", show address2, show address3]
    show (QuadrupleThreeAddresses quadNum op address1 address2 address3) = show quadNum ++ ". " ++ intercalate "\t" [show op,show address1, show address2, show address3]
    show (QuadrupleTwoAddresses quadNum op address1 address2) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show address2]
    show (QuadrupleOneAddressOneQuad quadNum op address1 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, id "_" ,show quadNumAssigned]
    show (QuadrupleOneQuad quadNum op quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show quadNumAssigned]
    show (QuadrupleOneAddress quadNum DISPLAY_VALUE_IN_INDEX address) = show quadNum ++ ". "  ++ intercalate "\t" [show DISPLAY_VALUE_IN_INDEX,id "_", id "_" , "[" ++ (show address) ++ "]" ]
    show (QuadrupleOneAddress quadNum op address) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show address]
    show (QuadrupleEmpty quadNum op) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,id "_"]
    show (QuadrupleTwoAddressesOneQuad quadNum op address1 address2 quadNumAssigned) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show address1, show address2 ,show quadNumAssigned]
    show (QuadrupleFunctionCall quadNum op addressesObjParams addressesParams funcName) = show quadNum ++ ". "  ++ intercalate "\t" [show op,show addressesObjParams, show addressesParams ,show funcName]
    show (QuadrupleReturnSet quadNum op addresses) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show addresses]
    show (QuadrupleReturn quadNum op addresses) = show quadNum ++ ". "  ++ intercalate "\t" [show op,id "_", id "_" ,show addresses]

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

buildFuncCall :: QuadNum -> [(Address,Address)] -> [(Address,Address)] -> String -> Quadruple
buildFuncCall quadNum addressesObjParams addressesParams funcName = (QuadrupleFunctionCall quadNum (GO_SUB) addressesObjParams addressesParams funcName)

buildReturnAssignment :: QuadNum -> [Address] -> Quadruple
buildReturnAssignment quadNum addressesCurrentContext = (QuadrupleReturnSet quadNum (RETURN_SET) addressesCurrentContext)

buildReturnFromFunction :: QuadNum -> [Address] -> Quadruple
buildReturnFromFunction quadNum addressesToReturn = (QuadrupleReturn quadNum (RETURN) addressesToReturn)

getQuadNum :: Quadruple -> QuadNum
getQuadNum (QuadrupleThreeAddresses quadNum op address1 address2 address3) = quadNum
getQuadNum (QuadrupleTwoAddresses quadNum op address1 address2) = quadNum
getQuadNum (QuadrupleOneAddressOneQuad quadNum op address1 quadNumAssigned) = quadNum
getQuadNum (QuadrupleOneQuad quadNum op quadNumAssigned) = quadNum
getQuadNum (QuadrupleOneAddress quadNum op address) = quadNum
getQuadNum (QuadrupleEmpty quadNum op) = quadNum
getQuadNum (QuadrupleTwoAddressesOneQuad quadNum op address1 address2 quadNumAssigned) = quadNum
getQuadNum (QuadrupleFunctionCall quadNum op addressesObjParams addressesParams funcName) = quadNum
getQuadNum (QuadrupleReturnSet quadNum op addresses) = quadNum
getQuadNum (QuadrupleReturn quadNum op addresses) = quadNum


getLastAddress :: Quadruple -> Address
getLastAddress (QuadrupleThreeAddresses quadNum op address1 address2 address3) = address3
getLastAddress (QuadrupleTwoAddresses quadNum op address1 address2) = address2
getLastAddress (QuadrupleReturnSet quadNum op (a : [])) = a
getLastAddress (QuadrupleReturn quadNum op (a : [])) = a
getLastAddress (QuadrupleOneAddress quadNum op address) = address

getLastReturnAddresses :: Quadruple -> [Address]
getLastReturnAddresses (QuadrupleReturn quadNum op addresses) = addresses
getLastReturnAddresses (QuadrupleReturnSet quadNum op addresses) = addresses
getLastReturnAddresses (QuadrupleThreeAddresses quadNum op address1 address2 address3) = [address3]
getLastReturnAddresses (QuadrupleTwoAddresses quadNum op address1 address2) = [address2]
getLastReturnAddresses (QuadrupleOneAddress quadNum op address) = [address]

buildGoto :: QuadNum -> QuadNum -> Quadruple
buildGoto quadNum quadNumAssigned = (QuadrupleOneQuad quadNum (GOTO) quadNumAssigned) 










