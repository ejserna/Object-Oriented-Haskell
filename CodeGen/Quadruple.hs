{-# LANGUAGE MultiParamTypeClasses #-}
module Quadruple where
import DataTypes
import CodeGenDataTypes
import Data.List (intercalate)

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










