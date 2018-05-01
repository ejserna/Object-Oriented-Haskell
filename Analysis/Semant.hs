module Semant where 
import DataTypes

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