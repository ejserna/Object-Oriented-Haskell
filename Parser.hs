{-# OPTIONS_GHC -w #-}
module Main where
import Scanner
import Data.Decimal
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (60) = happyShift action_10
action_2 (61) = happyShift action_11
action_2 (70) = happyShift action_12
action_2 (77) = happyShift action_13
action_2 (79) = happyShift action_14
action_2 (80) = happyShift action_15
action_2 (81) = happyShift action_16
action_2 (82) = happyShift action_17
action_2 (86) = happyShift action_18
action_2 (87) = happyShift action_19
action_2 (6) = happyGoto action_4
action_2 (11) = happyGoto action_5
action_2 (12) = happyGoto action_6
action_2 (14) = happyGoto action_7
action_2 (15) = happyGoto action_8
action_2 (22) = happyGoto action_9
action_2 _ = happyFail

action_3 (89) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (13) = happyGoto action_28
action_5 _ = happyReduce_26

action_6 (86) = happyShift action_27
action_6 _ = happyFail

action_7 (86) = happyShift action_26
action_7 _ = happyFail

action_8 _ = happyReduce_4

action_9 _ = happyReduce_5

action_10 (73) = happyShift action_25
action_10 (30) = happyGoto action_24
action_10 _ = happyFail

action_11 (87) = happyShift action_23
action_11 _ = happyFail

action_12 (36) = happyShift action_22
action_12 _ = happyFail

action_13 _ = happyReduce_19

action_14 _ = happyReduce_20

action_15 _ = happyReduce_21

action_16 _ = happyReduce_22

action_17 _ = happyReduce_23

action_18 (65) = happyShift action_21
action_18 _ = happyFail

action_19 (13) = happyGoto action_20
action_19 _ = happyReduce_26

action_20 (68) = happyShift action_29
action_20 _ = happyReduce_25

action_21 (70) = happyShift action_12
action_21 (77) = happyShift action_13
action_21 (79) = happyShift action_14
action_21 (80) = happyShift action_15
action_21 (81) = happyShift action_16
action_21 (82) = happyShift action_17
action_21 (83) = happyShift action_43
action_21 (87) = happyShift action_44
action_21 (8) = happyGoto action_40
action_21 (11) = happyGoto action_41
action_21 (14) = happyGoto action_42
action_21 _ = happyFail

action_22 (77) = happyShift action_13
action_22 (79) = happyShift action_14
action_22 (80) = happyShift action_15
action_22 (81) = happyShift action_16
action_22 (82) = happyShift action_17
action_22 (87) = happyShift action_39
action_22 (11) = happyGoto action_38
action_22 _ = happyFail

action_23 (62) = happyShift action_36
action_23 (73) = happyShift action_37
action_23 (23) = happyGoto action_35
action_23 _ = happyFail

action_24 _ = happyReduce_1

action_25 (74) = happyShift action_34
action_25 _ = happyFail

action_26 (31) = happyShift action_32
action_26 (72) = happyShift action_33
action_26 _ = happyFail

action_27 (31) = happyShift action_31
action_27 (16) = happyGoto action_30
action_27 _ = happyReduce_37

action_28 (68) = happyShift action_29
action_28 _ = happyReduce_24

action_29 (85) = happyShift action_64
action_29 (86) = happyShift action_65
action_29 _ = happyFail

action_30 (32) = happyShift action_62
action_30 (72) = happyShift action_63
action_30 _ = happyFail

action_31 (68) = happyShift action_57
action_31 (84) = happyShift action_58
action_31 (85) = happyShift action_59
action_31 (86) = happyShift action_60
action_31 (88) = happyShift action_61
action_31 (17) = happyGoto action_54
action_31 (19) = happyGoto action_55
action_31 (29) = happyGoto action_56
action_31 _ = happyFail

action_32 (68) = happyShift action_53
action_32 (17) = happyGoto action_51
action_32 (21) = happyGoto action_52
action_32 _ = happyFail

action_33 _ = happyReduce_36

action_34 _ = happyReduce_69

action_35 _ = happyReduce_51

action_36 (87) = happyShift action_50
action_36 _ = happyFail

action_37 (24) = happyGoto action_49
action_37 _ = happyReduce_53

action_38 _ = happyReduce_30

action_39 _ = happyReduce_29

action_40 (67) = happyShift action_48
action_40 (73) = happyShift action_25
action_40 (30) = happyGoto action_47
action_40 _ = happyFail

action_41 (10) = happyGoto action_46
action_41 _ = happyReduce_17

action_42 _ = happyReduce_12

action_43 _ = happyReduce_13

action_44 (10) = happyGoto action_45
action_44 _ = happyReduce_17

action_45 (68) = happyShift action_89
action_45 _ = happyReduce_11

action_46 (68) = happyShift action_89
action_46 _ = happyReduce_10

action_47 _ = happyReduce_7

action_48 (70) = happyShift action_12
action_48 (77) = happyShift action_13
action_48 (79) = happyShift action_14
action_48 (80) = happyShift action_15
action_48 (81) = happyShift action_16
action_48 (82) = happyShift action_17
action_48 (87) = happyShift action_87
action_48 (9) = happyGoto action_88
action_48 (11) = happyGoto action_81
action_48 (14) = happyGoto action_82
action_48 _ = happyFail

action_49 (63) = happyShift action_85
action_49 (64) = happyShift action_86
action_49 (70) = happyShift action_12
action_49 (77) = happyShift action_13
action_49 (79) = happyShift action_14
action_49 (80) = happyShift action_15
action_49 (81) = happyShift action_16
action_49 (82) = happyShift action_17
action_49 (87) = happyShift action_87
action_49 (9) = happyGoto action_80
action_49 (11) = happyGoto action_81
action_49 (14) = happyGoto action_82
action_49 (25) = happyGoto action_83
action_49 (28) = happyGoto action_84
action_49 _ = happyReduce_63

action_50 (73) = happyShift action_37
action_50 (23) = happyGoto action_79
action_50 _ = happyFail

action_51 _ = happyReduce_48

action_52 (72) = happyShift action_78
action_52 _ = happyFail

action_53 (69) = happyShift action_71
action_53 (85) = happyShift action_77
action_53 (86) = happyShift action_73
action_53 _ = happyFail

action_54 (72) = happyShift action_76
action_54 _ = happyFail

action_55 (72) = happyShift action_75
action_55 _ = happyFail

action_56 (72) = happyShift action_74
action_56 _ = happyFail

action_57 (68) = happyShift action_70
action_57 (69) = happyShift action_71
action_57 (85) = happyShift action_72
action_57 (86) = happyShift action_73
action_57 (17) = happyGoto action_69
action_57 _ = happyFail

action_58 _ = happyReduce_67

action_59 _ = happyReduce_66

action_60 _ = happyReduce_65

action_61 _ = happyReduce_68

action_62 (86) = happyShift action_68
action_62 _ = happyFail

action_63 _ = happyReduce_31

action_64 (69) = happyShift action_67
action_64 _ = happyFail

action_65 (69) = happyShift action_66
action_65 _ = happyFail

action_66 _ = happyReduce_27

action_67 _ = happyReduce_28

action_68 _ = happyReduce_38

action_69 (20) = happyGoto action_103
action_69 _ = happyReduce_46

action_70 (69) = happyShift action_71
action_70 (85) = happyShift action_72
action_70 (86) = happyShift action_73
action_70 _ = happyFail

action_71 _ = happyReduce_41

action_72 (18) = happyGoto action_100
action_72 _ = happyReduce_42

action_73 (18) = happyGoto action_102
action_73 _ = happyReduce_42

action_74 _ = happyReduce_32

action_75 _ = happyReduce_34

action_76 _ = happyReduce_33

action_77 (71) = happyShift action_101
action_77 (18) = happyGoto action_100
action_77 _ = happyReduce_42

action_78 _ = happyReduce_35

action_79 _ = happyReduce_50

action_80 (86) = happyShift action_99
action_80 _ = happyFail

action_81 (10) = happyGoto action_98
action_81 _ = happyReduce_17

action_82 _ = happyReduce_16

action_83 _ = happyReduce_54

action_84 (26) = happyGoto action_97
action_84 _ = happyReduce_59

action_85 (70) = happyShift action_12
action_85 (77) = happyShift action_13
action_85 (79) = happyShift action_14
action_85 (80) = happyShift action_15
action_85 (81) = happyShift action_16
action_85 (82) = happyShift action_17
action_85 (87) = happyShift action_19
action_85 (11) = happyGoto action_5
action_85 (12) = happyGoto action_95
action_85 (14) = happyGoto action_96
action_85 _ = happyFail

action_86 (70) = happyShift action_12
action_86 (77) = happyShift action_13
action_86 (79) = happyShift action_14
action_86 (80) = happyShift action_15
action_86 (81) = happyShift action_16
action_86 (82) = happyShift action_17
action_86 (87) = happyShift action_19
action_86 (11) = happyGoto action_5
action_86 (12) = happyGoto action_93
action_86 (14) = happyGoto action_94
action_86 _ = happyFail

action_87 (10) = happyGoto action_92
action_87 _ = happyReduce_17

action_88 (86) = happyShift action_91
action_88 _ = happyFail

action_89 (69) = happyShift action_90
action_89 _ = happyFail

action_90 _ = happyReduce_18

action_91 (7) = happyGoto action_119
action_91 _ = happyReduce_8

action_92 (68) = happyShift action_89
action_92 _ = happyReduce_15

action_93 (86) = happyShift action_118
action_93 _ = happyFail

action_94 (86) = happyShift action_117
action_94 _ = happyFail

action_95 (86) = happyShift action_116
action_95 _ = happyFail

action_96 (86) = happyShift action_115
action_96 _ = happyFail

action_97 (63) = happyShift action_112
action_97 (64) = happyShift action_113
action_97 (74) = happyShift action_114
action_97 (27) = happyGoto action_111
action_97 _ = happyFail

action_98 (68) = happyShift action_89
action_98 _ = happyReduce_14

action_99 (7) = happyGoto action_110
action_99 _ = happyReduce_8

action_100 (32) = happyShift action_106
action_100 (69) = happyShift action_109
action_100 _ = happyFail

action_101 (85) = happyShift action_108
action_101 _ = happyFail

action_102 (32) = happyShift action_106
action_102 (69) = happyShift action_107
action_102 _ = happyFail

action_103 (32) = happyShift action_104
action_103 (69) = happyShift action_105
action_103 _ = happyFail

action_104 (68) = happyShift action_70
action_104 (17) = happyGoto action_132
action_104 _ = happyFail

action_105 _ = happyReduce_45

action_106 (85) = happyShift action_130
action_106 (86) = happyShift action_131
action_106 _ = happyFail

action_107 _ = happyReduce_39

action_108 (69) = happyShift action_129
action_108 _ = happyFail

action_109 _ = happyReduce_40

action_110 (66) = happyShift action_121
action_110 (73) = happyShift action_25
action_110 (30) = happyGoto action_128
action_110 _ = happyFail

action_111 _ = happyReduce_60

action_112 (86) = happyShift action_18
action_112 (6) = happyGoto action_127
action_112 _ = happyFail

action_113 (86) = happyShift action_18
action_113 (6) = happyGoto action_126
action_113 _ = happyFail

action_114 _ = happyReduce_52

action_115 (72) = happyShift action_125
action_115 _ = happyFail

action_116 (72) = happyShift action_124
action_116 _ = happyFail

action_117 (72) = happyShift action_123
action_117 _ = happyFail

action_118 (72) = happyShift action_122
action_118 _ = happyFail

action_119 (66) = happyShift action_121
action_119 (73) = happyShift action_25
action_119 (30) = happyGoto action_120
action_119 _ = happyFail

action_120 _ = happyReduce_6

action_121 (70) = happyShift action_12
action_121 (77) = happyShift action_13
action_121 (79) = happyShift action_14
action_121 (80) = happyShift action_15
action_121 (81) = happyShift action_16
action_121 (82) = happyShift action_17
action_121 (87) = happyShift action_87
action_121 (9) = happyGoto action_133
action_121 (11) = happyGoto action_81
action_121 (14) = happyGoto action_82
action_121 _ = happyFail

action_122 _ = happyReduce_57

action_123 _ = happyReduce_58

action_124 _ = happyReduce_55

action_125 _ = happyReduce_56

action_126 _ = happyReduce_62

action_127 _ = happyReduce_61

action_128 _ = happyReduce_64

action_129 _ = happyReduce_49

action_130 _ = happyReduce_43

action_131 _ = happyReduce_44

action_132 _ = happyReduce_47

action_133 (86) = happyShift action_134
action_133 _ = happyFail

action_134 _ = happyReduce_9

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (FVCEmpty
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (FVCFunction happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (FVCVariable happy_var_1 happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (FVCClass happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 8 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn30  happy_var_8) `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_6)) `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Function happy_var_1 happy_var_3 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (FunctionEmptyParams happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_0  7 happyReduction_8
happyReduction_8  =  HappyAbsSyn7
		 (ParamsEmpty
	)

happyReduce_9 = happyReduce 4 7 happyReduction_9
happyReduction_9 ((HappyTerminal (TVarIdent _ happy_var_4)) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Params happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (TypeFuncReturnPrimitive happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TClassIdent _ happy_var_1))
	 =  HappyAbsSyn8
		 (TypeFuncReturnClassId happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (TypeFuncReturnList happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn8
		 (TypeFuncReturnNothing
	)

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (TypeFuncParamsPrimitive happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TClassIdent _ happy_var_1))
	 =  HappyAbsSyn9
		 (TypeFuncParamsClassId happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn9
		 (TypeFuncParamsList happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  10 happyReduction_17
happyReduction_17  =  HappyAbsSyn10
		 (ClosingBracketsNoIdentifierEmpty
	)

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 _
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (ClosingBracketsNoIdentifier happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn11
		 (PrimitiveInt
	)

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn11
		 (PrimitiveDouble
	)

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn11
		 (PrimitiveMoney
	)

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn11
		 (PrimitiveString
	)

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn11
		 (PrimitiveBool
	)

happyReduce_24 = happySpecReduce_2  12 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (TypePrimitive happy_var_1 happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  12 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_2)
	(HappyTerminal (TClassIdent _ happy_var_1))
	 =  HappyAbsSyn12
		 (TypeClassId happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  13 happyReduction_26
happyReduction_26  =  HappyAbsSyn13
		 (ArrayIndexesEmpty
	)

happyReduce_27 = happyReduce 4 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ArrayIndexesIdentifier happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 13 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyTerminal (TIntegerLiteral _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ArrayIndexesIntLiteral happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  14 happyReduction_29
happyReduction_29 (HappyTerminal (TClassIdent _ happy_var_3))
	_
	_
	 =  HappyAbsSyn14
		 (ListTypeClassId happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  14 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (ListTypePrimitive happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 15 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (VariableNoAssignment happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 15 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (VariableLiteralOrVariable happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 5 15 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (VariableAssignment1D happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 5 15 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (VariableAssignment2D happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 5 15 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (VariableListAssignment happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  15 happyReduction_36
happyReduction_36 _
	(HappyTerminal (TVarIdent _ happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (VariableList happy_var_1 happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  16 happyReduction_37
happyReduction_37  =  HappyAbsSyn16
		 (VarIdentifiersEmpty
	)

happyReduce_38 = happySpecReduce_3  16 happyReduction_38
happyReduction_38 (HappyTerminal (TVarIdent _ happy_var_3))
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (VarIdentifiers happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 17 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (ArrayAssignmentVarIdentifier happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 17 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyTerminal (TIntegerLiteral _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (ArrayAssignmentIntLiteral happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_2  17 happyReduction_41
happyReduction_41 _
	_
	 =  HappyAbsSyn17
		 (ArrayAssignment1DEmpty
	)

happyReduce_42 = happySpecReduce_0  18 happyReduction_42
happyReduction_42  =  HappyAbsSyn18
		 (Array1DAssignmentsEmpty
	)

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 (HappyTerminal (TIntegerLiteral _ happy_var_3))
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Array1DAssignmentsInteger happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 (HappyTerminal (TVarIdent _ happy_var_3))
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Array1DAssignmentsVarIdentifier happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 4 19 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (ArrayAssignment2D happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_0  20 happyReduction_46
happyReduction_46  =  HappyAbsSyn20
		 (Array2DAssignmentsEmpty
	)

happyReduce_47 = happySpecReduce_3  20 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Array2DAssignments happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (ListAssignmentArray happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyReduce 5 21 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyTerminal (TIntegerLiteral _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIntegerLiteral _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (ListAssignmentRange happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 5 22 happyReduction_50
happyReduction_50 ((HappyAbsSyn23  happy_var_5) `HappyStk`
	(HappyTerminal (TClassIdent _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TClassIdent _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (ClassInheritance happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_3  22 happyReduction_51
happyReduction_51 (HappyAbsSyn23  happy_var_3)
	(HappyTerminal (TClassIdent _ happy_var_2))
	_
	 =  HappyAbsSyn22
		 (ClassNormal happy_var_2 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 5 23 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (ClassBlock happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_0  24 happyReduction_53
happyReduction_53  =  HappyAbsSyn24
		 (ClassAttributesEmpty
	)

happyReduce_54 = happySpecReduce_2  24 happyReduction_54
happyReduction_54 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (ClassAttributes happy_var_1 happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 4 25 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_3)) `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ClassAttributeTypePublic happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 4 25 happyReduction_56
happyReduction_56 (_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_3)) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ClassAttributeListPublic happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_57 = happyReduce 4 25 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_3)) `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ClassAttributeTypePrivate happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 4 25 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_3)) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ClassAttributeListPrivate happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_0  26 happyReduction_59
happyReduction_59  =  HappyAbsSyn26
		 (ClassFunctionsEmpty
	)

happyReduce_60 = happySpecReduce_2  26 happyReduction_60
happyReduction_60 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (ClassFunctions happy_var_1 happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  27 happyReduction_61
happyReduction_61 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (ClassFunctionPublic happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  27 happyReduction_62
happyReduction_62 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (ClassFunctionPrivate happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  28 happyReduction_63
happyReduction_63  =  HappyAbsSyn28
		 (ClassConstructorEmpty
	)

happyReduce_64 = happyReduce 4 28 happyReduction_64
happyReduction_64 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TVarIdent _ happy_var_2)) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (ClassConstructor happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 (HappyTerminal (TVarIdent _ happy_var_1))
	 =  HappyAbsSyn29
		 (VarIdentifier happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 (HappyTerminal (TIntegerLiteral _ happy_var_1))
	 =  HappyAbsSyn29
		 (IntegerLiteral happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  29 happyReduction_67
happyReduction_67 (HappyTerminal (TDecimalLiteral _ happy_var_1))
	 =  HappyAbsSyn29
		 (DecimalLiteral happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  29 happyReduction_68
happyReduction_68 (HappyTerminal (TStringLiteral _ happy_var_1))
	 =  HappyAbsSyn29
		 (StringLiteral happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  30 happyReduction_69
happyReduction_69 _
	_
	 =  HappyAbsSyn30
		 (Block
	)

happyNewToken action sts stk [] =
	action 89 89 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TEquals _ -> cont 31;
	TComma _ -> cont 32;
	TIf _ -> cont 33;
	TElse _ -> cont 34;
	TCase _ -> cont 35;
	TOf _ -> cont 36;
	TOtherwise _ -> cont 37;
	TFor _ -> cont 38;
	TWhile _ -> cont 39;
	TRead _ -> cont 40;
	TDisplay _ -> cont 41;
	TPlus _ -> cont 42;
	TMinus _ -> cont 43;
	TMultiply _ -> cont 44;
	TDivide _ -> cont 45;
	TPlusPlus _ -> cont 46;
	TMinusMinus _ -> cont 47;
	TPower _ -> cont 48;
	TNot _ -> cont 49;
	TTrue _ -> cont 50;
	TTrue _ -> cont 51;
	TGreaterThan _ -> cont 52;
	TLessThan _ -> cont 53;
	TGreaterEqualThan _ -> cont 54;
	TLessEqualThan _ -> cont 55;
	TDoubleEqual _ -> cont 56;
	TNotEqual _ -> cont 57;
	TAnd _ -> cont 58;
	TOr _ -> cont 59;
	TMain _ -> cont 60;
	TClass _ -> cont 61;
	TColon _ -> cont 62;
	TPublic _ -> cont 63;
	TPrivate _ -> cont 64;
	TEqualsRightArrow _ -> cont 65;
	TDashRightArrow _ -> cont 66;
	TDoubleColon _ -> cont 67;
	TLeftBracket _ -> cont 68;
	TRightBracket _ -> cont 69;
	TList _ -> cont 70;
	TDoublePoint _ -> cont 71;
	TSemiColon _ -> cont 72;
	TRightBrace _ -> cont 73;
	TLeftBrace _ -> cont 74;
	TLeftParen _ -> cont 75;
	TRightParen _ -> cont 76;
	TInt _ -> cont 77;
	TInteger _ -> cont 78;
	TDouble _ -> cont 79;
	TMoney _ -> cont 80;
	TString _ -> cont 81;
	TBool _ -> cont 82;
	TString _ -> cont 83;
	TDecimalLiteral _ happy_dollar_dollar -> cont 84;
	TIntegerLiteral _ happy_dollar_dollar -> cont 85;
	TVarIdent _ happy_dollar_dollar -> cont 86;
	TClassIdent _ happy_dollar_dollar -> cont 87;
	TStringLiteral _ happy_dollar_dollar -> cont 88;
	_ -> happyError' (tk:tks)
	}

happyError_ 89 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

ooh tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) 
  in 
  error ("Parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

-- Esta sección tiene las producciones semánticas para producir el árbol abstracto de sintaxis 

data Program 
    = Program FunctionsVariablesClasses Block
  deriving (Show, Eq)

data FunctionsVariablesClasses 
    = FVCFunction FunctionsVariablesClasses Function
    | FVCVariable FunctionsVariablesClasses Variable
    | FVCClass FunctionsVariablesClasses Class
    | FVCEmpty
  deriving (Show, Eq)

data Function 
    = Function String TypeFuncReturn TypeFuncParams String Params Block
    | FunctionEmptyParams String TypeFuncReturn Block
  deriving (Show, Eq)

data Params 
    = ParamsEmpty
    | Params Params TypeFuncParams String
  deriving (Show, Eq)

data TypeFuncReturn 
    = TypeFuncReturnPrimitive Primitive ClosingBracketsNoIdentifier
    | TypeFuncReturnClassId String ClosingBracketsNoIdentifier
    | TypeFuncReturnList ListType
    | TypeFuncReturnNothing
  deriving (Show, Eq)

data TypeFuncParams 
    = TypeFuncParamsPrimitive Primitive ClosingBracketsNoIdentifier
    | TypeFuncParamsClassId String ClosingBracketsNoIdentifier
    | TypeFuncParamsList ListType
  deriving (Show, Eq)

data ClosingBracketsNoIdentifier 
    = ClosingBracketsNoIdentifierEmpty
    | ClosingBracketsNoIdentifier ClosingBracketsNoIdentifier
  deriving (Show, Eq)

data Primitive 
    = PrimitiveInt
    | PrimitiveDouble
    | PrimitiveMoney
    | PrimitiveString
    | PrimitiveBool
  deriving (Show, Eq)

data Type 
    = TypePrimitive Primitive ArrayIndexes
    | TypeClassId String ArrayIndexes
  deriving (Show, Eq)

data ArrayIndexes 
    = ArrayIndexesEmpty
    | ArrayIndexesIdentifier ArrayIndexes String
    | ArrayIndexesIntLiteral ArrayIndexes Integer
  deriving (Show, Eq)

data ListType 
    = ListTypeClassId String
    | ListTypePrimitive Primitive
  deriving (Show, Eq)

data Variable 
    = VariableNoAssignment Type String VarIdentifiers
    | VariableLiteralOrVariable Type String LiteralOrVariable
    | VariableAssignment1D Type String ArrayAssignment1D
    | VariableAssignment2D Type String ArrayAssignment2D
    | VariableListAssignment ListType String ListAssignment
    | VariableList ListType String
  deriving (Show, Eq)

data VarIdentifiers 
    = VarIdentifiersEmpty
    | VarIdentifiers VarIdentifiers String
  deriving (Show, Eq)

data ArrayAssignment1D 
    = ArrayAssignmentVarIdentifier String Array1DAssignments
    | ArrayAssignmentIntLiteral Integer Array1DAssignments
    | ArrayAssignment1DEmpty
  deriving (Show, Eq)

data Array1DAssignments 
    = Array1DAssignmentsEmpty
    | Array1DAssignmentsInteger Array1DAssignments Integer
    | Array1DAssignmentsVarIdentifier Array1DAssignments String
  deriving (Show, Eq)

data ArrayAssignment2D 
    = ArrayAssignment2D ArrayAssignment1D Array2DAssignments
  deriving (Show, Eq)

data Array2DAssignments 
    = Array2DAssignmentsEmpty
    | Array2DAssignments Array2DAssignments ArrayAssignment1D
  deriving (Show, Eq)

data ListAssignment 
    = ListAssignmentArray ArrayAssignment1D
    | ListAssignmentRange Integer Integer
  deriving (Show, Eq)

data Class 
    = ClassInheritance String String ClassBlock
    | ClassNormal String ClassBlock
  deriving (Show, Eq)

data ClassBlock 
    = ClassBlock ClassAttributes ClassConstructor ClassFunctions
  deriving (Show, Eq)

data ClassAttributes 
    = ClassAttributesEmpty
    | ClassAttributes ClassAttributes ClassAttribute
  deriving (Show, Eq)

data ClassAttribute 
    = ClassAttributeTypePublic Type String
    | ClassAttributeListPublic ListType String
    | ClassAttributeTypePrivate Type String
    | ClassAttributeListPrivate ListType String
  deriving (Show, Eq)

data ClassFunctions 
    = ClassFunctionsEmpty
    | ClassFunctions ClassFunctions ClassFunction
  deriving (Show, Eq)

data ClassFunction 
    = ClassFunctionPublic Function
    | ClassFunctionPrivate Function
  deriving (Show, Eq)

data ClassConstructor 
    = ClassConstructorEmpty
    | ClassConstructor TypeFuncParams String Params Block
  deriving (Show, Eq)

data LiteralOrVariable 
    = VarIdentifier String
    | IntegerLiteral Integer
    | DecimalLiteral Decimal
    | StringLiteral String
  deriving (Show, Eq)

data Block 
    = Block
  deriving (Show, Eq)  


main = do 
  inStr <- getContents
  let parseTree = ooh (alexScanTokens2 inStr)
  putStrLn ("SUCCESS " ++ show(parseTree) )
{-# LINE 1 "templates/GenericTemplate.hs" #-}



















































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

