{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module VirtualMachine where 
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Cont
import Control.Exception
import Data.Stack as Stack
import Quadruple
import DataTypes
import CodeGenDataTypes
import Data.Decimal
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import System.CPUTime
import Text.Printf
import  System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)
import System.IO
import Control.Concurrent
data VMValue 
    = VMInteger Integer
    | VMDecimal Decimal
    | VMString String
    | VMBool Bool
    | VMEmpty
  deriving (Eq)

instance Show VMValue where
    show val = case val of
        VMInteger int  -> id $ show int
        VMDecimal dec -> id $ show dec
        VMString str -> id str
        VMBool bool -> id $ show bool
        VMEmpty -> id "~~~"

instance ExpressionOperation VMValue where
   (VMInteger int1) |+| (VMInteger int2) = (VMInteger (int1 + int2))
   (VMInteger int1) |+| (VMDecimal dec2) = (VMDecimal ((intToDecimal int1) + dec2))
   (VMDecimal dec1) |+| (VMInteger int2) = (VMDecimal (dec1 + (intToDecimal int2)))
   (VMDecimal dec1) |+| (VMDecimal dec2) = (VMDecimal (dec1 + dec2))
   (VMString str1) |+| (VMString str2) = (VMString (str1 ++ str2))

   (VMInteger int1) |-| (VMInteger int2) = (VMInteger (int1 - int2))
   (VMInteger int1) |-| (VMDecimal dec2) = (VMDecimal ((intToDecimal int1) - dec2))
   (VMDecimal dec1) |-| (VMInteger int2) = (VMDecimal (dec1 - (intToDecimal int2)))
   (VMDecimal dec1) |-| (VMDecimal dec2) = (VMDecimal (dec1 - dec2))

   (VMInteger int1) |*| (VMInteger int2) = (VMInteger (int1 * int2))
   (VMInteger int1) |*| (VMDecimal dec2) = (VMDecimal ((intToDecimal int1) * dec2))
   (VMDecimal dec1) |*| (VMInteger int2) = (VMDecimal (dec1 * (intToDecimal int2)))
   (VMDecimal dec1) |*| (VMDecimal dec2) = (VMDecimal (dec1 * dec2))

   (VMInteger int1) |/| (VMInteger int2) = (VMInteger ( decToInt $ (intToDecimal int1) / (intToDecimal int2) ))
   (VMInteger int1) |/| (VMDecimal dec2) = (VMDecimal ((intToDecimal int1) / dec2))
   (VMDecimal dec1) |/| (VMInteger int2) = (VMDecimal (dec1 / (intToDecimal int2)))
   (VMDecimal dec1) |/| (VMDecimal dec2) = (VMDecimal (dec1 / dec2))

   (VMInteger int1) |%| (VMInteger int2) = (VMInteger (int1 `mod` int2))

   (VMInteger int1) |^| (VMInteger int2) = (VMInteger (int1 ^ int2))
   (VMInteger int1) |^| (VMDecimal dec2) = (VMDecimal (doubleToDecimal ((intToDouble int1) ** (decToDouble dec2))))
   (VMDecimal dec1) |^| (VMInteger int2) = (VMDecimal (doubleToDecimal ((decToDouble dec1) ** (intToDouble int2))))
   (VMDecimal dec1) |^| (VMDecimal dec2) = (VMDecimal (doubleToDecimal ((decToDouble dec1) ** (decToDouble dec2))))

   (VMInteger int1) |==| (VMInteger int2) = (VMBool (int1 == int2))
   (VMDecimal dec1) |==| (VMDecimal dec2) = (VMBool (dec1 == dec2))
   (VMBool bool1)   |==| (VMBool bool2) = (VMBool (bool1 == bool2))
   (VMString str1) |==| (VMString str2) = (VMBool (str1 == str2))

   (VMInteger int1) |!=| (VMInteger int2) = (VMBool (int1 /= int2))
   (VMDecimal dec1) |!=| (VMDecimal dec2) = (VMBool (dec1 /= dec2))
   (VMBool bool1)   |!=| (VMBool bool2) = (VMBool (bool1 /= bool2))
   (VMString str1)  |!=| (VMString str2) = (VMBool (str1 /= str2))

   (VMBool bool1)   |&&| (VMBool bool2) = (VMBool (bool1 && bool2))

   (VMBool bool1)   |-||-| (VMBool bool2) = (VMBool (bool1 || bool2))

   (VMInteger int1) |>| (VMInteger int2) = (VMBool (int1 > int2))
   (VMDecimal dec1) |>| (VMDecimal dec2) = (VMBool (dec1 > dec2))

   (VMInteger int1) |<| (VMInteger int2) = (VMBool (int1 < int2))
   (VMDecimal dec1) |<| (VMDecimal dec2) = (VMBool (dec1 < dec2))

   (VMInteger int1) |>=| (VMInteger int2) = (VMBool (int1 >= int2))
   (VMDecimal dec1) |>=| (VMDecimal dec2) = (VMBool (dec1 >= dec2))

   (VMInteger int1) |<=| (VMInteger int2) = (VMBool (int1 <= int2))
   (VMDecimal dec1) |<=| (VMDecimal dec2) = (VMBool (dec1 <= dec2))

   (|!|)  (VMBool bool)  = (VMBool (not bool))

data ExitState = PANIC
                | HALT
                | OK
                deriving (Show,Eq)


data CPUState = CPUState
                {   exitState :: ExitState, 
                    ip :: Int,
                    globalMemory :: Memory,
                    localMemory :: Memory,
                    objectMemory :: ObjectMemory,
                    returnAddresses :: [Address] -- Cuando se retornan valores en una funcion, se llena este arreglo
                }
                deriving (Show)

data CPUContext = CPUContext
                {   currentInstructions :: [Quadruple], 
                    functionDirectory :: FunctionMemoryMap,
                    isMainContext :: Bool
                }
                deriving (Show)

data FunctionMemory = FunctionMemory
                      {   instructions :: [Quadruple],
                          funcMainMemory :: Memory,
                          funcObjectMemory :: ObjectMemory
                      } deriving (Show)

type Memory = Map.HashMap Address VMValue
-- La memoria de un objeto puede verse como una relación unos a muchos: un sólo registro está ligado a muchos otros, que son sus atributos
type ObjectMemory = Map.HashMap Address [Address]
type FunctionMemoryMap = Map.HashMap String FunctionMemory


type Output = String

type VirtualMachine a = RWST CPUContext [String] (CPUState) IO a

type VM =  VirtualMachine ()

printMessage :: String -> IO ()
printMessage str = putStrLn $ (color Cyan $ style Bold $ "[VM] ") ++ str  

startVM :: [Quadruple] -> Memory -> Memory -> ObjectMemory -> FunctionMemoryMap -> IO ()
startVM quads globalMemory localMemory objectMemory funcMemMap = 
    do 
       printMessage $ color White $ style Bold $ "Execution in process..."
       start <- getCPUTime
       (a,w) <- evalRWST (runVM) (CPUContext quads funcMemMap True) (setInitialCPUState globalMemory localMemory objectMemory []) 
       end   <- getCPUTime
       mapM_ (putStrLn) $ w 
       let diff = (fromIntegral (end - start)) / (10^12)
       let msg1 = style Bold $ "Finished" ++ " in " ++ ( show (diff::Decimal) ) ++ " sec"
       printMessage msg1
       -- putStrLn $ msg1 

setInitialCPUState :: Memory -> Memory -> ObjectMemory -> [Address] -> CPUState
setInitialCPUState globalMem localMem objMemory returnAddresses = CPUState OK 0 globalMem localMem objMemory returnAddresses

getCPUState :: CPUState -> (ExitState,Int,Memory,Memory,ObjectMemory,[Address])
getCPUState (CPUState eState ip globalMemory localMemory objectMemory returnAddresses) = (eState,ip,globalMemory,localMemory,objectMemory,returnAddresses)

runVM :: VM
runVM = do
        context <-  ask
        let quadruples = (currentInstructions context)
        cpuState <-  get
        let (exitState,currentInstructionPointer,_,_,_,_) = getCPUState cpuState
        if (exitState == PANIC) 
            then do 
                tell $ [("Ended execution with an error at quadruple number " ++ (style Bold (show currentInstructionPointer)) )]
                return ()
        else if (exitState == HALT)
            then return ()
        else do
            if currentInstructionPointer < (length quadruples) then do
                let currentInstruction = quadruples !! currentInstructionPointer
                (runInstruction currentInstruction)
                runVM 
                return ()
            else do
                -- tell $ [color Cyan "Done"]
                return ()

runInstruction :: Quadruple -> VM
-- Si es un NOP, solamente aumentamos a uno el instruction pointer
runInstruction (QuadrupleEmpty _ _) = do 
                                        cpuState <- get
                                        let (_,currentIP,_,_,_,_) = getCPUState cpuState
                                        let s' = (cpuState { ip = (ip cpuState) + 1 })
                                        put s'
                                        return ()
runInstruction (QuadrupleThreeAddresses quadNum ADD_ a1 a2 a3) = do doAbstractOperation (|+|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum SUB_ a1 a2 a3) =  do doAbstractOperation (|-|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum MULTIPLY_ a1 a2 a3) =  do doAbstractOperation (|*|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum DIVIDE_ a1 a2 a3) =  do doAbstractOperation (|/|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum MOD_ a1 a2 a3) =  do doAbstractOperation (|%|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum POWER_ a1 a2 a3) =  do doAbstractOperation (|^|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum GT_ a1 a2 a3) =  do doAbstractOperation (|>|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum LT_ a1 a2 a3) =  do doAbstractOperation (|<|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum GTEQ_ a1 a2 a3) =  do doAbstractOperation (|>=|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum LTEQ_ a1 a2 a3) =  do doAbstractOperation (|<=|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum EQ_ a1 a2 a3) =  do doAbstractOperation (|==|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum NOTEQ_ a1 a2 a3) =  do doAbstractOperation (|!=|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum AND_ a1 a2 a3) =  do doAbstractOperation (|&&|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum OR_ a1 a2 a3) =  do doAbstractOperation (|-||-|) a1 a2 a3 
runInstruction (QuadrupleTwoAddresses quadNum NOT_ a1 a2) =  do doAbstractUnaryOp (|!|) a1 a2 
runInstruction (QuadrupleTwoAddresses quadNum ASSIGNMENT a1 a2) =  do 
                                                                    doAssignment a1 a2 
                                                                    modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleOneAddressOneQuad quadNum GOTO_IF_FALSE a1 quadNumToJump) =  do doGotoIfFalse a1 quadNumToJump 
runInstruction (QuadrupleOneQuad quadNum GOTO quadNumToJump) =  
                                                                do
                                                                    cpuState <- get
                                                                    let (_,currentIP,_,_,_,_) = getCPUState cpuState
                                                                    modify $ \s -> (cpuState { ip = fromIntegral quadNumToJump })
runInstruction (QuadrupleOneAddress quadNum READ a1) 
                                    | a1 >= startIntGlobalMemory && a1 <= endIntGlobalMemory     
                                      || a1 >= startIntLocalMemory && a1 <= endIntLocalMemory =
                                                                do 
                                                                    cpuState <- get
                                                                    context <-  ask
                                                                    let quadruples = (currentInstructions context)
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ printMessage $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: Integer" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    -- lift $ catch (seq (read x :: Integer) $ return()) showError
                                                                    case (checkInt x) of 
                                                                        Just int -> do
                                                                                        insertValueInAddress (VMInteger int) a1
                                                                                        modify $ \s -> (s { ip = (ip s) + 1 })
                                                                        Nothing -> do
                                                                            liftIO $ printMessage $ color Yellow . style Bold $ ("Runtime Recovery: Please enter an Integer number")
                                                                            return()
                                                                    liftIO $ hClose tty
                                    | a1 >= startDecimalGlobalMemory && a1 <= endDecimalGlobalMemory     
                                      || a1 >= startDecimalLocalMemory && a1 <= endDecimalLocalMemory =
                                                                do 
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ putStrLn $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: Decimal" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    -- lift $ catch (seq (read x :: Integer) $ return()) showError
                                                                    case (checkDecimal x) of 
                                                                        Just dec -> do 
                                                                                        insertValueInAddress (VMDecimal dec) a1
                                                                                        modify $ \s -> (s { ip = (ip s) + 1 })
                                                                        Nothing -> do
                                                                            liftIO $ printMessage $ color Yellow . style Bold $ ("Runtime Recovery: Please enter a Decimal number")
                                                                            return()
                                                                    liftIO $ hClose tty
                                    | a1 >= startStringGlobalMemory && a1 <= endStringGlobalMemory     
                                      || a1 >= startStringLocalMemory && a1 <= endStringLocalMemory =
                                                                do 
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ putStrLn $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: String" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    -- lift $ catch (seq (read x :: Integer) $ return()) showError
                                                                    insertValueInAddress (VMString x) a1
                                                                    modify $ \s -> (s { ip = (ip s) + 1 })
                                                                    liftIO $ hClose tty
                                    | a1 >= startBoolGlobalMemory && a1 <= endBoolGlobalMemory     
                                      || a1 >= startBoolLocalMemory && a1 <= endBoolLocalMemory =
                                                                do 
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ putStrLn $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: True | False" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    -- lift $ catch (seq (read x :: Integer) $ return()) showError
                                                                    case (checkBool x) of 
                                                                        Just bool -> do 
                                                                                        insertValueInAddress (VMBool bool) a1
                                                                                        modify $ \s -> (s { ip = (ip s) + 1 })
                                                                        Nothing -> do
                                                                            liftIO $ putStrLn $ color Yellow . style Bold $ ("Runtime Recovery: Please enter a Bool")
                                                                            return()
                                                                    liftIO $ hClose tty
                                    | otherwise = do 
                                                    modify $ \s -> (s { exitState = PANIC })
                                                    tell $ [color Red $ "BAD ADDRESS : " ++ show a1] 
                                                    return ()       
runInstruction (QuadrupleOneAddress quadNum DISPLAY a1) = do 
                                                            doDisplay a1 0
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleOneAddress quadNum DISPLAY_LINE a1) = do 
                                                            doDisplay a1 0 
                                                            liftIO $ putStrLn $ ""
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleOneAddress quadNum DISPLAY_VALUE_IN_INDEX a1) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger addressFromArray) -> do 
                                                                                                      doDisplay addressFromArray 0
                                                                                                      modify $ \s -> (s { ip = (ip s) + 1 })
                                                            
runInstruction (QuadrupleOneAddress quadNum DOUBLE a1) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState 
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                                Just (VMDecimal dec) -> 
                                                                    let roundedDec = roundTo' (ceiling) 16 dec
                                                                    in insertValueInAddress (VMDecimal roundedDec) a1
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleOneAddress quadNum INT_64 a1) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState 
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                                Just (VMInteger int) -> 
                                                                    do
                                                                        let maxInt64 = maxBound :: Int
                                                                        let minInt64 = minBound :: Int
                                                                        if (int < (fromIntegral minInt64) || int > (fromIntegral maxInt64))
                                                                            then do 
                                                                                modify $ \s -> (s { exitState = PANIC })
                                                                                liftIO $ printMessage $ (color Red . style Bold $ ("Int underflowed/overflowed with value: " )) ++ (color White . style Bold $ (show int))
                                                                                liftIO $ printMessage $ color Magenta . style Bold $ ("Consider using an Integer instead" )
                                                                                insertValueInAddress (VMInteger 0) a1
                                                                            else return ()
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleThreeAddresses quadNum ADD_INDEX index value a3) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                                                            case (Map.lookup value (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger int) -> do 
                                                                                        -- liftIO $ putStrLn $ (show (index + int))  ++ " " ++ (show a3) 
                                                                                        insertValueInAddress (VMInteger (index + int)) a3 
                                                            modify $ \s -> (s { ip = (ip s) + 1 })


runInstruction (QuadrupleTwoAddresses quadNum ACCESS_INDEX addressThatHasIndex a2) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                                                            case (Map.lookup addressThatHasIndex (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger addressFromArray) -> do 
                                                                                                      -- liftIO $ putStrLn $ (show addressFromArray)  ++ " " ++ (show a2)
                                                                                                      -- liftIO $ putStrLn.ppShow $ globalMemory
                                                                                                      doAssignment addressFromArray a2
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleTwoAddresses quadNum PUT_INDEX a1 addressThatHasIndex) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                                                            case (Map.lookup addressThatHasIndex (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger addressFromArray) -> doAssignment a1 addressFromArray
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleTwoAddresses quadNum BOUNDS a1 a2) = do 
                                                              cpuState <- get
                                                              let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                                                              case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                                Just (VMInteger val) -> 
                                                                           do 
                                                                            case (Map.lookup a2 (Map.union globalMemory localMemory)) of 
                                                                              Just (VMInteger upperBound) -> 
                                                                                      if (val >= upperBound || val < 0) then 
                                                                                        do
                                                                                          modify $ \s -> (s { exitState = PANIC })
                                                                                          liftIO $ printMessage $ (color Red . style Bold $ ("Index out of bounds with value: " )) ++ (color White . style Bold $ (show val))
                                                                                      else do 
                                                                                          modify $ \s -> (s { ip = (ip s) + 1 })

runInstruction (QuadrupleReturn quadNum RETURN addressesToReturn) = do 
                                                                      modify $ \s -> (s { returnAddresses = addressesToReturn })
                                                                      modify $ \s -> (s { exitState = HALT })



runInstruction (QuadrupleFunctionCall quadNum GO_SUB addressesObjParams addressesParams funcName) = 
                                                                                  do 
                                                                                    context <-  ask
                                                                                    cpuState <- get
                                                                                    let funcMap = (functionDirectory context)
                                                                                    case (Map.lookup funcName funcMap) of 
                                                                                      Just (FunctionMemory funcInstructions funcIdMem funcObjMem) ->
                                                                                          do 
                                                                                            let (_,currentIP,globalMemory,currentLocalMem,currentObjMem,_) = getCPUState cpuState
                                                                                            -- A continuacion se sustituira la currentLocalMemory por la local memory de la funcion. Sin embargo, se guarda el currentLocalMemory
                                                                                            let addressesCurrentContext = (map (\f-> fst f) addressesParams)
                                                                                            let addressesFuncContext = (map (\f-> snd f) addressesParams)

                                                                                            let addressesObjParamsCurrentContext = (map (\f-> fst f) addressesObjParams)
                                                                                            let addressesObjFuncContext = (map (\f-> snd f) addressesObjParams)

                                                                                            let localMemFunc = doDeepAssignmentMemories (Map.union globalMemory currentLocalMem) funcIdMem currentObjMem funcObjMem addressesCurrentContext addressesFuncContext
                                                                                            let localMemFuncNew = doDeepAssignmentMemories (Map.union globalMemory currentLocalMem) localMemFunc currentObjMem funcObjMem addressesObjParamsCurrentContext addressesObjFuncContext   
                                                            
                                                                                            (stateAfterFunc,_) <- liftIO $ execRWST (runVM) (CPUContext funcInstructions funcMap False) (setInitialCPUState globalMemory localMemFuncNew funcObjMem [])
                                                                                            let (_,_,_,localMemAfterFunc,objMemAfterFunc,returnAddresses) = getCPUState stateAfterFunc
                                                                                            -- Se sustituyen
                                                                                            if (currentIP + 1) < (length (currentInstructions context)) then do
                                                                                                let returnInstruction = (currentInstructions context) !! (currentIP + 1)
                                                                                                case returnInstruction of 
                                                                                                  (QuadrupleReturnSet quadNum RETURN_SET addressesCurrentContext) ->
                                                                                                        if ((isMainContext context)) then 
                                                                                                          do 
                                                                                                            let newGlobalMemory = doDeepAssignmentMemories localMemAfterFunc globalMemory objMemAfterFunc currentObjMem returnAddresses addressesCurrentContext
                                                                                                            modify $ \s -> (s { globalMemory =  newGlobalMemory })
                                                                                                            modify $ \s -> (s { ip = (ip s) + 1 })
                                                                                                            modify $ \s -> (s { objectMemory =  currentObjMem })
                                                                                                        else do 
                                                                                                               let newLocalMemory = doDeepAssignmentMemories localMemAfterFunc currentLocalMem objMemAfterFunc currentObjMem returnAddresses addressesCurrentContext
                                                                                                               modify $ \s -> (s { localMemory =  newLocalMemory })
                                                                                                               modify $ \s -> (s { ip = (ip s) + 1 })
                                                                                                               modify $ \s -> (s { objectMemory =  currentObjMem })
                                                                                                  _ -> do 
                                                                                                        modify $ \s -> (s { localMemory =  currentLocalMem })
                                                                                                        modify $ \s -> (s { objectMemory =  currentObjMem })
                                                                                                        modify $ \s -> (s { ip = (ip s) + 1 })
                                                                                            else 
                                                                                              do 
                                                                                                modify $ \s -> (s { localMemory =  currentLocalMem })
                                                                                                modify $ \s -> (s { objectMemory =  currentObjMem })
                                                                                                modify $ \s -> (s { ip = (ip s) + 1 })
                                                                                            
                                        

runInstruction _ =  do
                        modify $ \s -> (s { ip = (ip s) + 1 })
                        return ()


doDisplay :: Address -> Int -> VM
doDisplay a1 nestFactor
        -- Si es un objeto, la asignacion debe hacerse considerando todos sus atributos
     | a1 >= startObjectLocalMemory && a1 <= endObjectLocalMemory 
        || a1 >= startObjectGlobalMemory && a1 <= endObjectGlobalMemory  = do 
                                                                            cpuState <- get
                                                                            liftIO $ putStr $ take nestFactor $ cycle "\t"
                                                                            liftIO $ putStrLn $ (color Magenta $ "<object>" ) ++ (color White $ "{")
                                                                            let (panic,currentIP,globalMemory,localMemory,objectMemory,_) = getCPUState cpuState
                                                                            case (Map.lookup a1 objectMemory) of
                                                                                Just addressesFromObject -> do 
                                                                                        doDeepDisplay addressesFromObject (nestFactor + 1)
                                                                            liftIO $ putStrLn $ ""
                                                                            liftIO $ putStr $ take nestFactor $ cycle "\t"
                                                                            liftIO $ putStrLn $ color White $ "}"
     | otherwise = do 
                cpuState <- get
                let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                let memories = (Map.union globalMemory localMemory) 
                liftIO $ putStr $ take nestFactor $ cycle "\t"
                case (Map.lookup a1 memories) of 
                    Just (VMString val) -> do 
                            liftIO $ putStr $ (style Underline $ val)  ++ " "
                    Just VMEmpty ->
                                do liftIO $ putStr $ " " 
                                   return ()
                    Just val -> do 
                            liftIO $ putStr $ (show $ val) ++ " "
                            -- tell $ [show val]
                    _ -> do 
                            return ()
                return ()

doAssignment :: Address -> Address -> VM
doAssignment a1 a2 
    -- Si es un objeto, la asignacion debe hacerse considerando todos sus atributos
     | a1 >= startObjectLocalMemory && a1 <= endObjectLocalMemory 
     || a1 >= startObjectGlobalMemory && a1 <= endObjectGlobalMemory = do 
                                                                        cpuState <- get
                                                                        let (panic,currentIP,globalMemory,localMemory,objectMemory,_) = getCPUState cpuState
                                                                        -- liftIO $ putStrLn $ (show a1)  ++ " " ++ (show a2) 
                                                                        case (Map.lookup a1 objectMemory) of
                                                                            Just addressesAttributesGiver ->
                                                                                do 
                                                                                    case (Map.lookup a2 objectMemory) of
                                                                                        Just addressesAttributesReceiver ->
                                                                                            do 
                                                                                                doDeepAssignment addressesAttributesGiver addressesAttributesReceiver
                                                                                                                                                           
    | otherwise = do 
                    cpuState <- get
                    let (panic,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                    let memories = (Map.union globalMemory localMemory)
                    case (Map.lookup a1 memories) of
                        Just val -> do 
                                        insertValueInAddress val a2
                        _ -> do 
                                -- tell $ ["Address " ++ show a1  ++  " was not found in any memory"]
                                return ()

                  
doDeepAssignment :: [Address] -> [Address] -> VM
doDeepAssignment [] [] = do return ()
doDeepAssignment (addrGiver : addressesGiver ) (addrReceiver : addressesReceiver ) = do 
                                                                                    doAssignment addrGiver addrReceiver
                                                                                    doDeepAssignment addressesGiver addressesReceiver
                                                                                    return ()


doAssignmentMemories :: Memory -> Memory -> ObjectMemory -> ObjectMemory -> Address -> Address -> Memory
doAssignmentMemories memBase memDestiny objMemBase objMemDestiny a1 a2 
    -- Si es un objeto, la asignacion debe hacerse considerando todos sus atributos
     | a1 >= startObjectLocalMemory && a1 <= endObjectLocalMemory 
     || a1 >= startObjectGlobalMemory && a1 <= endObjectGlobalMemory = case (Map.lookup a1 objMemBase) of
                                                                            Just addressesAttributesGiver ->
                                                                                    case (Map.lookup a2 objMemDestiny) of
                                                                                        Just addressesAttributesReceiver ->
                                                                                            doDeepAssignmentMemories memBase memDestiny objMemBase objMemDestiny addressesAttributesGiver addressesAttributesReceiver 
                                                                        
                                                                                                                                                           
    | otherwise = case (Map.lookup a1 memBase) of
                        Just valBase -> 
                          let newDestinyMemory = (Map.insert a2 valBase memDestiny)
                          in newDestinyMemory


                  
doDeepAssignmentMemories :: Memory -> Memory -> ObjectMemory -> ObjectMemory -> [Address] -> [Address] -> Memory
doDeepAssignmentMemories _ memoryDestiny _ _ [] [] = memoryDestiny
doDeepAssignmentMemories memBase memDestiny objMemBase objMemDestiny (addrGiver : addressesGiver ) (addrReceiver : addressesReceiver ) =
                                                                                    let memDestinyNew = doAssignmentMemories memBase memDestiny objMemBase objMemDestiny addrGiver addrReceiver
                                                                                    in doDeepAssignmentMemories memBase memDestinyNew objMemBase objMemDestiny addressesGiver addressesReceiver
                                                                                    

doDeepDisplay :: [Address] -> Int -> VM
doDeepDisplay [] _ = do return ()
doDeepDisplay (addr : addresses ) nestFactor = do 

                                        liftIO $ putStr $ take (nestFactor)  $ cycle "\t"
                                        liftIO $ putStrLn $  (color Blue $ "<attribute>" ) ++ (color White $ "{")
                                        doDisplay addr (nestFactor + 1)
                                        liftIO $ putStrLn $ ""
                                        liftIO $ putStr $ take nestFactor $ cycle "\t"
                                        liftIO $ putStrLn $ color White $ "}"
                                        doDeepDisplay addresses nestFactor
                                        return ()

doAbstractOperation :: (VMValue -> VMValue -> VMValue) -> Address -> Address -> Address -> VM
doAbstractOperation f a1 a2 a3 = do 
                                        cpuState <- get
                                        let (_,_,globalMemory,localMemory,_,_) = getCPUState cpuState
                                            memories = (Map.union globalMemory localMemory) 
                                            valResult = doOperation f a1 a2 memories
                                        case valResult of 
                                            Left err ->  
                                                    do 
                                                        modify $ \s -> (cpuState { exitState = PANIC })
                                                        tell $ [color Red $ err]
                                            Right val -> do 
                                                        insertValueInAddress val a3
                                                        modify $ \s -> (s { ip = (ip s) + 1 })
                                        

doOperation :: (VMValue -> VMValue -> VMValue) -> Address -> Address -> Memory -> (Either String VMValue)
doOperation f a1 a2 memory = case (Map.lookup a1 memory) of
                                Just VMEmpty -> (Left "ERROR: Variable in expression was never initialized")
                                Just vmVal1 -> do 
                                                case (Map.lookup a2 memory) of
                                                    Just VMEmpty -> (Left "ERROR: Variable in expression was never initialized")
                                                    Just vmVal2 -> (Right $ f vmVal1 vmVal2)
                                                                

insertValueInAddress :: VMValue -> Address -> VM
insertValueInAddress val address = do
                            cpuState <- get
                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState 
                            if (address >= startIntGlobalMemory && address <= endBoolGlobalMemory) 
                                then do
                                    let newGlobalMemory = (Map.insert address val globalMemory)
                                    modify $ \s -> (cpuState { globalMemory = newGlobalMemory})

                            else do
                                if (address >= startIntLocalMemory && address <= endBoolLocalMemory) then do
                                    let newLocalMemory = (Map.insert address val localMemory)
                                    modify $ \s -> (cpuState { localMemory = newLocalMemory})
                                else do
                                    modify $ \s -> (cpuState { exitState = PANIC})
                                    tell $ [("Address " ++ show address  ++  " assignment underflow/overflow ")]
                            return ()

doGotoIfFalse :: Address -> QuadNum -> VM
doGotoIfFalse a1 quadNum = do 
                            cpuState <- get
                            let (_,currentIP,globalMemory,localMemory,_,_) = getCPUState cpuState
                            let memories = (Map.union globalMemory localMemory)
                            case (Map.lookup a1 memories) of 
                                Just (VMBool bool) -> do
                                    -- Si es falso, entonces si hago el jump
                                    if not bool then 
                                        modify $ \s -> (cpuState { ip = fromIntegral quadNum })
                                    -- Si no solo sigo al siguiente cuadruplo
                                    else modify $ \s -> (cpuState { ip = currentIP + 1  })
                                Just _ -> do
                                            modify $ \s -> (cpuState { exitState = PANIC}) 
                                            tell $ ["Address " ++ show a1  ++  " was not a boolean"]
                                _ -> do
                                        modify $ \s -> (cpuState { exitState = PANIC}) 
                                        tell $ ["Address " ++ show a1  ++  " not found"]

doAbstractUnaryOp :: (VMValue -> VMValue) -> Address  -> Address -> VM
doAbstractUnaryOp f a1 a2 = do 
                                        cpuState <- get
                                        let (_,_,globalMemory,localMemory,_,_) = getCPUState cpuState
                                        let memories = (Map.union globalMemory localMemory) 
                                        case (Map.lookup a1 memories) of 
                                            Just val -> 
                                                        do 
                                                          let valResult = f val
                                                          insertValueInAddress valResult a2
                                                          modify $ \s -> (s { ip = (ip s) + 1 })
                                            _ -> do
                                                    modify $ \s -> (cpuState { exitState = PANIC}) 
                                                    tell $ ["Address " ++ show a1  ++  " not found"]


checkInt :: String -> Maybe Integer
checkInt str =
  case reads str of
     [(i, [])] -> Just i
     _         -> Nothing

checkDecimal :: String -> Maybe Decimal
checkDecimal str =
  case reads str of
     [(i, [])] -> Just i
     _         -> Nothing

checkBool :: String -> Maybe Bool
checkBool str =
  case reads str of
     [(i, [])] -> Just i
     _         -> Nothing

