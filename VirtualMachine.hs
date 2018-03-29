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
import Data.Decimal
import Text.Show.Pretty
import qualified Data.HashMap.Strict as Map
import System.CPUTime
import Text.Printf
import  System.Console.Pretty (Color (..), Style (..), bgColor, color,
                                        style, supportsPretty)
import System.IO
import Control.Concurrent
data CPUState = CPUState
                {   panic :: Bool, 
                    ip :: Int,
                    globalMemory :: Memory,
                    localMemory :: Memory,
                    objectMemory :: ObjectMemory
                }
                deriving (Show, Eq)

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

type Memory = Map.HashMap Address VMValue
-- La memoria de un objeto puede verse como una relación unos a muchos: un sólo registro está ligado a muchos otros, que son sus atributos
type ObjectMemory = Map.HashMap Address [Address]
type Output = String

-- newtype VirtualMachine a = VirtualMachine{
--     unwrapVM :: RWST [Quadruple] [String] (CPUState) IO a
-- } deriving (Functor, Applicative, Monad, MonadIO,MonadRWS [Quadruple] [String] CPUState)

-- instance MonadReader [Quadruple] VirtualMachine where
--     ask = VirtualMachine ask

-- instance MonadWriter [String] VirtualMachine where
--     tell = VirtualMachine . tell
  

-- instance MonadState CPUState VirtualMachine where
--     get = VirtualMachine get
--     put s = VirtualMachine . put $ s 

type VirtualMachine a = RWST [Quadruple] [String] (CPUState) IO a

type VM =  VirtualMachine ()

printMessage :: String -> IO ()
printMessage str = putStrLn $ (color Cyan $ style Bold $ "[VM] ") ++ str  

startVM :: [Quadruple] -> Memory -> Memory -> ObjectMemory -> IO ()
startVM quads globalMemory localMemory objectMemory = 
    do 
       printMessage $ color White $ style Bold $ "Execution in process..."
       start <- getCPUTime
       (a,w) <- evalRWST (runVM) quads (setInitialCPUState globalMemory localMemory objectMemory) 
       end   <- getCPUTime
       mapM_ (putStrLn) $ w 
       let diff = (fromIntegral (end - start)) / (10^12)
       let msg1 = style Bold $ "Finished" ++ " in " ++ ( show (diff::Decimal) ) ++ " sec"
       printMessage msg1
       -- putStrLn $ msg1 

setInitialCPUState :: Memory -> Memory -> ObjectMemory -> CPUState
setInitialCPUState globalMem localMem objMemory = CPUState False 0 globalMem localMem objMemory

getCPUState :: CPUState -> (Bool,Int,Memory,Memory,ObjectMemory)
getCPUState (CPUState panic ip globalMemory localMemory objectMemory) = (panic,ip,globalMemory,localMemory,objectMemory)

runVM :: VM
runVM = do
        quadruples <-  ask
        cpuState <-  get
        let (isPanicState,currentInstructionPointer,_,_,_) = getCPUState cpuState
        if (isPanicState) 
            then do 
                tell $ [("Ended execution with an error at quadruple number " ++ (style Bold (show currentInstructionPointer)) )]
                return ()
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
                                        let (_,currentIP,_,_,_) = getCPUState cpuState
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
                                                                    let (_,currentIP,_,_,_) = getCPUState cpuState
                                                                    modify $ \s -> (cpuState { ip = fromIntegral quadNumToJump })
runInstruction (QuadrupleOneAddress quadNum READ a1) 
                                    | a1 >= startIntGlobalMemory && a1 <= endIntGlobalMemory     
                                      || a1 >= startIntLocalMemory && a1 <= endIntLocalMemory =
                                                                do 
                                                                    cpuState <- get
                                                                    quadruples <-  ask
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ printMessage $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: Integer" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    let (_,currentIP,globalMemory,localMemory,objectMemory) = getCPUState cpuState
                                                                    (a,s) <- liftIO $ execRWST (runVM) [] (setInitialCPUState globalMemory localMemory objectMemory) 
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
                                                    modify $ \s -> (s { panic = True })
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
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger addressFromArray) -> do 
                                                                                                      doDisplay addressFromArray 0
                                                                                                      modify $ \s -> (s { ip = (ip s) + 1 })
                                                            
runInstruction (QuadrupleOneAddress quadNum DOUBLE a1) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState 
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                                Just (VMDecimal dec) -> 
                                                                    let roundedDec = roundTo' (ceiling) 16 dec
                                                                    in insertValueInAddress (VMDecimal roundedDec) a1
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleOneAddress quadNum INT_64 a1) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState 
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                                Just (VMInteger int) -> 
                                                                    do
                                                                        let maxInt64 = maxBound :: Int
                                                                        let minInt64 = minBound :: Int
                                                                        if (int < (fromIntegral minInt64) || int > (fromIntegral maxInt64))
                                                                            then do 
                                                                                modify $ \s -> (s { panic = True })
                                                                                liftIO $ printMessage $ (color Red . style Bold $ ("Int underflowed/overflowed with value: " )) ++ (color White . style Bold $ (show int))
                                                                                liftIO $ printMessage $ color Magenta . style Bold $ ("Consider using an Integer instead" )
                                                                                insertValueInAddress (VMInteger 0) a1
                                                                            else return ()
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleThreeAddresses quadNum ADD_INDEX index value a3) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
                                                            case (Map.lookup value (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger int) -> do 
                                                                                        -- liftIO $ putStrLn $ (show (index + int))  ++ " " ++ (show a3) 
                                                                                        insertValueInAddress (VMInteger (index + int)) a3 
                                                            modify $ \s -> (s { ip = (ip s) + 1 })


runInstruction (QuadrupleTwoAddresses quadNum ACCESS_INDEX addressThatHasIndex a2) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
                                                            case (Map.lookup addressThatHasIndex (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger addressFromArray) -> do 
                                                                                                      -- liftIO $ putStrLn $ (show addressFromArray)  ++ " " ++ (show a2)
                                                                                                      -- liftIO $ putStrLn.ppShow $ globalMemory
                                                                                                      doAssignment addressFromArray a2
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleTwoAddresses quadNum PUT_INDEX a1 addressThatHasIndex) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
                                                            case (Map.lookup addressThatHasIndex (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger addressFromArray) -> doAssignment a1 addressFromArray
                                                            modify $ \s -> (s { ip = (ip s) + 1 })
runInstruction (QuadrupleTwoAddresses quadNum BOUNDS a1 a2) = do 
                                                            cpuState <- get
                                                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
                                                            case (Map.lookup a1 (Map.union globalMemory localMemory)) of
                                                              Just (VMInteger val) -> 
                                                                         do 
                                                                          case (Map.lookup a2 (Map.union globalMemory localMemory)) of 
                                                                            Just (VMInteger upperBound) -> 
                                                                                    if (val >= upperBound || val < 0) then 
                                                                                      do
                                                                                        modify $ \s -> (s { panic = True })
                                                                                        liftIO $ printMessage $ (color Red . style Bold $ ("Index out of bounds with value: " )) ++ (color White . style Bold $ (show val))
                                                                                    else do 
                                                                                        modify $ \s -> (s { ip = (ip s) + 1 })

                                                            

                                        

runInstruction _ =  return ()


doDisplay :: Address -> Int -> VM
doDisplay a1 nestFactor
        -- Si es un objeto, la asignacion debe hacerse considerando todos sus atributos
     | a1 >= startObjectLocalMemory && a1 <= endObjectLocalMemory 
        || a1 >= startObjectGlobalMemory && a1 <= endObjectGlobalMemory  = do 
                                                                            cpuState <- get
                                                                            liftIO $ putStr $ take nestFactor $ cycle "\t"
                                                                            liftIO $ putStrLn $ (color Magenta $ "<object>" ) ++ (color White $ "{")
                                                                            let (panic,currentIP,globalMemory,localMemory,objectMemory) = getCPUState cpuState
                                                                            case (Map.lookup a1 objectMemory) of
                                                                                Just addressesFromObject -> do 
                                                                                        doDeepDisplay addressesFromObject (nestFactor + 1)
                                                                            liftIO $ putStrLn $ ""
                                                                            liftIO $ putStr $ take nestFactor $ cycle "\t"
                                                                            liftIO $ putStrLn $ color White $ "}"
     | otherwise = do 
                cpuState <- get
                let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
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
                                                                        let (panic,currentIP,globalMemory,localMemory,objectMemory) = getCPUState cpuState
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
                    let (panic,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
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
                                        let (_,_,globalMemory,localMemory,_) = getCPUState cpuState
                                            memories = (Map.union globalMemory localMemory) 
                                            valResult = doOperation f a1 a2 memories
                                        case valResult of 
                                            Left err ->  
                                                    do 
                                                        modify $ \s -> (cpuState { panic = True })
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
                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState 
                            if (address >= startIntGlobalMemory && address <= endBoolGlobalMemory) 
                                then do
                                    let newGlobalMemory = (Map.insert address val globalMemory)
                                    modify $ \s -> (cpuState { globalMemory = newGlobalMemory})

                            else do
                                if (address >= startIntLocalMemory && address <= endBoolLocalMemory) then do
                                    let newLocalMemory = (Map.insert address val localMemory)
                                    modify $ \s -> (cpuState { localMemory = newLocalMemory})
                                else do
                                    modify $ \s -> (cpuState { panic = True})
                                    tell $ [("Address " ++ show address  ++  " assignment underflow/overflow ")]
                            return ()

doGotoIfFalse :: Address -> QuadNum -> VM
doGotoIfFalse a1 quadNum = do 
                            cpuState <- get
                            let (_,currentIP,globalMemory,localMemory,_) = getCPUState cpuState
                            let memories = (Map.union globalMemory localMemory)
                            case (Map.lookup a1 memories) of 
                                Just (VMBool bool) -> do
                                    -- Si es falso, entonces si hago el jump
                                    if not bool then 
                                        modify $ \s -> (cpuState { ip = fromIntegral quadNum })
                                    -- Si no solo sigo al siguiente cuadruplo
                                    else modify $ \s -> (cpuState { ip = currentIP + 1  })
                                Just _ -> do
                                            modify $ \s -> (cpuState { panic = True}) 
                                            tell $ ["Address " ++ show a1  ++  " was not a boolean"]
                                _ -> do
                                        modify $ \s -> (cpuState { panic = True}) 
                                        tell $ ["Address " ++ show a1  ++  " not found"]

doAbstractUnaryOp :: (VMValue -> VMValue) -> Address  -> Address -> VM
doAbstractUnaryOp f a1 a2 = do 
                                        cpuState <- get
                                        let (_,_,globalMemory,localMemory,_) = getCPUState cpuState
                                        let memories = (Map.union globalMemory localMemory) 
                                        case (Map.lookup a1 memories) of 
                                            Just val -> 
                                                        do 
                                                          let valResult = f val
                                                          insertValueInAddress valResult a2
                                                          modify $ \s -> (s { ip = (ip s) + 1 })
                                            _ -> do
                                                    modify $ \s -> (cpuState { panic = True}) 
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

