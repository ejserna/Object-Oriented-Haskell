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
data CPUState = CPUState
                {   panic :: Bool, 
                    ip :: Int,
                    globalMemory :: Memory,
                    localMemory :: Memory
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

type Memory = Map.HashMap Address VMValue
type Output = String

newtype VirtualMachine a = VirtualMachine{
    unwrapVM :: RWST [Quadruple] [String] (CPUState) IO a
} deriving (Functor, Applicative, Monad, MonadIO,MonadRWS [Quadruple] [String] CPUState)

instance MonadReader [Quadruple] VirtualMachine where
    ask = VirtualMachine ask

instance MonadWriter [String] VirtualMachine where
    tell = VirtualMachine . tell

instance MonadState CPUState VirtualMachine where
    get = VirtualMachine get
    put s = VirtualMachine . put $ s 

type VM =  VirtualMachine ()

startVM :: [Quadruple] -> Memory -> Memory -> IO ()
startVM quads globalMemory localMemory = 
    do 
       start <- getCPUTime
       (a,w) <- evalRWST (unwrapVM $ runVM) quads (setInitialCPUState globalMemory localMemory) 
       end   <- getCPUTime
       mapM_ (putStrLn) $ w 
       let diff = (fromIntegral (end - start)) / (10^12)
       let msg1 = style Bold $ "Finished" ++ " in " ++ ( show (diff::Decimal) ) ++ " sec"
       putStrLn $ msg1 

setInitialCPUState :: Memory -> Memory -> CPUState
setInitialCPUState globalMem localMem = CPUState False 0 globalMem localMem

getCPUState :: CPUState -> (Bool,Int,Memory,Memory)
getCPUState (CPUState panic ip globalMemory localMemory) = (panic,ip,globalMemory,localMemory)

runVM :: VM
runVM = do
        quadruples <-  ask
        cpuState <-  get
        let (isPanicState,currentInstructionPointer,_,_) = getCPUState cpuState
        if (isPanicState) 
            then do 
                tell $ [("Ended execution with an error at quad num" ++ (show currentInstructionPointer))]
                return ()
        else do
            if currentInstructionPointer < (length quadruples) then do
                let currentInstruction = quadruples !! currentInstructionPointer
                (runInstruction currentInstruction)
                runVM 
                return ()
            else do
                tell $ [color Cyan "Done"]
                return ()

runInstruction :: Quadruple -> VM
-- Si es un NOP, solamente aumentamos a uno el instruction pointer
runInstruction (QuadrupleEmpty _ _) = do 
                                        cpuState <- get
                                        let (_,currentIP,_,_) = getCPUState cpuState
                                        let s' = (cpuState { ip = (ip cpuState) + 1 })
                                        put s'
                                        return ()
runInstruction (QuadrupleThreeAddresses quadNum ADD_ a1 a2 a3) = do runInstructionAbstract (|+|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum SUB_ a1 a2 a3) =  do runInstructionAbstract (|-|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum MULTIPLY_ a1 a2 a3) =  do runInstructionAbstract (|*|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum DIVIDE_ a1 a2 a3) =  do runInstructionAbstract (|/|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum MOD_ a1 a2 a3) =  do runInstructionAbstract (|%|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum POWER_ a1 a2 a3) =  do runInstructionAbstract (|^|) a1 a2 a3 
runInstruction (QuadrupleTwoAddresses quadNum ASSIGNMENT a1 a2) =  do doAssignment a1 a2 
runInstruction (QuadrupleOneAddress quadNum READ a1) 
                                    | a1 >= startIntGlobalMemory && a1 <= endIntGlobalMemory     
                                      || a1 >= startIntLocalMemory && a1 <= endIntLocalMemory =
                                                                do 
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ putStrLn $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: Integer" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    -- lift $ catch (seq (read x :: Integer) $ return()) showError
                                                                    case (checkInt x) of 
                                                                        Just int -> insertValueInAddress (VMInteger int) a1
                                                                        Nothing -> do
                                                                            liftIO $ putStrLn $ color Red . style Bold $ ("Runtime Recovery: Please enter an Integer number")
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
                                                                        Just dec -> insertValueInAddress (VMDecimal dec) a1
                                                                        Nothing -> do
                                                                            liftIO $ putStrLn $ color Red . style Bold $ ("Runtime Recovery: Please enter a Decimal number")
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
                                                                    liftIO $ hClose tty
                                    | a1 >= startBoolGlobalMemory && a1 <= endBoolGlobalMemory     
                                      || a1 >= startBoolLocalMemory && a1 <= endBoolLocalMemory =
                                                                do 
                                                                    tty <- liftIO $ openFile "/dev/tty" ReadMode
                                                                    liftIO $ putStrLn $ (style SlowBlink $ "<") ++ (style Bold $ "Expected type: True | False" ) ++ (style SlowBlink $ ">")
                                                                    x  <- liftIO $ hGetLine tty
                                                                    -- lift $ catch (seq (read x :: Integer) $ return()) showError
                                                                    case (checkBool x) of 
                                                                        Just bool -> insertValueInAddress (VMBool bool) a1
                                                                        Nothing -> do
                                                                            liftIO $ putStrLn $ color Red . style Bold $ ("Runtime Recovery: Please enter a Bool")
                                                                            return()
                                                                    liftIO $ hClose tty
                                    | otherwise = return ()       
                                            
                                            

runInstruction (QuadrupleOneAddress quadNum DISPLAY a1) = do 
                                        cpuState <- get
                                        let (_,currentIP,globalMemory,localMemory) = getCPUState cpuState
                                        let memories = (Map.union globalMemory localMemory) 
                                        case (Map.lookup a1 memories) of 
                                            Just (VMString val) -> do 
                                                    modify $ \s -> (cpuState { ip = (ip cpuState) + 1 })
                                                    liftIO $ putStrLn $ (style Underline $ val) 
                                            Just val -> do 
                                                    modify $ \s -> (cpuState { ip = (ip cpuState) + 1 })
                                                    liftIO $ putStrLn $ show $ val
                                                    -- tell $ [show val]
                                            _ -> do 
                                                    modify $ \s -> (cpuState { panic = True })
                                                    tell $ ["Address " ++ show a1  ++  " was not found in any memory"]
                                        return ()

runInstruction _ =  return ()

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

--MARK TODO: Los demás operadores

showValue :: VMValue -> String
showValue (VMInteger int)  = id $ show int
showValue (VMDecimal dec) = id $ show dec
showValue (VMString str) = id str
showValue (VMBool bool) = id $ show bool
showValue (VMEmpty) = id "~~~"

doAssignment :: Address -> Address -> VM
doAssignment a1 a2 = do 
                        cpuState <- get
                        let (panic,currentIP,globalMemory,localMemory) = getCPUState cpuState
                        let memories = (Map.union globalMemory localMemory)
                        case (Map.lookup a1 memories) of
                            Just val -> insertValueInAddress val a2
                            _ -> do 
                                    modify $ \s -> (cpuState { panic = True })
                                    tell $ ["Address " ++ show a1  ++  " was not found in any memory"]
                                    return ()

                                     

runInstructionAbstract :: (VMValue -> VMValue -> VMValue) -> Address -> Address -> Address -> VM
runInstructionAbstract f a1 a2 a3 = do 
                                        cpuState <- get
                                        let (_,_,globalMemory,localMemory) = getCPUState cpuState
                                            memories = (Map.union globalMemory localMemory) 
                                            valResult = doArithmeticOperation f a1 a2 memories 
                                        insertValueInAddress valResult a3

doArithmeticOperation :: (VMValue -> VMValue -> VMValue) -> Address -> Address -> Memory -> VMValue
doArithmeticOperation f a1 a2 memory = case (Map.lookup a1 memory) of
                                            Just vmVal1 -> case (Map.lookup a2 memory) of
                                                                Just vmVal2 -> f vmVal1 vmVal2
                                                                

insertValueInAddress :: VMValue -> Address -> VM
insertValueInAddress val address = do
                            cpuState <- get
                            let (_,currentIP,globalMemory,localMemory) = getCPUState cpuState 
                            if (address >= startIntGlobalMemory && address <= endBoolGlobalMemory) 
                                then do
                                    let newGlobalMemory = (Map.insert address val globalMemory)
                                    let s' = (cpuState { globalMemory = newGlobalMemory, ip = currentIP + 1 })
                                    modify $ \s -> (cpuState { globalMemory = newGlobalMemory, ip = currentIP + 1 })
                                    -- modify $ \s -> 

                            else do
                                if (address >= startIntLocalMemory && address <= endBoolLocalMemory) then do
                                    let newLocalMemory = (Map.insert address val localMemory)
                                    modify $ \s -> (cpuState { localMemory = newLocalMemory, ip = currentIP + 1 })
                                else do
                                    modify $ \s -> (cpuState { panic = True})
                                    tell $ [("Address " ++ show address  ++  " assignment underflow/overflow ")]
                            return ()

