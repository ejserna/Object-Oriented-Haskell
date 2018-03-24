module VirtualMachine where 
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
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
type VM =  RWS [Quadruple] [String] CPUState 


startVM :: [Quadruple] -> Memory -> Memory -> IO ()
startVM quads globalMemory localMemory = 
    do start <- getCPUTime
       mapM_ (putStrLn) $ snd $ evalRWS runVM quads (setInitialCPUState globalMemory localMemory)
       end   <- getCPUTime
       let diff = (fromIntegral (end - start)) / (10^12)
       let msg1 = style Bold $ "Finished" ++ " in " ++ ( show (diff::Decimal) ) ++ " sec"
       putStrLn $ msg1 
       -- printf $ msg1 ++ " in: %0.9f sec\n" (diff :: Double)

setInitialCPUState :: Memory -> Memory -> CPUState
setInitialCPUState globalMem localMem = CPUState False 0 globalMem localMem

getCPUState :: CPUState -> (Bool,Int,Memory,Memory)
getCPUState (CPUState panic ip globalMemory localMemory) = (panic,ip,globalMemory,localMemory)

runVM :: VM ()
runVM = do
        quadruples <- ask
        cpuState <- get
        let (isPanicState,currentInstructionPointer,_,_) = getCPUState cpuState
        -- modify $ \state -> (CPUState {panic = False , ip = (ip estado) + 1})
        -- estado2 <- get
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

-- main = print $ evalRWS go [1,2,3,4,5,6,7] (CPUState {panic = False, ip = 1})

runInstruction :: Quadruple -> VM ()
-- Si es un NOP, solamente aumentamos a uno el instruction pointer
runInstruction (QuadrupleEmpty _ _) = do 
                                        cpuState <- get
                                        let (_,currentIP,_,_) = getCPUState cpuState
                                        modify $ \s -> (cpuState { ip = (ip cpuState) + 1 })
                                        return ()
runInstruction (QuadrupleThreeAddresses quadNum ADD_ a1 a2 a3) = do runInstructionAbstract (|+|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum SUB_ a1 a2 a3) =  do runInstructionAbstract (|-|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum MULTIPLY_ a1 a2 a3) =  do runInstructionAbstract (|*|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum DIVIDE_ a1 a2 a3) =  do runInstructionAbstract (|/|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum MOD_ a1 a2 a3) =  do runInstructionAbstract (|%|) a1 a2 a3 
runInstruction (QuadrupleThreeAddresses quadNum POWER_ a1 a2 a3) =  do runInstructionAbstract (|^|) a1 a2 a3 
runInstruction (QuadrupleTwoAddresses quadNum ASSIGNMENT a1 a2) =  do doAssignment a1 a2 
runInstruction (QuadrupleOneAddress quadNum (DISPLAY) a1) = do 
                                        cpuState <- get
                                        let (_,currentIP,globalMemory,localMemory) = getCPUState cpuState
                                        let memories = (Map.union globalMemory localMemory) 
                                        case (Map.lookup a1 memories) of 
                                            Just val -> do 
                                                    modify $ \s -> (cpuState { ip = (ip cpuState) + 1 })
                                                    tell $ [show val]
                                            _ -> do 
                                                    modify $ \s -> (cpuState { panic = True })
                                                    tell $ ["Address " ++ show a1  ++  " was not found in any memory"]
                                        return ()

runInstruction _ =  return ()

--MARK TODO: Los demás operadores

showValue :: VMValue -> String
showValue (VMInteger int)  = id $ show int
showValue (VMDecimal dec) = id $ show dec
showValue (VMString str) = id str
showValue (VMBool bool) = id $ show bool
showValue (VMEmpty) = id "~~~"

doAssignment :: Address -> Address -> VM ()
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
                                     

runInstructionAbstract :: (VMValue -> VMValue -> VMValue) -> Address -> Address -> Address -> VM ()
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
                                                                

insertValueInAddress :: VMValue -> Address -> VM()
insertValueInAddress val address = do
                            cpuState <- get
                            let (_,currentIP,globalMemory,localMemory) = getCPUState cpuState 
                            if (address >= startIntGlobalMemory && address <= endBoolGlobalMemory) 
                                then do
                                    let newGlobalMemory = (Map.insert address val globalMemory)
                                    modify $ \s -> (cpuState { globalMemory = newGlobalMemory, ip = currentIP + 1 })
                            else do
                                if (address >= startIntLocalMemory && address <= endBoolLocalMemory) then do
                                    let newLocalMemory = (Map.insert address val localMemory)
                                    modify $ \s -> (cpuState { localMemory = newLocalMemory, ip = currentIP + 1 })
                                else do
                                    modify $ \s -> (cpuState { panic = True})
                                    tell $ [("Address " ++ show address  ++  " assignment underflow/overflow ")]
                            return ()

