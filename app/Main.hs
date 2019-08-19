{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Monad (ap,liftM)
import Control.Monad.State (State,execState,get,put)
import Data.Bits
import Data.List.Extra (upper)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set,(\\))
import Data.Word8 (Word8)
import Graphics.Gloss.Interface.IO.Game as Gloss
import Prelude hiding (pred)
import System.Environment (getArgs)
import System.Random (newStdGen,randomRs)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

maxHistory :: Int
maxHistory = 100

----------------------------------------------------------------------
-- parameters of the Chip Machine

delayTickHertz :: Int
delayTickHertz = 60 -- this is fixed in the chip8 spec

initialIPS :: Int -- instructions/second
initialIPS = 512 -- this can be varied dynamically

----------------------------------------------------------------------
-- parameter of the Gloss simulation

fps :: Int -- frames/second
fps = 50 -- this can be changed (but is fixed for the simulation)

----------------------------------------------------------------------
-- display parameters

theScale :: Int
theScale = 20

nonFullWindowPos :: (Int,Int)
nonFullWindowPos = (150,100)

----------------------------------------------------------------------

main :: IO ()
main = do
    args@Args{dump,file} <- parseCommandLine <$> getArgs
    progBytes <- readBytes file
    if dump
        then printCode progBytes
        else
        do
            rands <- randBytes
            runChip8 args rands progBytes

data Args = Args { full :: Bool, dump :: Bool, file :: FilePath }

parseCommandLine :: [String] -> Args -- very basic support!
parseCommandLine = \case
    [file] ->  Args { full = False, dump = False, file }
    [file,"--full"] ->  Args { full = True, dump = False, file }
    [file,"--dump"] ->  Args { full = False, dump = True, file }
    xs -> error $ show xs

runChip8 :: Args -> [Byte] -> [Byte] -> IO ()
runChip8 Args{file,full} rands progBytes = do
    Gloss.playIO dis black fps model0
        pictureModel
        (handleEventModel model0)
        updateModel
    where
        model0 = initModel (initCS rands progBytes)
        dis = if full then FullScreen else InWindow title size nonFullWindowPos
        size = (xmax * theScale + 2 + 2*extraX, ymax * theScale + 2 + 2*extraY)
        title = "Chip8: " <> file

----------------------------------------------------------------------
-- making pictures

pictureModel :: Model -> IO Picture
pictureModel Model{cs,ss} = do
    let ChipState{screen} = cs
    return $ myTranslate $ pictures
        [ border, oborder
        , myScale (pictureFromScreen screen)
        , picInternals ss cs
        ]

myTranslate :: Picture -> Picture
myTranslate =
    translate
    (- fromIntegral (theScale * xmax `div` 2))
    (- fromIntegral (theScale * ymax `div` 2))

border :: Picture
border = color red $ square (0,0) (x,y)
    where (x,y) = (fromIntegral (theScale * xmax), fromIntegral (theScale * ymax))

oborder :: Picture
oborder = color red $ trans$ square (0,0) (x,y)
    where (x,y) = (fromIntegral (theScale * xmax + 2 * extraX)
                  , fromIntegral (theScale * ymax + 2 * extraY))
          trans = translate (fromIntegral (-extraX)) (fromIntegral (-extraY))

myScale :: Picture -> Picture
myScale = scale (fromIntegral theScale) (fromIntegral theScale)

pictureFromScreen :: Screen -> Picture
pictureFromScreen Screen{on} = Gloss.pictures $ do
    y <- [0..ymax-1]
    x <- [0..xmax-1]
    return $ if (x,y) `Set.member` on
        then pixel (fromIntegral x, fromIntegral (ymax - y - 1))
        else Gloss.blank

pixel :: Point -> Picture
pixel (x,y) = color white $ polygon [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]

square :: Point -> Point -> Picture
square (x1,y1) (x2,y2) = line [(x1,y1),(x1,y2),(x2,y2),(x2,y1),(x1,y1)]

extraX :: Int
extraX = 10 * theScale

extraY :: Int
extraY = 5 * theScale

picInternals :: SimState -> ChipState -> Picture
picInternals SimState{ips,mode,tracing} ChipState{nExec,mem,regs,delay,regI,pc} =
    if tracing then rescale $ translate (-150) (145) $ scale 0.2 0.2 $ pic else blank
    where
        rescale = scale (fromIntegral theScale / 20.0) (fromIntegral theScale / 20.0)

        pic = picIPS <> picCount <> picMode
            <> pictures (map picReg [0..15])
            <> picD <> picI <> picPC <> picInstr <> picOp

        picIPS = onLine (-3) $ labBoxText "ips" (show derivedIPS) 320
            where derivedIPS = case mode of
                      Running ->  ips
                      Stopped ->  0
                      Stepping -> fps
                      Step1 ->    0
                      Backing ->  (- fps)
                      Back1 ->    0

        picMode = translate 600 0 $ onLine (-3) $ labBoxText "" (upper $ show mode) 600

        picCount = translate 2000 0 $ onLine (-3) $ labBoxText "#i" (show nExec) 1200

        picReg :: Int -> Picture
        picReg n = onLine n $ labBoxText s1 s2 170
            where
                nib = nibOfInt n
                reg = Reg nib
                value = fromMaybe byte0 $ Map.lookup reg regs
                s1 = show reg
                s2 = show value

        picD = onLine 17 $ labBoxText "D" (show delay) 170
        picI = onLine 18 $ labBoxText "I" (show regI) 380
        picPC = onLine 19 $ labBoxText "PC" (show pc) 380

        picInstr = onLine 22 $ labBoxText "i" (show instr) 320
        picOp = translate 600 0 $ onLine 22 $ labBoxText "op" (show op) 1200

        instr = instructionLookup pc mem
        op = decode instr

        onLine :: Int -> Picture -> Picture
        onLine n = translate 0 (150 * (15 - fromIntegral n))

labBoxText :: String -> String -> Float -> Picture
labBoxText s1 s2 w = pictures
    [ translate (-160) 0 $ scale 0.7 0.7 $ color red $ text s1
    , boxText s2 15.0 (100.0,w)
    ]

boxText :: String -> Float -> (Float,Float) -> Picture
boxText mes x (h,w) = pictures
    [ color cyan $ Text mes,
      color red $ square (-x,-x) (w + x, h + x)
    ]

----------------------------------------------------------------------
-- read, dissassemble

readBytes :: FilePath -> IO [Byte]
readBytes path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    let xs :: [Byte] = map (byteOfInt . fromIntegral) ws
    return xs

printCode :: [Byte] -> IO ()
printCode progBytes = do
    let instructions = pairUpBytes progBytes
    putStrLn $ showCode baseProgram instructions
    putStrLn "--------------------"
    putStrLn $ showDisassemble baseProgram instructions

showCode :: Addr -> [Instruction] -> String
showCode a is = showCodeLine a xs <> rest
    where (xs,ys) = splitAt size is; size = 8
          rest = if null is then "" else "\n" <> showCode (incAddr a size) ys

showCodeLine :: Addr -> [Instruction] -> String
showCodeLine a bs = show a <> " : " <> List.intercalate " " (map show bs)

showDisassemble :: Addr -> [Instruction] -> String
showDisassemble a0 instructions =
    unlines $ map (showLocatedOp interesting) $ zip addrs ops
    where
        addrs = map (incAddr a0) $ map (2*) [0..]
        ops = map decode instructions
        interesting a = a `Set.member` allAddrs
        allAddrs = Set.fromList (ops >>= opAddresses)

showLocatedOp :: (Addr -> Bool) -> (Addr,Op) -> String
showLocatedOp pred (a,op) = (if pred a then show a <> " : " else replicate 8 ' ') <> show op

----------------------------------------------------------------------
-- events

type Keys = Set Char

initKeys :: Keys
initKeys = Set.empty

handleEventModel :: Model -> Event -> Model -> IO Model
handleEventModel model0 event model@Model{keys,ss=ss@SimState{tracing,ips}} = do
    return $ case event of
        EventKey (SpecialKey KeyInsert) Down _ _ -> model { ss = doFlipTrace }
        EventKey (SpecialKey KeyUp) Down _ _ -> model { ss = doFaster }
        EventKey (SpecialKey KeyDown) Down _ _ -> model { ss = doSlower }
        EventKey (SpecialKey KeyTab) Down _ _ -> model { ss = doRun }
        EventKey (SpecialKey KeyF5) Down _ _ -> model { ss = doRunNoTrace }
        EventKey (SpecialKey KeyF6) Down _ _ -> model { ss = doRunTrace }
        EventKey (SpecialKey KeyEnter) Down _ _ -> model { ss = doOneStep }
        EventKey (Char '\b') Down _ _ -> model { ss = doBackStep } -- Backspace
        EventKey (SpecialKey KeyShiftR) Down _ _ -> model { ss = doStepContinuous }
        EventKey (SpecialKey KeyShiftL) Down _ _ -> model { ss = doBackContinuous }
        EventKey (SpecialKey KeyShiftR) Up _ _ -> model { ss = doStop }
        EventKey (SpecialKey KeyShiftL) Up _ _ -> model { ss = doStop }
        EventKey (Char char) Down _ _ -> model { keys = Set.insert char keys }
        EventKey (Char char) Up _ _ -> model { keys = Set.delete char keys }
        EventKey (SpecialKey KeyEsc) Down _ _ -> model { ss = doQuit }
        EventKey (SpecialKey KeyDelete) Down _ _ -> doReset
        _ -> model
    where
        doFlipTrace = ss { tracing = not tracing }
        doFaster = ss { ips = 2 * ips }
        doSlower = ss { ips = max 1 (ips `div` 2) }
        doRun = ss { mode = Running }
        doRunNoTrace = ss { mode = Running, tracing = False}
        doRunTrace = ss { mode = Running, tracing = True}
        doOneStep = ss { mode = Step1, tracing = True}
        doBackStep = ss { mode = Back1, tracing = True}
        doStepContinuous = ss { mode = Stepping }
        doBackContinuous = ss { mode = Backing }
        doStop = ss { mode = Stopped }
        doQuit = error "quit"
        doReset = model0
            { cs = (cs model0) { rands = rands $ cs model } -- dont reset rands
            , ss -- dont reset sim-state
            }

chipKeys :: Keys -> ChipKeys
chipKeys keys nib = mapKey nib `elem` keys

mapKey :: Nib -> Char
mapKey = \case
    N1 -> '1'; N2 -> '2'; N3 -> '3'; NC -> '4';
    N4 -> 'q'; N5 -> 'w'; N6 -> 'e'; ND -> 'r';
    N7 -> 'a'; N8 -> 's'; N9 -> 'd'; NE -> 'f';
    NA -> 'z'; N0 -> 'x'; NB -> 'c'; NF -> 'v';

----------------------------------------------------------------------
-- simulating Chip8

initModel :: ChipState -> Model
initModel cs = do
    let keys = initKeys
    let ss = initSS
    let csHistory = []
    let frame = 0
    Model{..}

updateModel :: Float -> Model -> IO Model
updateModel _delta model0 = do
    let model = incFrameCount model0
    let Model{ss} = model
    let SimState{mode} = ss
    return $ case mode of
        Running ->      stepRunning model
        Stopped ->      model
        Stepping ->     stepForward model
        Step1 ->        stepForward (stop model)
        Backing ->      stepBack model
        Back1 ->        stepBack (stop model)

incFrameCount :: Model -> Model
incFrameCount model = do model { frame = frame model + 1 }

stepRunning :: Model -> Model
stepRunning model = stepForwardN (calcRunSteps model) model

calcRunSteps :: Model -> Int
calcRunSteps model = do
    let Model{frame,ss} = model
    let SimState{ips} = ss
    fractionalModSeries frame (ips,fps)

stepForwardN :: Int -> Model -> Model
stepForwardN n model = nTimes n stepForward model

stepForward :: Model -> Model
stepForward model = do
    let cs2 = stepModel model
    if noProgress model cs2
        then if crashed cs2 then stopAndTrace model else model
        else commitStep model cs2

stop :: Model -> Model
stop model@ Model{ss} = model { ss = ss { mode = Stopped } }

noProgress :: Model -> ChipState -> Bool
noProgress model cs2 = do
    let Model{cs=cs1} = model
    let ChipState{nExec=n1} = cs1
    let ChipState{nExec=n2} = cs2
    n1 == n2

stopAndTrace :: Model -> Model
stopAndTrace model@Model{ss} = model { ss = ss { tracing = True, mode = Stopped } }

commitStep :: Model -> ChipState -> Model
commitStep model@Model{cs,csHistory} csNew =
    model { cs = csNew, csHistory = take maxHistory (cs : csHistory) }

stepBack :: Model -> Model
stepBack model =
    case csHistory model of
        [] -> model
        cs:csHistory -> model { cs, csHistory }

stepModel :: Model -> ChipState
stepModel model = do
    let Model{keys,cs,ss} = model
    let SimState{ips} = ss
    let ChipState{nExec} = cs
    let ck = chipKeys keys
    let cs1 = execState (runAction ck $ fetchDecodeExec) $ cs
    let nDelayTicks = fractionalModSeries nExec (delayTickHertz,ips)
    let cs2 = tickN nDelayTicks cs1
    cs2

fetchDecodeExec :: Action ()
fetchDecodeExec = do
    pc <- PC
    SetPC (nextInstr pc)
    Fetch pc >>= (exec . decode)

tickN :: Int -> ChipState -> ChipState
tickN n cs = cs { delay = tickDelayN n (delay cs) }

fractionalModSeries :: Int -> (Int,Int) -> Int -- TODO: a comment on this would be nice
fractionalModSeries n (a,b) =
    (a `div` b) + (if (x * n) `mod` b < x then 1 else 0)
    where x = a `mod` b

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldl (.) id $ replicate n f

----------------------------------------------------------------------
-- Model

data Model = Model
    { keys :: Keys
    , cs :: ChipState
    , ss :: SimState -- TODO: inline SimState into Model
    , csHistory :: [ChipState]
    , frame :: Int
    }

----------------------------------------------------------------------
-- SimState

data SimMode = Running | Stopped | Stepping | Step1 | Backing | Back1
    deriving (Eq,Show)

data SimState = SimState
    { mode :: SimMode
    , ips :: Int
    , tracing :: Bool
    }
    deriving (Show)

initSS :: SimState
initSS = SimState
    { mode = Running
    , ips = initialIPS
    , tracing = False
    }

----------------------------------------------------------------------
-- Instruction

pairUpBytes :: [Byte] -> [Instruction]
pairUpBytes = \case
    b1:b2:rest -> Instruction b1 b2 : pairUpBytes rest
    [] -> []
    [_] -> [] -- error "stray byte"

data Instruction = Instruction Byte Byte -- 16 bits, 2 bytes, 4 nibbles
instance Show Instruction where show (Instruction b1 b2) = show b1 <> show b2

----------------------------------------------------------------------
-- decode

decode :: Instruction -> Op
decode (Instruction b1 b2) =
    decodeMatch (n1,n2,n3,n4) where (n1,n2) = nibsOfByte b1; (n3,n4) = nibsOfByte b2

decodeMatch :: (Nib,Nib,Nib,Nib) -> Op
decodeMatch tup = case tup of
    (N0,N0,NE,N0) -> OpCls
    (N0,N0,NE,NE) -> OpReturn
    (N1,a,b,c) -> OpJump (addr a b c)
    (N2,a,b,c) -> OpCall (addr a b c)
    (N3,x,n,m) -> OpSkipEqLit (Reg x) (byte n m)
    (N4,x,n,m) -> OpSkipNotEqLit (Reg x) (byte n m)
    (N5,x,y,N0) -> OpSkipEq (Reg x) (Reg y)
    (N6,x,n,m) -> OpStoreLit (Reg x) (byte n m)
    (N7,x,n,m) -> OpAddLit (Reg x) (byte n m)
    (N8,x,y,N0) -> OpStore (Reg x) (Reg y)
    (N8,x,y,N1) -> OpOr (Reg x) (Reg y)
    (N8,x,y,N2) -> OpAnd (Reg x) (Reg y)
    (N8,x,y,N3) -> OpXor (Reg x) (Reg y)
    (N8,x,y,N4) -> OpAdd (Reg x) (Reg y)
    (N8,x,y,N5) -> OpSub (Reg x) (Reg y)
    (N8,x,y,N6) -> OpShiftR (Reg x) (Reg y)
    (N8,x,y,N7) -> OpMinus (Reg x) (Reg y)
    (N8,x,y,NE) -> OpShiftL (Reg x) (Reg y)
    (N9,x,y,N0) -> OpSkipNotEq (Reg x) (Reg y)
    (NA,a,b,c) -> OpStoreI (Addr a b c)
    (NB,a,b,c) -> OpJumpOffset (Addr a b c)
    (NC,x,n,m) -> OpRandom (Reg x) (Byte n m)
    (ND,x,y,n) -> OpDraw (Reg x) (Reg y) n
    (NE,x,N9,NE) -> OpSkipKey (Reg x)
    (NE,x,NA,N1) -> OpSkipNotKey (Reg x)
    (NF,x,N0,N7) -> OpReadDelay (Reg x)
    (NF,x,N0,NA) -> OpWaitKeyPress (Reg x)
    (NF,x,N1,N5) -> OpSetDelay (Reg x)
    (NF,x,N1,N8) -> OpSetSound (Reg x)
    (NF,x,N1,NE) -> OpIncreaseI (Reg x)
    (NF,x,N2,N9) -> OpStoreDigitSpriteI (Reg x)
    (NF,x,N3,N3) -> OpStoreBCD (Reg x)
    (NF,x,N5,N5) -> OpSaveRegs (Reg x)
    (NF,x,N6,N5) -> OpRestoreRegs (Reg x)
    _ -> unknown
    where
        unknown = OpUnknown (Instruction (Byte a b) (Byte c d)) where (a,b,c,d) = tup

----------------------------------------------------------------------
-- Op

data Op
    = OpUnknown Instruction
    | OpCls
    | OpReturn
    | OpJump Addr
    | OpJumpOffset Addr
    | OpCall Addr
    | OpSkipEqLit Reg Byte
    | OpSkipNotEqLit Reg Byte
    | OpSkipEq Reg Reg
    | OpSkipNotEq Reg Reg
    | OpStoreLit Reg Byte
    | OpAddLit Reg Byte
    | OpStore Reg Reg
    | OpStoreI Addr
    | OpDraw Reg Reg Nib
    | OpSetDelay Reg
    | OpReadDelay Reg
    | OpRandom Reg Byte
    | OpSkipKey Reg
    | OpSkipNotKey Reg
    | OpAnd Reg Reg
    | OpOr Reg Reg
    | OpXor Reg Reg
    | OpShiftL Reg Reg
    | OpShiftR Reg Reg
    | OpAdd Reg Reg
    | OpSub Reg Reg
    | OpMinus Reg Reg
    | OpSetSound Reg
    | OpStoreBCD Reg
    | OpSaveRegs Reg
    | OpRestoreRegs Reg
    | OpStoreDigitSpriteI Reg
    | OpIncreaseI Reg
    | OpWaitKeyPress Reg

opAddresses :: Op -> [Addr]
opAddresses = \case
    OpJump a -> [a]
    OpJumpOffset a -> [a]
    OpCall a -> [a]
    OpStoreI a -> [a]
    _ -> []

instance Show Op where
    show = \case
        OpUnknown _i            -> "???"
        OpCls                   -> "CLS"
        OpReturn                -> "RET"
        OpJump a                -> una "JMP" a
        OpJumpOffset a          -> bin "JMP" (Reg N0) a
        OpCall a                -> una "CALL" a
        OpSkipEqLit x b         -> skip (eq x b)
        OpSkipNotEqLit x b      -> skip (neq x b)
        OpSkipEq x y            -> skip (eq x y)
        OpSkipNotEq x y         -> skip (neq x y)
        OpStoreLit x b          -> set x b
        OpAddLit x b            -> bin "ADD" x b
        OpStore x y             -> set x y
        OpStoreI a              -> set I a
        OpDraw x y n            -> tri "DRAW" x y n
        OpSetDelay x            -> set D x
        OpReadDelay x           -> set x D
        OpRandom x b            -> bin "RAND" x b
        OpSkipKey x             -> skip ("PRESS " <> show x)
        OpSkipNotKey x          -> skip ("!PRESS " <> show x)
        OpAnd x y               -> bin "AND" x y
        OpOr  x y               -> bin "OR" x y
        OpXor x y               -> bin "XOR" x y
        OpAdd x y               -> bin "ADD" x y
        OpSub x y               -> bin "SUB" x y
        OpMinus x y             -> bin "MINUS" x y
        OpShiftL x y            -> bin "SHL" x y
        OpShiftR x y            -> bin "SHR" x y
        OpSetSound x            -> una "SOUND" x
        OpStoreBCD x            -> una "BCD" x
        OpSaveRegs x            -> una "SAVE" x
        OpRestoreRegs x         -> una "RESTORE" x
        OpStoreDigitSpriteI x   -> una "SPRITE" x
        OpIncreaseI x           -> bin "ADD" I x
        OpWaitKeyPress x        -> una "WAIT" x
     where
        una tag a = tag <> " " <> show a
        skip p = "SKP " <> p
        eq a b = show a <> " = " <> show b
        neq a b = show a <> " != " <> show b
        set a b = "SET " <> show a <> " <- " <> show b
        bin tag a b = tag <> " " <> show a <> " " <> show b
        tri tag a b c = tag <> " " <> show a <> " " <> show b <> " " <> show c

data I = I deriving (Show)
data D = D deriving (Show)

----------------------------------------------------------------------
-- exec

exec :: Op -> Action ()
exec i = case i of
    OpUnknown _ -> do Crash; Stall
    OpCls -> Cls
    OpReturn -> PopStack >>= SetPC
    OpJump a -> SetPC a
    OpJumpOffset a -> Read (Reg N0) >>= (SetPC . incAddr a . byteToInt)
    OpCall a -> do PC >>= PushStack; SetPC a
    OpSkipEqLit x b -> do a <- Read x; skipInstructionIf (eqByte a b)
    OpSkipNotEqLit x b -> do a <- Read x; skipInstructionIf (not $ eqByte a b)
    OpSkipEq x y -> do a <- Read x; b <- Read y; skipInstructionIf (eqByte a b)
    OpSkipNotEq x y -> do a <- Read x; b <- Read y; skipInstructionIf (not $ eqByte a b)
    OpStoreLit x b -> Write x b
    OpAddLit x b -> Read x >>= Write x . addByte b
    OpStore x y -> Read y >>= Write x
    OpStoreI a -> StoreI a

    OpDraw x y n -> do
        bx <- Read x
        by <- Read y
        let xy = (byteToInt bx, byteToInt by)
        a <- ReadI
        let as = map addrOfInt [addrToInt a .. addrToInt a + nibToInt n - 1]
        bytes <- mapM ReadMem as
        collide <- Draw xy bytes
        setFlag collide

    OpSetDelay x -> Read x >>= SetDelay
    OpReadDelay x -> ReadDelay >>= Write x
    OpRandom x b -> andByte b <$> RandomByte >>= Write x
    OpSkipKey x -> Read x >>= (KeyStatus . lowNib) >>= skipInstructionIf
    OpSkipNotKey x -> Read x >>= (KeyStatus . lowNib) >>= (skipInstructionIf . not)
    OpAnd x y -> (andByte <$> Read x <*> Read y) >>= Write x
    OpOr  x y -> (orByte <$> Read x <*> Read y) >>= Write x
    OpXor x y -> (xorByte <$> Read x <*> Read y) >>= Write x

    OpAdd x y -> do
        (v,carry) <- addByteCarry <$> Read x <*> Read y
        Write x v
        setFlag carry

    OpSub x y -> do
        (v,borrow) <- subByteBorrow <$> Read x <*> Read y
        Write x v
        setFlag (not borrow)

    OpMinus x y -> do -- (Sub with args reversed)
        (v,borrow) <- subByteBorrow <$> Read y <*> Read x
        Write x v
        setFlag (not borrow)

    OpShiftL x _y -> do
        (shifted,overflow) <- byteShiftL <$> Read x -- ignoring y
        Write x shifted
        setFlag overflow

    OpShiftR x _y -> do
        (shifted,overflow) <- byteShiftR <$> Read x -- ignoring y
        Write x shifted
        setFlag overflow

    OpSetSound x -> Read x >>= SetSound

    OpStoreBCD x -> do
        a <- ReadI
        b <- Read x
        let (h,t,u) = bcd b
        WriteMem a h
        WriteMem (incAddr a 1) t
        WriteMem (incAddr a 2) u

    OpSaveRegs x -> do
        a <- ReadI
        let n = nibToInt (unReg x)
        let rs = map (Reg . nibOfInt) [0..n]
        mapM_ (\r -> Read r >>= WriteMem (incAddr a $ nibToInt $ unReg r)) rs
        --StoreI (incAddr a (n+1)) --???
        return ()

    OpRestoreRegs x -> do
        a <- ReadI
        let n = nibToInt (unReg x)
        let rs = map (Reg . nibOfInt) [0..n]
        mapM_ (\r -> ReadMem (incAddr a $ nibToInt $ unReg r) >>= Write r) rs
        --StoreI (incAddr a (n+1)) --???
        return ()

    OpStoreDigitSpriteI x ->
        Read x >>= (StoreI . hexDigitSpriteAddr . lowNib)

    OpIncreaseI x -> do
        a <- ReadI
        b <- Read x
        StoreI (incAddr a $ byteToInt b)

    OpWaitKeyPress x -> -- TODO: first, should wait for keys to be released
        AnyKey >>= \case Nothing -> Stall
                         Just nib -> Write x (byte N0 nib)

skipInstructionIf :: Bool -> Action ()
skipInstructionIf cond = if cond then (PC >>= SetPC . nextInstr) else return ()

setFlag :: Bool -> Action ()
setFlag cond = Write (Reg NF) (if cond then byte1 else byte0)

----------------------------------------------------------------------
-- Action

data Action a where
    Ret :: a -> Action a
    Bind :: Action a -> (a -> Action b) -> Action b
    Read :: Reg -> Action Byte
    Write :: Reg -> Byte -> Action ()
    SetSound :: Byte -> Action ()
    SetDelay :: Byte -> Action ()
    ReadDelay :: Action Byte
    Fetch :: Addr -> Action Instruction
    PC :: Action Addr
    SetPC :: Addr -> Action ()
    PushStack :: Addr -> Action ()
    PopStack :: Action Addr
    KeyStatus :: Nib -> Action Bool
    AnyKey :: Action (Maybe Nib)
    RandomByte :: Action Byte
    StoreI :: Addr -> Action ()
    ReadI :: Action Addr
    ReadMem :: Addr -> Action Byte
    WriteMem :: Addr -> Byte -> Action ()
    Draw :: XY -> [Byte] -> Action Bool
    Cls :: Action ()
    Crash :: Action ()
    Stall :: Action ()

instance Functor Action where fmap = liftM
instance Applicative Action where pure = return; (<*>) = ap
instance Monad Action where return = Ret; (>>=) = Bind

----------------------------------------------------------------------
-- runAction

type ChipKeys = Nib -> Bool

runAction :: ChipKeys -> Action a -> State ChipState a
runAction keys steps = do
    cs <- get
    let ChipState{mem,regs,delay,pc,stack,regI,screen,nExec,rands} = cs
    case steps of
        Ret a -> return a
        Bind left g -> runAction keys left >>= runAction keys . g
        Read x -> return $ fromMaybe byte0 $ Map.lookup x regs
        Write x b -> put $ cs { regs = Map.insert x b regs }
        SetSound _ -> return () -- No sound :(
        SetDelay b -> put $ cs { delay = b }
        ReadDelay -> return delay
        Fetch a -> do put $ cs { nExec = nExec + 1 }; return $ instructionLookup a mem
        PC -> return pc
        SetPC a -> put $ cs { pc = a }
        PushStack a -> put $ cs { stack = a : stack }
        PopStack -> do put $ cs { stack = tail stack }; return $ head stack
        KeyStatus n -> return $ keys n
        AnyKey -> return $ checkAnyKey keys
        RandomByte -> do put $ cs { rands = tail rands }; return $ head rands
        StoreI a -> put $ cs { regI = a }
        ReadI -> return regI
        ReadMem a -> return $ fromMaybe byte0 $ Map.lookup a mem
        WriteMem a b -> put $ cs { mem = Map.insert a b mem }
        Draw xy bytes -> do
            let (screen',collide) = drawSprite screen xy bytes
            put $ cs { screen = screen' }
            return collide
        Cls -> put $ cs { screen = emptyScreen }
        Crash -> put $ cs { crashed = True }
        Stall -> put $ cs { pc = backupInstr pc, nExec = nExec - 1 }

checkAnyKey :: ChipKeys -> Maybe Nib
checkAnyKey keys = do
    let look n = (n,keys n)
    let xs = map fst $ filter snd $ map (look . nibOfInt) [0..15]
    case xs of [] -> Nothing; nib:_ -> Just nib

drawSprite :: Screen -> XY -> [Byte] -> (Screen,Bool)
drawSprite screen (x0,y0) bytes = do
    let xys = do
            (y,by) <- zip [y0..] bytes
            (x,b) <- zip [x0..] $ bitsOfByte by
            if b then [(x,y)] else []
    flipPixels screen $ Set.fromList xys

flipPixels :: Screen -> Set XY -> (Screen,Bool)
flipPixels Screen{on=before} sprite = do
    let stayOn = before \\ sprite
    let comeOn = sprite \\ before
    let after = stayOn `Set.union` comeOn
    let flipped = before `Set.intersection` sprite
    let collide = not $ Set.null flipped
    (Screen{on=after},collide)

----------------------------------------------------------------------
-- ChipState

initCS :: [Byte] -> [Byte] -> ChipState
initCS rands progBytes = do
    let mem = initMem progBytes
    let regs = Map.empty
    let delay = byte0
    let pc = baseProgram
    let stack = []
    let regI = addrOfInt 0
    let screen = emptyScreen
    let nExec = 0
    let crashed = False
    ChipState{..}

data ChipState = ChipState
    { mem :: Mem
    , regs :: Regs
    , delay :: Byte
    , pc :: Addr
    , stack :: [Addr]
    , regI :: Addr
    , screen :: Screen
    , nExec :: Int -- number of intructions executed so far
    , rands :: [Byte]
    , crashed :: Bool
    }

_showChipStateLine :: ChipState -> String
_showChipStateLine ChipState{delay,regI,regs,mem,pc,stack,nExec} = do
    let instr = instructionLookup pc mem
    let op = decode instr
    unwords (["#" <> show nExec, show delay, show regI, "--"]
             <> map show (regVals regs)
             <> ["--", "(" <> show (length stack) <> ")",
                 show pc, ":", show op])

regVals :: Regs -> [Byte]
regVals regs = map (\i -> fromMaybe byte0 $ Map.lookup (Reg (nibOfInt i)) regs) [0..15]

----------------------------------------------------------------------
-- Regs

type Regs = Map Reg Byte

newtype Reg = Reg { unReg :: Nib } deriving (Eq,Ord)
instance Show Reg where show r = "v" <> show (unReg r)

----------------------------------------------------------------------
-- Mem

type Mem = Map Addr Byte

instructionLookup :: Addr -> Mem -> Instruction
instructionLookup a mem  = Instruction b1 b2
    where b1 = fromMaybe byte0 $ Map.lookup a mem
          b2 = fromMaybe byte0 $ Map.lookup (incAddr a 1) mem

initMem :: [Byte] -> Mem
initMem bytes = Map.fromList $ do
    (i,b) <- zip [addrToInt baseProgram..] bytes
        ++ zip [addrToInt baseHexSpriteData..] (map byteOfInt digitData)
    return $ (addrOfInt i, b)

hexDigitSpriteAddr :: Nib -> Addr
hexDigitSpriteAddr n = incAddr baseHexSpriteData (5 * nibToInt n)

digitData :: [Int]
digitData = [
    0xF0,0x90,0x90,0x90,0xF0,
    0x20,0x60,0x20,0x20,0x70,
    0xF0,0x10,0xF0,0x80,0xF0,
    0xF0,0x10,0xF0,0x10,0xF0,
    0x90,0x90,0xF0,0x10,0x10,
    0xF0,0x80,0xF0,0x10,0xF0,
    0xF0,0x80,0xF0,0x90,0xF0,
    0xF0,0x10,0x20,0x40,0x40,
    0xF0,0x90,0xF0,0x90,0xF0,
    0xF0,0x90,0xF0,0x10,0xF0,
    0xF0,0x90,0xF0,0x90,0x90,
    0xE0,0x90,0xE0,0x90,0xE0,
    0xF0,0x80,0x80,0x80,0xF0,
    0xE0,0x90,0x90,0x90,0xE0,
    0xF0,0x80,0xF0,0x80,0xF0,
    0xF0,0x80,0xF0,0x80,0x80
    ]

----------------------------------------------------------------------
-- Screen

type XY = (Int,Int)
newtype Screen = Screen { on :: Set XY }

instance Show Screen where
    show Screen{on} = unlines $ do
        y <- [0..ymax-1]
        return $ do
            x <- [0..xmax-1]
            if (x,y) `Set.member` on then "X" else "."

emptyScreen :: Screen
emptyScreen = Screen Set.empty

xmax,ymax :: Int
(xmax,ymax) = (64,32) -- fixed size for standard Chip8

----------------------------------------------------------------------
-- Addr

baseProgram :: Addr
baseProgram = addrOfInt 0x200

baseHexSpriteData :: Addr
baseHexSpriteData = addrOfInt 0x0

data Addr = Addr Nib Nib Nib -- 12 bits, 3 nibble
    deriving (Eq,Ord)

instance Show Addr where show a = printf "0x%03X" (addrToInt a)

addr :: Nib -> Nib -> Nib -> Addr
addr = Addr -- TODO: inline if keep this rep

nextInstr :: Addr -> Addr
nextInstr a = incAddr a 2

backupInstr :: Addr -> Addr
backupInstr a = incAddr a (-2)

incAddr :: Addr -> Int -> Addr
incAddr a i = addrOfInt (addrToInt a + i)

addrOfInt :: Int -> Addr
addrOfInt i = if bad then error "addrOfInt" else a
    where a = Addr n1 n2 n3
          bad = i < 0 || shiftR i 12 > 0
          n1 = nibOfInt (shiftR i 8 .&. 0xF)
          n2 = nibOfInt (shiftR i 4 .&. 0xF)
          n3 = nibOfInt (i .&. 0xF)

addrToInt :: Addr -> Int
addrToInt (Addr a b c) = (256 * nibToInt a) + (16 * nibToInt b) + nibToInt c

----------------------------------------------------------------------
-- Byte

data Byte = Byte Nib Nib -- 8 bit, 2 nibbles
    deriving (Eq)

instance Show Byte where show (Byte n1 n2) = show n1 <> show n2

byte :: Nib -> Nib -> Byte
byte = Byte -- TODO: inline if keep this rep

byte1 :: Byte
byte1 = Byte N0 N1

byte0 :: Byte
byte0 = Byte N0 N0

andByte :: Byte -> Byte -> Byte
andByte b1 b2 = byteOfInt (byteToInt b1 .&. byteToInt b2)

orByte :: Byte -> Byte -> Byte
orByte b1 b2 = byteOfInt (byteToInt b1 .|. byteToInt b2)

xorByte :: Byte -> Byte -> Byte
xorByte b1 b2 = byteOfInt (byteToInt b1 `xor` byteToInt b2)

addByte :: Byte -> Byte -> Byte
addByte a b = fst $ addByteCarry a b

addByteCarry :: Byte -> Byte -> (Byte,Bool)
addByteCarry a b = do
    let n = byteToInt a + byteToInt b
    (byteOfInt (n `mod` 256), n >= 256)

subByteBorrow :: Byte -> Byte -> (Byte,Bool)
subByteBorrow a b = do
    let n = byteToInt a - byteToInt b
    if n < 0
        then (byteOfInt (n+256), True)
        else (byteOfInt n, False)

eqByte :: Byte -> Byte -> Bool
eqByte = (==)

lowNib :: Byte -> Nib
lowNib (Byte _ n) = n

byteOfInt :: Int -> Byte
byteOfInt i = if bad then error "byteOfInt" else a
    where a = Byte n1 n2
          bad = i < 0 || shiftR i 8 > 0
          n1 = nibOfInt (shiftR i 4 .&. 0xF)
          n2 = nibOfInt (i .&. 0xF)

byteToInt :: Byte -> Int
byteToInt (Byte n m) = nibToInt n * 16 + nibToInt m

nibsOfByte :: Byte -> (Nib,Nib)
nibsOfByte (Byte n m) = (n,m)

bitsOfByte :: Byte -> [Bool]
bitsOfByte b = map (testBit (byteToInt b)) (reverse [0..7])

bcd :: Byte -> (Byte,Byte,Byte)
bcd b = (byteOfInt h, byteOfInt t, byteOfInt u)
    where
        h = n `div` 100
        t = n `div` 10 `mod` 10
        u = n `mod` 10
        n = byteToInt b

byteShiftL :: Byte -> (Byte,Bool)
byteShiftL b = (byteOfInt ((n*2) `mod` 256), n>=128) where n = byteToInt b

byteShiftR :: Byte -> (Byte,Bool)
byteShiftR b = (byteOfInt (n `div` 2), testBit n 0) where n = byteToInt b

randBytes :: IO [Byte]
randBytes = do
  g <- newStdGen
  return $ map byteOfInt $ randomRs (0,255) g

tickDelayN :: Int -> Byte -> Byte
tickDelayN n b = byteOfInt (max 0 (byteToInt b - n))

----------------------------------------------------------------------
-- Nib

data Nib = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA | NB | NC | ND | NE | NF
    deriving (Eq,Ord)

instance Show Nib where show n = printf "%01X" (nibToInt n)

nibToInt :: Nib -> Int
nibToInt = \case
    N0 -> 0;  N1 -> 1;  N2 -> 2;  N3 -> 3
    N4 -> 4;  N5 -> 5;  N6 -> 6;  N7 -> 7
    N8 -> 8;  N9 -> 9;  NA -> 10; NB -> 11
    NC -> 12; ND -> 13; NE -> 14; NF -> 15

nibOfInt :: Int -> Nib
nibOfInt = \case
    0  -> N0; 1 ->  N1; 2 ->  N2; 3 ->  N3
    4  -> N4; 5 ->  N5; 6 ->  N6; 7 ->  N7
    8  -> N8; 9 ->  N9; 10 -> NA; 11 -> NB
    12 -> NC; 13 -> ND; 14 -> NE; 15 -> NF
    i -> error $ "nibOfInt: " <> show i
