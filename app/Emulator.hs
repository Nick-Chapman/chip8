module Emulator (

    Byte(..), byteOfNibs, byteToInt, byteOfInt,
    randBytes,
    showDisassemble,

    ChipState(..),
    initCS,
    step, ChipKeys,
    tick,
    showChipStateLine,

    instructionLookup, Instruction(..),

    encode, decode, Op(..),
    Screen(..), xmax,ymax,
    Reg(..),
    Regs,
    regValue,
    Nib(..), nibKey,
    Addr(..), baseProgram, nextInstr, addAddr, addrOfInt, addrToInt

    ) where

import Control.Monad (ap,liftM)
import Control.Monad.State (State,execState,get,put)
import Data.Bits
import Data.Char as Char
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Set (Set,(\\))
--import Data.Word8 as Word8 -- (Word8)
import Prelude hiding (pred)
import System.Random (newStdGen,randomRs)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

----------------------------------------------------------------------
-- dissassemble

{-showCode :: Addr -> [Instruction] -> String
showCode a is = showCodeLine a xs <> rest
    where (xs,ys) = splitAt size is; size = 8
          rest = if null is then "" else "\n" <> showCode (addAddr a size) ys

showCodeLine :: Addr -> [Instruction] -> String
showCodeLine a bs = show a <> " : " <> List.intercalate " " (map show bs)-}

showDisassemble :: [Byte] -> String
showDisassemble bytes =
    unlines $ map (showLocatedOp reachSet amap) $ zip addrs ops
    where
        instructions = pairUpBytes bytes
        a0 = baseProgram
        addrs = map (addAddr a0) $ map (2*) [0..]
        ops = map decode instructions
        amap = Map.fromList $ zip targetAddrs (map Lab [1..])
        targetAddrs = List.nub $ List.sort (reachableOps >>= opAddresses)
        reachSet = reachable ops

        reachableOps :: [Op] = map snd $ filter ((`Set.member` reachSet) . fst) $ zip addrs ops

newtype Lab = Lab Int
instance Show Lab where show (Lab i) = "L" <> printf "%02d" i

showLocatedOp :: Set Addr -> Map Addr Lab -> (Addr,Op) -> String
showLocatedOp reachSet aMap (a,op) =
    show a
    <> " : " <> case Map.lookup a aMap of Just lab -> show lab; Nothing -> "   "
    <> " : " <> show (encode op)
    <> " : " <>
    if Set.member a reachSet
    then "     : " <> showOpWithLabels aMap op
    else showIasChars (encode op) <> " :"



{-showIasBits :: Instruction -> String
showIasBits (Instruction b0 b1) = bitString b0 <> " " <> bitString b1
    where bitString = map (\b -> if b then '1' else '0') . bitsOfByte-}

showIasChars :: Instruction -> String
showIasChars (Instruction b0 b1) = if length s == 4 then s else "    "
    where s = show [c0,c1]
          c0 = Char.chr (byteToInt b0)
          c1 = Char.chr (byteToInt b1)

showOpWithLabels :: Map Addr Lab -> Op -> String
showOpWithLabels m op =
    show op <> (seeLabs $ mapMaybe (flip Map.lookup m) $ opAddresses op)
    where seeLabs =
              \case [] -> ""; xs -> " (" <> unwords (map show xs) <> ")"


-- static determination of address which contain executable code

reachable :: [Op] -> Set Addr
reachable ops = reachFromAcc Set.empty [baseProgram] where

    m :: Map Addr Op = Map.fromList $ zip (map (addAddr baseProgram) $ map (2*) [0..]) ops

    step :: Addr -> [Addr]
    step a = case Map.lookup a m of
        Nothing -> [] -- the address is outside the static code
        Just op -> nextPC a op

    reachFromAcc :: Set Addr -> [Addr] -> Set Addr
    reachFromAcc seen fringe =
        case fringe of
            [] -> seen
            _ -> do
                let new = filter (not . (`Set.member` seen)) fringe
                let seen' = seen `Set.union` Set.fromList new
                let next = new >>= step
                reachFromAcc seen' next


nextPC :: Addr -> Op -> [Addr]
nextPC pc = \case
    OpJump a -> [a]
    OpJumpOffset a -> [a] -- ??
    OpCall a -> [a,next]
    OpReturn -> []
    OpSkipEqLit _ _ -> skippy
    OpSkipNotEqLit _ _ -> skippy
    OpSkipEq _ _ -> skippy
    OpSkipNotEq _ _ -> skippy
    OpSkipKey _ -> skippy
    OpSkipNotKey _ -> skippy
    _ -> [next]
    where
        next = nextInstr pc -- +2 (fallthrough)
        twoAhead = nextInstr next -- +2 (fallthrough)
        skippy = [next,twoAhead]

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
-- step, tick

step :: ChipKeys -> ChipState -> ChipState
step ck cs = execState (runAction ck $ fetchDecodeExec) $ cs

fetchDecodeExec :: Action ()
fetchDecodeExec = do
    pc <- PC
    SetPC (nextInstr pc)
    Fetch pc >>= (exec . decode)

tick :: Int -> ChipState -> ChipState
tick n cs = cs { delay = tickDelayN n (delay cs) }

----------------------------------------------------------------------
-- encode

encode :: Op -> Instruction
encode = mkInstruction . \case
    OpCls -> (0,0,0xE,0)
    OpReturn -> (0,0,0xE,0xE)
    OpJump (Addr a b c) -> (1,a,b,c)
    OpCall (Addr a b c) -> (2,a,b,c)
    OpSkipEqLit (Reg x) b -> (3,x,n,m) where (n,m) = byteNibs b
    OpSkipNotEqLit (Reg x) b -> (4,x,n,m) where (n,m) = byteNibs b
    OpSkipEq (Reg x) (Reg y) -> (5,x,y,0)
    OpStoreLit (Reg x) b -> (6,x,n,m) where (n,m) = byteNibs b
    OpAddLit (Reg x) b -> (7,x,n,m) where (n,m) = byteNibs b
    OpStore (Reg x) (Reg y) -> (8,x,y,0)
    OpOr (Reg x) (Reg y) -> (8,x,y,1)
    OpAnd (Reg x) (Reg y) -> (8,x,y,2)
    OpXor (Reg x) (Reg y) -> (8,x,y,3)
    OpAdd (Reg x) (Reg y) -> (8,x,y,4)
    OpSub (Reg x) (Reg y) -> (8,x,y,5)
    OpShiftR (Reg x) (Reg y) -> (8,x,y,6)
    OpMinus (Reg x) (Reg y) -> (8,x,y,7)
    OpShiftL (Reg x) (Reg y) -> (8,x,y,0xE)
    OpSkipNotEq (Reg x) (Reg y) -> (9,x,y,0)
    OpStoreI (Addr a b c) -> (0xA,a,b,c)
    OpJumpOffset (Addr a b c) -> (0xB,a,b,c)
    OpRandom (Reg x) b -> (0xC,x,n,m) where (n,m) = byteNibs b
    OpDraw (Reg x) (Reg y) n -> (0xD,x,y,n)
    OpSkipKey (Reg x) -> (0xE,x,9,0xE)
    OpSkipNotKey (Reg x) -> (0xE,x,0xA,1)
    OpReadDelay (Reg x) -> (0xF,x,0,7)
    OpWaitKeyPress (Reg x) -> (0xF,x,0,0xA)
    OpSetDelay (Reg x) -> (0xF,x,1,5)
    OpSetSound (Reg x) -> (0xF,x,1,8)
    OpIncreaseI (Reg x) -> (0xF,x,1,0xE)
    OpStoreDigitSpriteI (Reg x) -> (0xF,x,2,9)
    OpStoreBCD (Reg x) -> (0xF,x,3,3)
    OpSaveRegs (Reg x) -> (0xF,x,5,5)
    OpRestoreRegs (Reg x) -> (0xF,x,6,5)
    --OpUnknown i -> error $ show i
    OpUnknown (Instruction b0 b1) -> (w,x,y,z) where (w,x) = byteNibs b0; (y,z) = byteNibs b1

mkInstruction :: (Nib,Nib,Nib,Nib) ->  Instruction
mkInstruction (a,b,c,d) = Instruction (byteOfNibs a b) (byteOfNibs c d)

----------------------------------------------------------------------
-- decode,

decode :: Instruction -> Op
decode (Instruction b1 b2) =
    decodeMatch (n1,n2,n3,n4) where (n1,n2) = nibsOfByte b1; (n3,n4) = nibsOfByte b2

decodeMatch :: (Nib,Nib,Nib,Nib) -> Op
decodeMatch tup = case tup of
    (0,0,0xE,0) -> OpCls
    (0,0,0xE,0xE) -> OpReturn
    (1,a,b,c) -> OpJump (addr a b c)
    (2,a,b,c) -> OpCall (addr a b c)
    (3,x,n,m) -> OpSkipEqLit (Reg x) (byteOfNibs n m)
    (4,x,n,m) -> OpSkipNotEqLit (Reg x) (byteOfNibs n m)
    (5,x,y,0) -> OpSkipEq (Reg x) (Reg y)
    (6,x,n,m) -> OpStoreLit (Reg x) (byteOfNibs n m)
    (7,x,n,m) -> OpAddLit (Reg x) (byteOfNibs n m)
    (8,x,y,0) -> OpStore (Reg x) (Reg y)
    (8,x,y,1) -> OpOr (Reg x) (Reg y)
    (8,x,y,2) -> OpAnd (Reg x) (Reg y)
    (8,x,y,3) -> OpXor (Reg x) (Reg y)
    (8,x,y,4) -> OpAdd (Reg x) (Reg y)
    (8,x,y,5) -> OpSub (Reg x) (Reg y)
    (8,x,y,6) -> OpShiftR (Reg x) (Reg y)
    (8,x,y,7) -> OpMinus (Reg x) (Reg y)
    (8,x,y,0xE) -> OpShiftL (Reg x) (Reg y)
    (9,x,y,0) -> OpSkipNotEq (Reg x) (Reg y)
    (0xA,a,b,c) -> OpStoreI (Addr a b c)
    (0xB,a,b,c) -> OpJumpOffset (Addr a b c)
    (0xC,x,n,m) -> OpRandom (Reg x) (byteOfNibs n m)
    (0xD,x,y,n) -> OpDraw (Reg x) (Reg y) n
    (0xE,x,9,0xE) -> OpSkipKey (Reg x)
    (0xE,x,0xA,1) -> OpSkipNotKey (Reg x)
    (0xF,x,0,7) -> OpReadDelay (Reg x)
    (0xF,x,0,0xA) -> OpWaitKeyPress (Reg x)
    (0xF,x,1,5) -> OpSetDelay (Reg x)
    (0xF,x,1,8) -> OpSetSound (Reg x)
    (0xF,x,1,0xE) -> OpIncreaseI (Reg x)
    (0xF,x,2,9) -> OpStoreDigitSpriteI (Reg x)
    (0xF,x,3,3) -> OpStoreBCD (Reg x)
    (0xF,x,5,5) -> OpSaveRegs (Reg x)
    (0xF,x,6,5) -> OpRestoreRegs (Reg x)
    _ -> unknown
    where
        unknown = OpUnknown (Instruction (byteOfNibs a b) (byteOfNibs c d)) where (a,b,c,d) = tup

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
        OpUnknown (Instruction b0 b1) -> "???(" <> show b0 <> show b1 <> ")"
        OpCls                   -> "CLS"
        OpReturn                -> "RET"
        OpJump a                -> una "JMP" a
        OpJumpOffset a          -> bin "JMP" (Reg 0) a
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
    OpJumpOffset a -> Read (Reg 0) >>= (SetPC . addAddr a . byteToInt)
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
        WriteMem (addAddr a 1) t
        WriteMem (addAddr a 2) u

    OpSaveRegs x -> do
        a <- ReadI
        let n = unReg x
        let rs = map Reg [0..n]
        mapM_ (\r -> Read r >>= WriteMem (addAddr a $ nibToInt $ unReg r)) rs
        --StoreI (addAddr a (n+1)) --???
        return ()

    OpRestoreRegs x -> do
        a <- ReadI
        let n = unReg x
        let rs = map Reg [0..n]
        mapM_ (\r -> ReadMem (addAddr a $ nibToInt $ unReg r) >>= Write r) rs
        --StoreI (addAddr a (n+1)) --???
        return ()

    OpStoreDigitSpriteI x ->
        Read x >>= (StoreI . hexDigitSpriteAddr . lowNib)

    OpIncreaseI x -> do
        a <- ReadI
        b <- Read x
        StoreI (addAddr a $ byteToInt b)

    OpWaitKeyPress x -> -- TODO: should wait afterwards for key to be released
        AnyKey >>= \case Nothing -> Stall
                         Just nib -> Write x (byteOfNibs N0 nib)

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
instance Applicative Action where pure = Ret; (<*>) = ap
instance Monad Action where (>>=) = Bind

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
            let (screen',collide) = drawSprite screen (wraparound xy) bytes
            put $ cs { screen = screen' }
            return collide
        Cls -> put $ cs { screen = emptyScreen }
        Crash -> put $ cs { crashed = True }
        Stall -> put $ cs { pc = backupInstr pc, nExec = nExec - 1 }

checkAnyKey :: ChipKeys -> Maybe Nib
checkAnyKey keys = do
    let look n = (n,keys n)
    let xs = map fst $ filter snd $ map look [0..15]
    case xs of [] -> Nothing; nib:_ -> Just nib

drawSprite :: Screen -> XY -> [Byte] -> (Screen,Bool)
drawSprite screen (x0,y0) bytes = do
    let xys = do
            (y,by) <- zip [y0..] bytes
            (x,b) <- zip [x0..] $ bitsOfByte by
            --if b then [wraparound (x,y)] else []  -- pixel based wraparound -- wrong!
            if b then [(x,y)] else []
    flipPixels screen $ Set.fromList xys

wraparound :: XY -> XY
wraparound (x,y) = (x `mod` xmax,y `mod` ymax)
         --if b then [] else [] -- pixel based wraparound -- wrong!

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
    { mem :: !Mem
    , regs :: !Regs
    , delay :: !Byte
    , pc :: !Addr
    , stack :: ![Addr]
    , regI :: !Addr
    , screen :: !Screen
    , nExec :: !Int -- number of intructions executed so far
    , rands :: [Byte]
    , crashed :: !Bool
    }

showChipStateLine :: ChipState -> String
showChipStateLine ChipState{delay,regI,regs,mem,pc,stack,nExec} = do
    let instr = instructionLookup pc mem
    let op = decode instr
    unwords (["#" <> show nExec, show delay, show regI, "--"]
             <> map show (regVals regs)
             <> ["--", "(" <> show (length stack) <> ")",
                 show pc, ":", show op])

regVals :: Regs -> [Byte]
regVals regs = map (regValue regs) [Reg 0 .. Reg 0xF]

----------------------------------------------------------------------
-- Regs

type Regs = Map Reg Byte

regValue :: Regs -> Reg -> Byte
regValue regs reg = fromMaybe byte0 $ Map.lookup reg regs

newtype Reg = Reg { unReg :: Nib } deriving (Eq,Ord,Enum)
instance Show Reg where show r = "v" <> show (unReg r)

----------------------------------------------------------------------
-- Mem

type Mem = Map Addr Byte

instructionLookup :: Addr -> Mem -> Instruction
instructionLookup a mem  = Instruction b1 b2
    where b1 = fromMaybe byte0 $ Map.lookup a mem
          b2 = fromMaybe byte0 $ Map.lookup (addAddr a 1) mem

initMem :: [Byte] -> Mem
initMem bytes = Map.fromList $ do
    (i,b) <- zip [addrToInt baseProgram..] bytes
        ++ zip [addrToInt baseHexSpriteData..] digitData
    return $ (addrOfInt i, b)

hexDigitSpriteAddr :: Nib -> Addr
hexDigitSpriteAddr n = addAddr baseHexSpriteData (5 * nibToInt n)

digitData :: [Byte]
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

data Addr = Addr !Nib !Nib !Nib -- 12 bits, 3 nibble
    deriving (Eq,Ord)

instance Num Addr where
    (+) a1 a2 = addrOfInt (addrToInt a1 + addrToInt a2)
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = addrOfInt . fromIntegral
    negate = undefined

instance Show Addr where show a = printf "0x%03X" (addrToInt a)

addr :: Nib -> Nib -> Nib -> Addr
addr = Addr

nextInstr :: Addr -> Addr
nextInstr a = addAddr a 2

backupInstr :: Addr -> Addr
backupInstr a = addAddr a (-2)

addAddr :: Addr -> Int -> Addr
addAddr a i = addrOfInt (addrToInt a + i)

addrOfInt :: Int -> Addr
addrOfInt i = if bad then error $ "addrOfInt: " <> show i else a
    where a = Addr n1 n2 n3
          bad = i < 0 || shiftR i 12 > 0
          n1 = nibOfInt (shiftR i 8 .&. 0xF)
          n2 = nibOfInt (shiftR i 4 .&. 0xF)
          n3 = nibOfInt (i .&. 0xF)

addrToInt :: Addr -> Int
addrToInt (Addr a b c) = (256 * nibToInt a) + (16 * nibToInt b) + nibToInt c

----------------------------------------------------------------------
-- Byte


data Byte = Byte !Nib !Nib -- 8 bit, 2 nibbles
    deriving (Eq,Ord)

instance Num Byte where
    (+) b1 b2 = byteOfInt (byteToInt b1 + byteToInt b2)
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = byteOfInt . fromIntegral
    negate = undefined

instance Show Byte where show (Byte n1 n2) = show n1 <> show n2

byteNibs :: Byte -> (Nib,Nib)
byteNibs (Byte n1 n2) = (n1,n2)

byteOfNibs :: Nib -> Nib -> Byte
byteOfNibs = Byte

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
byteOfInt i = if bad then error $ "byteOfInt: " <> show i else a
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
{-
newtype Byte = Byte Word8
    deriving (Eq,Num,Bits)

instance Show Byte where
    show b = show n1 <> show n2
        where (n1,n2) = nibsOfByte b

byteOfNibs :: Nib -> Nib -> Byte
byteOfNibs = undefined

byteNibs :: Byte -> (Nib,Nib)
byteNibs = undefined

byte1 :: Byte
byte1 = 1

byte0 :: Byte
byte0 = 0

andByte :: Byte -> Byte -> Byte
andByte b1 b2 = b1 .&. b2

orByte :: Byte -> Byte -> Byte
orByte b1 b2 = b1 .|. b2

xorByte :: Byte -> Byte -> Byte
xorByte b1 b2 = b1 `xor` b2

addByte :: Byte -> Byte -> Byte
addByte a b = fst $ addByteCarry a b

addByteCarry :: Byte -> Byte -> (Byte,Bool)
addByteCarry = undefined

subByteBorrow :: Byte -> Byte -> (Byte,Bool)
subByteBorrow = undefined

eqByte :: Byte -> Byte -> Bool
eqByte = (==)

lowNib :: Byte -> Nib
lowNib = undefined

byteOfInt :: Int -> Byte
byteOfInt i = if bad then error $ "byteOfInt: " <> show i else a
    where a = byteOfNibs n1 n2
          bad = i < 0 || shiftR i 8 > 0
          n1 = nibOfInt (shiftR i 4 .&. 0xF)
          n2 = nibOfInt (i .&. 0xF)

byteToInt :: Byte -> Int
byteToInt = undefined -- fromIntegral . toInteger -- (Byte n m) = nibToInt n * 16 + nibToInt m

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
-}

----------------------------------------------------------------------
-- Nib

data Nib = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA | NB | NC | ND | NE | NF
    deriving (Eq,Ord,Enum)

instance Num Nib where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = nibOfInt . fromIntegral
    negate = undefined

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

nibKey :: Nib -> Char
nibKey = \case
    N1 -> '1'; N2 -> '2'; N3 -> '3'; NC -> '4';
    N4 -> 'q'; N5 -> 'w'; N6 -> 'e'; ND -> 'r';
    N7 -> 'a'; N8 -> 's'; N9 -> 'd'; NE -> 'f';
    NA -> 'z'; N0 -> 'x'; NB -> 'c'; NF -> 'v';
