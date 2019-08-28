{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Assemble (
    myGameBytes,
    ) where

import Control.Monad (ap,liftM)

import Emulator(
    Instruction(..),
    Op(..),
    Reg(..),
    Addr,
    Byte, byteOfNibs,
    Nib,
    baseProgram,
    nextInstr,
    encode,
    )

----------------------------------------------------------------------
-- myGame...

myGameBytes :: [Byte]
myGameBytes = assemble myAsm


_qq :: ()
_qq = do
    let _ = (waitTime,waitKey,halt,crash,writePatToMem,writeGliderToMem,decrementReg,ifKeyDown,
             div8,div8mul32,copyRectByPixel,wait)
    ()

myAsm :: Asm ()
myAsm = do
    let a1 = 0x500
    let a2 = 0x700
    --_writePatToScreen
    --writePatToMem a1 -- what is word for Mem intended to be blatted to a screen, a Rect?

    --writeGliderToMem a1
    writeDigitToMem 1 0x500
    writeDigitToMem 2 0x508
    --writeDigitToMem 3 0x510
    --writeDigitToMem 4 0x518

    blatRectToScreen a1

    forever $ do
        copyRectByPixel_shiftedR a1 a2
        copyMemBytes a2 a1 32
        cls
        wait
        blatRectToScreen a1

    crash


writeDigitToMem :: Int -> Addr -> Asm ()
writeDigitToMem n addr = do
    let a = fromIntegral $ 5*n -- from emulator embedding at 0,5,10...
    copyMemBytes a addr 5

writeGliderToMem :: Addr -> Asm ()
writeGliderToMem addr = do
    let a = undefined
    copyMemBytes a addr 5


stripesFragX,screenFragY :: (Byte,Byte)
stripesFragX = (0,2) -- (0,8)
screenFragY = (0,16) --(0,32)

blatRectToScreen :: Addr -> Asm ()
blatRectToScreen addr = do
    Emit $ OpStoreI addr
    withInitReg 1 $ \one -> do
        loopFromUpto stripesFragX $ \xStripe -> do
            loopFromUpto screenFragY $ \y -> do
                mul8 xStripe $ \x -> do
                    draw 1 (x,y)
                    Emit $ OpIncreaseI one
                    return ()



rectMaxX,rectMaxY :: Byte
(rectMaxX,rectMaxY) = (16,16) -- (16,16) -- (64,32)

copyRectByPixel :: Addr -> Addr -> Asm ()
copyRectByPixel a b = do
    loopUpto rectMaxX $ \x -> do
        loopUpto rectMaxY $ \y -> do
            copyCell (a,x,y) (b,x,y)


copyRectByPixel_shiftedR :: Addr -> Addr -> Asm ()
copyRectByPixel_shiftedR a b = do
    loopUpto rectMaxX $ \x -> do
        loopUpto rectMaxY $ \y -> do
            right x $ \x' -> do
                copyCell (a,x,y) (b,x',y)


right :: Reg -> (Reg -> Asm ()) -> Asm ()
right r k = do
    copy r $ \s -> do
        incrementReg s
        mod16 s $ \t -> do
            k t

copyCell :: (Addr,Reg,Reg) -> (Addr,Reg,Reg) -> Asm ()
copyCell (a1,x1,y1) (a2,x2,y2) = do
    WithReg $ \hold -> do
        readCell a1 (x1,y1) $ \v -> do
            copyReg hold v
        ifRegNotZero hold $
            setCell a2 (x2,y2)


readCell :: Addr -> (Reg,Reg) -> (Reg -> Asm ()) -> Asm ()
readCell addr (x,y) k = do
    xyToOffsetBitnum (x,y) $ \offset bitnum -> do
        readMem addr offset $ \loaded -> do
            bitnum2mask bitnum $ \mask -> do
                logicalAnd mask loaded k

setCell :: Addr -> (Reg,Reg) -> Asm ()
setCell addr (x,y) = do
    xyToOffsetBitnum (x,y) $ \offset bitnum -> do
        readMem addr offset $ \loaded -> do
            bitnum2mask bitnum $ \mask -> do
                logicalOr mask loaded $ \updated ->
                    writeMem addr offset updated


{-
_old_xyToOffsetBitnum :: (Reg,Reg) -> (Reg -> Reg -> Asm ()) -> Asm ()
_old_xyToOffsetBitnum (x,y) k =
    mul8 y $ \y8 -> do
        div8 x $ \xH -> do
            add xH y8 $ \offset -> do
                mod8 x $ \bitnum -> do
                    k offset bitnum
-}

xyToOffsetBitnum :: (Reg,Reg) -> (Reg -> Reg -> Asm ()) -> Asm ()
xyToOffsetBitnum (x,y) k = do
    --div8mul32 x $ \xH -> do
    div8mul16 x $ \xH -> do -- because we are working on a hlf-height screen fragment
        add xH y $ \offset -> do
            mod8 x $ \bitnum -> do
                k offset bitnum


bitnum2mask :: Reg -> (Reg -> Asm ()) -> Asm () -- couting bits from 0..7
bitnum2mask r k = do
    withInitReg 128 $ \res -> do
        ifRegNotZero r $ do
            loopUptoR r $ \_ -> do
                rshift 1 res
        k res


ifRegNotZero :: Reg -> Asm () -> Asm ()
ifRegNotZero r asm = do
    Emit $ OpSkipNotEqLit r 0
    JumpOver asm





copyMemBytes :: Addr -> Addr -> Byte -> Asm ()
copyMemBytes a1 a2 n = do
    loopUpto n $ \offset -> do
        readMem a1 offset $ \v -> do
            writeMem a2 offset v


readMem :: Addr -> Reg -> (Reg -> Asm ()) -> Asm ()
readMem addr offset k = do
    setI addr
    offsetI offset
    readMemAtI k

readMemAtI :: (Reg -> Asm ()) -> Asm ()
readMemAtI k = do
    Emit $ OpRestoreRegs (Reg 0)
    WithReg $ \r -> do
        copyReg r (Reg 0)
        k r

writeMem :: Addr -> Reg -> Reg -> Asm ()
writeMem addr offset v = do
    setI addr
    offsetI offset
    writeMemAtI v

writeMemAtI :: Reg -> Asm ()
writeMemAtI r = do
    copyReg (Reg 0) r
    Emit $ OpSaveRegs (Reg 0)


setI :: Addr -> Asm ()
setI addr = Emit $ OpStoreI addr

offsetI :: Reg -> Asm ()
offsetI r = Emit $ OpIncreaseI r


writePatToMem :: Addr -> Asm ()
writePatToMem addr = do
    Emit $ OpStoreI addr
    withInitReg 1 $ \one -> do
        loop00toFF $ \n -> do
            writeMemAtI n
            Emit $ OpIncreaseI one
            return ()


{-
_writePatToScreen :: Asm ()
_writePatToScreen = do
    Emit $ OpStoreI 0x300
    withInitReg 0 $ \n -> do
        loopUpto 32 $ \y -> do
            loopUpto 8 $ \xStripe -> do
                mul8 xStripe $ \x -> do
                    writeMemAtI n
                    draw 1 (x,y)
                    incrementReg n
                    return ()
-}


mul8 :: Reg -> (Reg -> Asm a) -> Asm a
mul8 x k = do
    copy x $ \y -> do
        lshift 3 y
        k y

div8 :: Reg -> (Reg -> Asm a) -> Asm a
div8 x k = do
    copy x $ \y -> do
        rshift 3 y
        k y

div8mul32 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul32 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        rshift 3 y
        lshift 5 y
        k y

div8mul16 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul16 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        rshift 3 y
        lshift 4 y
        k y

mod8 :: Reg -> (Reg -> Asm a) -> Asm a
mod8 x k = do
    withInitReg 0x7 $ \mask -> do
        logicalAnd x mask k

mod16 :: Reg -> (Reg -> Asm a) -> Asm a
mod16 x k = do
    withInitReg 0xF $ \mask -> do
        logicalAnd x mask k


logicalAnd :: Reg -> Reg -> (Reg -> Asm a) -> Asm a
logicalAnd x y k = do
    copy x $ \res -> do
        Emit $ OpAnd res y
        k res

logicalOr :: Reg -> Reg -> (Reg -> Asm a) -> Asm a
logicalOr x y k = do
    copy x $ \res -> do
        Emit $ OpOr res y
        k res


add :: Reg -> Reg -> (Reg -> Asm a) -> Asm a
add a b k = do
    copy a $ \res -> do
        Emit $ OpAdd res b
        k res



lshift :: Int -> Reg -> Asm ()
lshift n r = if n < 0 then error "lshift" else
    sequence_ (replicate n $ Emit $ OpShiftL r r)

rshift :: Int -> Reg -> Asm ()
rshift n r = if n < 0 then error "rshift" else
    sequence_ (replicate n $ Emit $ OpShiftR r r)


loopUpto :: Byte -> (Reg -> Asm ()) -> Asm ()
loopUpto b = loopFromUpto (0,b)

loopFromUpto :: (Byte,Byte) -> (Reg -> Asm ()) -> Asm ()
loopFromUpto (a,b) k = do
    withInitReg a $ \x -> do
        forever $ do
            k x
            incrementReg x
            Emit $ OpSkipEqLit x b


loopUptoR :: Reg -> (Reg -> Asm ()) -> Asm ()
loopUptoR r k = do
    withInitReg 0 $ \x -> do
        forever $ do
            k x
            incrementReg x
            Emit $ OpSkipEq x r


loop00toFF ::  (Reg -> Asm ()) -> Asm ()
loop00toFF k = do
    withInitReg 0 $ \x -> do
        forever $ do
            k x
            incrementReg x
            Emit $ OpSkipEqLit x 0

cls ::  Asm ()
cls = Emit $ OpCls

halt :: Asm ()
halt = forever $ return ()

crash ::  Asm ()
crash = Emit $ OpJump 0

{-
_myAsm :: Asm ()
_myAsm = do
    withInitReg 0 $ \num -> do
        withInitReg 30 $ \x -> do
            withInitReg 25 $ \y -> do
                Emit $ OpStoreDigitSpriteI num
                draw 5 (x,y) --on
                forever $ do
                    waitTime 3
                copy x $ \x' -> do
                    copy y $ \y' -> do
                        ifKeyDown 4 $ decrementReg x
                        ifKeyDown 6 $ incrementReg x
                        draw 5 (x',y') --off
                        draw 5 (x,y) --on
-}

copy :: Reg -> (Reg -> Asm a) -> Asm a
copy x k = do
    WithReg $ \y -> do
        copyReg y x
        k y

copyReg :: Reg -> Reg -> Asm ()
copyReg target source =
    Emit $ OpStore target source


withInitReg :: Byte -> (Reg -> Asm a) -> Asm a
withInitReg v k = do
    WithReg $ \x -> do
        Emit $ OpStoreLit x v
        k x

_drawDig :: Reg -> (Reg,Reg) -> Asm ()
_drawDig num (x,y) = do
    Emit $ OpStoreDigitSpriteI num
    draw 5 (x,y)

draw :: Nib -> (Reg,Reg) -> Asm ()
draw h (x,y) = Emit $ OpDraw x y h


incrementReg :: Reg -> Asm ()
incrementReg reg = Emit $ OpAddLit reg 1

decrementReg :: Reg -> Asm ()
decrementReg reg = Emit $ OpAddLit reg 0xFF

ifKeyDown :: Nib -> Asm () -> Asm ()
ifKeyDown nib asm = do
   WithReg $ \r -> do
    Emit $ OpStoreLit r (byteOfNibs 0 nib)
    Emit $ OpSkipKey r
    JumpOver asm

waitTime :: Byte -> Asm ()
waitTime b = do
   WithReg $ \r -> do
    Emit $ OpStoreLit r b
    Emit $ OpSetDelay r
    forever $ do
        Emit $ OpReadDelay r
        Emit $ OpSkipEqLit r 0

wait :: Asm ()
wait = do
    waitKeyUp 4
    Emit $ OpWaitKeyPress (Reg 0)

waitKey :: Nib -> Asm ()
waitKey nib = do waitKeyUp nib; waitKeyDown nib

waitKeyUp :: Nib -> Asm ()
waitKeyUp nib = do
   WithReg $ \r -> do
    Emit $ OpStoreLit r (byteOfNibs 0 nib)
    forever $ Emit $ OpSkipNotKey r

waitKeyDown :: Nib -> Asm ()
waitKeyDown nib = do
   WithReg $ \r -> do
    Emit $ OpStoreLit r (byteOfNibs 0 nib)
    forever $ Emit $ OpSkipKey r

forever :: Asm () -> Asm ()
forever asm = do
    q <- Here
    asm
    Emit $ OpJump q
    --return x




----------------------------------------------------------------------
-- assemble

assemble :: Asm () -> [Byte]
assemble asm = instructionsToBytes $ map encode ops where
    regs = [Reg 1..Reg 14]
    (_,(),ops) = assembleAsm baseProgram regs asm

instructionsToBytes :: [Instruction] -> [Byte]
instructionsToBytes is = do Instruction b1 b2 <- is; [b1,b2]

assembleAsm :: Addr -> [Reg] -> Asm a -> (Addr,a,[Op])
assembleAsm q free asm = do
    case asm of
        Ret a -> (q,a,[])
        Bind m f -> do
            let (q1,a,ops1) = assembleAsm q free m
            let (q2,b,ops2) = assembleAsm q1 free $ f a
            (q2,b,ops1<>ops2)
        Here -> (q,q,[])
        Emit op -> (nextInstr q,(),[op])
        WithReg k -> case free of [] -> error "free=[]"; next:free' -> assembleAsm q free' (k next)
        JumpOver m -> do
            let (q',a,ops) = assembleAsm (nextInstr q) free m
            let j = OpJump q'
            (q',a,j:ops)

data Asm a where
    Ret :: a -> Asm a
    Bind :: Asm a -> (a -> Asm b) -> Asm b
    Here :: Asm Addr
    Emit :: Op -> Asm ()
    WithReg :: (Reg -> Asm a) -> Asm a
    JumpOver :: Asm a -> Asm a

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = return; (<*>) = ap
instance Monad Asm where return = Ret; (>>=) = Bind
