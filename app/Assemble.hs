{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Assemble (

    Reg(..), Addr, Byte, --reexport

    halt,
    break,
    crash,
    jump,
    draw,
    cls,
    forever,

    withInitReg,
    copy,

    copyReg,
    setReg,
    incrementReg,
    decrementReg,

    ifRegIs,
    ifRegNotZero,
    loopFor,
    loopForR,
    waitKeyUp,

    insertSubroutine,
    insertBytes,

    inPlaceAnd,
    inPlaceOr,
    inPlaceAdd,
    inPlaceShiftL,
    inPlaceShiftR,

    writeMem,
    readMem,

    setI,
    increaseI,

    Asm(..),

    assemble,

    ) where

import Prelude hiding (break)
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
    incAddr,
    encode,
    )

----------------------------------------------------------------------

halt :: Asm ()
halt = forever $ return ()

break :: Asm()
break = do
    waitKeyUp 4
    Emit $ OpWaitKeyPress (Reg 0)

crash :: Asm ()
crash = jump 0

jump :: Addr -> Asm ()
jump a = Emit $ OpJump a

draw :: Nib -> (Reg,Reg) -> Asm ()
draw h (x,y) = Emit $ OpDraw x y h

cls ::  Asm ()
cls = Emit $ OpCls

forever :: Asm () -> Asm ()
forever asm = do
    q <- Here
    asm
    Emit $ OpJump q

withInitReg :: Byte -> (Reg -> Asm a) -> Asm a
withInitReg v k = do
    WithReg $ \x -> do
        Emit $ OpStoreLit x v
        k x

copy :: Reg -> (Reg -> Asm a) -> Asm a
copy x k = do
    WithReg $ \y -> do
        copyReg x y
        k y

copyReg :: Reg -> Reg -> Asm ()
copyReg source target  = Emit $ OpStore target source

setReg :: Reg -> Byte -> Asm ()
setReg x v = Emit $ OpStoreLit x v

incrementReg :: Reg -> Asm ()
incrementReg reg = Emit $ OpAddLit reg 1

decrementReg :: Reg -> Asm ()
decrementReg reg = Emit $ OpAddLit reg 0xFF

ifRegIs :: Byte -> Reg -> Asm () -> Asm ()
ifRegIs n r act = do
    Emit $ OpSkipEqLit r n
    JumpOver act

ifRegNotZero :: Reg -> Asm () -> Asm ()
ifRegNotZero r asm = do
    Emit $ OpSkipNotEqLit r 0
    JumpOver asm

_loopFor :: (Int,Int) -> (Reg -> Asm ()) -> Asm ()
_loopFor (a,b) k = do
    withInitReg (fromIntegral a) $ \x -> do
        forever $ do
            k x
            incrementReg x
            Emit $ OpSkipEqLit x (fromIntegral b)

loopFor :: (Int,Int) -> (Reg -> Asm ()) -> Asm ()
loopFor (a,b) k = do
    withInitReg (fromIntegral a) $ \x -> do
        decrementReg x
        q <- Here
        incrementReg x
        k x
        Emit $ OpSkipEqLit x (fromIntegral $ b-1)
        Emit $ OpJump q


loopForR :: (Int,Reg) -> (Reg -> Asm ()) -> Asm ()
loopForR (a,r) k = do
    withInitReg (fromIntegral a) $ \x -> do
        forever $ do
            k x
            incrementReg x
            Emit $ OpSkipEq x r

waitKeyUp :: Nib -> Asm ()
waitKeyUp nib = do
    WithReg $ \r -> do
        Emit $ OpStoreLit r (byteOfNibs 0 nib)
        forever $ Emit $ OpSkipNotKey r

insertSubroutine :: Asm () -> Asm (Asm ())
insertSubroutine asm = do
    q <- Here
    JumpOver $ do
        asm
        Emit $ OpReturn
    return $ Emit $ OpCall $ nextInstr q

insertBytes :: [Byte] -> Asm Addr
insertBytes bs = do
    q <- Here
    JumpOver $ Bytes bs
    return (nextInstr q)

inPlaceAnd :: Reg -> Reg -> Asm ()
inPlaceAnd x y = Emit $ OpAnd x y

inPlaceOr :: Reg -> Reg -> Asm ()
inPlaceOr x y = Emit $ OpOr x y

inPlaceAdd :: Reg -> Reg -> Asm ()
inPlaceAdd x y = Emit $ OpAdd x y

inPlaceShiftL :: Int -> Reg -> Asm ()
inPlaceShiftL n r = if n < 0 then error "inPlaceShiftL" else
    sequence_ (replicate n $ Emit $ OpShiftL r r)

inPlaceShiftR :: Int -> Reg -> Asm ()
inPlaceShiftR n r = if n < 0 then error "inPlaceShiftR" else
    sequence_ (replicate n $ Emit $ OpShiftR r r)

readMem :: Addr -> Reg -> (Reg -> Asm ()) -> Asm ()
readMem addr offset k = do
    setI addr
    increaseI offset
    readMemAtI k

readMemAtI :: (Reg -> Asm ()) -> Asm ()
readMemAtI k = do
    Emit $ OpRestoreRegs (Reg 0)
    WithReg $ \r -> do
        copyReg (Reg 0) r
        k r

writeMem :: Addr -> Reg -> Reg -> Asm ()
writeMem addr offset v = do
    setI addr
    increaseI offset
    writeMemAtI v

writeMemAtI :: Reg -> Asm ()
writeMemAtI r = do
    copyReg r (Reg 0)
    Emit $ OpSaveRegs (Reg 0)

setI :: Addr -> Asm ()
setI addr = Emit $ OpStoreI addr

increaseI :: Reg -> Asm ()
increaseI r = Emit $ OpIncreaseI r

----------------------------------------------------------------------
-- assemble

assemble :: Asm () -> [Byte]
assemble asm = bytes where -- instructionsToBytes $ map encode ops where
    regs = [Reg 1..Reg 14]
    (_,(),bytes) = assembleAsm baseProgram regs asm

instructionsToBytes :: [Instruction] -> [Byte]
instructionsToBytes is = do Instruction b1 b2 <- is; [b1,b2]

assembleAsm :: Addr -> [Reg] -> Asm a -> (Addr,a,[Byte])
assembleAsm q free asm = do
    case asm of
        Ret a -> (q,a,[])
        Bind m f -> do
            let (q1,a,ops1) = assembleAsm q free m
            let (q2,b,ops2) = assembleAsm q1 free $ f a
            (q2,b,ops1<>ops2)
        Here -> (q,q,[])
        Bytes bs -> (incAddr q (length bs), (), bs)
        Emit op -> (nextInstr q, (), bEncode op)
        WithReg k -> case free of [] -> error "free=[]"; next:free' -> assembleAsm q free' (k next)
        JumpOver m -> do
            let (q',a,ops) = assembleAsm (nextInstr q) free m
            let j = OpJump q'
            (q',a, bEncode j ++ ops)

        Switch m1 m2 -> do
            let (q1,(),ops1) = assembleAsm (nextInstr (nextInstr q)) free m1
            let (q2,(),ops2) = assembleAsm q1 free m2
            let j1 = OpJump q1
            let j2 = OpJump q2
            (q2,(), bEncode j1 ++ ops1 ++ bEncode j2 ++ ops2)

bEncode :: Op -> [Byte]
bEncode op = instructionsToBytes [encode op]

data Asm a where
    Ret :: a -> Asm a
    Bind :: Asm a -> (a -> Asm b) -> Asm b
    Here :: Asm Addr
    Bytes :: [Byte] -> Asm ()
    Emit :: Op -> Asm ()
    WithReg :: (Reg -> Asm a) -> Asm a
    JumpOver :: Asm a -> Asm a
    Switch :: Asm () -> Asm () -> Asm ()

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = return; (<*>) = ap
instance Monad Asm where return = Ret; (>>=) = Bind
