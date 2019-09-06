{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

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
    insertSubroutineLater,
    insertBytes,
    insertBytesLater,

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
import Control.Monad.Fix (MonadFix,mfix)

import Emulator(
    Instruction(..),
    Op(..),
    Reg(..),
    Addr,
    Byte, byteOfNibs,
    Nib,
    baseProgram,
    addAddr,
    encode,
    )

----------------------------------------------------------------------

halt :: Asm ()
halt = forever $ return ()

break :: Asm()
break = do
    waitKeyUp 4
    emit $ OpWaitKeyPress (Reg 0)

crash :: Asm ()
crash = jump 0

jump :: Addr -> Asm ()
jump a = emit $ OpJump a

draw :: Nib -> (Reg,Reg) -> Asm ()
draw h (x,y) = emit $ OpDraw x y h

cls ::  Asm ()
cls = emit $ OpCls

forever :: Asm () -> Asm ()
forever asm = do
    q <- Here
    asm
    emit $ OpJump q

withInitReg :: Byte -> (Reg -> Asm a) -> Asm a
withInitReg v k = do
    WithReg $ \x -> do
        emit $ OpStoreLit x v
        k x

copy :: Reg -> (Reg -> Asm a) -> Asm a
copy x k = do
    WithReg $ \y -> do
        copyReg x y
        k y

copyReg :: Reg -> Reg -> Asm ()
copyReg source target = emit $ OpStore target source

setReg :: Reg -> Byte -> Asm ()
setReg x v = emit $ OpStoreLit x v

incrementReg :: Reg -> Asm ()
incrementReg reg = emit $ OpAddLit reg 1

decrementReg :: Reg -> Asm ()
decrementReg reg = emit $ OpAddLit reg 0xFF

ifRegIs :: Byte -> Reg -> Asm () -> Asm ()
ifRegIs n r act = do
    emit $ OpSkipEqLit r n
    jumpOver act

ifRegNotZero :: Reg -> Asm () -> Asm ()
ifRegNotZero r asm = do
    emit $ OpSkipNotEqLit r 0
    jumpOver asm

_loopFor :: (Int,Int) -> (Reg -> Asm ()) -> Asm ()
_loopFor (a,b) k = do
    withInitReg (fromIntegral a) $ \x -> do
        forever $ do
            k x
            incrementReg x
            emit $ OpSkipEqLit x (fromIntegral b)

loopFor :: (Int,Int) -> (Reg -> Asm ()) -> Asm ()
loopFor (a,b) k = do
    withInitReg (fromIntegral a) $ \x -> do
        decrementReg x
        q <- Here
        incrementReg x
        k x
        emit $ OpSkipEqLit x (fromIntegral $ b-1)
        emit $ OpJump q


loopForR :: (Int,Reg) -> (Reg -> Asm ()) -> Asm ()
loopForR (a,r) k = do
    withInitReg (fromIntegral a) $ \x -> do
        forever $ do
            k x
            incrementReg x
            emit $ OpSkipEq x r

waitKeyUp :: Nib -> Asm ()
waitKeyUp nib = do
    WithReg $ \r -> do
        emit $ OpStoreLit r (byteOfNibs 0 nib)
        forever $ emit $ OpSkipNotKey r

insertSubroutine :: Asm () -> Asm (Asm ())
insertSubroutine asm = mdo
    jump after
    loc <- subroutine asm
    after <- Here
    return $ call loc

insertSubroutineLater :: Asm () -> Asm (Asm ())
insertSubroutineLater asm = mdo
    loc <- Later $ subroutine asm
    return $ call loc

subroutine :: Asm () -> Asm Addr
subroutine body = do
    q <- Here
    body
    emit $ OpReturn
    return q

call :: Addr -> Asm ()
call q = emit $ OpCall q

insertBytes :: [Byte] -> Asm Addr
insertBytes bs = mdo
    jump q2
    q1 <- Here
    Emit bs
    q2 <- Here
    return q1

insertBytesLater :: [Byte] -> Asm Addr
insertBytesLater bs = do
    Later $ do
        q <- Here
        Emit bs
        return q


inPlaceAnd :: Reg -> Reg -> Asm ()
inPlaceAnd x y = emit $ OpAnd x y

inPlaceOr :: Reg -> Reg -> Asm ()
inPlaceOr x y = emit $ OpOr x y

inPlaceAdd :: Reg -> Reg -> Asm ()
inPlaceAdd x y = emit $ OpAdd x y

inPlaceShiftL :: Int -> Reg -> Asm ()
inPlaceShiftL n r = if n < 0 then error "inPlaceShiftL" else
    sequence_ (replicate n $ emit $ OpShiftL r r)

inPlaceShiftR :: Int -> Reg -> Asm ()
inPlaceShiftR n r = if n < 0 then error "inPlaceShiftR" else
    sequence_ (replicate n $ emit $ OpShiftR r r)

readMem :: Addr -> Reg -> (Reg -> Asm ()) -> Asm ()
readMem addr offset k = do
    setI addr
    increaseI offset
    readMemAtI k

readMemAtI :: (Reg -> Asm ()) -> Asm ()
readMemAtI k = do
    emit $ OpRestoreRegs (Reg 0)
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
    emit $ OpSaveRegs (Reg 0)

setI :: Addr -> Asm ()
setI addr = emit $ OpStoreI addr

increaseI :: Reg -> Asm ()
increaseI r = emit $ OpIncreaseI r

jumpOver :: Asm a -> Asm a
jumpOver asm = mdo
    jump q
    res <- asm
    q <- Here
    return res

emit :: Op -> Asm ()
emit op = Emit [b1,b2] where Instruction b1 b2 = encode op


----------------------------------------------------------------------
-- Asm

data Asm a where
    Ret :: a -> Asm a
    Bind :: Asm a -> (a -> Asm b) -> Asm b
    Mfix :: (a -> Asm a) -> Asm a
    Here :: Asm Addr
    Emit :: [Byte] -> Asm ()
    Later :: Asm a -> Asm a
    WithReg :: (Reg -> Asm a) -> Asm a

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = return; (<*>) = ap
instance Monad Asm where return = Ret; (>>=) = Bind
instance MonadFix Asm where mfix = Mfix

----------------------------------------------------------------------
-- assemble

assemble :: Asm () -> [Byte]
assemble asm = runEmitter baseProgram (assembleAsm asm)

type Emitter a = [Reg] -> Addr -> Addr -> (a,[Byte],[Byte])

runEmitter :: Addr -> Emitter () -> [Byte]
runEmitter q1 e = do
    let free = [Reg 1..Reg 14]
    let q2 = baseProgram `addAddr` length bs1
        ((),bs1,bs2) = e free q1 q2
    bs1++bs2

assembleAsm :: Asm a -> Emitter a
assembleAsm asm free q1 q2 = do
    case asm of
        Ret a -> (a,[],[])
        Bind m f -> do
            let (x,bs1,bs2) = assembleAsm m free q1 q2
            let q1' = q1 `addAddr` length bs1
            let q2' = q2 `addAddr` length bs2
            let (y,bs3,bs4) = assembleAsm (f x) free q1' q2'
            (y,bs1++bs3,bs2++bs4)
        Here -> (q1,[],[])
        Emit bs -> ((),bs,[])
        WithReg k ->
            case free of [] -> error "free=[]"; next:free -> assembleAsm (k next) free q1 q2
        Mfix f -> do
            let x@(a,_,_) = assembleAsm (f a) free q1 q2
            x
        Later m -> do
            let (x,bs1,bs2) = assembleAsm m free q2 q3
                q3 = q2 `addAddr` length bs1
            (x,[],bs1++bs2)
