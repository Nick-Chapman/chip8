module Bits where

{-myAsm :: Asm ()
myAsm = do
    switchRegZero (Reg 0) (
        do
            copyReg (Reg 1) (Reg 2)) (
        do
            copyReg (Reg 3) (Reg 4))
    crash
-}

{-
myAsm :: Asm ()
myAsm = do
    WithReg $ \on -> do
        WithReg $ \count -> do
            ifRemainAliveOrBeBorn on count $ jump 1
    jump 2
-}

{-
myAsm :: Asm ()
myAsm = do
    let a1 = 0x700
    let a2 = 0x800
    --WithReg $ \x -> do WithReg $ \y -> do lifeStepCell a1 (x,y) a2
    lifeStep a1 a2
    crash
    --halt
-}


{-
_old_xyToOffsetBitnum :: (Reg,Reg) -> (Reg -> Reg -> Asm ()) -> Asm ()
_old_xyToOffsetBitnum (x,y) k =
    mul8 y $ \y8 -> do
        div8 x $ \xH -> do
            add xH y8 $ \offset -> do
                mod8 x $ \bitnum -> do
                    k offset bitnum
-}


{-
_writeDigitToMem :: Int -> Addr -> Asm ()
_writeDigitToMem n addr = do
    let a = fromIntegral $ 5*n -- from emulator embedding at 0,5,10...
    copyMemBytes 5 a addr
-}


{-
copyRectByPixel :: Addr -> Addr -> Asm ()
copyRectByPixel a b = do
    loopUpto rectMaxX $ \x -> do
        loopUpto rectMaxY $ \y -> do
            copyCell (a,x,y) (b,x,y)
-}

{-
lifeStep_dummy :: Addr -> Addr -> Asm ()
lifeStep_dummy = copyRectByPixel_shiftedRD

copyRectByPixel_shiftedRD :: Addr -> Addr -> Asm ()
copyRectByPixel_shiftedRD a b = do
    loopUpto rectMaxX $ \x -> do
        loopUpto rectMaxY $ \y -> do
            incMod16 x $ \x' -> do
                incMod8 y $ \y' -> do -- run out of regs!
                    copyCell (a,x,y) (b,x',y')
-}


{-
copyCell :: (Addr,Reg,Reg) -> (Addr,Reg,Reg) -> Asm ()
copyCell (a1,x1,y1) (a2,x2,y2) = do
    WithReg $ \hold -> do
        readCell a1 (x1,y1) $ \v -> do
            copyReg v hold
        ifRegNotZero hold $
            setCell a2 (x2,y2)
-}

{-
_lifeStep :: Addr -> Addr -> Asm () -- see behav for 1 cell
_lifeStep a1 a2 = do
    withInitReg 0x3 $ \x -> do
        withInitReg 0x3 $ \y -> do
            lifeStepCell a1 (x,y) a2
-}


{-
switchRegZero :: Reg -> Asm () -> Asm () -> Asm ()
switchRegZero r case0 caseNZ = do
    Emit $ OpSkipEqLit r 0
    Switch case0 caseNZ
-}


{-
loop00toFF ::  (Reg -> Asm ()) -> Asm ()
loop00toFF k = do
    withInitReg 0 $ \x -> do
        forever $ do
            k x
            incrementReg x
            Emit $ OpSkipEqLit x 0
-}



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



{-
writePatToMem :: Addr -> Asm ()
writePatToMem addr = do
    Emit $ OpStoreI addr
    withInitReg 1 $ \one -> do
        loop00toFF $ \n -> do
            writeMemAtI n
            Emit $ OpIncreaseI one
            return ()
-}

{-
writePatToScreen :: Asm ()
writePatToScreen = do
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



{-
mul2 :: Reg -> (Reg -> Asm a) -> Asm a
mul2 x k = do
    copy x $ \y -> do
        lshift 1 y
        k y
-}

{-
div8 :: Reg -> (Reg -> Asm a) -> Asm a
div8 x k = do
    copy x $ \y -> do
        rshift 3 y
        k y
-}


{-
incMod16 :: Reg -> (Reg -> Asm ()) -> Asm ()
incMod16 r k = do
    copy r $ \s -> do
        incrementReg s
        mod16 s $ \t -> do
            k t

incMod8 :: Reg -> (Reg -> Asm ()) -> Asm ()
incMod8 r k = do
    copy r $ \s -> do
        incrementReg s
        mod8 s $ \t -> do
            k t
-}

{-
mod16 :: Reg -> (Reg -> Asm a) -> Asm a
mod16 x k = do
    withInitReg 0xF $ \mask -> do
        --logicalAnd x mask k
        Emit $ OpAnd mask x -- in place
        k mask
-}



{-
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
-}

{-
waitKey :: Nib -> Asm ()
waitKey nib = do waitKeyUp nib; waitKeyDown nib
-}

{-
waitKeyDown :: Nib -> Asm ()
waitKeyDown nib = do
   WithReg $ \r -> do
    Emit $ OpStoreLit r (byteOfNibs 0 nib)
    forever $ Emit $ OpSkipKey r
-}


{-
_drawDig :: Reg -> (Reg,Reg) -> Asm ()
_drawDig num (x,y) = do
    Emit $ OpStoreDigitSpriteI num
    draw 5 (x,y)
-}


{-
_bitnum2mask :: Addr -> Reg -> (Reg -> Asm ()) -> Asm () -- couting bits from 0..7
_bitnum2mask _tab r k = do
    withInitReg 128 $ \res -> do
        ifRegNotZero r $ do
            loopForR (0,r) $ \_ -> do
                inPlaceShiftR 1 res
        k res
-}

{-
_div8mul32 :: Reg -> (Reg -> Asm a) -> Asm a
_div8mul32 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        inPlaceShiftR 3 y
        inPlaceShiftL 5 y
        k y
-}

{-
_div8mul16 :: Reg -> (Reg -> Asm a) -> Asm a
_div8mul16 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        inPlaceShiftR 3 y
        inPlaceShiftL 4 y
        k y
-}
{-
_div8mul8 :: Reg -> (Reg -> Asm a) -> Asm a
_div8mul8 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        inPlaceShiftR 3 y
        inPlaceShiftL 3 y
        k y
-}
