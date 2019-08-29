{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Life (bytes) where

import Prelude hiding (break)
import Assemble

----------------------------------------------------------------------
-- parameters
xfrag :: Xfrag
yfrag :: Yfrag

--xfrag = Xall -- error: byteOfInt: 512
--xfrag = Xhalf -- good
xfrag = Xquarter
yfrag = Yquarter


data Xfrag = Xall | Xhalf | Xquarter | Xeighth
data Yfrag = Yall | Yhalf | Yquarter


----------------------------------------------------------------------
-- bytes

bytes :: [Byte]
bytes = assemble asm

asm :: Asm ()
asm = do

    -- write the byte stream to a file. try in other emulators!
    -- TODO: insert text with info about me
    aGlider <- insertBytes gliderData

    let a1 = 0x700 -- how to auto pick these to be after all code?
    let a2 = 0x800

    copyMemBytes (length gliderData) aGlider 0x700
    --writeDigitToMem 2 0x708

    forever $ do
        blatRectToScreen a1
        zeroBytes a2 screenSizeInBytes
        lifeStep a1 a2
        copyMemBytes screenSizeInBytes a2 a1
        --break
        cls

    crash


gliderData :: [Byte]
gliderData =
{-
........
...x.... 0x10
.x.x.... 0x50
..xx.... 0x30
-}
    [0,0x10,0x50,0x30]

rectMaxX :: Int
rectMaxX = case xfrag of
    Xall -> 64
    Xhalf -> 32
    Xquarter -> 16
    Xeighth -> 8

rectMaxY :: Int
rectMaxY = case yfrag of
    Yall -> 32
    Yhalf -> 16
    Yquarter -> 8

screenSizeInBytes :: Int
screenSizeInBytes = rectMaxX * rectMaxX `div` 8

maskXv :: Byte
maskXv = fromIntegral $ rectMaxX - 1

maskYv :: Byte
maskYv = fromIntegral $ rectMaxY - 1

xyToOffsetBitnum :: (Reg,Reg) -> (Reg -> Reg -> Asm ()) -> Asm ()
xyToOffsetBitnum (x,y) k = do
    xHigh x $ \xH -> do
        add xH y $ \offset -> do
            mod8 x $ \bitnum -> do
                k offset bitnum

xHigh :: Reg -> (Reg -> Asm a) -> Asm a
xHigh = case yfrag of
    Yall ->  div8mul32
    Yhalf ->  div8mul16 -- half-height screen fragment
    Yquarter -> div8mul8 -- quarter-height screen fragment

stripesFragX,screenFragY :: Int
stripesFragX = rectMaxX `div` 8
screenFragY = rectMaxY

blatRectToScreen :: Addr -> Asm ()
blatRectToScreen addr = do
    setI addr
    withInitReg 1 $ \one -> do
        loopFor (0,stripesFragX) $ \xStripe -> do
            mul8 xStripe $ \x -> do
                loopFor (0,screenFragY) $ \y -> do
                    draw 1 (x,y)
                    increaseI one
                    return ()

lifeStep :: Addr -> Addr -> Asm ()
lifeStep a1 a2 = do
    loopFor (0,rectMaxX) $ \x -> do
        loopFor (0,rectMaxY) $ \y -> do
            lifeStepCell a1 (x,y) a2

lifeStepCell :: Addr -> (Reg,Reg) -> Addr -> Asm ()
lifeStepCell a (x,y) a2 = do
    withInitReg 0 $ \currentlyOn -> do
        withInitReg 0 $ \neigbourCount -> do
            -- do we need this extra register copying?
            readCell a (x,y) $ \v ->
                copyReg v currentlyOn
            countNeighbours (a,x,y) $ \c ->
                copyReg c neigbourCount
            ifRemainAliveOrBeBorn currentlyOn neigbourCount $
                setCell a2 (x,y)

ifRemainAliveOrBeBorn :: Reg -> Reg -> Asm () -> Asm ()
ifRemainAliveOrBeBorn on count act = do
    withInitReg 0 $ \flag -> do
        let setFlag = setReg flag 1
        ifRegIs 3 count $ setFlag
        ifRegNotZero on $ ifRegIs 2 count $ setFlag
        ifRegIs 1 flag $ act

countNeighbours :: (Addr,Reg,Reg) -> (Reg -> Asm ()) -> Asm ()
countNeighbours (a,x,y) k = do
    withInitReg 0 $ \count -> do
        foreachCellNeigbour (x,y) $ do
            -- the body of this foreach is duplicated 8 times. can we share? call/ret
            readCell a (x,y) $ \v -> do
                ifRegNotZero v $
                    incrementReg count
        k count

foreachCellNeigbour :: (Reg,Reg) -> Asm () -> Asm ()
foreachCellNeigbour (x,y) act = do
    withInitReg maskXv $ \maskX -> do
        withInitReg maskYv $ \maskY -> do
            let wrapX = inPlaceAnd x maskX
            let wrapY = inPlaceAnd y maskY
            incrementReg x; wrapX; act
            incrementReg y; wrapY; act
            decrementReg x; wrapX; act
            decrementReg x; wrapX; act
            decrementReg y; wrapY; act
            decrementReg y; wrapY; act
            incrementReg x; wrapX; act
            incrementReg x; wrapX; act
            decrementReg x; wrapX
            incrementReg y; wrapY

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

bitnum2mask :: Reg -> (Reg -> Asm ()) -> Asm () -- couting bits from 0..7
bitnum2mask r k = do
    withInitReg 128 $ \res -> do
        ifRegNotZero r $ do
            loopForR (0,r) $ \_ -> do
                inPlaceShiftR 1 res
        k res

zeroBytes :: Addr -> Int -> Asm ()
zeroBytes a n = do
    withInitReg 0 $ \zero -> do
        loopFor (0, fromIntegral n) $ \offset -> do
            writeMem a offset zero

copyMemBytes :: Int -> Addr -> Addr -> Asm ()
copyMemBytes n a1 a2 = do
    loopFor (0, fromIntegral n) $ \offset -> do
        readMem a1 offset $ \v -> do
            writeMem a2 offset v

mod8 :: Reg -> (Reg -> Asm a) -> Asm a
mod8 x k = do
    withInitReg 0x7 $ \mask -> do
        inPlaceAnd mask x
        k mask

mul8 :: Reg -> (Reg -> Asm a) -> Asm a
mul8 x k = do
    copy x $ \y -> do
        inPlaceShiftL 3 y
        k y

div8mul32 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul32 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        inPlaceShiftR 3 y
        inPlaceShiftL 5 y
        k y

div8mul16 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul16 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        inPlaceShiftR 3 y
        inPlaceShiftL 4 y
        k y

div8mul8 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul8 x k = do
    copy x $ \y -> do
        -- can reduce the number of instructions here
        inPlaceShiftR 3 y
        inPlaceShiftL 3 y
        k y

logicalAnd :: Reg -> Reg -> (Reg -> Asm a) -> Asm a
logicalAnd x y k = do
    copy x $ \res -> do
        inPlaceAnd res y
        k res

logicalOr :: Reg -> Reg -> (Reg -> Asm a) -> Asm a
logicalOr x y k = do
    copy x $ \res -> do
        inPlaceOr res y
        k res

add :: Reg -> Reg -> (Reg -> Asm a) -> Asm a
add a b k = do
    copy a $ \res -> do
        inPlaceAdd res b
        k res
