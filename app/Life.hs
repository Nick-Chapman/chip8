{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Life (bytes) where

import Prelude hiding (break)
import Assemble
import Emulator (incAddr)

----------------------------------------------------------------------
-- parameters
xfrag :: Xfrag
yfrag :: Yfrag

xfrag = Xall
--xfrag = Xhalf -- good
--xfrag = Xquarter
--xfrag = Xeighth

yfrag = Yall
--yfrag = Yhalf
--yfrag = Yquarter

data Xfrag = Xall | Xhalf | Xquarter | Xeighth
data Yfrag = Yall | Yhalf | Yquarter

----------------------------------------------------------------------
-- bytes

bytes :: [Byte]
bytes = assemble asm

asm :: Asm ()
asm = do
    p2tab <- insertBytes [128,64,32,16,8,4,2,1]

    -- write the byte stream to a file. try in other emulators!
    -- TODO: insert text with info about me
    --aGlider <- insertBytes gliderData
    aGosper <- insertBytes gosperData

    let a1 = 0xE00 -- how to auto pick these to be after all code?
    let a2 = 0xF00

    --copyMemBytes (length gliderData) aGlider a1
    -- repeated calls to copyMemBytes duplicates code...
    let aStart = a1 `incAddr` 1
    copyMemBytes 9  aGosper                   aStart
    copyMemBytes 9 (aGosper `incAddr`    9)  (aStart `incAddr`    sizeY)
    copyMemBytes 9 (aGosper `incAddr` (2*9)) (aStart `incAddr` (2*sizeY))
    copyMemBytes 9 (aGosper `incAddr` (3*9)) (aStart `incAddr` (3*sizeY))
    copyMemBytes 9 (aGosper `incAddr` (4*9)) (aStart `incAddr` (4*sizeY))

    forever $ do
        blatRectToScreen a1
        zeroBytes a2 screenSizeInBytes
        lifeStep p2tab a1 a2
        copyMemBytes screenSizeInBytes a2 a1
        --break
        cls

    crash

gosperData :: [Byte]
gosperData = [
    0x00, 0x00, 0x00, 0x00, 0xC0, 0xC0, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x0C, 0x11, 0x20, 0x22, 0x20, 0x11, 0x0C,
    0x00, 0x02, 0x0C, 0x0C, 0x8C, 0xC2, 0x80, 0x00, 0x00,
    0x80, 0x80, 0x00, 0x00, 0x00, 0x80, 0x80, 0x00, 0x00,
    0x00, 0x00, 0x30, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00] -- alignment only necessary for dissassembler

_gliderData :: [Byte]
_gliderData =
{-
........
...x.... 0x10
.x.x.... 0x50
..xx.... 0x30
-}
    [0,0x10,0x50,0x30]

sizeX :: Int
sizeX = case xfrag of
    Xall -> 64
    Xhalf -> 32
    Xquarter -> 16
    Xeighth -> 8

sizeY :: Int
sizeY = case yfrag of
    Yall -> 32
    Yhalf -> 16
    Yquarter -> 8

screenSizeInBytes :: Int
screenSizeInBytes = sizeX * sizeY `div` 8

maskXv :: Byte
maskXv = fromIntegral $ sizeX - 1

maskYv :: Byte
maskYv = fromIntegral $ sizeY - 1

xyToOffsetBitnum :: (Reg,Reg) -> (Reg -> Reg -> Asm ()) -> Asm ()
xyToOffsetBitnum (x,y) k = do
    xHigh x $ \xH -> do
        add xH y $ \offset -> do
            mod8 x $ \bitnum -> do
                k offset bitnum

xHigh :: Reg -> (Reg -> Asm a) -> Asm a
xHigh = case yfrag of
    Yall ->  div8mul32
    Yhalf ->  div8mul16
    Yquarter -> div8mul8

stripesFragX,screenFragY :: Int
stripesFragX = sizeX `div` 8
screenFragY = sizeY

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

lifeStep :: Addr -> Addr -> Addr -> Asm ()
lifeStep p2tab a1 a2 = do
    loopFor (0,sizeX) $ \x -> do
        loopFor (0,sizeY) $ \y -> do
            lifeStepCell p2tab a1 (x,y) a2

lifeStepCell :: Addr -> Addr -> (Reg,Reg) -> Addr -> Asm ()
lifeStepCell p2tab a (x,y) a2 = do
    withInitReg 0 $ \currentlyOn -> do
        withInitReg 0 $ \neigbourCount -> do
            -- do we need this extra register copying?
            readCell p2tab a (x,y) $ \v ->
                copyReg v currentlyOn
            countNeighbours p2tab (a,x,y) $ \c ->
                copyReg c neigbourCount
            ifRemainAliveOrBeBorn currentlyOn neigbourCount $
                setCell p2tab a2 (x,y)

ifRemainAliveOrBeBorn :: Reg -> Reg -> Asm () -> Asm ()
ifRemainAliveOrBeBorn on count act = do
    withInitReg 0 $ \flag -> do
        let setFlag = setReg flag 1
        ifRegIs 3 count $ setFlag
        ifRegNotZero on $ ifRegIs 2 count $ setFlag
        ifRegIs 1 flag $ act

countNeighbours :: Addr -> (Addr,Reg,Reg) -> (Reg -> Asm ()) -> Asm ()
countNeighbours p2tab (a,x,y) k = do
    withInitReg 0 $ \count -> do
        foreachCellNeigbour (x,y) $ do
            -- the body of this foreach is duplicated 8 times. can we share? call/ret
            readCell p2tab a (x,y) $ \v -> do
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

readCell :: Addr -> Addr -> (Reg,Reg) -> (Reg -> Asm ()) -> Asm ()
readCell p2tab addr (x,y) k = do
    xyToOffsetBitnum (x,y) $ \offset bitnum -> do
        readMem addr offset $ \loaded -> do
            bitnum2mask p2tab bitnum $ \mask -> do
                logicalAnd mask loaded k

setCell :: Addr -> Addr -> (Reg,Reg) -> Asm ()
setCell p2tab addr (x,y) = do
    xyToOffsetBitnum (x,y) $ \offset bitnum -> do
        readMem addr offset $ \loaded -> do
            bitnum2mask p2tab bitnum $ \mask -> do
                logicalOr mask loaded $ \updated ->
                    writeMem addr offset updated

bitnum2mask :: Addr -> Reg -> (Reg -> Asm ()) -> Asm () -- couting bits from 0..7
bitnum2mask p2tab r k = do
    readMem p2tab r $ \loaded -> do
        k loaded

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
    withInitReg 0xF8 $ \mask -> do
        inPlaceAnd mask x
        inPlaceShiftL 2 mask
        k mask

div8mul16 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul16 x k = do
    withInitReg 0xF8 $ \mask -> do
        inPlaceAnd mask x
        inPlaceShiftL 1 mask
        k mask

div8mul8 :: Reg -> (Reg -> Asm a) -> Asm a
div8mul8 x k = do
    withInitReg 0xF8 $ \mask -> do
        inPlaceAnd mask x
        k mask

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
