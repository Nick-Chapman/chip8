{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Data.List.Extra (upper)
import Data.Set (Set)
import Data.Word8 (Word8)
import Graphics.Gloss.Interface.IO.Game as Gloss
import Prelude hiding (pred)
import System.Environment (getArgs)
import qualified Data.ByteString as BS
import qualified Data.Set as Set

import Emulator(xmax,ymax,Byte,ChipState(..),Screen(..),ChipKeys,Nib(..))
import qualified Emulator as EM

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
theScale = 8

nonFullWindowPos :: (Int,Int)
nonFullWindowPos = (150,100)

----------------------------------------------------------------------

main :: IO ()
main = do
    args@Args{dump,file} <- parseCommandLine <$> getArgs
    progBytes <- readBytes file
    if dump
        then EM.printCode progBytes
        else
        do
            rands <- EM.randBytes
            runChip8 args rands progBytes

data Args = Args { full :: Bool, dump :: Bool, file :: FilePath }

parseCommandLine :: [String] -> Args -- very basic support!
parseCommandLine = \case
    [file] ->  Args { full = False, dump = False, file }
    [file,"--full"] ->  Args { full = True, dump = False, file }
    [file,"--dump"] ->  Args { full = False, dump = True, file }
    xs -> error $ show xs

readBytes :: FilePath -> IO [Byte]
readBytes path = do
    bs :: BS.ByteString <- BS.readFile path
    let ws :: [Word8] = BS.unpack bs
    let xs :: [Byte] = map EM.byteOfWord8 ws
    return xs

runChip8 :: Args -> [Byte] -> [Byte] -> IO ()
runChip8 Args{file,full} rands progBytes = do
    Gloss.playIO dis black fps model0
        pictureModel
        (handleEventModel model0)
        updateModel
    where
        model0 = initModel (EM.initCS rands progBytes)
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
                nib = EM.nibOfInt n
                reg = EM.Reg nib
                value = EM.regValue reg regs
                s1 = show reg
                s2 = show value

        picD = onLine 17 $ labBoxText "D" (show delay) 170
        picI = onLine 18 $ labBoxText "I" (show regI) 380
        picPC = onLine 19 $ labBoxText "PC" (show pc) 380

        picInstr = onLine 22 $ labBoxText "i" (show instr) 320
        picOp = translate 600 0 $ onLine 22 $ labBoxText "op" (show op) 1200

        instr = EM.instructionLookup pc mem
        op = EM.decode instr

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
    let cs1 = EM.step ck cs
    let nDelayTicks = fractionalModSeries nExec (delayTickHertz,ips)
    let cs2 = EM.tick nDelayTicks cs1
    cs2

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
