
-- A Chip8 interpreter coded in Chip8

module Self (Control(..),bytes) where

import Prelude hiding (break)
import Assemble
import Emulator

data Control = NoControl | WithPause | SingleStep

data R = R { slide,sp,pc,op :: Wide , mask,x,y :: Reg }

bytes :: Control -> [Byte]
bytes control = assemble $ mdo

  let R {slide,sp,pc,op,mask} = allocateRegs

  -- The object program expects to be loaded at 0x200 , but is at a different location here
  -- but cause disassembly of object programm (but all the jumps are off)
  -- emit $ OpSkipEqLit rTemp 0
  -- jump objLoadAddr -- never taken

  setWide slide (addrOfInt (addrToInt objLoadAddr - 0x200))
  setWide pc objLoadAddr
  setWide sp stackSpace

  next <- Here

  readPC
  checkControl control
  bumpPC
  opFX29
  opFX1E
  op00EE
  op1NNN
  op2NNN
  opANNN
  storeWide op templateOp
  switchObjectContext
  (templateSetTemp,templateSetupIndex) <-
    preservingTempOver $ do
    -- use 9s templates to show up better in disassembly
    t1 <- Here ; setLit rTemp 0x99
    t2 <- Here ; setI 0x999
    return (t1,t2)
  templateOp <- Here; Emit [0x99,0x90] -- looks like a skip to improve disassembly
  jump noSkip
  switchMetaContext
  bumpPC
  jump next
  noSkip <- Here
  switchMetaContext
  jump next

  let
    op00EE = do -- Return
      let Wide oph opl = op
      ifRegEq oph 0x00 $ do
        ifRegEq opl 0xEE $ do
          pullPCfromStack
          jump next

    op1NNN = do -- Jump
      let Wide oph _ = op
      setLit rTemp 0xF0
      opAnd rTemp oph
      ifRegEq rTemp 0x10 $ do
        setPC op
        jump next

    op2NNN = do -- Call
      let Wide oph _ = op
      setLit rTemp 0xF0
      opAnd rTemp oph
      ifRegEq rTemp 0x20 $ do
        pushPCtoStack
        setPC op
        jump next

    opANNN = do -- Set Index
      let Wide oph _ = op
      setLit rTemp 0xF0
      opAnd rTemp oph
      ifRegEq rTemp 0xA0 $ do
        addWide op slide
        storeWide op templateSetupIndex
        jump next

    opFX29 = do  -- Set Font Character
      let Wide oph opl = op
      setLit rTemp 0xF0
      opAnd rTemp oph
      ifRegEq rTemp 0xF0 $ do
        ifRegEq opl 0x29 $ do
          -- FX29 --> F029
          setLit rTemp 0xF0 ; setI templateSetupIndex ; storeTemp
          setLit rTemp 0x29 ; setI (templateSetupIndex+1) ; storeTemp
          setLit mask 0x0F
          opAnd oph mask
          readBankRegisterAsTemp objBank oph
          setI (templateSetTemp+1)
          storeTemp
          jump next

    opFX1E = do -- Add to Index
      let Wide oph opl = op
      setLit rTemp 0xF0
      opAnd rTemp oph
      ifRegEq rTemp 0xF0 $ do
        ifRegEq opl 0x1E $ do

          -- We handle FX1E by incrementing the NNN in the ANNN instruction at templateSetupIndex.
          -- This assumes that Index was set by an ANNN instruction.

          -- But if Index was set by FX29, then incrementing will produce an illegal instruction.
          -- Current we just detect this will happen, and panic insead
          -- TODO: Make an example for this case. Do something better than panic!

          -- Note any chip program which performs FX29 followed by FX1E is dancing on very thin ice,
          -- as it assumes how and where the hex-font is laid out in memory.

          setI templateSetupIndex
          readTemp
          setLit mask 0xF0
          opAnd rTemp mask
          ifRegNotEq rTemp 0xA0 $ panic 0x1

          setLit mask 0x0F
          opAnd oph mask
          readBankRegisterAsTemp objBank oph
          setReg mask rTemp -- using mask as 2nd temp

          setI (templateSetupIndex+1)
          readTemp
          opAdd rTemp mask
          setI (templateSetupIndex+1)
          storeTemp
          ifNotCarry $ jump next

          setI templateSetupIndex
          readTemp
          incrementReg rTemp
          -- dont allow AF-->B0; wrap back to A0
          ifRegEq rTemp 0xB0 $ setLit rTemp 0xA0
          setI templateSetupIndex
          storeTemp

          jump next

  let
    bumpPC = do
      incWide pc
      incWide pc

    setPC op = do
      let Wide oph opl = op
      let Wide pch pcl = pc
      setLit mask 0x0F
      opAnd oph mask
      setReg pch oph
      setReg pcl opl
      addWide pc slide

    pushPCtoStack = do
      let Wide pch pcl = pc
      setIw sp
      storeI pch
      incWide sp
      setIw sp
      storeI pcl
      incWide sp
      pure ()

    pullPCfromStack = do
      let Wide pch pcl = pc
      decWide sp
      setIw sp
      readI pcl
      decWide sp
      setIw sp
      readI pch
      pure ()

    readPC = do
      let Wide oph opl = op
      setIw pc
      readI oph
      incWide pc
      setIw pc
      readI opl
      decWide pc

    saveAllRegs = emit $ OpSaveRegs (Reg 15)
    restoreAllRegs = emit $ OpRestoreRegs (Reg 15)

    -- TODO: reduce registers saved/restored when switching context
    switchMetaContext = do
      setI objBank
      saveAllRegs
      setI metaBank
      restoreAllRegs

    switchObjectContext = do
      setI metaBank
      saveAllRegs
      setI objBank
      restoreAllRegs

  -- Space to save obj/meta reg banks
  objBank <- Here ; Emit (replicate 16 0)
  metaBank <- Here ; Emit (replicate 16 0)

  stackSpace <- Here ; Emit (replicate 32 0) -- 16 levels of nesting

  objLoadAddr <- Here
  pure ()


checkControl :: Control -> Asm ()
checkControl = \case

  NoControl -> mdo
    pure ()

  WithPause -> mdo
    -- simple "touch and hold Z" for pause
    setLit rTemp 0xA
    emit (OpSkipKey rTemp) -- pause key pressed?
    jump done
    showState
    setLit rTemp 0xA
    do loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop -- wait until released
    -- "click-on/click-off" implemented by adding the following two lines:
    -- do loop <- Here ; emit (OpSkipKey rTemp) ; jump loop
    -- do loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
    showState
    done <- Here
    pure ()

  SingleStep -> mdo
    showState
    -- press a key and release to advance instruction
    waitKey
    --compensates for bug in haskell chip8 emulator...
    --loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
    showState


-- TODO: first code to push into subroutine
showState :: Asm ()
showState = do

  let R {slide,pc,op,mask,x,y} = allocateRegs

  setLit x 0
  setLit y 27
  let
    showNib r = do
      storeDigitSpriteI r
      draw 5 (x,y)
      incReg x 5

    showByte r = do
      setReg rTemp r
      setLit mask 0xF0
      opAnd rTemp mask
      opShiftR 4 rTemp
      showNib rTemp
      setReg rTemp r
      setLit mask 0x0F
      opAnd rTemp mask
      showNib rTemp

    showWide w = do
      let Wide hi lo = w
      showByte hi
      showByte lo

  -- slide PC back to be 0x200 relative (just for display)
  subWide pc slide
  showWide pc
  addWide pc slide

  incReg x 25
  showWide op


allocateRegs :: R
allocateRegs = R
  { slide = Wide (Reg 1) (Reg 2)
  , sp = Wide (Reg 3) (Reg 4)
  , pc = Wide (Reg 5) (Reg 6)
  , op = Wide (Reg 7) (Reg 8)
  , mask = Reg 9
  , x = Reg 10
  , y = Reg 11
  }

preservingTempOver :: Asm a -> Asm a
preservingTempOver asm = mdo
  setI (after+1)
  storeTemp
  res <- asm
  after <- Here
  setLit rTemp 0
  pure res


readBankRegisterAsTemp :: Addr -> Reg -> Asm ()
readBankRegisterAsTemp bank r = do
  setI bank
  increaseI r
  readTemp

--bad :: Asm ()
--bad = Emit [0x0B, 0xAD]
