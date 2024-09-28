
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
  templateSetupIndex <- Here; setI 0
  templateOp <- Here; emit (OpSkipEq (Reg 5) (Reg 5))
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
          -- TODO: handle case where X gets changed before the sprite is drawn
          storeWide op templateSetupIndex
          jump next

    opFX1E = do -- Add to Index
      let Wide oph opl = op
      setLit rTemp 0xF0
      opAnd rTemp oph
      ifRegEq rTemp 0xF0 $ do
        ifRegEq opl 0x1E $ do
          setLit mask 0x0F
          opAnd oph mask
          setI objBank
          increaseI oph
          readTemp
          setReg mask rTemp -- using mask as 2nd temp

          -- We handle FX1E by incrementing the NNN in the ANNN instruction at templateSetupIndex.
          -- This assumes that Index was set by an ANNN instruction.

          -- But if Index was set by FX29, then incrementing will produce an illegal instruction.
          -- TODO: Make an example for this case. Detect it. Do something better than crash!

          -- Note any chip program which performs FX29 followed by FX1E is dancing on very thin ice,
          -- as it assumes how and where the hex-font is laid out in memory.

          setI (templateSetupIndex+1)
          readTemp
          opAdd rTemp mask
          -- TODO: should deal with carry into lo-nibble of hi-byte. (bug in Pi example?)
          -- ifCarry (panic 0x2) -- TODO : wip
          setI (templateSetupIndex+1)
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

  -- TODO: how much space is needed for the stack?
  stackSpace <- Here ; Emit (replicate 16 0)

  objLoadAddr <- Here
  pure ()


checkControl :: Control -> Asm ()
checkControl = \case

  NoControl -> mdo
    pure ()

  WithPause -> mdo
    setLit rTemp 0xA -- TODO: generalize which is the control key
    -- check if the control key A is pressed...
    emit (OpSkipKey rTemp)
    jump done
    showState
    setLit rTemp 0xA
    -- wait until released
    do loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
    -- and pressed and released again
    -- TODO: reinstate the simpler "touch and hold Z" for pause
    -- instead of the click-on/click-off implemented in the following two lines
    do loop <- Here ; emit (OpSkipKey rTemp) ; jump loop
    do loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
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
