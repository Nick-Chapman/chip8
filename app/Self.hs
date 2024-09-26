
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
  setWa slide (addrOfInt (addrToInt objLoadAddr - 0x200))
  setWa pc objLoadAddr
  setWa sp stackSpace

  next <- Here

  readPC
  checkControl control
  bumpPC
  opFX29
  op00EE
  op1NNN
  op2NNN
  opANNN
  -- TODO: FX1E (Add to Index)
  storeWide op templateOp
  switchObjectContext
  templateSetupIndex <- Here; setI 0
  -- TODO: init with skip-instruction for better dissaasembly
  templateOp <- Here; Emit [0x55,0x55]
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
      ifRegIs 0x00 oph $ do
        ifRegIs 0xEE opl $ do
          pullPCfromStack
          jump next

    op1NNN = do -- Jump
      let Wide oph _ = op
      setReg rTemp 0xF0
      inPlaceAnd rTemp oph
      ifRegIs 0x10 rTemp $ do
        setPC op
        jump next

    op2NNN = do -- Call
      let Wide oph _ = op
      setReg rTemp 0xF0
      inPlaceAnd rTemp oph
      ifRegIs 0x20 rTemp $ do
        pushPCtoStack
        setPC op
        jump next

    opANNN = do -- Set Index
      let Wide oph _ = op
      setReg rTemp 0xF0
      inPlaceAnd rTemp oph
      ifRegIs 0xA0 rTemp $ do
        addWide op slide
        storeWide op templateSetupIndex
        jump next

    opFX29 = do  -- Set Font Character
      let Wide oph opl = op
      setReg rTemp 0xF0
      inPlaceAnd rTemp oph
      ifRegIs 0xF0 rTemp $ do
        ifRegIs 0x29 opl $ do
          -- TODO: handle case where X gets changed before the sprite is drawn
          storeWide op templateSetupIndex
          jump next

  let
    bumpPC = do
      incWide pc
      incWide pc

    setPC op = do
      let Wide oph opl = op
      let Wide pch pcl = pc
      setReg mask 0x0F
      inPlaceAnd oph mask
      copyReg oph pch
      copyReg opl pcl
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
    setReg rTemp 0xA -- TODO: generalize which is the control key
    -- check if the control key A is pressed...
    emit (OpSkipKey rTemp)
    jump done
    showState
    -- wait until released
    setReg rTemp 0xA
    loop <- Here
    emit (OpSkipNotKey rTemp)
    jump loop
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

  setReg x 0
  setReg y 27
  let
    showNib r = do
      storeDigitSpriteI r
      draw 5 (x,y)
      incReg x 5

    showByte r = do
      copyReg r rTemp
      setReg mask 0xF0
      inPlaceAnd rTemp mask
      inPlaceShiftR 4 rTemp
      showNib rTemp
      copyReg r rTemp
      setReg mask 0x0F
      inPlaceAnd rTemp mask
      showNib rTemp

    showWide w = do
      let Wide hi lo = w
      showByte hi
      showByte lo

  -- slide PC back to be 0x200 relative (just for display)
  subWide pc slide
  showWide pc
  addWide pc slide

  -- TODO: show op code in right-hand-corner?
  incReg x 3
  showWide op


-- TODO: use registers with smallest numbers to reduce save/restore when switching context
allocateRegs :: R
allocateRegs = R
  { slide = Wide (Reg 4) (Reg 5)
  , sp = Wide (Reg 6) (Reg 7)
  , pc = Wide (Reg 8) (Reg 9)
  , op = Wide (Reg 10) (Reg 11)
  , mask = Reg 12
  , x = Reg 13
  , y = Reg 14
  }


storeWide :: Wide -> Addr -> Asm ()
storeWide w addr = do
  let Wide hi lo = w
  setI addr
  storeI hi
  setI (addr+1)
  storeI lo
