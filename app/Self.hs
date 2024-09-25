
module Self (bytes) where -- A Chip8 interpreter coded in Chip8

import Prelude hiding (break)
import Assemble
import Emulator

debugState :: Bool
debugState = True

{-
_wait0 :: Asm () -- press a key to advance instruction
_wait0 = do
  waitKey

_wait1 :: Asm () -- press a key and release to advance instruction
_wait1 = do
  waitKey
  --compensates for bug in haskell chip8 emulator...
  loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop

_wait2 :: Asm () -- run freely when A is pressed
_wait2 = do
  setReg rTemp 0xA
  loop <- Here ; emit (OpSkipKey rTemp) ; jump loop

_wait3 :: Asm () -- run freely unless A is pressed
_wait3 = do
  setReg rTemp 0xA
  loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop

_checkControl :: Wide -> Wide -> Asm ()
_checkControl pc op = do
  if debugState then showState pc op else pure ()
  _wait0
  if debugState then showState pc op else pure ()
-}

checkControl :: Wide -> Wide -> Asm ()
checkControl pc op = mdo
  setReg rTemp 0xA

  -- check if the control key A is pressed...
  emit (OpSkipKey rTemp)
  jump done

  if debugState then showState pc op else pure ()
  -- wait until released
  setReg rTemp 0xA
  loop <- Here

  emit (OpSkipNotKey rTemp)
  jump loop
  if debugState then showState pc op else pure ()

  done <- Here
  pure ()


--panic :: Byte -> Asm ()
--panic b = Emit [0,b]

bytes :: [Byte]
bytes = assemble $ mdo

  -- trick dissembler to show object program
  --emit $ OpSkipNotEqLit rTemp 0
  -- even though we unconditionally jump to the intepreter
  --jump interpreterStart
  -- and never reach the jump to the object program
  --jump objectProgramAddr

  --interpreterStart <- Here
  interpreter objectProgramAddr
  --panic 0xA

  objectProgramAddr <- Here
  --Emit objectBytes
  --panic 0xB
  -- object program will be concatenated here
  pure ()


showState :: Wide -> Wide -> Asm ()
showState pc op = do
  let slide = Wide (Reg 4) (Reg 5)
  let mask = Reg 12
  let x = Reg 13
  let y = Reg 14
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

  let Wide pch pcl = pc
  let Wide oph opl = op

  -- slide PC to be 0x200 relative (just for display)
  subWide pc slide
  showByte pch
  showByte pcl
  addWide pc slide

  incReg x 3
  showByte oph
  showByte opl


interpreter :: Addr -> Asm ()
interpreter objLoadAddr = mdo
  let
    slide = Wide (Reg 4) (Reg 5)
    sp = Wide (Reg 6) (Reg 7)
    pc = Wide (Reg 8) (Reg 9)
    op = Wide (Reg 10) (Reg 11)
    mask = Reg 12

  -- object program was compiled to be run from 0x200
  -- but is installed here at a different place
  setWa slide (addrOfInt (addrToInt objLoadAddr - 0x200))

  let
    storeWide w addr = do
      let Wide hi lo = w
      setI addr
      storeI hi
      setI (addr+1)
      storeI lo

    bumpPC = do
      incWide pc
      incWide pc

    {-bumpSP = do
      incWide sp
      incWide sp

    unbumpSP = do
      decWide sp
      decWide sp-}

    setPC op = do
      let Wide oph opl = op
      let Wide pch pcl = pc
      setReg mask 0x0F
      inPlaceAnd oph mask
      copyReg oph pch
      copyReg opl pcl
      addWide pc slide

    {-incI = do
      setReg rTemp 1
      increaseI rTemp-}

    pushPCtoStack = do
      let Wide pch pcl = pc
      setIw sp
      storeI pch
      incWide sp
      --incI
      setIw sp
      storeI pcl
      incWide sp
      --bumpSP
      pure ()

    pullPCfromStack = do
      let Wide pch pcl = pc
      --unbumpSP
      decWide sp
      setIw sp
      readI pcl
      --incI
      decWide sp
      setIw sp
      readI pch
      pure ()

    readPC = do
      let Wide oph opl = op
      setIw pc
      readI oph
      incWide pc
      --incI
      setIw pc
      readI opl
      decWide pc

  let
    saveAllRegs = emit $ OpSaveRegs (Reg 15)
    restoreAllRegs = emit $ OpRestoreRegs (Reg 15)

    saveObjectRegs = do
      setI objBank
      saveAllRegs

    restoreObjectRegs = do
      setI objBank
      restoreAllRegs

    saveMetaRegs = do
      setI metaBank
      saveAllRegs

    restoreMetaRegs = do
      setI metaBank
      restoreAllRegs

    switchMetaContext = do
      saveObjectRegs
      restoreMetaRegs

    switchObjectContext = do
      saveMetaRegs
      restoreObjectRegs

  setWa pc objLoadAddr
  setWa sp stackSpace
  next <- Here

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
        storeWide op setupI
        jump next

    opFX29 = do  -- Set Font Character
      let Wide oph opl = op
      setReg rTemp 0xF0
      inPlaceAnd rTemp oph
      ifRegIs 0xF0 rTemp $ do
        ifRegIs 0x29 opl $ do
          --switchObjectContext -- not tried yet
          storeWide op setupI
          jump next

  readPC
  checkControl pc op
  bumpPC

  -- object program may use any regiser
  -- current I am good if they stick to low num regs: 1,2,3
  -- but maze uses 0 -- the temp needed for save/restore
  -- SO. need to preserve the object prog's value for r0
  -- between instructions

  opFX29
  op00EE
  op1NNN
  op2NNN
  opANNN
  storeWide op templateOp

  switchObjectContext

  -- Here we write an instruction for setting I
  -- need rethink to also support FX1E (Add To Index)
  setupI <- Here; setI 0 -- gets overwritten

  templateOp <- Here; Emit [0x55,0x55] -- and this

  jump noSkip
  switchMetaContext
  bumpPC
  jump next
  noSkip <- Here
  switchMetaContext
  jump next

  -- space to save obj/meta reg banks
  objBank <- Here ; Emit (replicate 16 0)
  metaBank <- Here ; Emit (replicate 16 0)
  stackSpace <- Here ; Emit (replicate 16 0) -- how big nesc?
  pure ()
  --Emit [0x77,0x77]
