
-- A Chip8 interpreter coded in Chip8

module Self (Control(..),bytes) where

import Prelude hiding (break)
import Assemble {-
  (assemble,Asm(Here,Later,Emit),panic
  ,insertSubroutineLater
  ,waitKey
  ,emit
  ,storeDigitSpriteI,draw
  ,setLit,setReg,incrementReg,incReg
  ,jump
  ,ifRegEq,ifRegNotEq,ifNotCarry
  ,setI,increaseI
  ,opAnd,opAdd,opShiftR
  ,rTemp,storeTemp,readTemp
  ,Wide(..),setWide,storeWide
  ,addWide,incWide,subWide,decWide
  ,setIw
  ,storeI,readI,readI2
  ) -}
import Emulator

data Control = NoControl | WithPause Byte | SingleStep

bytes :: Control -> [Byte]
bytes control = assemble $ mdo

  let R {maxClobber,maxPreserve
        ,slide,sp,pc,op,temp2,ophh,ophl} = allocateRegs

  let Wide pch pcl = pc
  let Wide oph opl = op

  -- The object program expects to be loaded at 0x200 , but is at a different location here
  -- but cause disassembly of object programm (but all the jumps are off)
  -- emit $ OpSkipEqLit rTemp 0
  -- jump objLoadAddr -- never taken

  let offset = addrOfInt (addrToInt objLoadAddr - 0x200)

  -- TODO: use restore for reg init

  setWide pc 0x200
  setWide sp stackSpace

  next <- Here
  readPC
  checkControl control
  bumpPC
  decodeOp
  opFX29
  opFX1E
  op00EE
  op1NNN
  op2NNN
  opANNN
  storeWide op templateOp
  switchObjectContext
  (templateSetPreLit,templateSetupIndex,templateSetPostLit) <-
    preservingTempOver $ do
    -- use 9s templates to show up better in disassembly
    t1 <- Here ; setLit rTemp 0x99
    t2 <- Here ; setI 0x999 -- replaced with either ANNN or FX29
    t3 <- Here ; setLit rTemp 0 ; increaseI rTemp
    return (t1,t2,t3)
  -- templateOp looks like a skip to improve disassembly
  templateOp <- Here; Emit [0x99,0x90]
  jump noSkip
  switchMetaContext
  bumpPC
  jump next
  noSkip <- Here
  switchMetaContext -- TODO: dedup?
  jump next

  let
    decodeOp = do
      setLit ophh 0xF0
      opAnd ophh oph
      setLit ophl 0x0F
      opAnd ophl oph

  let
    op00EE = do -- Return
      ifRegEq oph 0x00 $ do
        ifRegEq opl 0xEE $ do
          pullPCfromStack
          jump next

    op1NNN = do -- Jump
      ifRegEq ophh 0x10 $ do
        setPC
        jump next

    op2NNN = do -- Call
      ifRegEq ophh 0x20 $ do
        pushPCtoStack
        setPC
        jump next

    opANNN = do -- Set Index
      ifRegEq ophh 0xA0 $ do
        setWide slide offset
        addWide op slide
        storeWide op templateSetupIndex
        setLit rTemp 0
        setI (templateSetPostLit+1)
        storeTemp
        jump next

    -- TODO: share ophh test for FX29 and FX1E

    opFX29 = do  -- Set Font Character
      ifRegEq ophh 0xF0 $ do
        ifRegEq opl 0x29 $ do
          -- FX29 --> F029
          setLit rTemp 0xF0 ; setI templateSetupIndex ; storeTemp
          setLit rTemp 0x29 ; setI (templateSetupIndex+1) ; storeTemp
          readBankRegisterAsTemp objBank ophl
          setI (templateSetPreLit+1)
          storeTemp
          setLit rTemp 0
          setI (templateSetPostLit+1)
          storeTemp
          jump next

    opFX1E = do -- Add to Index
      ifRegEq ophh 0xF0 $ do
        ifRegEq opl 0x1E $ do

          -- We handle FX1E by incrementing the NNN in the ANNN instruction at templateSetupIndex.
          -- This assumes that Index was set by an ANNN instruction.

          -- But if Index was set by FX29, then incrementing will produce an illegal instruction.
          -- So instead we increment a literal (at templateSetPostLit) which is assigned to a register,
          -- given as an argument to an FX1E instruction placed after the templateSetupIndex.

          -- Note any chip program which performs FX29 followed by FX1E is dancing on very thin ice,
          -- as it assumes how and where the hex-font is laid out in memory.

          setI templateSetupIndex
          readTemp
          -- TODO: test for 0xF0 special case; and avoid mask
          setLit temp2 0xF0
          opAnd rTemp temp2
          ifRegNotEq rTemp 0xA0 $ do
            readBankRegisterAsTemp objBank ophl
            setReg temp2 rTemp
            setI (templateSetPostLit+1)
            readTemp
            opAdd rTemp temp2
            setI (templateSetPostLit+1)
            storeTemp
            jump next

          readBankRegisterAsTemp objBank ophl
          setReg temp2 rTemp

          setI (templateSetupIndex+1)
          readTemp
          opAdd rTemp temp2
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

    setPC = do
      setReg pch ophl
      setReg pcl opl

    -- TODO: setIw as subroutine

    pushPCtoStack = do
      setIw sp
      storeI2 pc
      incWide sp
      incWide sp

    pullPCfromStack = do
      decWide sp
      decWide sp
      setIw sp
      readI2 pc

    readPC = do
      setWide slide offset
      addWide slide pc
      setIw slide
      readI2 op

    saveRegs n = emit $ OpSaveRegs (Reg n)
    restoreRegs n = emit $ OpRestoreRegs (Reg n)

    switchMetaContext = do
      setI objBank
      saveRegs maxClobber
      setI metaBank
      restoreRegs maxPreserve

    switchObjectContext = do
      setI metaBank
      saveRegs maxPreserve
      setI objBank
      restoreRegs maxClobber

  -- 16 levels of subroutine nesting
  stackSpace <- Here ; Emit (replicate 32 0)

  -- Space to save obj/meta reg banks
  objBank <- Here ; Emit (replicate 16 0)
  metaBank <- Here ; Emit (replicate (1+nibToInt maxPreserve) 0)

  objLoadAddr <- Later Here
  pure ()


checkControl :: Control -> Asm ()
checkControl = \case

  NoControl -> mdo
    pure ()

  WithPause keycode -> mdo

    showState <- showStateSR

    -- simple "touch and hold Z" for pause
    setLit rTemp keycode
    emit (OpSkipKey rTemp) -- pause key pressed?
    jump done
    showState
    setLit rTemp keycode
    do loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop -- wait until released
    -- "click-on/click-off" implemented by adding the following two lines:
    -- do loop <- Here ; emit (OpSkipKey rTemp) ; jump loop
    -- do loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
    showState
    done <- Here
    pure ()

  SingleStep -> mdo
    showState <- showStateSR

    showState
    -- press a key and release to advance instruction
    waitKey
    --compensates for bug in haskell chip8 emulator...
    --loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
    showState


showStateSR :: Asm (Asm ())
showStateSR = insertSubroutineLater $ do

  let R {pc,op,x,y,nib} = allocateRegs

  setLit x 0
  setLit y 27

  showNib <- insertSubroutineLater $ do
    storeDigitSpriteI nib
    draw 5 (x,y)
    incReg x 5

  showByte <- insertSubroutineLater $ do
      setLit nib 0xF0
      opAnd nib rTemp
      opShiftR 4 nib
      showNib
      setLit nib 0x0F
      opAnd nib rTemp
      showNib

  let
    showWide w = do
      let Wide hi lo = w
      setReg rTemp hi
      showByte
      setReg rTemp lo
      showByte

  showWide pc
  incReg x 25
  showWide op


data R = R { maxClobber, maxPreserve :: Nib, slide,sp,pc,op :: Wide , temp2,x,y,nib,ophh,ophl :: Reg }

allocateRegs :: R
allocateRegs = R
  { maxPreserve = 5
  , maxClobber = 15
  , temp2 = Reg 1
  , sp = Wide (Reg 2) (Reg 3)
  , pc = Wide (Reg 4) (Reg 5)

  , slide = Wide (Reg 6) (Reg 7)
  , op = Wide (Reg 8) (Reg 9)
  , ophh = Reg 10
  , ophl = Reg 11
  , x = Reg 12
  , y = Reg 13
  , nib = Reg 14
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
