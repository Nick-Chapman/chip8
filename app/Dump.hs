module Dump (bytes) where

-- Chip8 program to display memory contents.

import Prelude hiding (break)
import Emulator
import Assemble

bytes :: [Byte]
bytes = assemble $ mdo

  let x = Reg 1
  let y = Reg 2
  let d = Reg 3
  let n = Reg 4
  let m = Reg 5
  let i = Reg 6
  let mask = Reg 7
  let one = Reg 8

  setLit x 0
  setLit y 0
  let
    showNib r = do
      storeDigitSpriteI r
      draw 5 (x,y)
      incReg x 5
      ifRegEq x 40 $ do
        setLit x 0
        incReg y 6
        ifRegEq y 24 $ do
          setLit y 0
          waitKeyH
          cls

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

  setLit n 0
  setLit m 0
  setLit one 1

  forever $ do
    setI 0x200
    increaseI n
    mdo
      setLit i 0
      jump start
      back <- Here
      setLit mask 0x80
      increaseI mask
      increaseI mask
      incrementReg i
      start <- Here
      emit $ OpSkipEq i m
      jump back
      pure ()
    incrementReg n
    emit $ OpSkipNotEqLit n 0
    incrementReg m
    readI d
    showByte d

waitKeyH :: Asm ()
waitKeyH = do
  emit $ OpWaitKeyPress rTemp
  loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
