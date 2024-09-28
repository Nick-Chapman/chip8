module Evens (bytes) where

import Prelude hiding (break)
import Assemble
import Emulator (Op(OpSkipEqLit,OpSetSound))

bytes :: [Byte]
bytes = assemble asm

asm :: Asm ()
asm = mdo
    WithReg $ \r1 -> do
      WithReg $ \r2 -> do
        WithReg $ \r3 -> do
          go r1 r2 r3
    pure ()

go :: Reg -> Reg -> Reg -> Asm ()
go x y n = do

  setLit x 1
  setLit y 1
  setLit n 2
  loop <- Here
  storeDigitSpriteI n
  draw 5 (x,y)
  incReg x 5
  incReg n 2
  emit $ OpSkipEqLit n 10
  jump loop
  emit $ OpSetSound n
