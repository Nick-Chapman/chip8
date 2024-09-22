module Three (bytes) where

import Prelude hiding (break)
import Assemble

bytes :: [Byte]
bytes = assemble asm

asm :: Asm ()
asm = mdo
    WithReg $ \r1 -> do
      WithReg $ \r2 -> do
        WithReg $ \r3 -> do
          go r1 r2 r3
    halt
    pure ()

go :: Reg -> Reg -> Reg -> Asm ()
go rx ry rn = do

  setReg rx 1
  setReg ry 2
  setReg rn 3
  storeDigitSpriteI rn
  draw 5 (rx,ry)
