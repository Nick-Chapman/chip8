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

  setLit rx 1
  setLit ry 2
  setLit rn 3
  storeDigitSpriteI rn
  draw 5 (rx,ry)
