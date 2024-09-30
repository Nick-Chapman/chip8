module Seven (bytes) where

import Prelude hiding (break)
import Assemble

bytes :: [Byte]
bytes = assemble $ mdo

  let x = Reg 1
  let y = Reg 2
  let n = Reg 3

  setLit n 7
  storeDigitSpriteI n
  setLit n 3
  increaseI n
  setLit rTemp 0xf0 -- make this a German style 7 with a bar
  storeTemp

  setLit x 1
  setLit y 2
  setLit n 7
  storeDigitSpriteI n
  draw 5 (x,y)
  panic 0x7
