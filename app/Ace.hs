module Ace (bytes) where

-- This program should output "ACE".
-- It's designed to provoke a bug in Self.hs where it outputs "BAD".

import Prelude hiding (break)
import Assemble

bytes :: [Byte]
bytes = assemble $ mdo
  let x = Reg 1
  let y = Reg 2
  let a = Reg 3
  let c = Reg 4
  let e = Reg 0
  setLit x 1
  setLit y 1
  setLit a 0xA
  setLit c 0xC
  setLit e 0xE
  storeDigitSpriteI a
  setLit a 0xB
  draw 5 (x,y)
  incReg x 5
  storeDigitSpriteI c
  setLit c 0xA
  draw 5 (x,y)
  incReg x 5
  storeDigitSpriteI e
  setLit e 0xD
  draw 5 (x,y)
  panic 0
