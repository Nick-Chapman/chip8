module Bf (bytes) where

import Prelude hiding (break)
import Assemble
import Emulator (addrToInt,byteOfInt,Op(..))
import qualified Data.Char as Char

bytes :: String -> [Byte]
bytes bfCode= assemble $ mdo
  let progSize = byteOfInt (addrToInt mem - addrToInt prog)
  WithReg $ \r1 -> do
    WithReg $ \r2 -> do
      WithReg $ \r3 -> do
        WithReg $ \r4 -> do
          WithReg $ \r5 -> do
            executeBF prog progSize mem r1 r2 r3 r4 r5
  prog <- insertString bfCode
  mem <- Here
  pure ()

panic :: Byte -> Asm ()
panic b = Emit [0,b]

executeBF :: Addr -> Byte -> Addr -> Reg -> Reg -> Reg -> Reg -> Reg -> Asm ()
executeBF prog progSize mem pc mp x y nest = mdo

  box <- insertBytes [0,0,0x40,0,0]

  setReg nest 0
  setReg x 1
  setReg y 1

  let nextOp = incReg pc 1
  let prevOp = incReg pc 255

  let incrementTemp = incReg rTemp 1
  let decrementTemp = incReg rTemp 255

  let isOp c code = ifRegIs (fromIntegral $ Char.ord c) rTemp code

  jump start

  next <- Here -- after each op is executed we return to here
  nextOp
  start <- Here

  let

    readOp = do
      setI prog
      increaseI pc
      readTemp

    readCell = do
      setI mem
      increaseI mp
      readTemp

    storeCell = do
      setI mem
      increaseI mp
      storeTemp

    dot = mdo
      readCell
      let
        newline = do
          setI box
          jump doDraw

      ifRegIs 10 rTemp $ newline
      storeDigitSpriteI rTemp
      doDraw <- Here
      draw 5 (x,y)
      incReg x 5

      ifRegIs 61 x $ do
        setReg x 1
        incReg y 6
        ifRegIs 31 y $ do
          setReg y 1
          --waitKey
          cls
      jump next

    comma = do
      waitKey
      storeCell
      -- next line just compensates for but in haskell chip8 emulator
      loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
      jump next

    plus = do
      readCell
      incrementTemp
      storeCell
      jump next

    minus = do
      readCell
      decrementTemp
      storeCell
      jump next

    langle = do
      incReg mp 255
      jump next

    rangle = do
      incReg mp 1
      jump next

    lsquare = do
      readCell
      ifRegIs 0 rTemp $ do
        loop <- Here
        nextOp
        ifRegIs progSize pc $ panic 0xA
        readOp
        isOp '[' $ do incReg nest 1; jump loop
        isOp ']' $ do ifRegIs 0 nest (jump next); incReg nest 255; jump loop
        jump loop
      jump next

    rsquare = do
      readCell
      ifRegIsNot 0 rTemp $ do
        loop <- Here
        prevOp
        ifRegIs 255 pc $ panic 0xB
        readOp
        isOp ']' $ do incReg nest 1; jump loop
        isOp '[' $ do ifRegIs 0 nest (jump next); incReg nest 255; jump loop
        jump loop
      jump next

  ifRegIs progSize pc $ halt
  readOp
  isOp '.' $ dot
  isOp ',' $ comma
  isOp '+' $ plus
  isOp '-' $ minus
  isOp '<' $ langle
  isOp '>' $ rangle
  isOp '[' $ lsquare
  isOp ']' $ rsquare
  jump next
