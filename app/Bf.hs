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
  prog <- insertBytesLater (bytesOfString bfCode)
  mem <- Later Here
  pure ()

executeBF :: Addr -> Byte -> Addr -> Reg -> Reg -> Reg -> Reg -> Reg -> Asm ()
executeBF prog progSize mem pc mp x y nest = mdo

  setLit nest 0
  setLit x 1
  setLit y 1

  let nextOp = incReg pc 1
  let prevOp = incReg pc 255

  let incrementTemp = incReg rTemp 1
  let decrementTemp = incReg rTemp 255

  let
    isOp c asm =
      ifRegEq rTemp (fromIntegral $ Char.ord c) asm

    isNotOp :: Char -> Op -> Asm ()
    isNotOp c maybeSkipped = do
      emit (OpSkipEqLit rTemp (fromIntegral $ Char.ord c))
      emit maybeSkipped

  jump start

  next <- Here -- after each op is executed we return to here
  nextOp
  start <- Here

  readOp <- insertSubroutineLater $ do
    setI prog
    increaseI pc
    readTemp

  readCell <- insertSubroutineLater $ do
    setI mem
    increaseI mp
    readTemp

  storeCell <- insertSubroutineLater $ do
    setI mem
    increaseI mp
    storeTemp

  let
    maskReg r1 v = WithReg $ \r2 -> do setLit r2 v ; opAnd r1 r2

    dot = mdo
      readCell

      ifRegEq rTemp 10 $ do
        setI box
        jump doDraw

      maskReg rTemp 0xF
      storeDigitSpriteI rTemp
      doDraw <- Here
      draw 5 (x,y)
      incReg x 5

      emit (OpSkipEqLit x 61) ; emit (OpJump next)
      setLit x 1
      incReg y 6
      emit (OpSkipEqLit y 31) ; emit (OpJump next)
      setLit y 1
      --waitKey
      cls
      jump next

    comma = do
      waitKey
      storeCell
      --compensates for bug in haskell chip8 emulator...
      --loop <- Here ; emit (OpSkipNotKey rTemp) ; jump loop
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
      emit (OpSkipEqLit rTemp 0) ; emit (OpJump next)
      scan <- Here
      nextOp
      --ifRegEq pc progSize $ panic 0xA -- save 6 bytes - test/skip/panic
      readOp
      isOp '[' $ do incReg nest 1; jump scan
      isNotOp ']' (OpJump scan)
      emit (OpSkipNotEqLit nest 0) ; emit (OpJump next)
      incReg nest 255
      jump scan

    rsquare = do
      readCell
      emit (OpSkipNotEqLit rTemp 0) ; emit (OpJump next)
      scan <- Here
      prevOp
      --ifRegEq pc 255 $ panic 0xB -- save another 6 bytes
      readOp
      isOp ']' $ do incReg nest 1; jump scan
      isNotOp '[' (OpJump scan)
      emit (OpSkipNotEqLit nest 0) ; emit (OpJump next)
      incReg nest 255
      jump scan

  ifRegEq pc progSize $ halt
  readOp
  isOp '.' $ dot
  isOp ',' $ comma
  isOp '+' $ plus
  isOp '-' $ minus
  isOp '<' $ langle
  isOp '>' $ rangle
  isOp '[' $ lsquare
  isNotOp ']' (OpJump next)
  rsquare

  box <- Here; Emit [0,0,0x40,0,0,0]
  pure ()
