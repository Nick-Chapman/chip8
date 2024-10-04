module Bfw (bytes) where -- wide version: support Brainfuck programs >256 bytes

import Prelude hiding (break)
import Assemble
import Emulator (Op(..))
import qualified Data.Char as Char

bytes :: [Byte]
bytes = assemble $ mdo
  withWide $ \w1 -> do
    withWide $ \w2 -> do
      WithReg $ \r1 -> do
        WithReg $ \r2 -> do
          WithReg $ \r3 -> do
            WithReg $ \r4 -> do
              executeBF w1 w2 prog r1 r2 r3 r4
  prog <- Later Here
  pure ()

executeBF :: Wide -> Wide -> Addr -> Reg -> Reg -> Reg -> Reg -> Asm ()
executeBF mp pc progAddr x y nest charToStore = mdo

  setLit nest 0
  setLit x 1
  setLit y 1

  let nextOp = incWide pc
  let prevOp = decWide pc

  let incrementTemp = incReg rTemp 1
  let decrementTemp = incReg rTemp 255

  let
    isOp0 asm =
      ifRegEq rTemp 0 asm

    isOp c asm =
      ifRegEq rTemp (fromIntegral $ Char.ord c) asm

    isNotOp :: Char -> Op -> Asm ()
    isNotOp c maybeSkipped = do
      emit (OpSkipEqLit rTemp (fromIntegral $ Char.ord c))
      emit maybeSkipped

  setWide pc progAddr

  -- initialize memory-pointer to begin after the program text, with one zero byte separation.
  setWide mp progAddr
  forever $ do
    setIw mp
    incWide mp
    readTemp
    isOp0 $ jump start

  next <- Here -- after each op is executed we return to here
  nextOp
  start <- Here

  readOp <- insertSubroutineLater $ do
    setIw pc
    readTemp

  readCell <- insertSubroutineLater $ do
    setIw mp
    readTemp

  storeCell <- insertSubroutineLater $ do
    setReg charToStore rTemp
    setIw mp
    storeI charToStore

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
      -- no checking we dont move too far left
      decWide mp
      jump next

    rangle = do
      incWide mp
      jump next

    lsquare = do
      readCell
      emit (OpSkipEqLit rTemp 0) ; emit (OpJump next)
      scan <- Here
      nextOp
      readOp
      isOp0 $ panic 0xA
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
      readOp
      isOp0 $ panic 0xB
      isOp ']' $ do incReg nest 1; jump scan
      isNotOp '[' (OpJump scan)
      emit (OpSkipNotEqLit nest 0) ; emit (OpJump next)
      incReg nest 255
      jump scan

  readOp
  isOp0 $ halt
  isOp '.' $ dot
  isOp ',' $ comma
  isOp '+' $ plus
  isOp '-' $ minus
  isOp '<' $ langle
  isOp '>' $ rangle
  isOp '[' $ lsquare
  isOp ']' $ rsquare
  --panic 0xC
  jump next

  box <- Here; Emit [0,0,0x40,0,0,0]
  pure ()
