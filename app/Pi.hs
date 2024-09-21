-- TODO: move these to package.yaml
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Pi (bytes) where

import Prelude hiding (break)
import Assemble

bytes :: [Byte]
bytes = assemble asm

asm :: Asm ()
asm = do
    WithReg $ \r1 -> do
      WithReg $ \r2 -> do
        WithReg $ \r3 -> do
          WithReg $ \r4 -> do
            displayPI r1 r2 r3 r4
    crash

displayPI :: Reg -> Reg -> Reg -> Reg -> Asm ()
displayPI rd rx ry rn = do
  point <- insertBytesLater [ 0x00, 0x00, 0x00, 0x60, 0x60 ]
  let decimalData = [ 1,4,1,5,9,2,6,5,3,5, 8,9,7,9,3,2,3,8,4,6,
                      2,6,4,3,3,8,3,2,7,9, 5,0,2,8,8,4,1,9,7,1,
                      6,9,3,9,9,3,7,5,1,0, 5,8,2,0,9,7,4,9,4,4,
                      5,9,2,3,0,7,8,1,6,4, 0,6,2,8,6,2,0,8,9,9,
                      8,6,2,8,0,3,4,8,2,5, 3,4,2,1,1,7,0,6,7,9 ]

  decimals <- insertBytesLater decimalData

  let
    putI = do
      draw 5 (rx,ry)
      incReg rx 5
  let
    put n = do
      setReg rd n
      storeDigitSpriteI rd
      putI
  let
    putPoint = do
      setI point
      putI

  let
    scrollup n = mdo
      setReg rn n
      setReg rx 11
      loop <- Here
      setI decimals
      increaseI rn
      incReg rn 1 -- move to next elem of data
      readI rd
      storeDigitSpriteI rd
      setReg ry 2
      draw 5 (rx,ry)
      setReg ry 8
      draw 5 (rx,ry)
      incReg rx 5
      ifRegIs (n+10) rn $ jump after
      jump loop
      after <- Here
      pure ()

  let
    wipeTop n = mdo
      setReg rn n
      setReg rx 11 -- TODO: compute these 2*5+1
      loop <- Here
      setI decimals
      increaseI rn
      incReg rn 1 -- move to next elem of data
      readI rd
      storeDigitSpriteI rd
      setReg ry 2
      draw 5 (rx,ry)
      incReg rx 5
      ifRegIs (n+10) rn $ jump after
      jump loop
      after <- Here
      pure ()

  let
    line n = mdo
      setReg rn n -- first elem of digit data
      setReg ry 8
      setReg rx 11
      loop <- Here
      setI decimals
      increaseI rn
      incReg rn 1 -- move to next elem of data
      readI rd
      storeDigitSpriteI rd
      putI
      ifRegIs (n+10) rn $ jump after
      jump loop
      after <- Here
      pure ()

  setReg ry 7
  setReg rx 1
  put 3
  putPoint

  line 0
  scrollup 0

  line 10
  wipeTop 0
  scrollup 10

  line 20
  wipeTop 10
  scrollup 20

  line 30
  wipeTop 20
  scrollup 30

  line 40
  wipeTop 30
  --scrollup 40

  crash
