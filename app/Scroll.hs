-- TODO: move these to package.yaml
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

-- Goal for this example is to scroll a text message strored in mem.
-- And to develop SMC techniques needed to manipulate I.
module Scroll (bytes) where

import Data.Char as Char
import Prelude hiding (break)
import Assemble
import FontData (fontData)

bytes :: [Byte]
bytes = assemble asm

asm :: Asm ()
asm = mdo
    WithReg $ \r1 -> do
      WithReg $ \r2 -> do
        WithReg $ \r3 -> do
          withWide $ \w1 -> do
            withWide $ \w2 -> do
              scrollMessage font message r1 r2 r3 w1 w2
    halt
    font <- insertBytes fontData
    message <- Here
    pure ()

scrollMessage :: Addr -> Addr -> Reg -> Reg -> Reg -> Wide -> Wide -> Asm ()
scrollMessage font message rx ry rn mp fp = do

  let
    setFontI rn = do
      setWr fp rn
      shiftLw fp
      shiftLw fp
      shiftLw fp
      addWa fp font
      setIw fp

  let
    offsetSpace r = do
      incReg r (fromIntegral (256 - Char.ord ' '))

  setReg rx 0
  setReg ry 0
  let
    putCharI :: Reg -> Asm ()
    putCharI rn = do
      ifRegIs 10 rn $ do incReg rn 22 -- newline --> space
      offsetSpace rn
      setFontI rn
      draw 8 (rx,ry) -- height of 8
      incReg rx 8 -- width of 8
      ifRegIs 64 rx $ do
        setReg rx 0
        incReg ry 8
        ifRegIs 32 ry $ do
          setReg ry 0
          waitKey
          cls

  setWa mp message

  loop <- Here
  readW mp rn
  ifRegNotZero rn $ do
    putCharI rn
    incWide mp
    jump loop
