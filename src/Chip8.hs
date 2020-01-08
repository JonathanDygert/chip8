module Chip8
    ( emulateCycle
    )
where

import           Control.Lens
import           Control.Monad.State            ( MonadState )
import           Control.Monad.Random.Class     ( MonadRandom )

import           Data.Bits                      ( (.|.)
                                                , (.&.)
                                                , shiftL
                                                , shiftR
                                                )
import           Data.Finite                    ( Finite )
import           Data.Word                      ( Word16 )
import qualified Data.Vector.Sized             as Vec

import           Numeric                        ( showHex )

import qualified Chip8.Ops                     as Ops
import           Chip8.Cpu

-- emulate :: Cpu -> IO ()
-- emulate r cpu = evalStateT emulate' cpu

-- emulate' :: (Gfx g r, MonadState Cpu m, MonadRandom m) => m ()
-- emulate' r = do
--     shouldBeep <- (== 1) <$> use soundTimer
--     emulateCycle
--     drawGraphics
--     setKeys

emulateCycle :: (MonadState Cpu m, MonadRandom m) => m ()
emulateCycle = do
    opcode <- fetchOpcode
    pc += 2
    execute opcode
    delayTimer . from (non 0) . _Just -= 1
    soundTimer . from (non 0) . _Just -= 1

fetchOpcode :: MonadState Cpu m => m Word16
fetchOpcode = do
    pc_     <- use pc
    memory_ <- use memory
    let high = Vec.index memory_ pc_
    let low  = Vec.index memory_ (pc_ + 1)
    pure $ (fromIntegral high `shiftL` 8) .|. (fromIntegral low)

execute :: (MonadState Cpu m, MonadRandom m) => Word16 -> m ()
execute opcode = case nibbles opcode of
    (0x0, 0x0, 0xE, 0x0) -> Ops.clr
    (0x0, 0x0, 0xE, 0xE) -> Ops.rts
    (0x1, _  , _  , _  ) -> Ops.jump addr
    (0x2, _  , _  , _  ) -> Ops.call addr
    (0x3, x  , _  , _  ) -> Ops.ske x imm
    (0x4, x  , _  , _  ) -> Ops.skne x imm
    (0x5, x  , y  , 0x0) -> Ops.skre x y
    (0x6, x  , _  , _  ) -> Ops.load x imm
    (0x7, x  , _  , _  ) -> Ops.add x imm
    (0x8, x  , y  , 0x0) -> Ops.move x y
    (0x8, x  , y  , 0x1) -> Ops.or x y
    (0x8, x  , y  , 0x2) -> Ops.and x y
    (0x8, x  , y  , 0x3) -> Ops.xor x y
    (0x8, x  , y  , 0x4) -> Ops.addr x y
    (0x8, x  , y  , 0x5) -> Ops.sub x y
    (0x8, x  , y  , 0x6) -> Ops.shr x y
    (0x8, x  , y  , 0x7) -> Ops.subn x y
    (0x8, x  , y  , 0xE) -> Ops.shl x y
    (0x9, x  , y  , 0x0) -> Ops.skrne x y
    (0xA, _  , _  , _  ) -> Ops.loadi addr
    (0xB, _  , _  , _  ) -> Ops.jumpi addr
    (0xC, x  , _  , _  ) -> Ops.rand x imm
    (0xD, x  , y  , n  ) -> Ops.draw x y n
    (0xE, x  , 0x9, 0xE) -> Ops.skpr x
    (0xE, x  , 0xA, 0x1) -> Ops.skup x
    (0xF, x  , 0x0, 0x7) -> Ops.moved x
    (0xF, x  , 0x0, 0xA) -> Ops.keyd x
    (0xF, x  , 0x1, 0x5) -> Ops.loadd x
    (0xF, x  , 0x1, 0x8) -> Ops.loads x
    (0xF, x  , 0x1, 0xE) -> Ops.addi x
    (0xF, x  , 0x2, 0x9) -> Ops.ldspr x
    (0xF, x  , 0x3, 0x3) -> Ops.bcd x
    (0xF, x  , 0x5, 0x5) -> Ops.stor x
    (0xF, x  , 0x6, 0x5) -> Ops.read x
    _                    -> error $ "Unknown opcode: " <> showHex opcode ""
  where
    addr = fromIntegral $ opcode .&. 0x0FFF
    imm  = fromIntegral $ opcode .&. 0x00FF

nibbles :: Word16 -> (Finite 16, Finite 16, Finite 16, Finite 16)
nibbles word = (n3, n2, n1, n0)
  where
    n0 = fromIntegral (word .&. 0xF)
    n1 = fromIntegral ((word `shiftR` 4) .&. 0xF)
    n2 = fromIntegral ((word `shiftR` 8) .&. 0xF)
    n3 = fromIntegral ((word `shiftR` 12) .&. 0xF)
