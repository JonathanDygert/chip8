module Chip8.Cpu where

import           Prelude                 hiding ( (++) )
import           Control.Lens                   ( makeLenses )
import           Data.Finite                    ( Finite )
import           Data.Vector.Sized              ( Vector
                                                , (++)
                                                )
import qualified Data.Vector.Sized             as Vec
import           Data.Word                      ( Word8 )

data KeyState = Pressed | Held | NotPressed
    deriving (Show, Eq)

data Cpu = Cpu
    { _ireg :: Finite 4096
    , _pc :: Finite 4096
    , _memory :: Vector 4096 Word8
    , _regs :: Vector 16 Word8
    , _gfx :: Vector 32 (Vector 64 Bool)
    , _delayTimer :: Word8
    , _soundTimer :: Word8
    , _stack :: [Finite 4096]
    , _key :: Vector 16 KeyState
    }
    deriving Show
makeLenses ''Cpu

mkCpu :: Vector 0xE00 Word8 -> Cpu
mkCpu prog = Cpu { _ireg       = 0
                 , _pc         = 0x200
                 , _memory     = fontset ++ Vec.replicate 0 ++ prog
                 , _regs       = Vec.replicate 0
                 , _gfx        = Vec.replicate (Vec.replicate False)
                 , _delayTimer = 0
                 , _soundTimer = 0
                 , _stack      = []
                 , _key        = Vec.replicate NotPressed
                 }

fontset :: Vector 0x50 Word8
fontset =
    digit (0xF0, 0x90, 0x90, 0x90, 0xF0) -- 0
        ++ digit (0x20, 0x60, 0x20, 0x20, 0x70) -- 1
        ++ digit (0xF0, 0x10, 0xF0, 0x80, 0xF0) -- 2
        ++ digit (0xF0, 0x10, 0xF0, 0x10, 0xF0) -- 3
        ++ digit (0x90, 0x90, 0xF0, 0x10, 0x10) -- 4
        ++ digit (0xF0, 0x80, 0xF0, 0x10, 0xF0) -- 5
        ++ digit (0xF0, 0x80, 0xF0, 0x90, 0xF0) -- 6
        ++ digit (0xF0, 0x10, 0x20, 0x40, 0x40) -- 7
        ++ digit (0xF0, 0x90, 0xF0, 0x90, 0xF0) -- 8
        ++ digit (0xF0, 0x90, 0xF0, 0x10, 0xF0) -- 9
        ++ digit (0xF0, 0x90, 0xF0, 0x90, 0x90) -- A
        ++ digit (0xE0, 0x90, 0xE0, 0x90, 0xE0) -- B
        ++ digit (0xF0, 0x80, 0x80, 0x80, 0xF0) -- C
        ++ digit (0xE0, 0x90, 0x90, 0x90, 0xE0) -- D
        ++ digit (0xF0, 0x80, 0xF0, 0x80, 0xF0) -- E
        ++ digit (0xF0, 0x80, 0xF0, 0x80, 0x80) -- F
    where digit t = (Vec.fromTuple t) :: Vector 5 Word8
