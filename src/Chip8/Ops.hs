module Chip8.Ops where

import           Control.Lens
import           Control.Monad.State            ( MonadState )
import           Control.Monad.Random.Class     ( MonadRandom
                                                , getRandomR
                                                )
import           Control.Monad                  ( forM_
                                                , forM
                                                , when
                                                )

import           Data.Bits                      ( (.|.)
                                                , (.&.)
                                                , testBit
                                                )
import qualified Data.Bits                     as Bits
                                                ( xor )
import           Data.Finite                    ( Finite
                                                , weakenN
                                                )
import qualified Data.Vector.Sized             as Vec
import           Data.Word                      ( Word8 )

import           Chip8.Cpu

-- | Clear the screen
clr :: MonadState Cpu m => m ()
clr = do -- 00E0 	0 	Clear the screen
    gfx .= Vec.replicate (Vec.replicate False)

-- | Return from subroutine
rts :: MonadState Cpu m => m ()
rts = do
    use stack >>= \case
        []       -> error "return from top of stack"
        (x : xs) -> do
            stack .= xs
            pc .= x

-- | Jump to address nnn
jump :: MonadState Cpu m => Finite 4096 -> m ()
jump address = do
    pc .= address

-- | Call routine at address nnn
call :: MonadState Cpu m => Finite 4096 -> m ()
call address = do
    pc_ <- use pc
    stack %= cons pc_
    pc .= address

-- | Skip next instruction if register s equals nn
ske :: MonadState Cpu m => Finite 16 -> Word8 -> m ()
ske x imm = do
    vx <- use $ regs . Vec.ix x
    when (vx == imm) (pc += 2)

-- | Do not skip next instruction if register s equals nn
skne :: MonadState Cpu m => Finite 16 -> Word8 -> m ()
skne x imm = do
    vx <- use $ regs . Vec.ix x
    when (vx /= imm) (pc += 2)

-- | Skip if register s equals register t
skre :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
skre x y = do
    vx <- use $ regs . Vec.ix x
    vy <- use $ regs . Vec.ix y
    when (vx == vy) (pc += 2)

-- | Load register s with value nn
load :: MonadState Cpu m => Finite 16 -> Word8 -> m ()
load x imm = do
    regs . Vec.ix x .= imm

-- | Add value nn to register s
add :: MonadState Cpu m => Finite 16 -> Word8 -> m ()
add x imm = do
    regs . Vec.ix x += imm

-- | Move value from register s to register t
move :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
move x y = do
    vy <- use $ regs . Vec.ix y
    regs . Vec.ix x .= vy

-- | Perform logical OR on register s and t and store in t
or :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
or x y = do
    vy <- use $ regs . Vec.ix y
    regs . Vec.ix x %= (.|. vy)

-- | Perform logical AND on register s and t and store in t
and :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
and x y = do
    vy <- use $ regs . Vec.ix y
    regs . Vec.ix x %= (.&. vy)

-- | Perform logical XOR on register s and t and store in t
xor :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
xor x y = do
    vy <- use $ regs . Vec.ix y
    regs . Vec.ix x %= Bits.xor vy

-- | Add s to t and store in s - register F set on carry
addr :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
addr x y = do
    vx <- use $ regs . Vec.ix x
    vy <- use $ regs . Vec.ix y
    let carry = if vy > 0xFF - vx then 1 else 0
    regs . Vec.ix 0xF .= carry
    regs . Vec.ix x += vy

-- | Subtract s from t and store in s - register F set on !borrow
sub :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
sub x y = do
    vx <- use $ regs . Vec.ix x
    vy <- use $ regs . Vec.ix y
    let not_borrow = if vx > vy then 1 else 0
    regs . Vec.ix 0xF .= not_borrow
    regs . Vec.ix x -= vy

-- | Shift bits in s 1 bit right, store in t - bit 0 shifts to register F
shr :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
shr x _y = do
    vx <- use $ regs . Vec.ix x
    regs . Vec.ix 0xF .= (if testBit vx 0 then 1 else 0)
    regs . Vec.ix x .= vx `div` 2

subn :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
subn x y = do
    vx <- use $ regs . Vec.ix x
    vy <- use $ regs . Vec.ix y
    let not_borrow = if vy > vx then 1 else 0
    regs . Vec.ix 0xF .= not_borrow
    regs . Vec.ix x .= vy - vx

-- | Shift bits in s 1 bit left, store in t - bit 7 shifts to register F
shl :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
shl x _y = do
    vx <- use $ regs . Vec.ix x
    regs . Vec.ix 0xF .= if testBit vx 7 then 1 else 0
    regs . Vec.ix x *= 2

-- | Skip next instruction if register s not equal register t
skrne :: MonadState Cpu m => Finite 16 -> Finite 16 -> m ()
skrne x y = do
    vx <- use $ regs . Vec.ix x
    vy <- use $ regs . Vec.ix y
    when (vx /= vy) (pc += 2)

-- | Load index with value nnn
loadi :: MonadState Cpu m => Finite 4096 -> m ()
loadi address = do
    ireg .= address

-- | Jump to address nnn + index
jumpi :: MonadState Cpu m => Finite 4096 -> m ()
jumpi address = do
    v0 <- use $ regs . Vec.ix 0
    pc .= fromIntegral v0 + address

-- | Generate random number between 0 and nn and store in t
rand :: (MonadState Cpu m, MonadRandom m) => Finite 16 -> Word8 -> m ()
rand x top = do
    r <- getRandomR (0, top)
    regs . Vec.ix x .= r

-- | Draw n byte sprite at x location reg s, y location reg t
draw :: MonadState Cpu m => Finite 16 -> Finite 16 -> Finite 16 -> m ()
draw x y n = do
    vx <- use $ regs . Vec.ix x
    vy <- use $ regs . Vec.ix y
    i  <- use ireg
    forM_ [0 .. n - 1] $ \yline -> do
        pixels <- use $ memory . Vec.ix (i + weakenN yline)
        forM_ [0 .. 7] $ \xline -> do
            let gfxPixel :: Lens' Cpu Bool
                gfxPixel =
                    gfx . Vec.ix (fromIntegral vy + weakenN yline) . Vec.ix
                        (fromIntegral vx + xline)
            let pixel = testBit pixels (7 - fromIntegral xline)
            when pixel $ do
                old <- use gfxPixel
                when old $ regs . Vec.ix 15 .= 1
                gfxPixel .= (old /= pixel)

-- | Skip next instruction if the key in reg s is pressed
skpr :: MonadState Cpu m => Finite 16 -> m ()
skpr x = do
    vx       <- use $ regs . Vec.ix x
    keyState <- use $ key . Vec.ix (fromIntegral vx)
    when (isDown keyState) $ pc += 2

-- | Skip next instruction if the key in reg s is not pressed
skup :: MonadState Cpu m => Finite 16 -> m ()
skup x = do
    vx       <- use $ regs . Vec.ix x
    keyState <- use $ key . Vec.ix (fromIntegral vx)
    when (not $ isDown keyState) $ pc += 2

-- | Move delay timer value into register t
moved :: MonadState Cpu m => Finite 16 -> m ()
moved x = do
    val <- use delayTimer
    regs . Vec.ix x .= val

-- | Wait for keypress and store in register t
keyd :: MonadState Cpu m => Finite 16 -> m ()
keyd x = do
    keypad <- use key
    case Vec.elemIndex Pressed keypad of
        Nothing -> pc -= 2
        Just i  -> regs . Vec.ix x .= fromIntegral i

-- | Load delay timer with value in register s
loadd :: MonadState Cpu m => Finite 16 -> m ()
loadd x = do
    vx <- use $ regs . Vec.ix x
    delayTimer .= vx

-- | Load sound timer with value in register s
loads :: MonadState Cpu m => Finite 16 -> m ()
loads x = do
    vx <- use $ regs . Vec.ix x
    soundTimer .= vx

-- | Add value in register s to index
addi :: MonadState Cpu m => Finite 16 -> m ()
addi x = do
    vx <- use $ regs . Vec.ix x
    ireg += fromIntegral vx

-- | Load index with sprite from register s
ldspr :: MonadState Cpu m => Finite 16 -> m ()
ldspr x = do
    vx <- use $ regs . Vec.ix x
    when (vx > 0xF) $ error $ "No sprite for " <> show vx
    ireg .= fromIntegral vx * 5

-- | Store the binary coded decimal value of register s at index
bcd :: MonadState Cpu m => Finite 16 -> m ()
bcd x = do
    vx <- use $ regs . Vec.ix x
    i  <- use ireg
    memory . Vec.ix i .= vx `div` 100
    memory . Vec.ix (i + 1) .= vx `div` 10 `mod` 10
    memory . Vec.ix (i + 2) .= vx `mod` 10

-- | Store the values of register s registers at index
stor :: MonadState Cpu m => Finite 16 -> m ()
stor x = do
    i  <- use ireg
    vs <- uses regs (take (fromIntegral x + 1) . Vec.toList)
    memory %= \mem -> mem Vec.// zip [i ..] vs

-- | Read back the stored values at index into registers
read :: MonadState Cpu m => Finite 16 -> m ()
read x = do
    i  <- use ireg
    vs <- forM [i .. i + weakenN x] $ \j -> use (memory . Vec.ix j)
    regs %= \r -> r Vec.// zip [0 ..] vs

isDown :: KeyState -> Bool
isDown Pressed    = True
isDown Held       = True
isDown NotPressed = False
