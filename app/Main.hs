import           Control.Concurrent             ( threadDelay )
import           Control.Lens
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.State            ( MonadState
                                                , StateT
                                                , evalStateT
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

import qualified Data.ByteString.Lazy          as BL
import           Data.Int                       ( Int16 )
import           Data.IORef                     ( readIORef
                                                , newIORef
                                                , writeIORef
                                                )
import qualified Data.Vector.Sized             as Vec
import           Data.Vector.Sized              ( Vector )
import qualified Data.Vector.Storable.Mutable  as MVec
import           Data.Vector.Storable.Mutable   ( IOVector )

import           System.Environment             ( getArgs )

import qualified SDL

import           Chip8                          ( emulateCycle )
import           Chip8.Cpu                      ( Cpu
                                                , KeyState(..)
                                                , mkCpu
                                                , key
                                                , gfx
                                                , soundTimer
                                                )

pixelWidth :: Num a => a
pixelWidth = 20

main :: IO ()
main = do
    path <- head <$> getArgs
    bs   <- BL.readFile path
    let prog = Vec.replicate 0 Vec.// zip [0 ..] (BL.unpack bs)
    let cpu  = mkCpu prog
    let windowConfig = SDL.defaultWindow
            { SDL.windowInitialSize = SDL.V2 (64 * pixelWidth) (32 * pixelWidth)
            }
    let audioConfig = SDL.OpenDeviceSpec
            { SDL.openDeviceFreq     = SDL.Mandate 48000
            , SDL.openDeviceFormat   = SDL.Mandate SDL.Signed16BitNativeAudio
            , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
            , SDL.openDeviceSamples  = 4096 * 2
            , SDL.openDeviceCallback = beep
            , SDL.openDeviceUsage    = SDL.ForPlayback
            , SDL.openDeviceName     = Nothing
            }
    SDL.initializeAll
    window     <- SDL.createWindow "Chip8 Emulator" windowConfig
    (audio, _) <- SDL.openAudioDevice audioConfig
    renderer   <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.setAudioDevicePlaybackState audio SDL.Play
    threadDelay 100000
    SDL.setAudioDevicePlaybackState audio SDL.Pause
    evalStateT (appLoop renderer audio) cpu
    SDL.closeAudioDevice audio

timestep :: Int
timestep = round $ (10000 :: Double) / 60

appLoop :: SDL.Renderer -> SDL.AudioDevice -> StateT Cpu IO ()
appLoop renderer audio = do
    emulateCycle
    time <- use soundTimer
    SDL.setAudioDevicePlaybackState
        audio
        (if time > 0 then SDL.Play else SDL.Pause)
    pixels <- use gfx
    drawScreen renderer pixels
    exit <- readInput
    liftIO $ threadDelay timestep
    unless exit $ appLoop renderer audio

beep :: SDL.AudioFormat s -> IOVector s -> IO ()
beep format buffer = case format of
    SDL.Signed16BitLEAudio ->
        -- forM_ [0 .. length buffer] $ \i -> MVec.write buffer i 10000
        let
            sinWave = map
                (\n ->
                    let
                        t :: Double
                        t    = fromIntegral n / 48000.0
                        freq = 440 * 4
                    in
                        round
                            ( fromIntegral (maxBound `div` 2 :: Int16)
                            * sin (t * freq)
                            )
                )
                [0 :: Integer ..]
        in  forM_ (zip [0 .. MVec.length buffer - 1] sinWave)
                $ \(i, x) -> MVec.write buffer i x
    _ -> error $ "Unsupported audio format: " <> show format

drawScreen :: MonadIO m => SDL.Renderer -> Vector 32 (Vector 64 Bool) -> m ()
drawScreen renderer pixels = do
    SDL.rendererDrawColor renderer SDL.$= black
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= white
    forM_ [0 .. 31] $ \r -> forM_ [0 .. 63] $ \c ->
        when (pixels `Vec.index` r `Vec.index` c) $ do
            let x    = fromIntegral c * pixelWidth
            let y    = fromIntegral r * pixelWidth
            let o    = (SDL.P (SDL.V2 x y))
            let rect = SDL.Rectangle o size
            SDL.fillRect renderer (Just rect)
    SDL.present renderer
  where
    black = SDL.V4 0 0 0 255
    white = SDL.V4 255 255 255 255
    size  = (SDL.V2 pixelWidth pixelWidth)

readInput :: (MonadState Cpu m, MonadIO m) => m Bool
readInput = do
    exit    <- liftIO $ newIORef False
    oldKeys <- use key
    SDL.mapEvents $ \e -> case SDL.eventPayload e of
        SDL.KeyboardEvent kdata -> do
            let motion = SDL.keyboardEventKeyMotion kdata
            let state = case motion of
                    SDL.Pressed  -> True
                    SDL.Released -> False
            let scancode = SDL.keysymScancode $ SDL.keyboardEventKeysym kdata
                setKey i = if state
                    then key . Vec.ix i %= \case
                        Pressed    -> Held
                        Held       -> Held
                        NotPressed -> Pressed
                    else key . Vec.ix i .= NotPressed
            case scancode of
                SDL.Scancode1      -> setKey 0x1
                SDL.Scancode2      -> setKey 0x2
                SDL.Scancode3      -> setKey 0x3
                SDL.Scancode4      -> setKey 0xC
                SDL.ScancodeQ      -> setKey 0x4
                SDL.ScancodeW      -> setKey 0x5
                SDL.ScancodeE      -> setKey 0x6
                SDL.ScancodeR      -> setKey 0xD
                SDL.ScancodeA      -> setKey 0x7
                SDL.ScancodeS      -> setKey 0x8
                SDL.ScancodeD      -> setKey 0x9
                SDL.ScancodeF      -> setKey 0xE
                SDL.ScancodeZ      -> setKey 0xA
                SDL.ScancodeX      -> setKey 0x0
                SDL.ScancodeC      -> setKey 0xB
                SDL.ScancodeV      -> setKey 0xF
                SDL.ScancodeEscape -> liftIO $ writeIORef exit True
                _                  -> pure ()
        _ -> pure ()
    key %= \keys -> Vec.zipWith
        (\old new -> if old == Pressed && new == Pressed then Held else new)
        oldKeys
        keys
    liftIO $ readIORef exit
