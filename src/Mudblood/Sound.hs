{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mudblood.Sound
    ( Sound, SoundHandle, initSound, withSoundSync, withSoundAsync
    , playHelloWorld, playMusic, playHealth
    ) where

import Prelude hiding (catch)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Exception (try, catch, SomeException)
import Control.Concurrent

import Sound.ALUT

import Mudblood.Error


newtype Sound a = Sound { runSound :: StateT SoundState IO a }
    deriving (Monad, MonadIO, MonadState SoundState)

data SoundState = SoundState
    { musicSource :: Source
    , healthSource :: Source
    }

type SoundHandle = Chan (SoundState -> IO SoundState)

soundThread :: SoundHandle -> IO ()
soundThread chan = do
    withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
        (Just device) <- openDevice Nothing
        (Just context) <- createContext device []
        currentContext $= Just context
        [ms] <- genObjectNames 1
        [hs] <- genObjectNames 1
        
        let initstate = SoundState
                { musicSource = ms
                , healthSource = hs
                }

        let runloop state = do
                act <- readChan chan
                state' <- act state
                runloop state'

        runloop initstate

        closeDevice device
        return ()

withSoundAsync :: SoundHandle -> Sound a -> IO (MVar (Either StackTrace a))
withSoundAsync handle act = do
    ref <- newEmptyMVar
    writeChan handle $ \s -> do
        r <- try $ runStateT (runSound act) s
        case r of
            Left err -> do
                putMVar ref $ Left $ stackTrace "openal" $ show (err :: SomeException)
                return s
            Right (x, s') -> do
                putMVar ref $ Right x
                return s'
    return ref

withSoundSync :: SoundHandle -> Sound a -> IO (Either StackTrace a)
withSoundSync handle act = do
    ref <- withSoundAsync handle act
    takeMVar ref

initSound :: IO (SoundHandle)
initSound = do
    handle <- newChan
    forkIO $ soundThread handle
    return handle

-----------------------------------------------------------------------------

playHelloWorld :: Sound ()
playHelloWorld = liftIO $ do
    buffer <- createBuffer HelloWorld
    [source] <- genObjectNames 1
    queueBuffers source [buffer]
    play [source]

playMusic :: FilePath -> Sound ()
playMusic path = do
    src <- gets musicSource
    [newsrc] <- liftIO $ genObjectNames 1
    modify $ \s -> s { musicSource = newsrc }

    liftIO $ do
        buffer <- createBuffer $ File path
        queueBuffers newsrc $ take 50 $ repeat buffer
        play [newsrc]

        forkIO $ do
            sourceGain newsrc $=! 0
            forM_ [0..1000] $ \i -> do
                sourceGain newsrc $= (0.001 * i)
                sourceGain src $= (1 - 0.001 * i)
                sleep 0.001
            stop [src]
    return ()

playHealth :: Maybe Float -> Sound ()
playHealth v = do
    src <- gets healthSource
    case v of
        Nothing -> liftIO $ stop [src]
        Just v' -> do
            [newsrc] <- liftIO $ genObjectNames 1
            modify $ \s -> s { healthSource = newsrc }
            liftIO $ do
                buffer <- createBuffer $ Impulse (20 * (1 - v')) 0 10
                queueBuffers newsrc $ take 1000 $ repeat buffer
                play [newsrc]
                stop [src]

{-
playSound :: FilePath -> Sound ()
playSound path = do
    buffer <- createBuffer $ File path
    -}
