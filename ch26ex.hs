import Control.Monad.Reader
import Data.Functor.Identity
import Control.Monad.State

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> putStrLn ("hi: " ++ show r) >> return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> putStrLn ("hi: " ++ show s) >> return (show s, s+1)
