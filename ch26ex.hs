import Control.Monad.Reader
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans.Maybe

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r ->
  putStrLn ("hi: " ++ show r) >>
  return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s ->
  putStrLn ("hi: " ++ show s) >>
  return (show s, s+1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
--  guard $ isValid v
  case isValid v of
    False -> return Nothing
    True -> return $ Just v

doExcite :: IO ()
doExcite = forever $ do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
