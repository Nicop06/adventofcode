import Control.Monad

main :: IO ()
main = do
  forM_ [0 ..] $ \i -> do
    if i > 10
      then return ()
      else print i
  return ()
