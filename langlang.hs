import Text.Parsec
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Langlang.Parsers
import Langlang.Interpreters

startWerte :: M.Map String Gespeichertes
startWerte = M.fromList
              [ ("e",   Left (exp 1))
              , ("pi",  Left pi)
              , ("SinndesLebens", Left 42)
              ]

berechne :: String -> UnfehlbarerRechner ()
berechne s =
  case p of
    Left e -> liftIO $ putStrLn $ "Fehler: " ++ (show e)
    Right n -> do res <- runErrorT $ interpretiereAnweisung n
                  case res of
                    Left e' -> liftIO $ putStrLn $ "Laufzeitfehler: " ++ e'
                    Right _ -> return ()
  where
    p = parse parseInput "" s

rechner :: UnfehlbarerRechner ()
rechner =
  liftIO getContents >>= (mapM_ berechne) . lines

main :: IO ()
main = evalStateT rechner startWerte
