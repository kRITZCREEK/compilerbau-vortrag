module Langlang.Interpreters
       ( interpretiereBedingung
       , interpretiereAusdruck
       , interpretiereAnweisung
       , UnfehlbarerRechner
       , Gespeichertes
       ) where

import Langlang.Data
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M

type Gespeichertes = Either Double FunktionsKoerper

type UnfehlbarerRechner = StateT (M.Map String Gespeichertes) IO
type Rechner a = ErrorT String UnfehlbarerRechner a

interpretiereBedingung :: Bedingung -> Rechner Bool
interpretiereBedingung (Nicht k) = interpretiereBedingung k >>= return . not
interpretiereBedingung (Und k1 k2) = do
  b1 <- interpretiereBedingung k1
  b2 <- interpretiereBedingung k2
  return (b1 && b2)

interpretiereBedingung (Oder k1 k2) = do
  b1 <- interpretiereBedingung k1
  b2 <- interpretiereBedingung k2
  return (b1 || b2)

interpretiereBedingung (Gleich a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  return (w1 == w2)

interpretiereBedingung (Kleiner a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  return (w1 < w2)

interpretiereBedingung (KleinerGleich a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  return (w1 <= w2)

interpretiereAusdruck :: Ausdruck -> Rechner Double
interpretiereAusdruck (Konstante n) = return n
interpretiereAusdruck (Bezeichner i) = do
  varmap <- get
  case M.lookup i varmap of
    Nothing -> fail ("Unbekannter Bezeichner: " ++ i)
    Just (Left n) -> return n
    Just (Right _) -> fail ("Diese Funktion muss aufgerufen werden: " ++ i)

interpretiereAusdruck (Addition a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  return (w1 + w2)

interpretiereAusdruck (Subtraktion a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  return (w1 - w2)

interpretiereAusdruck (Multiplikation a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  return (w1 * w2)

interpretiereAusdruck (Division a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  case w2 of
   0 -> fail "Durch 0 kann nicht geteilt werden."
   _ -> return (w1 / w2)

interpretiereAusdruck (Modulo a1 a2) = do
  w1 <- interpretiereAusdruck a1
  w2 <- interpretiereAusdruck a2
  let n1 = floor w1
      n2 = floor w2
      m = n1 `mod` n2
  return $ fromInteger m

interpretiereAusdruck (Negation a1) = do
  w1 <- interpretiereAusdruck a1
  return $ negate w1

interpretiereAusdruck (FunktionsAufruf fn e) = do
  kontext <- get
  case M.lookup fn kontext of
    Nothing -> fail ("Unbekannte Funktion: " ++ fn)
    Just (Left _) -> fail ("Man kann eine Konstante nicht aufrufen: " ++ fn)
    Just (Right (FunktionsKoerper argname ausdruck)) -> do
      n <- interpretiereAusdruck e
      modify (M.insert argname (Left n))
      r <- interpretiereAusdruck ausdruck
      put kontext
      return r

interpretiereAusdruck (Konditional kond a1 a2) = do
  b <- interpretiereBedingung kond
  if b then interpretiereAusdruck a1 else interpretiereAusdruck a2

interpretiereAnweisung :: Anweisung -> Rechner ()
interpretiereAnweisung (ListenAusgabe (Cons h (Liste t))) = do
  head <- interpretiereAusdruck h
  tail <- mapM interpretiereAusdruck t
  liftIO $ print $ head : tail

interpretiereAnweisung (Ausgabe ausdruck) = do
  n <- interpretiereAusdruck ausdruck
  liftIO $ print n

interpretiereAnweisung (Zuweisung bez ausdruck) = do
  n <- interpretiereAusdruck ausdruck
  modify (M.insert bez (Left n))

interpretiereAnweisung (FunktionsDefinition fn k) = do
  modify (M.insert fn (Right k))
