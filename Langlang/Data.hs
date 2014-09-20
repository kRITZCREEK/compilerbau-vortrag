module Langlang.Data
       (
         Ausdruck(Konstante, Bezeichner, Addition, Subtraktion, Multiplikation
                   , Division
                   , Modulo
                   , Negation
                   , FunktionsAufruf
                   , Konditional
                   )
       , Bedingung(Und, Oder, Nicht, Gleich, Kleiner, KleinerGleich)
       , FunktionsKoerper(FunktionsKoerper)
       , Anweisung(Ausgabe, Zuweisung, FunktionsDefinition)
       ) where

data Ausdruck = Konstante Double
                | Bezeichner String
                | Addition Ausdruck Ausdruck
                | Subtraktion Ausdruck Ausdruck
                | Multiplikation Ausdruck Ausdruck
                | Division Ausdruck Ausdruck
                | Modulo Ausdruck Ausdruck
                | Negation Ausdruck
                | FunktionsAufruf String Ausdruck
                | Konditional Bedingung Ausdruck Ausdruck
                deriving (Show)

data Bedingung = Und Bedingung Bedingung
               | Oder Bedingung Bedingung
               | Nicht Bedingung
               | Gleich Ausdruck Ausdruck
               | Kleiner Ausdruck Ausdruck
               | KleinerGleich Ausdruck Ausdruck
               deriving (Show)

data FunktionsKoerper = FunktionsKoerper String Ausdruck
                    deriving (Show)

data Anweisung = Ausgabe Ausdruck
               | Zuweisung String Ausdruck
               | FunktionsDefinition String FunktionsKoerper
               deriving (Show)
