module Langlang.Data
       (
         Wert,
         Liste(Liste, Cons, LeereListe, Tail),
         Ausdruck(Konstante, Bezeichner, Addition, Subtraktion, Multiplikation
                   , Division
                   , Modulo
                   , Negation
                   , FunktionsAufruf
                   , Konditional
                   )
       , Bedingung(Und, Oder, Nicht, Gleich, Kleiner, KleinerGleich)
       , FunktionsKoerper(FunktionsKoerper)
       , Anweisung(ListenAusgabe, Ausgabe, Zuweisung, FunktionsDefinition)
       ) where

type Wert = Either Liste Ausdruck

data Liste = Liste [Ausdruck]
           | Cons Ausdruck Liste
           | Tail Liste
           | LeereListe
             deriving (Show)

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

data Anweisung = ListenAusgabe Liste
               | Ausgabe Ausdruck
               | Zuweisung String Ausdruck
               | FunktionsDefinition String FunktionsKoerper
               deriving (Show)
