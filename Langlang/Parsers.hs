module Langlang.Parsers
       (
         parseNummer
       , parseAusdruck
       , parseFunktionsAufruf
       , parseKonditional
       , parseBedingung
       , parseKonditionalTerm
       , parseVergleich
       , parseTerm
       , parsePrint
       , parseZuweisung
       , parseFunktionsDefinition
       , parseInput
       ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Langlang.Data


lexer :: TokenParser ()
lexer = makeTokenParser
        (javaStyle { opStart  = oneOf "+-*/%|&=!<>"
                   , opLetter = oneOf "+-*/%|&=!<>"
                   , reservedNames = ["let", "def", "print"
                                     ,"if", "then", "else"]})

parseNummer :: Parser Ausdruck
parseNummer = do
  wert <- naturalOrFloat lexer
  case wert of
    Left i -> return $ Konstante $ fromIntegral i
    Right n -> return $ Konstante $ n

parseAusdruck :: Parser Ausdruck
parseAusdruck = (flip buildExpressionParser) parseTerm $ [
    [ Prefix (reservedOp lexer "-" >> return Negation)
    , Prefix (reservedOp lexer "+" >> return id) ]
  , [ Infix  (reservedOp lexer "*" >> return Multiplikation) AssocLeft
    , Infix  (reservedOp lexer "/" >> return Division) AssocLeft
    , Infix  (reservedOp lexer "%" >> return Modulo) AssocLeft ]
  , [ Infix  (reservedOp lexer "+" >> return Addition) AssocLeft
    , Infix  (reservedOp lexer "-" >> return Subtraktion) AssocLeft ]
  ]

parseFunktionsAufruf :: Parser Ausdruck
parseFunktionsAufruf = do
  ident <- identifier lexer
  expr <- parens lexer parseAusdruck
  return $ FunktionsAufruf ident expr

parseKonditional :: Parser Ausdruck
parseKonditional = do
  reserved lexer "if"
  b <- parseBedingung
  reserved lexer "then"
  a1 <- parseAusdruck
  reserved lexer "else"
  a2 <- parseAusdruck
  return $ Konditional b a1 a2

parseBedingung :: Parser Bedingung
parseBedingung = (flip buildExpressionParser) parseKonditionalTerm $ [
  [ Infix  (reservedOp lexer "&&" >> return Und) AssocLeft
    , Infix  (reservedOp lexer "||" >> return Oder) AssocLeft ]
  ]

parseKonditionalTerm :: Parser Bedingung
parseKonditionalTerm =
  parens lexer parseBedingung
  <|> parseVergleich

parseVergleich :: Parser Bedingung
parseVergleich = do
  a1 <- parseAusdruck
  f <- (reserved lexer "==" >> return (Gleich a1))
       <|> (reserved lexer "<" >> return (Kleiner a1))
       <|> (reserved lexer "<=" >> return (KleinerGleich a1))
       <|> (reserved lexer ">" >> return (Nicht . (KleinerGleich a1)))
       <|> (reserved lexer ">=" >> return (Nicht . (Kleiner a1)))
       <|> (reserved lexer "!=" >> return (Nicht . (Gleich a1)))
  a2 <- parseAusdruck
  return $ f a2

parseTerm :: Parser Ausdruck
parseTerm =
  parens lexer parseAusdruck
  <|> parseNummer
  <|> try parseFunktionsAufruf
  <|> try parseKonditional
  <|> (identifier lexer >>= return . Bezeichner)

parsePrint :: Parser Anweisung
parsePrint = do
  reserved lexer "print"
  expr <- parseAusdruck
  return $ Ausgabe expr

parseZuweisung :: Parser Anweisung
parseZuweisung = do
  reserved lexer "let"
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseAusdruck
  return $ Zuweisung ident expr

parseFunktionsDefinition :: Parser Anweisung
parseFunktionsDefinition = do
  reserved lexer "def"
  bezeichner <- identifier lexer
  argumentName <- parens lexer $ identifier lexer
  reservedOp lexer "="
  ausdruck <- parseAusdruck
  return $ FunktionsDefinition bezeichner (FunktionsKoerper argumentName ausdruck)

parseInput :: Parser Anweisung
parseInput = do
  whiteSpace lexer
  a <- (parsePrint
        <|> parseZuweisung
        <|> parseFunktionsDefinition
       )
  eof
  return a
