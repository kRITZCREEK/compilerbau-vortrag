module Main where

newtype Parser a = Parser (String -> [(a,String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                       ""    -> []
                       (h:t) -> [(h,t)]
              )

one :: Parser Char
one = Parser (\cs -> case cs of
                         ('1': t) -> [('1', t)]
                         _ -> []
                )

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
parse :: Parser t -> String -> [(t, String)]
parse (Parser p) = p

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                              (a,cs') <- parse p cs])

class MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (++#) :: m a -> m a -> m a

instance MonadZero Parser where
  zero = Parser (\_ -> [])

instance MonadPlus Parser where
  p ++# q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++# q) cs of
                          [] -> []
                          (x:_) -> [x])
oneAtPos3 :: Parser String
oneAtPos3  = do
  first <- item
  second <- item
  one
  return $ first:[second]

main :: IO ()
main = do
  putStrLn "HI THAR"
