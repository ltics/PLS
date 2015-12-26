{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts,TypeOperators #-}
module Parsers.MiniParsec where
-- import Prelude hiding (Monad, (>>=), return)
import Control.Applicative
import Control.Monad
import Data.Char
import Cota    

data Parser t = Parser (String -> [(t, String)]) deriving (Functor)

parse :: Parser t -> String -> [(t, String)]
parse (Parser p) = p

{-
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (\s -> concat $
                         map (\ (a, s') -> parse (f a) s')
                                 $ parse p s)
-}

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser (\s -> parse p s |>
                         map (\(a, s') -> parse (f a) s') |>
                         concat)
                   
item :: Parser Char
item = Parser (\s -> case s of
                       ""     -> []
                       (c:cs) -> [(c, cs)])

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- bind得到的一个Parser实例依然需要用parser才可以继续用的
{-
satisfied :: (Char -> Bool) -> Parser Char
satisfied pred = item `bind` \c ->
                      if pred c
                      then unit c
                      else (Parser (\cs -> []))
-}

{-
class Monad m where
    return :: a -> m a 
    (>>=) :: m a -> (a -> m b) -> m b
-}

instance Applicative Parser where
    pure a = Parser $ \str -> [(a,str)]
    f <*> a = do
      f' <- f
      a' <- a
      pure (f' a')

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

-- {bind: >>= unit: return}
instance Monad Parser where
    return a = Parser (\s -> [(a, s)])
    p >>= f = Parser (\s -> parse p s |>
                            map (\(a, s') -> parse (f a) s') |>
                            -- concat [[]] => []
                            concat)

-- MonadPlus has similar semantic with Monoid
-- A Monoid is parameterized over a type of kind *
-- A MonadPlus is parameterized over a type of kind * -> * just like Monad through the type constraint
{-
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
-}

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p q = Parser (\s -> (parse p) s ++ (parse q) s)

option :: Parser a -> Parser a -> Parser a
option p q = Parser (\s -> case parse (mplus p q) s of
                             []     -> []
                             -- 如果第一个成功了就取第一个结果 如果第一个失败了那么就取第二个结果 pattern match magic here
                             (x:xs) -> [x])

satisfied :: (Char -> Bool) -> Parser Char
satisfied pred = item >>= (\c ->
                           if pred c
                           then return c
                           else mzero)
                                                    
char :: Char -> Parser Char
char c = satisfied (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs) }

-- 0 or more applications of Parser p
many0 :: Parser a -> Parser [a]
many0 p = many1 p `option` return []

-- 1 or more applications of Parser p
many1 :: Parser a -> Parser [a]
many1 p = do { a<- p; as <- many0 p; return (a:as) }

--sepBy
sepBy0 :: Parser a -> Parser b -> Parser [a]
p `sepBy0` sep = (p `sepBy1` sep) `option` return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do a <- p
                    as <- many0 $ do {sep; p}
                    return (a:as)
