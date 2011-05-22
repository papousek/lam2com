module Parser where
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Author: 	Jan Papousek (325494)
-- Contact:	xpapous1@fi.muni.cz
-- Year:	2010
--
-- Description:	This module contains parser definition and some simple parsers.
--
--		The source code has been created for purpose of studying
--		IA014 - Functional programming taught by Libor Skarvada
--		at Masaryk University.

--		More info you will find at http://fi.muni.cz/~libor/vyuka/IA014/
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Control.Monad
import Char
--------------------------------------------------------------------------------
-- Parser definition
--------------------------------------------------------------------------------
newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
	fmap f (P p) = P (\inp -> [(f v, out) | (v, out) <- p inp])

instance Monad Parser where
	return v     = P (\inp -> [(v,inp)])
	(P p) >>= f  = P (\inp -> concat [ papply (f v) out | (v, out) <- p inp])


instance MonadPlus Parser where
	mzero 			= P (\inp -> [])
	(P p) `mplus` (P q)	= P (\inp -> (p inp) ++ (q inp))

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

papply :: Parser a -> String -> [(a,String)]
papply (P p) = p

first :: Parser a -> Parser a
first(P p)   = P (\s -> case p s of
				[]  -> []
				x:t -> [x])		

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `mplus` q)

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
	     s <- many p
	     return (x:s)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

chainl1 :: Parser a ->  Parser (a -> a -> a) -> Parser a
chainl1 p q = do {a <- p; rest a}
		where
			rest a = (do f <- q
				     b <- p
				     rest (f a b))
				 +++ return a

--------------------------------------------------------------------------------
-- Some simple parsers
--------------------------------------------------------------------------------
item :: Parser Char
item = P (\x -> case x of
		[]    -> []
		(x:t) -> [(x,t)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
	   if (p x) then return x
		    else mzero

char :: Char -> Parser Char
char = sat . (==)

digit, lower, upper, letter, alphaNum :: Parser Char
	
digit    = sat isDigit
lower    = sat isLower
upper    = sat isUpper
letter   = lower `mplus` upper
alphaNum = letter `mplus` digit

string :: String -> Parser String
string ""    = return ""
string (x:s) = do char x
		  string s
		  return (x:s)

spaces :: Parser ()
spaces = many1 (sat isSpace) >> return ()

comment :: Parser()
comment = string "--" >> many (sat (/= '\n')) >> return ()

junk :: Parser ()
junk = many(spaces +++ comment) >> return ()

token :: Parser a -> Parser a
token p = do	v <- p
		junk
		return v

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket l c r = do l
		   content <- c
		   r
		   return content

symbol :: String -> Parser String
symbol = token . string
