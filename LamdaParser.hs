module LamdaParser where
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Author: 	Jan Papousek (325494)
-- Contact:	xpapous1@fi.muni.cz
-- Year:	2010
--
-- Description:	This module contains definition of lamda term
--		and function for working with them
--
--		The source code has been created for purpose of studying
--		IA014 - Functional programming taught by Libor Skarvada
--		at Masaryk University.

--		More info you will find at http://fi.muni.cz/~libor/vyuka/IA014/
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Parser
import Lamda
--------------------------------------------------------------------------------
-- Lamda term parsers
--------------------------------------------------------------------------------
term :: Parser Term
term = atom `chainl1` return App

atom :: Parser Term
atom = var +++ lam +++ paren

var :: Parser Term
var = token letter >>= return . Var

lam :: Parser Term
lam = symbol "\\" >> variables >>= \v -> symbol "." >> term >>= return . mklam v
	where variables      = many1 (token letter)
	      mklam (x:[]) t = Lam x t
	      mklam (x:s)  t = Lam x (mklam s t)
	      

paren :: Parser Term
paren = bracket (symbol "(") term (symbol ")")

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- This function applies the lamda parser on string and returns formed term
runLP :: String -> Term
runLP inp = process $ papply term inp where process x = fst $ head x
