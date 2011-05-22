module Lamda where
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

--------------------------------------------------------------------------------
-- Lamda term definition
--------------------------------------------------------------------------------

data Term = Var Char | App Term Term | Lam Char Term | I | K | S | B | C deriving Eq

instance Show Term where
	show I 		= "I"
	show K 		= "K"
	show S 		= "S"
	show B		= "B"
	show C 		= "C"
	show (Var x)	= [x]
	show (App a b)  = "(" ++ show a ++ " " ++ (show b) ++ ")"
	show (Lam x t)	= "\\" ++ [x] ++ "." ++ (show t)

--------------------------------------------------------------------------------
-- Functions for working with lamda terms
--------------------------------------------------------------------------------

-- Free values
fv:: Term -> [Term]
fv (Var a)    = [Var a]
fv (Lam a b)  = remove (fv b) (Var a)
fv (App a b)  = (fv a) ++ (fv b)
fv _	      = []

--------------------------------------------------------------------------------
-- Auxiliary functions for working with lists as sets
--------------------------------------------------------------------------------

-- Remove an element from set
remove [] a    = []
remove (x:s) a
	| x == a    = remove s a
	| otherwise = x : (remove s a)

-- Remove elements of the second set from the first one
setminus xs []     = xs
setminus xs (y:ys) = setminus (remove xs y) ys

-- Check whether the set contains the element
inset [] a     = False
inset (x:xs) a
	| x == a    = True
	| otherwise = inset xs a
