module Main where
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Author: 	Jan Papousek (325494)
-- Contact:	xpapous1@fi.muni.cz
-- Year:	2010
--
-- Description:	This module contains only main function for executing
--		the program.
--
--		The source code has been created for purpose of studying
--		IA014 - Functional programming taught by Libor Skarvada
--		at Masaryk University.

--		More info you will find at http://fi.muni.cz/~libor/vyuka/IA014/
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import LamdaParser
import LamdaCombinators
--------------------------------------------------------------------------------
-- Main function
--	* load the lamda term
--	* convert the term into combinators
--	* show the term with combinators
--------------------------------------------------------------------------------

introduction = "Welcome to [lam2com] convertor.\n\nWrite the lamda term which you want to convert (for example \\x.x):"

main = do putStrLn introduction
	  v <- getLine
	  p <- (return $ lam2com $ runLP v)
	  putStrLn "\nThe same term using combinators S,K,I,B and C:"
	  putStrLn $ show p
	  return ()
