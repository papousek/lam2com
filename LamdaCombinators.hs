module LamdaCombinators where
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Author: 	Jan Papousek (325494)
-- Contact:	xpapous1@fi.muni.cz
-- Year:	2010
--
-- Description:	This module convert common lamda terms to combinators.
--
--		The source code has been created for purpose of studying
--		IA014 - Functional programming taught by Libor Skarvada
--		at Masaryk University.

--		More info you will find at http://fi.muni.cz/~libor/vyuka/IA014/
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Lamda
--------------------------------------------------------------------------------
-- Conversion function
--------------------------------------------------------------------------------
lam2com :: Term -> Term

-- \x.x -> I
lam2com (Lam x (Var y))
	| x == y = I

-- \x.M x -> M
lam2com (Lam x (App a (Var b)))
	| (not $ inset (fv a) (Var x)) && (x == b) = lam2com a

-- \x.M -> K M
lam2com (Lam x t)
	| not $ inset (fv t) (Var x) = lam2com (App K (lam2com t))

-- \x.M N -> S (\x.M) (\x.N)
lam2com (Lam x (App a b)) = lam2com (App (lam2com (App S (lam2com (Lam x a)))) (lam2com(Lam x b)))

-- S (K M) N -> B M N
lam2com (App (App S (App K m)) n) = lam2com (App (App B (lam2com m)) (lam2com n))

-- S M (K N) -> C M N
lam2com (App (App S m) (App K n)) = lam2com (App (App C (lam2com m)) (lam2com n))

-- S (K M) (K N) -> K (M N)
lam2com (App (App S (App K m)) (App K n)) = lam2com (App K (App (lam2com m) (lam2com n)))

-- S (K M) I -> M
lam2com (App (App S (App K m)) I)= lam2com m

-- Removing lamda
lam2com (Lam x y) = lam2com (Lam x (lam2com y))

-- The term can not be no longer converted
lam2com a = a
