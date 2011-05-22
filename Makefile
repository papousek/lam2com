ghc6:
	ghc6 -o lam2com --make Main;
	rm *.hi;
	rm *.o;

ghc:	
	ghc -o lam2com --make Main;
	rm *.hi;
	rm *.o;	
