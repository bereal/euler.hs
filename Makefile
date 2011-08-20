all:
	ghc -XParallelListComp -O2 -o euler euler.hs
