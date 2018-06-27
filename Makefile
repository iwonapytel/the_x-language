all:
	cd parser && $(MAKE)
	ghc --make Interpreter.hs -i./parser -XScopedTypeVariables -o intepreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi intepreter
	cd parser && $(MAKE) clean
	-rm -f parser/Testdeklaracja
