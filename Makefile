ifeq ($(shell which erl), /opt/local/bin/erl)
	OS = Macports
else
    OS = $(shell uname)
endif

ERL = /usr/local/bin/erl
ERLC = /usr/local/bin/erlc

.PHONY: all clean test

all:
	$(MAKE) -f Makefile.$(OS)

Macports:
	$(MAKE) -f Makefile.Macports

Darwin:
	$(MAKE) -f Makefile.Darwin

Linux:
	$(MAKE) -f Makefile.Linux

clean:
	$(MAKE) -f Makefile.$(OS) clean

test:
	$(MAKE) -f Makefile.$(OS) test

Macports_test:
	$(MAKE) -f Makefile.Macports test

Darwin_test:
	$(MAKE) -f Makefile.Darwin test

Linux_test:
	$(MAKE) -f Makefile.Linux test
	
hello: all ./examples/hello/hello.erl
	@ echo "----------------------------------" 
	@ echo "Compiling and running .examples/hello/hello.erl:" 
	cd ./examples/hello \
&& $(ERLC) hello.erl \
&& $(ERL) -pa ../../ebin -s hello run -s init stop -noshell

minibench: ./examples/minibench/minibench.erl
	@ echo "----------------------------------" 
	@ echo "Compiling and running minibench.erl:" 
	cd ./examples/minibench \
&& $(ERLC) minibench.erl \
&& $(ERL) -pa ../../ebin -s minibench run -s init stop -noshell

minibench2: ./examples/minibench/minibench2.erl
	@ echo "----------------------------------" 
	@ echo "Compiling and running minibench2.erl:" 
	cd ./examples/minibench \
&& $(ERLC) minibench2.erl \
&& $(ERL) -pa ../../ebin -s minibench2 run -s init stop -noshell	

