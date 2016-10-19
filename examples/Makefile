SHELL = /bin/bash

.SUFFIXES:
.SUFFIXES: .ml .out

all: simple featTesting

simple: simpleC

simpleC: simple/simple.ml
	../compileEx.sh simple/simple.ml

simpleD: simple/simple.ml
	../desugar.sh simple/simple.ml

simpleR: 
	if [ ! -e "simple.out" ]; then make simple; fi; ./simple.out

dircomp: simpleC

dircompC: dircomp/dircomp.ml
	../compileEx.sh dircomp/dircomp.ml

dircompD: dircomp/dircomp.ml
	../desugar.sh dircomp/dircomp.ml

dircompR:
	if [ ! -e "dircomp.out" ]; then make dircomp; fi; ./dircomp.out


oopsla: oopslaC

oopslaC: oopsla/oopsla.ml
	../compileEx.sh oopsla/oopsla.ml

oopslaD: oopsla/oopsla.ml
	../desugar.sh oopsla/oopsla.ml

oopslaR:
	if [ ! -e "oopsla.out" ]; then make oopsla; fi; ./oopsla.out

paperEx: paperExC

paperExC: paperEx/paperEx.ml
	../compileEx.sh paperEx/paperEx.ml

paperExD: paperEx/paperEx.ml
	../desugar.sh paperEx/paperEx.ml

paperExR:
	if [ ! -e "paperEx.out" ]; then make paperEx; fi; ./paperEx.out

accounts: accountsC

accountsC : accounts.ml
	../compileEx.sh accounts.ml

accountsD : accounts.ml
	../desugar.sh accounts.ml

accountsR : 
	if [ ! -e "accounts.out" ]; then make accounts; fi; ./accounts.out

universal: universalC

universalC: universal/universal.ml
	../compileEx.sh universal/universal.ml

universalD: universal/universal.ml
	../desugar.sh universal/universal.ml

universalR: 
	if [ ! -e "universal.out" ]; then make universal; fi; ./universal.out

clean:
	rm -f simple.out oopsla.out universal.out ../accounts.out paperEx.out
