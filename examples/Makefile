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

featTesting: featTestingC

featTestingC: featTesting/test.ml
	cd ..; ./compileEx.sh examples/featTesting/test.ml

featTestingD: featTesting/test.ml
	../desugar.sh featTesting/test.ml

featTestingR: 
	if [ ! -e "../test.out" ]; then make featTesting; fi; cd ..; ./test.out

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
	rm -f simple.out ../test.out universal.out ../accounts.out paperEx.out
