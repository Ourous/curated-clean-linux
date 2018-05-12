#
#	Version 1.0.3
#	Ronny Wichers Schreur
#	ronny@cs.kun.nl
#
CLM=clm

CC=gcc
COPTIONS=-Wall -pedantic -O

printenv : printenv.o ArgEnv.o ArgEnvC.o
	$(CLM) -b -nt printenv -o printenv

ArgEnvC.o : ArgEnvC.c
	$(CC) $(COPTIONS) -c ArgEnvC.c
	mkdir -p "Clean System Files"
	cp ArgEnvC.o "Clean System Files/ArgEnvC.o"

ArgEnv.o : ArgEnv.icl ArgEnv.dcl
	$(CLM) -O ArgEnv

printenv.o : printenv.icl ArgEnv.dcl
	$(CLM) -O printenv

cleanup :
	$(RM) printenv *.o *.abc
