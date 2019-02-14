#!/bin/bash

# script based on Dennis's bash wizardry at https://github.com/TryItOnline/tiosetup

OWNDIR="$(cd "$(dirname "$0")"; pwd)"
if [[ ! -z "$1" ]]; then
	TARGET="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
	if [[ "$TARGET" == "$OWNDIR" ]]; then
		echo "Cannot install to current directory. Try running without arguments" >&2
		exit 1
	fi
	exit 1
	echo "Installing Clean to $TARGET"
	if [[ "$CLEAN_HOME" != "$TARGET" ]]; then
		echo "CLEAN_HOME needs to be set to $TARGET for Clean to work properly"
	fi
	export CLEAN_HOME=$TARGET
	rm -rf "$CLEAN_HOME"
	cp -r . "$CLEAN_HOME"
elif [[ ! -z "$CLEAN_HOME" ]]; then
	if [[ "$CLEAN_HOME" == "$OWNDIR" ]]; then
		echo "Repeating Clean setup and pre-compilation on existing installation"
	else
		echo "Replacing existing Clean installation located in $CLEAN_HOME"
		rm -rf "$CLEAN_HOME"
		cp -r . "$CLEAN_HOME"
	fi
fi

if [[ ! -z "$CLEAN_HOME" ]]; then
	if [[ ":$PATH:" != *":$CLEAN_HOME/bin:"* ]]; then
		echo "Consider adding $CLEAN_HOME/bin to your PATH"
		PATH=$CLEAN_HOME/bin:$PATH
	fi
	echo "Performing Clean setup and pre-compilation"
else
	echo "CLEAN_HOME needs to be set to the final install location for Clean to work properly"
	echo "Performing Clean setup and pre-compilation in-place"
	export CLEAN_HOME=$MYDIR
fi


err=0
trap 'err=1' ERR

cd $CLEAN_HOME/lib
shopt -s nullglob globstar
after=0

while
	printf .
	before=$after
	after=($(
		for d in [[:upper:]]*; do
			cd $d
			for f in **/[[:alpha:]]*.dcl; do
				if [[ $f =~ ^Deprecated|^ESMVizTool|^Internet ]]; then
					continue
				fi
				f=${f%.*}
				f=${f//\//.}
				printf 'module main\nimport %s\nStart = ""\n' $f > main.icl
				clm -dynamics -IL Dynamics -IL Gast -IL GraphCopy -IL Generics -IL Platform -IL Sapl -IL TCPIP -IL ArgEnv -IL StdLib -IL Directory -IL compiler -IL compiler/frontend -IL compiler/backend -IL compiler/main -IL compiler/main/Unix main -o main.out
			done
			rm {.,Clean\ System\ Files}/main.*
			cd ..
		done 2> .stderr | sha512sum
	))
	[[ $before != $after ]]
do
	:
done

printf ' done.\nThe following errors occurred during the last pass:\n'
grep -Po 'Error((?!Error).)*' .stderr | cat -n
rm .stderr

exit "$err"
