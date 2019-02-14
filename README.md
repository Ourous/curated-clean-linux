# Curated Clean

The intention of the Curated Clean project is to provide a version of the Clean programming language more current than the infrequent full releases and more stable than the nightly builds, intended primarily for consumption by online IDE/compiler services.  
Curated Clean is based off of the nightly Clean builds found at ftp://ftp.cs.ru.nl/pub/Clean/builds/ and the repositories at https://gitlab.science.ru.nl/ with minimal changes. 

Currently, it is composed of:

	12 May 2018:
		lib/Generics
	13 February 2019:
		gitlab.science.ru.nl/clean-compiler-and-rts -> lib/compiler

All other contents are from:

	13 February 2019

With these changes made to allow compilation:

	Specializations for UNIT added to several modules in the Generics library

Known errors:

	Several modules in the Generics library no longer compile without nontrivial modification, using the equivalent modules in Platform is reccomended
	Clean.PrettyPrint appears to depend on code present in a version of the compiler source not generally available

Below follows Clean's official `README.md`:

# Using Clean

Compiling Clean programs on linux can be done using one of the following tools:

- clm: A build tool for simple projects. All compilation options and paths to libraries are specified as command line arguments
- cpm: A build tool that allows you to specify build configurations of programs in Clean project files (.prj).
       You can use cpm to create or edit project files and build those projects.

Both tools rely on the environment variable `CLEAN_HOME` to point to where you installed Clean to find libraries and executables.
So for example if you have unpacked this package to `/Users/myusername/clean` you should the following exports to your .bashrc/.profile/etc
```
export CLEAN_HOME=/Users/myusername/clean
export PATH=$PATH:$CLEAN_HOME/bin
```

# More Info

Additional information about Clean is can be found on the website:

  http://clean.cs.ru.nl

Bug reports, questions and suggestions are welcome. Please send them to:

  mailto:clean@cs.ru.nl
