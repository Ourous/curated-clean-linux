# Curated Clean

The intention of the Curated Clean project is to provide a current and stable version of the Clean programming language.
Curated Clean is based off of the nightly Clean builds found at ftp://ftp.cs.ru.nl/pub/Clean/builds/

Currently, it is composed of:

	.
	
All other contents are from:
	
	12 May 2018
	
With these changes made to allow compilation:
	
	Specializations for UNIT added to several modules in the Generics library
	Dependencies on the old Data.Generics modules have been re-pointed to their new locations
	
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
