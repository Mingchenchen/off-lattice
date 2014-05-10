Off-lattice protein folder
============================

Installation
------------

To compile and install, first make sure you have haskell platform installed.

Make sure that the LA package is installed as well (https://github.com/chalmers-kandidat14/LA)

Then run:
    
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build
    $ cabal install

There is now a runnable file in the location 
"(path to project)/dist/build/off-lattice". On windows, it is called 
"off-lattice.exe".

To see how it is used run it with

    $ <path to project>/dist/build/off-lattice/off-lattice -h
    
If you have the cabal bin directory in your path, you can use
    
    $ off-lattice -h


Usage
-----

To make a ordinary run with the resulting chain printed to stdout:

    $ off-lattice -c <residues> -i <iterations>

To make a large run of the algorithm where the chain is folded several times
you use the following command:
    
    $ off-lattice -l <number of run> -c <residues> -i <iterations>
