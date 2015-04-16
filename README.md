# gprm

The Glasgow Parallel Reduction Machine, a task-based parallel programming framework. Parallel programming finally made easy!



## PREREQUISITES:

### Compilers

- [g++ >= 4.8](http://gcc.gnu.org) for compiling the Virtual Machine and the SystemC model
- [ghc >= 7.6.3](http://www.haskell.org) to compile the Gannet compiler

### Dynamic languages

- [perl >= 5.12](http://www.perl.org) installed in `/usr/bin/perl`
- [python >= 2.5.1](http://www.python.org) for SCons

### Build tools

- [scons >= 1.2.0](http://www.scons.org) for building the Virtual Machine and the SystemC model

### Libraries

- Install the [Haskell Platform](http://hackage.haskell.org/platform/)

- Install the following Haskell packages with `cabal`:

    $ cabal install lens
    $ cabal install errors
    $ cabal install wl-pprint
    $ cabal install HsSyck

- Install the YAML::Syck Perl module from [CPAN](http://search.cpan.org/dist/YAML-Syck/), the easiest way is to install `cpanm` first, then do

    $ cpanm YAML::Syck

## INSTALLATION:

    # `git clone` the repo

The environment variable `GPRM_DIR` must be set to full path to the `gprm` directory.

    $ cd gprm
    $ export GPRM_DIR=$PWD

The file `etc/gprmc` contains the environment settings for GPRM.
To source it:

    $ . $GPRM_DIR/etc/gprmrc

That's it, you can start using GPRM!

## HOW TO USE:

Create a skeleton example `gprm-project-skeleton`:

    $ gprm -i

Or copy the example from `$GPRM_DIR/examples`. To build e.g. the `HelloWorld` example with 4 threads:

    $ gprm -t HelloTask -n 4

To run it:

    $ ./main
