# PhitSolver

## What

PhitSolver is a Haskell program to solve puzzles from the Phit Droid puzzle game
on Android (http://www.mtoy.biz/phitdroid2ndEdition.html).

## Usage

Pieces are described by `#` characters. Leave an empty line between every piece.
The first piece given is considered as the board.

## Example

    $ ghc --make PhitSolver.hs
    [1 of 1] Compiling Main             ( PhitSolver.hs, PhitSolver.o )
    Linking PhitSolver ...
    $ cat input.txt
    ####
    ####
    ####
    ####

    ###
    
    #
    ##
    
    ##
    
    #
    #
    
    ##
    #
    
    ##
     #
    $ cat input.txt | ./PhitSolver
    aadd
    eabb
    eebc
    fffc
