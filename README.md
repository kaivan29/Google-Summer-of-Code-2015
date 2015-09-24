# Google Summer of Code 2015
Prototype versions of the Atomspace and MOSES deme implemented in Haskell.

#GenProg
Experimenting with Genprog library to create random trees and learn its behavior.

#Proto
Experimenting with existential typeclasses and GADTs so that we can make valid trees.

#ExprGADT.hs
After all the experimentation, GADT is the correct approach to build valid trees. 
Genetic programming is an evolutionary technique, inspired by biological evolution, to evolve programs for solving specific problems. A genetic program is represented by a value of an algebraic datatype and associated with a custom-defined fitness value indicating the quality of the solution. Starting from a randomly generated initial population of genetic programs, the genetic operators of selection, crossover, and mutation are used to evolve programs of increasingly better quality.