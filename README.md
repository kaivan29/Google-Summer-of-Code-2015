— an extensive README markdown file explaining
—— what is there and why it’s there
—— what choices were made in creating the code, and what alternative
choices were rejected and why
—— what you see as next steps and challenges for ongoing development
(I may give some feedback on this part especially)

##Google Summer of Code 2015
Prototype versions of the Atomspace and MOSES deme implemented in Haskell.
Genetic programming is an evolutionary technique, inspired by biological evolution, to evolve programs for solving specific problems. A genetic program is represented by a value of an algebraic datatype and associated with a custom-defined fitness value indicating the quality of the solution. Starting from a randomly generated initial population of genetic programs, the genetic operators of selection, crossover, and mutation are used to evolve programs of increasingly better quality.

#GenProg
-Experimenting with Genprog library to create random trees and learn its behavior.

Installation
To make load GenProg Library one needs to have 'ghci version 7.6.3 or 7.10.*'. It will give you a lot of problems otherwise. For me Cabal 1.22 and ghc-7.10 combination worked. Genprog installs perfectly fine. A change is needed to be made to install it though, Steps:
<br>1. cabal get genprog-0.1.0.2. Unzip the file.
<br>2. Open genprog.cabal in text editor and set base to 4.8.* which is 4.6.* originally.
<br>3. run cabal install in the folder.
<br>4. set the package in ghci. Works like a charm.

Experimentation:
After experimenting with genprog a bit, we made a proper eval function and test it on handmade operation trees. If everything looked good, we would have a very clear idea of what we want genprog to be able to output, mutate and crossover. But, When generating expressions, GenProg just pays attention to whether it's adding a terminal or nonterminal, not which types they are. Likewise, in the crossover, it just looks at whether the nodes are internal or external (nonterminal or terminal).
Setback since, we had not expected to change anything in GenProg.

Conclusion: We have decided to code Genetic Programming from the scratch and we are going to try two ways to do it:
1. Existential Prototypes.
2. GADTs

#Proto
Experimenting with existential typeclasses and GADTs so that we can make valid trees.

After experimenting it was found that dealing with existentials is weird in a way. The thing about existentials is that one has to work really hard to get something usable and useful but in the end one gets something that could be done exactly the same with just normal data types. Typically people try to use existentials to port over design intuitions they've learned from other languages like object oriented programming but in the end it leads to complex and hard-to-reason about code with a lot of extra scaffolding one has to write him/herself...
Also, https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/ 

Conclusion: We turn to GADTs

#ExprGADT.hs
After all the experimentation, GADT is the correct approach to build valid trees. 


Things done:
Generation of valid of trees with random nodes i.e random generation of population.

Things to be done:
Crossover, mutation and fitness function.

