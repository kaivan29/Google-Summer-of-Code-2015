##GenProg
Experimenting with Genprog library to create random trees and learn its behavior.

###Installation:
To make load GenProg Library one needs to have 'ghci version 7.6.3 or 7.10.*'. It will give you a lot of problems otherwise. For me Cabal 1.22 and ghc-7.10 combination worked. Genprog installs perfectly fine. A change is needed to be made to install it though, Steps:
<br>1. cabal get genprog-0.1.0.2. Unzip the file.
<br>2. Open genprog.cabal in text editor and set base to 4.8.* which is 4.6.* originally.
<br>3. run cabal install in the folder.
<br>4. set the package in ghci. Works like a charm.

###Experimentation:
After experimenting with genprog a bit, we made a proper eval function and test it on handmade operation trees. If everything looked good, we would have a very clear idea of what we want genprog to be able to output, mutate and crossover. But, when generating expressions we were not able to have foldr expressions as nodes. GenProg just pays attention to whether it's adding a terminal or nonterminal, not which types they are. Likewise, in the crossover, it just looks at whether the nodes are internal or external (nonterminal or terminal). So we could have decided to meddle with GenProg library or start from scratch.
Setback since, we had not expected to change anything in GenProg.

###Conclusion:
We have decided to code Genetic Programming from the scratch and we are going to try two ways to do it:
1. Existential Prototypes.
2. GADTs