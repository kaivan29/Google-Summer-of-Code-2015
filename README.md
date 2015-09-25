#Google Summer of Code 2015
Prototype versions of the Atomspace and MOSES deme implemented in Haskell.
Genetic programming is an evolutionary technique, inspired by biological evolution, to evolve programs for solving specific problems. A genetic program is represented by a value of an algebraic datatype and associated with a custom-defined fitness value indicating the quality of the solution. Starting from a randomly generated initial population of genetic programs, the genetic operators of selection, crossover, and mutation are used to evolve programs of increasingly better quality.

##GenProg
Experimenting with Genprog library to create random trees and learn its behavior.

###Installation
To make load GenProg Library one needs to have 'ghci version 7.6.3 or 7.10.*'. It will give you a lot of problems otherwise. For me Cabal 1.22 and ghc-7.10 combination worked. Genprog installs perfectly fine. A change is needed to be made to install it though, Steps:
<br>1. cabal get genprog-0.1.0.2. Unzip the file.
<br>2. Open genprog.cabal in text editor and set base to 4.8.* which is 4.6.* originally.
<br>3. run cabal install in the folder.
<br>4. set the package in ghci. Works like a charm.

###Experimentation
After experimenting with genprog a bit, we made a proper eval function and test it on handmade operation trees. If everything looked good, we would have a very clear idea of what we want genprog to be able to output, mutate and crossover. But, when generating expressions we were not able to have foldr expressions as nodes. GenProg just pays attention to whether it's adding a terminal or nonterminal, not which types they are. Likewise, in the crossover, it just looks at whether the nodes are internal or external (nonterminal or terminal). So we could have decided to meddle with GenProg library or start from scratch.
Setback since, we had not expected to change anything in GenProg.

###Conclusion
We have decided to code Genetic Programming from the scratch and we are going to try two ways to do it:
1. Existential Prototypes.
2. GADTs

##Proto
Experimenting with existential typeclasses and GADTs so that we can make valid trees.

###Experimentation
After experimenting it was found that dealing with existentials is weird in a way. The thing about existentials is that one has to work really hard to get something usable and useful but in the end one gets something that could be done exactly the same with just normal data types. Typically people try to use existentials to port over design intuitions they've learned from other languages like object oriented programming but in the end it leads to complex and hard-to-reason about code with a lot of extra scaffolding one has to write him/herself...
Also, https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/ 

###Conclusion
We turn to GADTs

#ExprGADT.hs
After all the experimentation, GADT is the correct approach to build valid trees. Huff.

###Experimentation
With such stongly typed GP it is not possible to build an AST with unbound variables. Expr now carries a type level list of bound variables from shallowest to deepest.
`Expr [Int] a` is a lambda expression that is expecting Expr vs Int, and is an Expr [] a when applied.

So, Example :: Expr (a ': vs) b -> Expr vs a -> Expr vs b
Example takes an Expr that is waiting for an "a" at the outermost level and then gives an Expr that provides "b". In both the cases "vs" is the variables available to both. Variables are "holes" : V :: Indexor vs a -> Expr vs a.

Indexor is a GADT here.
IZ :: Indexor (k ': ks) k
An Indexor is IZ, which is the most outer-level bound variable. IS IZ, the second most outer level bound variable, IS (IS IZ) the third most and so on. IZ can only construct Indexor's whose list parameter is at least of lenght 1.  IZ's type has to have the second parameter (the index-into type) be the head of the first parameter, so you can't IZ into something with no variables in the environmet.
<br>For Example:
<br>1. If we have Expr ‘[Int] a, then V IZ :: Expr ‘[Int] Int
<br>2. If you have Expr '[Int, Bool] a, then V IZ :: Expr '[Int, Bool] Int
<br>3. And so you can't have V (IS (IS IZ)) or V IZ for Expr '[] a because it just doesn’t typecheck.

The kind of '[Int, Bool, String] is [*]
because Int, Bool, and String are of type '*'. So, one can not have '[Int, Bool, String, Maybe], becase Maybe is * -> *. Type level lists are necessarily phantom types. So, you can't actually have a value of type '[Int,Bool,String].

Note: phantom type is something we call that is in the type of your type as a parameter
but you don't have any actual values of that type in the type itself. It is just there to help you typecheck things and give you type safety.
Note: ' means type-level. Example - '[Int, Bool, String] is a type level list with elements Int, Bool, and String. (:) is value-level cons, and (':) is type-level cons.

So, IZ will typecheck with Indexor '[Int, String, Bool] Int or the more technical term is 'unify'. IZ will unify with Indexor '[Int, String, Bool] Int. ks can be anything so, IZ will unify with *any* ks and *any* k, just like Nothing will unify with *any* a for Maybe a i.e if you have Nothing, it can be Maybe Int, Maybe Bool, etc.

Example:
I :: Expr vs Int
I 5 can unify with Expr vs Int
I :: Int -> Expr vs Int
I 5 can unify with Expr '[Int] Int
it can unify with Expr '[Int, Bool] Int
it can unify with Expr '[Bool, String] Int

Now,
V doesn't directly care, but because we need an Indexor to make the V, it limits what Expr's you can make. (\x y -> x + y) would be translated as (V IZ + V (IS IZ)). That is what V gives us.
The magic happens with (:$)
(:$) :: Expr (a ': vs) b -> Expr vs a -> Expr vs b
And it is like ($) in Haskell normally
it takes an Expr that returns 'b' but is waiting for an 'a' and then an Expr that returns 'a' and gives an Expr that returns a 'b'. It "evaluates the outer level". So it turns an Expr (a ': vs) b into an Expr vs b by plugging in the whole a.
Example:
<br>V IZ :$ I 5  is I 5
<br>:$ (V IZ) (I 4) is (I 4) like (\x -> x) $ 4 in haskell.

V IZ is waiting for a value which is a hole and (:$) lets you fill the hole. So you can do
((V IZ - V (IS IZ) :$ I 5) :$ I 2 is like ((\x y -> x - y) $ 5) $ 2 i.e 5 - 2. (:$) does the hole-filling-in just like ($) in normal Haskell.

###Conclusion
V gives you a lambda like construct which we can use to evaluate fold expressions now. Foldr (V IZ + V (IS IZ)) (I 0) (List [1,2,3]) is foldr (\x y -> x + y) 0 [1,2,3]. Generated Trees with Integer, Boolean and List nodes successfully.

##Things done:
Generation of valid of trees with random nodes i.e random generation of population.

##Things to be done:
Crossover, mutation and fitness function.
