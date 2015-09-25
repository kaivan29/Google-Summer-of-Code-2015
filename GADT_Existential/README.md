#Proto
Experimenting with existential typeclasses and GADTs so that we can make valid trees.

##Existential
Existential types, or 'existentials' for short, are a way of 'squashing' a group of types into one, single type. The existential quantification in datatypes (and newtypes) declarations is a powerful mechanism that can be used to incapsulate information or even provide stronger type guarantees.

###Experimentation:
After experimenting it was found that dealing with existentials is weird in a way. The thing about existentials is that one has to work really hard to get something usable and useful but in the end one gets something that could be done exactly the same with just normal data types. Typically people try to use existentials to port over design intuitions they've learned from other languages like object oriented programming but in the end it leads to complex and hard-to-reason about code with a lot of extra scaffolding one has to write him/herself...
Also, https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/ 

###Conclusion:
We turn to GADTs

##GADT
Generalised Algebraic Datatypes(GADT) are datatypes for which a constructor has a non standard type. It allows us to give the type signatures of constructors explicitly.

###Experimentation:
Learning how to implement GADTs by generating simple expressions.

###Conclusion:
GADT is the correct approach to build valid trees.
