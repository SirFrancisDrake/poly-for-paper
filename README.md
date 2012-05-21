poly-for-paper
==============

A small and rather dirty script that one of my friends needed to make messy computations for his diploma paper.

First. Take all symmetrical polynomials of power n.
Second. Add some substitutions like x1^2 = x2 + y2.
Look at factor-space of first factor second.
Determine its dimensionality.
Profit. (Been used for some Grassmannian over C^6.)

What works: 
1. arithmetics for polynomials of multiple variables: add, subt, mult, multByCoeff 
    (typeclass Ring with first 3 operations added for that purpose)
2. letter substitution: substituteLetter
3. Symmetrization over given list of variables: symmetrizeOver
    ex: symmetrizeOver /poly "x"/ "xyz" = /poly "6x + 6y + 6z"/
4. substitutions of some monomials for some polynomials, goes in two ways:
    1. cLIPSnaive :: [(Polynomial -> Polynomial)] -> Polynomial -> Polynomial
        goes on using first substitution until fixed point, then goes on with the second etc.
    2. clips :: [(Polynomial -> Polynomial)] -> Polynomial -> Polynomial
        goes on with the first one until fixed point, applies the second one, tries the first one etc.
    example: [ "x" -> "z"
             , "y" -> "x + z"
             ] on polynomial "x + y + z"
        cLIPSnaive: "x + y + z" {first subst}-> "y + 2z" 
                                {second subst}-> "x + 3z"
                                over, because it doesn't return to the first subst
        clips: "x + y + z" {first subst}-> "y + 2z"
                           {second subst}-> "x + 3z"
                           {first subst}-> "4z"
                           over
5. some parsers for making polynomial strings into polynomials, topmost one being
    poly :: String -> Polynomial (throws an error if it doesn't like its input)
