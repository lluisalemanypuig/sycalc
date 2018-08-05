# Sycalc

Sycalc is a small SYmbolic CALCulator implemented with [SWI-Prolog](http://www.swi-prolog.org/).
It provides a series of algorithms to perform basic arithmetic operations between multivariate
polynomial expressions.

Not only it offers an interactive mode, but it can also be used to make larger programs. Any
program can be compiled using the provided code by following the example in [this Makefile](https://github.com/lluisalemanypuig/sycalc/blob/master/build/Makefile.release).

The full documentation in html (generated using [DYP](https://github.com/lluisalemanypuig/docyourprolog))
can be found [here](https://github.com/lluisalemanypuig/sycalc/tree/master/docs).

Here are a few basic examples:
- Compute the product of several polynomials:

        ?- polynomial_expression_evaluation( n*(n - 1)*(n - 2)*(n - 3), R ).
        R = n^4-6*n^3+11*n^2-6*n.
        
- Compute the integer roots of a univariate polynomial:

        ?- integer_roots_unipolynomial(n^3-6*n^2+11*n-6, R).
        R = [1, 2, 3].

- Compute the polynomial that gives the sum of the first _n_ naturals raised to the power 10:

        P(n) = \sum_{i=1}^n i^10
        
        ?- power_sums(10, P).
        P = 1/11*n^11+1/2*n^10+5/6*n^9-n^7+n^5-1/2*n^3+5/66*n.

- Obtain the polynomial that corresponds to the binomial _n choose 7_:

        ?- polynomial_expression_evaluation( choose(n,7), B ).
        B = 1/5040*n^7-1/240*n^6+5/144*n^5-7/48*n^4+29/90*n^3-7/20*n^2+1/7*n.

- Compute the square of a multivariate polynomial:

         ?- polynomial_expression_evaluation( (z*q + y - x^2)^2, P ).
         P = -2*q*x^2*z + 2*q*y*z + q^2*z^2 - 2*x^2*y + x^4 + y^2.

- Perform composition of functions:
    - Compose the univariate polynomial P(x) = x^2 with the multivariate polynomial Q(x,y) = 3*x + y,
    that is, compute P(Q(x,y)):
    
            ?- P=x^2, Q=3*x + y, polynomial_composition(x,Q, P, C).
            C = 6*x*y+9*x^2+y^2.

    - Compose the multivariate polynomial P(x,y) = x^2 + y^2 with the multivariate polynomial Q(x,y) = 3*x + y.
    In particular, compute P(Q(x,y), y) and P(x, Q(x,y))
    
            ?- P=x^2 + y^2, Q=3*x+y, polynomial_composition(x,Q, P, C).
            C = 6*x*y + 9*x^2 + 2*y^2.
            
            ?- P=x^2 + y^2, Q=3*x + y, polynomial_composition(y,Q, P, C).
            C = 6*x*y + 10*x^2 + y^2.
