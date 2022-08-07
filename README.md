# Arrows
A computational algebra system in [Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev).

The goal of the project is to build an extensible and mathematically rigorous environment where one can construct mathematical objects (such as groups, rings, modules, chain complexes, schemes, sheaves, etc) and operate with them, their morphisms and their elements.

Some of the objects currently implemented include: permutation groups, linear groups, finite rings (ℤ/mℤ, ℤ/(p^k)ℤ, Galois rings) and finite fields, polynomial rings over arbitrary commutative rings, affine algebras, number fields, function fields, finitely presented modules over arbitrary rings, bounded (co)chain complexes (of modules, sheaves of modules, or other complexes), finite (finitely generated as modules) associative algebras, finite distributive (not necessarily associative) algebras, schemes (affine schemes, and closed subschemes of affine or projective space), and sheaves of modules.

### Where to start?
The most fundamental classes of the system are *Domain* and *Morphism*, corresponding to *objects* and *arrows* of category theory. Most objects in the system are algebraic structures such as groups, rings and modules.

### Smalltalk
The system is based on Smalltalk-80, in particular Cuis Smalltalk. It is multiplatform and runs with the OpenSmalltalk Virtual Machine. Conveniently, Smalltalk-80 comes equipped with arbitrary precision integers and rationals.

As many systems based on Smalltalk-80, it comes complete with an environment for interactive programming, blurring the distinction between user and programmer. It contains a complete development environment that allows you to browse classes, inspect objects, debug, and change anything 'live' while it is running.

### Notation
The notation is intended to be as close as possible to standard mathematical notation, yet adhering completly to Smalltalk syntax. For this we use unicode in Smalltalk code. Special characters can be input by typing the backslash character followed by the character name, similar to LaTeX. For example, ℤ can be input as \\Z, α as \\alpha and ⊗ as \\otimes.

Groups use multiplicative notation with the &centerdot; (\\cdot) binary operator, and the same operator is used for composition of morphisms, so that automorphisms can seemlessly be group elements. Rings and modules use operator \* for multiplication, and ring elements also support the &centerdot; operator as a synonym in order to seemlessly be members of groups of ring units. The ! operator is used for coercion. The ^ operator is used for exponentiation as well as conjugation by group elements, and for creating free modules. &oplus; (\\oplus) is used for direct sums, &otimes; (\\otimes) for tensor products, &times; (\\times) for cartesian product, direct product of groups, ring products, and in general for categorical products. Intersection or meet is implemented with the /\\ operator (as well as pullback of morphisms), and union or join with \\/ (as well as pushout of morphisms). The double arrow =\> is used for the internal hom functor, -\> for accessing canonical morphisms between objects, and the branching arrows -\< and \>- are used for retrieving canonical projections and coprojections in objects that were constructed as products or coproducts.

### Tests
Unit tests are used to verify the algebraic structures behave as they are supposed to. The simplest tests verify *axioms*. For example, a test for a ring would create random elements of the ring and verify associativity, distributivity, etc. Other tests verify *theorems*: given a theorem, the test builds random examples and corner cases and verifies the theorem holds in those particular cases.

### Bibliography
\[AL94\] William W. Adams; Philippe Loustaunau; "An Introduction To Groebner Bases", GSM Vol 3, AMS (1994) 
\[ADL06\] Nadia Ben Atti, Gema M. Diaz-Toca, Henri Lombardi, "The Berlekamp-Massey Algorithm Revisited" (2006) 
\[BW93\] Thomas Becker; Volker Weispfenning; "Groebner Bases: A Computational Approach to Commutative Algebra" (1993) 
\[BLH11\] Mohamed Barakat; Markus Lange-Hegermann; "An Axiomatic Setup for Algorithmic Homological Algebra and an Alternative Approach to Localization" (2017) 
\[BF15\] Bini, G.; Flamini, F. "Finite commutative rings and their applications" (2015) 
\[Coh96\] Henri Cohen, "A Course in Computational Algebraic Number Theory" (1996) 
\[CLO97\] David Cox; John Little; Donals O''Shea; "Ideals, Varieties and Algorithms", UTM, Springer, 1997 
\[CLO05\] David Cox; John Little; Donals O''Shea; "Using Algebraic Geometry", GTM, Springer, 2005 
\[DK02\] Harm Derksen; Gregor Kemper; "Computational Invariant Theory" 
\[Dix82\] John D. Dixon, "Exact Solution of Linear Equations Using p-Adic Expansions" (1982) 
\[Eis95\] David Eisenbud; "Commutative Algebra With a View Towards Algebraic Geometry", GTM 150, Springer, 1995 
\[EH99\] David Eisenbud; Joe Harris; "The Geometry of Schemes", GTM 197, Springer, 1999 
\[ESS02\] David Eisenbud; D. Grayson; M. Stillman; B. Sturmfels; "Computations in Algebraic Geometry with Macaulay 2" (2002) 
\[EIP00\] Michele Elia; J. Carmelo Interlando; Reginaldo Palazzo Jr.; "Computing the reciprocal of units in Galois rings" (2000) 
\[EH19\] Christian Eder; Tommy Hofmann; "Efficient Groebner Bases Computation over Principal Ideal Rings" (2019) 
\[EPP18\] Christian Eder; Gerhard Pfister; Adrian Popescu; "Standard Bases over Euclidean Domains" (2018) 
\[Fie71\] D. E. Fields; "Zero divisors and nilpotent elements in power series rings" (1971) 
\[FH16\] Claus Flieker, Tommy Hofmann, "Computing in quotients of rings of integers" (2016) 
\[Fle69\] Fletcher, C. R., "Unique Factorization Rings" (1969) 
\[Gal78\] Steven Galovich, "Unique Factorization Rings with Zero Divisors" (1978) 
\[GM73\] Ganske, G.; McDonald, B.R. "Finite local rings". Rocky Mountain J. Math. 3 (1973), no. 4, 521-540 
\[GP08\] Gert-Martin Greuel; Gerhard Pfister; "A Singular Introduction to Commutative Algebra" (2008) 
\[Har77\] Robin Hartshorne; "Algebraic Geometry", GTM 52, Springer, 1977 
\[HEO05\] Derek F. Holt; B. Eick; E. A. O''Brien; "Handbook of Computational Group Theory" (2005) 
\[HH11\] Jurgen Herzog, Takayuki Hibi, "Monomial Ideals" (2011) 
\[How86\] John A. Howell, "Spans in the module (Z/mZ)^s" (1986) 
\[KY86\] Mitsuo Kanemitsu; Ken-ichi Yoshida; "Euclidean rings" (1986) 
\[KSWZ17\] Deepak Kapur, Yao Sun, Dingkang Wang, Jie Zhou, "The Generalized Rabinowitsch Trick" (2017) 
\[Kem11\] Gregor Kemper; "A Course in Commutative Algebra", GTM 256, Springer, 2011 
\[Mat11\] Keith Matthews, "Solving AX=B using the Hermite normal form" (2011) 
\[McC42\] N. H. McCoy; "Remarks on divisors of zero" (1942) 
\[McD74\] Bernard R. McDonald; "Finite Rings with Identity" (1974) 
\[Mis93\] Mishra, B; "Algorithmic Algebra", Springer, Monographs in Computer Science, 1993 
\[Mon04\] Michael Monagan; ''Maximal Quotient Rational Reconstruction: An Almost Optimal Algorithm for Rational Reconstructoin'' (2004) 
\[Pos17\] Sebastian Posur; "Linear Systems over Localizations of Rings" (2017) 
\[SY09\] Michael Sagraloff, Chee K. Yap, "An Efficient and Exact Subdivision Algorithm for Isolating Complex Roots of a Polynomial and its Complexity Analysis" (2009) 
\[Sam71\] Pierre Samuel, "About Euclidean Rings", J. Algebra, 19:282-301, 1971 
\[San11\] Metod Saniga; "Projective Lines over Finite Rings", 2011 
\[Sho09\] Victor Shoup; "A Computational Introduction to Number Theory and Algebra" (2009) 
\[Sim70\], Charles C. Sims, "Computational methods in the study of permutation groups" (1970) 
\[SM98\] Arne Storjohann, Thom Mulders, "Fast algorithms for linear algebra modulo N" (1998) 
\[Sto00\] Arne Storjohann, "Algorithms for Matrix Canonical Forms" (2000) 
\[Vak17\] Ravi Vakil, "The Rising Sea: Foundations of Algebraic Geometry" (2017) 

Two other invaluable references are [the nLab](https://ncatlab.org/nlab/show/HomePage) and [The Stacks Project](https://stacks.math.columbia.edu/browse).



