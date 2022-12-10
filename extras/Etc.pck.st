'From Cuis 6.0 [latest update: #5438] on 10 December 2022 at 11:43:00 am'!
'Description '!
!provides: 'Etc' 1 19!
!requires: 'Algebra' 1 7 nil!
SystemOrganization addCategory: #Etc!
SystemOrganization addCategory: #'Etc-Lie'!


!classDefinition: #IdentityArray category: #Etc!
Array variableSubclass: #IdentityArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'IdentityArray class' category: #Etc!
IdentityArray class
	instanceVariableNames: ''!

!classDefinition: #BitArray2 category: #Etc!
ArrayedCollection subclass: #BitArray2
	instanceVariableNames: 'bytes length'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BitArray2 class' category: #Etc!
BitArray2 class
	instanceVariableNames: ''!

!classDefinition: #StandardBasis category: #Etc!
SequenceableCollection subclass: #StandardBasis
	instanceVariableNames: 'elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'StandardBasis class' category: #Etc!
StandardBasis class
	instanceVariableNames: ''!

!classDefinition: #GroebnerBasis category: #Etc!
StandardBasis subclass: #GroebnerBasis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GroebnerBasis class' category: #Etc!
GroebnerBasis class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeFieldTest category: #Etc!
TestCase subclass: #SmallPrimeFieldTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeFieldTest class' category: #Etc!
SmallPrimeFieldTest class
	instanceVariableNames: ''!

!classDefinition: #DivisorGroup category: #Etc!
Group subclass: #DivisorGroup
	instanceVariableNames: 'scalars space'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'DivisorGroup class' category: #Etc!
DivisorGroup class
	instanceVariableNames: ''!

!classDefinition: #SemidirectProductGroup category: #Etc!
Group subclass: #SemidirectProductGroup
	instanceVariableNames: 'left right action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductGroup class' category: #Etc!
SemidirectProductGroup class
	instanceVariableNames: ''!

!classDefinition: #CoxeterGroup category: #'Etc-Lie'!
Group subclass: #CoxeterGroup
	instanceVariableNames: 'diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterGroup class' category: #'Etc-Lie'!
CoxeterGroup class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeField category: #Etc!
PrimeField subclass: #SmallPrimeField
	instanceVariableNames: 'expTable logTable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeField class' category: #Etc!
SmallPrimeField class
	instanceVariableNames: ''!

!classDefinition: #PowerAlgebra category: #Etc!
FiniteAlgebra subclass: #PowerAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PowerAlgebra class' category: #Etc!
PowerAlgebra class
	instanceVariableNames: ''!

!classDefinition: #CompletionRing category: #Etc!
Ring subclass: #CompletionRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'CompletionRing class' category: #Etc!
CompletionRing class
	instanceVariableNames: ''!

!classDefinition: #CoxeterSystem category: #'Etc-Lie'!
Structure subclass: #CoxeterSystem
	instanceVariableNames: 'group generators matrix diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterSystem class' category: #'Etc-Lie'!
CoxeterSystem class
	instanceVariableNames: ''!

!classDefinition: #Divisor category: #Etc!
GroupElement subclass: #Divisor
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Divisor class' category: #Etc!
Divisor class
	instanceVariableNames: ''!

!classDefinition: #SemidirectProductElement category: #Etc!
GroupElement subclass: #SemidirectProductElement
	instanceVariableNames: 'parent left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductElement class' category: #Etc!
SemidirectProductElement class
	instanceVariableNames: ''!

!classDefinition: #SmallPrimeResidue category: #Etc!
PrimeResidue subclass: #SmallPrimeResidue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmallPrimeResidue class' category: #Etc!
SmallPrimeResidue class
	instanceVariableNames: ''!

!classDefinition: #RelaxedAdicInteger category: #Etc!
RingElement subclass: #RelaxedAdicInteger
	instanceVariableNames: 'parent digits'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedAdicInteger class' category: #Etc!
RelaxedAdicInteger class
	instanceVariableNames: ''!

!classDefinition: #RelaxedAdicNumber category: #Etc!
RingElement subclass: #RelaxedAdicNumber
	instanceVariableNames: 'parent valuation unit precision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedAdicNumber class' category: #Etc!
RelaxedAdicNumber class
	instanceVariableNames: ''!

!classDefinition: #RelaxedPowerSeries category: #Etc!
RingElement subclass: #RelaxedPowerSeries
	instanceVariableNames: 'parent coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RelaxedPowerSeries class' category: #Etc!
RelaxedPowerSeries class
	instanceVariableNames: ''!

!classDefinition: #Sum category: #Etc!
ModuleElement subclass: #Sum
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Sum class' category: #Etc!
Sum class
	instanceVariableNames: ''!

!classDefinition: #TupleOld category: #Etc!
ModuleElement subclass: #TupleOld
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'TupleOld class' category: #Etc!
TupleOld class
	instanceVariableNames: ''!

!classDefinition: #EuclideanGaussianElimination category: #Etc!
GaussianElimination subclass: #EuclideanGaussianElimination
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'EuclideanGaussianElimination class' category: #Etc!
EuclideanGaussianElimination class
	instanceVariableNames: ''!

!classDefinition: #GaussBareiss category: #Etc!
GaussianElimination subclass: #GaussBareiss
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GaussBareiss class' category: #Etc!
GaussBareiss class
	instanceVariableNames: ''!

!classDefinition: #BerlekampMassey category: #Etc!
Object subclass: #BerlekampMassey
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BerlekampMassey class' category: #Etc!
BerlekampMassey class
	instanceVariableNames: ''!

!classDefinition: #BitTuple2 category: #Etc!
Object subclass: #BitTuple2
	instanceVariableNames: 'bits'
	classVariableNames: 'One Zero'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BitTuple2 class' category: #Etc!
BitTuple2 class
	instanceVariableNames: ''!

!classDefinition: #ExtendedSmithReduction category: #Etc!
Object subclass: #ExtendedSmithReduction
	instanceVariableNames: 'matrix elementaryDivisors L S R'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ExtendedSmithReduction class' category: #Etc!
ExtendedSmithReduction class
	instanceVariableNames: ''!

!classDefinition: #ModularHermiteReduction category: #Etc!
Object subclass: #ModularHermiteReduction
	instanceVariableNames: 'matrix HNF'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ModularHermiteReduction class' category: #Etc!
ModularHermiteReduction class
	instanceVariableNames: ''!

!classDefinition: #MultivariateReducer category: #Etc!
Object subclass: #MultivariateReducer
	instanceVariableNames: 'divisors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'MultivariateReducer class' category: #Etc!
MultivariateReducer class
	instanceVariableNames: ''!

!classDefinition: #GlobalMultivariateReducer category: #Etc!
MultivariateReducer subclass: #GlobalMultivariateReducer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'GlobalMultivariateReducer class' category: #Etc!
GlobalMultivariateReducer class
	instanceVariableNames: ''!

!classDefinition: #OEIS category: #Etc!
Object subclass: #OEIS
	instanceVariableNames: 'entries'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'OEIS class' category: #Etc!
OEIS class
	instanceVariableNames: ''!

!classDefinition: #RealBox category: #Etc!
Object subclass: #RealBox
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RealBox class' category: #Etc!
RealBox class
	instanceVariableNames: ''!

!classDefinition: #SmithReduction category: #Etc!
Object subclass: #SmithReduction
	instanceVariableNames: 'matrix invariants'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SmithReduction class' category: #Etc!
SmithReduction class
	instanceVariableNames: ''!

!classDefinition: #SparseTuple category: #Etc!
Object subclass: #SparseTuple
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SparseTuple class' category: #Etc!
SparseTuple class
	instanceVariableNames: ''!

!classDefinition: #SturmChain category: #Etc!
Object subclass: #SturmChain
	instanceVariableNames: 'chain'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SturmChain class' category: #Etc!
SturmChain class
	instanceVariableNames: ''!

!classDefinition: #SubgroupSeries category: #Etc!
Object subclass: #SubgroupSeries
	instanceVariableNames: 'subgroups'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SubgroupSeries class' category: #Etc!
SubgroupSeries class
	instanceVariableNames: ''!

!classDefinition: #CoxeterDiagram category: #'Etc-Lie'!
Object subclass: #CoxeterDiagram
	instanceVariableNames: 'matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterDiagram class' category: #'Etc-Lie'!
CoxeterDiagram class
	instanceVariableNames: ''!

!classDefinition: #RootSystem category: #'Etc-Lie'!
Object subclass: #RootSystem
	instanceVariableNames: 'roots ambient'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'RootSystem class' category: #'Etc-Lie'!
RootSystem class
	instanceVariableNames: ''!

!classDefinition: #RootSystemTest category: #'Etc-Lie'!
Object subclass: #RootSystemTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'RootSystemTest class' category: #'Etc-Lie'!
RootSystemTest class
	instanceVariableNames: ''!

!classDefinition: #PolynomialCode category: #Etc!
ProtoObject subclass: #PolynomialCode
	instanceVariableNames: 'generator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'PolynomialCode class' category: #Etc!
PolynomialCode class
	instanceVariableNames: ''!

!classDefinition: #CyclicCode category: #Etc!
PolynomialCode subclass: #CyclicCode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'CyclicCode class' category: #Etc!
CyclicCode class
	instanceVariableNames: ''!


!StandardBasis commentStamp: '<historical>' prior: 0!
Standard bases of polynomial ideals. A standard basis is nothing but a special set of generators with the property that the leading monomials generate the leading ideal.

When the choosen monomial ordering is global (a well-ordering), these are Groebner bases and are computed with Buchberger's algorithm. For arbitrary monomial orderings, they are computed using Mora's normal form.

See also Buchberger and ExtendedBuchberger.!

!GroebnerBasis commentStamp: '<historical>' prior: 0!
Groebner bases are a special case of standard bases.

For a fixed global monomial ordering, a subset G = {g1,...,gm} of an ideal I = <f1,...,fm> of a polynomial ring is called a Groebner basis for I if every polynomial f in I is reducible by G, i.e. there exists g in G such that its leading monomial LM(g) divides LM(f).

When all generators {f1,...,fm} are linear, a Groebner basis corresponds to "row echelon form" or "triangular system"; when they are univariate, then a Groebner basis corresponds to gcd(f1,...,fm). In general, a Groebner basis for an ideal I consists of all the "smallest polynomials" in I under the given monomial ordering.

See also Buchberger and ExtendedBuchberger.!

!SemidirectProductGroup commentStamp: '<historical>' prior: 0!
Semidirect products of groups.!

!SmallPrimeField commentStamp: '<historical>' prior: 0!
Prime fields Z/pZ for small primes p, such that it is feasable to build exponentiation and logarithmic tables and accelerate arithmetic through table lookups.

The initialization of the tables is very fast. The main concern is memory usage. For a field Z/pZ, each of the two tables have p-1 elements. They are WordArrays (32 bits per entry), so a field Z/pZ uses about 8*(p-1) bytes of memory. For example, Z/65537 uses 524K of memory, and Z/1048583 (a 20-bits modulus) uses about 8M.
!

!CompletionRing commentStamp: '<historical>' prior: 0!
Completion of a given commutative ring R at a given maximal ideal I. This is the inverse limit of the quotient rings R / I^n.!

!Divisor commentStamp: '<historical>' prior: 0!
Formal sums with integer or rational coefficients of prime divisors (integral closed subschemes of codimension 1) of an integral locally Noetherian scheme.!

!SemidirectProductElement commentStamp: '<historical>' prior: 0!
Elements of a semidirect product of groups. See SemidirectProductGroup.!

!RelaxedAdicInteger commentStamp: '<historical>' prior: 0!
Elements of the p-adic ring Zp. See AdicRing.!

!RelaxedAdicNumber commentStamp: '<historical>' prior: 0!
Elements of the p-adic field Qp. See AdicField.

A p-adic number z is represented uniquely by an integer k (the valuation) and a unit u in Zp, such that z = (p^k)*u.!

!RelaxedPowerSeries commentStamp: '<historical>' prior: 0!
Univariate formal power series with coefficients over a commutative ring. See PowerSeriesRing.!

!Sum commentStamp: '<historical>' prior: 0!
Formal sums (or formal linear combinations) of arbitrary symbols with coefficients over a given ring R. They can be thought of as Tuples indexed not by positive integers but by arbitrary symbols. They form free R-modules (see FreeModule).!

!TupleOld commentStamp: '<historical>' prior: 0!
R-tuples with coefficients in a ring R, i.e. finite ordered lists of n elements of R indexed by integers 1 to n. They are elements of free modules R^n (see FreeModule).

Different ways to create the 4-tuple (1,2,4,8) with integer coefficients:
	ℤ^4 !! #(1 2 4 8).
	ℤ^4 filling: [:i| 2^(i-1)].
	(1,2,4,8)
!

!GaussBareiss commentStamp: '<historical>' prior: 0!
This is the Gauss-Bereiss algorithm, a variant of Gassian elimination. It performs division-free Gaussian elimination on a matrix with coefficients in an integral domain, and computes the echelon form.

Input: a matrix with coefficients in a an integral domain;
Output: the reduced echelon form and the determinant.

The algorithm performs O(n^3) ring operations for nxn matrices. While the standard Gaussian elimination suffers from exponential intermediate coefficients growth, the Gauss-Bareiss algorithm has bit complexity O(n^5).
!

!BerlekampMassey commentStamp: '<historical>' prior: 0!
This is the Berlekamp-Massey algorithm. The initial purpose of this algorithm was to evaluate Binary BCH codes, but it has found many uses, including inverting matrices of constant diagonal, solving linear recurrences (synthesizing LFSRs with a specified output sequence), etc.

Input: a_1..a_{2n} the first 2n coefficients of a linear recursive sequence over a field K of order at most n (the minimal polynomial has degree bound n);
Output: the minimal polynomial of the sequence.

The algorithm performs O(n^2/2) multiplications; more precisely, at most 2 floor(n^2/4) - n + 1 multiplications.!

!BitTuple2 commentStamp: '<historical>' prior: 0!
Tuples of integer residue classes modulo 2, i.e. binary tuples, bit strings, or tuples of bits. The coefficient field is ℤ/2ℤ.

Example:
	"A handy way to create a binary tuple:"
	(1,0,1,1,0,1,1,1) % 2
!

!RealBox commentStamp: '<historical>' prior: 0!
Boxes in R^n built by the cartesian product of one-dimensional intervals (instances of RealInterval).

Example:
	RealBox new
		add: (RealInterval closed: -1 closed: 1);
		add: (RealInterval open: 3 open: 4);
		add: (RealInterval closed: 0 open: Infinity positive);
		yourself.
!

!SmithReduction commentStamp: '<historical>' prior: 0!
This is the Smith reduction algorithm. Given a matrix with coefficients in a PID, it reduces the matrix to its Smith normal form.

The Smith normal form is very useful for working with finitely generated modules over a PID, and in particular for deducing the structure of a quotient of a free module.

See [Coh96], Algorithm 2.4.14.!

!SparseTuple commentStamp: 'len 7/21/2020 17:34:32' prior: 0!
Sparse tuples are tuples that use a Dictionary to store the components, and zero entries are not stored explicitly. This makes them more efficient when most of the entries are zero.!

!PolynomialCode commentStamp: '<historical>' prior: 0!
In coding theory, a polynomial code is a type of linear code whose set of valid codewords consists of those polynomials (usually of some fixed length) that are divisible by some fixed polynomial (of shorter length, called the generator polynomial).

A polynomial code is cyclic if and only if the generator polynomial divides x^n - 1. See CyclicCode.
!

!IdentityArray methodsFor: 'as yet unclassified' stamp: 'len 12/23/2021 15:35:29'!
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceable ifFalse: [^ false].
	(size _ self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) == (otherCollection at: index) ifFalse: [^ false]].
	^ true! !

!IdentityArray methodsFor: 'as yet unclassified' stamp: 'len 12/23/2021 15:37:27'!
hashQuick
	| hash size step |
	size _ self size.
	hash _ (self species hash + size hash) hashMultiply.
	step _ size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem _ self at: i.
		elem == self ifFalse: [
			hash _ (hash + elem identityHash) hashMultiply]].
	^hash! !

!BitArray2 methodsFor: 'printing' stamp: 'len 12/21/2015 20:42'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:30'!
at: anInteger
	^ (bytes at: anInteger - 1 // 8 + 1 ifAbsent: [^ 0]) bitAt: anInteger - 1 \\ 8 + 1! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:07'!
at: anInteger put: aBit
	| i i0 b |
	i _ anInteger - 1 // 8 + 1.
	i0 _ anInteger - 1 \\ 8 + 1.
	b _ bytes at: i.
	(b bitAt: i0) == aBit
		ifFalse: [b _ b bitXor: (1 bitShift: i0-1)].
	bytes at: i put: b.
	^ aBit! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:16'!
byteAt: anInteger
	^ bytes at: anInteger ifAbsent: [0]! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 22:16'!
byteAt: anInteger put: aByte
	^ bytes at: anInteger put: aByte! !

!BitArray2 methodsFor: 'accessing' stamp: 'len 12/21/2015 20:41'!
size
	^ length! !

!BitArray2 methodsFor: 'arithmetic' stamp: 'len 12/21/2015 22:15'!
* aBitArray
	| answer |
	answer _ self class new: (self size max: aBitArray size).
	1 to: answer byteSize do: [:i|
		answer byteAt: i put: ((self byteAt: i) bitAnd: (aBitArray byteAt: i))].
	^ answer! !

!BitArray2 methodsFor: 'arithmetic' stamp: 'len 12/21/2015 22:14'!
+ aBitArray
	| answer |
	answer _ self class new: (self size max: aBitArray size).
	1 to: answer byteSize do: [:i|
		answer byteAt: i put: ((self byteAt: i) bitXor: (aBitArray byteAt: i))].
	^ answer! !

!BitArray2 methodsFor: 'converting' stamp: 'len 12/21/2015 22:17'!
asByteArray
	^ bytes! !

!BitArray2 methodsFor: 'converting' stamp: 'len 12/21/2015 22:21'!
asInteger
	"Answer the receiver as a possitive integer."
	| answer i |
	answer _ 0.
	i _ 0.
	bytes do: [:each| answer _ answer + (each bitShift: i). i _ i + 8].
	^ answer! !

!BitArray2 methodsFor: 'initialization' stamp: 'len 12/21/2015 20:31'!
bytes: aByteArray length: anInteger
	bytes _ aByteArray.
	length _ anInteger! !

!BitArray2 class methodsFor: 'instance creation' stamp: 'len 12/21/2015 20:41'!
new: anInteger
	^ self new: anInteger bytes: (ByteArray new: (anInteger / 8) ceiling)! !

!BitArray2 class methodsFor: 'instance creation' stamp: 'len 12/21/2015 20:40'!
new: anInteger bytes: aByteArray
	^ self basicNew bytes: aByteArray length: anInteger! !

!BitArray2 class methodsFor: 'instance creation' stamp: 'len 12/21/2015 22:26'!
random: anInteger
	| answer |
	answer _ self new: anInteger.
	1 to: answer size do: [:i| answer at: i put: 2 atRandom - 1].
	^ answer! !

!StandardBasis methodsFor: 'printing' stamp: 'len 5/7/2018 23:22:28'!
printOn: aStream
	aStream nextPut: ${.
	elements do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $}! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:22:50'!
add: aPolynomial
	^ elements add: aPolynomial! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:21:31'!
addAll: aCollection
	^ elements addAll: aCollection! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:22:47'!
at: anInteger
	^ elements at: anInteger! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 11/21/2015 00:10'!
first
	^ self at: 1! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 1/23/2017 11:47:32'!
indeterminates
	^ self anyOne parent indeterminates! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 1/23/2017 11:47:29'!
ordering
	"Answer the monomial ordering of the receiver."
	^ self anyOne parent ordering! !

!StandardBasis methodsFor: 'accessing' stamp: 'len 5/7/2018 23:21:22'!
size
	^ elements size! !

!StandardBasis methodsFor: 'accessing-private' stamp: 'len 5/7/2018 23:23:36'!
elements: aCollection
	elements _ aCollection! !

!StandardBasis methodsFor: 'copying' stamp: 'len 5/10/2018 08:03:11'!
copy
	"Answer a copy of the reciever."
	^ self class new elements: elements copy! !

!StandardBasis methodsFor: 'copying' stamp: 'len 11/20/2016 15:32:11'!
copyEmpty
	^ self class new! !

!StandardBasis methodsFor: 'copying' stamp: 'len 5/7/2018 23:23:29'!
copyWithout: aPolynomial
	^ self class new elements: (elements copyWithout: aPolynomial)! !

!StandardBasis methodsFor: 'enumerating' stamp: 'len 7/8/2016 20:57'!
apply: aMorphism
	| answer |
	answer _ self copyEmpty.
	self do: [:each| answer add: (aMorphism value: each)].
	^ answer! !

!StandardBasis methodsFor: 'enumerating' stamp: 'len 5/7/2018 23:21:19'!
do: aBlock
	elements do: aBlock! !

!StandardBasis methodsFor: 'initialization' stamp: 'len 5/10/2018 08:03:16'!
initialize
"	elements _ SortedCollection sortBlock: [:a :b| a degree <= b degree]."
"	elements _ SortedCollection sortBlock: [:a :b| a leadingMonomial <= b leadingMonomial]."
	elements _ OrderedCollection new! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/28/2020 17:47:47'!
elementAt: anArray
	^ (1 to: anArray size) sum: [:i| (self at: i) * (anArray at: i)]! !

!StandardBasis methodsFor: 'operations' stamp: 'len 1/26/2017 22:51:21'!
leadingTerms
	"Answer the standard basis made of all leading monomials of polynomials in the receiver."
	| answer |
	answer _ self class new.
	self do: [:each| | lt | lt _ each leadingTerm. (answer reduces: lt) ifFalse: [answer add: lt]].
	^ answer! !

!StandardBasis methodsFor: 'operations' stamp: 'len 11/23/2022 21:37:44'!
macaulayMatrix
	| monomials |
	monomials _ Set new.
	self do: [:each| monomials addAll: each monomials].
	monomials _ monomials asSortedCollection: [:a :b| a >= b].
	^ self anyOne parent scalars matrix: self size @ monomials size fill: [:i :j| (self at: i) at: (monomials at: j)]! !

!StandardBasis methodsFor: 'operations' stamp: 'len 12/17/2021 14:44:42'!
minimal
	"Answer the minimal Groebner basis of the receiver."
	| heads goods answer |
	self isMinimal ifTrue: [^ self].
	heads _ elements collect: [:each| each leadingMonomial].
	goods _ (1 to: heads size) asSet.
	heads withIndexDo: [:head :i|
		(goods anySatisfy: [:one| i ~= one and: [(heads at: one) | head]])
			ifTrue: [goods remove: i]].
	answer _ self class new.
	(goods asSortedCollection: [:a :b| "make it triangular for lex" (elements at: a) leadingMonomial >= (elements at: b) leadingMonomial])
		do: [:i| answer add: (elements at: i) normalized].
	^ answer! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/10/2019 08:31:13'!
reduce1: aPolynomial 
	"Answer Mora's normal form of aPolynomial with respect to the receiver)."
	| h L M |
	h _ aPolynomial.
	L _ OrderedCollection withAll: self.
	([h isZero or: [(M _ L select: [:g| g leadingMonomial | h leadingMonomial]) isEmpty]])
		whileFalse:
			[| g |
			g _ M detectMin: [:each| each ecart].
			g ecart > h ecart ifTrue: [L add: h].
			h _ h - (g * (h leadingMonomial / g leadingMonomial) * (h leadingCoefficient / g leadingCoefficient))].
	^ h! !

!StandardBasis methodsFor: 'operations' stamp: 'len 7/29/2021 17:12:23'!
reduce: aPolynomial 
	"Answer Mora's normal form of aPolynomial with respect to the receiver."
	| h L lh g ecartg |
	h _ aPolynomial.
	L _ OrderedCollection withAll: self.
	[h isZero ifTrue: [^ h].
	lh _ h leadingMonomial.
	g _ nil.
	ecartg _ -1.
	L do: [:each|
		| ecart |
		(each leadingMonomial | lh and: [ecart _ each ecart. g isNil or: [ecart < ecartg]])
			ifTrue: [g _ each. ecartg _ ecart]].
	g isNil ifTrue: [^ h].
	ecartg > h ecart ifTrue: [L add: h].
	h _ h + (g * (lh / g leadingMonomial) * (h leadingCoefficient negated / g leadingCoefficient))] repeat! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/11/2018 06:26:35'!
reduceFull: aPolynomial
	"In the general case of standard bases, this is the same as >>reduce:.
	It is reimplemented in GroebnerBasis."
	^ self reduce: aPolynomial! !

!StandardBasis methodsFor: 'operations' stamp: 'len 5/12/2018 17:54:24'!
reduceFull: aPolynomial startingAt: startIndex
	"In the general case of standard bases, this is the same as >>reduce:.
	It is reimplemented in GroebnerBasis."
	^ self reduce: aPolynomial! !

!StandardBasis methodsFor: 'operations' stamp: 'len 12/30/2021 14:30:32'!
reduced
	"Answer a reduced basis for the ideal generated by the receiver.
	Based on [BW93], algorithm REDGROEBNER (p. 216) and REDUCTION (p. 203).
	[BW93] Thomas Becker; Volker Weispfenning; 'Groebner Bases: A Computational Approach to Commutative Algebra' (1993)"
	| G H |
	H _ OrderedCollection new.
	1 to: self size do: [:i| | fi lm |
		fi _ self at: i.
		lm _ fi leadingMonomial.
		(((i+1 to: self size) noneSatisfy: [:j| (self at: j) leadingMonomial | lm])
			and: [H noneSatisfy: [:h| h leadingMonomial | lm]])
				ifTrue: [H add: fi]].
	H _ H asArray.
	G _ self class new.
	1 to: H size do: [:i| | hi r |
		hi _ H at: i.
		(r _ hi \\ (H copyWithoutIndex: i)) isZero ifFalse: [G add: r normalized]].
	G sort: [:a :b| a leadingMonomial >= b leadingMonomial]. "make it triangular for lex"
	^ G! !

!StandardBasis methodsFor: 'sorting' stamp: 'len 5/10/2018 17:52:05'!
sort: aBlock
	elements sort: aBlock! !

!StandardBasis methodsFor: 'testing' stamp: 'len 7/28/2021 15:19:12'!
isMinimal
	"A Groebner basis G of an ideal I is called 'minimal' if the lead terms of the elements in G minimally generate in(I); or equivalently, if for all p in G:
	- LC(p) = 1, and
	- LT(p) is not in <G-{p}> (i.e., leading-term-minimal).
	Note that any leading-term-minimal Groebner basis is also cardinality-minimal."
	self do: [:g| g leadingCoefficient isOne ifFalse: [^ false]].
	self do: [:g|
		self do: [:f|
			(f ~~ g and: [f leadingMonomial | g leadingMonomial]) ifTrue: [^ false]]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 5/10/2018 03:10:42'!
isMinimal2
	"A Groebner basis G is minimal when for all p in G:
	1. LC(p) = 1;
	2. LT(p) is not in <G-{p}>."
	| one |
	one _ self first scalars one.
	"Check all polynomials are monic:"
	self do: [:each| each leadingCoefficient ~= one ifTrue: [^ false]].
	self do: [:each| "Check if the basis without it still generates its leading term:"
		((self copyWithout: each) reduces: each leadingTerm)
				ifTrue: [^ false]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 7/28/2021 15:18:41'!
isReduced
	"A Groebner basis G is called 'reduced' (or 'auto-reduced') if for all p in G:
	- LC(p) = 1, and
	- no monomial of p is in <G-{p}>.
	In particular, a reduced Groebner basis is leading-term-minimal (no leading term of any generator divides any other generator's leading term), and a leading-term-minimal basis is also cardinality-minimal."
	self do: [:g| g leadingCoefficient isOne ifFalse: [^ false]].
	self do: [:g|
		g monomialsDo: [:gi|
			self do: [:f|
				(f ~~ g and: [f leadingMonomial | gi]) ifTrue: [^ false]]]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 5/9/2018 23:21:42'!
isReduced2
	"A Groebner basis G is reduced when for all p in G:
	1. LC(p) = 1;
	2. No monomial of p is contained in <G-{p}>."
	| one subBasis |
	one _ self first scalars one.
	"Check all polynomials are monic:"
	self do: [:each| each leadingCoefficient ~= one ifTrue: [^ false]].
	self do: [:each| "Check if the basis without it still generates its monomials:"
		subBasis _ self copyWithout: each.
		(each terms allSatisfy: [:other| subBasis reduces: other]) ifTrue: [^ false]].
	^ true! !

!StandardBasis methodsFor: 'testing' stamp: 'len 5/7/2018 23:21:47'!
isTrivial
	"Answer true if the basis is {1}."
	^ elements size = 1 and: [elements first isConstant and: [elements first isMonic]]
"	^ elements anySatisfy: [:f| f isConstant]"! !

!StandardBasis methodsFor: 'testing' stamp: 'len 10/29/2016 16:36'!
reduces: aPolynomial
	^ (self reduce: aPolynomial) isZero! !

!StandardBasis methodsFor: 'private' stamp: 'len 3/6/2016 20:37'!
species
	^ Array! !

!StandardBasis methodsFor: 'private' stamp: 'len 7/8/2022 11:12:23'!
validate
	1 to: self size do: [:i| | fi |
		fi _ self at: i.
		i+1 to: self size do: [:j| | fj |
			fj _ self at: j.
			self assert: (self reduces: (fi s: fj))]]! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 7/29/2021 16:58:08'!
reduce: aPolynomial
	"Answer a top-reduced normal form of aPolynomial with respect to the receiver (without doing tail reduction)."
	| h |
	h _ aPolynomial.
	[h isZero]
		whileFalse:
			[| lh lg g |
			lh _ h leadingMonomial.
			g _ elements detect: [:one| (lg _ one leadingMonomial) | lh] ifNone: [^ h].
			h _ h + (g * (lh / lg) * (h leadingCoefficient negated / g leadingCoefficient))].
	^ h! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 7/29/2021 16:58:21'!
reduce: aPolynomial startingAt: startIndex
	"Answer a top-reduced normal form of aPolynomial with respect to the receiver (without doing tail reduction)."
	| h |
	h _ aPolynomial.
	[h isZero]
		whileFalse:
			[| lh lg g |
			lh _ h leadingMonomial.
			(startIndex to: elements size) detect: [:i| (lg _ (g _ elements at: i) leadingMonomial) | lh] ifNone: [^ h].
			h _ h + (g * (lh / lg) * (h leadingCoefficient negated / g leadingCoefficient))].
	^ h! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 12/17/2021 14:43:33'!
reduceFull: aPolynomial
	"Answer the tail-reduced normal form of aPolynomial with respect to the receiver (doing full tail reduction)."
	| g h |
	g _ aPolynomial.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ self reduce: g.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h normalized! !

!GroebnerBasis methodsFor: 'operations' stamp: 'len 12/17/2021 14:43:39'!
reduceFull: aPolynomial startingAt: startIndex
	"Answer the tail-reduced normal form of aPolynomial with respect to the receiver (doing full tail reduction)."
	| g h |
	g _ aPolynomial.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ self reduce: g startingAt: startIndex.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h normalized! !

!SmallPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/13/2022 13:17:56'!
testSmallPrimeField
	| p F E |
	p _ 20 atRandom nextPrime.
	F _ ℤ/p.
	E _ SmallPrimeField new: p.
	F do: [:a|
		F do: [:b| | c d |
			c _ E !! a representative.
			d _ E !! b representative.
			self assert: (a+b) representative = (c+d) representative.
			self assert: (a-b) representative = (c-d) representative.
			self assert: (a*b) representative = (c*d) representative.
			b isZero ifFalse:
				[self assert: (a/b) representative = (c/d) representative.
				self assert: b inverse representative = d inverse representative]]]! !

!SmallPrimeFieldTest methodsFor: 'as yet unclassified' stamp: 'len 4/13/2022 13:18:02'!
testSmallPrimeFieldTables
	| p F g |
	p _ 20 atRandom nextPrime.
	F _ SmallPrimeField new: p.
	g _ F primitiveRoot.
	1 to: p-1 do: [:n| self assert: (F expTable at: n) = (g^n) representative].
	0 to: p-2 do: [:n| self assert: (F logTable at: (g^n) representative) = n]! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/15/2022 11:43:09'!
isAdditiveGroup
	^ true! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:58:39'!
printOn: aStream
	aStream nextPutAll: 'Div'.
	scalars = ℤ ifFalse: [aStream nextPutAll: scalars printString sub].
	aStream nextPut: $(; print: space; nextPut: $)! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:57:29'!
scalars
	^ scalars! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:57:21'!
space
	^ space! !

!DivisorGroup methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 12:00:07'!
space: aScheme scalars: aRing
	scalars _ aRing.
	space _ aScheme! !

!DivisorGroup class methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:59:50'!
on: aScheme
	^ self on: aScheme over: ℤ! !

!DivisorGroup class methodsFor: 'as yet unclassified' stamp: 'len 7/25/2022 11:59:31'!
on: aScheme over: aRing
	^ self new space: aScheme scalars: aRing! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:15'!
action
	^ action! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/16/2016 07:30'!
arity
	^ 2! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:16'!
left
	^ left! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:16'!
right
	^ right! !

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 2/12/2016 06:55'!
size
	^ left size * right size! !

!SemidirectProductGroup methodsFor: 'converting' stamp: 'len 4/19/2016 20:55'!
asCartesianProduct
	^ (left, right)! !

!SemidirectProductGroup methodsFor: 'elements' stamp: 'len 6/19/2022 12:29:46'!
identity
	"Answer the identity element of the receiver."
	^ self project: {left identity. right identity}! !

!SemidirectProductGroup methodsFor: 'enumerating' stamp: 'len 2/12/2016 06:57'!
do: aBlock
	left do: [:g| right do: [:h| aBlock value: (self project: {g.h})]]! !

!SemidirectProductGroup methodsFor: 'morphisms' stamp: 'len 10/6/2016 15:35'!
project: anArray
	"Project a cartesian product of two elements to their semidirect product."
	^ SemidirectProductElement new
		parent: self;
		left: (anArray at: 1);
		right: (anArray at: 2)! !

!SemidirectProductGroup methodsFor: 'morphisms' stamp: 'len 9/16/2022 07:54:21'!
projection
	"Answer the projection from the cartesian product of two groups to their semidirect product."
	^ ((left, right) to: self map: [:each| self project: each]) name: 'π'! !

!SemidirectProductGroup methodsFor: 'printing' stamp: 'len 4/21/2016 00:16'!
printOn: aStream
	aStream print: left; nextPutAll: ' X| '; print: right! !

!SemidirectProductGroup methodsFor: 'random' stamp: 'len 4/19/2016 20:52'!
atRandom: aRandom
	^ self project: (self asCartesianProduct atRandom: aRandom)! !

!SemidirectProductGroup methodsFor: 'random' stamp: 'len 4/19/2016 20:52'!
atRandom: aRandom bits: bitSize
	^ self project: (self asCartesianProduct atRandom: aRandom bits: bitSize)! !

!SemidirectProductGroup methodsFor: 'private' stamp: 'len 2/12/2016 06:17'!
action: anAction
	action _ anAction! !

!SemidirectProductGroup methodsFor: 'private' stamp: 'len 2/12/2016 06:17'!
left: aGroup
	left _ aGroup! !

!SemidirectProductGroup methodsFor: 'private' stamp: 'len 2/12/2016 06:17'!
right: aGroup
	right _ aGroup! !

!SemidirectProductGroup class methodsFor: 'instance creation' stamp: 'len 2/12/2016 06:16'!
left: aGroup right: anotherGroup action: anAction
	^ self new left: aGroup; right: anotherGroup; action: anAction! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:25'!
diagram
	^ diagram! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:24'!
diagram: aGraph
	diagram _ aGraph! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:39'!
gramMatrix
	^ self matrix collect: [:x| -2 * (Float pi / x) cos]! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:39'!
isHyperbolic
	^ (self gramMatrix spectrum count: [:each| each negative]) = 1! !

!CoxeterGroup methodsFor: 'as yet unclassified' stamp: 'len 5/10/2019 09:57:34'!
presentation
	self notYetImplemented
"	| generators relators |
	generators _ diagram values collect: [:each| Word x: each].
	relators _ OrderedCollection new.
	diagram edgesAndLabelsDo: [:each :label|
		label isInfinity
			ifFalse: [relators add: (Word x: each key) * (Word x: each value) ^ (label ifNil: [3])]].
	^ GroupPresentation generators: generators relators: relators"! !

!CoxeterGroup class methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 01:25'!
diagram: aGraph
	^ self new diagram: aGraph! !

!CoxeterGroup class methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:26:58'!
matrix: aMatrix
	| n graph |
	n _ aMatrix width.
	graph _ Graph unorderedLabeled.
	graph addAll: (1 to: n).
	1 to: n do: [:i|
		1 to: n do: [:j|
			| m |
			m _ aMatrix at: i@j.
			m = 3 ifTrue: [graph addEdgeFrom: i to: j label: nil].
			m > 3 ifTrue: [graph addEdgeFrom: i to: j label: m]]].
	^ self diagram: graph! !

!SmallPrimeField methodsFor: 'accessing' stamp: 'len 6/9/2018 20:23:46'!
expTable
	^ expTable! !

!SmallPrimeField methodsFor: 'accessing' stamp: 'len 6/9/2018 20:23:52'!
logTable
	^ logTable! !

!SmallPrimeField methodsFor: 'accessing-private' stamp: 'len 11/30/2016 09:43:55'!
modulus: anInteger
	super modulus: anInteger.
	self computeTables! !

!SmallPrimeField methodsFor: 'private' stamp: 'len 9/2/2018 21:27:06'!
computeTables
	| g x |
	expTable _ WordArray new: modulus - 1. "n -> g^n"
	logTable _ WordArray new: modulus - 1. "g^n -> n"
	g _ self primitiveRoot representative.
	x _ 1.
	1 to: modulus - 1 do: [:i|
		"g^n -> n:"
		logTable at: x put: i - 1.
		x _ x * g \\ modulus.
		"n -> g^n:"
		expTable at: i put: x]! !

!SmallPrimeField methodsFor: 'private' stamp: 'len 6/8/2019 12:35:48'!
elementClass
	^ SmallPrimeResidue! !

!SmallPrimeField methodsFor: 'private' stamp: 'len 11/30/2016 19:12:33'!
inverseOfRepresentative: anInteger
	anInteger == 0 ifTrue: [^ nil].
	anInteger == 1 ifTrue: [^ 1].
	^ expTable at: modulus - 1 - (logTable at: anInteger)! !

!SmallPrimeField class methodsFor: 'instance creation' stamp: 'len 5/1/2022 12:35:48'!
new: aPrime
	^ self new modulus: aPrime! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:40:15'!
arity
	^ self rank! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 05:09:10'!
components
	^ (1 to: self rank) collect: [:each| self scalars]! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:40:52'!
dimension
	"Answer the Krull dimension of the receiver. This is the length of the longest ascending chain of prime ideals."
	^ self scalars dimension! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:48:22'!
one
	| one |
	one _ self scalars one.
	^ self compose: self scalars !!!! ((1 to: self rank) collect: [:each| one])! !

!PowerAlgebra methodsFor: 'accessing' stamp: 'len 10/13/2022 04:42:30'!
size
	^ self scalars size ^ self rank! !

!PowerAlgebra methodsFor: 'comparing' stamp: 'len 10/13/2022 04:45:09'!
= anObject
	^ self class = anObject class and: [self asModule = anObject asModule]! !

!PowerAlgebra methodsFor: 'comparing' stamp: 'len 10/13/2022 04:44:48'!
hash
	^ self asModule hash! !

!PowerAlgebra methodsFor: 'computing-matrices' stamp: 'len 11/23/2022 21:38:37'!
matrix: B quotientBy: A
	"Answer X such that AX + N = B, satisfying that if the i-th column of B is a linear combination of the columns of A then the i-th column of N is zero.
	The algorithm solves the system for each projection and then composes back to the product ring."
	| results |
	results _ self ⇉ self components collect: [:p| (B apply: p) quotientBy: (A apply: p)].
	^ self matrix: A width @ B width fill: [:i :j| self compose: (results collect: [:each| each at: i@j])]! !

!PowerAlgebra methodsFor: 'computing-matrices' stamp: 'len 11/23/2022 21:38:47'!
syzygiesOf: aMatrix
	"Answer a matrix whose rows span the kernel of the given matrix.
	For a matrix A, the output is X with AX = 0.
	The algorithm computes syzygies for each projection and then composes back to the product ring."
	| results |
	results _ self ⇉ self components collect: [:p| (aMatrix apply: p) syzygies].
	^ self matrix: aMatrix width @ (results max: [:each| each width]) fill: [:i :j| self compose: (results withIndexCollect: [:each :k| each at: i@j ifAbsent: [(self components at: k) zero]])]! !

!PowerAlgebra methodsFor: 'converting' stamp: 'len 10/14/2022 08:51:41'!
asAbelianGroup
	"Answer the receiver as a ℤ-module."
	^ self scalars asAbelianGroup ^⊕ self rank! !

!PowerAlgebra methodsFor: 'elements' stamp: 'len 10/13/2022 04:40:39'!
compose: anArray
	^ ProductRingElement components: self scalars !!!! anArray parent: self! !

!PowerAlgebra methodsFor: 'elements' stamp: 'len 11/24/2022 17:50:53'!
fromModuleElement: aTuple
	^ ProductRingElement components: aTuple parent: self! !

!PowerAlgebra methodsFor: 'morphisms' stamp: 'len 10/13/2022 04:50:49'!
projection: i
	"Answer the canonical projection to the i-th component."
	^ (self to: self scalars map: [:x| x at: i]) name: 'π', i printString sub! !

!PowerAlgebra methodsFor: 'operations' stamp: 'len 10/13/2022 07:39:54'!
nilradical
	"Answer the ideal consisting of all nilpotent elements, i.e. all x such that there exists some n with x^n = 0. It is the intersection of all prime ideals."
	| generators |
	generators _ OrderedCollection new.
	self scalars nilradical generators do: [:g|
		1 to: self rank do: [:i|
			generators add: self !! ((self asModule x: i) * g)]].
	^ self * generators! !

!PowerAlgebra methodsFor: 'printing' stamp: 'len 10/13/2022 04:40:08'!
printOn: aStream
	aStream print: self scalars; nextPutAll: ('×', self rank printString) super! !

!PowerAlgebra methodsFor: 'random' stamp: 'len 10/13/2022 04:51:54'!
unitAtRandom: aRandom
	^ self compose: ((1 to: self rank) collect: [:each| self scalars unitAtRandom: aRandom])! !

!PowerAlgebra methodsFor: 'random' stamp: 'len 10/13/2022 04:52:25'!
unitAtRandom: aRandom bits: bitSize
	| n |
	n _ bitSize / self rank.
	^ self compose: ((1 to: self rank) collect: [:each| self scalars unitAtRandom: aRandom bits: n])! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:02'!
hasCanonicalAssociates
	^ self scalars hasCanonicalAssociates! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:12'!
hasGCD
	^ self scalars hasGCD! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:24'!
isBezout
	"Answer true if the receiver is a Bezout ring, i.e. every finitely generated ideal is principal."
	"Bezout rings (like PIRs) are closed under products:"
	^ self scalars isBezout or: [super isBezout]! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:33'!
isCommutative
	^ self scalars isCommutative! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:46'!
isIntegralDomain
	"The product of two or more nontrivial rings always has nontrivial zero divisors."
	^ self scalars isTrivial or: [self rank < 2]! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:41:55'!
isInvolutive
	^ self scalars isInvolutive! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:42:03'!
isPIR
	"Answer true if the receiver is a principal ideal ring, i.e. all ideals are principal (but it is not necessarily a PID, i.e. it is not necessarily an integral domain)."
	"A product ring is a PIR if and only if all its components are PIRs."
	^ self scalars isPIR! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:48:50'!
isProduct
	^ true! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:49:17'!
isReduced
	"Answer true if the receiver has no nonzero nilpotent elements, or equivalently if it has no nonzero elements with square zero, i.e. x^2 = 0 implies x = 0."
	self scalars isReduced ifTrue: [^ true]. "is it iff?"
	^ super isReduced! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:42:11'!
isTrivial
	^ self scalars isTrivial! !

!PowerAlgebra methodsFor: 'testing' stamp: 'len 10/13/2022 04:42:19'!
isUFR
	"A unique factorization ring (UFR) is a commutative ring in which every nonzero nonunit element can be written as a product of irreducible elements, uniquely up to order and associates."
	"The product of a finite number of UFRs is a UFR. In particular, the product of a finite number of UFDs is a UFR [Fle69]."
	^ self scalars isUFR! !

!PowerAlgebra class methodsFor: 'instance creation' stamp: 'len 10/13/2022 04:58:29'!
on: aFreeModule
	^ self new module: aFreeModule! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 11:56:17'!
base
	^ self completionIdeal scalars! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 11:11:18'!
completionIdeal
	^ self propertyAt: #completionIdeal! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 18:52:55'!
dimension
	"Answer the Krull dimension of the receiver."
	(self base isNoetherian and: [self base isLocal])
		ifTrue: [^ self base dimension].
	^ super dimension! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 10:56:36'!
maximalIdeal
	"Answer the unique maximal ideal in the receiver."
	^ self * (self completionIdeal generators collect: [:each| self embed: each])! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 16:52:57'!
one
	^ self embed: self base one! !

!CompletionRing methodsFor: 'accessing' stamp: 'len 6/6/2019 16:53:02'!
zero
	^ self embed: self base zero! !

!CompletionRing methodsFor: 'as yet unclassified' stamp: 'len 6/22/2019 03:13:21'!
> aRing
	self base > aRing ifTrue: [^ true].
	^ super > aRing! !

!CompletionRing methodsFor: 'as yet unclassified' stamp: 'len 6/22/2019 03:14:37'!
>= aRing
	super >= aRing ifTrue: [^ true].
	self base >= aRing ifTrue: [^ true].
	^ false! !

!CompletionRing methodsFor: 'as yet unclassified' stamp: 'len 6/6/2019 18:58:05'!
hilbertSeriesAt: t
	"Answer the Hilbert series of the receiver."
	(self base isNoetherian and: [self base isLocal]) ifTrue: [^ self base hilbertSeriesAt: t].
	^ super hilbertSeriesAt: t! !

!CompletionRing methodsFor: 'comparing' stamp: 'len 6/6/2019 17:06:14'!
= anObject
	self == anObject ifTrue: [^ true].
	^ (anObject isKindOf: CompletionRing) and: [anObject completionIdeal = self completionIdeal]! !

!CompletionRing methodsFor: 'comparing' stamp: 'len 6/6/2019 17:06:28'!
hash
	^ self completionIdeal hash + self base hash! !

!CompletionRing methodsFor: 'morphisms' stamp: 'len 10/6/2022 11:38:03'!
from: aDomain
	aDomain = self base ifTrue: [^ self addMap: ((self base into: self map: [:x| self embed: x]) name: 'ι')].
	^ super from: aDomain! !

!CompletionRing methodsFor: 'operations' stamp: 'len 6/6/2019 11:59:39'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the quotient ring isomorphic to the anInteger-th truncation of the receiver."
	^ self base / (self completionIdeal ^ anInteger)! !

!CompletionRing methodsFor: 'testing' stamp: 'len 6/22/2019 03:11:07'!
isCompletion
	^ true! !

!CompletionRing methodsFor: 'testing' stamp: 'len 6/6/2019 10:55:42'!
isLocal
	^ true! !

!CompletionRing methodsFor: 'private' stamp: 'len 6/6/2019 11:11:28'!
completionIdeal: anIdeal
	self propertyAt: #completionIdeal put: anIdeal! !

!CompletionRing class methodsFor: 'instance creation' stamp: 'len 6/6/2019 12:04:12'!
at: anIdeal
	^ self new completionIdeal: anIdeal! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:27:04'!
computeDiagram
	| n graph |
	n _ matrix width.
	graph _ Graph unorderedLabeled.
	graph addAll: (1 to: n).
	1 to: n do: [:i|
		1 to: n do: [:j|
			| m |
			m _ matrix at: i@j.
			m = 3 ifTrue: [graph addEdgeFrom: i to: j label: nil].
			m > 3 ifTrue: [graph addEdgeFrom: i to: j label: m]]].
	^ graph! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:54'!
diagram
	^ diagram ifNil: [diagram _ self computeDiagram]! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:57'!
diagram: aGraph
	diagram _ aGraph! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:55'!
matrix
	^ matrix ifNil: [matrix _ self computeMatrix]! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 07:57'!
matrix: aMatrix
	matrix _ aMatrix! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 5/10/2019 09:57:21'!
presentation
	self notYetImplemented
"	| generators relators |
	generators _ self diagram values collect: [:each| Word x: each].
	relators _ OrderedCollection new.
	self diagram edgesAndLabelsDo: [:each :label|
		label isInfinity
			ifFalse: [relators add: (Word x: each key) * (Word x: each value) ^ (label ifNil: [3])]].
	^ GroupPresentation generators: generators relators: relators"! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 03:17'!
reflections
	"Answer the conjugates of the generators."
	^ self notYetImplemented! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 08:02'!
schlafliMatrix
	^ self matrix collect: [:x| -2 * (Float pi / x) cos]! !

!CoxeterSystem methodsFor: 'as yet unclassified' stamp: 'len 4/18/2016 03:28'!
simpleReflections
	^ generators! !

!Divisor methodsFor: 'accessing' stamp: 'len 7/18/2022 06:23:40'!
degree
	coefficients isEmpty ifTrue: [^ self scalars zero].
	^ coefficients sum! !

!Divisor methodsFor: 'accessing' stamp: 'len 7/18/2022 06:22:38'!
scalars
	^ parent scalars! !

!Divisor methodsFor: 'accessing' stamp: 'len 8/12/2022 13:18:46'!
support
	| answer |
	answer _ nil. "should start from the empty variety"
	coefficients keysDo: [:each| answer _ answer ifNil: [each] ifNotNil: [answer ∪ each]].
	^ answer! !

!Divisor methodsFor: 'comparing' stamp: 'len 1/11/2017 16:34:02'!
<= aDivisor
	^ (aDivisor - self) isEffective! !

!Divisor methodsFor: 'comparing' stamp: 'len 1/2/2017 15:43:41'!
= anObject
	^ (anObject isKindOf: Divisor) and: [self ~ anObject]! !

!Divisor methodsFor: 'comparing' stamp: 'len 10/27/2016 10:26'!
>= aDivisor
	^ (self - aDivisor) isEffective! !

!Divisor methodsFor: 'comparing' stamp: 'len 1/2/2017 15:43:29'!
~ aDivisor
	"Answer true if the receiver and the argument are linearly equivalent."
	^ (self - aDivisor) isPrincipal! !

!Divisor methodsFor: 'comparing' stamp: 'len 7/25/2022 11:56:06'!
hash
	^ 0! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:22:17'!
* anElement
	(anElement isInteger or: [self scalars includes: anElement])
		ifTrue: [^ self class coefficients: coefficients * anElement parent: parent].
	^ self error: 'not a scalar'! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:21:07'!
+ aDivisor
	(parent includes: aDivisor) ifFalse: [^ self adapt: aDivisor andSend: #+].
	^ self class coefficients: coefficients + aDivisor coefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:21:26'!
- aDivisor
	(parent includes: aDivisor) ifFalse: [^ self adapt: aDivisor andSend: #-].
	^ self class coefficients: coefficients - aDivisor coefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 8/21/2022 08:15:14'!
gcd: aDivisor
	| newCoefficients |
	newCoefficients _ SortedDictionary new.
	coefficients keysAndValuesDo: [:key :value|
		aDivisor coefficients at: key ifPresent: [:value2| newCoefficients at: key put: (value min: value2)]].
	^ self class coefficients: newCoefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:19:06'!
lcm: aDivisor
	| newCoefficients |
	newCoefficients _ coefficients copy.
	aDivisor coefficients keysAndValuesDo: [:key :value|
		newCoefficients at: key put: ((coefficients at: key) max: value)].
	^ self class coefficients: newCoefficients parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 7/18/2022 06:21:39'!
negated
	^ self class coefficients: coefficients negated parent: parent! !

!Divisor methodsFor: 'testing' stamp: 'len 2/25/2017 09:09:25'!
isEffective
	^ self allSatisfy: [:each| each positive]! !

!Divisor methodsFor: 'private' stamp: 'len 7/18/2022 06:20:16'!
coefficients: aDictionary parent: aDivisorGroup
	coefficients _ aDictionary.
	parent _ aDivisorGroup! !

!Divisor class methodsFor: 'instance creation' stamp: 'len 7/18/2022 06:19:38'!
coefficients: aDictionary parent: aDivisorGroup
	^ self new coefficients: aDictionary parent: aDivisorGroup! !

!SemidirectProductElement methodsFor: 'accessing' stamp: 'len 5/5/98 00:18'!
left
	"Answer the left component of the receiver."
	^ left! !

!SemidirectProductElement methodsFor: 'accessing' stamp: 'len 10/6/2016 15:37'!
parent
	^ parent! !

!SemidirectProductElement methodsFor: 'accessing' stamp: 'len 5/5/98 00:18'!
right
	"Answer the right component of the receiver."
	^ right! !

!SemidirectProductElement methodsFor: 'constants' stamp: 'len 1/23/2017 11:46:24'!
identity
	^ self parent identity! !

!SemidirectProductElement methodsFor: 'operations' stamp: 'len 5/5/2019 18:19:05'!
· aSemidirectProduct
	^ self parent
		project:
			{left · aSemidirectProduct left.
			parent action value: {aSemidirectProduct left. right} · aSemidirectProduct right}! !

!SemidirectProductElement methodsFor: 'operations' stamp: 'len 5/5/2019 18:17:09'!
inverse
	| leftInverse rightInverse |
	leftInverse _ left inverse.
	rightInverse _ right inverse.
	^ parent project: {leftInverse. (parent action value: {leftInverse. rightInverse}) inverse}! !

!SemidirectProductElement methodsFor: 'printing' stamp: 'len 2/12/2016 06:10'!
printOn: aStream
	aStream nextPut: $(; print: left; nextPutAll: ', '; print: right; nextPut: $)! !

!SemidirectProductElement methodsFor: 'private' stamp: 'len 5/5/98 00:17'!
left: anObject
	left _ anObject! !

!SemidirectProductElement methodsFor: 'private' stamp: 'len 10/6/2016 15:37'!
parent: aSemidirectProductGroup
	parent _ aSemidirectProductGroup! !

!SemidirectProductElement methodsFor: 'private' stamp: 'len 5/5/98 00:17'!
right: anObject
	right _ anObject! !

!SmallPrimeResidue methodsFor: 'operations' stamp: 'len 9/2/2018 21:50:50'!
* aModularInteger
	| b i p |
	aModularInteger class == self class ifFalse: [^ super * aModularInteger].
	b _ aModularInteger representative.
	(representative = 0 or: [b = 1]) ifTrue: [^ self].
	(representative = 1 or: [b = 0]) ifTrue: [^ aModularInteger].
	i _ (parent logTable at: representative) + (parent logTable at: b).
	i >= (p _ parent modulus) ifTrue: [i _ i - p + 1].
	^ parent representative: (parent expTable at: i)! !

!SmallPrimeResidue methodsFor: 'operations' stamp: 'len 5/28/2022 12:20:51'!
colift: anElement
	"Answer x such that xa = b, where a is the receiver and b is the argument. Answer nil if there's no solution.
	This is the right division of b by a, written b / a."
	| i |
	anElement class == self class ifFalse: [^ super colift: anElement].
	(anElement isZero or: [representative = 1]) ifTrue: [^ anElement].
	representative = 0 ifTrue: [^ nil].
	i _ (parent logTable at: anElement representative) - (parent logTable at: representative).
	i <= 0 ifTrue: [i _ i + parent modulus - 1].
	^ parent representative: (parent expTable at: i)! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/3/2019 17:26:18'!
<< anInteger
	^ self >> anInteger negated! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/3/2019 17:28:33'!
>> anInteger
	"Multiply the receiver by p^anInteger shifting the digits by anInteger places."
	^ parent digits: digits >> anInteger! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/5/2019 13:18:16'!
abs
	"Answer the p-adic absolute value (or norm) of the receiver."
	^ self isZero ifTrue: [0] ifFalse: [self p ^ self valuation negated]! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 10:18:59'!
absolutePrecision
	^ parent precision! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 16:35:37'!
at: anInteger
	anInteger < 0 ifTrue: [^ 0].
	^ digits at: anInteger + 1! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/1/2019 17:30:48'!
digits
	^ digits! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/1/2019 17:24:34'!
p
	^ parent p! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 12/17/2016 20:59:36'!
parent
	^ parent! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 13:54:50'!
relativePrecision
	^ self absolutePrecision! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new: 1! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/2/2019 11:30:57'!
valuation
	self isZero ifTrue: [^ Infinity positive].
	^ (digits findFirst: [:x| x isZero not]) - 1
"	i _ 0.
	precision _ parent precision.
	[(digits at: (i _ i + 1)) isZero not ifTrue: [^ i].
	precision > i ifFalse: [^ i]] repeat"! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/2/2019 11:02:42'!
* anAdic
	| a b n carry  p |
	anAdic class = self class ifFalse: [^ self * (parent !! anAdic)].
	a _ digits.
	b _ anAdic digits.
	p _ self p.
	n _ 1.
	carry _ 0.
	^ parent streaming:
		[| t |
		t _ ((1 to: n) sum: [:i| (a at: i) * (b at: n-i+1)]) + carry.
		n _ n + 1.
		carry _ t // p.
		t \\ p]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/1/2019 17:30:18'!
+ anAdic
	| a b n carry  p |
	a _ digits.
	b _ anAdic digits.
	p _ self p.
	n _ 1.
	carry _ 0.
	^ parent streaming:
		[| t |
		t _ (a at: n) + (b at: n) + carry.
		n _ n + 1.
		t < p ifTrue: [carry _ 0. t] ifFalse: [carry _ 1. t - p]]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/1/2019 17:30:22'!
- anAdic
	| a b n carry  p |
	a _ digits.
	b _ anAdic digits.
	p _ self p.
	n _ 1.
	carry _ 0.
	^ parent streaming:
		[| t |
		t _ (a at: n) - (b at: n) - carry.
		n _ n + 1.
		t >= 0 ifTrue: [carry _ 0. t] ifFalse: [carry _ 1. t + p]]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/2/2019 11:39:02'!
negated
	| n p |
	p _ self p.
	n _ 0.
	^ parent streaming: [n = 0 ifTrue: [p - (digits at: (n _ n + 1))] ifFalse: [p - 1 - (digits at: (n _ n + 1))]]! !

!RelaxedAdicInteger methodsFor: 'arithmetic' stamp: 'len 6/6/2019 03:43:40'!
reciprocal
	| p a b1 carry |
	p _ self p.
	a _ digits.
	b1 _ (a at: 1) reciprocalModulo: p.
	carry _ 0.
	^ parent digits:
		(Sequence
			initialValues: {b1}
			recurrence:
				[:b :n| | q |
				q _ b1 negated * (((2 to: n) sum: [:i| (a at: i) * (b at: n-i+1)]) + carry).
				carry _ q // p.
				q \\ p])! !

!RelaxedAdicInteger methodsFor: 'comparing' stamp: 'len 8/7/2019 06:47:58'!
= anAdic
	((anAdic isKindOf: RelaxedAdicInteger) and: [parent = anAdic parent]) ifFalse: [^ false].
	parent isBounded ifFalse: [^ false].
	"a bounded p-adics that are equal up to their precision are considered equal
	(but maybe it should answer always false, like Magma)"
	1 to: parent precision do: [:i|
		(digits at: i) = (anAdic digits at: i) ifFalse: [^ false]].
	^ true! !

!RelaxedAdicInteger methodsFor: 'comparing' stamp: 'len 6/3/2019 15:24:04'!
hash
	| answer |
	answer _ 0.
	1 to: 4 do: [:i| "just 4 terms? how to decide how many terms to use?"
		answer _ answer hashMultiply + (digits at: i) hash].
	^ answer! !

!RelaxedAdicInteger methodsFor: 'converting' stamp: 'len 8/7/2019 10:08:38'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the truncation to anInteger terms."
	^ (self parent jet: anInteger) !! self! !

!RelaxedAdicInteger methodsFor: 'printing' stamp: 'len 6/2/2019 10:09:25'!
printOn: aStream
	self printOn: aStream withPowersShiftedBy: 0! !

!RelaxedAdicInteger methodsFor: 'printing' stamp: 'len 5/23/2020 07:42:09'!
printOn: aStream withPowersShiftedBy: k
	| printPrecision first |
	self isZero
		ifTrue: [aStream nextPut: $0]
		ifFalse:
			[printPrecision _ 10.
			first _ true.
			self valuation - k to: self valuation + printPrecision - 1 - k do: [:i|
				| x |
				(x _ self digits at: i+1+k) isZero
					ifFalse:
						[first ifFalse: [aStream nextPutAll: ' + '].
						(x ~= 1 or: [i = 0]) ifTrue: [aStream print: x. i ~= 0 ifTrue: [aStream nextPut: $·]].
						i ~= 0 ifTrue: [aStream print: self p. i ~= 1 ifTrue: [aStream nextPutAll: i printString super]].
						first _ false]]].
	aStream nextPutAll: ' + ...'
	"parent isBounded
		ifTrue: [aStream nextPutAll: ' + ', 'O' italic; nextPut: $(; print: self p; nextPutAll: parent precision printString super; nextPut: $)]
		ifFalse: [aStream nextPutAll: ' + ...']"! !

!RelaxedAdicInteger methodsFor: 'testing' stamp: 'len 6/2/2019 10:00:28'!
isZero
	parent isBounded ifFalse: [^ false].
	"a bounded p-adic that is 0 up to its precision is considered 0
	(but maybe it should answers always false, like Magma)"
	1 to: parent precision do: [:i|
		(digits at: i) isZero ifFalse: [^ false]].
	^ true! !

!RelaxedAdicInteger methodsFor: 'private' stamp: 'len 6/1/2019 16:50:33'!
digits: aSequence
	digits _ aSequence! !

!RelaxedAdicInteger methodsFor: 'private' stamp: 'len 6/1/2019 16:50:14'!
parent: aPAdicIntegersRing
	parent _ aPAdicIntegersRing! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/5/2019 13:19:40'!
abs
	"Answer the p-adic absolute value (or norm) of the receiver."
	^ self isZero ifTrue: [0] ifFalse: [self p ^ valuation negated]! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:20:17'!
absolutePrecision
	^ unit absolutePrecision! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:37:29'!
denominator
	^ valuation < 0 ifTrue: [unit parent !! (self p ^ valuation negated)] ifFalse: [unit one]! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:21:46'!
numerator
	^ valuation <= 0 ifTrue: [unit] ifFalse: [unit >> valuation]! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:21:54'!
p
	^ unit p! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/5/2019 11:26:00'!
parent
	^ unit parent fractions! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/2/2019 10:46:39'!
relativePrecision
	^ self absolutePrecision - valuation! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/3/2019 18:22:52'!
unit
	^ unit! !

!RelaxedAdicNumber methodsFor: 'accessing' stamp: 'len 6/2/2019 10:48:05'!
valuation
	^ valuation! !

!RelaxedAdicNumber methodsFor: 'printing' stamp: 'len 6/3/2019 18:28:16'!
printOn: aStream
	unit printOn: aStream withPowersShiftedBy: valuation negated! !

!RelaxedAdicNumber methodsFor: 'private' stamp: 'len 6/3/2019 18:26:19'!
valuation: anInteger unit: anAdicInteger
	valuation _ anInteger.
	unit _ anAdicInteger! !

!RelaxedAdicNumber class methodsFor: 'instance creation' stamp: 'len 6/3/2019 18:25:44'!
valuation: anInteger unit: anAdicInteger
	^ self new valuation: anInteger unit: anAdicInteger! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 12/27/2015 15:45'!
coefficients
	^ coefficients! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 12/27/2015 15:45'!
indeterminate
	^ 1! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 10/28/2016 10:37'!
parent
	^ parent! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 12/28/2015 04:12'!
scalars
	^ coefficients codomain! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 9/16/2022 08:08:19'!
terms
	| P |
	P _ self scalars polynomials.
	^ Sequence to: P map: [:i| P coefficient: (coefficients at: i) degree: i-1]! !

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 6/14/2019 01:33:51'!
valuation
	^ (self coefficients findFirst: [:each| each isZero not]) - 1! !

!RelaxedPowerSeries methodsFor: 'accessing-private' stamp: 'len 12/27/2015 15:46'!
coefficients: aSequence
	coefficients _ aSequence! !

!RelaxedPowerSeries methodsFor: 'accessing-private' stamp: 'len 10/28/2016 10:38'!
parent: aPowerSeriesRing
	parent _ aPowerSeriesRing! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 8/9/2019 09:43:30'!
* anObject
	((anObject isKindOf: RelaxedPowerSeries) and: [anObject parent = parent])
		ifTrue: [^ parent coefficients: (coefficients convolution: anObject coefficients)].
	(self scalars includes: anObject)
		ifTrue: [^ parent coefficients: coefficients * anObject].
	(anObject isKindOf: Polynomial)
		ifTrue: [^ self * (parent !! anObject)].
	^ DomainError signal! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 8/9/2019 09:43:30'!
+ anObject
	(anObject isKindOf: RelaxedPowerSeries)
		ifTrue: [^ parent coefficients: coefficients + anObject coefficients].
	(self scalars includes: anObject)
		ifTrue: [^ parent coefficients: coefficients + (CFiniteSequence to: self scalars initialValues: {anObject} periodic: {anObject zero})].
	(anObject isKindOf: Polynomial)
		ifTrue: [^ self + (parent !! anObject)].
	^ DomainError signal! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 6/2/2019 17:14:04'!
negated
	^ parent coefficients: coefficients negated! !

!RelaxedPowerSeries methodsFor: 'arithmetic' stamp: 'len 6/5/2019 17:04:52'!
reciprocal
	| b1 a |
	a _ coefficients.
	b1 _ (a at: 1) reciprocal.
	^ parent coefficients:
		(Sequence
			to: self scalars
			initialValues: {b1}
			recurrence:
				[:b :n|
				b1 negated * ((2 to: n) sum: [:i| (a at: i) * (b at: n-i+1)])])! !

!RelaxedPowerSeries methodsFor: 'comparing' stamp: 'len 8/9/2019 09:43:30'!
= anObject
	^ (anObject isKindOf: RelaxedPowerSeries) and: [coefficients = anObject coefficients]! !

!RelaxedPowerSeries methodsFor: 'comparing' stamp: 'len 12/28/2015 19:45'!
hash
	^ coefficients hash! !

!RelaxedPowerSeries methodsFor: 'converting' stamp: 'len 12/27/2015 19:09'!
asRationalFunction
	^ coefficients generatingFunction! !

!RelaxedPowerSeries methodsFor: 'converting' stamp: 'len 8/7/2019 10:08:50'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the truncation to anInteger terms."
	^ (self parent jet: anInteger) !! self! !

!RelaxedPowerSeries methodsFor: 'operations' stamp: 'len 6/2/2019 17:11:59'!
derivative
	^ parent coefficients: coefficients << 1 * CFiniteSequence identity! !

!RelaxedPowerSeries methodsFor: 'operations' stamp: 'len 2/29/2016 04:58'!
value: anObject upTo: n
	| answer |
	answer _ anObject zero.
	self terms do: [:each| each degree > n ifTrue: [^ answer]. answer _ answer + (each value: anObject)].
	^ answer! !

!RelaxedPowerSeries methodsFor: 'printing' stamp: 'len 6/6/2019 13:22:39'!
printOn: aStream
	| P f n |
	n _ coefficients printLimit.
	P _ parent base.
	f _ (P coefficients: (coefficients first: n)) orderedBy: #llex.
	aStream print: f; nextPutAll: ' + ', 'O' italic, '('; print: P x ^ (n+1); nextPut: $)! !

!RelaxedPowerSeries methodsFor: 'testing' stamp: 'len 4/11/2016 23:09'!
isZero
	^ coefficients isZero! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 9/16/2022 08:08:03'!
cos
	"The power series expansion of cos(x)."
	^ ℚ powerSeries coefficients:
		(Sequence to: ℚ map: [:n| n even ifTrue: [0] ifFalse: [|k| k _ n-1/2. (-1)^k / (2*k) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 9/16/2022 08:08:09'!
exp
	"The power series expansion of exp(x)."
	^ ℚ powerSeries
		coefficients: (Sequence to: ℚ map: [:n| 1 / n factorial])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 9/16/2022 08:08:14'!
sin
	"The power series expansion of sin(x)."
	^ ℚ powerSeries
		coefficients: (Sequence to: ℚ map: [:n| n odd ifTrue: [0] ifFalse: [|k| k _ n/2-1. (-1)^k / (2*k+1) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 12/27/2015 16:21'!
coefficients: aSequence
	^ self new coefficients: aSequence! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 11/25/2016 10:32:44'!
fromPolynomial: aPolynomial
	^ self coefficients: (CFiniteSequence to: aPolynomial scalars initialValues: aPolynomial coefficients periodic: {aPolynomial scalars zero})! !

!Sum methodsFor: 'accessing' stamp: 'len 1/10/2018 21:04:34'!
at: anObject
	^ coefficients at: anObject ifAbsent: [self scalars zero]! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 10:02:25'!
at: key add: value
	^ coefficients at: key addAndRemoveIfZero: value! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 17:29:29'!
at: anObject ifAbsent: aBlock
	^ coefficients at: anObject ifAbsent: aBlock! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 17:29:44'!
at: anObject ifPresent: aBlock
	^ coefficients at: anObject ifPresent: aBlock! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 17:27:44'!
at: key put: value
	value isZero ifTrue: [coefficients removeKey: key ifAbsent: []. ^ value].
	^ coefficients at: key put: value! !

!Sum methodsFor: 'accessing' stamp: 'len 7/22/2020 09:33:46'!
bitSize
	| answer |
	answer _ 0.
	coefficients withSupportDo: [:value :key| answer _ answer + key bitSize + value bitSize].
	^ answer! !

!Sum methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:44'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars numerators one.
	self do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 20:33:21'!
keys
	^ coefficients keys! !

!Sum methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:48'!
numerator
	self scalars isLocalization ifFalse: [^ self].
	^ self * self denominator over: self scalars numerators! !

!Sum methodsFor: 'accessing' stamp: 'len 7/21/2020 17:39:14'!
parent
	^ parent! !

!Sum methodsFor: 'accessing' stamp: 'len 4/23/2018 20:40:20'!
representative
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ self apply: self scalars projection section! !

!Sum methodsFor: 'accessing' stamp: 'len 1/10/2018 19:57:14'!
scalars
	^ parent scalars! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 09:55:05'!
size
	^ coefficients size! !

!Sum methodsFor: 'accessing' stamp: 'len 1/10/2018 21:09:50'!
zero
	^ parent zero! !

!Sum methodsFor: 'comparing' stamp: 'len 7/23/2020 08:15:05'!
= aSum
	^ coefficients = aSum coefficients! !

!Sum methodsFor: 'comparing' stamp: 'len 11/18/2016 20:11:39'!
hash
	^ coefficients hash! !

!Sum methodsFor: 'converting' stamp: 'len 1/10/2018 21:09:44'!
over: aRing
	| newCoefficients |
	self scalars = aRing ifTrue: [^ self].
	newCoefficients _ Dictionary new: self size.
	self associationsDo: [:each| | newValue |
		(newValue _ aRing !! each value) isZero
			ifFalse: [newCoefficients add: (each copy value: newValue)]].
	^ (self class coefficients: newCoefficients) parent: (parent over: aRing)! !

!Sum methodsFor: 'copying' stamp: 'len 11/18/2016 17:34:33'!
postCopy
	coefficients _ coefficients copy! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 09:54:31'!
associationsDo: aBlock
	coefficients associationsDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 1/10/2018 21:43:05'!
coefficientsDo: aBlock
	coefficients do: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 1/10/2018 21:22:05'!
collect: aBlock 
	| newCoefficients |
	newCoefficients _ Dictionary new: self size.
	self associationsDo: [:each| | newValue |
		(newValue _ aBlock value: each value) isZero ifFalse: [newCoefficients at: each key put: newValue]].
	^ (self class coefficients: newCoefficients) parent: parent! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 09:54:10'!
collectNoZero: aBlock
	"The result of aBlock must never be zero."
	^ self copy associationsDo: [:each| each value: (aBlock value: each value)]! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 20:24:16'!
do: aBlock
	coefficients do: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 17:45:00'!
keysAndValuesDo: aBlock
	coefficients keysAndValuesDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 11/18/2016 17:45:38'!
keysDo: aBlock
	coefficients keysDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 7/22/2020 09:32:43'!
supportDo: aBlock
	coefficients keysDo: aBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 7/24/2020 12:14:38'!
withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	self withSupportDo: elementAndIndexBlock! !

!Sum methodsFor: 'enumerating' stamp: 'len 7/24/2020 12:14:26'!
withSupportDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	coefficients keysAndValuesDo: [:key :value| elementAndIndexBlock value: value value: key]! !

!Sum methodsFor: 'operations' stamp: 'len 11/18/2016 09:51:39'!
* aNumber
	^ self collect: [:each| each*aNumber]! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:56:21'!
+ aSum
	| other answer |
	self size <= aSum size
		ifTrue: [answer _ aSum copy. other _ self]
		ifFalse: [answer _ self copy. other _ aSum].
	other associationsDo: [:each| answer at: each key add: each value].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:56:07'!
- aSum
	| answer |
	self size <= aSum size
		ifTrue:
			[answer _ aSum negated.
			self associationsDo: [:each| answer at: each key add: each value]]
		ifFalse:
			[answer _ self copy.
			aSum associationsDo: [:each| answer at: each key add: each value negated]].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 2/6/2017 03:13:39'!
/ aNumber
	^ self collect: [:each| each / aNumber]! !

!Sum methodsFor: 'operations' stamp: 'len 5/14/2020 12:31:41'!
// anElement
	^ self collect: [:each| each // anElement]! !

!Sum methodsFor: 'operations' stamp: 'len 5/14/2020 12:31:08'!
\\ anElement
	^ self collect: [:each| each \\ anElement]! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:55:32'!
· aSum
	| answer |
	self size <= aSum size ifFalse: [^ aSum · self].
	answer _ self scalars zero.
	coefficients associationsDo: [:each|
		aSum at: each key ifPresent: [:value2| answer _ answer + (each value*value2)]].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 7/18/2020 05:17:14'!
convolution: aSum
	| newCoefficients |
	newCoefficients _ Dictionary new: self size + aSum size.
	self associationsDo: [:each|
		aSum associationsDo: [:other|
			newCoefficients at: each key * other key addAndRemoveIfZero: each value * other value]].
	^ (self class coefficients: newCoefficients) parent: parent! !

!Sum methodsFor: 'operations' stamp: 'len 5/6/2019 02:55:06'!
hadamard: aSum
	"Answer the pointwise product with the argument, also called Hadamard product."
	| answer |
	self size <= aSum size ifFalse: [^ aSum hadamard: self]. "assuming it's commutative"
	answer _ self copyEmpty.
	coefficients associationsDo: [:each| | key |
		key _ each key.
		answer at: key put: each value * (aSum at: key)].
	^ answer! !

!Sum methodsFor: 'operations' stamp: 'len 11/18/2016 09:57:04'!
negated
	^ self collectNoZero: [:each| each negated]! !

!Sum methodsFor: 'printing' stamp: 'len 2/25/2017 09:43:39'!
printCoefficient: coefficient on: aStream
	| c |
	((c _ coefficient) printString first = $- "or: [coefficient isNumber and: [coefficient negative]]")
		ifTrue: [aStream nextPut: $-. c _ c negated].
	((c isKindOf: Polynomial) and: [c isMonomial])
		ifTrue: [aStream print: c. ^ self].
	(c printString allSatisfy: [:each| each isAlphaNumeric or: ['{}()/' includes: each]])
		ifTrue: [aStream print: c. ^ self].
	aStream nextPut: $(; print: c; nextPut: $)! !

!Sum methodsFor: 'printing' stamp: 'len 2/28/2017 08:43:47'!
printLiteral: anObject on: aStream
	((anObject isKindOf: Word) and: [anObject isEmpty])
		ifTrue: [^ self].
	anObject isInteger
		ifTrue:
			[aStream isText
				ifTrue: [aStream nextPut: $x; nextPutAll: anObject printString sub]
				ifFalse: [aStream nextPutAll: '{x', anObject printString, '}'].
			^ self].
	(aStream isText and: [anObject isString])
			ifTrue: [aStream nextPutAll: anObject asText]
			ifFalse: [aStream print: anObject]! !

!Sum methodsFor: 'printing' stamp: 'len 2/25/2017 08:38:18'!
printOn2: aStream
	aStream nextPut: $(.
	(Iterator on: self performing: #associationsDo:)
		do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)! !

!Sum methodsFor: 'printing' stamp: 'len 1/10/2018 21:04:41'!
printOn: aStream
	| elements c |
	self isZero ifTrue: [aStream nextPutAll: '0'. ^ self].
	elements _ self keys asSortedCollection: [:a :b| a printString <= b printString].
	c _ self at: elements first.
	c printString first = $-
		ifTrue: [c _ c negated. aStream nextPut: $-].
	c ~= self scalars one ifTrue: [self printCoefficient: c on: aStream].
	self printLiteral: elements first on: aStream.
	elements allButFirst do: [:each|
		c _ self at: each.
		c printString first = $-
			ifTrue: [aStream nextPutAll: ' - '. c _ c negated]
			ifFalse: [aStream nextPutAll: ' + '].
		c ~= c one ifTrue: [self printCoefficient: c on: aStream].
		self printLiteral: each on: aStream]! !

!Sum methodsFor: 'testing' stamp: 'len 11/18/2016 09:56:53'!
isZero
	^ coefficients isEmpty! !

!Sum methodsFor: 'private' stamp: 'len 11/18/2016 20:11:53'!
coefficients
	^ coefficients! !

!Sum methodsFor: 'private' stamp: 'len 9/16/2018 16:12:06'!
coefficients: aDictionary
	coefficients _ aDictionary.
	coefficients associationsDo: [:each| each value isZero ifTrue: [self error: 'the coefficinets dictionary contains zeroes']]! !

!Sum methodsFor: 'private' stamp: 'len 7/21/2020 17:39:28'!
parent: aFreeModule
	parent _ aFreeModule! !

!Sum class methodsFor: 'instance creation' stamp: 'len 1/10/2018 19:58:43'!
coefficients: aDictionary
	^ self basicNew coefficients: aDictionary! !

!Sum class methodsFor: 'instance creation' stamp: 'len 7/23/2020 08:13:42'!
coefficients: aDictionary parent: aFreeModule
	^ self basicNew coefficients: aDictionary; parent: aFreeModule! !

!Sum class methodsFor: 'instance creation' stamp: 'len 9/16/2018 16:12:13'!
new
	^ self coefficients: Dictionary new! !

!Sum class methodsFor: 'instance creation' stamp: 'len 1/10/2018 19:58:56'!
new: anInteger
	^ self coefficients: (Dictionary new: anInteger)! !

!Sum class methodsFor: 'instance creation' stamp: 'len 7/23/2020 08:14:37'!
new: anInteger parent: aFreeModule
	^ self coefficients: (Dictionary new: anInteger) parent: aFreeModule! !

!Sum class methodsFor: 'instance creation' stamp: 'len 7/23/2020 08:14:21'!
parent: aFreeModule
	^ self coefficients: Dictionary new parent: aFreeModule! !

!TupleOld methodsFor: 'accessing' stamp: 'len 5/10/2020 19:24:35'!
associates
	^ self scalars units collect: [:each| self * each]! !

!TupleOld methodsFor: 'accessing' stamp: 'len 4/20/2022 18:27:33'!
at: anInteger
	anInteger isInteger ifFalse: [^ (coefficients at: anInteger position) at: anInteger monomial].
	^ coefficients at: anInteger! !

!TupleOld methodsFor: 'accessing' stamp: 'len 4/20/2022 18:27:40'!
at: anInteger ifAbsent: aBlock
	anInteger isInteger ifFalse: [^ (coefficients at: anInteger position ifAbsent: [^aBlock value]) at: anInteger monomial ifAbsent: aBlock].
	^ coefficients at: anInteger ifAbsent: aBlock! !

!TupleOld methodsFor: 'accessing' stamp: 'len 3/29/2016 18:54'!
bitSize
	| answer |
	answer _ 0.
	self do: [:each| answer _ answer + each bitSize].
	^ answer! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:33'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars numerators one.
	self do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/5/2016 09:26'!
first
	^ self at: 1! !

!TupleOld methodsFor: 'accessing' stamp: 'len 4/26/2022 05:58:10'!
firstNonzeroIndex
	"Answer the index of the first nonzero entry, or nil."
	^ coefficients firstNonzeroIndex! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/6/2016 04:32'!
last
	^ self at: self size! !

!TupleOld methodsFor: 'accessing' stamp: 'len 5/19/2022 05:53:36'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	^ coefficients lastNonzeroIndex! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/23/2022 21:31:06'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ parent lift fill: [:i| (self at: i) lift]! !

!TupleOld methodsFor: 'accessing' stamp: 'len 11/20/2022 11:33:39'!
numerator
	| d |
	self scalars isLocalization ifFalse: [^ self].
	d _ self scalars !! self denominator.
	^ self scalars numerators ^ self size filling: [:i| ((self at: i) * d) numerator]! !

!TupleOld methodsFor: 'accessing' stamp: 'len 5/28/2020 13:09:52'!
parent
	^ parent! !

!TupleOld methodsFor: 'accessing' stamp: 'len 5/28/2020 13:09:44'!
scalars
	"Answer the ring of scalars (or ring of coefficients) of the receiver."
	^ parent scalars! !

!TupleOld methodsFor: 'accessing' stamp: 'len 5/16/2020 12:27:17'!
second
	^ self at: 2! !

!TupleOld methodsFor: 'accessing' stamp: 'len 4/20/2022 18:30:18'!
size
	^ coefficients size

"or:
	^ parent size"! !

!TupleOld methodsFor: 'accessing' stamp: 'len 3/29/2016 19:06'!
x
	^ self at: 1! !

!TupleOld methodsFor: 'accessing' stamp: 'len 3/29/2016 19:06'!
y
	^ self at: 2! !

!TupleOld methodsFor: 'accessing' stamp: 'len 3/29/2016 19:06'!
z
	^ self at: 3! !

!TupleOld methodsFor: 'comparing' stamp: 'len 5/19/2022 05:50:52'!
= anObject
	| n otherCoefficients |
	self class = anObject class ifFalse: [^ false].
"	scalars = anObject scalars ifFalse: [^ false]."
	otherCoefficients _ anObject coefficients.
	(n _ coefficients size) = otherCoefficients size ifFalse:
		[(n _ self lastNonzeroIndex) = anObject lastNonzeroIndex ifFalse: [^ false]].
	n ifNotNil: [1 to: n do: [:i| (coefficients at: i) = (otherCoefficients at: i) ifFalse: [^ false]]].
	^ true! !

!TupleOld methodsFor: 'comparing' stamp: 'len 5/24/2022 06:04:10'!
~ aTuple
	"Answer true if the receiver and the argument are equivalent as homogeneous tuples."
	self scalars isIntegralDomain ifTrue:
		[| depth x y |
		(depth _ self lastNonzeroIndex) ~= aTuple lastNonzeroIndex ifTrue: [^ false].
		depth isNil ifTrue: [^ true].
		x _ self at: depth.
		y _ aTuple at: depth.
		1 to: depth do: [:i| (self at: i) * y = ((aTuple at: i) * x) ifFalse: [^ false]].
		^ true].
	"Over a PIR we compare use their Howell normal form:"
"	self scalars isPIR ifTrue: [^ self asRowMatrix echelon = aTuple asRowMatrix echelon]."
	"In a general ring we have to check all units:"
	^ self scalars units anySatisfy: [:any| self * any = aTuple]! !

!TupleOld methodsFor: 'comparing' stamp: 'len 5/22/2022 20:09:29'!
⊥ aTuple
	"Answer true if the receiver and the argument are orthogonal."
	^ (self · aTuple) isZero! !

!TupleOld methodsFor: 'comparing' stamp: 'len 4/20/2022 18:29:33'!
hash
	| answer |
	answer _ 0.
	coefficients do: [:each| each isZero ifFalse: [answer _ answer hashMultiply + each hash]].
	^ answer! !

!TupleOld methodsFor: 'converting' stamp: 'len 11/8/2016 07:19'!
adaptToCollection: aCollection andSend: selector
	self size = aCollection size ifFalse: [^ self error: 'sizes don''t match'].
	^ aCollection withIndexCollect: [:each :index| each perform: selector with: (self at: index)]! !

!TupleOld methodsFor: 'converting' stamp: 'len 4/20/2022 18:27:20'!
asArray
	^ coefficients asArray! !

!TupleOld methodsFor: 'converting' stamp: 'len 7/7/2020 09:46:27'!
asMatrix
	"Tuples are equivalent to columns."
	^ Matrix columns: {self}! !

!TupleOld methodsFor: 'converting' stamp: 'len 3/13/2018 18:48:37'!
asRowMatrix
	^ Matrix rows: {self}! !

!TupleOld methodsFor: 'converting' stamp: 'len 11/7/2016 13:48'!
asTuple
	^ self! !

!TupleOld methodsFor: 'converting' stamp: 'len 5/29/2020 08:49:26'!
over: aRing
	self scalars = aRing ifFalse: [^ (self parent over: aRing) elementAt: self]! !

!TupleOld methodsFor: 'converting' stamp: 'len 12/13/2016 16:49:22'!
readStream
	^ ReadStream on: self! !

!TupleOld methodsFor: 'copying' stamp: 'len 4/20/2022 18:25:42'!
, anObject
	anObject isTuple
		ifTrue: [^ self scalars^(self size + anObject size) elementAt: coefficients, anObject coefficients].
	anObject isArray
		ifTrue: [^ self scalars^(self size + anObject size) elementAt: coefficients asArray, anObject asArray].
	^ self copyWith: anObject! !

!TupleOld methodsFor: 'copying' stamp: 'len 5/30/2020 14:29:08'!
coproject: aList
	| answer |
	answer _ self zero copy.
	aList do: [:i| answer at: i put: (self at: i)].
	^ answer! !

!TupleOld methodsFor: 'copying' stamp: 'len 4/20/2022 18:28:33'!
copyFrom: start to: stop
	^ self class coefficients: (coefficients copyFrom: start to: stop) parent: self scalars^(stop-start+1)! !

!TupleOld methodsFor: 'copying' stamp: 'len 4/20/2022 18:28:43'!
copyWith: anElement
	^ self class coefficients: (coefficients copyWith: self scalars !! anElement) parent: self scalars^(self size + 1)! !

!TupleOld methodsFor: 'copying' stamp: 'len 11/22/2022 15:51:10'!
postCopy
	coefficients _ coefficients copy! !

!TupleOld methodsFor: 'deprecated' stamp: 'len 5/30/2019 07:23:04'!
add: aTuple times: aNumber
	self add: aTuple times: aNumber startingAt: 1! !

!TupleOld methodsFor: 'deprecated' stamp: 'len 5/1/2022 07:25:10'!
add: aTuple times: aNumber from: startIndex to: stopIndex
	coefficients replaceFrom: startIndex to: stopIndex with: coefficients startingAt: startIndex plus: aTuple coefficients startingAt: startIndex timesScalar: aNumber! !

!TupleOld methodsFor: 'deprecated' stamp: 'len 5/30/2019 07:10:41'!
add: aTuple times: aNumber startingAt: startIndex
	self add: aTuple times: aNumber from: startIndex to: (self size min: aTuple size)! !

!TupleOld methodsFor: 'deprecated' stamp: 'len 1/16/2018 22:02:42'!
multiplyBy: aNumber
	self multiplyBy: aNumber startingAt: 1! !

!TupleOld methodsFor: 'deprecated' stamp: 'len 5/1/2022 07:26:25'!
multiplyBy: aNumber from: startIndex to: stopIndex
	coefficients replaceFrom: startIndex to: stopIndex with: coefficients startingAt: startIndex timesScalar: aNumber! !

!TupleOld methodsFor: 'deprecated' stamp: 'len 1/16/2018 22:01:18'!
multiplyBy: aNumber startingAt: startIndex
	self multiplyBy: aNumber from: startIndex to: self size! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:27:15'!
allSatisfy: aBlock
	^ coefficients allSatisfy: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:27:18'!
anySatisfy: aBlock
	^ coefficients anySatisfy: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:28:01'!
collect: aBlock
	^ self class coefficients: (coefficients collect: aBlock) parent: parent! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 5/1/2022 07:51:22'!
count: aBlock
	^ coefficients count: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:28:50'!
detect: aBlock ifNone: exceptionBlock 
	^ coefficients detect: aBlock ifNone: exceptionBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:28:57'!
do: aBlock
	coefficients do: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 11/5/2016 09:26'!
do: elementBlock separatedBy: separatorBlock
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst _ true.
	self do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst _ false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:29:04'!
findFirst: aBlock
	^ coefficients findFirst: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:29:13'!
findLast: aBlock
	^ coefficients findLast: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 11/5/2016 06:48'!
inject: thisValue into: binaryBlock 
	| nextValue |
	nextValue _ thisValue.
	self do: [:each | nextValue _ binaryBlock value: nextValue value: each].
	^ nextValue! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 1/13/2022 09:27:25'!
max: aBlock
	"Answer the maximum of evaluating aBlock on all the coefficients of the receiver."
	| answer |
	self isEmpty ifTrue: [^ self error: 'empty tuple'].
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:05'!
product
	self isEmpty ifTrue: [^ self scalars one].
	^ coefficients product! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 12/13/2016 09:55:53'!
product: aBlock
	^ self inject: self scalars one into: [:nextValue :each| nextValue * (aBlock value: each)]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:08'!
reverseDo: aBlock
	coefficients reverseDo: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:23'!
sum
	self isEmpty ifTrue: [^ self scalars zero].
	^ coefficients sum! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 12/13/2016 09:55:29'!
sum: aBlock
	^ self inject: self scalars zero into: [:nextValue :each| nextValue + (aBlock value: each)]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 7/22/2020 09:31:48'!
supportDo: aBlock
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: index]]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 1/4/2022 12:32:19'!
with: aTuple collect: aBlock
	^ parent filling: [:i| aBlock value: (self at: i) value: (aTuple at: i)]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 3/13/2020 16:15:51'!
with: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 5/29/2020 16:31:58'!
withIndexCollect: aBlock
	^ parent filling: [:i| aBlock value: (self at: i) value: i]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 18:30:29'!
withIndexDo: aBlock
	coefficients withIndexDo: aBlock! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 7/26/2020 13:16:06'!
withNamesDo: aBlock
	| names |
	names _ parent names.
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: (self at: index) value: (names at: index)]]! !

!TupleOld methodsFor: 'enumerating' stamp: 'len 7/19/2020 07:58:07'!
withSupportDo: aBlock
	1 to: self size do: [:index| (self isZeroAt: index) ifFalse: [aBlock value: (self at: index) value: index]]! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/1/2022 07:37:16'!
% anInteger
	self scalars = ℤ ifFalse: [self error: 'not over the integers'].
	^ self apply: (ℤ / anInteger) projection! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/1/2022 07:38:26'!
* anObject
	(self scalars includes: anObject) ifTrue: [^ self class coefficients: coefficients * anObject parent: parent].
	anObject isMatrix ifTrue: [^ (self asRowMatrix * anObject) asTuple].
	anObject isTuple ifTrue: [^ 	self class coefficients: coefficients * anObject coefficients parent: (self size <= anObject size ifTrue: [parent] ifFalse: [anObject parent])].
	^ self * (self scalars !! anObject)! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:25:30'!
+ aTuple
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #+].
	^ self class coefficients: coefficients + aTuple coefficients parent: (self size >= aTuple size ifTrue: [parent] ifFalse: [aTuple parent])! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:25:55'!
- aTuple
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #-].
	^ self class coefficients: coefficients - aTuple coefficients parent: (self size >= aTuple size ifTrue: [parent] ifFalse: [aTuple parent])! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/17/2022 04:23:43'!
/ anElement
	"If the argument is a matrix, answer X such that XA = B, where B is self and A is the argument.  X is called a lift of B along A.
	If the argument is a scalar, just divide each element of the tuple by it."
	anElement isMatrix ifTrue: [^ (anElement colift: self asMatrix transpose) asTuple].
	"We force coercion to the coefficients ring in case the division was not exact and it returned an element outside of the ring. This can happen with Integers, for example 1 / 2 doesn't produce a division error but instead returns a Fraction."
	^ self class coefficients: parent scalars !!!! (coefficients / anElement) parent: parent! !

!TupleOld methodsFor: 'operations' stamp: 'len 1/13/2022 09:14:56'!
// anElement
	^ self collect: [:each| each // anElement]! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/6/2022 05:55:57'!
<< anInteger
	"Answer the left shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	^ self >> anInteger negated! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/6/2022 06:03:31'!
<<< anInteger
	"Answer the circular left shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	^ self >>> anInteger negated! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/6/2022 05:55:42'!
>> anInteger
	"Answer the right shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	anInteger = 0 ifTrue: [^ self].
	^ self class coefficients: coefficients >> anInteger parent: parent! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/6/2022 06:04:28'!
>>> anInteger
	"Answer the circular right shift of the receiver by anInteger places.
	The answer is a new tuple of the same size as the receiver."
	anInteger = 0 ifTrue: [^ self].
	^ self class coefficients: coefficients >>> anInteger parent: parent! !

!TupleOld methodsFor: 'operations' stamp: 'len 1/13/2022 09:14:51'!
\\ anElement
	^ self collect: [:each| each \\ anElement]! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:27:03'!
· aTuple
	(self isEmpty or: [aTuple isEmpty]) ifTrue: [^ self scalars zero].
	^ coefficients · aTuple coefficients! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 08:07:21'!
× aTuple
	"Answer the cross product of the receiver with the argument."
	(self size = 3 and: [aTuple parent = self parent]) ifFalse: [DomainError signal].
	^ self parent elementAt:
			{(self at: 2)*(aTuple at: 3) - ((self at: 3)*(aTuple at: 2)).
			(self at: 3)*(aTuple at: 1) - ((self at: 1)*(aTuple at: 3)).
			(self at: 1)*(aTuple at: 2) - ((self at: 2)*(aTuple at: 1))}! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/28/2021 11:40:42'!
⊗ anElement
	"Answer the outer product of the receiver with the argument."
	anElement isTuple ifTrue: [^ Matrix from: anElement parent to: self parent filling: [:i :j| (self at: i)*(anElement at: j)]].
	anElement isMatrix ifTrue: [^ self asMatrix ⊗ anElement transpose].
	^ super ⊗ anElement! !

!TupleOld methodsFor: 'operations' stamp: 'len 7/15/2021 10:41:40'!
annihilator
	^ self annihilatorIdeal generator! !

!TupleOld methodsFor: 'operations' stamp: 'len 8/12/2022 13:13:17'!
annihilatorIdeal
	| answer |
	answer _ self scalars asIdeal.
	self isZero ifTrue: [^ answer].
	self do: [:each| answer _ answer ∩ each annihilatorIdeal. answer isTrivial ifTrue: [^ answer]].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/28/2022 07:36:07'!
apply: aFunction
	"Answer a new tuple with coefficients obtainted by applying aFunction to the coefficients of the receiver.
	If aFunction is a block we assume the parent doesn't change, i.e. the block must evaluate to values within the scalars ring of the receiver."

	^ self class coefficients: (coefficients apply: aFunction) parent: (aFunction isBlock ifTrue: [parent] ifFalse: [parent over: aFunction codomain])! !

!TupleOld methodsFor: 'operations' stamp: 'len 8/9/2020 07:47:06'!
conjugate
	^ self collect: [:each| each conjugate]! !

!TupleOld methodsFor: 'operations' stamp: 'len 7/17/2019 11:02:11'!
content
	"Answer the GCD of the coefficients of the receiver.
	In a field, it is convenient to define the content as 1.
	For example, (2,4) has content 2 over Z and 1 over Q."
	| answer |
	self isZero ifTrue: [^ self scalars zero].
	self scalars isField ifTrue: [^ self scalars one].
	answer _ self scalars zero.
	self do: [:each| answer _ answer gcd: each].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:28:14'!
convolution: aTuple
	| result |
	self isEmpty ifTrue: [^ self].
	aTuple isEmpty ifTrue: [^ aTuple].
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #convolution:].
	result _ coefficients convolution: aTuple coefficients.
	^ self class coefficients: result parent: self scalars ^ result size! !

!TupleOld methodsFor: 'operations' stamp: 'len 11/27/2016 13:25:12'!
height
	| answer |
	answer _ 1.
	self do: [:each| answer _ answer max: each height].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 1/14/2022 10:59:49'!
homogenizedAt: anInteger
	^ (self copyFrom: 1 to: anInteger - 1), self scalars one, (self copyFrom: anInteger to: self size)! !

!TupleOld methodsFor: 'operations' stamp: 'len 7/27/2020 15:03:24'!
length
	^ self norm sqrt! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/1/2022 07:44:09'!
max
	^ coefficients max! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/1/2022 07:44:17'!
min
	^ coefficients min! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:29:59'!
negated
	^ self class coefficients: coefficients negated parent: parent! !

!TupleOld methodsFor: 'operations' stamp: 'len 8/14/2022 12:24:20'!
norm
	^ self · self! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/23/2022 20:20:56'!
normalization
	"Answer a unit u such that 'self * u' is a unique choice of associate. See >>normalized.
	NOTE: when the coefficients ring is not an integral domain this choice of unit might not be unique."
	^ (self at: (self lastNonzeroIndex ifNil: [^ self scalars one])) normalization! !

!TupleOld methodsFor: 'operations' stamp: 'len 1/9/2022 08:19:26'!
primitivePart
	"Answer the primitive part of the receiver."
	^ self isZero ifTrue: [self] ifFalse: [self / self content]! !

!TupleOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:30:15'!
reversed
	^ self class coefficients: coefficients reversed parent: parent! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/16/2020 13:32:19'!
stabilizers
	| answer last |
	self size <= 1 ifTrue: [^ #()].
	answer _ OrderedCollection new: self size - 1.
	last _ self at: 1.
	2 to: self size do: [:i| | ai ci |
		ai _ self at: i.
		ci _ last stabilizerWith: ai.
		answer add: ci.
		last _ ci*ai + last].
	^ answer! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/1/2022 07:44:56'!
support
	^ (1 to: self size) reject: [:each| self isZeroAt: each]! !

!TupleOld methodsFor: 'operations' stamp: 'len 5/1/2022 07:46:21'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ coefficients weight! !

!TupleOld methodsFor: 'printing' stamp: 'len 4/20/2022 18:30:02'!
printOn: aStream
	coefficients printOn: aStream! !

!TupleOld methodsFor: 'printing' stamp: 'len 7/8/2022 11:17:38'!
validate
	super validate.
	self assert: self parent rank = self size.
	self assert: (self allSatisfy: [:each| self scalars includes: each])! !

!TupleOld methodsFor: 'testing' stamp: 'len 8/11/2019 12:20:17'!
beginsWith: anArray
	"Answer true if the receiver starts with the argument collection."
	| n |
	self size < (n _ anArray size) ifTrue: [^ false].
	1 to: n do: [:index| (anArray at: index) = (self at: index) ifFalse: [^ false]].
	^ true! !

!TupleOld methodsFor: 'testing' stamp: 'len 4/20/2022 18:29:38'!
includes: anObject
	^ coefficients includes: anObject! !

!TupleOld methodsFor: 'testing' stamp: 'len 11/5/2016 07:12'!
isEmpty
	^ self size = 0! !

!TupleOld methodsFor: 'testing' stamp: 'len 7/26/2021 15:24:17'!
isMonomial
	^ self allSatisfy: [:each| each isMonomial]! !

!TupleOld methodsFor: 'testing' stamp: 'len 5/22/2018 16:53:54'!
isSequenceable
	^ true! !

!TupleOld methodsFor: 'testing' stamp: 'len 3/28/2018 18:01:30'!
isTuple
	^ true! !

!TupleOld methodsFor: 'testing' stamp: 'len 5/16/2020 10:34:21'!
isUnimodular
	"Answer true if the coefficients of the receiver generate the unit ideal.
	Some authors use the name 'primitive' instead of 'unimodular'."
	^ (self scalars * self asArray) isOne! !

!TupleOld methodsFor: 'testing' stamp: 'len 4/20/2022 18:29:44'!
isZero
	^ coefficients isZero! !

!TupleOld methodsFor: 'testing' stamp: 'len 4/20/2022 18:29:46'!
isZeroAt: anInteger
	^ coefficients isZeroAt: anInteger! !

!TupleOld methodsFor: 'private' stamp: 'len 6/6/2019 09:41:21'!
at: anInteger add: anElement
	^ self at: anInteger put: (self at: anInteger) + anElement! !

!TupleOld methodsFor: 'private' stamp: 'len 4/20/2022 18:27:52'!
at: anInteger put: anObject
	^ coefficients at: anInteger put: anObject! !

!TupleOld methodsFor: 'private' stamp: 'len 4/20/2022 18:31:34'!
coefficients
	^ coefficients! !

!TupleOld methodsFor: 'private' stamp: 'len 4/20/2022 18:30:48'!
coefficients: anArray parent: aFreeModule
	coefficients _ anArray.
	parent _ aFreeModule! !

!TupleOld methodsFor: 'private' stamp: 'len 4/20/2022 18:30:26'!
swap: i with: j
	coefficients swap: i with: j! !

!TupleOld class methodsFor: 'instance creation' stamp: 'len 4/20/2022 18:32:21'!
coefficients: anArray parent: aFreeModule
	^ self basicNew coefficients: anArray parent: aFreeModule! !

!TupleOld class methodsFor: 'instance creation' stamp: 'len 4/18/2022 15:41:57'!
new
	^ self shouldNotImplement! !

!EuclideanGaussianElimination methodsFor: 'private' stamp: 'len 4/28/2022 12:58:26'!
reduceStep
	"Perform one step in the reduction of the matrix using the current pivot."
	| row j |
	row _ currentPivot x.
	j _ currentPivot y.
	self assert: row = j.
	1 to: matrix height do: [:i|
		(i = j or: [matrix isZeroAt: i @ j])
			ifFalse:
				[| ajj aij euclid g bi bj ri rj |
				ajj _ matrix at: j @ j.
				aij _ matrix at: i @ j.
				euclid _ ExtendedEuclid on: {aij. ajj}.
				g _ euclid gcd.
				bi _ euclid bezout first.
				bj _ euclid bezout last.
"				euclid validate ifFalse: [self halt]."
				ri _ matrix rowAt: i.
				rj _ matrix rowAt: j.
				matrix rowAt: j put: ri * bi + (rj * bj).
				matrix rowAt: i put: rj * (aij // g) - (ri * (ajj // g))]]! !

!GaussBareiss methodsFor: 'private' stamp: 'len 5/30/2019 07:18:55'!
reduceStep
	"Perform one step reduction of the matrix using the current pivot."
	| row col pivot value |
	row _ currentPivot x.
	col _ currentPivot y.
	pivot _ matrix at: row @ col.
	1 to: matrix height do: [:i|
		(i = row or: [(value _ matrix at: i @ col) isZero])
			ifFalse:
				[self multiplyRow: i by: pivot.
				self addRow: row times: value negated toRow: i]]! !

!BerlekampMassey methodsFor: 'as yet unclassified' stamp: 'len 1/1/2022 09:33:44'!
run
	| F Fx x C L N m B d b s |
	x _ Fx x.
	C _ Fx one.
	L _ 0. "current number of errors"
	N _ 100. "total number of syndromes"
	B _ C.
	b _ F one. "copy of the last discrepancy d"
	m _ 1. "number of iterations since last update"
	0 to: N do: [:n|
		"calculate discrepancy:"
		d _ (s at: n) + ((1 to: L) sum: [:i| (C at: i) * (s at: n-i)]).
		d = 0
			ifTrue: [m _ m + 1]
			ifFalse: [2*L <= n
				ifTrue:
					[|t|
					t _ C.
					C _ C - ((x^m)*(d/b)*B).
					B _ t.
					L _ n + 1 - L.
					b _ d.
					m _ 1]
				ifFalse:
					[C _ C - ((x^m)*(d/b)*B).
					m _ m + 1]]].
	^ L! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/15/2019 23:57:46'!
* aBit
	aBit isMatrix ifTrue: [^ super * aBit].
	^ ((aBit isInteger and: [aBit even]) or: [aBit isZero]) ifTrue: [self zero] ifFalse: [self]! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:27:21'!
+ aBitTuple
	^ self class
		bits: (bits bitXor: aBitTuple asInteger)
		parent: (self size >= aBitTuple size ifTrue: [parent] ifFalse: [aBitTuple parent])! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 3/28/2016 06:51'!
- aBitTuple
	^ self + aBitTuple! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 7/15/2021 12:42:04'!
>> anInteger
	^ self class bits: (bits bitShift: anInteger) parent: parent! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:30:57'!
>>> anInteger
	anInteger = 0 ifTrue: [^ self].
	^ self class bits: (bits bitShift: anInteger) parent: self scalars^(self size + anInteger max: 0)! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 03:09:41'!
· aBitTuple
	^ (bits bitAnd: aBitTuple asInteger) bitParity = 0 ifTrue: [Zero] ifFalse: [One]

"	^ self hadamard: aBitTuple :: sum
"

"	| x |
	x _ self asInteger bitAnd: aBitTuple asInteger.
	[x > 1]
		whileTrue: [x _ x >> (x highBit // 2) bitAnd: x].
	^ x"! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/29/2020 07:47:48'!
convolution: aBitTuple
	| a b c |
	a _ bits.
	b _ aBitTuple asInteger.
	c _ 0.
	1 to: a highBit do: [:i|
		(a bitAt: i) = 1 ifTrue: [c _ c bitXor: b].
		b _ b bitShift: 1].
	^ self class bits: c parent: self scalars ^ (self size + aBitTuple size - 1 max: 0)! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:34:18'!
hadamard: aBitTuple
	"Answer the entrywise product with the argument, also called Hadamard product."
	^ self class bits: (bits bitAnd: aBitTuple asInteger) parent: (self size <= aBitTuple size ifTrue: [parent] ifFalse: [aBitTuple parent])! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 7/10/2020 17:23:55'!
length
	^ self norm! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 03:09:27'!
lift
	"Answer a representative integer tuple."
	^ self representative! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 3/28/2016 07:30'!
negated
	^ self! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 7/10/2020 17:25:49'!
norm
	self flag: #fix. "this assumes the inner product is the dot product"
	^ self sum! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 03:08:16'!
parity
	"Answer 1 if the receiver has an odd number of bits on, 0 if it has an even number of bits on."
	| n parity |
	bits = 0 ifTrue: [^ 0].
"	n _ bits >> (bits lowBit - 1). skip initial zeros, we could also skip chunks of zeros in the middle"
	n _ bits.
	parity _ 0.
	[n = 0] whileFalse: [parity _ parity bitXor: (n bitAnd: 1). n _ n >> 1].
	^ parity! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/29/2020 19:40:07'!
polynomialDivisionBy: aTuple
	| q r b n m |
	b _ aTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialDivisionBy: argument: aTuple) signal].
	q _ 0.
	r _ self asInteger.
	[(n _ r highBit) >= m] "n = degree(r)+1"
		whileTrue:
			[| i |
			i _ n-m.
			q _ q bitXor: 1 << i. "wasting some memory while creating 2^i"
			r _ r bitXor: b << i]. "also wasting some memory here"
	^ {self class newFrom: q. self class newFrom: r}! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/29/2020 19:40:15'!
polynomialRemainderOfDivisionBy: aTuple
	| r b n m |
	b _ aTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialRemainderOfDivisionBy: argument: aTuple) signal].
	r _ self asInteger.
	[(n _ r highBit) >= m] "n = degree(r)+1"
		whileTrue:
			[r _ r bitXor: b << (n-m)]. "wasting some memory here"
	^ self class newFrom: r! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 11/23/2016 05:42:05'!
polynomialValue: anObject
	Zero = anObject ifTrue: [^ self at: 1].
	One = anObject ifFalse: [^ super polynomialValue: anObject].
	^ self sum! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/28/2020 14:25:18'!
reversed
	^ self class bits: (bits bitReverse: self size) parent: parent! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 5/8/2019 02:58:43'!
sum
	"Answer the sum of the bits, i.e. the parity."
	^ bits bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!BitTuple2 methodsFor: 'operations' stamp: 'len 11/12/2016 08:41'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ self asInteger bitCount! !

!BitTuple2 methodsFor: 'printing' stamp: 'len 3/28/2016 04:52'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 5/8/2019 02:58:37'!
at: anInteger
	^ (bits bitAt: anInteger) = 1 ifTrue: [One] ifFalse: [Zero]! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 1/1/2022 21:56:38'!
at: anInteger put: aBit
	| bit |
	anInteger > self size ifTrue: [self error: 'index out of bounds'].
	bit _ aBit.
	bit isInteger ifTrue: [bit _ bit even ifTrue: [Zero] ifFalse: [One]].
	(self at: anInteger) = bit
		ifFalse: [bits _ bits bitXor: (1 bitShift: anInteger - 1)].
	^ bit! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 7/19/2020 08:06:56'!
bitAt: anInteger
	^ bits bitAt: anInteger! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 7/19/2020 08:08:54'!
bitAt: anInteger put: oneOrZero
	anInteger > self size ifTrue: [self error: 'index out of bounds'].
	(self bitAt: anInteger) = oneOrZero
		ifFalse: [bits _ bits bitXor: 1 << (anInteger - 1)].
	^ oneOrZero! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 1/2/2022 09:56:51'!
depth
	"Answer the index of the last nonzero entry, or 0.
	(Instead, Magma defines this as the index of the *first* nonzero entry.)
	See >>pivot for the index of the first nonzero entry."
	^ bits highBit! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 5/29/2020 15:21:10'!
representative
	"Answer a representative integer tuple."
	^ parent lift filling: [:i| bits bitAt: i]! !

!BitTuple2 methodsFor: 'accessing' stamp: 'len 5/4/2018 19:09:27'!
scalars
	^ One parent! !

!BitTuple2 methodsFor: 'comparing' stamp: 'len 7/27/2020 08:43:41'!
= anObject
	^ self class = anObject class and: [bits = anObject asInteger]! !

!BitTuple2 methodsFor: 'comparing' stamp: 'len 5/8/2019 03:06:15'!
hash
	^ bits hash! !

!BitTuple2 methodsFor: 'constants' stamp: 'len 5/28/2020 14:24:55'!
zero
	^ self class bits: 0 parent: parent! !

!BitTuple2 methodsFor: 'converting' stamp: 'len 5/8/2019 03:10:19'!
asInteger
	^ bits! !

!BitTuple2 methodsFor: 'copying' stamp: 'len 5/28/2020 14:29:53'!
, anObject
	anObject isTuple
		ifTrue: [^ self class bits: (bits bitXor: (anObject asInteger bitShift: self size)) parent: self scalars^(self size+anObject size)].
	^ self copyWith: anObject! !

!BitTuple2 methodsFor: 'copying' stamp: 'len 1/1/2022 21:09:00'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."
	| n |
	n _ stop - start + 1 max: 0.
	^ self class bits: ((bits bitShift: 1 - start) bitAnd: (1 bitShift: n) - 1) parent: self scalars^n! !

!BitTuple2 methodsFor: 'copying' stamp: 'len 5/28/2020 14:24:12'!
copyWith: aBit
	| answer |
	answer _ self class bits: bits parent: self scalars^(self size + 1).
	aBit = One ifTrue: [answer at: self size+1 put: One. ^ answer].
	aBit = Zero ifFalse: [DomainError signal].
	^ answer! !

!BitTuple2 methodsFor: 'enumerating' stamp: 'len 11/5/2016 09:14'!
anySatisfy: aBlock
	"Optimized. There are only two possible elements that could satisfy aBlock (1 or 0)."
	self isEmpty ifTrue: [^ false].
	(self isZero not and: [aBlock value: One]) ifTrue: [^ true].
	^ (aBlock value: Zero) and: [super anySatisfy: [:any| any = Zero]]! !

!BitTuple2 methodsFor: 'testing' stamp: 'len 1/3/2022 18:29:45'!
isZero
	^ bits == 0! !

!BitTuple2 methodsFor: 'testing' stamp: 'len 12/30/2021 11:34:15'!
isZeroAt: anInteger
	^ (bits bitAt: anInteger) = 0! !

!BitTuple2 methodsFor: 'private' stamp: 'len 5/8/2019 03:03:28'!
bits: anInteger
	bits _ anInteger! !

!BitTuple2 class methodsFor: 'initialization' stamp: 'len 5/23/2020 08:58:58'!
initialize
	| Z2 |
	super initialize.
	Z2 _ ℤ / 2.
	One _ Z2 one.
	Zero _ Z2 zero! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 1/1/2022 20:27:22'!
bits: anInteger parent: aFreeModule
	self assert: anInteger highBit <= aFreeModule rank.
	^ self basicNew bits: anInteger; parent: aFreeModule! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 5/28/2020 14:51:42'!
components: anArray parent: aFreeModule
	| answer |
	answer _ self bits: 0 parent: aFreeModule.
	anArray withIndexDo: [:each :i| answer at: i put: each].
	^ answer! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 11/5/2016 07:09'!
new
	^ self shouldNotImplement! !

!BitTuple2 class methodsFor: 'instance creation' stamp: 'len 5/28/2020 14:38:16'!
newFrom: anInteger
	anInteger isInteger ifTrue: [^ self bits: anInteger parent: One parent^anInteger highBit].
	^ super newFrom: anInteger! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 12:43:36'!
matrix
	^ matrix! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 5/5/2019 12:44:19'!
matrix: aMatrix
	matrix _ aMatrix! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 09:43:10'!
reduceStep: i mod: D
	| j c euclid B Ai Aj |
	"2. Initialize j for row reduction:"
	[j _ i-1. c _ 0.
	"3. Check zero:"
	[[j >= 1 and: [(S at: i@j) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "4. Euclidean step:"
			[euclid _ ExtendedEuclid on: {S at: i@i. S at: i@j}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ S columnAt: i) * euclid bezout first + ((Aj _ S columnAt: j) * euclid bezout second).
			S columnAt: j put: Aj * ((S at: i@i) // euclid gcd) - (Ai * ((S at: i@j) // euclid gcd)) \\ D.
			S columnAt: i put: B \\ D.
			j _ j - 1].
	"5. Initialize j for column reduction:"
	j _ i-1.
	"6. Check zero (find pivot):"
	[[j >= 1 and: [(S at: j@i) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "7. Euclidean step:"
			[euclid _ ExtendedEuclid on: {S at: i@i. S at: j@i}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ S rowAt: i) * euclid bezout first + ((Aj _ S rowAt: j) * euclid bezout second).
			S rowAt: j put: Aj * ((S at: i@i) // euclid gcd) - (Ai * ((S at: j@i) // euclid gcd)) \\ D.
			S rowAt: i put: B \\ D.
			j _ j - 1.
			c _ c + 1].
	c > 0] whileTrue "8. Repeat stage i?"! !

!ExtendedSmithReduction methodsFor: 'as yet unclassified' stamp: 'len 1/9/2022 09:42:05'!
run
	| n i D b k |
	n _ matrix height.
	L _ matrix codomain id.
	R _ matrix domain id.
	S _ matrix copy.
	elementaryDivisors _ OrderedCollection new: n.
	"1. Initialize i:"
	i _ n.
	D _ S determinant normalized.
	n = 1 ifTrue: [^ self].
	"2-9. Reduction:"
	[[self reduceStep: i mod: D.
	"9. Check the rest of the matrix:"
	b _ S at: i@i.
	k _ (1 to: i-1) detect: [:k0| ((1 to: i-1) allSatisfy: [:l| b | (S at: k0@l)]) not] ifNone: [].
	k isNil]
		whileFalse:
			[S rowAt: i put: (S rowAt: i) + (S rowAt: k).
			L rowAt: i put: (L rowAt: i) + (L rowAt: k)].
	"10. Next stage:"
	elementaryDivisors add: ((S at: i@i) gcd: D).
	D _ D // elementaryDivisors last.
	i = 2]
		whileFalse: [i _ i - 1].
	elementaryDivisors add: ((S at: 1@1) gcd: D).
	^ {L. S. R}! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:36:20'!
addColumn: a to: B
	^ (B columnAugmented: a) HNF columnAt: B width + 1! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 4/5/2018 15:36:28'!
addRow: b to: H
	| H0 |
	H0 _ H isSquare
		ifTrue: [H]
		ifFalse:
			[H rowAugmented: (H scalars ^ (H width - 1) :: zero, (H rowAugmented: b) determinant)].
	"..."! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 7/16/2016 01:28'!
matrix: aMatrix
	matrix _ aMatrix! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 4/12/2021 12:37:29'!
probablePivotsOf: aMatrix
	^ aMatrix % (10007 to: 46000) atRandom nextPrime :: echelon pivots
! !

!ModularHermiteReduction methodsFor: 'as yet unclassified' stamp: 'len 7/16/2016 04:35'!
run
"
	HNF _ matrix zero.
	HNF at: 1@1 put: (matrix at: 1@1).
	2 to: matrix numberOfColumns do: [:j|
		a _ matrix columnAt: j.
		x _ self addColumn: a.
		HNF _ addRow: a to: HNF | x
"! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:24:18'!
divide: anElement
	"Perform the division algorithm computing quotients, remainder and a unit."
	^ self notYetImplemented! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:25:02'!
divisors: anArray
	divisors _ anArray! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:18:30'!
reduce: anElement 
	"Answer Mora's normal form of anElement."
	| h L lh g ecartg |
	h _ anElement.
	L _ OrderedCollection withAll: divisors.
	[h isZero ifTrue: [^ h].
	lh _ h leadingMonomial.
	g _ nil.
	ecartg _ -1.
	L do: [:each|
		| ecart |
		(each leadingMonomial | lh and: [ecart _ each ecart. g isNil or: [ecart < ecartg]])
			ifTrue: [g _ each. ecartg _ ecart]].
	g isNil ifTrue: [^ h].
	ecartg > h ecart ifTrue: [L add: h].
	h _ h + (g * (lh / g leadingMonomial) * (h leadingCoefficient negated / g leadingCoefficient))] repeat! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:18:57'!
reduceFull: anElement
	^ self reduce: anElement! !

!MultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:19:20'!
reduces: anElement
	^ (self reduce: anElement) isZero! !

!MultivariateReducer class methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:25:16'!
divisors: anArray
	^ self new divisors: anArray! !

!GlobalMultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:23:36'!
divide: anElement
	"Perform the division algorithm computing quotients and remainder."
	| R quotients remainder p s fi q head i |
	R _ anElement isTuple ifTrue: [anElement scalars] ifFalse: [anElement parent].
	quotients _ Array new: divisors size withAll: R zero.
	divisors isEmpty ifTrue: [remainder _ anElement. ^ self].
	remainder _ anElement zero.
	s _ divisors size.
	p _ anElement.
	[p isZero] whileFalse:
		[head _ p leadingMonomial.
		i _ 1.
		[(fi _ divisors at: i) leadingMonomial | head or: [(i _ i + 1) > s]] whileFalse.
		i <= s "i.e. found fi with leading monomial dividing head"
			ifTrue:
				[q _ R coefficient: p leadingCoefficient / fi leadingCoefficient monomial: head / fi leadingMonomial.
				quotients at: i put: (quotients at: i) + q.
				p _ p + (fi * q negated)]
			ifFalse:
				[remainder _ remainder + p leadingTerm.
				p _ p tail]].
	^ {quotients. remainder}! !

!GlobalMultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:23:11'!
reduce: anElement
	"Answer a top-reduced normal form of anElement (without doing tail reduction)."
	| h |
	divisors isEmpty ifTrue: [^ anElement].
	h _ anElement.
	[h isZero]
		whileFalse:
			[| lh lg g |
			lh _ h leadingMonomial.
			g _ divisors detect: [:one| (lg _ one leadingMonomial) | lh] ifNone: [^ h].
			h _ h + (g * (lh / lg) * (h leadingCoefficient negated / g leadingCoefficient))].
	^ h! !

!GlobalMultivariateReducer methodsFor: 'as yet unclassified' stamp: 'len 1/14/2022 11:22:49'!
reduceFull: anElement
	"Answer the tail-reduced normal form of anElement (doing full tail reduction)."
	| g h |
	g _ anElement.
	h _ g zero.
	[g isZero]
		whileFalse:
			[g _ self reduce: g.
			g isZero ifFalse: [h _ h + g leadingTerm. g _ g tail]].
	^ h normalized! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/27/2016 07:02'!
initialize
	entries _ OrderedCollection new.
	'oeis.txt' asFileEntry readStream: [:aStream|
		[aStream atEnd]
			whileFalse:
				[| line |
				line _ aStream nextLine.
				line first = $#
					ifFalse:
						[| name tokens |
						tokens _ line findTokens: ','.
						name _ tokens first allButLast.
						entries add: name -> (tokens allButFirst collect: [:each| each asInteger]) asArray]]].
	entries _ entries asArray sort: [:a :b| a value size >= b value size]! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/27/2016 07:02'!
longestEntrySize
	^ entries first value size! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/20/2016 21:41'!
lookup2: anArray
	'oeis.txt' asFileEntry readStream: [:aStream|
		[aStream atEnd]
			whileFalse:
				[| line |
				line _ aStream nextLine.
				line first = $#
					ifFalse:
						[| name tokens |
						tokens _ line findTokens: ','.
						name _ tokens first allButLast.
						((1 to: (tokens size - 1 min: anArray size))
							allSatisfy: [:i| (tokens at: i+1) asInteger = (anArray at: i)])
								ifTrue: [^ name]]]].
		^ nil! !

!OEIS methodsFor: 'as yet unclassified' stamp: 'len 4/27/2016 07:04'!
lookup: anArray
	| array |
	array _ (anArray is: #Sequence) ifTrue: [anArray copyFrom: 1 to: self longestEntrySize] ifFalse: [anArray].
	entries do: [:each|
		each value size = 0 ifTrue: [^ nil].
		((1 to: (each value size min: array size))
			allSatisfy: [:i| (each value at: i) = (array at: i)])
				ifTrue: [^ each key]].			
	^ nil! !

!OEIS class methodsFor: 'as yet unclassified' stamp: 'len 4/25/2016 07:16'!
default
	^ Default ifNil: [Default _ self new]! !

!RealBox methodsFor: 'printing' stamp: 'len 5/23/2020 07:19:23'!
printOn: aStream
	components do: [:each| aStream print: each] separatedBy: [aStream nextPut: $×]! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:33'!
add: aRealInterval
	^ components add: aRealInterval! !

!RealBox methodsFor: 'accessing' stamp: 'len 12/13/97 15:21'!
addAll: aCollection
	"Add the elements in the argument to the receiver. Answer the argument."

	aCollection do: [ :each | self add: each].
	^ aCollection! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:33'!
at: anInteger
	"Answer the anInteger-th component of the receiver."
	^ components at: anInteger! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:35'!
dimension
	^ components inject: 0 into: [:sum :each| sum + each]! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:35'!
first
	^ self at: 1! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:35'!
last
	^ self at: self size! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:36'!
measure
	^ components inject: 1 into: [:measure :each| measure * each measure]! !

!RealBox methodsFor: 'accessing' stamp: 'len 11/23/2022 17:14:31'!
midpoint
	^ ℝ !!!! (components collect: [:each| each midpoint])! !

!RealBox methodsFor: 'accessing' stamp: 'len 11/23/2022 21:31:50'!
sample
	"Answer a sample point in the receiver."
	^ ℝ^self size fill: [:each| (self at: each) sample]! !

!RealBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:37'!
size
	^ components size! !

!RealBox methodsFor: 'accessing-private' stamp: 'len 1/11/2016 02:37'!
components
	^ components! !

!RealBox methodsFor: 'accessing-private' stamp: 'len 1/11/2016 02:37'!
components: aCollection
	components _ aCollection! !

!RealBox methodsFor: 'comparing' stamp: 'len 12/19/97 22:06'!
= anObject
	"Answer true if the receiver equals the argument."

	self size = anObject size ifFalse: [^ false].
	1 to: self size do: [ :each | (self at: each) = (anObject at: each) ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'comparing' stamp: 'len 6/28/2016 09:28'!
hash
	"Answer the hash value of the receiver."

	^ self size = 0
		ifTrue: [0]
		ifFalse: [self first hash hashMultiply + self last hash hashMultiply + self size hash]! !

!RealBox methodsFor: 'enumerating' stamp: 'len 1/11/2016 02:38'!
do: aBlock
	"Enumerate the components of the receiver."
	components do: aBlock! !

!RealBox methodsFor: 'initialization' stamp: 'len 1/11/2016 02:38'!
initialize
	components _ OrderedCollection new: 3! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:40'!
includes: aTuple
	1 to: self size do: [:each|
		((self at: each) includes: (aTuple at: each)) ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:40'!
isBounded
	self do: [:each| each isBounded ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:40'!
isClosed
	self do: [:each| each isClosed ifFalse: [^ false]].
	^ true! !

!RealBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:41'!
isOpen
	self do: [ :each | each isOpen ifFalse: [^ false]].
	^ true! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 5/28/2019 08:51:31'!
invariants
	invariants isNil ifTrue: [self run].
	^ invariants! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 5/25/2019 10:28:41'!
matrix
	^ matrix! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 5/25/2019 10:28:41'!
matrix: aMatrix
	matrix _ aMatrix! !

!SmithReduction methodsFor: 'accessing' stamp: 'len 6/1/2020 12:00:45'!
reduced
	^ (matrix scalars^self invariants size) endomorphisms diagonal: self invariants! !

!SmithReduction methodsFor: 'private' stamp: 'len 1/9/2022 09:42:37'!
reduceStep: i mod: R
	| j c euclid B Ai Aj |
	"2. Initialize j for row reduction:"
	[j _ i-1. c _ 0.
	"3. Check zero:"
	[[j >= 1 and: [(matrix at: i@j) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "4. Euclidean step:"
			[euclid _ ExtendedEuclid on: {matrix at: i@i. matrix at: i@j}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ matrix columnAt: i) * euclid bezout first + ((Aj _ matrix columnAt: j) * euclid bezout second).
			matrix columnAt: j put: Aj * ((matrix at: i@i) // euclid gcd) - (Ai * ((matrix at: i@j) // euclid gcd)) \\ R.
			matrix columnAt: i put: B \\ R.
			j _ j - 1].
	"5. Initialize j for column reduction:"
	j _ i-1.
	"6. Check zero (find pivot):"
	[[j >= 1 and: [(matrix at: j@i) isZero]] whileTrue: [j _ j - 1].
	j >= 1]
		whileTrue: "7. Euclidean step:"
			[euclid _ ExtendedEuclid on: {matrix at: i@i. matrix at: j@i}.
"			euclid verifyMinimal ifFalse: [self halt]."
			B _ (Ai _ matrix rowAt: i) * euclid bezout first + ((Aj _ matrix rowAt: j) * euclid bezout second).
			matrix rowAt: j put: Aj * ((matrix at: i@i) // euclid gcd) - (Ai * ((matrix at: j@i) // euclid gcd)) \\ R.
			matrix rowAt: i put: B \\ R.
			j _ j - 1.
			c _ c + 1].
	c > 0] whileTrue "8. Repeat stage i?"! !

!SmithReduction methodsFor: 'private' stamp: 'len 1/9/2022 09:42:47'!
run
	| n i R b k |
	n _ matrix height.
	invariants _ OrderedCollection new: n.
	"1. Initialize i:"
	i _ n.
	R _ matrix determinant.
	R _ R normalized. "replaces R _ R abs"
	n = 1 ifTrue: [invariants add: R. ^ self].
	"2-9. Reduction:"
	[[self reduceStep: i mod: R.
	"9. Check the rest of the matrix:"
	b _ matrix at: i@i.
	k _ (1 to: i-1) detect: [:k0| ((1 to: i-1) allSatisfy: [:l| b | (matrix at: k0@l)]) not] ifNone: [].
	k isNil]
		whileFalse: [matrix rowAt: i put: (matrix rowAt: i) + (matrix rowAt: k)].
	"10. Next stage:"
	invariants add: ((matrix at: i@i) gcd: R).
	R _ R // invariants last.
	i = 2]
		whileFalse: [i _ i - 1].
	invariants add: ((matrix at: 1@1) gcd: R).
"	diagonal _ matrix scalars tuple: diagonal"! !

!SparseTuple methodsFor: 'copying' stamp: 'len 7/18/2020 06:12:12'!
postCopy
	components _ components copy! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:41:20'!
at: key
	^ components at: key ifAbsent: [self scalars zero]! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:42:55'!
at: key ifAbsent: exceptionBlock
	^ components at: key ifAbsent: exceptionBlock! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 06:10:41'!
at: anObject ifPresent: aBlock
	^ components at: anObject ifPresent: aBlock! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:43:24'!
at: key put: value
	value isZero ifTrue: [components removeKey: key ifAbsent: []. ^ value].
	^ components at: key put: value! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 05:43:40'!
bitSize
	| answer |
	answer _ 0.
	components keysAndValuesDo: [:key :value| answer _ answer + key bitSize + value bitSize].
	^ answer! !

!SparseTuple methodsFor: 'accessing' stamp: 'len 7/18/2020 07:00:41'!
size
	^ components size! !

!SparseTuple methodsFor: 'comparing' stamp: 'len 7/18/2020 06:08:29'!
= anObject
	^ self class = anObject class and: [components = anObject components]! !

!SparseTuple methodsFor: 'comparing' stamp: 'len 7/18/2020 06:08:44'!
hash
	^ components hash! !

!SparseTuple methodsFor: 'enumerating' stamp: 'len 7/18/2020 07:02:56'!
collect: aBlock 
	| newComponents |
	newComponents _ Dictionary new: self size.
	components associationsDo: [:each| | newValue |
		(newValue _ aBlock value: each value) isZero ifFalse: [newComponents at: each key put: newValue]].
	^ self class components: newComponents parent: parent! !

!SparseTuple methodsFor: 'enumerating' stamp: 'len 7/18/2020 06:57:14'!
withIndexDo: elementAndIndexBlock 
	components keysAndValuesDo: [:key :value| elementAndIndexBlock value: value value: key]! !

!SparseTuple methodsFor: 'initialization' stamp: 'len 7/18/2020 07:04:57'!
components: aDictionary
	components _ aDictionary! !

!SparseTuple methodsFor: 'initialization' stamp: 'len 7/18/2020 05:37:44'!
initialize
	super initialize.
	components _ Dictionary new! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 06:58:12'!
+ aTuple
	| other answer |
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #+].
	self size <= aTuple size
		ifTrue: [answer _ aTuple copy. other _ self]
		ifFalse: [answer _ self copy. other _ aTuple].
	other withIndexDo: [:each :index| answer at: index add: each].
	^ answer! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 06:58:49'!
- aTuple
	| answer |
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #-].
	self size <= aTuple size
		ifTrue:
			[answer _ aTuple negated.
			self withIndexDo: [:each :index| answer at: index add: each]]
		ifFalse:
			[answer _ self copy.
			aTuple withIndexDo: [:each :index| answer at: index add: each negated]].
	^ answer! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 06:59:44'!
· aTuple
	| answer |
	answer _ self scalars zero.
	self withIndexDo: [:each :index|
		aTuple at: index ifPresent: [:other| answer _ each*other + answer]].
	^ answer! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/18/2020 07:00:07'!
negated
	^ self copy components associationsDo: [:each| each value: each value negated]! !

!SparseTuple methodsFor: 'operations' stamp: 'len 7/27/2020 15:48:16'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ components size! !

!SparseTuple methodsFor: 'private' stamp: 'len 7/18/2020 05:41:14'!
at: key add: value
	^ components at: key addAndRemoveIfZero: value! !

!SparseTuple methodsFor: 'private' stamp: 'len 7/18/2020 06:04:59'!
components
	^ components! !

!SparseTuple class methodsFor: 'instance creation' stamp: 'len 7/18/2020 07:03:57'!
components: anArrayOrDictionary parent: aFreeModule
	| answer |
	anArrayOrDictionary class = Dictionary
		ifTrue: [^ self basicNew components: anArrayOrDictionary; parent: aFreeModule].
	answer _ self new parent: aFreeModule.
	anArrayOrDictionary withIndexDo: [:each :i| each isZero ifFalse: [answer at: i put: each]].
	^ answer! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:50'!
countSignChanges: signs
	| x count |
	x _ signs first.
	count _ 0.
	signs do: [ :each | each sign ~= x ifTrue: [ count _ count + 1 ]. x _ each sign ].
	^ count! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:44'!
numberOfRealRoots
	^ self numberOfSignChangesAtMinusInfinity - self numberOfSignChangesAtInfinity! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:51'!
numberOfSignChangesAt: aNumber
	^ self countSignChanges: (chain collect: [ :each | (each value: aNumber) sign])! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:51'!
numberOfSignChangesAtInfinity
	^ self countSignChanges: (chain collect: [ :each | each leadingCoefficient sign ])! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:51'!
numberOfSignChangesAtMinusInfinity
	^ self countSignChanges: (chain collect: [ :each | each leadingCoefficient sign * (each degree odd ifTrue: [-1] ifFalse: [1])])! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 20:45'!
polynomial
	^ chain first! !

!SturmChain methodsFor: 'as yet unclassified' stamp: 'len 1/15/2016 06:27'!
polynomial: aPolynomial
	chain _ OrderedCollection with: aPolynomial with: aPolynomial derivative.
	[ chain last degree > 0 ] whileTrue: [ chain add: ((chain at: chain size - 1) \\ chain last) negated ]! !

!SturmChain class methodsFor: 'as yet unclassified' stamp: 'len 11/3/2015 06:46'!
on: aPolynomial
	^ self basicNew setPolynomial: aPolynomial! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/1/98 17:35'!
add: aSubgroup
	^ self subgroups add: aSubgroup! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/1/98 17:34'!
at: anInteger
	^ self subgroups at: anInteger! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 2/8/2016 03:55'!
objects
	^ subgroups! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/4/2016 06:25'!
size
	^ subgroups size! !

!SubgroupSeries methodsFor: 'accessing' stamp: 'len 5/1/98 17:33'!
subgroups
	^ subgroups! !

!SubgroupSeries methodsFor: 'enumerating' stamp: 'len 5/1/98 17:35'!
do: aBlock
	self subgroups do: aBlock! !

!SubgroupSeries methodsFor: 'initialization' stamp: 'len 5/1/98 17:32'!
initialize
	self subgroups: OrderedCollection new! !

!SubgroupSeries methodsFor: 'testing' stamp: 'len 5/1/98 17:52'!
isAbelian
	"Answer true if the receiver is an abelian tower of subgroups."
	self isNormal ifFalse: [^ false].
	2 to: self size do: [ :each | ((self at: each - 1) / (self at: each)) isAbelian ifFalse: [^ false]].
	^ true! !

!SubgroupSeries methodsFor: 'testing' stamp: 'len 5/3/98 01:07'!
isCyclic
	"Answer true if the receiver is a cyclic tower of subgroups."
	self isNormal ifFalse: [^ false].
	2 to: self size do: [ :each | ((self at: each - 1) / (self at: each)) isCyclic ifFalse: [^ false]].
	^ true! !

!SubgroupSeries methodsFor: 'testing' stamp: 'len 5/1/98 17:47'!
isNormal
	"Answer true if the receiver is a normal tower of subgroups."
	2 to: self size do: [ :each | ((self at: each) isNormalIn: (self at: each - 1)) ifFalse: [^ false]].
	^ true! !

!SubgroupSeries methodsFor: 'private' stamp: 'len 5/1/98 17:33'!
subgroups: aCollection
	subgroups _ aCollection! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:11'!
gramMatrix
	^ matrix collect: [:x| -2 * (Float pi / x) cos]! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:14'!
isConnected
	^ self notYetImplemented! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:14'!
isElliptic
	^ self notYetImplemented! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:41'!
isHyperbolic
	^ (self gramMatrix spectrum count: [:each| each negative]) = 1! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:22'!
isLannerDiagram
	^ self isConnected and: [self isElliptic not and: [self gramMatrix determinant < 0]]! !

!CoxeterDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 22:23'!
isParabolic
	| spectrum |
	spectrum _ self gramMatrix spectrum.
	^ (spectrum occurrencesOf: 0) = 1 and: [spectrum allSatisfy: [:each| each >= 0]]! !

!RootSystem methodsFor: 'printing' stamp: 'len 2/11/2016 01:34'!
printOn: aStream
	aStream nextPut: ${.
	roots do: [:each| aStream print: each] separatedBy: [aStream nextPut: $,].
	aStream nextPut: $}! !

!RootSystem methodsFor: 'accessing' stamp: 'len 5/23/2016 05:00'!
ambient
	"Answer the ambient space of the receiver."
	^ ambient! !

!RootSystem methodsFor: 'accessing' stamp: 'len 5/23/2016 04:59'!
rank
	^ ambient dimension! !

!RootSystem methodsFor: 'accessing' stamp: 'len 1/6/2016 04:46'!
roots
	^ roots! !

!RootSystem methodsFor: 'accessing-private' stamp: 'len 5/23/2016 05:00'!
ambient: aVectorSpace
	ambient _ aVectorSpace! !

!RootSystem methodsFor: 'accessing-private' stamp: 'len 2/11/2016 19:21'!
roots: aCollection
	roots _ aCollection! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/5/2019 18:33:56'!
coxeterElement
	| O answer |
	O _ ambient automorphisms orthogonal.
	answer _ ambient id.
	self simpleRoots do: [:each| answer _ answer · (O reflectionAt: each)].
	^ answer! !

!RootSystem methodsFor: 'operations' stamp: 'len 11/23/2022 21:40:16'!
coxeterMatrix
	"Answer the Coxeter matrix of the receiver.
	Given a basis a_i of simple roots, the Coxeter matrix m_ij is defined by <a_i,a_j> = -2 cos(pi / m_ij)."
	| S |
	S _ self normalized simpleRoots.
	^ ambient scalars
		matrix: S size
		fill: [:i :j| Float pi / ((ambient innerProduct value: {(S at: i). (S at: j)}) / -2) arcCos]! !

!RootSystem methodsFor: 'operations' stamp: 'len 6/18/2016 17:14'!
coxeterNumber
	^ self weylGroup orderOf: self coxeterElement! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/23/2016 04:59'!
dual
	^ self class on: ambient roots: self coroots! !

!RootSystem methodsFor: 'operations' stamp: 'len 2/11/2016 20:59'!
exponents
	"Answer the 'sequence of exponents' of the root system."
	| h |
	self flag: #fix. "complex eigenvalues, but how about finite fields?"
	h _ self coxeterNumber.
	^ self coxeterElement eigenvalues collect: [:each| each log / (Float pi * 2 i / h)]! !

!RootSystem methodsFor: 'operations' stamp: 'len 8/6/2022 07:13:23'!
lattice
	"Answer the root lattice, the ℤ-submodule spanned by roots of the receiver."
	^ Lattice basis: self simpleRoots! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/23/2016 05:01'!
normalized
	^ self class on: ambient roots: (roots collect: [:x| x * (2 / (ambient innerProduct value: {x.x}))])! !

!RootSystem methodsFor: 'operations' stamp: 'len 2/10/2016 23:01'!
positiveWeylChamber
	"Answer the positive Weyl chamber that is choosen in the receiver to define the positive roots."
	^ self notYetImplemented! !

!RootSystem methodsFor: 'operations' stamp: 'len 5/20/2018 15:59:29'!
weightLattice
	"Answer the lattice spanned by the weights of the receiver. This lattice is invariant under the Weyl group and also under the map v -> -v. Also, the root lattice is contained in the weight lattice."
	^ Lattice basis: self weights! !

!RootSystem methodsFor: 'operations' stamp: 'len 1/7/2016 05:42'!
weights
	"Answer the fundamental weights of the receiver.
	A vector v is integral iff it can be expressed as integer linear combination of the fundamental weights."
	^ self notYetImplemented! !

!RootSystem methodsFor: 'operations' stamp: 'len 9/16/2022 08:17:49'!
weylAction
	| W |
	W _ self weylGroup.
	W name: 'W'.
	^ GroupAction from: (W, self roots) to: self roots mapWithArguments: [:f :x| f value: x]! !

!RootSystem methodsFor: 'operations' stamp: 'len 9/27/2018 21:19:17'!
weylGroup
	"Answer the Weyl group of the root system, i.e. the (finite) group of isometries of the ambient vector space generated by reflections through hyperplanes perpendicular to the roots. The Weyl group W(R) is the subgroup of GL(V) (and more specifically O(V)) generated by the reflections {s_r} that act on each root r by -1: s_r(r) = -r."
	| O W |
	O _ ambient automorphisms orthogonal.
	W _ O span: (self roots collect: [:each| O reflectionAt: each]).
"	W name: 'W'."
	^ W! !

!RootSystem methodsFor: 'operations' stamp: 'len 1/6/2016 00:00'!
weylVector
	^ self positiveRoots sum / 2! !

!RootSystem methodsFor: 'roots' stamp: 'len 5/23/2016 05:00'!
coroots
	^ roots collect: [:v| v * 2 / (ambient innerProduct value: {v.v})]! !

!RootSystem methodsFor: 'roots' stamp: 'len 1/6/2016 05:01'!
degreeOf: aPositiveRoot
	"Answer the degree of the given positive root."
	^ (self simpleRoots coordinatesOf: aPositiveRoot) asArray sum! !

!RootSystem methodsFor: 'roots' stamp: 'len 1/6/2016 03:52'!
negativeRoots
	^ self positiveRoots negated! !

!RootSystem methodsFor: 'roots' stamp: 'len 6/25/2016 23:08'!
positiveRoots
	| answer |
	answer _ Set new.
	roots do: [:a| (answer includes: a negated)
		ifFalse:
			[answer add: a.
			answer copy do: [:b| (roots includes: a+b) ifTrue: [answer add: b]]]].
	^ answer! !

!RootSystem methodsFor: 'roots' stamp: 'len 1/6/2016 04:51'!
positiveRootsSorted
	^ self positiveRoots asSortedCollection: [:a :b| (self simpleRoots coordinatesOf: b - a) asArray allSatisfy: [:each| each positive]]! !

!RootSystem methodsFor: 'roots' stamp: 'len 3/6/2020 18:19:43'!
simpleRoots
	| M |
	M _ Matrix columns: (self positiveRoots asArray collect: [:each| ambient coordinatesOf: each]).
	^ M image! !

!RootSystem methodsFor: 'testing' stamp: 'len 5/23/2016 04:59'!
isCrystallographic
	^ self roots allSatisfy: [:x|
		self coroots allSatisfy: [:y|
			(ambient innerProduct value: {x. y}) isInteger]]! !

!RootSystem methodsFor: 'testing' stamp: 'len 2/11/2016 21:16'!
isIntegral
	^ self isCrystallographic! !

!RootSystem methodsFor: 'testing' stamp: 'len 2/11/2016 20:46'!
isIrreducible
	^ self coxeterDiagram isConnected! !

!RootSystem methodsFor: 'testing' stamp: 'len 5/23/2016 05:01'!
isNormalized
	^ roots allSatisfy: [:each| (ambient innerProduct value: {each.each}) = 2]! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
A: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
B: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		roots add: (V e: i).
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: i) + (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
C: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		roots add: (V e: i)*2.
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: i) + (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 7/14/2016 00:09'!
D: V
	| roots |
	roots _ Set new.
	1 to: V dimension do: [:i|
		i to: V dimension do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: i) + (V e: j)]].
	^ self on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example1
	"This is a normalized root system in R^n. The Weyl group associated to it is the symmetric group S_n."
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: V dimension do: [:i|
		1 to: i-1 do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: j) - (V e: i)]].
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 12/9/2022 21:35:13'!
example2
	"This is a normalized root system in R^n. The Weyl group associated to it is the hyperoctahedral group."
	
	self notYetImplemented
"	| V roots |
	V _ ℝ ^ 3.
	roots _ Set new.
	1 to: V dimension do: [:i|
		1 to: i-1 do: [:j|
			roots
				add: (V e: i) * (RealAlgebraicNumber sqrt: 2);
				add: (V e: i) * (RealAlgebraicNumber sqrt: 2) negated;
				add: (V e: i) - (V e: j);
				add: (V e: j) - (V e: i);
				add: (V e: i) + (V e: j);
				add: ((V e: i) + (V e: j)) negated]].
	^ RootSystem on: V roots: roots"! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example3
	"This is a normalized root system in R^n. The Weyl group associated to it is an index 2 subgroup of the hyperoctahedral group."
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i|
		1 to: i-1 do: [:j|
			roots
				add: (V e: i) - (V e: j);
				add: (V e: j) - (V e: i);
				add: (V e: i) + (V e: j);
				add: ((V e: i) + (V e: j)) negated]].
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
hexagonal
	^ self A: ℚ ^ 2! !

!RootSystem class methodsFor: 'instance creation' stamp: 'len 5/23/2016 05:00'!
on: aVectorSpace roots: aCollection
	^ self new ambient: aVectorSpace; roots: aCollection! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem1
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i| 1 to: i-1 do: [:j| |r| roots add: (r _ (V x: i) - (V x: j)); add: r negated]].
	^ RootSystem on: V roots: roots! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem2
	| V roots |
	V _ ℚ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i|
		1 to: i-1 do: [:j|
			roots
				add: (V x: i) - (V x: j);
				add: (V x: j) - (V x: i);
				add: (V x: i) + (V x: j);
				add: ((V x: i) + (V x: j)) negated]].
	^ RootSystem on: V roots: roots! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/23/2016 07:26'!
testBaseSignCondition
	| R |
	R _ self rootSystem1.
	R roots do: [:each| | coordinates |
		coordinates _ R simpleRoots coordinatesOf: each :: asArray.
		self assert: (coordinates allSatisfy: [:xi| xi >= 0]) | (coordinates allSatisfy: [:xi| xi <= 0])]! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 6/25/2016 23:09'!
testPositiveRoots
	| V R |
	R _ self rootSystem1.
	V _ R ambient.
	R positiveRoots do: [:each|
		self assert: (V innerProduct value: {each. (1,1,1)}) > 0] "the point must be in the positive Weyl chamber"! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/11/2016 01:39'!
testWeylGroup
	| R W |
	R _ self rootSystem1.
	W _ R weylGroup.
	"The Weyl group of this root system is isomorphic to Sym(3), so we test some properties of Sym(3):"
	self assert: W order = 6.
	self assert: W center isTrivial.
	self assert: W isSolvable.
	self assert: W isCyclic not! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/10/2016 22:43'!
testWeylGroup2
	| R W |
	R _ self rootSystem2.
	W _ R weylGroup. "this is an index 2 subgroup of the hyperoctahedral group"
	self assert: W order = 24! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 2/10/2016 22:56'!
testWeylGroupAndSimpleRoots
	"For every root, there exists an transformation in the Weyl group that makes it a simple root."
	| R W |
	R _ self rootSystem1.
	W _ R weylGroup.
	R roots do: [:r| self assert: (W anySatisfy: [:w| R simpleRoots includes: (w value: r)])]! !

!PolynomialCode methodsFor: 'accessing' stamp: 'len 4/4/2016 20:19'!
generator
	^ generator! !

!PolynomialCode methodsFor: 'accessing' stamp: 'len 5/12/2016 07:20'!
rank
	^ self length - generator degree! !

!PolynomialCode methodsFor: 'accessing' stamp: 'len 1/23/2017 11:44:09'!
size
	^ generator parent scalars size ^ (self length - generator degree)! !

!PolynomialCode methodsFor: 'converting' stamp: 'len 7/31/2020 10:28:26'!
asIdeal
	^ generator parent * generator! !

!PolynomialCode methodsFor: 'encoding/decoding' stamp: 'len 5/12/2016 09:33'!
decode: aPolynomial
	self flag: #fix. "add error-detection and correction"
	^ aPolynomial // generator! !

!PolynomialCode methodsFor: 'encoding/decoding' stamp: 'len 7/15/2021 12:38:39'!
encode: aPolynomial
	| f |
	f _ aPolynomial << generator degree.
	^ f - (f \\ generator)! !

!PolynomialCode methodsFor: 'enumerating' stamp: 'len 7/31/2020 10:31:23'!
do: aBlock
	self asIdeal do: aBlock! !

!PolynomialCode methodsFor: 'initialization' stamp: 'len 4/4/2016 20:19'!
generator: aPolynomial
	generator _ aPolynomial! !

!PolynomialCode methodsFor: 'testing' stamp: 'len 5/12/2016 07:17'!
includes: aCodeword
	^ aCodeword | generator! !

!PolynomialCode methodsFor: 'testing' stamp: 'len 1/23/2017 11:44:04'!
isCyclic
	| n x |
	n _ self length.
	x _ generator parent x.
	^ generator | (x ^ n - x parent one)! !

!PolynomialCode class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
G23
	"Answer the perfect binary Golay code [23,12,7]."
	| g |
	g _ (ℤ/2) polynomials !! [:x| x^11 + (x^10) + (x^6) + (x^5) + (x^4) + (x^2) + 1].
	^ self new: 23 generator: g! !

!PolynomialCode class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
H7
	"Answer the Hamming code [7,4,3]."
	^ self new: 7 generator: (ℤ/2) polynomials !! [:x| x^3 + x + 1]! !

!PolynomialCode class methodsFor: 'instance creation' stamp: 'len 5/12/2016 07:42'!
generator: aPolynomial
	^ self new: aPolynomial degree generator: aPolynomial! !

!PolynomialCode class methodsFor: 'instance creation' stamp: 'len 8/2/2020 12:23:40'!
new: n generator: aPolynomial
	| V W subspace |
	V _ aPolynomial parent filtrationAt: n - 1.
	W _ aPolynomial parent filtrationAt: n - aPolynomial degree - 1.
	subspace _ V span: (W asArray collect: [:f| aPolynomial * f]).
	^ (self on: subspace) generator: aPolynomial! !

!CyclicCode methodsFor: 'accessing' stamp: 'len 1/23/2017 11:31:24'!
checkPolynomial
	| x one |
	x _ generator parent x.
	one _ generator parent one.
	^ x ^ self length - one / generator! !

!CyclicCode methodsFor: 'accessing' stamp: 'len 7/31/2020 10:30:03'!
dimension
	^ self length - generator degree! !

!CyclicCode methodsFor: 'operations' stamp: 'len 6/29/2016 22:18'!
dual
	^ self class new: self length generator: self generator reversed! !

!CyclicCode methodsFor: 'testing' stamp: 'len 5/15/2019 01:42:35'!
includes: aCodeword
	| x |
	x _ generator parent x.
	^ (self checkPolynomial * aCodeword \\ (x ^ self length - x one)) isZero! !

!CyclicCode methodsFor: 'testing' stamp: 'len 6/29/2016 22:18'!
isIrreducible
	^ generator isIrreducible! !
BitTuple2 initialize!
