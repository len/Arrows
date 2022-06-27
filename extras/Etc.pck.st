'From Cuis 6.0 [latest update: #5053] on 27 June 2022 at 1:31:30 pm'!
'Description '!
!provides: 'Etc' 1 10!
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

!classDefinition: #CompletionRing category: #Etc!
Ring subclass: #CompletionRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'CompletionRing class' category: #Etc!
CompletionRing class
	instanceVariableNames: ''!

!classDefinition: #PolynomialCode category: #Etc!
Code subclass: #PolynomialCode
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

!classDefinition: #CoxeterSystem category: #'Etc-Lie'!
Structure subclass: #CoxeterSystem
	instanceVariableNames: 'group generators matrix diagram'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc-Lie'!
!classDefinition: 'CoxeterSystem class' category: #'Etc-Lie'!
CoxeterSystem class
	instanceVariableNames: ''!

!classDefinition: #ColumnMatrix category: #Etc!
Matrix subclass: #ColumnMatrix
	instanceVariableNames: 'columns'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ColumnMatrix class' category: #Etc!
ColumnMatrix class
	instanceVariableNames: ''!

!classDefinition: #RowMatrix category: #Etc!
Matrix subclass: #RowMatrix
	instanceVariableNames: 'rows'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'RowMatrix class' category: #Etc!
RowMatrix class
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

!classDefinition: #Divisor category: #Etc!
Sum subclass: #Divisor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Divisor class' category: #Etc!
Divisor class
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

!classDefinition: #BitTuple category: #Etc!
Object subclass: #BitTuple
	instanceVariableNames: 'bits'
	classVariableNames: 'One Zero'
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'BitTuple class' category: #Etc!
BitTuple class
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

!classDefinition: #HermiteColumnReduction category: #Etc!
Object subclass: #HermiteColumnReduction
	instanceVariableNames: 'matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'HermiteColumnReduction class' category: #Etc!
HermiteColumnReduction class
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

!PolynomialCode commentStamp: '<historical>' prior: 0!
In coding theory, a polynomial code is a type of linear code whose set of valid codewords consists of those polynomials (usually of some fixed length) that are divisible by some fixed polynomial (of shorter length, called the generator polynomial).

A polynomial code is cyclic if and only if the generator polynomial divides x^n - 1. See CyclicCode.
!

!ColumnMatrix commentStamp: '<historical>' prior: 0!
Matrices stored as a list of column tuples. Column operations are fast on these matrices.!

!RowMatrix commentStamp: '<historical>' prior: 0!
Matrices stored as a list of row tuples. Row operations are fast on these matrices.!

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

!Divisor commentStamp: '<historical>' prior: 0!
Weil divisors, i.e. formal sums (with integer coefficients) of prime divisors (integral closed subschemes of codimension 1) of an integral locally Noetherian scheme.!

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

!BitTuple commentStamp: '<historical>' prior: 0!
Tuples of integer residue classes modulo 2, i.e. binary tuples, bit strings, or tuples of bits. The coefficient field is ƒ/2ƒ.

Example:
	"A handy way to create a binary tuple:"
	(1,0,1,1,0,1,1,1) % 2
!

!HermiteColumnReduction commentStamp: '<historical>' prior: 0!
This is the Hermite column reduction algorithm. Given a matrix with coefficients in the rational integers Z, it reduces the matrix to its column-style Hermite normal form.

See [Coh96], Algorithm 2.4.4.

See HermiteReduction for the algorithm modified to do row reduction instead of column reduction, and generalized to work over any computable Euclidean domain.!

!SmithReduction commentStamp: '<historical>' prior: 0!
This is the Smith reduction algorithm. Given a matrix with coefficients in a PID, it reduces the matrix to its Smith normal form.

The Smith normal form is very useful for working with finitely generated modules over a PID, and in particular for deducing the structure of a quotient of a free module.

See [Coh96], Algorithm 2.4.14.!

!SparseTuple commentStamp: 'len 7/21/2020 17:34:32' prior: 0!
Sparse tuples are tuples that use a Dictionary to store the components, and zero entries are not stored explicitly. This makes them more efficient when most of the entries are zero.!

!BitArray2 methodsFor: 'printing' stamp: 'len 12/21/2015 20:42'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!StandardBasis methodsFor: 'printing' stamp: 'len 5/7/2018 23:22:28'!
printOn: aStream
	aStream nextPut: ${.
	elements do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $}! !

!SemidirectProductGroup methodsFor: 'printing' stamp: 'len 4/21/2016 00:16'!
printOn: aStream
	aStream print: left; nextPutAll: ' X| '; print: right! !

!SemidirectProductElement methodsFor: 'printing' stamp: 'len 2/12/2016 06:10'!
printOn: aStream
	aStream nextPut: $(; print: left; nextPutAll: ', '; print: right; nextPut: $)! !

!RelaxedAdicInteger methodsFor: 'accessing' stamp: 'len 6/3/2019 17:28:33'!
>> anInteger
	"Multiply the receiver by p^anInteger shifting the digits by anInteger places."
	^ parent digits: digits >> anInteger! !

!RelaxedAdicInteger methodsFor: 'printing' stamp: 'len 6/2/2019 10:09:25'!
printOn: aStream
	self printOn: aStream withPowersShiftedBy: 0! !

!RelaxedAdicNumber methodsFor: 'printing' stamp: 'len 6/3/2019 18:28:16'!
printOn: aStream
	unit printOn: aStream withPowersShiftedBy: valuation negated! !

!RelaxedPowerSeries methodsFor: 'printing' stamp: 'len 6/6/2019 13:22:39'!
printOn: aStream
	| P f n |
	n _ coefficients printLimit.
	P _ parent base.
	f _ (P coefficients: (coefficients first: n)) orderedBy: #llex.
	aStream print: f; nextPutAll: ' + ', 'O' italic, '('; print: P x ^ (n+1); nextPut: $)! !

!Sum methodsFor: 'copying' stamp: 'len 11/18/2016 17:34:33'!
postCopy
	coefficients _ coefficients copy! !

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

!BitTuple methodsFor: 'operations' stamp: 'len 7/15/2021 12:42:04'!
>> anInteger
	^ self class bits: (bits bitShift: anInteger) parent: parent! !

!BitTuple methodsFor: 'printing' stamp: 'len 3/28/2016 04:52'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!SparseTuple methodsFor: 'copying' stamp: 'len 7/18/2020 06:12:12'!
postCopy
	components _ components copy! !

!RootSystem methodsFor: 'printing' stamp: 'len 2/11/2016 01:34'!
printOn: aStream
	aStream nextPut: ${.
	roots do: [:each| aStream print: each] separatedBy: [aStream nextPut: $,].
	aStream nextPut: $}! !

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

!StandardBasis methodsFor: 'operations' stamp: 'len 1/1/2022 09:37:58'!
macaulayMatrix
	| monomials |
	monomials _ Set new.
	self do: [:each| monomials addAll: each monomials].
	monomials _ monomials asSortedCollection: [:a :b| a >= b].
	^ self anyOne parent scalars matrix: self size @ monomials size filling: [:i :j| (self at: i) at: (monomials at: j)]! !

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

!StandardBasis methodsFor: 'private' stamp: 'len 4/28/2022 13:00:32'!
validate
	1 to: self size do: [:i| | fi |
		fi _ self at: i.
		i+1 to: self size do: [:j| | fj |
			fj _ self at: j.
			(self reduces: (fi s: fj))
				ifFalse: [^ false]]].
	^ true! !

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
	F _ ƒ/p.
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

!SemidirectProductGroup methodsFor: 'morphisms' stamp: 'len 5/23/2020 07:33:35'!
projection
	"Answer the projection from the cartesian product of two groups to their semidirect product."
	^ ((left, right) to: self evaluating: [:each| self project: each]) name: 'Ž'! !

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

!CompletionRing methodsFor: 'morphisms' stamp: 'len 12/22/2021 15:04:23'!
from: aDomain
	aDomain = self base ifTrue: [^ self addMap: ((self base into: self evaluating: [:x| self embed: x]) name: 'i')].
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
	g _ (ƒ/2) polynomials !! [:x| x^11 + (x^10) + (x^6) + (x^5) + (x^4) + (x^2) + 1].
	^ self new: 23 generator: g! !

!PolynomialCode class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
H7
	"Answer the Hamming code [7,4,3]."
	^ self new: 7 generator: (ƒ/2) polynomials !! [:x| x^3 + x + 1]! !

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

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 12/20/2021 13:24:06'!
atRow: i column: j
	^ (columns at: j) at: i! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 12/20/2021 13:24:26'!
atRow: i column: j ifAbsent: exceptionBlock
	^ (columns at: j ifAbsent: [^ exceptionBlock value]) at: i ifAbsent: exceptionBlock! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 12/20/2021 13:24:40'!
atRow: i column: j put: anObject
	^ (columns at: j) at: i put: anObject! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 02:53'!
columnAt: j
	^ columns at: j! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 02:54'!
columnAt: i put: aTuple
	^ columns at: i put: aTuple! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 02:55'!
columns
	^ columns! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 02:56'!
columns: anArray
	columns _ anArray! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 4/23/2018 21:09:52'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars integers one.
	columns do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 5/29/2020 05:47:14'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ self class from: domain lift to: codomain lift columns: (columns collect: [:each| each lift])! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 5/29/2020 05:48:17'!
representative
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ self class from: domain lift to: codomain lift columns: (columns collect: [:each| each representative])! !

!ColumnMatrix methodsFor: 'accessing' stamp: 'len 5/29/2020 16:35:32'!
rowAt: i
	^ domain filling: [:j| (columns at: j) at: i]! !

!ColumnMatrix methodsFor: 'converting' stamp: 'len 7/30/2021 14:27:52'!
asTuple
	"Answer the receiver converted to Tuple by concatenating its rows (which is also the coordinates of the receiver in the canonical basis)."
	^ columns size = 1 ifTrue: [columns first] ifFalse: [super asTuple]! !

!ColumnMatrix methodsFor: 'converting' stamp: 'len 5/29/2020 08:52:58'!
over: aRing
	| newCodomain |
	self scalars = aRing ifTrue: [^ self].
	^ self class from: (self domain over: aRing) to: (newCodomain _ self codomain over: aRing) columns: (columns collect: [:each| newCodomain !! each])! !

!ColumnMatrix methodsFor: 'copying' stamp: 'len 5/29/2020 05:46:48'!
copy
	^ self class from: domain to: codomain columns: (columns collect: [:each| each copy])! !

!ColumnMatrix methodsFor: 'enumerating' stamp: 'len 3/24/2018 19:28:43'!
allSatisfy: aBlock
	^ columns allSatisfy: [:each| each allSatisfy: aBlock]! !

!ColumnMatrix methodsFor: 'enumerating' stamp: 'len 5/29/2020 05:46:38'!
collect: aBlock
	^ self class from: domain to: codomain columns: (columns collect: [:each| each collect: aBlock])! !

!ColumnMatrix methodsFor: 'enumerating' stamp: 'len 3/23/2016 02:57'!
columnsDo: aBlock
	columns do: aBlock! !

!ColumnMatrix methodsFor: 'enumerating' stamp: 'len 7/14/2019 15:20:19'!
do: aBlock
	columns do: [:each| each do: aBlock]! !

!ColumnMatrix methodsFor: 'enumerating' stamp: 'len 7/16/2021 22:24:56'!
withIndicesDo: aBlock
	columns withIndexDo: [:column :j| column withIndexDo: [:each :i| aBlock value: each value: i value: j]]! !

!ColumnMatrix methodsFor: 'operations' stamp: 'len 6/9/2022 07:14:12'!
+ aMatrix
	self checkShapesMatch: aMatrix.
	self checkScalarsMatch: aMatrix.
	^ self class from: domain to: codomain columns: columns + aMatrix columns! !

!ColumnMatrix methodsFor: 'operations' stamp: 'len 6/9/2022 07:14:21'!
- aMatrix
	self checkShapesMatch: aMatrix.
	self checkScalarsMatch: aMatrix.
	^ self class from: domain to: codomain columns: columns - aMatrix columns! !

!ColumnMatrix methodsFor: 'operations' stamp: 'len 5/29/2020 05:47:22'!
negated
	^ self class from: domain to: codomain columns: columns negated! !

!ColumnMatrix methodsFor: 'operations' stamp: 'len 8/9/2020 07:49:04'!
transpose
	^ Matrix from: codomain to: domain rows: columns! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 5/30/2019 07:11:23'!
addColumn: src times: aNumber toColumn: dst
	self addColumn: src times: aNumber toColumn: dst startingAt: 1! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 5/30/2019 07:11:05'!
addColumn: src times: aNumber toColumn: dst startingAt: columnIndex
	(columns at: dst) add: (columns at: src) times: aNumber startingAt: columnIndex! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 5/30/2019 07:18:45'!
addRow: i times: aNumber toRow: j
	columns do: [:each| each at: j put: (each at: i) * aNumber + (each at: j)]! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 1/16/2018 22:10:18'!
multiplyColumn: i by: aNumber
	self multiplyColumn: i by: aNumber startingAt: 1! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 1/16/2018 22:10:00'!
multiplyColumn: i by: aNumber startingAt: startIndex
	(columns at: i) multiplyBy: aNumber startingAt: startIndex! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 1/16/2018 22:10:27'!
multiplyRow: i by: aNumber
	columns do: [:each| each at: i put: (each at: i) * aNumber]! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 3/3/2017 12:11:47'!
swapColumn: i with: j
	columns swap: i with: j! !

!ColumnMatrix methodsFor: 'reduction-operations' stamp: 'len 3/3/2017 12:07:39'!
swapRow: i with: j
	columns do: [:each| each swap: i with: j]! !

!ColumnMatrix methodsFor: 'testing' stamp: 'len 3/23/2016 02:44'!
isZero
	^ columns allSatisfy: [:each| each isZero]! !

!ColumnMatrix methodsFor: 'testing' stamp: 'len 12/1/2016 17:05:02'!
isZeroAt: aPoint
	^ (columns at: aPoint y) isZeroAt: aPoint x! !

!ColumnMatrix methodsFor: 'private' stamp: 'len 5/29/2020 09:15:50'!
validate
	columns size = domain rank ifFalse: [^ false].
	^ super validate! !

!ColumnMatrix class methodsFor: 'instance creation' stamp: 'len 5/29/2020 05:45:24'!
from: aDomain to: aCodomain columns: anArray
	^ self new domain: aDomain; codomain: aCodomain; columns: anArray! !

!ColumnMatrix class methodsFor: 'instance creation' stamp: 'len 5/29/2020 05:48:59'!
newFrom: aMatrix
	^ self from: aMatrix domain to: aMatrix codomain columns: (aMatrix columns collect: [:each| each copy])! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 12/20/2021 13:28:22'!
atRow: i column: j
	^ (rows at: i) at: j! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 12/20/2021 13:28:38'!
atRow: i column: j ifAbsent: exceptionBlock
	^ (rows at: i ifAbsent: [^ exceptionBlock value]) at: j ifAbsent: exceptionBlock! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 12/20/2021 13:28:50'!
atRow: i column: j put: anObject
	^ (rows at: i) at: j put: anObject! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 5/29/2020 16:36:51'!
columnAt: j
	^ codomain filling: [:i| (rows at: i) at: j]! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 4/23/2018 21:09:39'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars integers one.
	rows do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 5/29/2020 09:02:16'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ self class from: domain lift to: codomain lift rows: (rows collect: [:each| each lift])! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 5/29/2020 09:02:30'!
representative
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ self class from: domain lift to: codomain lift rows: (rows collect: [:each| each representative])! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 00:16'!
rowAt: i
	^ rows at: i! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 00:18'!
rowAt: i put: aTuple
	^ rows at: i put: aTuple! !

!RowMatrix methodsFor: 'accessing' stamp: 'len 3/23/2016 00:17'!
rows
	^ rows! !

!RowMatrix methodsFor: 'converting' stamp: 'len 7/30/2021 14:28:43'!
asTuple
	"Answer the receiver converted to Tuple by concatenating its rows (which is also the coordinates of the receiver in the canonical basis)."
	^ rows size = 1 ifTrue: [rows first] ifFalse: [super asTuple]! !

!RowMatrix methodsFor: 'converting' stamp: 'len 5/29/2020 08:53:54'!
over: aRing
	| newDomain |
	self scalars = aRing ifTrue: [^ self].
	^ self class from: (newDomain _ self domain over: aRing) to: (self codomain over: aRing) rows: (rows collect: [:each| newDomain !! each])! !

!RowMatrix methodsFor: 'copying' stamp: 'len 5/29/2020 09:02:03'!
copy
	^ self class from: domain to: codomain rows: (rows collect: [:each| each copy])! !

!RowMatrix methodsFor: 'enumerating' stamp: 'len 3/24/2018 19:28:25'!
allSatisfy: aBlock
	^ rows allSatisfy: [:each| each allSatisfy: aBlock]! !

!RowMatrix methodsFor: 'enumerating' stamp: 'len 5/29/2020 09:01:43'!
collect: aBlock
	^ self class from: domain to: codomain rows: (rows collect: [:each| each collect: aBlock])! !

!RowMatrix methodsFor: 'enumerating' stamp: 'len 7/14/2019 15:20:53'!
do: aBlock
	rows do: [:each| each do: aBlock]! !

!RowMatrix methodsFor: 'enumerating' stamp: 'len 3/23/2016 00:18'!
rowsDo: aBlock
	rows do: aBlock! !

!RowMatrix methodsFor: 'enumerating' stamp: 'len 7/16/2021 22:24:07'!
withIndicesDo: aBlock
	rows withIndexDo: [:row :i| row withIndexDo: [:each :j| aBlock value: each value: i value: j]]! !

!RowMatrix methodsFor: 'initialization' stamp: 'len 3/23/2016 00:20'!
rows: anArray
	rows _ anArray! !

!RowMatrix methodsFor: 'operations' stamp: 'len 6/9/2022 07:14:29'!
+ aMatrix
	self checkShapesMatch: aMatrix.
	self checkScalarsMatch: aMatrix.
	^ self class from: domain to: codomain rows: rows + aMatrix rows! !

!RowMatrix methodsFor: 'operations' stamp: 'len 6/9/2022 07:14:37'!
- aMatrix
	self checkShapesMatch: aMatrix.
	self checkScalarsMatch: aMatrix.
	^ self class from: domain to: codomain rows: rows - aMatrix rows! !

!RowMatrix methodsFor: 'operations' stamp: 'len 5/29/2020 09:01:31'!
negated
	^ self class from: domain to: codomain rows: rows negated! !

!RowMatrix methodsFor: 'operations' stamp: 'len 8/9/2020 07:50:03'!
transpose
	^ Matrix from: codomain to: domain columns: rows! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 5/30/2019 07:22:05'!
addColumn: i times: aNumber toColumn: j
	rows do: [:each| each at: j put: (each at: i) * aNumber + (each at: j)]! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 5/30/2019 07:21:28'!
addRow: src times: aNumber toRow: dst
	self addRow: src times: aNumber toRow: dst startingAt: 1! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 5/30/2019 07:21:44'!
addRow: src times: aNumber toRow: dst startingAt: columnIndex
	(rows at: dst) add: (rows at: src) times: aNumber startingAt: columnIndex! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 1/16/2018 22:11:15'!
multiplyColumn: i by: aNumber
	rows do: [:each| each at: i put: (each at: i) * aNumber]! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 1/16/2018 22:08:39'!
multiplyRow: i by: aNumber
	self multiplyRow: i by: aNumber startingAt: 1! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 1/16/2018 22:08:30'!
multiplyRow: i by: aNumber startingAt: startIndex
	(rows at: i) multiplyBy: aNumber startingAt: startIndex! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 3/23/2016 00:20'!
swapColumn: i with: j
	rows do: [:each| each swap: i with: j]! !

!RowMatrix methodsFor: 'reduction-operations' stamp: 'len 3/23/2016 00:17'!
swapRow: i with: j
	rows swap: i with: j! !

!RowMatrix methodsFor: 'testing' stamp: 'len 3/23/2016 00:19'!
isZero
	^ rows allSatisfy: [:each| each isZero]! !

!RowMatrix methodsFor: 'testing' stamp: 'len 12/1/2016 17:04:49'!
isZeroAt: aPoint
	^ (rows at: aPoint x) isZeroAt: aPoint y! !

!RowMatrix methodsFor: 'private' stamp: 'len 5/29/2020 09:16:26'!
validate
	rows size = codomain rank ifFalse: [^ false].
	^ super validate! !

!RowMatrix class methodsFor: 'instance creation' stamp: 'len 5/29/2020 06:36:44'!
from: aDomain to: aCodomain rows: anArray
	^ self new domain: aDomain; codomain: aCodomain; rows: anArray! !

!RowMatrix class methodsFor: 'instance creation' stamp: 'len 5/29/2020 05:49:49'!
newFrom: aMatrix
	^ self from: aMatrix domain to: aMatrix codomain rows: (aMatrix rows collect: [:each| each copy])! !

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

!RelaxedPowerSeries methodsFor: 'accessing' stamp: 'len 9/22/2018 18:55:44'!
terms
	| P |
	P _ self scalars polynomials.
	^ Sequence to: P evaluating: [:i| P coefficient: (coefficients at: i) degree: i-1]! !

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

!RelaxedPowerSeries methodsFor: 'testing' stamp: 'len 4/11/2016 23:09'!
isZero
	^ coefficients isZero! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
cos
	"The power series expansion of cos(x)."
	^ „ powerSeries coefficients:
		(Sequence to: „ evaluating: [:n| n even ifTrue: [0] ifFalse: [|k| k _ n-1/2. (-1)^k / (2*k) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
exp
	"The power series expansion of exp(x)."
	^ „ powerSeries
		coefficients: (Sequence to: „ evaluating: [:n| 1 / n factorial])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
sin
	"The power series expansion of sin(x)."
	^ „ powerSeries
		coefficients: (Sequence to: „ evaluating: [:n| n odd ifTrue: [0] ifFalse: [|k| k _ n/2-1. (-1)^k / (2*k+1) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 12/27/2015 16:21'!
coefficients: aSequence
	^ self new coefficients: aSequence! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 11/25/2016 10:32:44'!
fromPolynomial: aPolynomial
	^ self coefficients: (CFiniteSequence to: aPolynomial scalars initialValues: aPolynomial coefficients periodic: {aPolynomial scalars zero})! !

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

!Sum methodsFor: 'accessing' stamp: 'len 4/23/2018 21:08:15'!
denominator
	| answer |
	self scalars isLocalization ifFalse: [^ self scalars one].
	answer _ self scalars integers one.
	self do: [:each| answer _ each denominator lcm: answer].
	^ answer! !

!Sum methodsFor: 'accessing' stamp: 'len 11/18/2016 20:33:21'!
keys
	^ coefficients keys! !

!Sum methodsFor: 'accessing' stamp: 'len 4/23/2018 21:08:38'!
numerator
	self scalars isLocalization ifFalse: [^ self].
	^ self * self denominator over: self scalars integers! !

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

!Divisor methodsFor: 'accessing' stamp: 'len 1/10/2018 21:59:43'!
degree
	| answer |
	answer _ 0.
	self coefficientsDo: [:each| answer _ answer + each].
	^ answer! !

!Divisor methodsFor: 'accessing' stamp: 'len 5/10/2019 04:36:00'!
support
	| answer |
	answer _ nil. "should start from the empty variety"
	self keysDo: [:each| answer _ answer ifNil: [each] ifNotNil: [answer \/ each]].
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

!Divisor methodsFor: 'operations' stamp: 'len 1/10/2018 21:58:23'!
gcd: aDivisor
	| newCoefficients |
	newCoefficients _ Dictionary new.
	self keysAndValuesDo: [:key :value|
		aDivisor at: key ifPresent: [:value2| newCoefficients at: key put: (value min: value2)]].
	^ (self class coefficients: newCoefficients) parent: parent! !

!Divisor methodsFor: 'operations' stamp: 'len 2/25/2017 09:09:10'!
lcm: aDivisor
	| answer |
	answer _ self copy.
	aDivisor keysAndValuesDo: [:key :value|
		answer at: key put: ((self at: key) max: value)].
	^ answer! !

!Divisor methodsFor: 'testing' stamp: 'len 2/25/2017 09:09:25'!
isEffective
	^ self allSatisfy: [:each| each positive]! !

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

!BitTuple methodsFor: 'operations' stamp: 'len 5/15/2019 23:57:46'!
* aBit
	aBit isMatrix ifTrue: [^ super * aBit].
	^ ((aBit isInteger and: [aBit even]) or: [aBit isZero]) ifTrue: [self zero] ifFalse: [self]! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:27:21'!
+ aBitTuple
	^ self class
		bits: (bits bitXor: aBitTuple asInteger)
		parent: (self size >= aBitTuple size ifTrue: [parent] ifFalse: [aBitTuple parent])! !

!BitTuple methodsFor: 'operations' stamp: 'len 3/28/2016 06:51'!
- aBitTuple
	^ self + aBitTuple! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:30:57'!
>>> anInteger
	anInteger = 0 ifTrue: [^ self].
	^ self class bits: (bits bitShift: anInteger) parent: self scalars^(self size + anInteger max: 0)! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/8/2019 03:09:41'!
· aBitTuple
	^ (bits bitAnd: aBitTuple asInteger) bitParity = 0 ifTrue: [Zero] ifFalse: [One]

"	^ self hadamard: aBitTuple :: sum
"

"	| x |
	x _ self asInteger bitAnd: aBitTuple asInteger.
	[x > 1]
		whileTrue: [x _ x >> (x highBit // 2) bitAnd: x].
	^ x"! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/29/2020 07:47:48'!
convolution: aBitTuple
	| a b c |
	a _ bits.
	b _ aBitTuple asInteger.
	c _ 0.
	1 to: a highBit do: [:i|
		(a bitAt: i) = 1 ifTrue: [c _ c bitXor: b].
		b _ b bitShift: 1].
	^ self class bits: c parent: self scalars ^ (self size + aBitTuple size - 1 max: 0)! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:34:18'!
hadamard: aBitTuple
	"Answer the entrywise product with the argument, also called Hadamard product."
	^ self class bits: (bits bitAnd: aBitTuple asInteger) parent: (self size <= aBitTuple size ifTrue: [parent] ifFalse: [aBitTuple parent])! !

!BitTuple methodsFor: 'operations' stamp: 'len 7/10/2020 17:23:55'!
length
	^ self norm! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/8/2019 03:09:27'!
lift
	"Answer a representative integer tuple."
	^ self representative! !

!BitTuple methodsFor: 'operations' stamp: 'len 3/28/2016 07:30'!
negated
	^ self! !

!BitTuple methodsFor: 'operations' stamp: 'len 7/10/2020 17:25:49'!
norm
	self flag: #fix. "this assumes the inner product is the dot product"
	^ self sum! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/8/2019 03:08:16'!
parity
	"Answer 1 if the receiver has an odd number of bits on, 0 if it has an even number of bits on."
	| n parity |
	bits = 0 ifTrue: [^ 0].
"	n _ bits >> (bits lowBit - 1). skip initial zeros, we could also skip chunks of zeros in the middle"
	n _ bits.
	parity _ 0.
	[n = 0] whileFalse: [parity _ parity bitXor: (n bitAnd: 1). n _ n >> 1].
	^ parity! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:40:07'!
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

!BitTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:40:15'!
polynomialRemainderOfDivisionBy: aTuple
	| r b n m |
	b _ aTuple asInteger.
	(m _ b highBit) = 0 ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialRemainderOfDivisionBy: argument: aTuple) signal].
	r _ self asInteger.
	[(n _ r highBit) >= m] "n = degree(r)+1"
		whileTrue:
			[r _ r bitXor: b << (n-m)]. "wasting some memory here"
	^ self class newFrom: r! !

!BitTuple methodsFor: 'operations' stamp: 'len 11/23/2016 05:42:05'!
polynomialValue: anObject
	Zero = anObject ifTrue: [^ self at: 1].
	One = anObject ifFalse: [^ super polynomialValue: anObject].
	^ self sum! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:25:18'!
reversed
	^ self class bits: (bits bitReverse: self size) parent: parent! !

!BitTuple methodsFor: 'operations' stamp: 'len 5/8/2019 02:58:43'!
sum
	"Answer the sum of the bits, i.e. the parity."
	^ bits bitParity = 0 ifTrue: [Zero] ifFalse: [One]! !

!BitTuple methodsFor: 'operations' stamp: 'len 11/12/2016 08:41'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ self asInteger bitCount! !

!BitTuple methodsFor: 'accessing' stamp: 'len 5/8/2019 02:58:37'!
at: anInteger
	^ (bits bitAt: anInteger) = 1 ifTrue: [One] ifFalse: [Zero]! !

!BitTuple methodsFor: 'accessing' stamp: 'len 1/1/2022 21:56:38'!
at: anInteger put: aBit
	| bit |
	anInteger > self size ifTrue: [self error: 'index out of bounds'].
	bit _ aBit.
	bit isInteger ifTrue: [bit _ bit even ifTrue: [Zero] ifFalse: [One]].
	(self at: anInteger) = bit
		ifFalse: [bits _ bits bitXor: (1 bitShift: anInteger - 1)].
	^ bit! !

!BitTuple methodsFor: 'accessing' stamp: 'len 7/19/2020 08:06:56'!
bitAt: anInteger
	^ bits bitAt: anInteger! !

!BitTuple methodsFor: 'accessing' stamp: 'len 7/19/2020 08:08:54'!
bitAt: anInteger put: oneOrZero
	anInteger > self size ifTrue: [self error: 'index out of bounds'].
	(self bitAt: anInteger) = oneOrZero
		ifFalse: [bits _ bits bitXor: 1 << (anInteger - 1)].
	^ oneOrZero! !

!BitTuple methodsFor: 'accessing' stamp: 'len 1/2/2022 09:56:51'!
depth
	"Answer the index of the last nonzero entry, or 0.
	(Instead, Magma defines this as the index of the *first* nonzero entry.)
	See >>pivot for the index of the first nonzero entry."
	^ bits highBit! !

!BitTuple methodsFor: 'accessing' stamp: 'len 5/29/2020 15:21:10'!
representative
	"Answer a representative integer tuple."
	^ parent lift filling: [:i| bits bitAt: i]! !

!BitTuple methodsFor: 'accessing' stamp: 'len 5/4/2018 19:09:27'!
scalars
	^ One parent! !

!BitTuple methodsFor: 'comparing' stamp: 'len 7/27/2020 08:43:41'!
= anObject
	^ self class = anObject class and: [bits = anObject asInteger]! !

!BitTuple methodsFor: 'comparing' stamp: 'len 5/8/2019 03:06:15'!
hash
	^ bits hash! !

!BitTuple methodsFor: 'constants' stamp: 'len 5/28/2020 14:24:55'!
zero
	^ self class bits: 0 parent: parent! !

!BitTuple methodsFor: 'converting' stamp: 'len 5/8/2019 03:10:19'!
asInteger
	^ bits! !

!BitTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:29:53'!
, anObject
	anObject isTuple
		ifTrue: [^ self class bits: (bits bitXor: (anObject asInteger bitShift: self size)) parent: self scalars^(self size+anObject size)].
	^ self copyWith: anObject! !

!BitTuple methodsFor: 'copying' stamp: 'len 1/1/2022 21:09:00'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."
	| n |
	n _ stop - start + 1 max: 0.
	^ self class bits: ((bits bitShift: 1 - start) bitAnd: (1 bitShift: n) - 1) parent: self scalars^n! !

!BitTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:24:12'!
copyWith: aBit
	| answer |
	answer _ self class bits: bits parent: self scalars^(self size + 1).
	aBit = One ifTrue: [answer at: self size+1 put: One. ^ answer].
	aBit = Zero ifFalse: [DomainError signal].
	^ answer! !

!BitTuple methodsFor: 'enumerating' stamp: 'len 11/5/2016 09:14'!
anySatisfy: aBlock
	"Optimized. There are only two possible elements that could satisfy aBlock (1 or 0)."
	self isEmpty ifTrue: [^ false].
	(self isZero not and: [aBlock value: One]) ifTrue: [^ true].
	^ (aBlock value: Zero) and: [super anySatisfy: [:any| any = Zero]]! !

!BitTuple methodsFor: 'testing' stamp: 'len 1/3/2022 18:29:45'!
isZero
	^ bits == 0! !

!BitTuple methodsFor: 'testing' stamp: 'len 12/30/2021 11:34:15'!
isZeroAt: anInteger
	^ (bits bitAt: anInteger) = 0! !

!BitTuple methodsFor: 'private' stamp: 'len 5/8/2019 03:03:28'!
bits: anInteger
	bits _ anInteger! !

!BitTuple class methodsFor: 'initialization' stamp: 'len 5/23/2020 08:58:58'!
initialize
	| Z2 |
	super initialize.
	Z2 _ ƒ / 2.
	One _ Z2 one.
	Zero _ Z2 zero! !

!BitTuple class methodsFor: 'instance creation' stamp: 'len 1/1/2022 20:27:22'!
bits: anInteger parent: aFreeModule
	self assert: anInteger highBit <= aFreeModule rank.
	^ self basicNew bits: anInteger; parent: aFreeModule! !

!BitTuple class methodsFor: 'instance creation' stamp: 'len 5/28/2020 14:51:42'!
components: anArray parent: aFreeModule
	| answer |
	answer _ self bits: 0 parent: aFreeModule.
	anArray withIndexDo: [:each :i| answer at: i put: each].
	^ answer! !

!BitTuple class methodsFor: 'instance creation' stamp: 'len 11/5/2016 07:09'!
new
	^ self shouldNotImplement! !

!BitTuple class methodsFor: 'instance creation' stamp: 'len 5/28/2020 14:38:16'!
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

!HermiteColumnReduction methodsFor: 'as yet unclassified' stamp: 'len 9/28/2018 19:46:17'!
matrix: aMatrix
	matrix _ ColumnMatrix newFrom: aMatrix.
	self run! !

!HermiteColumnReduction methodsFor: 'as yet unclassified' stamp: 'len 7/10/2019 16:42:08'!
reduceStep: k row: i
	| j0 b |
	"2. Row finished?"
	[j0 _ (1 to: k-1)
		detect: [:j| (matrix at: i@j) ~= 0]
		ifNone: [(matrix at: i@k) < 0 ifTrue: [matrix multiplyColumn: k by: -1]. ^ self "go to step 5"].
	"3. Choose non-zero entry with smallest absolute value:"
	j0 _ ((k to: j0 by: -1) select: [:j| (matrix at: i@j) ~= 0]) detectMin: [:j| (matrix at: i@j) abs].
	j0 < k ifTrue: [matrix swapColumn: j0 with: k].
	(matrix at: i@k) < 0 ifTrue: [matrix multiplyColumn: k by: -1].
	b _ matrix at: i@k.
	"4. Reduce:"
	1 to: k-1 do: [:j| | q |
		q _ ((matrix at: i@j) / b) rounded.
"		q _ (matrix at: i@j) // b."
		matrix addColumn: k times: q negated toColumn: j]] repeat! !

!HermiteColumnReduction methodsFor: 'as yet unclassified' stamp: 'len 9/28/2018 19:46:34'!
reduced
	^ matrix! !

!HermiteColumnReduction methodsFor: 'as yet unclassified' stamp: 'len 5/30/2019 07:11:48'!
run
	| n m i k l b |
	m _ matrix height.
	n _ matrix width.
	"1. Initialize:"
	i _ m.
	k _ n.
	l _ m > n ifTrue: [m-n+1] ifFalse: [1].
	[self reduceStep: k row: i.
	"5. Final reductions:"
	b _ matrix at: i@k.
	b = 0
		ifTrue:
			[k _ k + 1]
		ifFalse:
			[k+1 to: n do: [:j| | q |
				q _ ((matrix at: i@j) / b) floor. "(matrix at: i@j) // b."
				matrix addColumn: k times: q negated toColumn: j]].
	"6. Finished?"
	i = l] whileFalse: [i _ i - 1. k _ k - 1].
	^ matrix "copyFrom: 1@k to: m@n"! !

!HermiteColumnReduction class methodsFor: 'instance creation' stamp: 'len 9/28/2018 19:47:06'!
on: aMatrix
	^ self new matrix: aMatrix! !

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

!RootSystem methodsFor: 'operations' stamp: 'len 12/18/2021 20:31:19'!
coxeterMatrix
	"Answer the Coxeter matrix of the receiver.
	Given a basis a_i of simple roots, the Coxeter matrix m_ij is defined by <a_i,a_j> = -2 cos(pi / m_ij)."
	| S |
	S _ self normalized simpleRoots.
	^ ambient scalars
		matrix: S size
		filling: [:i :j| Float pi / ((ambient innerProduct value: {(S at: i). (S at: j)}) / -2) arcCos]! !

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

!RootSystem methodsFor: 'operations' stamp: 'len 5/20/2018 15:59:24'!
lattice
	"Answer the root lattice, the Z-submodule spanned by roots of the receiver."
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

!RootSystem methodsFor: 'operations' stamp: 'len 10/17/2016 11:03'!
weylAction
	| W |
	W _ self weylGroup.
	W name: 'W'.
	^ GroupAction from: (W, self roots) to: self roots evaluatingWithArguments: [:f :x| f value: x]! !

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
	V _ „ ^ 3.
	roots _ Set new.
	1 to: V dimension do: [:i|
		1 to: i-1 do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: j) - (V e: i)]].
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example2
	"This is a normalized root system in R^n. The Weyl group associated to it is the hyperoctahedral group."
	| V roots |
	V _ … ^ 3.
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
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example3
	"This is a normalized root system in R^n. The Weyl group associated to it is an index 2 subgroup of the hyperoctahedral group."
	| V roots |
	V _ „ ^ 3.
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
	^ self A: „ ^ 2! !

!RootSystem class methodsFor: 'instance creation' stamp: 'len 5/23/2016 05:00'!
on: aVectorSpace roots: aCollection
	^ self new ambient: aVectorSpace; roots: aCollection! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem1
	| V roots |
	V _ „ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i| 1 to: i-1 do: [:j| |r| roots add: (r _ (V x: i) - (V x: j)); add: r negated]].
	^ RootSystem on: V roots: roots! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem2
	| V roots |
	V _ „ ^ 3.
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
BitTuple initialize!
