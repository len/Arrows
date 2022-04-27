'From Cuis 6.0 [latest update: #5053] on 27 April 2022 at 8:02:52 pm'!
'Description '!
!provides: 'Etc' 1 3!
!requires: 'Domains' 1 647 nil!
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

!classDefinition: #DirectSumModule category: #Etc!
Module subclass: #DirectSumModule
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'DirectSumModule class' category: #Etc!
DirectSumModule class
	instanceVariableNames: ''!

!classDefinition: #TensorModule category: #Etc!
Module subclass: #TensorModule
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'TensorModule class' category: #Etc!
TensorModule class
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

!classDefinition: #SemidirectProductElement category: #Etc!
GroupElement subclass: #SemidirectProductElement
	instanceVariableNames: 'parent left right'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'SemidirectProductElement class' category: #Etc!
SemidirectProductElement class
	instanceVariableNames: ''!

!classDefinition: #ModuleTuple category: #Etc!
ModuleElement subclass: #ModuleTuple
	instanceVariableNames: 'parent components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ModuleTuple class' category: #Etc!
ModuleTuple class
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

!classDefinition: #Tensor2 category: #Etc!
Sum subclass: #Tensor2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Tensor2 class' category: #Etc!
Tensor2 class
	instanceVariableNames: ''!

!classDefinition: #Tensor category: #Etc!
ModuleElement subclass: #Tensor
	instanceVariableNames: 'map table'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'Tensor class' category: #Etc!
Tensor class
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

!classDefinition: #DenseTuple category: #Etc!
Object subclass: #DenseTuple
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'DenseTuple class' category: #Etc!
DenseTuple class
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

!classDefinition: #ModularTuple category: #Etc!
Object subclass: #ModularTuple
	instanceVariableNames: 'representatives'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ModularTuple class' category: #Etc!
ModularTuple class
	instanceVariableNames: ''!

!classDefinition: #ByteModularTuple category: #Etc!
ModularTuple subclass: #ByteModularTuple
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'ByteModularTuple class' category: #Etc!
ByteModularTuple class
	instanceVariableNames: ''!

!classDefinition: #WordModularTuple category: #Etc!
ModularTuple subclass: #WordModularTuple
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'WordModularTuple class' category: #Etc!
WordModularTuple class
	instanceVariableNames: ''!

!classDefinition: #MultivariatePolynomialOld category: #Etc!
Object subclass: #MultivariatePolynomialOld
	instanceVariableNames: 'parts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc'!
!classDefinition: 'MultivariatePolynomialOld class' category: #Etc!
MultivariatePolynomialOld class
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

!DirectSumModule commentStamp: '<historical>' prior: 0!
Direct sums of modules. Elements are tuples of module elements (see ModuleTuple) where the i-th component corresponds to an element in the i-th module of the direct sum, and multiplication by scalars and addition are defined component wise.!

!TensorModule commentStamp: '<historical>' prior: 0!
Tensor product over R of a list of free R-modules. This is a free R-module with basis given by elementary tensors on the choice of basis of the component modules.

See Tensor.!

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

!SemidirectProductElement commentStamp: '<historical>' prior: 0!
Elements of a semidirect product of groups. See SemidirectProductGroup.!

!ModuleTuple commentStamp: '<historical>' prior: 0!
Elements of direct sums of modules, i.e. tuples of module elements. See DirectSumModule.!

!Sum commentStamp: '<historical>' prior: 0!
Formal sums (or formal linear combinations) of arbitrary symbols with coefficients over a given ring R. They can be thought of as Tuples indexed not by positive integers but by arbitrary symbols. They form free R-modules (see FreeModule).!

!Divisor commentStamp: '<historical>' prior: 0!
Weil divisors, i.e. formal sums (with integer coefficients) of prime divisors (integral closed subschemes of codimension 1) of an integral locally Noetherian scheme.!

!Tensor2 commentStamp: '<historical>' prior: 0!
Elements of tensor product modules (see TensorProductModule).

Tensors are represented as sparse formal sums of formal products of basis elements.!

!RelaxedAdicInteger commentStamp: '<historical>' prior: 0!
Elements of the p-adic ring Zp. See AdicRing.!

!RelaxedAdicNumber commentStamp: '<historical>' prior: 0!
Elements of the p-adic field Qp. See AdicField.

A p-adic number z is represented uniquely by an integer k (the valuation) and a unit u in Zp, such that z = (p^k)*u.!

!RelaxedPowerSeries commentStamp: '<historical>' prior: 0!
Univariate formal power series with coefficients over a commutative ring. See PowerSeriesRing.!

!BerlekampMassey commentStamp: '<historical>' prior: 0!
This is the Berlekamp-Massey algorithm. The initial purpose of this algorithm was to evaluate Binary BCH codes, but it has found many uses, including inverting matrices of constant diagonal, solving linear recurrences (synthesizing LFSRs with a specified output sequence), etc.

Input: a_1..a_{2n} the first 2n coefficients of a linear recursive sequence over a field K of order at most n (the minimal polynomial has degree bound n);
Output: the minimal polynomial of the sequence.

The algorithm performs O(n^2/2) multiplications; more precisely, at most 2 floor(n^2/4) - n + 1 multiplications.!

!BitTuple commentStamp: '<historical>' prior: 0!
Tuples of integer residue classes modulo 2, i.e. binary tuples, bit strings, or tuples of bits. The coefficient field is É/2É.

Example:
	"A handy way to create a binary tuple:"
	(1,0,1,1,0,1,1,1) % 2
!

!DenseTuple commentStamp: '<historical>' prior: 0!
General n-tuples internally represented as an Array. These are the tuples used by default for an arbitrary ring.!

!ModularTuple commentStamp: '<historical>' prior: 0!
Tuples with coefficients in an integer residue class ring (or field) É/mÉ. See ModularInteger, ModularIntegerRing, PrimeField.

Example:
	"A handy way to create a tuple of integers modulo 3:"
	(1,2,3,4) % 3
!

!MultivariatePolynomialOld commentStamp: '<historical>' prior: 0!
Polynomials in one or more indeterminates, represented in sparse form.

Multivariate polynomials are internally stored as a list of associations 'monomial -> coefficient'. Zero coefficients are excluded, and the list is kept sorted according to the monomial ordering.!

!SmithReduction commentStamp: '<historical>' prior: 0!
This is the Smith reduction algorithm. Given a matrix with coefficients in a PID, it reduces the matrix to its Smith normal form.

The Smith normal form is very useful for working with finitely generated modules over a PID, and in particular for deducing the structure of a quotient of a free module.

Reference: Henri Cohen, 'A Course in Computational Algebraic Number Theory', Algorithm 2.4.14.!

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

!DirectSumModule methodsFor: 'printing' stamp: 'len 5/23/2020 07:22:45'!
printOn: aStream
	components do: [:each| aStream print: each] separatedBy: [aStream nextPut: $ê]! !

!TensorModule methodsFor: 'printing' stamp: 'len 5/23/2020 07:22:26'!
printOn: aStream
	components do: [:each| aStream print: each] separatedBy: [aStream nextPut: $ë]! !

!SemidirectProductElement methodsFor: 'printing' stamp: 'len 2/12/2016 06:10'!
printOn: aStream
	aStream nextPut: $(; print: left; nextPutAll: ', '; print: right; nextPut: $)! !

!ModuleTuple methodsFor: 'printing' stamp: 'len 4/25/2020 07:18:35'!
printOn: aStream
	aStream print: components! !

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

!BitTuple methodsFor: 'operations' stamp: 'len 7/15/2021 12:42:04'!
>> anInteger
	^ self class bits: (bits bitShift: anInteger) parent: parent! !

!BitTuple methodsFor: 'printing' stamp: 'len 3/28/2016 04:52'!
printOn: aStream
	1 to: self size do: [:i| aStream print: (self at: i)]! !

!DenseTuple methodsFor: 'operations' stamp: 'len 4/17/2022 17:17:47'!
>> anInteger
	anInteger == 0 ifTrue: [^ self].
	^ self class components: components >> anInteger parent: parent! !

!DenseTuple methodsFor: 'printing' stamp: 'len 4/18/2022 12:31:35'!
printOn: aStream
	components printOn: aStream! !

!ModularTuple methodsFor: 'printing' stamp: 'len 5/18/2020 13:47:12'!
printOn: aStream
	self modulus > 16 ifTrue: [^ super printOn: aStream].
	representatives do: [:each| each printOn: aStream base: 16]! !

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

!StandardBasis methodsFor: 'private' stamp: 'len 5/10/2018 06:27:11'!
verify
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
	F _ É/p.
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

!SemidirectProductGroup methodsFor: 'accessing' stamp: 'len 4/22/2016 20:49'!
identity
	^ self project: {left identity. right identity}! !

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
	^ ((left, right) to: self evaluating: [:each| self project: each]) name: 'é'! !

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

!DirectSumModule methodsFor: 'accessing' stamp: 'len 4/25/2020 07:00:24'!
components
	^ components! !

!DirectSumModule methodsFor: 'accessing' stamp: 'len 7/10/2021 05:40:52'!
generatorMatrix
	^ self propertyAt: #generatorMatrix ifAbsentPut:
		[components inject: (self scalars^0) id into: [:answer :each| answer ê each coordinates id]]! !

!DirectSumModule methodsFor: 'accessing' stamp: 'len 7/6/2021 09:56:40'!
generators
	^ self propertyAt: #generators ifAbsentPut:
		[| generators |
		generators _ OrderedCollection new: (components product: [:each| each generators size]).
		1 to: components size do: [:i|
			(components at: i) generators do: [:each|
				generators add: (self compose: (components withIndexCollect: [:eachModule :j| i=j ifTrue: [each] ifFalse: [eachModule zero]]))]].
		generators asArray]! !

!DirectSumModule methodsFor: 'accessing' stamp: 'len 7/10/2021 05:40:02'!
presentationMatrix
	^ self propertyAt: #presentationMatrix ifAbsentPut:
		[components inject: (self scalars^0) id into: [:answer :each| answer ê each presentationMatrix]]! !

!DirectSumModule methodsFor: 'accessing' stamp: 'len 4/25/2020 09:24:37'!
scalars
	^ components first scalars! !

!DirectSumModule methodsFor: 'accessing' stamp: 'len 4/25/2020 07:00:35'!
size
	^ components product: [:each| each size]! !

!DirectSumModule methodsFor: 'accessing' stamp: 'len 4/25/2020 07:02:42'!
zero
	^ self compose: (components collect: [:each| each zero])! !

!DirectSumModule methodsFor: 'comparing' stamp: 'len 4/25/2020 07:01:13'!
= anObject
	^ self == anObject or: [self class == anObject class and: [components = anObject components]]! !

!DirectSumModule methodsFor: 'comparing' stamp: 'len 4/25/2020 07:01:17'!
hash
	^ components hash! !

!DirectSumModule methodsFor: 'elements' stamp: 'len 5/3/2021 11:24:54'!
\ anElement
	"Answer coordinates of anElement expressed as a linear combination of the elements of the distinguished generating set of the receiver. Answer nil if anElement is not in the receiver."
	^ (1 to: components size) inject: (self scalars^0) zero into: [:answer :i| answer, ((components at: i) \ (anElement at: i))]! !

!DirectSumModule methodsFor: 'elements' stamp: 'len 7/6/2021 08:47:47'!
compose: anArray
	^ ModuleTuple new parent: self; components: anArray asArray! !

!DirectSumModule methodsFor: 'enumerating' stamp: 'len 10/17/2020 20:00:00'!
do: aBlock
	self asCartesianProduct do: [:each| aBlock value: (self compose: each)]! !

!DirectSumModule methodsFor: 'morphisms' stamp: 'len 4/25/2020 07:04:35'!
coprojection: i
	^ ((self at: i) to: self evaluating: [:x| self compose: (components withIndexCollect: [:each :k| k = i ifTrue: [x] ifFalse: [each zero]])]) name: 'i', i printText sub! !

!DirectSumModule methodsFor: 'morphisms' stamp: 'len 5/23/2020 07:30:35'!
projection: i
	"Answer the canonical projection to the i-th component."
	^ (self to: (self components at: i) evaluating: [:x| x at: i]) name: 'é', i printText sub! !

!DirectSumModule methodsFor: 'testing' stamp: 'len 4/25/2020 07:04:03'!
includes: anObject
	^ anObject parent = self! !

!DirectSumModule methodsFor: 'testing' stamp: 'len 4/25/2020 07:04:49'!
isCoproduct
	^ true! !

!DirectSumModule methodsFor: 'testing' stamp: 'len 5/3/2021 11:19:54'!
isFreelyGenerated
	"Answer true if the receiver is free on free generators."
	^ components allSatisfy: [:each| each isFreelyGenerated]! !

!DirectSumModule methodsFor: 'testing' stamp: 'len 4/25/2020 07:04:53'!
isProduct
	^ true! !

!DirectSumModule methodsFor: 'private' stamp: 'len 4/25/2020 07:03:54'!
components: anArray
	components _ anArray! !

!DirectSumModule class methodsFor: 'instance creation' stamp: 'len 4/25/2020 07:13:51'!
components: anArray
	^ self new components: anArray! !

!TensorModule methodsFor: 'accessing' stamp: 'len 6/3/2018 20:15:48'!
at: anInteger
	^ components at: anInteger! !

!TensorModule methodsFor: 'accessing' stamp: 'len 6/3/2018 20:15:26'!
components
	^ components! !

!TensorModule methodsFor: 'accessing' stamp: 'len 9/16/2018 16:18:35'!
indices
	^ self propertyAt: #indices ifAbsentPut: [self computeIndices]! !

!TensorModule methodsFor: 'accessing' stamp: 'len 9/16/2018 16:18:48'!
scalars
	components do: [:any| ^ any scalars].
	^ nil! !

!TensorModule methodsFor: 'accessing' stamp: 'len 9/16/2018 16:18:53'!
type
	"Tensor products of n copies of a vector space and m copies of its dual are of type (n,m)."
	self propertyAt: #type ifAbsentPut: [self computeType]! !

!TensorModule methodsFor: 'basis' stamp: 'len 4/26/2021 11:37:27'!
\ anElement
	"Answer coordinates of anElement expressed as a linear combination of the elements of the distinguished generating set of the receiver. Answer nil if anElement is not in the receiver."
	^ self scalars tuple: (self indices collect: [:each| anElement at: each])! !

!TensorModule methodsFor: 'basis' stamp: 'len 7/13/2020 07:24:11'!
basis
	^ self propertyAt: #basis ifAbsentPut: [self indices collect: [:each| Tensor2 new parent: self; at: each put: self scalars one; yourself]]! !

!TensorModule methodsFor: 'basis' stamp: 'len 6/3/2018 20:18:15'!
rank
	^ components product: [:each| each rank]! !

!TensorModule methodsFor: 'converting' stamp: 'len 9/15/2018 17:06:12'!
over: aRing
	"Answer the change of base ring of the receiver."
	^ self class components: (components collect: [:each| each over: aRing])! !

!TensorModule methodsFor: 'elements' stamp: 'len 9/16/2018 16:17:45'!
!! anObject
	(anObject isSequenceable and: [anObject size = self arity and: [(1 to: self arity) allSatisfy: [:i| (self at: i) includes: (anObject at: i)]]])
		ifTrue: [^ self compose: anObject].
	^ super !! anObject! !

!TensorModule methodsFor: 'elements' stamp: 'len 7/13/2020 07:24:11'!
evaluating: aBlock
	| answer |
	answer _ Tensor2 new parent: self.
	self indices do: [:each| answer at: each put: (aBlock value: each)].
	^ answer! !

!TensorModule methodsFor: 'morphisms' stamp: 'len 4/26/2021 11:44:13'!
compose: anArray
	| coordinates answer |
	anArray size = self arity ifFalse: [DomainError signal].
	coordinates _ anArray withIndexCollect: [:each :i| (self at: i) \ each].
	answer _ Tensor2 new parent: self.
	self indices do: [:each|
		answer at: each put: ((1 to: coordinates size) inject: self scalars one into: [:product :i| product * ((coordinates at: i) at: (each at: i))])].
	^ answer! !

!TensorModule methodsFor: 'morphisms' stamp: 'len 6/29/2020 12:47:04'!
composition
	"This is the product of tensors. This is a multilinear map."
	^ (self carrier to: self evaluating: [:each| self compose: each]) name: 'ë'! !

!TensorModule methodsFor: 'testing' stamp: 'len 4/22/2020 06:12:53'!
includes: anObject
	^ anObject parent = self! !

!TensorModule methodsFor: 'private' stamp: 'len 9/16/2018 16:17:53'!
components: anArray
	components _ anArray

"	| algebra indeterminates |
	components _ anArray.
	indeterminates _ OrderedCollection new.
	anArray withIndexDo: [:each :i| 1 to: each rank do: [:j| indeterminates add: 'x', (i printString, ',' j printString) sub]].
	algebra _ components anyOne scalars freeAlgebraIn: indeterminates.
	self propertyAt: #ambientAlgebra put: algebra"! !

!TensorModule methodsFor: 'private' stamp: 'len 9/16/2018 16:18:10'!
computeIndices
	| indices |
	indices _ OrderedCollection with: {}.
	components do: [:X| | newIndices |
		newIndices _ OrderedCollection new.
		indices do: [:each| 1 to: X rank do: [:i| newIndices add: each, {i}]].
		indices _ newIndices].
	^ indices! !

!TensorModule methodsFor: 'private' stamp: 'len 9/16/2018 16:18:14'!
computeType
	| V W i n |
	V _ components first.
	i _ components findFirst: [:each| each ~= V].
	i = 0 ifTrue: [^ {self arity. 0}].
	W _ components at: i.
	W = V dual ifFalse: [^ nil].
	n _ components findLast: [:each| each ~= W].
	i-1 = n ifFalse: [^ nil].
	^ {n. self arity - n}! !

!TensorModule class methodsFor: 'instance creation' stamp: 'len 6/3/2018 20:18:41'!
components: anArray
	^ self new components: anArray! !

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
	g _ (É/2) polynomials !! [:x| x^11 + (x^10) + (x^6) + (x^5) + (x^4) + (x^2) + 1].
	^ self new: 23 generator: g! !

!PolynomialCode class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
H7
	"Answer the Hamming code [7,4,3]."
	^ self new: 7 generator: (É/2) polynomials !! [:x| x^3 + x + 1]! !

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
∑ aSemidirectProduct
	^ self parent
		project:
			{left ∑ aSemidirectProduct left.
			parent action value: {aSemidirectProduct left. right} ∑ aSemidirectProduct right}! !

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

!ModuleTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 07:15:45'!
at: anInteger
	^ components at: anInteger! !

!ModuleTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 09:29:24'!
coordinates
	^ components inject: (self scalars tuple: #()) into: [:answer :each| answer, each coordinates]! !

!ModuleTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 07:15:52'!
parent
	^ parent! !

!ModuleTuple methodsFor: 'accessing' stamp: 'len 4/25/2020 07:15:56'!
size
	^ components size! !

!ModuleTuple methodsFor: 'comparing' stamp: 'len 4/25/2020 07:16:08'!
= anObject
	^ self class = anObject class and: [components = anObject components]! !

!ModuleTuple methodsFor: 'comparing' stamp: 'len 4/25/2020 07:16:03'!
hash
	^ components hash! !

!ModuleTuple methodsFor: 'operations' stamp: 'len 5/3/2020 12:52:25'!
* anElement
	anElement isInteger ifTrue: [^ self class new parent: parent; components: components * anElement].
	(parent scalars includes: anElement) ifTrue: [^ self class new parent: parent; components: components * anElement].
	^ DomainError signal! !

!ModuleTuple methodsFor: 'operations' stamp: 'len 4/25/2020 07:16:30'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ self class new parent: parent; components: components + anElement components! !

!ModuleTuple methodsFor: 'operations' stamp: 'len 4/25/2020 07:16:38'!
negated
	^ self class new parent: parent; components: (components collect: [:each| each negated])! !

!ModuleTuple methodsFor: 'testing' stamp: 'len 4/25/2020 09:26:58'!
isZero
	^ components allSatisfy: [:each| each isZero]! !

!ModuleTuple methodsFor: 'private' stamp: 'len 4/25/2020 07:19:10'!
components
	^ components! !

!ModuleTuple methodsFor: 'private' stamp: 'len 4/25/2020 07:19:06'!
components: anArray
	components _ anArray! !

!ModuleTuple methodsFor: 'private' stamp: 'len 4/25/2020 07:19:01'!
parent: aDirectSumModule
	parent _ aDirectSumModule! !

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
∑ aSum
	| answer |
	self size <= aSum size ifFalse: [^ aSum ∑ self].
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

!Tensor2 methodsFor: 'converting' stamp: 'len 9/22/2018 22:07:17'!
asSimpleTensor
	"Answer an array of basis elements."
	self size = 1 ifFalse: [^ self error: 'not a simple tensor'].
	self keysAndValuesDo: [:any| ^ (any withIndexCollect: [:i :j| (parent at: j) e: i])].
	^ self halt! !

!Tensor2 methodsFor: 'operations' stamp: 'len 5/14/2019 20:50:19'!
ë aTensor
	^ self parent ë aTensor parent compose: {self. aTensor}! !

!Tensor2 methodsFor: 'printing' stamp: 'len 5/23/2020 07:22:06'!
printLiteral: anArray on: aStream
	(1 to: anArray size) do: [:i| aStream nextPutAll: ((parent at: i) names at: (anArray at: i))] separatedBy: [aStream nextPut: $ë]! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 07:26:26'!
asMultilinearMap
	^ map! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 07:33:43'!
associatedForm
	"Answer the associated multilinear form."
	self notYetImplemented
"	^ MultilinearMap from: self frame to: self scalars table: table"! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 09:01:44'!
at: anArray
	| answer |
	answer _ table.
	anArray do: [:i| answer _ answer at: i].
	^ answer! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 07:35:15'!
compression
	"Answers the compression of the receiver, removing all 1-dimensional spaces in the domain."
	^ self notYetImplemented! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 07:33:57'!
frame
	^ map domain, map codomain! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 13:18:58'!
isContravariant
	self notYetImplemented! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 13:18:52'!
isCovariant
	self notYetImplemented! !

!Tensor methodsFor: 'as yet unclassified' stamp: 'len 7/13/2020 07:34:20'!
valence
	^ map domain arity! !

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

!SmallPrimeResidue methodsFor: 'operations' stamp: 'len 1/6/2022 19:42:05'!
/ anElement
	| b i |
	anElement class == self class ifFalse: [^ super / anElement].
	b _ anElement representative.
	(representative = 0 or: [b = 1]) ifTrue: [^ self].
	b = 0 ifTrue: [^ InversionError signal: anElement].
	i _ (parent logTable at: representative) - (parent logTable at: b).
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
						(x ~= 1 or: [i = 0]) ifTrue: [aStream print: x. i ~= 0 ifTrue: [aStream nextPut: $∑]].
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
	^ Ñ powerSeries coefficients:
		(Sequence to: Ñ evaluating: [:n| n even ifTrue: [0] ifFalse: [|k| k _ n-1/2. (-1)^k / (2*k) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
exp
	"The power series expansion of exp(x)."
	^ Ñ powerSeries
		coefficients: (Sequence to: Ñ evaluating: [:n| 1 / n factorial])! !

!RelaxedPowerSeries class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
sin
	"The power series expansion of sin(x)."
	^ Ñ powerSeries
		coefficients: (Sequence to: Ñ evaluating: [:n| n odd ifTrue: [0] ifFalse: [|k| k _ n/2-1. (-1)^k / (2*k+1) factorial]])! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 12/27/2015 16:21'!
coefficients: aSequence
	^ self new coefficients: aSequence! !

!RelaxedPowerSeries class methodsFor: 'instance creation' stamp: 'len 11/25/2016 10:32:44'!
fromPolynomial: aPolynomial
	^ self coefficients: (CFiniteSequence to: aPolynomial scalars initialValues: aPolynomial coefficients periodic: {aPolynomial scalars zero})! !

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
∑ aBitTuple
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
	Z2 _ É / 2.
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

!DenseTuple methodsFor: 'accessing' stamp: 'len 1/1/2022 10:24:41'!
at: anInteger
	anInteger isInteger ifFalse: [^ (components at: anInteger position) at: anInteger monomial].
	^ components at: anInteger! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 1/1/2022 10:23:49'!
at: anInteger ifAbsent: aBlock
	anInteger isInteger ifFalse: [^ (components at: anInteger position ifAbsent: [^aBlock value]) at: anInteger monomial ifAbsent: aBlock].
	^ components at: anInteger ifAbsent: aBlock! !

!DenseTuple methodsFor: 'accessing' stamp: 'len 7/26/2020 13:29:00'!
size
	^ components size! !

!DenseTuple methodsFor: 'comparing' stamp: 'len 4/15/2021 09:47:16'!
= anObject
	| n otherComponents |
	self class = anObject class ifFalse: [^ false].
"	scalars = anObject scalars ifFalse: [^ false]."
	otherComponents _ anObject components.
	(n _ components size) = otherComponents size ifFalse:
		[(n _ self depth) = anObject depth ifFalse: [^ false]].
	1 to: n do: [:i| (components at: i) = (otherComponents at: i) ifFalse: [^ false]].
	^ true! !

!DenseTuple methodsFor: 'comparing' stamp: 'len 7/27/2020 08:31:26'!
hash
	| answer |
	answer _ 0.
	components do: [:each| each isZero ifFalse: [answer _ answer hashMultiply + each hash]].
	^ answer! !

!DenseTuple methodsFor: 'converting' stamp: 'len 4/18/2022 15:10:39'!
asArray
	self flag: #review.
	^ components asArray
"	^ components"! !

!DenseTuple methodsFor: 'copying' stamp: 'len 4/18/2022 15:18:12'!
, anObject
	anObject isTuple
		ifTrue: [^ self scalars^(self size + anObject size) elementAt: components, anObject components].
	anObject isArray
		ifTrue: [^ self scalars^(self size + anObject size) elementAt: components asArray, anObject asArray].
	^ self copyWith: anObject! !

!DenseTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:41:59'!
copy
	^ self class components: components copy parent: parent! !

!DenseTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:42:15'!
copyFrom: start to: stop
	^ self class components: (components copyFrom: start to: stop) parent: self scalars^(stop-start+1)! !

!DenseTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:42:40'!
copyWith: anElement
	^ self class components: (components copyWith: self scalars !! anElement) parent: self scalars^(self size + 1)! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:46:09'!
allSatisfy: aBlock
	^ components allSatisfy: aBlock! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:49:31'!
anySatisfy: aBlock
	^ components anySatisfy: aBlock! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/4/2021 14:37:35'!
collect: aBlock
	| R |
	R _ self scalars.
	^ self class components: (components collect: [:each| R !! (aBlock value: each)]) parent: parent! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/12/2019 16:51:45'!
detect: aBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true. If none 
	evaluate to true, then evaluate the argument, exceptionBlock."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:50:26'!
do: aBlock
	components do: aBlock! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:45:55'!
findFirst: aBlock
	^ components findFirst: aBlock! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:44:35'!
findLast: aBlock
	^ components findLast: aBlock! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:45:37'!
reverseDo: aBlock
	components reverseDo: aBlock! !

!DenseTuple methodsFor: 'enumerating' stamp: 'len 8/18/2019 18:45:43'!
withIndexDo: aBlock
	components withIndexDo: aBlock! !

!DenseTuple methodsFor: 'operations' stamp: 'len 4/18/2022 12:52:28'!
* anObject
	((self scalars includes: anObject) or: [anObject isInteger]) ifTrue: [^ self class components: components * anObject parent: parent].
	anObject isMatrix ifTrue: [^ (self asRowMatrix * anObject) asTuple].
	anObject isTuple ifTrue: [^ self ∑ anObject].
	^ self * (self scalars !! anObject)! !

!DenseTuple methodsFor: 'operations' stamp: 'len 4/17/2022 17:11:42'!
+ aTuple
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #+].
	^ self class components: components + aTuple components parent: (self size >= aTuple size ifTrue: [parent] ifFalse: [aTuple parent])

"
	| a b n m c |
	a _ components.
	b _ aTuple components.
	n _ a size.
	m _ b size.
	c _ Array new: (n max: m).
	1 to: (n min: m) do: [:i| c at: i put: (a at: i) + (b at: i)].
	m > n
		ifTrue: [c replaceFrom: n+1 to: m with: b startingAt: n+1]
		ifFalse: [m < n ifTrue: [c replaceFrom: m+1 to: n with: a startingAt: m+1]].
	^ self class components: c parent: (n >= m ifTrue: [parent] ifFalse: [aTuple parent])"! !

!DenseTuple methodsFor: 'operations' stamp: 'len 4/17/2022 17:12:18'!
- aTuple
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #-].
	^ self class components: components - aTuple components parent: (self size >= aTuple size ifTrue: [parent] ifFalse: [aTuple parent])

"	| a b n m c |
	a _ components.
	b _ aTuple components.
	n _ a size.
	m _ b size.
	c _ Array new: (n max: m).
	1 to: (n min: m) do: [:i| c at: i put: (a at: i) - (b at: i)].
	n > m
		ifTrue: [c replaceFrom: m+1 to: n with: a startingAt: m+1]
		ifFalse: [n < m ifTrue: [n+1 to: m do: [:i| c at: i put: (b at: i) negated]]].
	^ self class components: c parent: (n >= m ifTrue: [parent] ifFalse: [aTuple parent])"! !

!DenseTuple methodsFor: 'operations' stamp: 'len 4/18/2022 13:07:34'!
∑ aTuple
	(self isEmpty or: [aTuple isEmpty]) ifTrue: [^ self scalars zero].
	^ components ∑ aTuple components

"	| answer |
	answer _ self scalars zero.
	1 to: (self depth min: aTuple depth) do: [:i| answer _ (self at: i) * (aTuple at: i) + answer].
	^ answer"! !

!DenseTuple methodsFor: 'operations' stamp: 'len 6/6/2019 10:00:36'!
convolution2: aTuple
	| a aH aHbH aL aLbL b bH bL mid n m |
	self size >= aTuple size ifTrue: [a _ self. b _ aTuple] ifFalse: [a _ aTuple. b _ self].
	n _ a size.
	m _ b size.
	m > 200 ifFalse: [^ self convolution: aTuple].
	mid _ n // 2.
	m > mid ifFalse: [^ self convolution: aTuple].
	aL _ a copyFrom: 1 to: mid.
	aH _ a copyFrom: mid+1 to: n.
	bL _ b copyFrom: 1 to: mid.
	bH _ b copyFrom: mid+1 to: m.
	aLbL _ aL convolution2: bL.
	aHbH _ aH convolution2: bH.
	^ aLbL + ((aL + aH convolution2: bL + bH) - aLbL - aHbH >>> mid) + (aHbH >>> (mid*2))! !

!DenseTuple methodsFor: 'operations' stamp: 'len 4/17/2022 17:14:43'!
convolution: aTuple
	| result |
	self isEmpty ifTrue: [^ self].
	aTuple isEmpty ifTrue: [^ aTuple].
	(aTuple isTuple and: [self scalars = aTuple scalars]) ifFalse: [^ self adapt: aTuple andSend: #convolution:].
	result _ components convolution: aTuple components.
	^ self class components: result parent: self scalars ^ result size

"	array _ Array new: self size + aTuple size - 1.
	array atAllPut: self scalars zero.
	self withIndexDo: [:xi :i|
		xi isZero ifFalse:
			[aTuple withIndexDo: [:yj :j|
				array at: i+j-1 add: xi*yj]]].
	^ self class components: array parent: self scalars^array size"! !

!DenseTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:42:52'!
reversed
	^ self class components: components reversed parent: parent! !

!DenseTuple methodsFor: 'testing' stamp: 'len 8/18/2019 18:50:44'!
includes: anObject
	^ components includes: anObject! !

!DenseTuple methodsFor: 'private' stamp: 'len 8/18/2019 18:50:39'!
at: anInteger put: anObject
	^ components at: anInteger put: anObject! !

!DenseTuple methodsFor: 'private' stamp: 'len 9/4/2019 12:54:27'!
components
	^ components! !

!DenseTuple methodsFor: 'private' stamp: 'len 4/18/2022 12:29:34'!
components: anArray parent: aFreeModule
	components _ anArray.
	parent _ aFreeModule! !

!DenseTuple methodsFor: 'private' stamp: 'len 8/18/2019 18:45:24'!
swap: i with: j
	components swap: i with: j! !

!DenseTuple class methodsFor: 'instance creation' stamp: 'len 4/18/2022 12:29:47'!
components: anArray parent: aFreeModule
	^ self basicNew components: anArray parent: aFreeModule! !

!DenseTuple class methodsFor: 'instance creation' stamp: 'len 11/5/2016 07:13'!
new
	^ self shouldNotImplement! !

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

!ModularTuple methodsFor: 'accessing' stamp: 'len 5/28/2020 13:18:42'!
at: anInteger
	^ self scalars representative: (representatives at: anInteger ifAbsent: [^ self scalars zero])! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 5/28/2020 13:19:19'!
at: anInteger put: anElement
	anElement isInteger ifTrue: [^ representatives at: anInteger put: anElement \\ self scalars modulus].
	representatives at: anInteger put: anElement representative.
	^ anElement! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 1/6/2022 14:25:27'!
depth
	"Answer the index of the last nonzero entry, or 0.
	(Instead, Magma defines this as the index of the *first* nonzero entry.)
	See >>pivot for the index of the first nonzero entry."
	representatives size to: 1 by: -1 do: [:i| (representatives at: i) == 0 ifFalse: [^ i]].
	^ 0! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 4/4/2021 09:51:28'!
lift
	"Answer a balanced representative integer tuple."
	| m h |
	m _ self scalars modulus.
	h _ m >> 1.
	^ parent lift filling: [:i| | r | r _ representatives at: i. r <= h ifTrue: [r] ifFalse: [r - m]]! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 1/6/2022 14:25:11'!
pivot
	"Answer the index of the first nonzero entry, or nil.
	See >>depth for the index of the last nonzero entry."
	1 to: representatives size do: [:i| (representatives at: i) == 0 ifFalse: [^ i]].
	^ nil! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 5/29/2020 15:22:21'!
representative
	"Answer a representative integer tuple."
	^ parent lift elementAt: representatives! !

!ModularTuple methodsFor: 'accessing' stamp: 'len 7/26/2020 13:29:14'!
size
	^ representatives size! !

!ModularTuple methodsFor: 'comparing' stamp: 'len 4/15/2021 09:37:30'!
= anObject
	| n otherRepresentatives |
	(self class = anObject class and: [self scalars = anObject scalars]) ifFalse: [^ false].
	otherRepresentatives _ anObject representatives.
	(n _ representatives size) = otherRepresentatives size ifFalse:
		[(n _ self depth) = anObject depth ifFalse: [^ false]].
	1 to: n do: [:i| (representatives at: i) = (otherRepresentatives at: i) ifFalse: [^ false]].
	^ true! !

!ModularTuple methodsFor: 'comparing' stamp: 'len 7/27/2020 08:39:57'!
hash
	| answer |
	answer _ 0.
	representatives do: [:each| each = 0 ifFalse: [answer _ answer hashMultiply + each hash]].
	^ answer! !

!ModularTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:35:37'!
, anObject
	anObject isTuple
		ifTrue:
			[self scalars = anObject scalars ifFalse: [^ DomainError signal].
			^ self class representatives: representatives, anObject representatives parent: self scalars^(self size + anObject size)].
	^ self copyWith: anObject! !

!ModularTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:35:50'!
copy
	^ self class representatives: representatives copy parent: parent! !

!ModularTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:35:56'!
copyFrom: start to: stop
	^ self class representatives: (representatives copyFrom: start to: stop) parent: self scalars^(stop-start+1)! !

!ModularTuple methodsFor: 'copying' stamp: 'len 5/28/2020 14:36:06'!
copyWith: anElement
	(self scalars includes: anElement) ifFalse: [^ DomainError signal].
	^ self class representatives: (representatives copyWith: anElement representative) parent: self scalars^(self size + 1)! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:35:16'!
* anElement
	| modulus x array |
	anElement isMatrix ifTrue: [^ super * anElement].
	x _ anElement isInteger ifTrue: [anElement] ifFalse: [anElement representative].
	modulus _ self scalars modulus.
	array _ representatives copy.
	1 to: self size do: [:i| array at: i put: (array at: i) * x \\ modulus].
	^ self class representatives: array parent: parent! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:53:28'!
+ aTuple
	| modulus a an b bn c |
	(an _ self size) <= (bn _ aTuple size) ifFalse: [^ aTuple + self].
	modulus _ self scalars modulus.
	a _ representatives.
	b _ aTuple representatives.
	c _ representatives class new: bn.
	1 to: an do: [:i| c at: i put: (a at: i) + (b at: i) \\ modulus].
	c replaceFrom: an+1 to: bn with: b startingAt: an+1.
	^ self class representatives: c parent: aTuple parent! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 13:18:02'!
∑ aTuple
	| modulus representatives2 answer |
	(aTuple class = self class and: [self scalars = aTuple scalars])
		ifFalse: [^ super ∑ aTuple].
	modulus _ self scalars modulus.
	representatives2 _ aTuple representatives.
	answer _ 0.
	1 to: (representatives size min: representatives2 size) do: [:i|
		answer _ (representatives at: i) * (representatives2 at: i) + answer \\ modulus].
	^ self scalars representative: answer! !

!ModularTuple methodsFor: 'operations' stamp: 'len 3/11/2021 17:20:32'!
add: aTuple times: aNumber from: startIndex to: stopIndex
	| m representatives2 x |
	(self class = aTuple class "and: [self scalars = aTuple scalars]")
		ifFalse: [^ super add: aTuple times: aNumber from: startIndex to: stopIndex].
	m _ self scalars modulus.
	representatives2 _ aTuple representatives.
	x _ aNumber representative.
	0 = x ifTrue: [^ self].
	startIndex to: stopIndex do: [:i|
		| y |
		y _ (representatives2 at: i) * x + (representatives at: i).
		y >= m ifTrue: [y _ y \\ m].
		representatives at: i put: y]! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:35:44'!
convolution: aTuple
	| modulus array |
	self isEmpty ifTrue: [^ self].
	aTuple isEmpty ifTrue: [^ aTuple].
	modulus _ self scalars modulus.
	array _ representatives class new: self size + aTuple size - 1.
	array atAllPut: 0. "in case it's Array"
	representatives withIndexDo: [:xi :i|
		xi = 0 ifFalse:
			[aTuple representatives withIndexDo: [:yj :j| | index |
				index _ i+j-1.
				array at: index put: xi*yj + (array at: index) \\ modulus]]].
	^ self class representatives: array parent: self scalars^array size! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:36:16'!
hadamard: aTuple
	| modulus c b answerParent |
	(self class = aTuple class and: [self scalars = aTuple scalars])
		ifFalse: [^ super hadamard: aTuple].
	modulus _ self scalars modulus.
	answerParent _ self size <= aTuple size ifTrue: [parent] ifFalse: [aTuple parent].
	c _ representatives class new: answerParent rank.
	b _ aTuple representatives.
	1 to: c size do: [:i| c at: i put: (representatives at: i) * (b at: i) \\ modulus].
	^ self class representatives: c parent: answerParent! !

!ModularTuple methodsFor: 'operations' stamp: 'len 3/11/2021 17:22:19'!
multiplyBy: aNumber from: startIndex to: stopIndex
	| m x |
	m _ self modulus.
	x _ aNumber representative.
	startIndex to: stopIndex do: [:i|
		| y |
		y _ (representatives at: i) * x.
		y >= m ifTrue: [y _ y \\ m].
		representatives at: i put: y]! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:36:27'!
negated
	| modulus array |
	array _ representatives copy.
	modulus _ self modulus.
	1 to: self size do: [:i| array at: i put: (array at: i) negated \\ modulus].
	^ self class representatives: array parent: parent! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:42:36'!
polynomialDivisionBy: aTuple
	| quo q rem r bmInv n m b modulus |
	aTuple isZero ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialDivisionBy: argument: aTuple) signal].
	quo _ self zero.
	q _ quo representatives.
	rem _ self copy.
	r _ rem representatives.
	m _ aTuple depth. "m = degree(self)+1"
	bmInv _ (aTuple at: m) inverse representative.
	b _ aTuple representatives.
	modulus _ self modulus.
	[(n _ rem depth) >= m] "n = degree(rem)+1"
		whileTrue:
			[| i c |
			i _ n-m+1.
			c _ (r at: n) * bmInv \\ modulus. "<- is it better to reduce or not reduce here?"
			q at: i put: (q at: i) + c \\ modulus.
			1 to: m do: [:j| r at: j+i-1 put: (r at: j+i-1) - ((b at: j)*c) \\ modulus]].
	^ {quo. rem}! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:42:51'!
polynomialPartialDivisionBy: aTuple
	| quo q rem r n m b bm modulus |
	aTuple isZero ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialPartialDivisionBy: argument: aTuple) signal].
	quo _ self zero.
	q _ quo representatives.
	rem _ self copy.
	r _ rem representatives.
	m _ aTuple depth. "m = degree(self)+1"
	b _ aTuple representatives.
	bm _ b at: m.
	modulus _ self modulus.
	[(n _ rem depth) >= m] "n = degree(rem)+1"
		whileTrue:
			[| i c |
			i _ n-m+1.
			c _ (r at: n) quotientBy: bm modulo: modulus.
			c = 0 ifTrue: [^ {quo. rem}].
			q at: i put: (q at: i) + c \\ modulus.
			1 to: m do: [:j| r at: j+i-1 put: (r at: j+i-1) - ((b at: j)*c) \\ modulus]].
	^ {quo. rem}! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:42:57'!
polynomialRemainderOfDivisionBy: aTuple
	| rem r bmInv n m b modulus |
	aTuple isZero ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialRemainderOfDivisionBy: argument: aTuple) signal].
	rem _ self copy.
	r _ rem representatives.
	m _ aTuple depth. "m = degree(self)+1"
	bmInv _ (aTuple at: m) inverse representative.
	b _ aTuple representatives.
	modulus _ self modulus.
	[(n _ rem depth) >= m] "n = degree(rem)+1"
		whileTrue:
			[| i c |
			i _ n-m+1.
			c _ (r at: n) * bmInv \\ modulus. "<- is it better to reduce or not reduce here?"
			1 to: m do: [:j| r at: j+i-1 put: (r at: j+i-1) - ((b at: j)*c) \\ modulus]].
	^ rem! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/29/2020 19:43:05'!
polynomialRemainderOfPartialDivisionBy: aTuple
	| rem r n m b bm modulus |
	aTuple isZero ifTrue: [^ (ZeroDivide receiver: self selector: #polynomialRemainderOfPartialDivisionBy: argument: aTuple) signal].
	rem _ self copy.
	r _ rem representatives.
	m _ aTuple depth. "m = degree(self)+1"
	b _ aTuple representatives.
	bm _ b at: m.
	modulus _ self modulus.
	[(n _ rem depth) >= m] "n = degree(rem)+1"
		whileTrue:
			[| i c |
			i _ n-m+1.
			c _ (r at: n) quotientBy: bm modulo: modulus.
			c = 0 ifTrue: [^ rem].
			1 to: m do: [:j| r at: j+i-1 put: (r at: j+i-1) - ((b at: j)*c) \\ modulus]].
	^ rem! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 13:25:10'!
polynomialValue: anElement
	| modulus x answer |
	(self scalars includes: anElement)
		ifFalse: [^ super polynomialValue: anElement].
	modulus _ self modulus.
	x _ anElement representative.
	answer _ 0.
	representatives reverseDo: [:each| answer _ x * answer + each \\ modulus].
	^ self scalars representative: answer! !

!ModularTuple methodsFor: 'operations' stamp: 'len 5/28/2020 14:36:34'!
reversed
	^ self class representatives: representatives reversed parent: parent! !

!ModularTuple methodsFor: 'operations' stamp: 'len 12/3/2016 09:13:11'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ representatives count: [:each| each ~~ 0]! !

!ModularTuple methodsFor: 'testing' stamp: 'len 12/5/2016 17:54:18'!
isZero
	1 to: representatives size do: [:i| (representatives at: i) == 0 ifFalse: [^ false]].
	^ true! !

!ModularTuple methodsFor: 'testing' stamp: 'len 12/30/2021 11:34:38'!
isZeroAt: anInteger
	^ (representatives at: anInteger) = 0! !

!ModularTuple methodsFor: 'testing' stamp: 'len 4/12/2021 12:28:13'!
isZeroFrom: start to: stop
	start to: stop do: [:i| (representatives at: i) == 0 ifFalse: [^ false]].
	^ true! !

!ModularTuple methodsFor: 'private' stamp: 'len 5/28/2020 13:23:21'!
modulus
	^ self scalars modulus! !

!ModularTuple methodsFor: 'private' stamp: 'len 3/31/2016 07:43'!
representatives
	^ representatives! !

!ModularTuple methodsFor: 'private' stamp: 'len 12/1/2016 16:36:25'!
representatives: anArray
	representatives _ anArray
"	representatives do: [:each| (each between: 0 and: modulus) ifFalse: [self halt]]"! !

!ModularTuple class methodsFor: 'instance creation' stamp: 'len 3/25/2021 14:46:18'!
components: anArray parent: aFreeModule
	| n array |
	n _ anArray size.
	array _ self representativesArrayClass new: n.
	1 to: n do: [:i| array at: i put: (anArray at: i) representative].
	^ self representatives: array parent: aFreeModule! !

!ModularTuple class methodsFor: 'instance creation' stamp: 'len 5/28/2020 13:13:06'!
representatives: anArray parent: aFreeModule
	^ self basicNew parent: aFreeModule; representatives: anArray! !

!ModularTuple class methodsFor: 'private' stamp: 'len 3/25/2021 14:46:36'!
representativesArrayClass
	^ Array! !

!ByteModularTuple class methodsFor: 'private' stamp: 'len 3/25/2021 14:47:10'!
representativesArrayClass
	^ ByteArray! !

!WordModularTuple class methodsFor: 'private' stamp: 'len 3/25/2021 14:47:23'!
representativesArrayClass
	^ WordArray! !

!MultivariatePolynomialOld methodsFor: 'accessing' stamp: 'len 12/22/2017 17:51:19'!
bitSize
	self isZero ifTrue: [^ 0].
	^ parts sum: [:each| each key bitSize + each value bitSize]! !

!MultivariatePolynomialOld methodsFor: 'accessing-coefficients' stamp: 'len 1/1/2022 09:39:33'!
at: aMonomial
	aMonomial isInteger ifTrue: [^ self at: (self parent monomials x: self leadingIndeterminate to: aMonomial) ifAbsent: [self scalars zero]].
	^ self at: aMonomial ifAbsent: [self scalars zero]! !

!MultivariatePolynomialOld methodsFor: 'accessing-coefficients' stamp: 'len 6/7/2020 14:34:08'!
initialCoefficient
	^ parts isEmpty ifTrue: [self scalars zero] ifFalse: [parts first value]! !

!MultivariatePolynomialOld methodsFor: 'accessing-coefficients' stamp: 'len 8/18/2019 06:07:09'!
leadingCoefficient
	^ parts isEmpty ifTrue: [self scalars zero] ifFalse: [parts last value]! !

!MultivariatePolynomialOld methodsFor: 'accessing-monomials' stamp: 'len 6/7/2020 14:28:56'!
initialMonomial
	^ parts isEmpty ifFalse: [parts first key]! !

!MultivariatePolynomialOld methodsFor: 'accessing-monomials' stamp: 'len 8/18/2019 06:07:17'!
leadingMonomial
	^ parts isEmpty ifFalse: [parts last key]! !

!MultivariatePolynomialOld methodsFor: 'accessing-terms' stamp: 'len 5/9/2018 15:41:18'!
leadingTerm
	"Answer the first term of the receiver."

	| leadingPart |
	self isZero ifTrue: [^ self].
	leadingPart _ parts last.
	^ self parent coefficient: leadingPart value monomial: leadingPart key! !

!MultivariatePolynomialOld methodsFor: 'accessing-terms' stamp: 'len 4/20/2022 09:53:02'!
tail
	self isZero ifTrue: [^ self].
	^ self class parts: parts allButLast parent: parent! !

!MultivariatePolynomialOld methodsFor: 'comparing' stamp: 'len 4/20/2022 13:03:10'!
= anObject
	^ self == anObject or: [self class = anObject class and: [parts = anObject parts]]! !

!MultivariatePolynomialOld methodsFor: 'comparing' stamp: 'len 12/22/2017 17:51:38'!
hash
	^ parts hash! !

!MultivariatePolynomialOld methodsFor: 'converting' stamp: 'len 4/4/2021 09:49:25'!
lift
	"If the coefficients ring is a quotient R/I, answer a representative of the receiver over R. See >>%."
	^ parent lift parts: (parts collect: [:each| each copy value: each value lift])! !

!MultivariatePolynomialOld methodsFor: 'converting' stamp: 'len 4/20/2022 18:14:45'!
over: aRing
	| newParts |
	self scalars = aRing ifTrue: [^ self].
	newParts _ OrderedCollection new: parts size.
	newParts resetTo: 1.
	parts do: [:each| | newValue |
		(newValue _ aRing !! each value) isZero
			ifFalse: [newParts add: (each copy value: newValue)]].
	^ (self parent over: aRing) parts: newParts! !

!MultivariatePolynomialOld methodsFor: 'copying' stamp: 'len 4/20/2022 09:49:39'!
copy
	^ self class parts: (parts collect: [:each| each copy]) parent: parent! !

!MultivariatePolynomialOld methodsFor: 'enumerating' stamp: 'len 12/22/2017 17:51:42'!
coefficientsDo: aBlock
	parts do: [:each| aBlock value: each value]! !

!MultivariatePolynomialOld methodsFor: 'enumerating' stamp: 'len 4/20/2022 09:51:03'!
monomialsApply: aBlock
	| dictionary newParts |
	dictionary _ Dictionary new: parts size.
	parts do: [:each|
		dictionary at: (aBlock value: each key) add: each value].
	newParts _ OrderedCollection new: dictionary size.
	newParts resetTo: 1.
	dictionary associationsDo: [:each| each value isZero ifFalse: [newParts add: each]].
	newParts sort.
	^ self class parts: newParts parent: parent! !

!MultivariatePolynomialOld methodsFor: 'enumerating' stamp: 'len 1/2/2022 10:04:47'!
monomialsDo: aBlock
	"Iterate over the support of the receiver, i.e. the monomials with nonzero coefficient."
	parts do: [:each| aBlock value: each key]! !

!MultivariatePolynomialOld methodsFor: 'enumerating' stamp: 'len 12/24/2017 20:10:01'!
partsDo: aBlock
	parts do: aBlock! !

!MultivariatePolynomialOld methodsFor: 'operations' stamp: 'len 4/26/2022 22:03:37'!
+ anElement
	| newAssociations |
	(self parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	newAssociations _ OrderedCollection new: parts size + anElement parts size.
	newAssociations resetTo: 1.
	self sum: parts with: anElement parts into: newAssociations.
	^ self class parts: newAssociations parent: parent! !

!MultivariatePolynomialOld methodsFor: 'operations' stamp: 'len 1/13/2022 09:50:18'!
\\ anObject
	"Answer the remainder of the division of the receiver by the argument."
	(self scalars includes: anObject) ifTrue: [^ self apply: [:each| each \\ anObject]].
	^ self reductionFullBy: (anObject isCollection ifTrue: [anObject] ifFalse: [{anObject}])! !

!MultivariatePolynomialOld methodsFor: 'operations' stamp: 'len 4/20/2022 18:15:34'!
apply: aMorphism
	"Answer a new polynomial with coefficients obtainted by applying aMorphism to the coefficients of the receiver. This map is a ring homomorphism from the scalars ring.
	If aMorphism is a block we assume the parent doesn't change, i.e. the block must evaluate to values within the scalars ring of the receiver."

	| newParts |
	newParts _ OrderedCollection new: parts size.
	newParts resetTo: 1.
	parts do: [:each| | newValue |
		(newValue _ aMorphism value: each value) isZero
			ifFalse: [newParts add: (each copy value: newValue)]].
	^ (aMorphism isBlock ifTrue: [parent] ifFalse: [parent over: aMorphism codomain]) parts: newParts! !

!MultivariatePolynomialOld methodsFor: 'operations' stamp: 'len 1/13/2022 09:54:22'!
divisionBy: anArray
	self ordering isGlobal ifFalse: [^ self notYetImplemented].
	^ MultivariateDivision divide: self by: (anArray isCollection ifTrue: [anArray] ifFalse: [{anArray}])! !

!MultivariatePolynomialOld methodsFor: 'operations' stamp: 'len 4/20/2022 09:52:02'!
negated
	"Answer the additive inverse of the receiver."
	^ self class parts: (parts collect: [:each| each key -> each value negated]) parent: parent! !

!MultivariatePolynomialOld methodsFor: 'operations' stamp: 'len 4/20/2022 09:53:13'!
truncatedTo: anInteger
	self degree < anInteger ifTrue: [^ self].
	^ self class parts: (parts select: [:each| each key degree < anInteger]) parent: parent! !

!MultivariatePolynomialOld methodsFor: 'testing' stamp: 'len 12/22/2017 17:49:16'!
isZero
	^ parts isEmpty! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 12/22/2017 17:47:53'!
at: aMonomial add: aNumber
	parts isEmpty ifTrue: [aNumber isZero ifFalse: [parts add: aMonomial -> aNumber]. ^ self].
	parts
		findBinaryIndex: [:each| (each key compare: aMonomial) negated]
		do: [:index| | association newValue |
			association _ parts at: index.
			(newValue _ association value + aNumber) isZero
				ifTrue: [parts removeIndex: index]
				ifFalse: [association value: newValue "associations at: index put: aMonomial -> newValue"]]
		ifNone: [:low :high| aNumber isZero ifFalse: [parts add: aMonomial -> aNumber beforeIndex: high]]! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 12/22/2017 17:51:33'!
at: aMonomial ifAbsent: exceptionBlock
	^ parts findBinary: [:each| (each key compare: aMonomial) negated] do: [:each| each value] ifNone: [:high :low| exceptionBlock value]! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 12/21/2017 17:44:14'!
at: key ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v _ self at: key ifAbsent: [ ^ nil ].
	^ presentBlock value: v! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 12/22/2017 17:49:08'!
at: aMonomial put: aNumber
	parts isEmpty ifTrue: [aNumber isZero ifFalse: [parts add: aMonomial -> aNumber]. ^ self].
	parts
		findBinaryIndex: [:each| (each key compare: aMonomial) negated]
		do: [:index| aNumber isZero ifTrue: [parts removeIndex: index] ifFalse: [(parts at: index) value: aNumber]]
		ifNone: [:low :high| aNumber isZero ifFalse: [parts add: aMonomial -> aNumber beforeIndex: high]].
	^ aNumber! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 4/20/2022 09:50:07'!
dividedByMonomial: aMonomial
	"The monomial ordering is preserved by this operation."
	^ self class parts: (parts collect: [:each| each key / aMonomial -> each value]) parent: parent! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 4/20/2022 09:50:19'!
dividedByScalar: anElement
	^ self class parts: (parts collect: [:each| each copy value: each value / anElement]) parent: parent! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 4/20/2022 09:51:25'!
multipliedBy: aPolynomial
	| dictionary newAssociations |
	dictionary _ Dictionary new: parts size + aPolynomial parts size.
	parts do: [:each|
		aPolynomial parts do: [:other|
			dictionary at: each key * other key add: each value * other value]].
	newAssociations _ OrderedCollection new: dictionary size.
	newAssociations resetTo: 1.
	dictionary associationsDo: [:each| each value isZero ifFalse: [newAssociations add: each]].
	newAssociations sort.
	^ self class parts: newAssociations parent: parent! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 4/20/2022 09:51:40'!
multipliedByMonomial: aMonomial
	"The monomial ordering is preserved by this operation."
	^ self class parts: (parts collect: [:each| each key * aMonomial -> each value]) parent: parent! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 4/20/2022 09:51:52'!
multipliedByScalar: aNumber
	| newAssociations |
	aNumber isZero ifTrue: [^ parent zero].
"	parent scalars isIntegralDomain ifTrue: [^ parent parts: (parts collect: [:each| each copy value: each value * aNumber])]."
	newAssociations _ OrderedCollection new: parts size.
	newAssociations resetTo: 1.
	parts do: [:each| | newValue |
		(newValue _ each value * aNumber) isZero
			ifFalse: [newAssociations add: (each copy value: newValue)]].
	^ self class parts: newAssociations parent: parent! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 12/22/2017 17:52:34'!
parts
	^ parts! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 4/20/2022 10:38:18'!
parts: aCollection parent: aPolynomialRing
	parts _ aCollection.
	parent _ aPolynomialRing! !

!MultivariatePolynomialOld methodsFor: 'private' stamp: 'len 12/21/2017 18:38:44'!
sum: someAssociations with: otherAssociations into: newAssociations
	| stream1 stream2 |
	stream1 _ someAssociations readStream.
	stream2 _ otherAssociations readStream.
	[stream1 atEnd or: [stream2 atEnd]]
		whileFalse:
			[| cmp |
			cmp _ stream1 peek key compare: stream2 peek key.
			cmp < 0
				ifTrue: [newAssociations add: stream1 next copy]
				ifFalse: [cmp > 0
					ifTrue: [newAssociations add: stream2 next copy]
					ifFalse:
						[| a b newValue |
						a _ stream1 next.
						b _ stream2 next.
						(newValue _ a value + b value) isZero
							ifFalse: [newAssociations add: (a copy value: newValue)]]]].
	[stream1 atEnd] whileFalse: [newAssociations add: stream1 next copy].
	[stream2 atEnd] whileFalse: [newAssociations add: stream2 next copy].
	^ newAssociations! !

!MultivariatePolynomialOld class methodsFor: 'instance creation' stamp: 'len 4/20/2022 09:49:05'!
parts: aCollection parent: aPolynomialRing
	^ self basicNew parts: aCollection parent: aPolynomialRing! !

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
∑ aTuple
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
	self simpleRoots do: [:each| answer _ answer ∑ (O reflectionAt: each)].
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
	V _ Ñ ^ 3.
	roots _ Set new.
	1 to: V dimension do: [:i|
		1 to: i-1 do: [:j|
			roots add: (V e: i) - (V e: j); add: (V e: j) - (V e: i)]].
	^ RootSystem on: V roots: roots! !

!RootSystem class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example2
	"This is a normalized root system in R^n. The Weyl group associated to it is the hyperoctahedral group."
	| V roots |
	V _ Ö ^ 3.
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
	V _ Ñ ^ 3.
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
	^ self A: Ñ ^ 2! !

!RootSystem class methodsFor: 'instance creation' stamp: 'len 5/23/2016 05:00'!
on: aVectorSpace roots: aCollection
	^ self new ambient: aVectorSpace; roots: aCollection! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem1
	| V roots |
	V _ Ñ ^ 3.
	roots _ Set new.
	1 to: 3 do: [:i| 1 to: i-1 do: [:j| |r| roots add: (r _ (V x: i) - (V x: j)); add: r negated]].
	^ RootSystem on: V roots: roots! !

!RootSystemTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
rootSystem2
	| V roots |
	V _ Ñ ^ 3.
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
