'From Cuis 6.0 [latest update: #5801] on 1 June 2023 at 12:58:48 pm'!
'Description '!
!provides: 'Codes' 1 10!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: #Codes!


!classDefinition: #LinearCode category: #Codes!
Structure subclass: #LinearCode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Codes'!
!classDefinition: 'LinearCode class' category: #Codes!
LinearCode class
	instanceVariableNames: ''!


!LinearCode commentStamp: '<historical>' prior: 0!
Linear codes over rings or fields.

In coding theory, a linear code is an error-correcting code in which any linear combination of codewords is also a codeword. A linear code of length n over a ring R is a proper subset of the free module R^n that forms a submodule.
!

!LinearCode methodsFor: 'accessing' stamp: 'len 10/11/2022 10:55:26'!
completeWeightEnumerator
	"Answer the complete weight enumerator polynomial."
	| F P |
	F _ self scalars asArray.
	P _ ℤ polynomialsIn: F size.
	^ self sum: [:each| P monomial: (P monomials exponents: (F collect: [:i| each occurrencesOf: i]))]! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 17:34:11'!
diameter
	"Answer the maximum weight of the codewords of the receiver."
	^ self max: [:each| each weight]! !

!LinearCode methodsFor: 'accessing' stamp: 'len 5/12/2016 07:19'!
dimension
	^ self rank! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/10/2020 06:39:38'!
generatorMatrix
	"Answer a generator matrix of the receiver.
	Note that the code is the span of the columns of the generator matrix, contrary to the more common convention to define it as the span of the rows. This is in order to be consistent with modules and matrix left action on tuples."
	^ self asModule generatorMatrix! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 18:11:45'!
generatorPolynomial
	"If the receiver is a cyclic code, answer the generator polynomial."
	^ self propertyAt: #generatorPolynomial ifAbsentPut: [self computeGeneratorPolynomial]! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 18:13:34'!
length
	"Answer the length of a codeword."
	^ self space rank! !

!LinearCode methodsFor: 'accessing' stamp: 'len 12/28/2016 11:32:15'!
minimumDistance
	"Answer the minimum Hamming distance between codewords.
	If the minimum distance is d, the code can correct (d-1)/2 errors."
	^ self propertyAt: #minimumDistance ifAbsentPut: [self computeMinimumDistance]! !

!LinearCode methodsFor: 'accessing' stamp: 'len 5/31/2023 16:51:14'!
parityCheckMatrix
	"Answer a parity check matrix for the receiver. This is a matrix P such that P*x = 0 iff x is a word of the receiver, i.e. the receiver as module is the kernel of the matrix."
	^ self dual generatorMatrix transpose! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/3/2020 07:14:56'!
parityCheckPolynomial
	"If the receiver is a cyclic code, answer the parity check polynomial.
	If the generator polynomial of the receiver is g, the parity check polynomial is a polynomial h such that gh = x^n - 1."
	| g |
	g _ self generatorPolynomial.
	^ g parent x ^ self length - 1 / g! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 12:12:12'!
rank
	^ self asModule rank! !

!LinearCode methodsFor: 'accessing' stamp: 'len 4/3/2016 23:08'!
rate
	^ (self size log: 2) / self length! !

!LinearCode methodsFor: 'accessing' stamp: 'len 2/15/2016 20:33'!
relativeDistance
	^ self minimumDistance / self length! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 18:13:48'!
scalars
	^ self space scalars! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 12:10:21'!
size
	"Answer the number of codewords."
	^ self asModule size! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 12:10:11'!
space
	"Answer the ambient vector space or free module."
	^ self asModule ambient! !

!LinearCode methodsFor: 'accessing' stamp: 'len 8/2/2020 17:15:05'!
weightDistribution
	| answer |
	answer _ Bag new.
	self do: [:each| answer add: each weight].
	^ answer! !

!LinearCode methodsFor: 'accessing' stamp: 'len 1/1/2016 00:06'!
weightDistribution: aCodeword
	"Answer the weight distribution wrt aCodeword."
	self notYetImplemented! !

!LinearCode methodsFor: 'accessing' stamp: 'len 7/31/2020 08:47:05'!
weightEnumerator
	"Answer the Hamming weight enumerator polynomial."
	| P x y weights answer n |
	P _ ℤ polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	answer _ P zero.
	n _ self length.
	weights _ self weightDistribution.
	weights asSet do: [:i| answer _ x ^ (n - i) * (y ^ i) * (weights occurrencesOf: i) + answer].
	^ answer! !

!LinearCode methodsFor: 'comparing' stamp: 'len 8/2/2020 22:28:28'!
<= aCode
	^ self asModule <= aCode asModule! !

!LinearCode methodsFor: 'comparing' stamp: 'len 10/2/2022 17:41:37'!
= anObject
	^ (anObject isKindOf: LinearCode) and: [self asModule = anObject asModule]! !

!LinearCode methodsFor: 'comparing' stamp: 'len 8/2/2020 12:11:54'!
hash
	^ self asModule hash! !

!LinearCode methodsFor: 'converting' stamp: 'len 8/3/2020 07:16:22'!
asIdeal
	"A linear code is cyclic if and only if it is an ideal in R[x]/<x^n - 1>."
	| g |
	g _ self generatorPolynomial.
	^ g parent / {g parent x ^ self length - 1} * g! !

!LinearCode methodsFor: 'converting' stamp: 'len 5/3/2021 10:56:58'!
asLattice
	"A linear code in a finite field or ring can be lifted to a lattice.
	This is called the Construction A lifting."
	| B E H |
	B _ self generatorMatrix lift.
	E _ B ⊔ (B codomain endomorphisms scalar: self scalars characteristic).
	H _ E transpose HNF over: ℚ.
	^ Lattice on: H domain innerProduct basis: (H rows select: [:each| each isZero not])! !

!LinearCode methodsFor: 'converting' stamp: 'len 8/2/2020 12:08:46'!
asModule
	^ self propertyAt: #asModule! !

!LinearCode methodsFor: 'converting' stamp: 'len 8/2/2020 12:06:21'!
asVectorSpace
	self scalars isField ifFalse: [^ DomainError signal].
	^ self asModule! !

!LinearCode methodsFor: 'enumerating' stamp: 'len 8/2/2020 12:11:08'!
do: aBlock
	self asModule do: aBlock! !

!LinearCode methodsFor: 'graphs' stamp: 'len 10/11/2022 10:54:01'!
distanceGraph: d
	"Answer the graph of the codewords of the receiver, with two codewords adjacent if their Hamming distance is d."
	| G |
	G _ Graph unordered.
	self do: [:x| self do: [:y| (x-y) weight = d ifTrue: [G addEdgeFrom: x to: y]]].
	^ G! !

!LinearCode methodsFor: 'graphs' stamp: 'len 4/5/2018 15:27:59'!
tannerGraph
	| matrix G |
	matrix _ self parityCheckMatrix.
	G _ Graph unordered.
	1 to: matrix height do: [:i|
		1 to: matrix width do: [:j|
			(matrix at: i@j) isZero not
				ifTrue:
					[| subcode digit |
					subcode _ matrix rowAt: i.
					digit _ matrix columnAt: j.
					G addEdgeFrom: subcode to: digit]]].
	^ G! !

!LinearCode methodsFor: 'morphisms' stamp: 'len 4/20/2016 20:56'!
automorphisms
	"Answer the subgroup of Perm(F^k) (permutation of coordinates) that leave the receiver invariant.
	The automorphisms of a code and it's dual are the same."
	self notYetImplemented! !

!LinearCode methodsFor: 'morphisms' stamp: 'len 2/15/2016 20:26'!
isometries
	"Answer the subgroup of Iso(F^k) that leave the receiver invariant."
	self notYetImplemented! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/2/2020 22:28:01'!
+ aCode
	^ self class on: self asModule + aCode asModule! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/3/2020 20:35:51'!
× aCode
	"Answer the direct product of the receiver and the argument."
	^ self class generatorMatrix: self generatorMatrix ⊗ aCode generatorMatrix! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/3/2020 20:35:57'!
⊕ aCode
	"Answer the direct sum of the receiver and the argument."
	^ self class generatorMatrix: self generatorMatrix ⊕ aCode generatorMatrix! !

!LinearCode methodsFor: 'operations' stamp: 'len 11/23/2022 21:26:45'!
augmented
	"Answer a code like the receiver but including the all-ones vector amoung its codewords."
	| ones |
	ones _ self space fill: [:i| self scalars one].
	(self includes: ones) ifTrue: [^ self].
	^ self class on: self asModule + (self space span: {ones})! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/2/2020 17:59:32'!
complement
	"If the receiver is a subcode C1 <= C, answer C2 such that C = C1 + C2."
	self notYetImplemented! !

!LinearCode methodsFor: 'operations' stamp: 'len 11/5/2022 14:20:17'!
dual
	"Answer the dual or orthogonal code of the receiver."
	^ self class on: self asModule complement! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/11/2020 08:42:37'!
extended
	^ self class on: (self space scalars ^ (self length + 1) span: (self asModule generators collect: [:each| each, each sum negated]))! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/12/2022 13:12:32'!
hull
	"The hull of a linear code is the intersection of the code with its dual."
	^ self ∩ self dual! !

!LinearCode methodsFor: 'operations' stamp: 'len 11/5/2022 14:20:28'!
span: aCollection
	"Answer the subcode spanned by the elements in aCollection."
	^ (self class on: (self asModule span: aCollection)) ambient: self! !

!LinearCode methodsFor: 'operations' stamp: 'len 8/12/2022 13:12:21'!
∩ aCode
	^ self class on: self asModule ∩ aCode asModule! !

!LinearCode methodsFor: 'printing' stamp: 'len 7/31/2020 08:46:28'!
printOn: aStream
	aStream nextPut: $[; print: self length; nextPut: $,; print: self rank; nextPut: $,; print: self minimumDistance; nextPut: $].
	aStream isText
		ifTrue: [aStream nextPutAll: self scalars size printString sub]
		ifFalse: [aStream nextPut: $-; print: self scalars size]! !

!LinearCode methodsFor: 'random' stamp: 'len 8/2/2020 12:11:03'!
atRandom: aRandom bits: bitSize
	^ self asModule atRandom: aRandom bits: bitSize! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/2/2020 12:11:59'!
includes: aCodeword
	^ self asModule includes: aCodeword! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/3/2020 07:15:40'!
includesPolynomial: aPolynomial
	| g |
	g _ self generatorPolynomial.
	^ (self parityCheckPolynomial * aPolynomial \\ (g parent x ^ self length - 1)) isZero! !

!LinearCode methodsFor: 'testing' stamp: 'len 5/6/2022 06:05:57'!
isCyclic
	"A code is called a cylic code if for each codeword (c1,...,cn) it also contains the cyclic right shifted word (cn,c1,c2,...,cn-1), i.e. the code is invariant under cylic shifts."
	self asModule generators do: [:each| (self includes: each >>> 1) ifFalse: [^ false]].
	^ true

"alternative for polynomial codes:
	| g n x |
	n _ self length.
	g _ self generatorPolynomial.
	x _ g parent x.
	^ g | (x ^ n - x parent one)
"! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/2/2020 17:10:57'!
isDoublyEven
	^ self allSatisfy: [:each| each weight \\ 4 = 0]! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/2/2020 17:27:32'!
isEquidistant
	| distance |
	self do: [:x|
		self do: [:y|
			| d |
			x ~= y ifTrue: [d _ (x-y) weight. (distance ifNil: [distance _ d]) = d ifFalse: [^ false]]]].
	^ true! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/2/2020 17:10:15'!
isEven
	^ self allSatisfy: [:each| each weight even]! !

!LinearCode methodsFor: 'testing' stamp: 'len 5/12/2016 07:30'!
isMaximumDistanceSeparable
	"Any linear [n,k,d] code satisfies k + d <= n + 1 (Singleton bound). If a code satisfies k + d = n + 1 it's called Maximum Distance Separable (MDS)."
	| n k d |
	n _ self length.
	k _ self rank.
	d _ self minimumDistance.
	^ k + d = (n + 1)! !

!LinearCode methodsFor: 'testing' stamp: 'len 7/31/2020 08:46:03'!
isSelfDual
	self length even ifFalse: [^ false].
	self dimension = (self length / 2) ifFalse: [^ false].
	^ self = self dual! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/2/2020 12:12:06'!
isSelfOrthogonal
	^ self asModule <= self dual asModule! !

!LinearCode methodsFor: 'testing' stamp: 'len 8/2/2020 12:05:56'!
isTrivial
	^ self asModule isTrivial! !

!LinearCode methodsFor: 'private' stamp: 'len 8/2/2020 17:14:44'!
computeMinimumDistance
	| answer |
	answer _ Infinity positive.
	self do: [:each| each isZero ifFalse: [answer _ answer min: each weight]].
	^ answer! !

!LinearCode methodsFor: 'private' stamp: 'len 8/2/2020 12:24:36'!
submodule: aModule
	self propertyAt: #asModule put: aModule! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/10/2020 06:27:00'!
G11
	"Answer the perfect ternary Golay code [11,6,5]."
	^ self generatorMatrix:
		(Matrix columns: {
			(1,0,0,0,0,0, 1,1,1,1,1) % 3.
			(0,1,0,0,0,0, 0,1,2,2,1) % 3.
			(0,0,1,0,0,0, 1,0,1,2,2) % 3.
			(0,0,0,1,0,0, 2,1,0,1,2) % 3.
			(0,0,0,0,1,0, 2,2,1,0,1) % 3.
			(0,0,0,0,0,1, 1,2,2,1,0) % 3})! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/10/2020 06:26:55'!
G12
	"Answer the extended ternary Golay code [12,6,6]."
	^ self generatorMatrix:
		(Matrix columns: {
			(1,0,0,0,0,0, 0,1,1,1,1,1) % 3.
			(0,1,0,0,0,0, 1,0,1,2,2,1) % 3.
			(0,0,1,0,0,0, 1,1,0,1,2,2) % 3.
			(0,0,0,1,0,0, 1,2,1,0,1,2) % 3.
			(0,0,0,0,1,0, 1,2,2,1,0,1) % 3.
			(0,0,0,0,0,1, 1,1,2,2,1,0) % 3})! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/2/2020 18:29:21'!
G23
	"Answer the perfect binary Golay code [23,12,7]."
	| g |
	g _ (ℤ/2) polynomials !! [:x| x^11 + (x^10) + (x^6) + (x^5) + (x^4) + (x^2) + 1].
	^ self new: 23 generatorPolynomial: g! !

!LinearCode class methodsFor: 'examples' stamp: 'len 10/5/2022 18:39:48'!
G24
	"Answer the extended binary Golay code [24,12,8].
	Its generator matrix is I|A, where I is the 12x12 identity matrix and A is the complement of the adjacency matrix of the icosahedron."
	| F A |
	F _ ℤ/2.
	A _ Graph icosahedron adjacencyMatrix: (1 to: 12).
	A _ (A collect: [:each| each = 0 ifTrue: [1] ifFalse: [0]]) apply: ℤ → F.
	^ self generatorMatrix: A identity ⊓ A! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/10/2020 06:26:04'!
H7
	"Answer the [7,4,3] Hamming code, which is the binary cyclic code of length 7 with generator polynomial 1 + x + x^3."
	| generators c |
	generators _ OrderedCollection with: (c _ (1,1,0,1,0,0,0) % 2).
	3 timesRepeat: [generators add: (c _ c >> 1)].
	^ self generatorMatrix: (Matrix columns: generators)! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/2/2020 18:28:58'!
H7p
	"Answer the Hamming code [7,4,3], from a generator polynomial."
	^ self new: 7 generatorPolynomial: (ℤ/2) polynomials !! [:x| x^3 + x + 1]! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/10/2020 06:25:55'!
H8
	"Answer the extended Hamming code [8,4,4]."
	^ self generatorMatrix:
		(Matrix columns: {
			(1,0,0,0, 0,1,1,1) % 2.
			(0,1,0,0, 1,0,1,1) % 2.
			(0,0,1,0, 1,1,0,1) % 2.
			(0,0,0,1, 1,1,1,0) % 2})! !

!LinearCode class methodsFor: 'examples' stamp: 'len 5/12/2023 21:10:29'!
RM: parameters
	"Answer the Reed-Muller code RM(r, m).
	First we build the 2^m Sierpinski matrix by taking the m-fold Kronecher product of the 2x2 matrix
		[1 0]
		[1 1]
	over ℤ/2ℤ. Then we construct the RM(r,m) code as the span of the rows with Hamming weight >= 2^(m-r)."
	| r m F S d |
	r _ parameters at: 1.
	m _ parameters at: 2.
	F _ (ℤ/2^2) endomorphisms !! #(1 0 1 1).
	S _ (ℤ/2^0) endomorphisms zero.
	m timesRepeat: [S _ S ⊗ F].
	d _ 2^(m-r).
	^ self on: (S domain span: (S rows select: [:each| each weight >= d]))! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/10/2020 06:25:49'!
hadamard8
	"Answer the [8,3,4] Hadamard code."
	^ self generatorMatrix:
		(Matrix columns: {
			(0,0,0,0,1,1,1,1) % 2.
			(0,0,1,1,0,0,1,1) % 2.
			(0,1,0,1,0,1,0,1) % 2})! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/10/2020 06:25:43'!
simplex
	"Answer the [7,3,4] simplex code, which is the dual code of the Hamming code."
	^ self generatorMatrix:
		(Matrix columns: {
			(0,0,0,1,1,1,1) % 2.
			(0,1,1,0,0,1,1) % 2.
			(1,0,1,0,1,0,1) % 2})! !

!LinearCode class methodsFor: 'examples' stamp: 'len 8/2/2020 12:25:10'!
tetracode
	"Answer the tetracode, a self dual code of minimum distance 3."
	^ self on: ((ℤ/3)^4 span: {(1,1,1,0) % 3. (0,1,2,1) % 3})! !

!LinearCode class methodsFor: 'instance creation' stamp: 'len 8/10/2020 06:39:05'!
generatorMatrix: aMatrix
	"Answer a linear code with generator matrix aMatrix.
	Note that the code is the span of the columns of the generator matrix, contrary to the more common convention to define it as the span of the rows. This is in order to be consistent with modules and matrix left action on tuples."
	^ self on: aMatrix image! !

!LinearCode class methodsFor: 'instance creation' stamp: 'len 8/2/2020 18:27:51'!
generatorPolynomial: aPolynomial
	^ self new: aPolynomial degree generatorPolynomial: aPolynomial! !

!LinearCode class methodsFor: 'instance creation' stamp: 'len 8/2/2020 18:31:06'!
new: n generatorPolynomial: aPolynomial
	| V W subspace |
	V _ aPolynomial parent filtrationAt: n - 1.
	W _ aPolynomial parent filtrationAt: n - aPolynomial degree - 1.
	subspace _ V span: (W asArray collect: [:f| aPolynomial * f]).
	^ (self on: subspace)
		propertyAt: #generatorPolynomial put: aPolynomial;
		yourself! !

!LinearCode class methodsFor: 'instance creation' stamp: 'len 8/2/2020 12:25:23'!
on: aModule
	^ self new submodule: aModule! !

!Module methodsFor: '*codes' stamp: 'len 10/2/2022 17:42:37'!
asLinearCode
	^ LinearCode on: self! !
