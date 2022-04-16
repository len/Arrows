'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 1:26:56 pm'!
'Description '!
!provides: 'Tests-Lattices' 1 0!
!requires: 'Lattices' 1 0 nil!
SystemOrganization addCategory: #'Tests-Lattices'!


!classDefinition: #CodesTest category: #'Tests-Lattices'!
TestCase subclass: #CodesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Lattices'!
!classDefinition: 'CodesTest class' category: #'Tests-Lattices'!
CodesTest class
	instanceVariableNames: ''!

!classDefinition: #LatticeTest category: #'Tests-Lattices'!
TestCase subclass: #LatticeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Lattices'!
!classDefinition: 'LatticeTest class' category: #'Tests-Lattices'!
LatticeTest class
	instanceVariableNames: ''!


!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:24:05'!
newBinaryCode
	3 atRandom = 1 ifTrue: [| r m | m _ (1 to: 4) atRandom. r _ (0 to: m) atRandom. ^ Code RM: {r. m}].
	^ Code perform: #(H7 H8 G23 G24 hadamard8 simplex) atRandom! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:39:35'!
newCode
	^ 3 atRandom = 1 ifTrue: [self newTernaryCode] ifFalse: [self newBinaryCode]! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:24:54'!
newTernaryCode
	^ Code perform: #(G11 G12 tetracode) atRandom! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:54:45'!
testAugmentedCode
	| C A |
	C _ self newCode.
	A _ C augmented.
	self assert: (A includes: (A space filling: [:i| 1])).
	(C includes: (C space filling: [:i| 1])) ifTrue: [self assert: C = A]! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:39:56'!
testConstructionADeterminant
	| C L |
	C _ self newCode.
	L _ C asLattice.
	self assert: L determinant = (C scalars characteristic ^ (C length - C rank))! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:39:45'!
testConstructionADual
	| C |
	C _ self newCode.
	self assert: C asLattice dual = C dual asLattice! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:28:39'!
testDirectSumCode
	| C1 C2 C x y z |
	C1 _ self newBinaryCode.
	C2 _ self newBinaryCode.
	C _ C1 ê C2.
	x _ C1 atRandom.
	y _ C2 atRandom.
	self assert: (C includes: x, y).
	z _ C atRandom.
	self assert: (C1 includes: (z copyFrom: 1 to: C1 length)).
	self assert: (C2 includes: (z copyFrom: C1 length + 1 to: C length))! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testDual
	| C |
	C _ Code on: (É/2^5 span: {(0,0,0,0,0) % 2. (1,1,1,1,1) % 2}).
	self assert: C dual size = (C space size / C size).
	C dual do: [:v|
		"check each codeword of the dual has an even number of 1's, because it must be perpendicular to 11111 (and 00000 of course)."
		self assert: (v count: [:each| each = (1 % 2)]) even]
! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:55:28'!
testExtendedCode
	| C E |
	C _ self newCode.
	E _ C extended.
	self assert: (E asModule generators allSatisfy: [:each| each sum isZero])! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:45:14'!
testGolay11
	| C |
	C _ Code G11.
	self assert: C length = 11.
	self assert: C rank = 6.
	self assert: C minimumDistance = 5! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:45:37'!
testGolay12
	| C |
	C _ Code G12.
	self assert: C length = 12.
	self assert: C rank = 6.
	self assert: C minimumDistance = 6! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:47:05'!
testGolay23
	| C |
	C _ Code G23.
	self assert: C length = 23.
	self assert: C rank = 12.
	self assert: C minimumDistance = 7.
"	self assert: C isPerfect"! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:48:00'!
testGolay24
	| C W x y |
	C _ Code G24.
	self assert: C length = 24.
	self assert: C rank = 12.
	self assert: C minimumDistance = 8.
	self assert: C isSelfDual.
	W _ C weightEnumerator.
	x _ W parent x.
	y _ W parent y.
	self assert: W = (x^24 + (x^16*(y^8)*759) + (x^12*(y^12)*2576) + (x^8*(y^16)*759) + (y^24)) "typo in neil's video 22:20"! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:59:37'!
testHamming743
	| H7 |
	H7 _ Code H7.
	self assert: H7 isCyclic.
	self assert: H7 length = 7.
	self assert: H7 rank = 4.
	self assert: H7 minimumDistance = 3.
	self assert: H7 weightDistribution = #(0 3 3 3 3 3 3 3 4 4 4 4 4 4 4 7) asBag.
	self assert: H7 dual minimumDistance = 4! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:59:24'!
testHamming743p
	| H7 |
	H7 _ Code H7p.
	self assert: H7 isCyclic.
	self assert: H7 length = 7.
	self assert: H7 rank = 4.
	self assert: H7 minimumDistance = 3.
	self assert: H7 weightDistribution = #(0 3 3 3 3 3 3 3 4 4 4 4 4 4 4 7) asBag.
	self assert: H7 dual minimumDistance = 4! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 19:00:05'!
testHamming844
	| C P x y |
	C _ Code H8.
	self assert: C isSelfDual.
	self assert: C isSelfOrthogonal.
	self assert: C length = 8.
	self assert: C rank = 4.
	self assert: C minimumDistance = 4.
	self assert: C dual minimumDistance = 4.
	P _ É polynomialsIn: 2.
	x _ P x. y _ P y.
	self assert: C weightEnumerator = (x^8 + (x*y^4*14) + (y^8))! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testMacWilliamsTheorem
	"(MacWilliams, 1962) the theorem relates the weight enumerator of a linear code to the weight enumerator of its dual."
	"From youtube, Neil Sloane 'Error Correcting Codes, Group Theory and Invariant Theory, Part 1' 20:30."
	| C W Wperp x y |
	C _ Code H7.
	W _ C weightEnumerator.
	Wperp _ C dual weightEnumerator.
	x _ Wperp parent x.
	y _ Wperp parent y.
	self assert: Wperp = ((W value: {x+y. x-y}) / C size)! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/10/2020 06:35:48'!
testParityCheckMatrix
	| C G P |
	C _ self newCode.
	G _ C generatorMatrix.
	P _ C parityCheckMatrix.
	self assert: (P * G) isZero.
	self assert: (P * C atRandom) isZero! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/10/2020 06:34:49'!
testParityCheckMatrix2
	| H7 M |
	H7 _ Code H7.
	M _ H7 parityCheckMatrix.
	self assert: (M * H7 atRandom) isZero! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 09:25:14'!
testProductCode
	| C1 C2 C |
	C1 _ self newBinaryCode.
	C2 _ self newBinaryCode.
	C _ C1 ◊ C2.
	self assert: C length = (C1 length * C2 length).
	self assert: C dimension = (C1 dimension * C2 dimension)! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 07:02:46'!
testReedMuller
	| m r C |
	m _ (1 to: 4) atRandom.
	r _ (0 to: m) atRandom.
	C _ Code RM: {r. m}.
	C isTrivial ifTrue: [^ self].
	self assert: C length = (2^m).
	self assert: C minimumDistance = (2^(m-r)).
	self assert: C dimension = ((0 to: r) sum: [:k| m choose: k])! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 06:34:39'!
testReedMuller2
	| m i j Ci Cj |
	m _ (0 to: 4) atRandom.
	j _ (0 to: m) atRandom.
	i _ (0 to: j) atRandom.
	Ci _ Code RM: {i. m}.
	Cj _ Code RM: {j. m}.
	self assert: Ci <= Cj! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 06:49:54'!
testReedMuller3
	| m C |
	m _ (0 to: 4) atRandom.
	C _ Code RM: {m. m}.
	self assert: C dual isTrivial! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 06:51:28'!
testReedMuller4
	| m r C D |
	m _ (1 to: 4) atRandom.
	r _ (0 to: m-1) atRandom.
	C _ Code RM: {r. m}.
	D _ Code RM: {m-r-1. m}.
	self assert: C dual = D! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testSelfDual
	| F C |
	F _ É/2.
	"{00,11} is the smallest self-dual code"
	C _ Code on: (F^2 span: {(0,0) % 2. (1,1) % 2}).
	self assert: C isSelfDual.
	self assert: C isSelfOrthogonal.
	"TODO: test other examples of self-dual codes: H8, G24"! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 5/12/2016 07:28'!
testSingletonBound: C
	"Any linear [n,k,d] code satisfies k + d <= n + 1."
	| n k d |
	n _ C length.
	k _ C rank.
	d _ C minimumDistance.
	self assert: k + d <= (n + 1).! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testSingletonBoundH7
	"Any linear [n,k,d] code satisfies k + d <= n + 1."
	self testSingletonBound: Code H7! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testSingletonBoundTetracode
	"Any linear [n,k,d] code satisfies k + d <= n + 1."
	self testSingletonBound: Code tetracode! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testTetracode
	| C P x y |
	C _ Code on: (É/3 ^ 4 span: {(1,1,1,0) % 3. (0,1,2,1) % 3}).
	self assert: C isSelfDual.
	self assert: C minimumDistance = 3.
	P _ É polynomialsIn: #(x y).
	x _ P x.
	y _ P y.
	self assert: C weightEnumerator = (x^4 + (x*(y^3)*8))! !

!CodesTest methodsFor: 'as yet unclassified' stamp: 'len 8/2/2020 18:35:34'!
testWeightEnumerator
	"From youtube, Neil Sloane 'error correcting codes, group theory and invariant theory part 1' 19:30."
	| F C w x y |
	F _ É/2.
	C _ Code on: (F^5 span: {(0,0,0,0,0) % 2. (1,1,1,1,1) % 2}).
	w _ C weightEnumerator.
	x _ w parent x.
	y _ w parent y.
	self assert: w = (x^5 + (y^5)).
	self assert: C dual weightEnumerator = (x^5 + (x^3*(y^2)*10) + (y^4*x*5))! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/31/2020 18:22:45'!
newLattice
	^ {[Lattice A: 5 atRandom]. [Lattice Z: 5 atRandom]. [Lattice D: (2 to: 5) atRandom]. [Lattice bcc]. [Lattice fcc]} atRandom value! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/31/2020 18:09:03'!
testE8
	| E8 v |
	E8 _ Lattice E8.
	v _ (E8 atRandomBits: 100) asVector.
	self assert: ((v allSatisfy: [:x| x isIntegral]) or: [v allSatisfy: [:x| x isIntegral not and: [(x*2) isIntegral]]]).
	self assert: (v sum isIntegral and: [v sum asInteger even]).
	self assert: E8 isIntegral.
	self assert: E8 isUnimodular.
	self assert: E8 isEven.
	self assert: E8 isSelfDual! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2020 08:26:24'!
testLLL
	| V b1 b2 b3 reducer |
	V _ Ñ^3.
	b1 _ V !! (1, 1, 1).
	b2 _ V !! (-1, 0, 2).
	b3 _ V !! (3, 5, 6).
	reducer _ LLL basis: { b1. b2. b3 } innerProduct: V innerProduct.
	self assert: reducer reducedBasis = {V !! (0,1,0). V !! (1,0,1). V !! (-1,0,2)}! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/28/2020 19:46:17'!
testLLL2
	"Example from Magma handbook (H30E1)."
	| V b1 b2 reducer |
	V _ Ñ^3.
	b1 _ V !! (1, 2, 3).
	b2 _ V !! (3, 2, 1).
	reducer _ LLL basis: { b1. b2 } innerProduct: V innerProduct.
	self assert: reducer reducedBasis = {V !! (2,0,-2). V !! (1,2,3)}! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/11/2020 08:02:37'!
testLatticeDual
	| V L R |
	V _ Ñ^3.
	L _ Lattice basis: {V !! (-1,0,0). V !! (0,1,2). V !! (0,1,1)}.
	R _ L dual.
	self assert: (L space span: L basis) = (R space span: R basis).
	self assert: L determinant = (1 / R determinant).
	self assert: (R generatorMatrix * L generatorMatrix transpose) isIdentity.
	self assert: L gramian = R gramian inverse! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/11/2020 08:02:45'!
testLatticeDual2
	| V L R |
	V _ Ñ^3.
	L _ Lattice basis: {V !! (-1,0,0). V !! (0,1,2)}. "not full rank"
	R _ L dual.
	self assert: (L space span: L basis) = (R space span: R basis).
	self assert: L determinant = (1 / R determinant).
	self assert: (R generatorMatrix transpose * L generatorMatrix) isIdentity.
	self assert: L gramian = R gramian inverse! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 07:59:09'!
testLatticeDual3
	| L |
	L _ Lattice Z: 3.
	self assert: L dual = L.
	self assert: (L*3) dual = (L*(1/3))! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 07:59:14'!
testLatticeDualDeterminant
	| L R |
	L _ self newLattice.
	R _ L dual.
	self assert: L determinant = (1 / R determinant)! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 07:59:20'!
testLatticeDualDual
	| L |
	L _ self newLattice.
	self assert: L dual dual = L! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/3/2020 07:59:24'!
testLatticeDualGramian
	| L R |
	L _ self newLattice.
	R _ L dual.
	self assert: L gramian = R gramian inverse! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/30/2020 18:15:32'!
testLatticeElements
	| V L v w |
	V _ Ñ^3.
	L _ Lattice basis: {V !! (1,0,0). V !! (1,2,3). V !! (3,6,2)}.
	v _ L elementAt: #(1 2 1).
	self assert: v asVector = (V !! (6,10,8)).
	v _ L x: 2.
	self assert: v asVector = (V !! (1,2,3)).
	w _ v*2.
	self assert: w asVector = (V !! (2,4,6)).
	self assert: v norm = 14.
	self assert: (v | w) = 28! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 8/1/2020 19:16:10'!
testLeech
	| L v |
	L _ Lattice leech.
	v _ (L atRandomBits: 100) asVector.
	self assert: L determinant = 1.
"	self assert: L kissingNumber = 196560."
"	self assert: L minimalNorm = 4."
	self assert: L isIntegral.
	self assert: L isUnimodular.
	self assert: L isEven.
	self assert: L isSelfDual! !
