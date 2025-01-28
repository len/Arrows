'From Cuis7.3 [latest update: #7034] on 28 January 2025 at 8:58:52 pm'!
'Description '!
!provides: 'Tests-Lattices' 1 4!
!requires: 'Lattices' 1 0 nil!
SystemOrganization addCategory: #'Tests-Lattices'!


!classDefinition: #LatticeTest category: #'Tests-Lattices'!
RandomizedTestCase subclass: #LatticeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Lattices'!
!classDefinition: 'LatticeTest class' category: #'Tests-Lattices'!
LatticeTest class
	instanceVariableNames: ''!


!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/3/2023 00:27:55'!
newLattice
	^ ({[Lattice A: (5 atRandom: random)]. [Lattice Z: (5 atRandom: random)]. [Lattice D: ((2 to: 5) atRandom: random)]. [Lattice bcc]. [Lattice fcc]} atRandom: random) value! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/3/2023 00:28:06'!
testE8
	| E8 v |
	E8 := Lattice E8.
	v := (E8 atRandom: random bits: 100) asVector.
	self assert: ((v allSatisfy: [:x| x isIntegral]) or: [v allSatisfy: [:x| x isIntegral not and: [(x*2) isIntegral]]]).
	self assert: (v sum isIntegral and: [v sum asInteger even]).
	self assert: E8 isIntegral.
	self assert: E8 isUnimodular.
	self assert: E8 isEven.
	self assert: E8 isSelfDual! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/8/2020 08:26:24'!
testLLL
	| V b1 b2 b3 reducer |
	V _ ℚ^3.
	b1 _ V !! (1, 1, 1).
	b2 _ V !! (-1, 0, 2).
	b3 _ V !! (3, 5, 6).
	reducer _ LLL basis: { b1. b2. b3 } innerProduct: V innerProduct.
	self assert: reducer reducedBasis = {V !! (0,1,0). V !! (1,0,1). V !! (-1,0,2)}! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/28/2020 19:46:17'!
testLLL2
	"Example from Magma handbook (H30E1)."
	| V b1 b2 reducer |
	V _ ℚ^3.
	b1 _ V !! (1, 2, 3).
	b2 _ V !! (3, 2, 1).
	reducer _ LLL basis: { b1. b2 } innerProduct: V innerProduct.
	self assert: reducer reducedBasis = {V !! (2,0,-2). V !! (1,2,3)}! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:52:31'!
testLatticeDual
	| V L R |
	V := ℚ^3.
	L := Lattice basis: {V !! (-1,0,0). V !! (0,1,2). V !! (0,1,1)}.
	R := L dual.
	self assert: (L space sub: L basis) = (R space sub: R basis).
	self assert: L determinant = (1 / R determinant).
	self assert: (R generatorMatrix * L generatorMatrix transpose) isIdentity.
	self assert: L gramian = R gramian inverse! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:52:38'!
testLatticeDual2
	| V L R |
	V := ℚ^3.
	L := Lattice basis: {V !! (-1,0,0). V !! (0,1,2)}. "not full rank"
	R := L dual.
	self assert: (L space sub: L basis) = (R space sub: R basis).
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

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2023 14:36:55'!
testLatticeElements
	| V L v w |
	V := ℚ^3.
	L := Lattice basis: {V !! (1,0,0). V !! (1,2,3). V !! (3,6,2)}.
	v := L elementAt: #(1 2 1).
	self assert: v asVector = (V !! (6,10,8)).
	v := L x: 2.
	self assert: v asVector = (V !! (1,2,3)).
	w := v*2.
	self assert: w asVector = (V !! (2,4,6)).
	self assert: v norm = 14.
	self assert: v · w = 28! !

!LatticeTest methodsFor: 'as yet unclassified' stamp: 'len 7/3/2023 00:28:54'!
testLeech
	| L v |
	L := Lattice leech.
	v := (L atRandom: random bits: 100) asVector.
	self assert: L determinant = 1.
"	self assert: L kissingNumber = 196560."
"	self assert: L minimalNorm = 4."
	self assert: L isIntegral.
	self assert: L isUnimodular.
	self assert: L isEven.
	self assert: L isSelfDual! !
