'From Cuis 6.0 [latest update: #5801] on 30 May 2023 at 8:04:05 pm'!
'Description '!
!provides: 'Lattices' 1 11!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: #Lattices!


!classDefinition: #Lattice category: #Lattices!
Structure subclass: #Lattice
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Lattices'!
!classDefinition: 'Lattice class' category: #Lattices!
Lattice class
	instanceVariableNames: ''!

!classDefinition: #LatticeMap category: #Lattices!
Function subclass: #LatticeMap
	instanceVariableNames: 'matrix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Lattices'!
!classDefinition: 'LatticeMap class' category: #Lattices!
LatticeMap class
	instanceVariableNames: ''!

!classDefinition: #LLL category: #Lattices!
Object subclass: #LLL
	instanceVariableNames: 'basis innerProduct delta orthogonalBasis mu orthogonalBasisNorms'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Lattices'!
!classDefinition: 'LLL class' category: #Lattices!
LLL class
	instanceVariableNames: ''!

!classDefinition: #LatticeElement category: #Lattices!
Object subclass: #LatticeElement
	instanceVariableNames: 'parent coordinates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Lattices'!
!classDefinition: 'LatticeElement class' category: #Lattices!
LatticeElement class
	instanceVariableNames: ''!


!Lattice commentStamp: '<historical>' prior: 0!
A lattice is the set of integer linear combinations of a basis of a rational or real vector space. This is a discrete subgroup of the ambient vector space (as abelian group) isomorphic to ℤ^n.

More generally, given an integral domain R and a finite-dimensional vector space V over its field of fractions, a lattice or R-lattice in V is a finitely generated R-module over R that spans V. In particular, lattices are defined to have full rank in V.

A lattice is defined by giving a basis {v1,...,vn} consisting of linearly independent vectors of the ambient vector space V, and an inner product (a positive definite symmetric bilinear form) or equivalently the Gramian matrix aij := <vi, vj>. The inner product or the Gramian matrix correspond also to the associated quadratic form.!

!LLL commentStamp: '<historical>' prior: 0!
This is the Lenstra-Lenstra-Lovasz algorithm.

Input: integer lattice of dimension d given by a basis {b1, ..., bd} and an inner product;
Output: an almost H-reduced basis of the lattice.

The algorithm takes at most O(d^6 ln^3(B)), where the input vectors bi all satisfy |bi|^2 <= B. In practice however, this upper bound is quite pessimistic.
!

!Lattice methodsFor: 'accessing' stamp: 'len 7/8/2020 08:36:35'!
basis
	^ self propertyAt: #basis! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/29/2020 07:02:20'!
content
	"Answer the largest rational number c such that the inner products (u, v) is in c*Z for all u and v in the lattice."
	^ self gramian denominator! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/30/2020 19:57:59'!
coordinateLattice
	"Answer the coordinate lattice of the receiver."
	^ self class gramian: self gramian! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/29/2020 06:53:30'!
degree
	^ self space rank! !

!Lattice methodsFor: 'accessing' stamp: 'len 6/10/2022 11:17:01'!
generatorMatrix
	^ self propertyAt: #generatorMatrix ifAbsentPut: [Matrix to: self space cover columns: (self basis collect: [:each| self space \ each])]! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/29/2020 06:35:41'!
generators
	^ self propertyAt: #generators ifAbsentPut: [self asModule generators collect: [:each| self elementAt: each]]! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/10/2020 12:41:19'!
gramian
	^ self innerProduct gramianOf: self basis! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/8/2020 08:17:07'!
innerProduct
	^ self propertyAt: #innerProduct! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/29/2020 07:04:05'!
level
	"Answer the smallest integer k such that k * norm(v) is in 2*Z for all v in the lattice (assuming the receiver is an integral lattice)."
	^ self gramian denominator! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/29/2020 06:29:24'!
rank
	^ self basis size! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/8/2020 08:26:43'!
reducedBasis
	^ self propertyAt: #reducedBasis ifAbsentPut: [(LLL basis: self basis innerProduct: self innerProduct) reducedBasis]! !

!Lattice methodsFor: 'accessing' stamp: 'len 3/28/2018 05:05:34'!
shortVector
	^ self reducedBasis first! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/28/2020 19:32:24'!
space
	"Answer the ambient space of the receiver."
	^ self innerProduct space! !

!Lattice methodsFor: 'accessing' stamp: 'len 7/8/2020 08:35:58'!
zero
	^ self space zero! !

!Lattice methodsFor: 'comparing' stamp: 'len 8/11/2020 08:01:46'!
= anObject
	^ self class = anObject class and: [self innerProduct = anObject innerProduct and: [self generatorMatrix transpose HNF = anObject generatorMatrix transpose HNF]]! !

!Lattice methodsFor: 'comparing' stamp: 'len 8/11/2020 08:02:25'!
hash
	^ self generatorMatrix transpose HNF hash! !

!Lattice methodsFor: 'converting' stamp: 'len 10/20/2020 19:04:14'!
asAbelianGroup
	^ self asModule! !

!Lattice methodsFor: 'converting' stamp: 'len 9/16/2022 08:24:47'!
asModule
	^ self propertyAt: #asModule ifAbsentPut:
		[| answer |
		answer _ ℤ^self rank.
		self addCoercionTo: answer map: [:each| each coordinates].
		self addCoercionFrom: answer map: [:each| self elementAt: each].
		answer]! !

!Lattice methodsFor: 'elements' stamp: 'len 7/28/2020 19:57:44'!
elementAt: anArray
	"Answer the element with the given coordinates."
	^ LatticeElement coordinates: self asModule !! anArray parent: self! !

!Lattice methodsFor: 'elements' stamp: 'len 7/29/2020 06:44:50'!
newFrom: anObject
"	(anObject isSequenceable and: [anObject size = self rank])
		ifTrue: [^ self elementAt: (self scalars tuple: anObject)]."
	^ super newFrom: anObject! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/8/2017 18:41:52'!
coveringNormalizedThinkness
	"Answer the normalized thickness (or central density) of the covering."
	^ self coveringRadius ^ self rank / self fundamentalVolume! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/28/2016 11:30:25'!
coveringRadius
	^ self propertyAt: #coveringRadius! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/8/2017 18:42:18'!
coveringSphereVolume
	| n Vn pi p |
	pi _ Float pi.
	n _ self rank.
	Vn _ n even
		ifTrue: [pi ^ (n/2) / (n/2) factorial]
		ifFalse: [2^n * pi ^ ((n-1)/2) * ((n-1)/2) factorial / n factorial].
	p _ self coveringRadius.
	^ p ^ n * Vn! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/8/2017 18:42:49'!
coveringThickness
	"Answer the thickness of the covering, i.e. the average number of spheres that contain a point of space. The thickness is also called the density or the sparsity of the covering."
	^ self coveringSphereVolume / self fundamentalVolume! !

!Lattice methodsFor: 'invariants' stamp: 'len 8/11/2020 08:01:58'!
determinant
	"The determinant det(L) of a lattice L is defined as the volume of its fundamental parallelepiped."
	self generatorMatrix isSquare ifTrue: [^ self generatorMatrix determinant abs].
	^ self gramian determinant sqrt! !

!Lattice methodsFor: 'invariants' stamp: 'len 8/11/2020 08:02:07'!
determinant2
	"The determinant det(L) of a lattice L is defined as the volume of its fundamental paralleleliped."
	| B |
	B _ self generatorMatrix.
	"B square means the lattice is full-rank:"
	^ B isSquare ifTrue: [B determinant abs] ifFalse: [(B transpose * B) determinant sqrt]! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/10/2015 01:00'!
fundamentalParallelepiped
	^ self notYetImplemented! !

!Lattice methodsFor: 'invariants' stamp: 'len 3/19/2018 15:29:23'!
fundamentalVolume
	"Answer the volume of the fundamental region."
	^ self determinant sqrt! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/8/2017 18:44:28'!
packingCenterDensity
	^ self packingRadius ^ self rank / self fundamentalVolume! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/8/2017 18:44:07'!
packingDensity
	"Answer the density of the lattice packing, i.e. the volume of a sphere / the volume of the fundamental region."
	^ self packingSphereVolume / self fundamentalVolume! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/28/2016 11:30:14'!
packingRadius
	^ self propertyAt: #packingRadius! !

!Lattice methodsFor: 'invariants' stamp: 'len 12/8/2017 18:43:48'!
packingSphereVolume
	| n Vn pi p |
	pi _ Float pi.
	n _ self rank.
	Vn _ n even
		ifTrue: [pi ^ (n/2) / (n/2) factorial]
		ifFalse: [2^n * pi ^ ((n-1)/2) * ((n-1)/2) factorial / n factorial].
	p _ self packingRadius.
	^ p ^ n * Vn! !

!Lattice methodsFor: 'morphisms' stamp: 'len 9/16/2022 08:01:03'!
to: aLattice map: aBlock
	^ LatticeMap from: self to: aLattice matrix: (self asModule to: aLattice asModule map: aBlock)! !

!Lattice methodsFor: 'operations' stamp: 'len 8/3/2020 07:30:24'!
* aScalar
	^ self class on: self innerProduct basis: self basis * (self space scalars !! aScalar)

"	^ self class gramian: self gramian * aScalar"! !

!Lattice methodsFor: 'operations' stamp: 'len 8/1/2020 21:34:57'!
+ aLattice
	"Answer the sum of the receiver and the argument, assuming they are compatible."
	^ self notYetImplemented! !

!Lattice methodsFor: 'operations' stamp: 'len 8/1/2020 19:08:46'!
⊕ aLattice
	self notYetImplemented! !

!Lattice methodsFor: 'operations' stamp: 'len 8/1/2020 19:08:50'!
⊗ aLattice
	self notYetImplemented! !

!Lattice methodsFor: 'operations' stamp: 'len 3/14/2021 12:47:15'!
dual
	"Answer the dual lattice of the receiver (or reciprocal lattice, or polar lattice).
	Given a lattice L, the dual lattice L* is defined as the lattice of vectors having integral inner product with all vectors of L."
	| B G |
	B _ self generatorMatrix.
	G _ self gramian.
	^ self class on: self innerProduct basis: ((B * G inverse) columns collect: [:each| self space elementAt: each])! !

!Lattice methodsFor: 'operations' stamp: 'len 8/12/2022 13:15:53'!
∩ aLattice
	"Answer the intersection of the receiver and the argument, assuming they are compatible."
	^ self notYetImplemented! !

!Lattice methodsFor: 'printing' stamp: 'len 5/29/2023 13:59:52'!
printOn: aStream
	self isTrivial ifTrue: [aStream print: 0. ^ self].
	aStream withAngleBrackets: [:aStream2|
		self basis do: [:each| aStream2 print: each] separatedBy: [aStream2 nextPutAll: '; ']].
	aStream nextPutAll: 'ℤ' sub! !

!Lattice methodsFor: 'random' stamp: 'len 7/29/2020 06:21:59'!
atRandom: aRandom bits: bitSize
	^ self !! (self asModule atRandom: aRandom bits: bitSize)! !

!Lattice methodsFor: 'testing' stamp: 'len 7/29/2020 06:38:35'!
includes: anObject
	^ anObject parent == self! !

!Lattice methodsFor: 'testing' stamp: 'len 8/2/2020 17:11:36'!
isDoublyEven
	^ self isIntegral and: [self gramian diagonal allSatisfy: [:each| each asInteger \\ 4 = 0]]! !

!Lattice methodsFor: 'testing' stamp: 'len 7/17/2019 16:22:19'!
isEven
	^ self isIntegral and: [self gramian diagonal allSatisfy: [:each| each asInteger even]]! !

!Lattice methodsFor: 'testing' stamp: 'len 8/1/2020 10:19:33'!
isFullRank
	^ self rank = self length! !

!Lattice methodsFor: 'testing' stamp: 'len 8/1/2020 07:46:24'!
isIntegral
	"Answer true if the receiver is integral, i.e. if the inner product of any two lattice vectors is an integer. This is true iff the Gram matrix contains only integer entries. Equivalently, a lattice L is integral if and only if it is contained in its dual (or reciprocal) lattice L*: L <= L*. Also, an integral lattice L has the property: L <= L* <= L / det(L)."
	self gramian do: [:each| each isIntegral ifFalse: [^ false]].
	^ true! !

!Lattice methodsFor: 'testing' stamp: 'len 8/1/2020 08:06:40'!
isPositiveDefinite
	"Answer true if all x in the receiver L satisfy: <x,x> > 0."
	^ self innerProduct isPositiveDefinite "<- is this right? if is positive definite in the lattice it is always positive definite in the ambient vector space? also, aren't we defining all lattices as positive definite?"! !

!Lattice methodsFor: 'testing' stamp: 'len 12/8/2017 18:29:34'!
isRootLattice
	self isIntegral ifFalse: [^ false].
	self basis do: [:v| (self innerProduct value: v value: v) = 2 ifFalse: [^ false]].
	^ true! !

!Lattice methodsFor: 'testing' stamp: 'len 7/6/2021 14:04:41'!
isSelfDual
	"A self-dual lattice is a lattice L that equals its dual (or reciprocal) lattice L*: L = L*.
	A (integral) lattice is self-dual iff it is unimodular."
	^ self = self dual

"alternative:
	^ self isUnimodular
"! !

!Lattice methodsFor: 'testing' stamp: 'len 7/8/2020 08:41:48'!
isTrivial
	^ self basis isEmpty! !

!Lattice methodsFor: 'testing' stamp: 'len 8/1/2020 08:15:57'!
isTypeI
	^ self isUnimodular and: [self isEven not]! !

!Lattice methodsFor: 'testing' stamp: 'len 8/1/2020 08:15:46'!
isTypeII
	^ self isUnimodular and: [self isEven]! !

!Lattice methodsFor: 'testing' stamp: 'len 12/30/2015 21:51'!
isUnimodular
	^ self isIntegral and: [self determinant abs = 1]! !

!Lattice methodsFor: 'private' stamp: 'len 12/9/2022 17:35:54'!
basis: aCollection innerProduct: aBilinearForm
	aBilinearForm space scalars = ℚ ifFalse: [^ self error: 'the ambient vector space of a lattice must be rational'].
	self propertyAt: #basis put: aCollection; propertyAt: #innerProduct put: aBilinearForm.
	self addCoercionTo: self space map: [:each| each asVector]! !

!Lattice methodsFor: 'private' stamp: 'len 7/31/2020 11:42:31'!
species
	^ Lattice! !

!Lattice class methodsFor: 'examples' stamp: 'len 5/31/2020 20:34:03'!
A: n
	"Answer the lattice A_n = {(x0,...,xn) in Z^{n+1} : x0 + ... + xn = 0}."
	| V t B |
	V _ ℚ^(n+1).
	t _ (V x: 2) - (V x: 1).
	B _ OrderedCollection new.
	n timesRepeat: [B add: t. t _ t >> 1].
	^ self basis: B! !

!Lattice class methodsFor: 'examples' stamp: 'len 11/23/2022 21:26:24'!
Adual: n
	"Answer the dual of A_n."
	| V B |
	V _ ℚ^(n+1).
	B _ OrderedCollection new.
	1 to: n do: [:i| B add: (V x: 1) - (V x: i+1)].
	B add: (V fill: [:i| i=1 ifTrue: [n negated / (n+1)] ifFalse: [1 / (n+1)]]).
	^ self basis: B! !

!Lattice class methodsFor: 'examples' stamp: 'len 8/1/2020 19:06:12'!
D4
	"Another basis for the D4 lattice."
	| V |
	V _ ℚ^4.
	^ self basis: {V !! (1,0,0,0). V !! (0,1,0,0). V !! (0,0,1,0). V !! (1,1,1,1) / 2}! !

!Lattice class methodsFor: 'examples' stamp: 'len 5/31/2020 20:35:11'!
D: n
	"Answer the checkboard lattice D_n = {(x1,...,xn) in Z^n : x0 + ... + xn is even}."
	| V t0 t B |
	V _ ℚ^n.
	B _ OrderedCollection new.
	t0 _ ((V x: 1) + (V x: 2)) negated.
	B add: t0.
	t _ (V x: 1) - (V x: 2).
	n-1 timesRepeat: [B add: t. t _ t >> 1].
	^ self basis: B! !

!Lattice class methodsFor: 'examples' stamp: 'len 11/23/2022 21:26:30'!
Ddual: n
	"Answer the dual of D_n."
	| V B |
	V _ ℚ^n.
	B _ OrderedCollection new.
	1 to: n-1 do: [:i| B add: (V x: i)].
	B add: (V fill: [:i| 1/2]).
	^ self basis: B! !

!Lattice class methodsFor: 'examples' stamp: 'len 11/23/2022 21:26:35'!
E8
	| V t0 t B answer |
	V _ ℚ^8.
	B _ OrderedCollection new.
	t0 _ (V x: 1) * 2.
	B add: t0.
	t _ (V x: 2) - (V x: 1).
	6 timesRepeat: [B add: t. t _ t >> 1].
	B add: (V fill: [:i| 1/2]).
	answer _ self basis: B.
	answer properties
		at: #minimalNorm put: 2;
		at: #kissingNumber put: 240;
		at: #CoxeterNumber put: 30;
		at: #packingRadius put: 1/2 sqrt;
		at: #density put: Float pi ^ 4 / 384;
		at: #thickness put: Float pi ^ 4 / 24.
	^ answer! !

!Lattice class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
Z: n
	"Answer the n-dimensional cubic or integer lattice Z^n."
	^ self basis: (ℚ ^ n) basis! !

!Lattice class methodsFor: 'examples' stamp: 'len 7/30/2020 18:18:23'!
bcc
	"Answer the body-centered cubic lattice (bcc).
	D*_3 is equivalent to bcc."
	| V |
	V _ ℚ^3.
	^ self basis: {V !! (2,0,0). V !! (0,2,0). V !! (1,1,1)}! !

!Lattice class methodsFor: 'examples' stamp: 'len 7/30/2020 18:18:06'!
fcc
	"Answer the face-centered cubic lattice (fcc).
	Both A_3 and D_3 are equivalent to the fcc."
	| V |
	V _ ℚ^3.
	^ self basis: {V !! (-1,-1,0). V !! (1,-1,0). V !! (0,1,-1)}! !

!Lattice class methodsFor: 'examples' stamp: 'len 6/1/2020 12:37:32'!
leech
	"The Leech lattice is the unique even unimodular lattice in R^24 with with no norm 2 vectors."
	| G answer |
	G _ (ℚ^24) endomorphisms !!
#(8 4 4 4 4 4 4 2 4 4 4 2 4 2 2 2 4 2 2 2 0 0 0 -3
 4 4 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 1 1 2 1 0 0 -1
 4 2 4 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 1 1 1 0 0 -1
 4 2 2 4 2 2 2 2 2 2 2 2 2 1 1 2 2 1 2 1 1 0 0 -1
 4 2 2 2 4 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 0 0 -1
 4 2 2 2 2 4 2 2 2 2 2 1 2 2 1 1 2 1 2 1 0 0 0 -1
 4 2 2 2 2 2 4 2 2 2 2 1 2 1 2 1 2 1 1 2 0 0 0 -1
 2 2 2 2 2 2 2 4 1 1 1 2 1 2 2 2 1 2 2 2 2 0 0 1
 4 2 2 2 2 2 2 1 4 2 2 2 2 2 2 2 2 2 2 2 1 1 1 -1
 4 2 2 2 2 2 2 1 2 4 2 2 2 2 1 1 2 2 1 1 0 1 0 -1
 4 2 2 2 2 2 2 1 2 2 4 2 2 1 2 1 2 1 2 1 0 0 1 -1
 2 2 2 2 1 1 1 2 2 2 2 4 1 2 2 2 1 2 2 2 2 1 1 1
 4 2 2 2 2 2 2 1 2 2 2 1 4 2 2 2 2 1 1 1 1 1 1 -1
 2 2 1 1 2 2 1 2 2 2 1 2 2 4 2 2 1 2 2 2 2 2 1 1
 2 1 2 1 2 1 2 2 2 1 2 2 2 2 4 2 1 2 2 2 2 1 2 1
 2 1 1 2 2 1 1 2 2 1 1 2 2 2 2 4 1 2 2 2 2 1 1 1
 4 2 2 2 2 2 2 1 2 2 2 1 2 1 1 1 4 2 2 2 1 1 1 -1
 2 1 2 1 2 1 1 2 2 2 1 2 1 2 2 2 2 4 2 2 2 2 1 1
 2 1 1 2 2 2 1 2 2 1 2 2 1 2 2 2 2 2 4 2 2 1 2 1
 2 2 1 1 2 1 2 2 2 1 1 2 1 2 2 2 2 2 2 4 2 1 1 1
 0 1 1 1 1 0 0 2 1 0 0 2 1 2 2 2 1 2 2 2 4 2 2 2
 0 0 0 0 0 0 0 0 1 1 0 1 1 2 1 1 1 2 1 1 2 4 2 2
 0 0 0 0 0 0 0 0 1 0 1 1 1 1 2 1 1 1 2 1 2 2 4 2
 -3 -1 -1 -1 -1 -1 -1 1 -1 -1 -1 1 -1 1 1 1 -1 1 1 1 2 2 2 4).
	answer _ self gramian: G.
	answer properties
		at: #minimalNorm put: 4;
		at: #kissingNumber put: 196560;
		at: #coveringRadius put: 2 sqrt.
	^ answer! !

!Lattice class methodsFor: 'examples' stamp: 'len 6/1/2020 12:37:55'!
leech2
	"Another version of the Leech lattice."
	| G answer |
	G _ (ℚ^24) endomorphisms !!
#(4 -2 -2 2 2 2 -1 -1 2 2 -2 2 -2 1 -1 1 1 2 0 -1 0 -2 -2 2
-2 4 0 0 -2 0 -1 2 0 0 2 -1 1 1 -1 1 -2 -2 -1 1 1 2 1 -2
-2 0 4 0 0 -2 0 0 0 0 2 0 0 -2 1 -1 -1 -1 1 -1 -1 1 0 -1
2 0 0 4 0 0 -2 -1 2 2 0 2 -2 1 0 2 -1 0 -1 0 1 -1 -1 0
2 -2 0 0 4 0 -1 -1 1 1 -1 1 -1 0 0 -1 0 2 0 -2 -2 -1 -2 2
2 0 -2 0 0 4 0 0 1 1 -1 1 -1 1 -1 1 1 0 1 0 1 0 -1 1
-1 -1 0 -2 -1 0 4 1 -1 -1 0 -2 2 0 0 0 2 -1 0 0 -1 1 2 -1
-1 2 0 -1 -1 0 1 4 1 1 2 -1 1 0 -1 1 -1 -1 -1 -1 -1 1 1 -1
2 0 0 2 1 1 -1 1 4 2 0 2 -2 1 0 1 -1 0 -1 -2 0 -1 -1 0
2 0 0 2 1 1 -1 1 2 4 0 1 -2 0 -1 1 -1 1 0 -1 -1 -1 -1 1
-2 2 2 0 -1 -1 0 2 0 0 4 0 1 -1 0 1 -1 -2 0 0 0 2 1 -2
2 -1 0 2 1 1 -2 -1 2 1 0 4 -2 0 1 0 0 1 0 -1 1 -2 -2 1
-2 1 0 -2 -1 -1 2 1 -2 -2 1 -2 4 0 -1 0 1 -1 -1 1 0 2 2 -2
1 1 -2 1 0 1 0 0 1 0 -1 0 0 4 -1 1 0 -1 -2 0 0 0 0 0
-1 -1 1 0 0 -1 0 -1 0 -1 0 1 -1 -1 4 -1 -1 0 0 0 0 -1 0 0
1 1 -1 2 -1 1 0 1 1 1 1 0 0 1 -1 4 0 -1 -1 0 1 1 0 -1
1 -2 -1 -1 0 1 2 -1 -1 -1 -1 0 1 0 -1 0 4 1 1 0 0 0 0 0
2 -2 -1 0 2 0 -1 -1 0 1 -2 1 -1 -1 0 -1 1 4 1 -1 -1 -2 -2 2
0 -1 1 -1 0 1 0 -1 -1 0 0 0 -1 -2 0 -1 1 1 4 0 0 0 -1 1
-1 1 -1 0 -2 0 0 -1 -2 -1 0 -1 1 0 0 0 0 -1 0 4 2 0 2 -1
0 1 -1 1 -2 1 -1 -1 0 -1 0 1 0 0 0 1 0 -1 0 2 4 0 1 -1
-2 2 1 -1 -1 0 1 1 -1 -1 2 -2 2 0 -1 1 0 -2 0 0 0 4 1 -2
-2 1 0 -1 -2 -1 2 1 -1 -1 1 -2 2 0 0 0 0 -2 -1 2 1 1 4 -2
2 -2 -1 0 2 1 -1 -1 0 1 -2 1 -2 0 0 -1 0 2 1 -1 -1 -2 -2 4).
	answer _ self gramian: G.
	answer properties
		at: #minimalNorm put: 4;
		at: #kissingNumber put: 196560;
		at: #coveringRadius put: 2 sqrt.
	^ answer! !

!Lattice class methodsFor: 'instance creation' stamp: 'len 3/21/2021 14:55:54'!
basis: aCollection
	^ self on: aCollection first parent innerProduct basis: aCollection! !

!Lattice class methodsFor: 'instance creation' stamp: 'len 8/1/2020 08:18:05'!
gramian: aMatrix
	"Answer the lattice with the given Gram matrix."
	| B |
	B _ BilinearForm on: aMatrix domain matrix: aMatrix.
	^ self on: B basis: B space basis! !

!Lattice class methodsFor: 'instance creation' stamp: 'len 7/28/2020 19:29:35'!
on: aBilinearForm basis: aCollection
	^ self new basis: aCollection innerProduct: aBilinearForm! !

!LatticeMap methodsFor: 'accessing' stamp: 'len 7/29/2020 08:36:30'!
category
	^ Lattices! !

!LatticeMap methodsFor: 'accessing' stamp: 'len 7/29/2020 08:39:41'!
matrix
	^ matrix! !

!LatticeMap methodsFor: 'initialization' stamp: 'len 7/29/2020 13:03:05'!
matrix: aMatrix
	matrix _ aMatrix! !

!LatticeMap methodsFor: 'operations' stamp: 'len 7/29/2020 08:40:16'!
value: anElement
	^ self codomain elementAt: matrix * (self domain !! anElement) coordinates! !

!LatticeMap methodsFor: 'printing' stamp: 'len 7/29/2020 08:40:33'!
printOn: aStream
	aStream print: matrix! !

!LatticeMap class methodsFor: 'class initialization' stamp: 'len 7/29/2020 08:35:50'!
initialize
	super initialize.
	Smalltalk at: #Lattices put: (Category new name: #Lattices)! !

!LatticeMap class methodsFor: 'instance creation' stamp: 'len 7/29/2020 08:39:16'!
from: aDomain to: aCodomain matrix: aMatrix
	^ self new
		domain: aDomain;
		codomain: aCodomain;
		matrix: aMatrix! !

!LLL methodsFor: 'accessing' stamp: 'len 12/23/2015 03:01'!
reducedBasis
	orthogonalBasis notNil ifFalse: [self computeReducedBasis].
	^ basis! !

!LLL methodsFor: 'initialization' stamp: 'len 7/8/2020 08:19:26'!
basis: aCollection innerProduct: aBilinearForm
	basis _ aCollection asArray.
	innerProduct _ aBilinearForm.
	"d _ basis size."
	delta _ 3/4 "(4/3 + 0.0000000001)^((d-1)/2)"! !

!LLL methodsFor: 'private' stamp: 'len 11/23/2022 21:31:31'!
computeMu
	| n bi oj |
	n _ basis size.
	mu _ (ℚ^n) endomorphisms fill: [:i :j|
		bi _ basis at: i.
		oj _ orthogonalBasis at: j.
		(innerProduct value: bi value: oj) / (orthogonalBasisNorms at: j)] "redundant matrix multiplication by oj"! !

!LLL methodsFor: 'private' stamp: 'len 7/8/2020 08:24:22'!
computeOrthogonalBasis
	| k vectors |
	vectors _ Array withAll: basis.
	k _ vectors size.
	1 to: k do: [:i|
		| vi norm |
		vi _ vectors at: i.
		norm _ innerProduct value: vi value: vi.
		i+1 to: k do: [:j|
			| vj |
			vj _ vectors at: j.
			vj _ vj - (vi * ((innerProduct value: vj value: vi) / norm)).
			vectors at: j put: vj]].
	orthogonalBasis _ vectors.
	orthogonalBasisNorms _ orthogonalBasis collect: [:each| innerProduct value: each value: each]! !

!LLL methodsFor: 'private' stamp: 'len 7/8/2020 08:25:20'!
computeReducedBasis
	| k mukj bj bk a b |
	self computeOrthogonalBasis; computeMu.
	k _ 1.
	[k < basis size] whileTrue:
		[k-1 to: 0 by: -1 do: [ :j |
			mukj _ mu at: k+1 @ (j+1).
			bj _ basis at: j+1.
			bk _ basis at: k+1.
			mukj abs > (1/2) ifTrue:
				[basis at: k+1 put: bk - (bj * mukj rounded).
				self update: k+1]].
		a _ orthogonalBasisNorms at: k+1.
		b _ orthogonalBasisNorms at: k.
		a >= ((delta - (mu at: k+1 @ k) squared) * b)
			ifTrue: [k _ k + 1]
			ifFalse:
				[basis swap: k+1 with: k.
				self update: k+1.
				k _ k-1 max: 1]].
	^ basis! !

!LLL methodsFor: 'private' stamp: 'len 2/23/2016 07:33'!
update: k
	self flag: #fix. "don't need to recompute all"
	self computeOrthogonalBasis; computeMu! !

!LLL class methodsFor: 'examples' stamp: 'len 7/8/2020 08:27:58'!
example
	| V b1 b2 b3 reducer |
	V _ ℚ^3.
	b1 _ V !! (1, 1, 1).
	b2 _ V !! (-1, 0, 2).
	b3 _ V !! (3, 5, 6).
	reducer _ self basis: { b1. b2. b3 } innerProduct: V innerProduct.
	^ reducer reducedBasis! !

!LLL class methodsFor: 'examples' stamp: 'len 7/8/2020 08:28:50'!
example2
	"Find polynomial x^2 - x - 1 that has the golden ratio phi as root."
	| V r b1 b2 b3 reducer |
	V _ ℚ^4.
	r _ 1.618034.
	b1 _ V !! (1, 0, 0, (10000 * r squared) rounded).
	b2 _ V !! (0, 1, 0, (10000 * r) rounded).
	b3 _ V !! (0, 0, 1, 10000).
	reducer _ self basis: { b1. b2. b3 } innerProduct: V innerProduct.
	^ reducer reducedBasis! !

!LLL class methodsFor: 'instance creation' stamp: 'len 7/8/2020 08:18:40'!
basis: aCollection innerProduct: aBilinearForm
	^ self new basis: aCollection innerProduct: aBilinearForm! !

!LatticeElement methodsFor: 'printing' stamp: 'len 7/8/2020 19:13:32'!
printOn: aStream
	aStream print: self asVector! !

!LatticeElement methodsFor: 'converting' stamp: 'len 7/29/2020 06:32:28'!
asVector
	| answer |
	answer _ parent space zero.
	coordinates with: parent basis do: [:ai :xi| answer _ xi * ai + answer].
	^ answer! !

!LatticeElement methodsFor: 'initialization' stamp: 'len 7/29/2020 06:30:18'!
coordinates: aTuple parent: aLattice 
	coordinates _ aTuple.
	parent _ aLattice! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/29/2020 06:42:14'!
* anInteger
	anInteger isInteger ifFalse: [^ DomainError signal].
	^ self class coordinates: coordinates * anInteger parent: parent! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/29/2020 06:42:23'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ self class coordinates: coordinates + anElement coordinates parent: parent! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/8/2020 18:50:32'!
- anElement
	^ self + anElement negated! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/29/2020 06:42:36'!
// anInteger
	anInteger isInteger ifFalse: [^ DomainError signal].
	^ self class coordinates: coordinates // anInteger parent: parent! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/28/2020 19:39:07'!
| anElement
	"Answer the inner product of the receiver and the argument."
	^ parent innerProduct value: anElement asVector value: self asVector! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/8/2020 19:13:58'!
length
	^ self norm sqrt! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/29/2020 06:43:08'!
negated
	^ self class coordinates: coordinates negated parent: parent! !

!LatticeElement methodsFor: 'operations' stamp: 'len 7/8/2020 19:14:19'!
norm
	| v |
	v _ self asVector.
	^ parent innerProduct value: v value: v! !

!LatticeElement methodsFor: 'testing' stamp: 'len 7/8/2020 18:52:04'!
isZero
	^ coordinates isZero! !

!LatticeElement methodsFor: 'private' stamp: 'len 7/8/2020 18:47:55'!
coordinates
	^ coordinates! !

!LatticeElement methodsFor: 'private' stamp: 'len 7/8/2020 18:47:25'!
parent
	^ parent! !

!LatticeElement class methodsFor: 'instance creation' stamp: 'len 7/29/2020 06:30:28'!
coordinates: aTuple parent: aLattice
	^ self new coordinates: aTuple parent: aLattice! !
LatticeMap initialize!
