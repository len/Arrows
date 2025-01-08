'From Cuis6.3 [latest update: #6222] on 7 January 2025 at 7:49:48 am'!
'Description '!
!provides: 'Etc2' 1 2!
SystemOrganization addCategory: #Etc2!


!classDefinition: #QuasiGaloisRingTest category: #Etc2!
AbstractCommutativeRingTest subclass: #QuasiGaloisRingTest
	instanceVariableNames: 'p r n'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'QuasiGaloisRingTest class' category: #Etc2!
QuasiGaloisRingTest class
	instanceVariableNames: ''!

!classDefinition: #JetRing category: #Etc2!
AffineAlgebra subclass: #JetRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'JetRing class' category: #Etc2!
JetRing class
	instanceVariableNames: ''!

!classDefinition: #LaurentPolynomialRing category: #Etc2!
AffineAlgebra subclass: #LaurentPolynomialRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'LaurentPolynomialRing class' category: #Etc2!
LaurentPolynomialRing class
	instanceVariableNames: ''!

!classDefinition: #PathAlgebra category: #Etc2!
Ring subclass: #PathAlgebra
	instanceVariableNames: 'quiver scalars'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'PathAlgebra class' category: #Etc2!
PathAlgebra class
	instanceVariableNames: ''!

!classDefinition: #Jet category: #Etc2!
Residue subclass: #Jet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'Jet class' category: #Etc2!
Jet class
	instanceVariableNames: ''!

!classDefinition: #LaurentPolynomial category: #Etc2!
Residue subclass: #LaurentPolynomial
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'LaurentPolynomial class' category: #Etc2!
LaurentPolynomial class
	instanceVariableNames: ''!

!classDefinition: #PathAlgebraElement category: #Etc2!
RingElement subclass: #PathAlgebraElement
	instanceVariableNames: 'coefficients parent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etc2'!
!classDefinition: 'PathAlgebraElement class' category: #Etc2!
PathAlgebraElement class
	instanceVariableNames: ''!


!JetRing commentStamp: '<historical>' prior: 0!
Affine R-algebras of the form Jₖ := R[x₁,...,xₙ]/⟨x₁,...,xₙ⟩^k for an integer k > 0. These are local rings with maximal ideal ⟨x₁,...,xₙ⟩, and their limit (inverse limit) is the ring of formal power series R[[x₁,...,xₙ]]. They are analogous to the prime power rings ℤ/(p^k)ℤ, whose limit is the ring of p-adic integers ℤₚ.

In the particular case of R = ℤ/(p^i)ℤ and n=1, this is the quasi-Galois ring A(p^i,k) = (ℤ/(p^i)ℤ)[x]/⟨x^k⟩.

See also PrimePowerRing.
!

!QuasiGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 11/21/2023 15:26:31'!
setUp
	| F |
	super setUp.
	p := #(2 3) atRandom: random.
	r := 3 atRandom: random.
	n := (2 atRandom: random) + 1.
	F := GaloisField new: p to: r.
	R := JetRing new: n baseIdeal: (F polynomials x asIdeal)! !

!QuasiGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 7/12/2023 19:54:08'!
testNilradical
	| a |
	self assert: R nilradical = (R * R x).
	self assert: (R nilradical atRandom: random) isNilpotent.
	[a := R atRandom: random. a isNilpotent] whileFalse.
	self assert: (R nilradical includes: a)! !

!QuasiGaloisRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/30/2023 23:27:55'!
testQuasiGaloisRingBasicProperties
	self assert: R characteristic = p.
	self assert: R size = (p^(r*n)).
	self assert: R isLocal.
	self assert: R isFinite.
	self assert: R isField not! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:10:38'!
baseIdeal
	^ self propertyAt: #baseIdeal! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:11:05'!
baseIdeal: anIdeal order: anInteger
	self propertyAt: #baseIdeal put: anIdeal.
	self propertyAt: #order put: anInteger! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 11/21/2023 15:18:44'!
elementClass
	^ Jet! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 4/16/2024 18:39:09'!
from: aRing
"	((aDomain isKindOf: PowerSeriesRing) and: [aDomain baseIdeal = self baseIdeal])
		ifTrue: [^ (aDomain onto: self map: [:x| self project: x representative]) name: 'π', self exponent printString sub]."
"originally in PrimePowerRing:"
"	((aDomain isKindOf: ModularIntegerRing) and: [self modulus | aDomain modulus and: [(self modulus * self p | aDomain modulus) not]])" "localization map"
		"ifTrue: [^ aDomain onto: self map: [:x| self project: x lift]]. "
	^ super from: aRing! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 11/3/2023 14:17:11'!
isIntegralDomain
	^ false! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:21:53'!
isLocal
	^ true! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 1/3/2025 13:32:50'!
isQuasiGaloisRing
	^ self rank = 1 and: [self scalars isKindOf: LocalModularRing]! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:23:07'!
maximalIdeal
	^ self * self generators! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 11/21/2023 15:19:50'!
nilradical
	self isQuasiGaloisRing ifTrue: [^ self maximalIdeal].
	^ super nilradical! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:11:23'!
order
	^ self propertyAt: #order! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:13:40'!
printOn: aStream
	aStream print: self cover; nextPut: $╱; print: self baseIdeal; print: self order printString super! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:11:57'!
relations
	^ self propertyAt: #relations ifAbsentPut: [self baseIdeal ^ self order]! !

!JetRing methodsFor: 'as yet unclassified' stamp: 'len 11/21/2023 15:20:16'!
zeroDivisors
	self isQuasiGaloisRing ifTrue: [^ self maximalIdeal].
	^ super zeroDivisors! !

!JetRing class methodsFor: 'as yet unclassified' stamp: 'len 9/6/2023 16:32:38'!
new: anInteger baseIdeal: anIdeal
	^ self new baseIdeal: anIdeal order: anInteger! !

!LaurentPolynomialRing methodsFor: 'as yet unclassified' stamp: 'len 1/15/2022 17:59:24'!
elementClass
	^ LaurentPolynomial! !

!LaurentPolynomialRing methodsFor: 'as yet unclassified' stamp: 'len 1/15/2022 17:21:38'!
printIndeterminatesOn: aStream
	self indeterminates
		do: [:i| aStream print: (self x: i)]
		separatedBy: [aStream nextPut: $,]! !

!LaurentPolynomialRing methodsFor: 'as yet unclassified' stamp: 'len 1/15/2022 17:18:36'!
printOn: aStream
	aStream print: self scalars; nextPut: $[.
	self printIndeterminatesOn: aStream.
	aStream nextPut: $]! !

!LaurentPolynomialRing class methodsFor: 'as yet unclassified' stamp: 'len 1/15/2022 17:58:08'!
monomials: aFreeAbelianMonoid over: aRing
	| h R |
	aFreeAbelianMonoid rank even ifFalse: [^ self error: 'should have an even number of indeterminates'].
	h _ aFreeAbelianMonoid rank / 2.
	R _ PolynomialRing monomials: aFreeAbelianMonoid over: aRing.
	^ self relations: R * ((1 to: h) collect: [:i| (R x: i) * (R x: i+h) - 1])! !

!LaurentPolynomialRing class methodsFor: 'as yet unclassified' stamp: 'len 1/15/2022 18:02:14'!
new: anInteger over: aRing
	^ self monomials: (FreeAbelianMonoid new: anInteger*2) over: aRing! !

!LaurentPolynomialRing class methodsFor: 'as yet unclassified' stamp: 'len 1/15/2022 18:02:43'!
new: anInteger over: aRing ordering: aMonomialOrdering
	^ self monomials: (FreeAbelianMonoid new: anInteger*2 ordering: aMonomialOrdering) over: aRing! !

!PathAlgebra methodsFor: 'as yet unclassified' stamp: 'len 12/16/2024 08:03:55'!
printOn: aStream
	aStream print: scalars; nextPut: $[.
	quiver generators do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $]! !

!PathAlgebra methodsFor: 'as yet unclassified' stamp: 'len 12/16/2024 08:02:19'!
quiver
	^ quiver! !

!PathAlgebra methodsFor: 'as yet unclassified' stamp: 'len 12/16/2024 08:02:05'!
quiver: aFreeCategory scalars: aRing
	quiver := aFreeCategory.
	scalars := aRing! !

!PathAlgebra methodsFor: 'as yet unclassified' stamp: 'len 12/16/2024 08:02:14'!
scalars
	^ scalars! !

!PathAlgebra class methodsFor: 'instance creation' stamp: 'len 12/16/2024 08:04:10'!
on: aFreeCategory over: aRing
	^ self new quiver: aFreeCategory scalars: aRing! !

!Jet methodsFor: 'testing' stamp: 'len 11/21/2023 15:18:06'!
isNilpotent
	^ self isZeroDivisor! !

!Jet methodsFor: 'testing' stamp: 'len 11/21/2023 15:18:01'!
isUnit
	^ self isZeroDivisor not! !

!Jet methodsFor: 'testing' stamp: 'len 11/21/2023 15:18:17'!
isZeroDivisor
	^ representative independentCoefficient isZeroDivisor! !

!LaurentPolynomial methodsFor: 'as yet unclassified' stamp: 'len 12/1/2023 16:48:16'!
printCoefficient: coefficient monomial: m on: aStream
	| c |
	c := coefficient.
	(c isOne not or: [m degree = 0]) ifTrue:
		[c printText first = $-
			ifTrue: [aStream nextPut: $-. c := c negated].
		(m degree = 0 or: [c printText asString allSatisfy: [:each| each isAlphaNumeric or: ['{}()/' includes: each]]])
			ifTrue: [aStream print: c]
			ifFalse: [aStream nextPut: $(; print: c; nextPut: $)]].
	m degree = 0 ifFalse: [self printMonomial: m on: aStream]! !

!LaurentPolynomial methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 15:21:06'!
printIndeterminate: anInteger on: aStream
	| h i |
	h _ self parent rank / 2.
	i _ anInteger > h ifTrue: [anInteger - h] ifFalse: [anInteger].
	(self parent names ifNotNil: [:names| names at: i ifAbsent: []])
		ifNotNil: [:name| aStream nextPutAll: name displayStringOrText. ^ self].
	aStream nextPut: $x; nextPutAll: i printStringSubscript! !

!LaurentPolynomial methodsFor: 'as yet unclassified' stamp: 'len 5/6/2023 15:35:53'!
printMonomial: m on: aStream
	| h |
	m degree = 0 ifTrue: [aStream print: 1. ^ self].
	h _ self parent rank / 2.
	(m ordering indeterminates select: [:i| (m at: i) ~= 0])
		do: [:i| | r |
			self printIndeterminate: i on: aStream.
			r _ m at: i.
			i > h ifTrue: [r _ r negated].
			r = 1 ifFalse: [aStream nextPutAll: r printString super]]! !

!LaurentPolynomial methodsFor: 'as yet unclassified' stamp: 'len 12/3/2023 13:56:18'!
printOn: aStream
	| monomials leadingMonomial c |
	representative isConstant ifTrue: [aStream print: representative leadingCoefficient. ^ self].
	monomials := representative monomials sorted reversed.
	c := representative at: (leadingMonomial := monomials first).
	c printText first = $-
		ifTrue: [c := c negated. aStream nextPut: $-].
	self printCoefficient: c monomial: leadingMonomial on: aStream.
	monomials allButFirstDo: [:m|
		c := representative at: m.
		c printText first = $-
			ifTrue: [aStream nextPutAll: ' - '. c := c negated]
			ifFalse: [aStream nextPutAll: ' + '].
		self printCoefficient: c monomial: m on: aStream]! !

!PathAlgebraElement methodsFor: 'accessing' stamp: 'len 12/16/2024 07:43:37'!
coefficients
	^ coefficients! !

!PathAlgebraElement methodsFor: 'accessing' stamp: 'len 12/16/2024 07:43:37'!
parent
	^ parent! !

!PathAlgebraElement methodsFor: 'comparing' stamp: 'len 12/16/2024 07:43:37'!
= anObject
	^ self class = anObject class and: [coefficients = anObject coefficients]! !

!PathAlgebraElement methodsFor: 'comparing' stamp: 'len 12/16/2024 07:49:13'!
hash
	| answer |
	answer := 0.
	coefficients keysAndValuesDo: [:key :value| answer := key hash + value hash + answer].
	^ answer! !

!PathAlgebraElement methodsFor: 'operations' stamp: 'len 12/16/2024 07:54:55'!
* anElement
	| newCoefficients |
	(anElement isInteger or: [parent scalars includes: anElement])
		ifTrue: [^ self class coefficients: coefficients * anElement parent: parent].
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #*].
	newCoefficients := Dictionary new: coefficients size + anElement coefficients size.
	coefficients associationsDo: [:each|
		anElement coefficients associationsDo: [:other|
			each key domain = other key codomain
				ifTrue: [newCoefficients at: each key ∘ other key addAndRemoveIfZero: each value * other value]]].
	^ self class coefficients: newCoefficients parent: parent! !

!PathAlgebraElement methodsFor: 'operations' stamp: 'len 12/16/2024 07:43:37'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ self class coefficients: coefficients + anElement coefficients parent: parent! !

!PathAlgebraElement methodsFor: 'operations' stamp: 'len 12/16/2024 07:43:37'!
/ anElement
	(anElement isInteger or: [parent scalars includes: anElement])
		ifTrue: [^ self class coefficients: coefficients / anElement parent: parent].
	^ super / anElement! !

!PathAlgebraElement methodsFor: 'operations' stamp: 'len 12/16/2024 07:55:47'!
inverse
	coefficients size = 1 ifTrue: [coefficients keysAndValuesDo: [:key :value| ^ self class coefficients: (Dictionary with: key inverse -> value inverse) parent: parent]].
	^ super inverse! !

!PathAlgebraElement methodsFor: 'printing' stamp: 'len 12/16/2024 07:57:08'!
printCoefficient: coefficient path: path on: aStream
	| c |
	c := coefficient.
	(path isIdentity or: [c isOne not]) ifTrue:
		[c printText first = $-
			ifTrue: [aStream nextPut: $-. c := c negated].
		(path isIdentity or: [c printText asString allSatisfy: [:each| each isAlphaNumeric or: ['{}()/' includes: each]]])
			ifTrue: [aStream print: c]
			ifFalse: [aStream nextPut: $(; print: c; nextPut: $)]].
	path isIdentity ifFalse: [aStream print: path]! !

!PathAlgebraElement methodsFor: 'printing' stamp: 'len 12/24/2024 08:28:58'!
printOn: aStream
	| keys leadingKey c |
	coefficients isEmpty ifTrue: [aStream print: 0. ^ self].
"	(self scalars isKindOf: ModularRing)
		ifTrue: [aStream print: self lift. ^ self].
	self isConstant ifTrue: [aStream print: self leadingCoefficient. ^ self]."
	keys := coefficients keys sorted: [:a :b| a printString <= b printString].
	c := coefficients at: (leadingKey := keys first).
	c printText first = $- ifTrue: [c := c negated. aStream nextPut: $-].
	self printCoefficient: c path: leadingKey on: aStream.
	keys allButFirstDo: [:path|
		c := coefficients at: path.
		c printText first = $-
			ifTrue: [aStream nextPutAll: ' - '. c := c negated]
			ifFalse: [aStream nextPutAll: ' + '].
		self printCoefficient: c path: path on: aStream]! !

!PathAlgebraElement methodsFor: 'testing' stamp: 'len 12/16/2024 07:48:32'!
isZero
	^ coefficients isEmpty! !

!PathAlgebraElement methodsFor: 'private' stamp: 'len 12/16/2024 07:50:37'!
coefficients: aDictionary parent: aPathAlgebra
	coefficients := aDictionary.
	parent := aPathAlgebra.
	self assert: parent notNil.
	self assert: (aDictionary noneSatisfy: [:none| none isZero])! !
