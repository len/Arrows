'From Cuis7.3 [latest update: #7034] on 28 January 2025 at 8:56:53 pm'!
'Description '!
!provides: 'Clifford Algebra' 1 4!
SystemOrganization addCategory: #'Clifford Algebra'!


!classDefinition: #CliffordAlgebra category: #'Clifford Algebra'!
FiniteAlgebra subclass: #CliffordAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Clifford Algebra'!
!classDefinition: 'CliffordAlgebra class' category: #'Clifford Algebra'!
CliffordAlgebra class
	instanceVariableNames: ''!

!classDefinition: #Multivector category: #'Clifford Algebra'!
FiniteAlgebraElement subclass: #Multivector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Clifford Algebra'!
!classDefinition: 'Multivector class' category: #'Clifford Algebra'!
Multivector class
	instanceVariableNames: ''!


!Multivector commentStamp: 'len 7/15/2020 06:06:04' prior: 0!
Elements of a Clifford algebra or an exterior algebra. See CliffordAlgebra.!

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/5/2020 09:59:31'!
degree
	^ self space rank! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 12/1/2024 14:45:12'!
evenSubalgebra
	"Answer the subalgebra of the receiver spanned by the product of an even number of basis elements of the receiver. This are the fixed points of the main involution."
	^ self sub: (((1 to: self carrier rank) select: [:i| (i-1) bitParity = 0]) collect: [:i| self basis at: i])! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/7/2020 10:44:05'!
generators
	^ self propertyAt: #generators ifAbsentPut: [(0 to: self degree - 1) collect: [:i| self basis at: 1 << i + 1]]! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 12/28/2024 10:11:06'!
grade: anInteger
	"Answer the subspace of grade anInteger."
	^ self carrier sub: (((1 to: self carrier rank) select: [:i| (i - 1) bitCount = anInteger]) collect: [:i| self carrier basis at: i])! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 11/5/2022 13:45:22'!
one
	^ self basis first! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/22/2020 13:18:15'!
orthogonalBasis
	"Answer an orthogonal basis of the vector space V that the receiver acts on.
	The embedding of V in the receiver is built such that the i-th element of the orthogonal basis is sent to the (2^(i-1) + 1)-th element of the basis of the receiver."
	^ self propertyAt: #orthogonalBasis! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2020 20:14:50'!
pseudoscalar
	"Answer the unit pseudoscalar."
	^ self basis last! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/20/2020 06:33:54'!
pseudoscalarInverse
	^ self propertyAt: #pseudoscalarInverse ifAbsentPut: [self pseudoscalar inverse]! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/28/2020 11:00:15'!
quadraticForm
	"Answer the quadratic form associated to the receiver."
	^ self propertyAt: #quadraticForm! !

!CliffordAlgebra methodsFor: 'accessing' stamp: 'len 7/28/2020 11:00:59'!
space
	"Answer the space this Clifford algebra acts on."
	^ self quadraticForm domain! !

!CliffordAlgebra methodsFor: 'elements' stamp: 'len 12/1/2024 14:13:46'!
fromModuleElement: anElement
	^ Multivector parent: self carrier: anElement! !

!CliffordAlgebra methodsFor: 'morphisms' stamp: 'len 9/16/2022 07:57:21'!
antiautomorphism
	"Answer the main antiautomorphism of the receiver. This is the map that reverses the multiplication."
	^ self to: self map: [:x| x transpose]! !

!CliffordAlgebra methodsFor: 'morphisms' stamp: 'len 9/16/2022 07:57:26'!
involution
	"Answer the main involution of the receiver. This is an automorphism J such that J(u) = u if u is in the even subalgebra, and J(u) = -u if u is in the odd subspace."
	^ self to: self map: [:x| x reflection]! !

!CliffordAlgebra methodsFor: 'printing' stamp: 'len 8/12/2022 13:36:55'!
printOn: aStream
	aStream
		nextPutAll: (self isExteriorAlgebra ifTrue: ['⋀'] ifFalse: ['Cl']);
		nextPut: $(; print: self space; nextPutAll: ', '; print: self quadraticForm; nextPut: $)! !

!CliffordAlgebra methodsFor: 'testing' stamp: 'len 7/7/2020 19:48:03'!
isCommutative
	^ self space rank < 2! !

!CliffordAlgebra methodsFor: 'testing' stamp: 'len 7/28/2020 11:00:32'!
isExteriorAlgebra
	^ self quadraticForm isZero! !

!CliffordAlgebra methodsFor: 'testing' stamp: 'len 7/21/2021 16:01:40'!
isInvolutive
	^ true! !

!CliffordAlgebra methodsFor: 'private' stamp: 'len 12/1/2024 10:24:59'!
quadraticForm: aQuadraticForm
	| V n basis C T table |
	self propertyAt: #quadraticForm put: aQuadraticForm.
	V := aQuadraticForm domain.
	n := V rank.
	basis := aQuadraticForm polarization orthogonalizationOf: V basis.
	self propertyAt: #orthogonalBasis put: basis.
	C := V scalars ^ (2 ^ n).
	self carrier: C.
	"The embedding of V in the receiver is built such that the i-th element of the computed orthogonal basis is sent to the (2^(i-1) + 1)-th element of the basis of the receiver:"
	T := (V to: C images: ((1 to: n) collect: [:i| C basis at: 1 << (i-1) + 1])) · (V endomorphisms images: basis) inverse.
	self addCoercionFrom: V map: [:v| self fromModuleElement: (T value: v)].
	"Compute the multiplication table of the basis elements:"
	table := (0 to: self rank - 1) collect: [:x|
		(0 to: self rank - 1) collect: [:y|
			| scalar index |
			scalar := self scalars one.
			index := x.
			1 to: self degree do: [:i|
				(y bitAt: i) = 1 ifTrue:
					[(index >> i) bitParity = 1 ifTrue: [scalar := scalar negated].
					(index bitAt: i) = 1 ifTrue: [scalar := scalar * (aQuadraticForm value: (basis at: i))].
					index := index bitXor: 1 << (i - 1)]].
			{index + 1. scalar}]].
	self propertyAt: #basisMultiplicationTable put: table! !

!CliffordAlgebra class methodsFor: 'instance creation' stamp: 'len 7/28/2020 10:59:55'!
on: aQuadraticForm
	^ self new quadraticForm: aQuadraticForm! !

!Multivector methodsFor: 'accessing' stamp: 'len 12/1/2024 14:13:46'!
grade: anInteger
	"Answer the projection on grade anInteger. This is the homogeneous component of grade anInteger (an anInteger-vector)."
	| zero |
	zero _ parent scalars zero.
	^ self class parent: parent carrier: (carrier withIndexCollect: [:each :i| (i - 1) bitCount = anInteger ifTrue: [each] ifFalse: [zero]])! !

!Multivector methodsFor: 'converting' stamp: 'len 12/1/2024 14:10:59'!
asPolynomial
	| R answer |
	R _ self parent scalars polynomialsIn: ((1 to: self parent degree) collect: [:i| 'e', i printStringSubscript]).
	answer _ R zero.
	carrier withIndexDo: [:each :i| | term |
		term _ R !! each.
		1 to: parent degree do: [:j| (i-1 bitAt: j) = 1 ifTrue: [term _ term * (R x: j)]].
		answer _ answer + term].
	^ answer! !

!Multivector methodsFor: 'converting' stamp: 'len 12/1/2024 14:10:59'!
asScalar
	self isScalar ifFalse: [^ self error: 'not a scalar element'].
	^ carrier at: 1! !

!Multivector methodsFor: 'converting' stamp: 'len 12/1/2024 14:10:59'!
asVector
	| B answer |
	self isVector ifFalse: [^ self error: 'not a vector element'].
	B _ parent orthogonalBasis.
	answer _ parent space zero.
	1 to: parent degree do: [:i| answer _ (B at: i) * (carrier at: 1 << (i-1) + 1) + answer].
	^ answer! !

!Multivector methodsFor: 'operations' stamp: 'len 12/1/2024 14:13:46'!
* anElement
	"Answer the geometric product of the receiver with the argument.
	This is the main product operation of the Clifford algebra."
	(anElement isInteger or: [self parent scalars includes: anElement])
		ifTrue: [^ self class parent: parent carrier: carrier * anElement].
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #*].
	^ self product: anElement condition: [:i :j| true]! !

!Multivector methodsFor: 'operations' stamp: 'len 12/9/2024 12:53:01'!
|∙ anElement
	"Answer the right contraction (or right inner or interior product) of the receiver with the argument."
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #'|∙'].
	^ self product: anElement condition: [:i :j| (i bitAnd: j) = j]! !

!Multivector methodsFor: 'operations' stamp: 'len 12/9/2024 12:51:56'!
∙ anElement
	"Answer the dot product of the receiver with the argument."
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #∙].
	^ self product: anElement condition: [:i :j| i = j] "this can be optimized, lots of unnecesary iterations here"! !

!Multivector methodsFor: 'operations' stamp: 'len 12/9/2024 12:53:43'!
∙| anElement
	"Answer the left contraction (or left inner or interior product) of the receiver with the argument."
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #'∙|'].
	^ self product: anElement condition: [:i :j| (i bitAnd: j) = i]! !

!Multivector methodsFor: 'operations' stamp: 'len 8/12/2022 13:01:05'!
∧ anElement
	"Answer the exterior product (or outer product, or wedge product) of the receiver with the argument.
	This is the product of the exterior algebra contained in the parent of the receiver."
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #∧].
	^ self product: anElement condition: [:i :j| (i bitAnd: j) = 0]! !

!Multivector methodsFor: 'operations' stamp: 'len 8/12/2022 13:00:35'!
∨ anElement
	"Answer the regressive product of the receiver with the argument.
	This is the dual of the exterior product."
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #∨].
	^ (self dual ∧ anElement dual) * parent pseudoscalar! !

!Multivector methodsFor: 'operations' stamp: 'len 7/22/2020 15:59:30'!
conjugate
	"Answer the Clifford conjugate of the receiver."
	^ self reflection transpose! !

!Multivector methodsFor: 'operations' stamp: 'len 7/20/2020 06:34:01'!
dual
	^ self * parent pseudoscalarInverse! !

!Multivector methodsFor: 'operations' stamp: 'len 12/1/2024 14:49:53'!
product: anElement condition: conditionBlock
	| coordinates table |
	coordinates := parent carrier zero.
	table := parent propertyAt: #basisMultiplicationTable.
	self carrier withSupportDo: [:x :i|
		anElement carrier withSupportDo: [:y :j|
			| product |
			(conditionBlock value: i-1 value: j-1)
				ifTrue:
					[product := (table at: i) at: j.
					coordinates at: product first add: x * y * product second]]].
	^ self class parent: parent carrier: coordinates! !

!Multivector methodsFor: 'operations' stamp: 'len 12/1/2024 14:13:46'!
reflection
	"Answer the image of the reflection automorphism.
	This is also called grade involution or the (first) main involution, and it is denoted by a superscript star."
	^ self class parent: parent carrier: (carrier withIndexCollect: [:each :i| (i-1) bitParity = 1 ifTrue: [each negated] ifFalse: [each]])! !

!Multivector methodsFor: 'operations' stamp: 'len 12/1/2024 14:13:46'!
transpose
	"Answer the reversion of the receiver.
	This is also called the second main involution or the principal antiautomorphism, and it is denoted by a superscript dagger."
	^ self class parent: parent carrier: (carrier withIndexCollect: [:each :i| ((i-1) bitCount choose: 2) odd ifTrue: [each negated] ifFalse: [each]])! !

!Multivector methodsFor: 'printing' stamp: 'len 7/5/2020 08:52:03'!
printOn: aStream
	aStream print: self asPolynomial! !

!Multivector methodsFor: 'testing' stamp: 'len 12/1/2024 14:10:59'!
isBivector
	carrier supportDo: [:i| (i-1) bitCount = 2 ifFalse: [^ false]].
	^ true! !

!Multivector methodsFor: 'testing' stamp: 'len 7/6/2020 07:34:35'!
isEven
	^ self parent evenSubalgebra contains: self! !

!Multivector methodsFor: 'testing' stamp: 'len 12/1/2024 14:10:59'!
isParavector
	"A paravector is the sum of a vector and a scalar."
	^ (self parent degree + 2 to: self parent rank) allSatisfy: [:i| (carrier at: i) isZero]! !

!Multivector methodsFor: 'testing' stamp: 'len 12/1/2024 14:10:59'!
isPseudoscalar
	carrier supportDo: [:i| i = parent rank ifFalse: [^ false]].
	^ true! !

!Multivector methodsFor: 'testing' stamp: 'len 12/1/2024 14:10:59'!
isPseudovector
	| k |
	k _ parent degree - 1.
	carrier supportDo: [:i| (i-1) bitCount = k ifFalse: [^ false]].
	^ true! !

!Multivector methodsFor: 'testing' stamp: 'len 12/1/2024 14:10:59'!
isScalar
	carrier supportDo: [:i| i = 1 ifFalse: [^ false]].
	^ true! !

!Multivector methodsFor: 'testing' stamp: 'len 12/1/2024 14:10:59'!
isVector
	carrier supportDo: [:i| (i-1) bitCount = 1 ifFalse: [^ false]].
	^ true! !
