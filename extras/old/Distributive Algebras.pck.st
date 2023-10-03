'From Cuis 6.0 [latest update: #5898] on 2 October 2023 at 9:18:36 pm'!
'Description '!
!provides: 'Distributive Algebras' 1 0!
SystemOrganization addCategory: #'Distributive Algebras'!


!classDefinition: #DistributiveAlgebra category: #'Distributive Algebras'!
Structure subclass: #DistributiveAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Distributive Algebras'!
!classDefinition: 'DistributiveAlgebra class' category: #'Distributive Algebras'!
DistributiveAlgebra class
	instanceVariableNames: ''!

!classDefinition: #LieAlgebra category: #'Distributive Algebras'!
DistributiveAlgebra subclass: #LieAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Distributive Algebras'!
!classDefinition: 'LieAlgebra class' category: #'Distributive Algebras'!
LieAlgebra class
	instanceVariableNames: ''!

!classDefinition: #OctonionAlgebra category: #'Distributive Algebras'!
DistributiveAlgebra subclass: #OctonionAlgebra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Distributive Algebras'!
!classDefinition: 'OctonionAlgebra class' category: #'Distributive Algebras'!
OctonionAlgebra class
	instanceVariableNames: ''!

!classDefinition: #DistributiveAlgebraMap category: #'Distributive Algebras'!
Function subclass: #DistributiveAlgebraMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Distributive Algebras'!
!classDefinition: 'DistributiveAlgebraMap class' category: #'Distributive Algebras'!
DistributiveAlgebraMap class
	instanceVariableNames: ''!

!classDefinition: #DistributiveAlgebraElement category: #'Distributive Algebras'!
Object subclass: #DistributiveAlgebraElement
	instanceVariableNames: 'parent moduleElement representation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Distributive Algebras'!
!classDefinition: 'DistributiveAlgebraElement class' category: #'Distributive Algebras'!
DistributiveAlgebraElement class
	instanceVariableNames: ''!

!classDefinition: #Octonion category: #'Distributive Algebras'!
DistributiveAlgebraElement subclass: #Octonion
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Distributive Algebras'!
!classDefinition: 'Octonion class' category: #'Distributive Algebras'!
Octonion class
	instanceVariableNames: ''!


!DistributiveAlgebra commentStamp: '<historical>' prior: 0!
Nonassociative algebras over a commutative ring R.

A distributive algebra is constructed from a R-module, with multiplication given by a R-bilinear map. When the module is free of rank n, the multiplication can be given by n^3 structure constants (elements of R), or equivalently by n matrices of nxn.!

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2021 05:59:20'!
basis
	^ self propertyAt: #basis ifAbsentPut: [self asModule basis collect: [:each| self !! each]]! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2021 06:00:12'!
generators
	"Note: a basis is not necessarily a minimal set of generators (as algebra)."
	^ self propertyAt: #generators ifAbsent: [self basis]! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 9/16/2022 08:16:56'!
multiplication
	"Answer the multiplication as a bilinear map on the underlying module."
	^ self propertyAt: #multiplication ifAbsentPut: [BilinearMap from: self asModule, self asModule to: self asModule mapWithArguments: [:x :y| (self !! x) * (self !! y)]]! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 6/26/2023 12:59:30'!
null
	"Answer the trivial subalgebra, i.e. the zero object in the category of R-algebras."
	^ self ambient sub: #()! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2021 06:02:23'!
rank
	^ self asModule rank! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2021 06:02:28'!
scalars
	^ self asModule scalars! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2021 06:02:33'!
size
	^ self asModule size! !

!DistributiveAlgebra methodsFor: 'accessing' stamp: 'len 7/19/2021 06:05:21'!
zero
	^ self !! self asModule zero! !

!DistributiveAlgebra methodsFor: 'comparing' stamp: 'len 7/19/2021 05:58:05'!
< anAlgebra
	"Answer true if the receiver is strictly included in the argument."
	^ self asModule < anAlgebra asModule! !

!DistributiveAlgebra methodsFor: 'comparing' stamp: 'len 7/19/2021 05:58:10'!
<= anAlgebra
	"Answer true if the receiver is included in the argument."
	^ self asModule <= anAlgebra asModule! !

!DistributiveAlgebra methodsFor: 'comparing' stamp: 'len 7/19/2021 05:58:27'!
= anObject
	^ (anObject isKindOf: DistributiveAlgebra) and: [self ambient asModule = anObject ambient asModule and: [self asModule = anObject asModule]]! !

!DistributiveAlgebra methodsFor: 'converting' stamp: 'len 7/19/2021 05:59:06'!
asModule
	^ self propertyAt: #asModule! !

!DistributiveAlgebra methodsFor: 'elements' stamp: 'len 7/19/2021 05:59:01'!
adapt: anElement
	self isAmbient ifFalse: [^ super adapt: anElement].
	(self asModule includes: anElement) ifTrue: [^ self fromModuleElement: anElement].
	^ super adapt: anElement! !

!DistributiveAlgebra methodsFor: 'elements' stamp: 'len 7/19/2021 05:59:53'!
elementAt: anArray
	"Answer the element with the given coordinates."
	^ self fromModuleElement: (self asModule elementAt: anArray)! !

!DistributiveAlgebra methodsFor: 'elements' stamp: 'len 7/19/2021 06:00:06'!
fromModuleElement: anElement
	^ DistributiveAlgebraElement parent: self moduleElement: anElement! !

!DistributiveAlgebra methodsFor: 'elements' stamp: 'len 7/19/2021 06:01:59'!
newFrom: anObject
	(anObject isSequenceable and: [anObject size = self rank])
		ifTrue: [^ self elementAt: anObject].
	^ super newFrom: anObject! !

!DistributiveAlgebra methodsFor: 'enumerating' stamp: 'len 7/19/2021 05:59:48'!
do: aBlock
	self asModule do: [:each| aBlock value: self !! each]! !

!DistributiveAlgebra methodsFor: 'morphisms' stamp: 'len 7/19/2021 06:00:26'!
id
	"Answer the identity morphism."
	^ self propertyAt: #id ifAbsentPut: [self to: self moduleMap: self asModule id]! !

!DistributiveAlgebra methodsFor: 'morphisms' stamp: 'len 9/16/2022 07:58:18'!
to: aCodomain
	(aCodomain isDistributiveAlgebra and: [self isTrivial or: [aCodomain isTrivial]])
		ifTrue: [^ self to: aCodomain map: [:x| aCodomain zero]].
	^ super to: aCodomain! !

!DistributiveAlgebra methodsFor: 'morphisms' stamp: 'len 6/7/2023 15:23:06'!
to: anAlgebra images: aList
	"Answer a morphism defined by images of generators."
	((anAlgebra isKindOf: DistributiveAlgebra) and: [anAlgebra scalars = self scalars]) ifFalse: [^ super to: anAlgebra images: aList].
	^ self to: anAlgebra moduleMap: (self asModule to: anAlgebra asModule images: (aList isBlock ifTrue: [[:i| (aList value: i) asModuleElement]] ifFalse: [aList collect: [:each| each asModuleElement]]))! !

!DistributiveAlgebra methodsFor: 'morphisms' stamp: 'len 9/18/2022 07:06:29'!
to: anAlgebra map: aBlock
	((anAlgebra isKindOf: DistributiveAlgebra) and: [anAlgebra scalars = self scalars]) ifFalse: [^ super to: anAlgebra map: aBlock].
	^ self to: anAlgebra moduleMap: (self asModule to: anAlgebra asModule map: [:x| (aBlock value: self !! x) asModuleElement])! !

!DistributiveAlgebra methodsFor: 'morphisms' stamp: 'len 7/19/2021 06:05:15'!
to: anAlgebra moduleMap: aModuleMap
	((anAlgebra isKindOf: DistributiveAlgebra) and: [self scalars = anAlgebra scalars]) ifFalse: [^ DomainError signal].
	^ DistributiveAlgebraMap from: self to: anAlgebra moduleMap: aModuleMap! !

!DistributiveAlgebra methodsFor: 'operations' stamp: 'len 6/29/2022 07:30:09'!
× anAlgebra
	"Answer the direct product of the receiver with the argument."
	((anAlgebra isKindOf: DistributiveAlgebra) and: [self scalars = anAlgebra scalars]) ifTrue: [^ self ⊕ anAlgebra].
	^ super × anAlgebra! !

!DistributiveAlgebra methodsFor: 'operations' stamp: 'len 10/5/2022 17:21:05'!
⊕ anAlgebra
	"Answer the direct sum of the receiver with the argument."
	| answer coprojections projections |
	((anAlgebra isKindOf: DistributiveAlgebra) and: [self scalars = anAlgebra scalars]) ifFalse: [^ super ⊕ anAlgebra].
	answer _ self species multiplication: self multiplication ⊕ anAlgebra multiplication.
	projections _ answer asModule ⇉ {self asModule. anAlgebra asModule}.
	coprojections _ answer asModule ⇇ {self asModule. anAlgebra asModule}.
	answer
		addProjections: (projections with: {self. anAlgebra} collect: [:π :codomain| answer ⇒ codomain !! π]);
		addCoprojections: (coprojections with: {self. anAlgebra} collect: [:i :domain| domain ⇒ answer !! i]).
	^ answer! !

!DistributiveAlgebra methodsFor: 'operations' stamp: 'len 9/16/2022 08:19:50'!
opposite
	^ self propertyAt: #opposite ifAbsentPut: [(self species multiplication: (BilinearMap on: self asModule mapWithArguments: [:x :y| self multiplication value: y value: x])) propertyAt: #opposite put: self; yourself]! !

!DistributiveAlgebra methodsFor: 'operations' stamp: 'len 6/26/2023 12:47:26'!
sub: aCollection
	"Answer the subalgebra spanned by the elements in aCollection."
	self isAmbient ifFalse: [^ self ambient sub: aCollection].
	((aCollection isKindOf: Module) and: [aCollection ambient = self asModule])
		ifTrue: [^ self species new ambient: self; module: aCollection].
	^ self species new ambient: self; module: (self asModule sub: (aCollection collect: [:each| each asModuleElement]))! !

!DistributiveAlgebra methodsFor: 'operations' stamp: 'len 6/26/2023 12:59:37'!
∩ aSubalgebra
	"Answer the intersection of the receiver and the argument."
	((aSubalgebra isKindOf: DistributiveAlgebra) and: [self ambient = aSubalgebra ambient])
		ifFalse: [^ super ∩ aSubalgebra].
	^ self ambient sub: self asModule ∩ aSubalgebra asModule! !

!DistributiveAlgebra methodsFor: 'printing' stamp: 'len 6/3/2023 12:18:01'!
printOn: aStream
	self isAmbient ifTrue: [aStream print: self asModule. ^ self].
	self isTrivial ifTrue: [aStream print: 0. ^ self].
	aStream withAngleBrackets: [:aStream2|
		self generators
			do: [:each| aStream2 print: each]
			separatedBy: [aStream2 nextPutAll: '; ']]
! !

!DistributiveAlgebra methodsFor: 'random' stamp: 'len 7/19/2021 05:59:10'!
atRandom: aRandom
	^ self !! (self asModule atRandom: aRandom)! !

!DistributiveAlgebra methodsFor: 'random' stamp: 'len 7/19/2021 05:59:15'!
atRandom: aRandom bits: bitSize
	^ self !! (self asModule atRandom: aRandom bits: bitSize)! !

!DistributiveAlgebra methodsFor: 'testing' stamp: 'len 7/19/2021 05:59:36'!
contains: anElement
	self isAmbient ifTrue: [^ true].
	^ self asModule contains: anElement asModuleElement! !

!DistributiveAlgebra methodsFor: 'testing' stamp: 'len 7/19/2021 06:00:35'!
includes: anObject
	self isAmbient ifTrue: [^ anObject parent = self].
	^ (self ambient includes: anObject) and: [self contains: anObject]! !

!DistributiveAlgebra methodsFor: 'testing' stamp: 'len 12/15/2021 19:42:49'!
isAssociative
	^ self notYetImplemented! !

!DistributiveAlgebra methodsFor: 'testing' stamp: 'len 7/8/2022 10:48:00'!
isFinitelyGenerated
	^ true! !

!DistributiveAlgebra methodsFor: 'testing' stamp: 'len 7/18/2021 13:14:30'!
isLieAlgebra
	"Answer true if the receiver satisfies the axioms of a Lie algebra."
	self basis do: [:x|
		(x*x) isZero ifFalse: [^ false].
		self basis do: [:y|
			| xy |
			xy _ x*y.
			(xy + (y*x)) isZero ifFalse: [^ false].
			self basis do: [:z|
				"Jacobi identity:"
				(x*(y*z) + (y*(z*x)) + (z*xy)) isZero ifFalse: [^ false]]]].
	^ true! !

!DistributiveAlgebra methodsFor: 'testing' stamp: 'len 7/19/2021 06:01:07'!
isTrivial
	^ self asModule isTrivial! !

!DistributiveAlgebra methodsFor: 'private' stamp: 'len 7/19/2021 06:01:40'!
module: aModule
	self propertyAt: #asModule put: aModule! !

!DistributiveAlgebra methodsFor: 'private' stamp: 'len 9/16/2022 08:24:40'!
multiplication: aBilinearMap
	| V |
	V _ aBilinearMap domain at: 1.
	self propertyAt: #asModule put: V.
	self propertyAt: #multiplication put: aBilinearMap.
	self addCoercionTo: V map: [:each| each asModuleElement]! !

!DistributiveAlgebra methodsFor: 'private' stamp: 'len 7/19/2021 06:03:01'!
species
	^ DistributiveAlgebra! !

!DistributiveAlgebra class methodsFor: 'instance creation' stamp: 'len 7/19/2021 06:09:11'!
multiplication: aBilinearMap
	^ self new multiplication: aBilinearMap! !

!LieAlgebra methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 12:59:41'!
center
	| rows |
	rows := OrderedCollection new: self rank squared.
	self basis do: [:each| rows addAll: each representation rows].
	^ self sub: (Matrix rows: rows) kernel generators! !

!LieAlgebra methodsFor: 'as yet unclassified' stamp: 'len 7/15/2020 07:01:51'!
isUnital
	"Lie algebras are never unital."
	^ false! !

!OctonionAlgebra methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:36:17'!
fromModuleElement: anElement
	^ Octonion parent: self moduleElement: anElement! !

!OctonionAlgebra methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:07:45'!
isCommutative
	^ false! !

!OctonionAlgebra methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:07:28'!
isUnital
	^ true! !

!OctonionAlgebra methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:07:56'!
one
	^ self basis first! !

!OctonionAlgebra methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:09:11'!
printOn: aStream
	aStream nextPutAll: 'O('; print: self scalars; nextPut: $)! !

!OctonionAlgebra class methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 13:09:18'!
over: aField
	| V ε table |
	V _ aField^8.
	ε _ {#(2 3) -> 4. #(2 5) -> 6.  #(2 8) -> 7. #(3 5) -> 7. #(4 5) -> 8. #(4 7) -> 6} asDictionary.
	table _ (1 to: 8) collect: [:i| (1 to: 8) collect: [:j|
		i=1 ifTrue: [V x: j] ifFalse:
			[j=1 ifTrue: [V x: i] ifFalse:
				[i=j ifTrue: [(V x: 1) negated] ifFalse: [V x: (ε at: {i. j})]]]]].
	^ super multiplication: (BilinearMap on: V table: table)! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 7/18/2021 13:07:26'!
asModuleMap
	^ self propertyAt: #moduleMap! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 13:01:02'!
image
	^ self codomain sub: self asModuleMap image! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 6/26/2023 13:01:06'!
kernel
	^ self domain sub: self asModuleMap kernel! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 7/18/2021 13:07:46'!
moduleMap: aModuleMap
	self propertyAt: #moduleMap put: aModuleMap! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 7/8/2022 10:49:44'!
species
	^ DistributiveAlgebraMap! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 7/8/2022 11:16:19'!
validate
	"Since the map is linear by construction, we only need to verify that it preserves multiplication between elements of the basis, and that it sends the identity to the identity."
	super validate.
	self domain basis do: [:x|
		self domain basis do: [:y|
			self assert: (self value: x*y) = ((self value: x) * (self value: y))]]! !

!DistributiveAlgebraMap methodsFor: 'as yet unclassified' stamp: 'len 7/18/2021 13:07:55'!
value: anElement
	^ self codomain !! (self asModuleMap value: anElement asModuleElement)! !

!DistributiveAlgebraElement methodsFor: 'accessing' stamp: 'len 7/19/2021 06:12:52'!
parent
	^ parent! !

!DistributiveAlgebraElement methodsFor: 'comparing' stamp: 'len 7/19/2021 06:10:42'!
= anObject
	^ self class = anObject class and: [moduleElement = anObject asModuleElement]! !

!DistributiveAlgebraElement methodsFor: 'comparing' stamp: 'len 7/19/2021 06:11:38'!
hash
	^ moduleElement hash! !

!DistributiveAlgebraElement methodsFor: 'converting' stamp: 'len 7/19/2021 06:11:12'!
asModuleElement
	^ moduleElement! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:10:18'!
* anElement
	(anElement isInteger or: [self parent scalars includes: anElement])
		ifTrue: [^ self class parent: parent moduleElement: moduleElement * anElement].
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #*].
	^ self class parent: parent moduleElement: (parent multiplication value: moduleElement value: anElement asModuleElement)! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:10:28'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ self class parent: parent moduleElement: moduleElement + anElement asModuleElement! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 5/29/2022 19:39:19'!
/ anElement
	"Answer x such that xa = b, where a is the receiver and b is the argument.
	This is the right division of a by b, noted a / b."
	^ (self colift: anElement) ifNil: [DivisibilityError signalReceiver: self selector: #/ argument: anElement]! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 5/29/2022 19:39:19'!
\ anElement
	"Answer x such that ax = b, where a is the receiver and b is the argument.
	This is the left division of b by a, written a \ b."
	^ (self lift: anElement) ifNil: [DivisibilityError signalReceiver: self selector: #\ argument: anElement]! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:11:33'!
characteristicPolynomial
	^ self representation characteristicPolynomial! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2022 11:05:58'!
colift: anElement
	"Answer x such that xa = b, where a is the receiver and b is the argument. Answer nil if there's no solution.
	This is the right division of b by a, written b / a."
	parent isCommutative ifTrue: [^ self lift: anElement].
	^ self parent !! ((self corepresentation preimageRepresentative: anElement asModuleElement) ifNil: [^ nil])! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 9/16/2022 07:49:56'!
corepresentation
	"Answer the module endomorphism defined by right multiplication by the receiver."
	^ self parent asModule endomorphisms map: [:x| (self parent !! x * self) asModuleElement]! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2022 11:06:05'!
lift: anElement
	"Answer x such that ax = b, where a is the receiver and b is the argument. Answer nil if there's no solution.
	This is the left division of b by a, written a \ b."
	^ self parent !! ((self representation preimageRepresentative: anElement asModuleElement) ifNil: [^ nil])! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:12:38'!
minimalPolynomial
	^ self representation minimalPolynomial! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:12:42'!
negated
	^ self class parent: parent moduleElement: moduleElement negated! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:12:47'!
norm
	^ self representation determinant
	
"alternatively:
	| f |
	f _ self minimalPolynomial.
	^ f independentCoefficient / f leadingCoefficient * (f degree even ifTrue: [1] ifFalse: [-1])"! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 12/20/2021 13:05:57'!
opposite
	"Answer the receiver as an element of the opposite algebra."
	^ self parent opposite fromModuleElement: moduleElement! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 9/16/2022 07:50:02'!
representation
	"Answer the module endomorphism defined by left multiplication by the receiver."
	^ representation ifNil: [representation _ self parent asModule endomorphisms map: [:x| (self * (self parent !! x)) asModuleElement]]! !

!DistributiveAlgebraElement methodsFor: 'operations' stamp: 'len 7/19/2021 06:13:26'!
trace
	^ self representation trace

"alternatively:
	| f |
	f _ self minimalPolynomial.
	^ (f coefficientAtDegree: f degree - 1) negated"! !

!DistributiveAlgebraElement methodsFor: 'printing' stamp: 'len 7/19/2021 06:13:00'!
printOn: aStream
	aStream print: moduleElement! !

!DistributiveAlgebraElement methodsFor: 'testing' stamp: 'len 5/7/2022 07:29:27'!
isZeroDivisor
	"Answer true if there exists a nonzero element that multiplied by the receiver is zero."
"	self parent isIntegralDomain ifTrue: [^ self isZero]."
	^ self representation determinant isZero and: [self isCommutative or: [self corepresentation determinant isZero]]! !

!DistributiveAlgebraElement methodsFor: 'private' stamp: 'len 7/19/2021 06:12:56'!
parent: anAlgebra moduleElement: anElement
	parent _ anAlgebra.
	moduleElement _ anElement! !

!DistributiveAlgebraElement class methodsFor: 'instance creation' stamp: 'len 7/19/2021 06:09:42'!
parent: anAlgebra moduleElement: anElement
	^ self new parent: anAlgebra moduleElement: anElement! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:47:05'!
× anOctonion
	"Answer the cross-product of the receiver with the argument."
	^ (self commutator: anOctonion) / 2! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:38:18'!
asScalar
	self isScalar ifFalse: [^ self error: 'not a scalar element'].
	^ moduleElement at: 1! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:38:48'!
conjugate
	"Answer the conjugate of the receiver, such that the reduced trace is the conjugate added to the receiver, and the reduced norm is the conjugate times the receiver."
	^ self class parent: parent moduleElement: (moduleElement withIndexCollect: [:each :i| i=1 ifTrue: [each] ifFalse: [each negated]])

"alternatively:
	^ self negated + self trace
"! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:43:29'!
imaginary
	"Answer the imaginary part of the receiver."
	^ self - self real! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:44:32'!
inverse
	^ self conjugate * self norm inverse! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:40:27'!
norm
	"Answer the reduced norm of the receiver."
	^ (self * self conjugate) asModuleElement at: 1! !

!Octonion methodsFor: 'as yet unclassified' stamp: 'len 4/30/2021 12:43:24'!
real
	"Answer the real part of the receiver."
	^ self parent one * (moduleElement at: 1)! !
