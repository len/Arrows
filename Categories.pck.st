'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 2:12:32 pm'!
'Description '!
!provides: 'Categories' 1 0!
!requires: 'Collections-Extras' 1 1 nil!
!requires: 'Collections-Iterator' 1 1 nil!
SystemOrganization addCategory: #Categories!


!classDefinition: #Domain category: #Categories!
Object subclass: #Domain
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Domain class' category: #Categories!
Domain class
	instanceVariableNames: ''!

!classDefinition: #Structure category: #Categories!
Domain subclass: #Structure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Structure class' category: #Categories!
Structure class
	instanceVariableNames: ''!

!classDefinition: #CartesianProduct category: #Categories!
Structure subclass: #CartesianProduct
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'CartesianProduct class' category: #Categories!
CartesianProduct class
	instanceVariableNames: ''!

!classDefinition: #Category category: #Categories!
Structure subclass: #Category
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Category class' category: #Categories!
Category class
	instanceVariableNames: ''!

!classDefinition: #HomSet category: #Categories!
Structure subclass: #HomSet
	instanceVariableNames: 'domain codomain'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'HomSet class' category: #Categories!
HomSet class
	instanceVariableNames: ''!

!classDefinition: #Naturals category: #Categories!
Structure subclass: #Naturals
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Naturals class' category: #Categories!
Naturals class
	instanceVariableNames: ''!

!classDefinition: #Morphism category: #Categories!
Object subclass: #Morphism
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Morphism class' category: #Categories!
Morphism class
	instanceVariableNames: ''!

!classDefinition: #Function category: #Categories!
Morphism subclass: #Function
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Function class' category: #Categories!
Function class
	instanceVariableNames: ''!

!classDefinition: #Functor category: #Categories!
Function subclass: #Functor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Categories'!
!classDefinition: 'Functor class' category: #Categories!
Functor class
	instanceVariableNames: ''!


!Domain commentStamp: '<historical>' prior: 0!
Objects of a given Category, connected by the corresponding Morphisms. They don't necessarily have an underlying set of elements. Concrete examples are algebraic structures such as Groups, Rings and Modules.

Domains can be derived from other domains, for example by taking products, coproducts, quotients, extensions, localizations, completions, duals, etc.

Domains can have subobjects: groups have subgroups, vector spaces have subspaces, rings have subrings, schemes have subschemes, etc. Subobjects of an ambient object can be implemented either concretely as subsets of the ambient object (i.e., their elements are elements of the ambient domain), or more abstractly as isomorphism classes of injective monomorphisms from another object of the same category into the ambient object.

Given two objects A and B in the same category, the set Hom(A,B) of morphisms between them is obtained by sending the message 'A hom: B' (see HomSet). The #hom: message is the external Hom(A,-) functor. The internal hom(A,-) functor is implemented as 'A => B'. The identity morphism is returned by the message #id.

Domains can be equipped with canonical and universal morphisms. For example, field extensions and localizations come with embeddings, and quotient objects come with projections, products have projection maps to their components, and coproducts have coprojection maps from their components.

More generally, canonical and universal morphisms are uniquely determined by their domain and codomain and can be retrieved by sending the message #->. For example, for any ring R, 'É -> R' returns the canonical ring homomorphism that sends an integer 'n' to 'n times the unit element of R'. In order for this mechanism to work, subclasses must sometimes extend the messages #to: and #from: to answer canonical and universal morphisms, and #toAll: and #fromAll: to answer projections and coprojections.!

!Structure commentStamp: '<historical>' prior: 0!
Domains with an underlying set of elements (the 'carrier' set). These can be simple sets, or algebraic structures such as Groups, Rings, Modules, etc. In terms of category theory, these are objects of concrete categories, i.e. they are realized as sets of elements endowed with certain algebraic structure, and their morphisms are structure-preserving Maps.

The underlying set of a domain can have finite or infinite cardinality, and whenever it makes sense they implement some *Collection* protocol (such as #do:, #size, #asSet, #atRandom:, etc).

There's also a simple coercion mechanism that allows the conversion of elements from a domain to another via the message #!!. Known canonical morphisms are used for automatic coercion of elements between domains. For example, for any unital ring R there's a canonical ring homomorphism from the ring of rational integers É to R that sends an integer 'n' to 'n times the unit element of R', and this means that coercion of an integer 'n' to an element of R can be performed simply as 'R !! n'. Subclasses can also extend the coercion mechanism by implementing methods for #adapt: and #newFrom:.!

!CartesianProduct commentStamp: '<historical>' prior: 0!
Cartesian products of sets. The elements of the cartesian product of two or more sets are arrays of elements in the sets corresponding to each component of the cartesian product.!

!Category commentStamp: '<historical>' prior: 0!
Categories as defined in the the single-sorted version of category theory. Their elements are Morphisms of a given type, and their morphisms are Functors.

Categories are commonly defined as two sets: a set of objects (Domains) and a set of arrows (Morphisms). However, identifying objects with their identity morphism it is possible to define categories as a single set of morphisms. With this definition, functors are simply functions between two categories that preserve composition of morphisms.

References:
	https://ncatlab.org/nlab/show/single-sorted+definition+of+a+category
!

!HomSet commentStamp: '<historical>' prior: 0!
Hom-Set objects Hom(A,B) for domain A and codomain B, i.e. sets of all morphisms between two Domains. When A = B, this is the set of endomorphisms of A and it's noted End(A).

Given two domains A and B, the Hom-Set can be retrieved with 'A hom: B'. If there's an internal Hom-Set in the category of A and B, it can be retrieved with 'A => B'.!

!Naturals commentStamp: '<historical>' prior: 0!
The singleton global instance Ç is the set of natural numbers 1, 2, 3, ...!

!Morphism commentStamp: '<historical>' prior: 0!
Morphisms belonging to a given Category. See Map and other subclasses for morphisms of specific categories, and Domain and HomSet.!

!Function commentStamp: '<historical>' prior: 0!
Functions, unlike general Morphisms, can be evaluated at elements of the domain (#value:) and they produce elements of the codomain. An instance of Function is a morphism in the category of sets, while instances of subclasses can be morphisms in other categories with underlying sets.

In general, maps are defined by specifying a domain, a codomain, and an expression that can be evaluated at elements of the domain and produces elements of the codomain. One such expression can be a Smalltalk block, another Map, or more generally any Smalltalk object that admits evaluation (#value:), such as polynomials.!

!Functor commentStamp: '<historical>' prior: 0!
A functor between two categories is a function that preserves composition of morphisms. They are morphisms of categories (see Category).

References:
	https://ncatlab.org/nlab/show/single-sorted+definition+of+a+category
!

!Domain methodsFor: 'accessing' stamp: 'len 7/29/2020 08:34:23'!
category
	^ self propertyAt: #category ifAbsentPut: [self id category]! !

!Structure methodsFor: 'copying' stamp: 'len 3/19/2016 22:41'!
postCopy
	super postCopy.
	properties _ properties copy! !

!Structure methodsFor: 'printing' stamp: 'len 5/1/2020 14:11:05'!
printOn: aStream
	self propertyAt: #elements ifPresent: [:aCollection|
		(aCollection isKindOf: Structure) ifTrue: [aStream print: aCollection. ^ self].
		aStream nextPut: ${. "Wildberger notation for unordered sets: elements separated by spaces"
		aCollection asSet do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ' '].
		aStream nextPut: $}.
		^ self].
	^ super printOn: aStream! !

!CartesianProduct methodsFor: 'printing' stamp: 'len 5/23/2020 07:18:55'!
printOn: aStream
	components do: [:each| aStream print: each] separatedBy: [aStream nextPut: $◊]! !

!Category methodsFor: 'accessing' stamp: 'len 4/26/2020 17:30:11'!
name
	^ self propertyAt: #name! !

!Category methodsFor: 'printing' stamp: 'len 4/26/2020 17:31:22'!
printOn: aStream
	aStream nextPutAll: self name! !

!HomSet methodsFor: 'printing' stamp: 'len 12/5/2016 11:56:13'!
printOn: aStream
	self isEndomorphisms
		ifTrue: [aStream nextPutAll: 'End'; nextPut: $(; print: self domain; nextPut: $)]
		ifFalse: [aStream nextPutAll: 'Hom'; nextPut: $(; print: self domain; nextPut: $,; print: self codomain; nextPut: $)]! !

!Naturals methodsFor: 'printing' stamp: 'len 5/23/2020 07:23:35'!
printOn: aStream
	aStream nextPut: $Ç! !

!Morphism methodsFor: 'accessing' stamp: 'len 5/1/2020 18:32:45'!
category
	^ self subclassResponsibility! !

!Morphism methodsFor: 'printing' stamp: 'len 4/22/2020 14:36:36'!
name
	^ self propertyAt: #name ifAbsent: [String streamContents: [:aStream| super printOn: aStream]]! !

!Morphism methodsFor: 'printing' stamp: 'len 5/1/2020 22:17:52'!
printOn: aStream
	(self propertyAt: #isIdentity ifAbsent: [false]) ifTrue: [aStream nextPutAll: 'id'. ^ self].
	aStream nextPutAll: self name; nextPut: $:.
	self arrowPrintOn: aStream! !

!Function methodsFor: 'accessing' stamp: 'len 12/23/2021 13:45:28'!
category
	^ Sets! !

!Function methodsFor: 'printing' stamp: 'len 12/23/2021 15:29:25'!
printOn: aStream
	self propertyAt: #expression ifPresent:
		[:anObject| (anObject isBlock not and: [(anObject isKindOf: Function) not])
			ifTrue: [aStream print: anObject. ^ self]].
	^ super printOn: aStream! !

!Functor methodsFor: 'accessing' stamp: 'len 4/26/2020 17:40:38'!
category
	^ Categories! !

!Domain methodsFor: 'accessing' stamp: 'len 4/22/2020 05:45:14'!
arity
	(self isProduct or: [self isCoproduct]) ifFalse: [^ DomainError signal: 'not a product or coproduct'].
	^ self components size! !

!Domain methodsFor: 'accessing' stamp: 'len 4/22/2020 05:45:41'!
at: anInteger
	(self isProduct or: [self isCoproduct]) ifFalse: [^ DomainError signal: 'not a product or coproduct'].
	^ self components at: anInteger! !

!Domain methodsFor: 'morphisms' stamp: 'len 3/13/2021 09:53:12'!
-< aProduct
	"Answer the canonical projections from the receiver into the components of aProduct.
	See also message >-."
	| components |
	components _ aProduct isArray ifTrue: [aProduct] ifFalse: [aProduct components].
	self propertyAt: #projections ifPresent: [:aDictionary| aDictionary at: components ifPresent: [:anArrayOfMorphisms| ^ anArrayOfMorphisms]].
	^ self toAll: components! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/23/2021 17:37:55'!
-> aCodomain
	"Answer the canonical morphism from the receiver to aCodomain.
	If the argument is a morphism, answer the composition with a canonical morphism to its domain. See also Morphism>>->."
	aCodomain isMorphism ifTrue:
		[self = aCodomain domain ifTrue: [^ aCodomain].
		^ self -> aCodomain domain ifNotNil: [:aMorphism| aCodomain ∑ aMorphism]].
	self propertyAt: #outputs ifPresent: [:aDictionary| aDictionary at: aCodomain ifPresent: [:aMorphism| ^ aMorphism]].
	aCodomain propertyAt: #inputs ifPresent: [:aDictionary| aDictionary at: self ifPresent: [:aMorphism| ^ aMorphism]].
	^ (self to: aCodomain) ifNil: [aCodomain from: self]! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/25/2021 11:54:57'!
=> anObject
	"This is the Hom(A,-) functor, where A is the receiver:
	- if the argument is a domain X, answer Hom(A,X), the morphisms from A to X;
	- if the argument is a morphism g:X->Y, answer the morphism Hom(A,g):Hom(A,X)->Hom(A,Y) that sends the morphism f to g∑f.
	When appropriate, subclasses can implement #hom: to answer the internal Hom."
	anObject isMorphism ifTrue: [^ self => anObject domain to: self => anObject codomain evaluating: [:f| anObject ∑ f]].
	^ (self propertyAt: #homs ifAbsentPut: [WeakIdentityKeyDictionary new]) at: anObject ifAbsentPut: [self hom: anObject]! !

!Domain methodsFor: 'morphisms' stamp: 'len 3/4/2021 18:06:32'!
>- aCodomain
	"Answer the canonical coprojections from the components of the receiver to the argument.
	See also message -<."
	^ self components >- aCodomain! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/23/2021 19:01:16'!
addCoprojections: anArray
	anArray do: [:each| each codomain == self ifFalse: [^ DomainError signal]].
	^ (self propertyAt: #coprojections ifAbsentPut: [Dictionary new])
		at: (anArray collect: [:each| each domain]) put: anArray! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/25/2021 12:50:42'!
addMap: aMorphism
"	(self category includes: aMorphism) ifFalse: [Error signal: 'not a morphism']."
	aMorphism domain == self
		ifTrue: [^ (self propertyAt: #outputs ifAbsentPut: [WeakIdentityKeyDictionary new]) at: aMorphism codomain put: aMorphism].
	aMorphism codomain == self
		ifTrue: [^ (self propertyAt: #inputs ifAbsentPut: [WeakIdentityKeyDictionary new]) at: aMorphism domain put: aMorphism].
	^ DomainError signal! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/23/2021 19:01:07'!
addProjections: anArray
	anArray do: [:each| each domain == self ifFalse: [^ DomainError signal]].
	^ (self propertyAt: #projections ifAbsentPut: [Dictionary new])
		at: (anArray collect: [:each| each codomain]) put: anArray! !

!Domain methodsFor: 'morphisms' stamp: 'len 7/31/2020 11:49:25'!
automorphisms
	^ self subclassResponsibility! !

!Domain methodsFor: 'morphisms' stamp: 'len 4/22/2020 05:49:13'!
endomorphisms
	^ self => self! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/22/2021 15:13:59'!
from: aDomain
	"Answer the canonical morphism from aDomain to the receiver."
	^ nil! !

!Domain methodsFor: 'morphisms' stamp: 'len 4/22/2020 05:49:26'!
fromAll: anArray
	"Answer the canonical coprojections from the components of anArray to the receiver."
	(self isCoproduct and: [self components = anArray])
		ifTrue: [^ self addCoprojections: ((1 to: self arity) collect: [:i| self coprojection: i])].
	^ nil! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/24/2021 08:26:20'!
hom: aCodomain
	"If the argument is a domain X in the same category, answer Hom(A,X), the morphisms from A to X.
	When appropriate, subclasses can implement #hom: to answer an internal Hom.
	See also >>=>."
	(aCodomain isKindOf: ChainComplex)
		ifTrue: [^ ChainComplex differentials: (aCodomain differentials collect: [:each| self => each]) shift: aCodomain shift].
	^ HomSet from: self to: aCodomain! !

!Domain methodsFor: 'morphisms' stamp: 'len 4/22/2020 05:50:03'!
id
	"Answer the identity morphism."
	^ self subclassResponsibility! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/23/2021 17:55:50'!
to: aCodomain
	"Answer the canonical morphism from the receiver to aCodomain."
	self = aCodomain ifTrue: [^ self id]. "should be =="
	^ nil! !

!Domain methodsFor: 'morphisms' stamp: 'len 12/25/2021 11:12:28'!
to: aCodomain adapting: anObject
	"Answer a morphism from the receiver to aCodomain induced by anObject, or nil."
	^ nil! !

!Domain methodsFor: 'morphisms' stamp: 'len 4/22/2020 07:05:49'!
toAll: anArray
	"Answer the canonical projections from the receiver to the components of anArray."
	(self isProduct and: [anArray = self components])
		ifTrue: [^ self addProjections: ((1 to: self arity) collect: [:i| self projection: i])].
	^ nil! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:53:39'!
hasProperty: aSymbol
	^ (properties ifNil: [^ false]) includesKey: aSymbol! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:53:56'!
properties
	^ properties ifNil: [properties _ IdentityDictionary new]! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:54:02'!
propertyAt: aSymbol
	^ self propertyAt: aSymbol ifAbsent: [self error: 'no such property']! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:54:07'!
propertyAt: aSymbol ifAbsent: exceptionBlock
	^ (properties ifNil: [^ exceptionBlock value]) at: aSymbol ifAbsent: exceptionBlock! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:54:12'!
propertyAt: aSymbol ifAbsentPut: aBlock
	^ self properties at: aSymbol ifAbsentPut: aBlock! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:54:19'!
propertyAt: aSymbol ifPresent: aBlock
	^ (properties ifNil: [^ nil]) at: aSymbol ifPresent: aBlock! !

!Domain methodsFor: 'properties' stamp: 'len 4/22/2020 05:54:25'!
propertyAt: aSymbol put: anObject
	^ self properties at: aSymbol put: anObject! !

!Domain methodsFor: 'testing' stamp: 'len 7/6/2020 14:13:14'!
isCartesianProduct
	^ false! !

!Domain methodsFor: 'testing' stamp: 'len 12/25/2021 12:31:15'!
isCoproduct
	"Answer true if the receiver is a coproduct construction."
	^ false! !

!Domain methodsFor: 'testing' stamp: 'len 12/25/2021 12:31:05'!
isProduct
	"Answer true if the receiver is a product construction."
	^ false! !

!Domain methodsFor: 'testing' stamp: 'len 12/25/2021 12:30:37'!
isQuotient
	"Answer true if the receiver is a quotient construction."
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:32:39'!
isAbelianGroup
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 7/15/2020 10:29:37'!
isAgebra
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:33:10'!
isCategory
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:32:43'!
isGroup
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:32:53'!
isModule
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:32:57'!
isMonoid
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:33:00'!
isRing
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 7/31/2020 11:45:28'!
isSet
	^ false! !

!Domain methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:33:05'!
isVectorSpace
	^ false! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
example1asSet
	"The ring Z/5Z as a Smalltalk Set.
	This is possible because the ring is finite."
	^ (É/5) asSet! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
example1atRandom
	"Taking an element at random from the Z-module of rank 10.
	Use 50 bits to generate this element. Essentially, this can return
	at most 2^50 different elements, and their bitsize doesn't exceed 50
	bits (where bitsize is losely defined, in this case it's the sum
	of bitsize of the coefficients of the coordinates)."
	^ É^10 atRandom: (Random seed: 12345) bits: 50! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
example21VectorSpace
	"The vector space of 3-tuples with rational coefficients."
	^ Ñ ^ 3! !

!Domain class methodsFor: 'examples' stamp: 'len 4/29/2020 17:29:40'!
example3DirectProductOfGroups
	"The direct product of the group of permutations of
	the symbols {#a, #b, #c} by the group of alternating
	permutations of {1,2,3,4}."
	^ (SymmetricGroup on: #(a b c)) ◊ (AlternatingGroup new: 4)! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
example41Polynomials
	"Polynomials in two variables with integer coefficients."
	^ É polynomialsIn: #(a b)! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:03:31'!
example42AffineRing
	"The quotient of a polynomial ring by an ideal.
	This is an affine ring, in particular it is the coordinate ring of the twisted cubic."
	| R x y z I |
	R _ Ü polynomialsIn: #(x y z).
	x _ R x: 1.
	y _ R x: 2.
	z _ R x: 3.
	I _ R * {(x^2) - y. (x^3) - z}.
	^ R / I! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:03:36'!
example43AffineVariety
	"This is the twisted cubic affine variety, parameterized by (x, x^2, x^3) in C^3."
	| R x y z I |
	R _ Ü polynomialsIn: #(x y z).
	x _ R x: 1.
	y _ R x: 2.
	z _ R x: 3.
	I _ R * {(x^2) - y. (x^3) - z}.
	^ (R / I) spec! !

!Domain class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
example5GL
	"The group GL(Q^3) of invertible linear maps
	on the vector space of 3-tuples with rational coefficients.
	This is the automorphisms group of the vector space, and
	its elements are linear maps, not matrices."
	^ (Ñ ^ 3) automorphisms! !

!Domain class methodsFor: 'initialization' stamp: 'len 12/29/2021 14:06:39'!
initializeAll
	"First initialize all Domains, set globals like É, Ñ, etc:"
	Domain withAllSubclassesDo: [:each| (each class selectors includes: #initialize) ifTrue: [each initialize]].
	"Then initialize all Morphisms, set globals for categories like Sets, Groups, etc:"
	Morphism withAllSubclassesDo: [:each| (each class selectors includes: #initialize) ifTrue: [each initialize]].
	"And finally initialize any other classes that need initialization:"
	BitTuple initialize.
	Ordinal initialize! !

!Structure methodsFor: 'accessing' stamp: 'len 4/22/2020 07:02:11'!
ambient
	"Domains can be subobjects of other domains, for example subspeces are subobjects of vector spaces.
	Answer the ambient domain of which the receiver is a subobject."
	^ self propertyAt: #ambient ifAbsent: [self]! !

!Structure methodsFor: 'accessing' stamp: 'len 10/17/2020 20:11:19'!
carrier
	"Answer the carrier set of the receiver. This is the forgetful functor to the category of sets."
	self isSet ifTrue: [^ self].
	^ self propertyAt: #carrier ifAbsentPut: [Structure newFrom: self]! !

!Structure methodsFor: 'accessing' stamp: 'len 12/28/2016 11:23:04'!
elements
	self propertyAt: #elements ifPresent: [:aCollectionOrDomain| ^ aCollectionOrDomain].
	^ Iterator on: self performing: #do:! !

!Structure methodsFor: 'accessing' stamp: 'len 10/6/2016 10:26'!
generator
	self generators size = 1 ifFalse: [self error: 'more than one generator'].
	^ self generators anyOne! !

!Structure methodsFor: 'accessing' stamp: 'len 10/17/2016 18:53'!
generator: anElement
	self generators: {anElement}! !

!Structure methodsFor: 'accessing' stamp: 'len 12/22/2021 14:10:56'!
generatorNamed: anObject
	"Answer the generator indexed or named by anObject."
	^ self generators at: (self names indexOf: anObject)! !

!Structure methodsFor: 'accessing' stamp: 'len 12/28/2016 11:22:22'!
generators
	"Subclases can use this or reimplement it when it makes sense."
	^ self propertyAt: #generators ifAbsent: []! !

!Structure methodsFor: 'accessing' stamp: 'len 12/28/2016 11:19:29'!
generators: aCollection
	"Subclases can use this or reimplement it when it makes sense."
	self propertyAt: #generators put: aCollection asArray! !

!Structure methodsFor: 'accessing' stamp: 'len 7/24/2020 08:02:16'!
names
	"Answer a list of symbolic names for the generators."
	^ self propertyAt: #names ifAbsentPut: [self generators size = 1 ifTrue: [#(x)] ifFalse: [(1 to: self generators size) collect: [:i| 'x', i printString sub]]]! !

!Structure methodsFor: 'accessing' stamp: 'len 12/25/2021 11:45:10'!
null
	"Answer the null or zero object in the category of the receiver, i.e. an object that is both intial and terminal. For example {id} in the case of groups (the trivial group)."
	"The null object in the category of sets is the empty set."
	^ self propertyAt: #null ifAbsent: [self species newFrom: (Set new: 0)]! !

!Structure methodsFor: 'accessing' stamp: 'len 6/5/2019 19:55:56'!
size
	^ self propertyAt: #size ifAbsentPut: [self elements size]! !

!Structure methodsFor: 'comparing' stamp: 'len 4/19/2020 07:04:52'!
< aDomain
	"Answer true if the receiver is a proper subset of aDomain (strict inclusion)."
	aDomain isFinite ifTrue: [^ self size < aDomain size and: [self allSatisfy: [:each| aDomain includes: each]]].
	self isFinite ifTrue: [^ self allSatisfy: [:each| aDomain includes: each]].
	^ self subclassResponsibility! !

!Structure methodsFor: 'comparing' stamp: 'len 6/22/2019 02:56:21'!
<= aDomain
	"Answer true if the receiver is a subset of aDomain (inclusion)."
	^ self = aDomain or: [self < aDomain]! !

!Structure methodsFor: 'comparing' stamp: 'len 3/26/2021 17:20:28'!
= aDomain
	self == aDomain ifTrue: [^ true].
	self propertyAt: #elements ifPresent: [:aCollectionOrDomain| ^ aDomain = aCollectionOrDomain].
	aDomain propertyAt: #elements ifPresent: [:aCollectionOrDomain| ^ self = aCollectionOrDomain].
	self size = aDomain size ifFalse: [^ false].
	self isFinite ifTrue: [^ self asSet = aDomain asSet].
	^ super = aDomain! !

!Structure methodsFor: 'comparing' stamp: 'len 6/22/2019 02:56:33'!
> aDomain
	"Answer true if aDomain is a proper subset of the receiver (strict inclusion)."
	^ aDomain < self! !

!Structure methodsFor: 'comparing' stamp: 'len 6/22/2019 02:56:41'!
>= aDomain
	"Answer true if aDomain is a subset of the receiver (inclusion)."
	^ aDomain <= self! !

!Structure methodsFor: 'comparing' stamp: 'len 7/31/2020 11:53:56'!
~ aDomain
	"Answer true if the receiver is isomorphic to the argument."
	(self isSet and: [aDomain isSet]) ifFalse: [^ self subclassResponsibility].
	^ self size = aDomain size! !

!Structure methodsFor: 'comparing' stamp: 'len 4/23/2016 00:41'!
hash
	^ self isFinite ifTrue: [self size hash] ifFalse: [super hash]! !

!Structure methodsFor: 'converting' stamp: 'len 4/19/2020 07:08:55'!
asArray
	^ self elements asArray! !

!Structure methodsFor: 'converting' stamp: 'len 10/17/2020 20:05:43'!
asCartesianProduct
	^ self propertyAt: #asCartesianProduct ifAbsentPut: [CartesianProduct components: self components]! !

!Structure methodsFor: 'converting' stamp: 'len 4/19/2020 07:09:10'!
asSet
	^ self elements asSet! !

!Structure methodsFor: 'elements' stamp: 'len 7/23/2021 10:38:25'!
!! anObject
	"Coerce anObject to an element of the receiver."
	(self includes: anObject) ifTrue: [^ anObject].
	anObject isElement ifTrue: [(self adapt: anObject) ifNotNil: [:anElement| ^ anElement]].
	anObject isArray ifTrue: [self carrier propertyAt: #inputs ifPresent: [:aDictionary| aDictionary keysAndValuesDo: [:domain :map| (domain includes: anObject) ifTrue: [^ map value: anObject]]]].
	(self newFrom: anObject) ifNotNil: [:anElement| ^ anElement].
	(anObject isBlock and: [self generators notNil and: [anObject numArgs = self generators size]]) ifTrue: [^ self generate: anObject].
	^ DomainError signal: 'coercion failed'! !

!Structure methodsFor: 'elements' stamp: 'len 7/19/2021 17:25:52'!
adapt: anElement
	"Coerce anElement *not* in the receiver to an element of the receiver. Subclasses should extend."
	self isAmbient ifFalse: [(self ambient adapt: anElement) ifNotNil: [:coerced| (self contains: coerced) ifTrue: [^ coerced]]].
	self category = anElement parent category ifTrue: [anElement parent -> self ifNotNil: [:aMorphism| ^ aMorphism value: anElement]].
"	self propertyAt: #inputCoercions ifPresent: [:aDictionary| aDictionary at: anElement parent ifPresent: [:aMorphism| ^ aMorphism]].
	anElement parent propertyAt: #outputCoercions ifPresent: [:aDictionary| aDictionary at: self ifPresent: [:aMorphism| ^ aMorphism]]."
	anElement parent carrier -> self carrier ifNotNil: [:aMorphism| ^ aMorphism value: anElement].
	^ nil! !

!Structure methodsFor: 'elements' stamp: 'len 7/22/2021 18:47:03'!
addCoercionFrom: aDomain evaluating: aBlock
	^ self carrier addMap: (aDomain carrier to: self carrier evaluating: aBlock)! !

!Structure methodsFor: 'elements' stamp: 'len 7/22/2021 18:46:57'!
addCoercionFrom: aCartesianProduct evaluatingWithArguments: aBlock
	^ self carrier addMap: (aCartesianProduct to: self carrier evaluatingWithArguments: aBlock)! !

!Structure methodsFor: 'elements' stamp: 'len 7/22/2021 18:46:50'!
addCoercionTo: aDomain evaluating: aBlock
	^ self carrier addMap: (self carrier to: aDomain carrier evaluating: aBlock)! !

!Structure methodsFor: 'elements' stamp: 'len 12/26/2016 11:31:05'!
generate: aBlock
	"Answer the result of evaluating aBlock on the generators of the receiver."
	^ aBlock valueWithArguments: self generators asArray! !

!Structure methodsFor: 'elements' stamp: 'len 5/10/2020 16:24:03'!
newFrom: anObject
	((self isProduct or: [self isCoproduct]) and: [anObject isSequenceable and: [anObject size = self arity]])
		ifTrue: [^ self compose: (self components with: anObject collect: [:eachDomain :eachElement| eachDomain !! eachElement])].
	^ nil! !

!Structure methodsFor: 'elements' stamp: 'len 5/13/2019 14:18:36'!
x: anInteger
	"Answer the anInteger-th generator of the receiver."
	^ self generators at: anInteger! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/1/2016 23:22'!
allSatisfy: aBlock
	self do: [:each| (aBlock value: each) ifFalse: [^ false]].
	^ true! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/1/2016 23:21'!
anySatisfy: aBlock
	self do: [:each| (aBlock value: each) ifTrue: [^ true]].
	^ false! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/10/2020 23:06:40'!
collect: aBlock
	| elements |
	elements _ self propertyAt: #elements ifAbsent: [self].
	^ Iterator on: [:iteratorBlock|
		| visited image |
		visited _ Set new.
		elements do: [:each| visited add: (image _ aBlock value: each) ifAbsent: [iteratorBlock value: image]]]! !

!Structure methodsFor: 'enumerating' stamp: 'len 9/16/2018 16:11:56'!
count: aBlock
	| answer |
	answer _ 0.
	self do: [:each| (aBlock value: each) ifTrue: [answer _ answer + 1]].
	^ answer! !

!Structure methodsFor: 'enumerating' stamp: 'len 7/29/2016 06:45'!
detect: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true."

	^ self detect: aBlock ifNone: [self error: 'Element is not in the domain']! !

!Structure methodsFor: 'enumerating' stamp: 'len 9/3/2016 07:19'!
detect: aBlock ifNone: exceptionBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true. If none 
	evaluate to true, then evaluate the argument, exceptionBlock."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^ exceptionBlock value! !

!Structure methodsFor: 'enumerating' stamp: 'len 12/28/2016 11:21:06'!
do: aBlock
	(self propertyAt: #elements ifAbsent: [self subclassResponsibility]) do: aBlock! !

!Structure methodsFor: 'enumerating' stamp: 'len 4/25/2016 09:44'!
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

!Structure methodsFor: 'enumerating' stamp: 'len 2/15/2017 11:26:26'!
max: aBlock
	^ self elements max: aBlock! !

!Structure methodsFor: 'enumerating' stamp: 'len 2/15/2017 11:26:35'!
min: aBlock
	^ self elements min: aBlock! !

!Structure methodsFor: 'enumerating' stamp: 'len 4/28/2018 05:05:39'!
noneSatisfy: aBlock
	self do: [:each| (aBlock value: each) ifTrue: [^ false]].
	^ true! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/3/2018 01:29:25'!
product: aBlock
	^ self elements product: aBlock! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/10/2020 23:07:16'!
reject: aBlock
	| elements |
	elements _ self propertyAt: #elements ifAbsent: [self].
	^ Iterator on: [:iteratorBlock| elements do: [:each| (aBlock value: each) ifFalse: [iteratorBlock value: each]]]! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/10/2020 23:07:40'!
select: aBlock
	| elements |
	elements _ self propertyAt: #elements ifAbsent: [self].
	^ Iterator on: [:iteratorBlock| elements do: [:each| (aBlock value: each) ifTrue: [iteratorBlock value: each]]]! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/3/2018 01:29:16'!
sum: aBlock
	^ self elements sum: aBlock! !

!Structure methodsFor: 'enumerating' stamp: 'len 3/17/2021 14:24:58'!
tuples: n do: aBlock
	"Iterate over n-tuples without repetitions.
	The receiver can have infinite size, in which case this never ends. However, following the ordering of the iteration on single elements implemented in #do:, it is guaramteed that at some point in finite time all tuples of elements < to a given element will have been covered."
	self tuples: n upTo: self size do: aBlock! !

!Structure methodsFor: 'enumerating' stamp: 'len 5/28/2020 18:01:41'!
tuples: n upTo: max do: aBlock
	"Iterate over the n-tuples up to max elements."
	| k |
	n = 0 ifTrue: [^ aBlock value: #()].
	k _ 1.
	self upTo: max do: [:s|
		self  tuples: n-1 upTo: k do: [:p|
			aBlock value: (p copyWith: s).
			(p includes: s) ifTrue: [self upTo: k-1 do: [:t| aBlock value: (p copyWith: t)]]].
		k _ k + 1]! !

!Structure methodsFor: 'enumerating' stamp: 'len 8/1/2016 03:53'!
upTo: maximumNumberOfElements
	| answer |
	answer _ OrderedCollection new.
	self do: [:each|
		answer size >= maximumNumberOfElements ifTrue: [^ answer].
		answer add: each].
	^ answer! !

!Structure methodsFor: 'enumerating' stamp: 'len 7/25/2016 23:46'!
upTo: max do: aBlock
	"Iterate over the elements of the receiver, just like #do:, but stop after max elements."
	| k |
	k _ 1.
	self do: [:each|
		k > max ifTrue: [^ self].
		aBlock value: each.
		k _ k+1]! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/13/2019 17:12:18'!
apply: aMorphism
	"Answer the homomorphic image of the receiver."
	^ (aMorphism isBlock ifTrue: [self] ifFalse: [aMorphism codomain]) span: ((self generators ifNil: [^ self collect: aMorphism]) apply: aMorphism)! !

!Structure methodsFor: 'morphisms' stamp: 'len 7/31/2020 11:49:07'!
automorphisms
	self isSet ifFalse: [^ super automorphisms].
	^ SymmetricGroup on: self! !

!Structure methodsFor: 'morphisms' stamp: 'len 7/9/2021 06:39:32'!
id
	"Answer the identity morphism."
	^ self propertyAt: #id ifAbsentPut:
		[| answer |
		answer _ self to: self evaluating: [:x| x].
		answer properties
			at: #isIdentity put: true;
			at: #retraction put: answer;
			at: #section put: answer;
			at: #image put: self.
		answer]! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/27/2018 03:49:31'!
into: aCodomain evaluating: aBlock
	"Answer an embedding (an injective morphism) into the specified codomain (in the same category), evaluating as the given block."
	^ self to: aCodomain evaluating: aBlock! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/24/2019 10:47:21'!
into: aCodomain mapping: aBlock
	^ self to: aCodomain mapping: aBlock! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/27/2018 03:49:05'!
onto: aCodomain evaluating: aBlock
	"Answer a projection (a surjective morphism) onto the specified codomain (in the same category), evaluating as the given block."
	^ self to: aCodomain evaluating: aBlock! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/24/2019 10:47:30'!
onto: aCodomain mapping: aBlock
	^ self to: aCodomain mapping: aBlock! !

!Structure methodsFor: 'morphisms' stamp: 'len 12/22/2021 15:12:32'!
to: aCodomain
	"Answer the canonical morphism from the receiver to aCodomain."
	self ambient = aCodomain ifTrue: [^ self into: aCodomain evaluating: [:x| x]].
	self isAmbient ifFalse: [self ambient -> aCodomain ifNotNil: [:aMorphism| ^ self -> self ambient -> aMorphism]].
	(self ambient = aCodomain ambient and: [self <= aCodomain]) ifTrue: [^ self to: aCodomain evaluating: [:x| x]].
	^ super to: aCodomain! !

!Structure methodsFor: 'morphisms' stamp: 'len 12/23/2021 15:29:25'!
to: aCodomain evaluating: aBlock
	"Answer a morphism to the specified codomain (in the same category), evaluating as the given block."
	(self isSet and: [aCodomain isSet]) ifFalse: [^ self subclassResponsibility].
	^ Function from: self to: aCodomain evaluating: aBlock! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/23/2020 09:51:43'!
to: aCodomain evaluatingWithArguments: aBlock
	"When the receiver is N-ary, this method allows to define morphisms by N-ary blocks.
	Example: É ◊ É to: É evaluatingWithArguments: [:a :b| a * b]."
	^ self to: aCodomain evaluating: [:each| aBlock valueWithArguments: each asArray]! !

!Structure methodsFor: 'morphisms' stamp: 'len 5/24/2019 10:46:58'!
to: aCodomain mapping: aBlock
	"Answer a morphism defined by images of generators.
	aBlock takes indices i and evaluates to the image of the i-th generator."
	^ self subclassResponsibility! !

!Structure methodsFor: 'morphisms' stamp: 'len 7/8/2020 12:04:45'!
toAll: anArray
	"Answer the canonical projections from the receiver to the components of anArray."
	(self isProduct and: [anArray = self components])
		ifTrue: [^ self addProjections: ((1 to: self arity) collect: [:i| self projection: i])].
	self isAmbient ifFalse: [(self ambient toAll: anArray) ifNotNil: [:anArrayOfMorphisms| ^ anArrayOfMorphisms collect: [:each| self -> self ambient -> each]]].
	^ nil! !

!Structure methodsFor: 'operations' stamp: 'len 12/30/2017 15:19:47'!
, aDomain
	"Answer the cartesian set product of the receiver with the argument."
	^ CartesianProduct components: {self. aDomain}! !

!Structure methodsFor: 'operations' stamp: 'len 7/31/2020 11:56:22'!
/\ aDomain
	"Answer the intersection of the receiver and the argument."
	self isSet ifFalse: [^ self subclassResponsibility].
	^ self select: [:each| aDomain includes: each]! !

!Structure methodsFor: 'operations' stamp: 'len 12/23/2021 13:40:34'!
^ anInteger
	^ CartesianProduct components: (Array new: anInteger withAll: self carrier)! !

!Structure methodsFor: 'operations' stamp: 'len 12/30/2017 16:36:36'!
∑ aDomain
	"This is a convenience method for doing categorical products. Subclasses can reimplement it to mean something different from the categorical product in order to match customary notation."
	^ self ◊ aDomain! !

!Structure methodsFor: 'operations' stamp: 'len 7/31/2020 11:50:45'!
◊ aDomain
	"Answer the categorical product of the receiver with the argument."
	"The product in the category of sets is the cartesian product. Subclasses can redefine."
	self isSet ifFalse: [^ self subclassResponsibility].
	^ self, aDomain! !

!Structure methodsFor: 'operations' stamp: 'len 5/3/2021 10:38:56'!
ì aDomain
	"Answer the categorical product of the receiver with the argument."
	"The product in the category of sets is the cartesian product. Subclasses can redefine."
	self isSet ifFalse: [^ self subclassResponsibility].
	^ self, aDomain! !

!Structure methodsFor: 'operations' stamp: 'len 5/3/2021 10:40:41'!
î aDomain
	"Answer the categorical coproduct of the receiver with the argument."
	"The coproduct in the category of sets is the disjoint union. Subclasses can redefine."
	^ self notYetImplemented! !

!Structure methodsFor: 'random' stamp: 'len 3/11/2021 16:38:02'!
atRandom
	"Answer an element in the receiver choosen at random. This will fail if the receiver is not finite."
	^ self atRandom: self newRandom! !

!Structure methodsFor: 'random' stamp: 'len 4/19/2020 07:12:07'!
atRandom: aRandom
	"Answer an element in the receiver choosen at random. This will fail if the receiver is not finite."
	self propertyAt: #elements ifPresent: [:aCollectionOrDomain| ^ aCollectionOrDomain atRandom: aRandom].
	^ self atRandom: aRandom bits: Infinity positive! !

!Structure methodsFor: 'random' stamp: 'len 5/25/2020 07:55:53'!
atRandom: aRandom bits: bitSize
	"Answer an element of the receiver of at most n bits (as measured by #bitSize). The total number of elements that could be returned shouldn't exceed 2^n."
	self propertyAt: #elements ifPresent: [:aCollectionOrDomain| ^ aCollectionOrDomain atRandom: aRandom bits: bitSize].
	^ self elements asSet atRandom: aRandom! !

!Structure methodsFor: 'random' stamp: 'len 3/11/2021 16:39:29'!
atRandomBits: bitSize
	"Answer an element of the receiver of at most n bits (as measured by #bitSize). The total number of elements that could be returned shouldn't exceed 2^n."
	^ self atRandom: self newRandom bits: bitSize! !

!Structure methodsFor: 'random' stamp: 'len 12/25/2021 11:39:44'!
to: aCodomain atRandom: aRandom
	"Answer a morphism from the receiver to aCodomain choosen at random."
	self flag: #fix. "this doesnt always make sense, it works for free structures"
	^ self to: aCodomain mapping: [:i| aCodomain atRandom: aRandom]! !

!Structure methodsFor: 'random' stamp: 'len 12/25/2021 11:41:40'!
to: aCodomain atRandom: aRandom bits: bitSize
	"Answer a morphism from the receiver to aCodomain choosen at random of at most n bits (as measured by #bitSize). The total number of morphisms that could be returned shouldn't exceed 2^n."
	| n |
	self flag: #fix. "this doesnt always make sense, it works for free objects"
	n _ bitSize / (aCodomain generators size max: 1).
	^ self to: aCodomain mapping: [:i| aCodomain atRandom: aRandom bits: n]! !

!Structure methodsFor: 'testing' stamp: 'len 4/19/2019 07:15:03'!
contains: anElement
	"Answer true if the receiver contains the given element of its ambient.
	This is similar to >>includes: but less checks are needed, because the element is not an arbitrary object."
	self isAmbient ifTrue: [^ true].
	^ self elements includes: anElement! !

!Structure methodsFor: 'testing' stamp: 'len 12/13/2016 07:34:43'!
containsAllOf: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:each| (self contains: each) ifFalse: [^ false]].
	^ true! !

!Structure methodsFor: 'testing' stamp: 'len 6/23/2016 07:04'!
includes: anElement
	^ self elements includes: anElement! !

!Structure methodsFor: 'testing' stamp: 'len 6/24/2016 05:59'!
includesAllOf: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:each| (self includes: each) ifFalse: [^ false]].
	^ true! !

!Structure methodsFor: 'testing' stamp: 'len 4/29/2020 21:16:49'!
isAmbient
	^ self ambient == self! !

!Structure methodsFor: 'testing' stamp: 'len 11/2/2016 08:54'!
isEmpty
	^ self size = 0! !

!Structure methodsFor: 'testing' stamp: 'len 12/28/2016 11:23:17'!
isFinite
	^ self propertyAt: #isFinite ifAbsent: [self size isInfinite not]! !

!Structure methodsFor: 'testing' stamp: 'len 6/17/2016 22:20'!
isInfinite
	^ self isFinite not! !

!Structure methodsFor: 'testing-categories' stamp: 'len 7/31/2020 11:45:53'!
isSet
	^ self species = Structure! !

!Structure methodsFor: 'private' stamp: 'len 4/22/2020 07:06:13'!
ambient: aDomain
	self propertyAt: #ambient put: aDomain! !

!Structure methodsFor: 'private' stamp: 'len 12/28/2016 11:20:10'!
elements: aCollection
	self propertyAt: #elements put: aCollection! !

!Structure methodsFor: 'private' stamp: 'len 3/11/2021 16:38:21'!
newRandom
	^ Random seed: (Random withDefaultDo: [:aRandom| aRandom nextChunkBits])! !

!Structure methodsFor: 'private' stamp: 'len 7/31/2020 11:34:56'!
species
	^ Structure! !

!Structure class methodsFor: 'instance creation' stamp: 'len 4/23/2016 03:40'!
newFrom: anObject
	^ self new elements: anObject! !

!CartesianProduct methodsFor: 'accessing' stamp: 'len 11/10/2015 22:13'!
components
	^ components! !

!CartesianProduct methodsFor: 'accessing' stamp: 'len 2/26/2017 09:47:17'!
size
	^ components product: [:each| each size]! !

!CartesianProduct methodsFor: 'comparing' stamp: 'len 9/16/2018 16:11:29'!
= anObject
	^ (anObject isKindOf: CartesianProduct) and: [components = anObject components]! !

!CartesianProduct methodsFor: 'comparing' stamp: 'len 11/10/2015 23:19'!
hash
	^ components hash! !

!CartesianProduct methodsFor: 'elements' stamp: 'len 5/10/2020 16:25:06'!
compose: anArray
	^ anArray asArray! !

!CartesianProduct methodsFor: 'enumerating' stamp: 'len 7/29/2016 06:53'!
do: aBlock
	(self components allSatisfy: [:each| each isFinite])
		ifFalse: [^ self upTo: self size rank: self arity do: aBlock].
	self upToComponent: self arity do: aBlock! !

!CartesianProduct methodsFor: 'enumerating' stamp: 'len 11/5/2016 10:29'!
upTo: max rank: n do: aBlock
	| k |
	self notYetImplemented.
	n = 0 ifTrue: [^ aBlock value: #()].
	k _ 1.
	(self at: n) upTo: max do: [:s|
		self upTo: k rank: n-1 do: [:p|
			aBlock value: {p. s}.
			"(self at: n) upTo: k-1 do: [:t| (p includes: s) ifTrue: [aBlock value: (p,t)]]"].
		k _ k + 1]! !

!CartesianProduct methodsFor: 'enumerating' stamp: 'len 3/17/2021 14:29:36'!
upToComponent: k do: aBlock
	k = 0 ifTrue: [aBlock value: #(). ^ self].
	self upToComponent: k-1 do: [:each| (self components at: k) do: [:last| aBlock value: (each copyWith: last)]]! !

!CartesianProduct methodsFor: 'morphisms' stamp: 'len 12/22/2021 15:04:08'!
from: aDomain
	aDomain -< self ifNotNil: [:anArray| ^ aDomain carrier to: self evaluating: [:x| anArray collect: [:é| é value: x]]].
	^ super from: aDomain! !

!CartesianProduct methodsFor: 'morphisms' stamp: 'len 5/23/2020 07:29:42'!
projection: i
	^ (self to: (self at: i) evaluating: [:x| x at: i]) name: 'é', i printText sub! !

!CartesianProduct methodsFor: 'operations' stamp: 'len 12/30/2017 15:19:20'!
, aDomain
	^ aDomain class == self class
		ifTrue: [self class components: components, aDomain components]
		ifFalse: [self class components: (components copyWith: aDomain)]! !

!CartesianProduct methodsFor: 'random' stamp: 'len 4/19/2016 20:28'!
atRandom: aRandom
	^ components collect: [:each| each atRandom: aRandom]! !

!CartesianProduct methodsFor: 'random' stamp: 'len 4/19/2016 20:28'!
atRandom: aRandom bits: bitSize
	| n |
	n _ bitSize / components size.
	^ components collect: [:each| each atRandom: aRandom bits: n]! !

!CartesianProduct methodsFor: 'testing' stamp: 'len 4/21/2020 15:16:12'!
includes: anObject
	anObject isSequenceable ifFalse: [^ false].
	components size = anObject size ifFalse: [^ false].
	components with: anObject do: [:X :x| (X includes: x) ifFalse: [^ false]].
	^ true! !

!CartesianProduct methodsFor: 'testing' stamp: 'len 7/6/2020 14:13:25'!
isCartesianProduct
	^ true! !

!CartesianProduct methodsFor: 'testing' stamp: 'len 4/14/2020 21:09:33'!
isProduct
	^ true! !

!CartesianProduct methodsFor: 'private' stamp: 'len 11/5/2016 06:40'!
components: anArray
	components _ anArray! !

!CartesianProduct class methodsFor: 'examples' stamp: 'len 5/23/2020 08:59:16'!
example
	"The cartesian product of the ring of rational integers
	and the field of rational numbers (as sets).
	An element of this cartesian product is an array with an
	integer in the first component and a rational number
	in the second component."
	^ (É, Ñ)
! !

!CartesianProduct class methodsFor: 'instance creation' stamp: 'len 11/10/2015 22:17'!
components: anArray
	^ self new components: anArray! !

!Category methodsFor: 'comparing' stamp: 'len 4/26/2020 17:31:03'!
= anObject
	^ self class = anObject class and: [self name = anObject name]! !

!Category methodsFor: 'comparing' stamp: 'len 4/26/2020 17:31:07'!
hash
	^ self name hash! !

!Category methodsFor: 'morphisms' stamp: 'len 5/2/2020 21:09:51'!
to: aCodomain evaluating: aBlock
	aCodomain isCategory ifFalse: [^ DomainError signal].
	^ Functor from: self to: aCodomain evaluating: aBlock! !

!Category methodsFor: 'testing' stamp: 'len 4/26/2020 19:01:25'!
includes: aMorphism
	^ aMorphism category = self! !

!Category methodsFor: 'testing' stamp: 'len 10/30/2020 11:10:58'!
isAbelian
	^ false! !

!Category methodsFor: 'testing-categories' stamp: 'len 5/2/2020 21:33:26'!
isCategory
	^ true! !

!Category methodsFor: 'private' stamp: 'len 4/26/2020 17:40:03'!
name: aString
	self propertyAt: #name put: aString asSymbol! !

!Category methodsFor: 'private' stamp: 'len 5/2/2020 05:07:09'!
species
	^ Category! !

!Category methodsFor: 'as yet unclassified' stamp: 'len 10/30/2020 11:12:02'!
ChainComplexes
	self isAbelian ifFalse: [DomainError signal: 'not an abelian category'].
	^ self propertyAt: #ChainComplexes ifAbsentPut: [ChainComplexes over: self]! !

!Category methodsFor: 'as yet unclassified' stamp: 'len 10/30/2020 11:11:34'!
CochainComplexes
	^ self ChainComplexes opposite! !

!HomSet methodsFor: 'accessing' stamp: 'len 3/11/2020 11:21:36'!
codomain
	^ codomain! !

!HomSet methodsFor: 'accessing' stamp: 'len 3/11/2020 11:21:42'!
domain
	^ domain! !

!HomSet methodsFor: 'comparing' stamp: 'len 4/30/2020 09:13:30'!
= anObject
	^ self class == anObject class and: [self domain = anObject domain and: [self codomain = anObject codomain]]! !

!HomSet methodsFor: 'comparing' stamp: 'len 4/30/2020 09:14:39'!
hash
	^ self domain hash hashMultiply + self codomain hash! !

!HomSet methodsFor: 'elements' stamp: 'len 12/25/2021 11:06:08'!
adapt: anElement
	^ (super adapt: anElement) ifNil: [self domain to: self codomain adapting: anElement]! !

!HomSet methodsFor: 'elements' stamp: 'len 8/31/2016 03:37'!
evaluating: aBlock
	^ self domain to: self codomain evaluating: aBlock! !

!HomSet methodsFor: 'elements' stamp: 'len 5/24/2019 11:28:29'!
mapping: aBlock
	"Answer a morphism defined by images of generators.
	aBlock takes indices i and evaluates to the image of the i-th generator."
	^ self domain to: self codomain mapping: aBlock! !

!HomSet methodsFor: 'elements' stamp: 'len 4/9/2020 07:20:29'!
newFrom: anObject
	anObject isEvaluable ifTrue: [^ self evaluating: anObject].
	^ super newFrom: anObject! !

!HomSet methodsFor: 'random' stamp: 'len 12/25/2021 10:20:09'!
atRandom: aRandom
	^ self domain to: self codomain atRandom: aRandom! !

!HomSet methodsFor: 'random' stamp: 'len 12/25/2021 10:20:32'!
atRandom: aRandom bits: bitSize
	^ self domain to: self codomain atRandom: aRandom bits: bitSize! !

!HomSet methodsFor: 'testing' stamp: 'len 5/8/2020 15:46:16'!
includes: anObject
	^ anObject isMorphism and: [anObject domain = self domain and: [anObject codomain = self codomain]]! !

!HomSet methodsFor: 'testing' stamp: 'len 4/19/2016 04:53'!
isEndomorphisms
	^ self domain = self codomain! !

!HomSet methodsFor: 'private' stamp: 'len 3/11/2020 11:22:05'!
codomain: aDomain
	codomain _ aDomain! !

!HomSet methodsFor: 'private' stamp: 'len 3/11/2020 11:21:58'!
domain: aDomain
	domain _ aDomain! !

!HomSet class methodsFor: 'examples' stamp: 'len 12/23/2021 19:37:12'!
example1
	"The set Hom(N,Z), the functions from the natural numbers to the integers.
	This is the space of sequences. (See class Sequence)."
	^ Ç => É carrier! !

!HomSet class methodsFor: 'examples' stamp: 'len 5/23/2020 09:13:12'!
example2
	"The endomorphisms of the rational integers Z.
	This is Hom(Z,Z), the set of ring homomorphisms from Z to itself."
	^ É endomorphisms! !

!HomSet class methodsFor: 'examples' stamp: 'len 5/23/2020 09:11:37'!
example3
	"In some categories the Hom-Set has actually richer structure,
	and can be seen as an object of the category.
	Here's the vector space of linear maps from the Q^3 to Q^5.
	This is the internal hom(Q^3,Q^5), and it is a vector space itself.
	(It is not an instance of HomSet.)"
	^ (Ñ^3) => (Ñ^5)! !

!HomSet class methodsFor: 'instance creation' stamp: 'len 8/7/2020 15:07:48'!
from: aDomain to: aCodomain
	^ self new domain: aDomain; codomain: aCodomain! !

!Naturals methodsFor: 'accessing' stamp: 'len 2/13/2016 20:41'!
identity
	^ 1! !

!Naturals methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new! !

!Naturals methodsFor: 'comparing' stamp: 'len 12/14/2015 01:55'!
= anObject
	^ self class == anObject class! !

!Naturals methodsFor: 'comparing' stamp: 'len 12/14/2015 01:55'!
hash
	^ self class hash! !

!Naturals methodsFor: 'enumerating' stamp: 'len 12/3/2015 20:12'!
do: aBlock
	| n |
	n _ 1.
	[aBlock value: n. n _ n + 1] repeat! !

!Naturals methodsFor: 'morphisms' stamp: 'len 7/30/2020 20:38:12'!
to: aCodomain evaluating: aBlock
	"Answer a sequence with values in aCodomain."
	"aCodomain isSet ifFalse: [^ DomainError signal]."
	^ Sequence to: aCodomain evaluating: aBlock! !

!Naturals methodsFor: 'random' stamp: 'len 6/30/2016 00:34'!
atRandom: aRandom bits: bitSize
	^ (aRandom nextBits: bitSize truncated) + 1! !

!Naturals methodsFor: 'testing' stamp: 'len 12/5/2015 02:09'!
isFinite
	^ false! !

!Naturals methodsFor: 'testing' stamp: 'len 2/11/2016 22:19'!
isReal
	^ true! !

!Naturals class methodsFor: 'initialization' stamp: 'len 5/23/2020 08:50:31'!
initialize
	super initialize.
	Smalltalk at: #Ç put: self new! !

!Morphism methodsFor: 'accessing' stamp: 'len 4/22/2020 14:43:39'!
codomain
	^ self propertyAt: #codomain! !

!Morphism methodsFor: 'accessing' stamp: 'len 4/22/2020 14:43:43'!
domain
	^ self propertyAt: #domain! !

!Morphism methodsFor: 'accessing' stamp: 'len 5/29/2020 10:16:04'!
identity
	self isEndomorphism ifFalse: [^ DomainError signal: 'not an endomorphism'].
	^ self domain id! !

!Morphism methodsFor: 'accessing' stamp: 'len 12/24/2021 08:16:27'!
parent
	"A morphism f:A->B belongs to Hom(A,B)."
	^ self domain => self codomain! !

!Morphism methodsFor: 'printing' stamp: 'len 5/2/2020 09:34:13'!
arrow
	^ Text streamContents: [:s | self arrowPrintOn: s]! !

!Morphism methodsFor: 'printing' stamp: 'len 4/22/2020 14:37:56'!
arrowPrintOn: aStream
	aStream
		print: self domain;
		nextPut:
			((self propertyAt: #inverse ifAbsent: []) notNil
				ifTrue: [$õ]
				ifFalse:
					[(self propertyAt: #retraction ifAbsent: []) notNil
						ifTrue: [$ö]
						ifFalse:
							[(self propertyAt: #section ifAbsent: []) notNil
								ifTrue: [$ô]
								ifFalse: [$]]]);
		print: self codomain! !

!Morphism methodsFor: 'printing' stamp: 'len 4/22/2020 14:36:41'!
name: aString
	self propertyAt: #name put: aString! !

!Morphism methodsFor: 'comparing' stamp: 'len 4/22/2020 14:43:11'!
hash
	^ self domain hash hashMultiply + self codomain hash! !

!Morphism methodsFor: 'operations' stamp: 'len 3/13/2020 06:18:07'!
-> aCodomain
	"Answer the composition with the canonical morphism from the codomain of the receiver to aCodomain. See also Domain>>->."
	^ self codomain -> aCodomain ifNotNil: [:aMorphism| aMorphism ∑ self]! !

!Morphism methodsFor: 'operations' stamp: 'len 12/25/2021 11:56:21'!
=> aCodomain
	"This is the Hom(-,B) functor, where B is the argument.
	If the receiver is a morphism g:X->Y, answer the morphism Hom(g,B):Hom(Y,B)->Hom(X,B) that sends a morphism f to f composed with g.
	This is a contravariant functor, and it is often called the 'functor of points' of B."
	^ self codomain => aCodomain to: self domain => aCodomain evaluating: [:f| f ∑ self]! !

!Morphism methodsFor: 'operations' stamp: 'len 8/11/2020 08:47:16'!
^ anInteger
	"Answer the receiver composed with itself anInteger times.
	If the argument is another morphism, answer the conjugation."
	anInteger isInteger ifFalse: [^ anInteger inverse ∑ self ∑ anInteger].
	anInteger = 1 ifTrue: [^ self].
	anInteger = 0 ifTrue: [^ self identity].
	anInteger < 0 ifTrue: [^ self inverse ^ anInteger negated].
	^ anInteger even ifTrue: [self ∑ self ^ (anInteger // 2)] ifFalse: [self ∑ self ^ (anInteger // 2) ∑ self]! !

!Morphism methodsFor: 'operations' stamp: 'len 3/13/2020 06:12:50'!
∑ aMorphism
	"Answer the composition of the receiver with the argument.
	Given the argument f:X -> Y and the receiver g:Y -> Z, answer g∑f:X -> Z."
	^ self subclassResponsibility! !

!Morphism methodsFor: 'operations' stamp: 'len 8/8/2016 06:42'!
coimage
	^ self domain / self kernel! !

!Morphism methodsFor: 'operations' stamp: 'len 8/9/2020 19:49:42'!
coimageMap
	^ self domain -> self coimage! !

!Morphism methodsFor: 'operations' stamp: 'len 8/8/2016 06:42'!
cokernel
	^ self codomain / self image! !

!Morphism methodsFor: 'operations' stamp: 'len 8/9/2020 19:50:56'!
cokernelMap
	^ self codomain -> self cokernel! !

!Morphism methodsFor: 'operations' stamp: 'len 5/3/2020 10:06:33'!
commutator: aMorphism
	^ self inverse ∑ aMorphism inverse ∑ self ∑ aMorphism! !

!Morphism methodsFor: 'operations' stamp: 'len 4/22/2020 14:39:46'!
image
	^ self propertyAt: #image ifAbsent: [self domain apply: self]! !

!Morphism methodsFor: 'operations' stamp: 'len 8/11/2020 19:52:51'!
imageMap
	^ self image -> self codomain! !

!Morphism methodsFor: 'operations' stamp: 'len 4/22/2020 14:39:01'!
inverse
	"Answer a two-sided inverse of the receiver (i.e., a morphism that is both a retraction and a section) or nil."
	^ self propertyAt: #inverse! !

!Morphism methodsFor: 'operations' stamp: 'len 8/11/2020 19:53:19'!
kernelMap
	^ self kernel -> self domain! !

!Morphism methodsFor: 'operations' stamp: 'len 4/22/2020 14:39:16'!
retraction
	"Answer a left inverse of the receiver or nil."
	^ self propertyAt: #retraction ifAbsent: [self inverse]! !

!Morphism methodsFor: 'operations' stamp: 'len 4/22/2020 14:39:33'!
section
	"Answer a right inverse of the receiver or nil."
	^ self propertyAt: #section ifAbsent: [self inverse]! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:34:34'!
hasProperty: aSymbol
	^ (properties ifNil: [^ false]) includesKey: aSymbol! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:34:48'!
properties
	^ properties ifNil: [properties _ IdentityDictionary new]! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:34:53'!
propertyAt: aSymbol
	^ self propertyAt: aSymbol ifAbsent: [self error: 'no such property']! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:34:57'!
propertyAt: aSymbol ifAbsent: exceptionBlock
	^ (properties ifNil: [^ exceptionBlock value]) at: aSymbol ifAbsent: exceptionBlock! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:35:01'!
propertyAt: aSymbol ifAbsentPut: aBlock
	^ self properties at: aSymbol ifAbsentPut: aBlock! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:35:06'!
propertyAt: aSymbol ifPresent: aBlock
	^ (properties ifNil: [^ nil]) at: aSymbol ifPresent: aBlock! !

!Morphism methodsFor: 'properties' stamp: 'len 4/22/2020 14:35:11'!
propertyAt: aSymbol put: anObject
	^ self properties at: aSymbol put: anObject! !

!Morphism methodsFor: 'testing' stamp: 'len 8/8/2016 06:02'!
isAutomorphism
	^ self isEndomorphism and: [self isIsomorphism]! !

!Morphism methodsFor: 'testing' stamp: 'len 4/22/2020 06:00:10'!
isElement
	"Answer true if the receiver is an element of a Domain, i.e. if it understands the message #parent and answers its parent Domain.
	The parent of a morphism is a hom set."
	^ true! !

!Morphism methodsFor: 'testing' stamp: 'len 8/8/2016 06:02'!
isEndomorphism
	^ self domain = self codomain! !

!Morphism methodsFor: 'testing' stamp: 'len 12/25/2021 12:01:21'!
isEpimorphism
	"Answer true if the receiver is right cancelable: a morphism f:A->B is an epimorphism if, for any pair of morphisms g:B->C, h:B->C, gf = hf implies g = h."
	^ self subclassResponsibility! !

!Morphism methodsFor: 'testing' stamp: 'len 3/13/2020 06:18:30'!
isIdempotent
	^ self isEndomorphism and: [self ∑ self = self]! !

!Morphism methodsFor: 'testing' stamp: 'len 11/16/2016 08:33'!
isIdentity
	^ self = self domain id! !

!Morphism methodsFor: 'testing' stamp: 'len 5/16/2019 12:42:32'!
isIsomorphism
	^ self inverse notNil! !

!Morphism methodsFor: 'testing' stamp: 'len 12/25/2021 12:01:52'!
isMonomorphism
	"Answer true if the receiver is left cancelable: a morphism f:A->B is a monomorphism if, for any pair of morphisms g:C->A, h:C->A, fg = fh implies g = h."
	^ self subclassResponsibility! !

!Morphism methodsFor: 'testing' stamp: 'len 5/8/2020 17:57:22'!
isMorphism
	^ true! !

!Morphism methodsFor: 'testing' stamp: 'len 12/25/2021 12:15:27'!
isRetraction
	"A split epimorphism is a morphism that has a section (a right inverse).
	Dualy, a split epimorphism is a morphism that *is* a retraction (a left inverse)."
	^ self section notNil! !

!Morphism methodsFor: 'testing' stamp: 'len 12/25/2021 12:15:43'!
isSection
	"A split monomorphism is a morphism that has a retraction (a left inverse).
	Dualy, a split monomorphism is a morphism that *is* a section (a right inverse)."
	^ self retraction notNil! !

!Morphism methodsFor: 'testing' stamp: 'len 12/10/2016 12:59:06'!
isTrivial
	^ self kernel = self domain! !

!Morphism methodsFor: 'private' stamp: 'len 4/22/2020 14:44:26'!
codomain: aDomain
"	(aDomain isKindOf: Domain) ifFalse: [^ DomainError signal: 'not a Domain']."
	self propertyAt: #codomain put: aDomain! !

!Morphism methodsFor: 'private' stamp: 'len 4/22/2020 14:44:20'!
domain: aDomain
"	(aDomain isKindOf: Domain) ifFalse: [^ DomainError signal: 'not a Domain']."
	self propertyAt: #domain put: aDomain! !

!Morphism class methodsFor: 'examples' stamp: 'len 8/11/2020 08:50:51'!
example1Embedding
	"Here we construct Q[x], the ring of univariate polynomials over the rational numbers.
	It comes equipped with a canonical embedding from the rational numbers to the constant polynomials."
	| R phi F psi |
	R _ Ñ polynomials.
	phi _ Ñ -> R.
	"Then we construct its field of fractions Q(x) (the rational functions),
	that comes with a cannonical embedding from the polynomial ring to the rational functions with denominator 1 (the localization map)."
	F _ R fractions.
	psi _ R -> F.
	"If we compose them we get an embedding from the rational numbers Q to the field of univariate rational functions Q(x), that sends a number to a constant rational function."
	^ psi ∑ phi! !

!Morphism class methodsFor: 'examples' stamp: 'len 8/11/2020 08:50:57'!
example2EasierEmbedding
	"Another way to obtain the canonical embedding from Q to the rational functions over Q."
	^ Ñ -> Ñ polynomials fractions! !

!Morphism class methodsFor: 'examples' stamp: 'len 5/23/2020 09:34:55'!
example3CanonicalMorphism
	"Canonical morphisms can be retrieved through the message #->.
	More specific messages #>-> and #->> are also available
	(for monomorphisms and epimorphisms respectively). 
	For example, Z (the ring of integers) is the initial object in
	the category of commutative rings with unit, i.e. there's
	a canonical morphism from Z to any commutative ring:"
	^ É -> (Ñ polynomialsIn: #(x y z))! !

!Morphism class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example4CanonicalMorphism
	"The quotient map from a polynomial ring R to a quotient by
	one of its ideals R/I is another example of canonical morphism."
	| R x I |
	R _ Ñ polynomials.
	x _ R x.
	I _ R * (x squared + 1).
	^ R -> (R / I)! !

!Morphism class methodsFor: 'examples' stamp: 'len 5/23/2020 09:34:55'!
example5CanonicalMorphism
	"The message #-> can be sent to a Morphism with a Domain as
	argument, or to a Domain with a Morphism as argument.
	This yields canonical constructions.
	For example, given an arbitrary ring homomorphism,
	we can send it *canonically* to a quotient of its codomain."
	| R phi x I |
	R _ (É/5) polynomials. "univariate polynomials over Z/<5>"
	phi _ R frobenius. "the Frobenius endomorphism"
	x _ R x.
	I _ R * (x squared + 1).
	^ phi -> (R / I)! !

!Morphism class methodsFor: 'examples' stamp: 'len 12/23/2021 15:42:14'!
example6Projections
	"Algebraic structures often come equipped with some canonical morphisms.
	Here we construct the direct product of the ring of rational integers
	with the field of rational numbers, and get the projections to each component."
	^ É ◊ Ñ -< {É. Ñ}! !

!Morphism class methodsFor: 'examples' stamp: 'len 12/23/2021 15:47:09'!
example7Embeddings
	"Algebraic structures often come equipped with some canonical morphisms.
	Here we construct the direct sum of two vector spaces,
	and get the embedding from the first component."
	| V W |
	V _ Ñ^3.
	W _ Ñ^2.
	^ {V. W} >- (V ê W) at: 1! !

!Morphism class methodsFor: 'examples' stamp: 'len 8/11/2020 08:59:12'!
example8GLAsGroupAction
	"The action of the group GL(3,Q) on the vector space of dimension 3 over the rationals.
	The action is evaluation of a linear map at a vector."
	^ (Ñ^3) automorphisms action! !

!Function methodsFor: 'comparing' stamp: 'len 12/23/2021 15:29:25'!
= anObject
	self == anObject ifTrue: [^ true].
	(anObject isKindOf: Function) ifFalse: [^ false].
	(self domain = anObject domain and: [self codomain = anObject codomain]) ifFalse: [^ false].
	self domain isFinite ifFalse: [^ self error: 'infinite domain'].
	self domain do: [:each| (self value: each) = (anObject value: each) ifFalse: [^ false]].
	^ true! !

!Function methodsFor: 'converting' stamp: 'len 12/23/2021 15:29:25'!
asFunction
	self class = Function ifTrue: [^ self].
	^ self domain carrier to: self codomain carrier evaluating: self! !

!Function methodsFor: 'operations' stamp: 'len 5/1/2020 17:11:54'!
, aFunction
	"Answer the cartesian product of the receiver with the argument."
	^ (self domain, aFunction domain)
		to: (self codomain, aFunction codomain)
		evaluating: [:each| {self value: (each at: 1). aFunction value: (each at: 2)}]! !

!Function methodsFor: 'operations' stamp: 'len 5/1/2020 22:11:12'!
∑ aFunction
	"Answer the composition of the receiver with the argument.
	Given the argument f:X -> Y and the receiver g:Y -> Z, answer g∑f:X -> Z.
	This is, (g∑f)(x) = g(f(x))."
	self domain = aFunction codomain ifFalse: [^ DomainError signal: 'domains don''t match'].
	"self category = aFunction category ifFalse: [^ DomainError signal: 'categories don''t match']."
	^ aFunction domain to: self codomain evaluating: [:x| self value: (aFunction value: x)]! !

!Function methodsFor: 'operations' stamp: 'len 12/23/2021 15:29:05'!
◊ aFunction
	"Answer the product of the receiver with the argument."
	^ self, aFunction! !

!Function methodsFor: 'operations' stamp: 'len 3/3/2021 07:30:54'!
value: anElement
	^ (self propertyAt: #expression) value: anElement
"	^ self codomain !! ((self propertyAt: #expression) value: self domain !! anElement)"! !

!Function methodsFor: 'operations' stamp: 'len 11/5/2016 09:33'!
valueWithArguments: anArray
	^ self value: (self domain scalars tuple: anArray)! !

!Function methodsFor: 'testing' stamp: 'len 7/5/2021 12:49:55'!
imageContains: anElement
	^ self image contains: anElement! !

!Function methodsFor: 'testing' stamp: 'len 5/10/2019 07:30:02'!
isAbelianGroupHomomorphism
	(self domain isAbelianGroup and: [self codomain isAbelianGroup]) ifFalse: [^ false].
	self domain isFinite ifFalse: [^ self error: 'infinite domain'].
	^ self domain allSatisfy: [:x|
		self domain allSatisfy: [:y|
			(self value: x) + (self value: y) = (self value: x + y)]]! !

!Function methodsFor: 'testing' stamp: 'len 8/9/2019 13:25:28'!
isAlgebraHomomorphism
	"Answer true if the receiver is a (unital) K-algebra homomorphism,
	i.e. a ring homomorphism that satisfies f(ka) = kf(a)."
	| K codomainOne domainOne |
	self isRingHomomorphism ifFalse: [^ false].
	(K _ self domain scalars) = self codomain scalars ifFalse: [^ false].
	K isFinite ifFalse: [^ self error: 'infinite domain'].
	domainOne _ self domain one.
	codomainOne _ self codomain one.
	^ K allSatisfy: [:each| (self value: domainOne * each) = (codomainOne * each)]! !

!Function methodsFor: 'testing' stamp: 'len 4/16/2016 18:53'!
isBiyective
	^ self isInjective and: [self isSurjective]! !

!Function methodsFor: 'testing' stamp: 'len 12/25/2021 12:02:44'!
isEpimorphism
	"Answer true if the receiver is right cancelable: a morphism f:A->B is an epimorphism if, for any pair of morphisms g:B->C, h:B->C, gf = hf implies g = h.
	In the category of sets epimorphisms are surjections." 
	^ self isSurjective! !

!Function methodsFor: 'testing' stamp: 'len 5/19/2018 17:54:16'!
isEvaluable
	^ true! !

!Function methodsFor: 'testing' stamp: 'len 5/10/2019 07:29:16'!
isGroupHomomorphism
	(self domain isGroup and: [self codomain isGroup]) ifFalse: [^ false].
	self domain isFinite ifFalse: [^ self error: 'infinite domain'].
	^ self domain allSatisfy: [:x|
		self domain allSatisfy: [:y|
			(self value: x)∑(self value: y) = (self value: x∑y)]]! !

!Function methodsFor: 'testing' stamp: 'len 12/28/2016 11:10:18'!
isIdentity
	^ self propertyAt: #isIdentity ifAbsentPut: [self = self domain id]! !

!Function methodsFor: 'testing' stamp: 'len 5/16/2019 12:14:30'!
isInjective
	^ self retraction notNil! !

!Function methodsFor: 'testing' stamp: 'len 5/10/2019 07:38:30'!
isModuleHomomorphism
	(self domain isModule and: [self codomain isModule]) ifFalse: [^ false].
	self domain scalars = self codomain scalars ifFalse: [^ false].
	self domain isFinite ifFalse: [^ self error: 'infinite domain'].
	^ self domain allSatisfy: [:x|
		self domain allSatisfy: [:y|
			| z |
			(z _ self value: x) + (self value: y) = (self value: x + y) and:
				[self domain scalars allSatisfy: [:r| z * r = (self value: x * r)]]]]! !

!Function methodsFor: 'testing' stamp: 'len 4/26/2020 08:16:22'!
isMonoidHomomorphism
	(self domain isMonoid and: [self codomain isMonoid]) ifFalse: [^ false].
	self domain isFinite ifFalse: [^ self error: 'infinite domain'].
	^ self domain allSatisfy: [:x|
		self domain allSatisfy: [:y|
			(self value: x)∑(self value: y) = (self value: x∑y)]]! !

!Function methodsFor: 'testing' stamp: 'len 12/25/2021 12:03:05'!
isMonomorphism
	"Answer true if the receiver is left cancelable: a morphism f:A->B is a monomorphism if, for any pair of morphisms g:C->A, h:C->A, fg = fh implies g = h.
	In the category of sets monomorphisms are injections." 
	^ self isInjective! !

!Function methodsFor: 'testing' stamp: 'len 5/10/2019 07:34:08'!
isRingHomomorphism
	(self domain isRing and: [self codomain isRing]) ifFalse: [^ false].
	self domain isFinite ifFalse: [^ self error: 'infinite domain'].
	(self value: self domain one) = self codomain one ifFalse: [^ false].
	^ self domain allSatisfy: [:x|
		self domain allSatisfy: [:y|
			(self value: x) + (self value: y) = (self value: x + y) and:
				[(self value: x) * (self value: y) = (self value: x * y)]]]! !

!Function methodsFor: 'testing' stamp: 'len 5/16/2019 12:16:02'!
isSurjective
	^ self section notNil! !

!Function methodsFor: 'private' stamp: 'len 12/23/2021 15:29:25'!
expression: anObject
	self propertyAt: #expression put: ((anObject isKindOf: Function) ifTrue: [anObject propertyAt: #expression ifAbsent: [anObject]] ifFalse: [anObject])! !

!Function class methodsFor: 'class initialization' stamp: 'len 12/23/2021 13:47:29'!
initialize
	super initialize.
	Smalltalk at: #Sets put: (Category new name: #Sets)! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:34:55'!
example1Function1
	"É is the ring of rational integers, and it has an underying set of elements (rational integers), that can be accessed by sending the message #carrier. We can use this to create a function on the set of integers:"
	É carrier to: É carrier evaluating: [:x| x squared + 1]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:30:27'!
example1Function2
	"The function 'x^2 + 1' on the set of natural numbers.
	This is a Sequence, i.e. function with domain the natural numbers."
	^ Ç to: Ç evaluating: [:x| x squared + 1]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example2Morphism1
	"This is the most common way to define morphisms.
	Ñ^3 is the vector space of 3-tuples with rational coefficients,
	and this defines a linear map on it (an endomorphism)
	by sending the message #to:evaluating: to the vector space.
	This creates an instance of ModuleMap."
	^ Ñ^3 to: Ñ^3 evaluating: [:v| (v x - v y, v x, v z)]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example2Morphism12
	"This, on the other hand, is a function on the underlying set of
	Ñ^3. Is is a morphism in the category of sets, but is not a morphism
	of vector spaces. This creates an instance of Function."
	^ (Ñ^3) carrier to: (Ñ^3) carrier evaluating: [:v| (v x - v y, v x, v z)]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example2Morphism2
	"Here's another way to define the same morphism,
	by creating vector space Hom(Q^3, Q^3) of linear maps
	and sending the message #evaluating: to it."
	^ Ñ^3 => (Ñ^3) evaluating: [:v| (v x - v y, v x, v z)]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example2Morphism22
	"Or equivalently, creating Hom(Q^3, Q^3) and coercing
	a Smalltalk block into an element of the Hom vector space."
	^ Ñ^3 => (Ñ^3) !! [:v| (v x - v y, v x, v z)]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
example2Morphism3
	"And yet another way. Here Hom(Q^3, Q^3) is created
	by sending the message #endomorphisms to the vector space."
	^ (Ñ^3) endomorphisms !! [:v| (v x - v y, v x, v z)]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:34:55'!
example3Frobenius1
	"One way to define the Frobenius ring homomorphism."
	^ (É / 6) endomorphisms !! [:x| x^6]! !

!Function class methodsFor: 'examples' stamp: 'len 5/23/2020 09:34:55'!
example3Frobenius2
	"The Frobenius ring homomorphism defined by a polynomial."
	| R x |
	R _ É / 6.
	x _ R polynomials x.
	^ R endomorphisms evaluating: x ^ 6! !

!Function class methodsFor: 'instance creation' stamp: 'len 7/7/2020 09:15:30'!
from: aDomain to: aCodomain evaluating: anExpression
	^ self new domain: aDomain; codomain: aCodomain; expression: anExpression! !

!Function class methodsFor: 'instance creation' stamp: 'len 10/17/2016 10:43'!
from: aDomain to: aCodomain evaluatingWithArguments: aBlock
	^ self from: aDomain to: aCodomain evaluating: [:each| aBlock valueWithArguments: each asArray]! !

!Functor class methodsFor: 'class initialization' stamp: 'len 4/26/2020 17:41:52'!
initialize
	super initialize.
	Smalltalk at: #Categories put: (Category new name: #Categories)! !

!Object methodsFor: '*categories' stamp: 'len 4/22/2020 06:16:21'!
adapt: anElement andSend: selector
	self parent ifNotNil: [:aDomain| (aDomain adapt: anElement) ifNotNil: [:adapted| ^ self perform: selector with: adapted]].
	anElement parent ifNotNil: [:aDomain| (aDomain adapt: self) ifNotNil: [:adapted| ^ adapted perform: selector with: anElement]].
	^ DomainError signal: 'coercion failed'! !

!Object methodsFor: '*categories' stamp: 'len 11/6/2016 09:07'!
adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]! !

!Object methodsFor: '*categories' stamp: 'len 10/26/2020 14:21:55'!
id
	^ self -> self! !

!Object methodsFor: '*categories' stamp: 'len 5/8/2020 15:46:45'!
isDomain
	^ false! !

!Object methodsFor: '*categories' stamp: 'len 4/22/2020 06:09:43'!
isElement
	"Answer true if the receiver is an element of a Domain, i.e. if it understands the message #parent and answers its parent Domain."
	^ self parent notNil! !

!Object methodsFor: '*categories' stamp: 'len 5/19/2018 17:53:50'!
isEvaluable
	^ self isBlock! !

!Object methodsFor: '*categories' stamp: 'len 5/8/2020 15:46:36'!
isMorphism
	^ false! !

!Object methodsFor: '*categories' stamp: 'len 5/22/2018 16:53:19'!
isSequenceable
	"Answer true if the receiver is some sort of ordered tuple of objects, understands #at: and #size and it's indexed by integers starting at 1."
	^ false! !

!Object methodsFor: '*categories' stamp: 'len 4/22/2020 06:09:50'!
parent
	^ nil! !

!Object methodsFor: '*categories' stamp: 'len 5/29/2020 06:29:20'!
printText
	| answer |
	answer _ Text streamContents: [:aStream| self printOn: aStream].
	self validate ifFalse: [answer under].
	^ answer! !

!Object methodsFor: '*categories' stamp: 'len 5/29/2020 06:23:48'!
validate
	"Answer false if the internal invariants of this object are broken."
	^ true! !

!Object class methodsFor: '*categories' stamp: 'len 7/7/2020 11:06:32'!
!! anObject
	(self newFrom: anObject) ifNotNil: [:anInstance| ^ anInstance].
	^ DomainError signal: 'coercion failed'! !

!Workspace methodsFor: '*categories' stamp: 'len 11/23/2015 03:28'!
bindingOf: aString
	mustDeclareVariables ifTrue: [^ nil].
	(bindings includesKey: aString) ifFalse: [
		"aString first isUppercase
			ifTrue: [^nil]
			ifFalse: ["bindings at: aString put: nil]"]".
	^bindings associationAt: aString! !

!Class methodsFor: '*categories' stamp: 'len 12/1/2016 09:03:19'!
newSubclass
	"Answer a new subclass of the receiver. The new subclass will be unnamed and uncategorized, and if no instances exist it can be subject of garbage collection."
	| newMetaclass |
	newMetaclass _ Metaclass new.
	newMetaclass
		superclass: self class
		methodDictionary: MethodDictionary new
		format: self class format.
	^ newMetaclass new
		superclass: self
		methodDictionary: MethodDictionary new
		format: self format! !

!Association methodsFor: '*categories' stamp: 'len 10/26/2020 12:15:02'!
* anAssociation
	^ self ∑ anAssociation! !

!Association methodsFor: '*categories' stamp: 'len 10/26/2020 10:44:58'!
∑ anAssociation
	self value = anAssociation key ifFalse: [DomainError signal: 'domain and codomain don''t match'].
	^ self class key: key value: anAssociation value! !

!Association methodsFor: '*categories' stamp: 'len 10/26/2020 10:45:16'!
inverse
	^ self class key: value value: key! !

!BlockClosure methodsFor: '*categories' stamp: 'len 4/24/2016 06:00'!
count
	| count |
	count _ 0.
	self whileTrue: [count _ count + 1].
	^ count! !

!Collection methodsFor: '*categories' stamp: 'len 5/10/2019 04:26:01'!
/\ aCollection
	^ self intersection: aCollection! !

!Collection methodsFor: '*categories' stamp: 'len 2/19/2017 09:13:36'!
\ aCollection
	"Answer the complement of aCollection in the receiver."
	^ self reject: [:each| aCollection includes: each]! !

!Collection methodsFor: '*categories' stamp: 'len 5/10/2019 04:26:11'!
\/ aCollection
	^ self union: aCollection! !

!Collection methodsFor: '*categories' stamp: 'len 5/6/2019 03:01:10'!
∑ anObject
	^ anObject adaptToCollection: self andSend: #∑! !

!Collection methodsFor: '*categories' stamp: 'len 2/18/2003 21:40'!
anyIfNone: aBlock
	"Answer a representative sample of the receiver. This method can
	be helpful when needing to preinfer the nature of the contents of 
	semi-homogeneous collections."

	self do: [:each | ^ each].
	^aBlock value! !

!Collection methodsFor: '*categories' stamp: 'len 4/27/2018 01:29:32'!
apply: aFunction
	^ self collect: aFunction! !

!Collection methodsFor: '*categories' stamp: 'len 5/25/2018 18:10:51'!
atRandom: aRandom bits: bitSize
	^ self atRandom: aRandom! !

!Collection methodsFor: '*categories' stamp: 'len 1/30/2018 21:35:53'!
atRandomBits: anInteger
	^ self atRandom! !

!Collection methodsFor: '*categories' stamp: 'len 2/13/2003 23:19'!
copyEmpty
	^ self species new! !

!Collection methodsFor: '*categories' stamp: 'len 2/13/2003 23:19'!
copyEmpty: anInteger
	^ self species new: anInteger! !

!Collection methodsFor: '*categories' stamp: 'len 2/19/2017 10:40:52'!
intersects: aCollection
	^ self anySatisfy: [:any| aCollection includes: any]! !

!Collection methodsFor: '*categories' stamp: 'len 5/6/2019 02:58:39'!
inverse
	^ self collect: [:a | a inverse]! !

!Collection methodsFor: '*categories' stamp: 'len 6/23/2016 05:29'!
isFinite
	^ true! !

!Collection methodsFor: '*categories' stamp: 'len 6/23/2016 05:29'!
isInfinite
	^ false! !

!Collection methodsFor: '*categories' stamp: 'len 5/10/2020 12:26:12'!
isSingleton
	"Answer true if the receiver contains exactly one element."
	| seenOneElement |
	seenOneElement _ false.
	self do: [:each| seenOneElement ifTrue: [^ false]. seenOneElement _ true].
	^ seenOneElement! !

!Collection methodsFor: '*categories' stamp: 'len 5/8/2018 07:15:12'!
printElementsOn: aStream
	aStream nextPut: $(.
	self do: [:element| aStream print: element] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)! !

!Collection methodsFor: '*categories' stamp: 'len 6/23/2019 04:34:56'!
upTo: max do: aBlock
	"Iterate over the elements of the receiver, just like #do:, but stop after max elements."
	| k |
	k _ 1.
	self do: [:each|
		k > max ifTrue: [^ self].
		aBlock value: each.
		k _ k+1]! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 8/12/2016 22:01'!
<= aCollection
	"Answer true if the receiver is a subcollection of aCollection."
	(aCollection isKindOf: self species) ifFalse: [^ false].
	self size <= aCollection size ifFalse: [^ false].
	^ (aCollection indexOfSubCollection: self startingAt: 1) > 0! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 3/23/2016 05:04'!
at: anInteger add: anObject
	^ self at: anInteger put: (self at: anInteger) + anObject! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 2/21/2017 18:59:40'!
combinations: r
	^ Iterator on: [:aBlock| self combinations: r atATimeDo: [:each| aBlock value: each copy]]! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 11/22/2016 05:40:12'!
convolution: aCollection
	| answer |
	self isEmpty ifTrue: [^ self].
	aCollection isEmpty ifTrue: [^ aCollection].
	answer _ self species new: self size + aCollection size - 1.
	answer atAllPut: (self at: 1) - (self at: 1).
	1 to: self size do: [:i|
		1 to: aCollection size do: [:k|
			answer at: i+k-1 add: (self at: i) * (aCollection at: k)]].
	^ answer! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 3/3/2016 23:07'!
copyWithoutIndex: index 
	| answer |
	answer _ self species new: self size - 1.
	answer replaceFrom: 1 to: index - 1 with: self startingAt: 1.
	answer replaceFrom: index to: answer size with: self startingAt: index + 1.
	^ answer! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 11/6/2016 09:06'!
isZero
	self do: [:each| each isZero ifFalse: [^ false]].
	^ true! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 2/21/2017 19:00:52'!
permutations
	^ Iterator on: [:aBlock| self permutationsDo: [:each| aBlock value: each copy]]! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 11/18/2016 08:42:32'!
permutedBy: aPermutation
	| answer |
	answer _ Array new: self size.
	1 to: self size do: [:i| answer at: (aPermutation at: i) put: (self at: i)].
	^ answer! !

!SequenceableCollection methodsFor: '*categories' stamp: 'len 11/18/2016 08:58:16'!
sum
	| answer |
	answer _ self at: 1.
	2 to: self size do: [:i| answer _ answer + (self at: i)].
	^ answer! !

!Array methodsFor: '*categories' stamp: 'len 3/4/2021 18:06:17'!
>- aCodomain
	"Answer the canonical coprojections from the components of the receiver to the argument.
	See also message -<."
	aCodomain propertyAt: #coprojections ifPresent: [:aDictionary| aDictionary at: self ifPresent: [:anArrayOfMorphisms| ^ anArrayOfMorphisms]].
	^ aCodomain fromAll: self! !

!Array methodsFor: '*categories' stamp: 'len 11/6/2016 09:26'!
printOn: aStream
	self == Smalltalk specialObjectsArray
		ifTrue: [aStream nextPutAll: 'Smalltalk specialObjectsArray' ]
		ifFalse:
			[aStream nextPut: $(.
			self do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
			aStream nextPut: $)]! !

!Interval methodsFor: '*categories' stamp: 'len 11/14/2016 13:59'!
isStandard
	"Answer true if the receiver is an interval [1..n]."
	^ start = 1 and: [stop isInteger and: [stop >= 1 and: [stop - start + 1 = count]]]! !

!Interval methodsFor: '*categories' stamp: 'len 11/14/2016 10:15'!
printOn: aStream
	self increment = 1
		ifTrue: [aStream nextPut: $[; print: start; nextPutAll: '..'; print: stop; nextPut: $]]
		ifFalse: [self storeOn: aStream]! !

!Interval methodsFor: '*categories' stamp: 'len 11/14/2016 10:14'!
storeOn: aStream
	aStream nextPut: $(; print: start; nextPutAll: ' to: '; print: stop.
	self increment ~= 1 ifTrue: [aStream nextPutAll: ' by: '; print: self increment].
	aStream nextPut: $)! !

!Bag methodsFor: '*categories' stamp: 'len 4/24/2018 17:42:11'!
apply: aFunction
	| answer |
	answer _ self species new.
	self withOccurrencesDo: [:each :times| answer add: (aFunction value: each) withOccurrences: times].
	^ answer! !

!Bag methodsFor: '*categories' stamp: 'len 8/5/2021 08:32:53'!
printOn: aStream
	aStream nextPut: ${.
	self asSet
		do: [:each|
			| times |
			times _ self occurrencesOf: each.
			(each printString isAlphaNumeric or: [times = 1])
				ifTrue: [aStream print: each]
				ifFalse: [aStream nextPut: $(; print: each; nextPut: $)].
			times > 1 ifTrue: [aStream nextPutAll: times printString super]]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $}! !

!Bag methodsFor: '*categories' stamp: 'len 12/10/2016 11:26:24'!
withOccurrencesDo: aBlock
	contents associationsDo: [:each| aBlock value: each key value: each value]! !

!Set methodsFor: '*categories' stamp: 'len 7/31/2020 11:44:10'!
asDomain
	^ self as: Structure! !

!Set methodsFor: '*categories' stamp: 'len 11/7/2016 09:28'!
printOn: aStream
	(self isMemberOf: Set)
		ifFalse: [^ super printOn: aStream].
	aStream nextPut: ${.
	self do: [:each| aStream print: each] separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $}! !

!Dictionary methodsFor: '*categories' stamp: 'len 11/30/2016 14:47:44'!
at: key add: value
	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	assoc
		ifNil:
			[value isZero ifFalse: [self atNewIndex: index put: (Association key: key value: value)]]
		ifNotNil:
			[|newValue|
			newValue _ assoc value + value.
			assoc value: newValue.
			^ newValue].
	^ value! !

!Dictionary methodsFor: '*categories' stamp: 'len 11/30/2016 14:47:14'!
at: key addAndRemoveIfZero: value
	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	assoc
		ifNil:
			[value isZero ifFalse: [self atNewIndex: index put: (Association key: key value: value)]]
		ifNotNil:
			[|newValue|
			newValue _ assoc value + value.
			newValue isZero
				ifTrue:
					[array at: index put: nil.
					tally _ tally - 1.
					self fixCollisionsFrom: index]
				ifFalse: [assoc value: newValue].
			^ newValue].
	^ value! !

!Dictionary methodsFor: '*categories' stamp: 'len 3/6/2017 19:54:46'!
printElementsOn: aStream
	aStream nextPut: ${.
	self keysSortedSafely do:
		[:key | aStream print: key; nextPutAll: '->'; print: (self at: key)] separatedBy: [aStream space].
	aStream nextPut: $}! !

!Stream methodsFor: '*categories' stamp: 'len 5/15/2020 06:10:16'!
printWithParenthesis: anObject
	(anObject printString includesAnyOf: ' +-/')
		ifTrue: [self nextPut: $(; print: anObject; nextPut: $)]
		ifFalse: [self print: anObject]! !

!StrikeFont class methodsFor: '*categories-character shapes' stamp: 'len 12/8/2016 12:11:12'!
buildMathFont
	"
	StrikeFont buildMathFont
	"
	#(5 6 7 8 9 10 11 12 13 14 17 22) do: [:pointSize| self buildMathFont: pointSize]! !

!StrikeFont class methodsFor: '*categories-character shapes' stamp: 'len 3/7/2017 08:00:46'!
buildMathFont: pointSize
	| font normal larger muchLarger |
	font _ AbstractFont familyName: 'DejaVu Sans Mono' pointSize: pointSize.
	font characterToGlyphMap at: 96 put: 28. "use left arrow instead of underscore"
	normal _ AbstractFont familyName: 'DejaVu' pointSize: pointSize.
"	#(1 0) detect: [:one| (slightlyLarger _ (AbstractFont familyName: 'DejaVu' pointSize: pointSize+one)) notNil]."
	#(2 1 3 0) detect: [:one| (larger _ (AbstractFont familyName: 'DejaVu' pointSize: pointSize+one)) notNil].
	#(4 3 2 1 0) detect: [:one| (muchLarger _ (AbstractFont familyName: 'DejaVu' pointSize: pointSize+one)) notNil].
	"replace some math glyphs with their normal counterpart, because the monospaced glyphs are far too small (and some are missing):"
	'ÄÅÇÉÖÜáàâäãåçéèêëíìîïñóòôöõúùû' do: [:each|
		font takeAllGlyphFor: each from: each in: normal].
"	'ÄÉÑÖÜâäãåçéè' do: [:each|
		font takeAllGlyphFor: each from: each in: slightlyLarger].
"	'Äêëíìîïñóòôöõú' do: [:each|
		font takeAllGlyphFor: each from: each in: larger].
	'Ä' do: [:each|
		font takeAllGlyphFor: each from: each in: muchLarger].
	"make arrows bigger"
	'' do: [:each|
		font takeAllGlyphFor: each from: each in: larger]! !

!StrikeFont class methodsFor: '*categories-character shapes' stamp: 'len 3/7/2017 08:03:34'!
installMathFonts
"
StrikeFont installMathFonts
"
	| family codeFamily |
	StrikeFont install: 'DejaVu'.
	StrikeFont install: 'DejaVu Sans Mono'.
	StrikeFont buildMathFont.
	Character initialize.
	family _ 'DejaVu Sans Mono'.
	codeFamily _ 'DejaVu Sans Mono'.
	Preferences setDefaultFonts: {
		{#setSystemFontTo:. family. 10}.
		{#setListFontTo:. family. 10}.
		{#setMenuFontTo:. family. 9}.
		{#setWindowTitleFontTo:. family. 10}.
		{#setCodeFontTo:. codeFamily. 10}.
		{#setButtonFontTo:. family. 9}}.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ].
! !
Naturals initialize!
Function initialize!
Functor initialize!
