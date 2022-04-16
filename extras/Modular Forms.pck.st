'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 2:12:59 pm'!
'Description '!
!provides: 'Modular Forms' 1 2!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: #'Modular Forms'!


!classDefinition: #ArithmeticGroup category: #'Modular Forms'!
Group subclass: #ArithmeticGroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ArithmeticGroup class' category: #'Modular Forms'!
ArithmeticGroup class
	instanceVariableNames: ''!

!classDefinition: #CongruenceSubgroup category: #'Modular Forms'!
ArithmeticGroup subclass: #CongruenceSubgroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'CongruenceSubgroup class' category: #'Modular Forms'!
CongruenceSubgroup class
	instanceVariableNames: ''!

!classDefinition: #Gamma0CongruenceSubgroup category: #'Modular Forms'!
CongruenceSubgroup subclass: #Gamma0CongruenceSubgroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'Gamma0CongruenceSubgroup class' category: #'Modular Forms'!
Gamma0CongruenceSubgroup class
	instanceVariableNames: ''!

!classDefinition: #Gamma1CongruenceSubgroup category: #'Modular Forms'!
CongruenceSubgroup subclass: #Gamma1CongruenceSubgroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'Gamma1CongruenceSubgroup class' category: #'Modular Forms'!
Gamma1CongruenceSubgroup class
	instanceVariableNames: ''!

!classDefinition: #PrincipalCongruenceSubgroup category: #'Modular Forms'!
CongruenceSubgroup subclass: #PrincipalCongruenceSubgroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'PrincipalCongruenceSubgroup class' category: #'Modular Forms'!
PrincipalCongruenceSubgroup class
	instanceVariableNames: ''!

!classDefinition: #ModularGroup category: #'Modular Forms'!
PrincipalCongruenceSubgroup subclass: #ModularGroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ModularGroup class' category: #'Modular Forms'!
ModularGroup class
	instanceVariableNames: ''!

!classDefinition: #DirichletGroup category: #'Modular Forms'!
Group subclass: #DirichletGroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'DirichletGroup class' category: #'Modular Forms'!
DirichletGroup class
	instanceVariableNames: ''!

!classDefinition: #MoebiusGroup category: #'Modular Forms'!
Group subclass: #MoebiusGroup
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'MoebiusGroup class' category: #'Modular Forms'!
MoebiusGroup class
	instanceVariableNames: ''!

!classDefinition: #UpperHalfPlane category: #'Modular Forms'!
Structure subclass: #UpperHalfPlane
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'UpperHalfPlane class' category: #'Modular Forms'!
UpperHalfPlane class
	instanceVariableNames: ''!

!classDefinition: #DirichletCharacter category: #'Modular Forms'!
GroupMap subclass: #DirichletCharacter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'DirichletCharacter class' category: #'Modular Forms'!
DirichletCharacter class
	instanceVariableNames: ''!

!classDefinition: #LinearFractionalMap category: #'Modular Forms'!
RationalMap subclass: #LinearFractionalMap
	instanceVariableNames: 'a b c d'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'LinearFractionalMap class' category: #'Modular Forms'!
LinearFractionalMap class
	instanceVariableNames: ''!

!classDefinition: #ModularMap category: #'Modular Forms'!
LinearFractionalMap subclass: #ModularMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ModularMap class' category: #'Modular Forms'!
ModularMap class
	instanceVariableNames: ''!

!classDefinition: #ModularForm category: #'Modular Forms'!
Function subclass: #ModularForm
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ModularForm class' category: #'Modular Forms'!
ModularForm class
	instanceVariableNames: ''!

!classDefinition: #EisensteinSeries category: #'Modular Forms'!
ModularForm subclass: #EisensteinSeries
	instanceVariableNames: 'weight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'EisensteinSeries class' category: #'Modular Forms'!
EisensteinSeries class
	instanceVariableNames: ''!

!classDefinition: #FareySymbol category: #'Modular Forms'!
Object subclass: #FareySymbol
	instanceVariableNames: 'group fractions pairings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'FareySymbol class' category: #'Modular Forms'!
FareySymbol class
	instanceVariableNames: ''!

!classDefinition: #ManinSymbol category: #'Modular Forms'!
Object subclass: #ManinSymbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ManinSymbol class' category: #'Modular Forms'!
ManinSymbol class
	instanceVariableNames: ''!

!classDefinition: #ModularGroupTest category: #'Modular Forms'!
Object subclass: #ModularGroupTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ModularGroupTest class' category: #'Modular Forms'!
ModularGroupTest class
	instanceVariableNames: ''!

!classDefinition: #ModularSymbol category: #'Modular Forms'!
Object subclass: #ModularSymbol
	instanceVariableNames: 'alpha beta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Modular Forms'!
!classDefinition: 'ModularSymbol class' category: #'Modular Forms'!
ModularSymbol class
	instanceVariableNames: ''!


!ArithmeticGroup commentStamp: '<historical>' prior: 0!
Finite index subgroups of the modular group.!

!CongruenceSubgroup commentStamp: '<historical>' prior: 0!
Congruence subgroups of the modular group SL(2,Z), i.e. subgroups that contain the kernel of SL(2,Z) -> SL(2,Z/nZ) for some positive integer n (which is called the level of the congruence subgroup). See also ModularGroup.!

!Gamma0CongruenceSubgroup commentStamp: '<historical>' prior: 0!
Subgroups of SL(2,Z) associated with matrices of the form:
  [a b]   [* *]
  [c d] = [0 *] (mod n).!

!Gamma1CongruenceSubgroup commentStamp: '<historical>' prior: 0!
Subgroups of SL(2,Z) associated with matrices of the form:
  [a b]   [1 *]
  [c d] = [0 1] (mod n).!

!PrincipalCongruenceSubgroup commentStamp: '<historical>' prior: 0!
Principal congruence subgroups of SL(2,Z), i.e. subgroups of SL(2,Z) associated with matrices of the form:
  [a b]   [1 0]
  [c d] = [0 1] (mod n).

They are the kernel of the quotient projection SL(2,Z) -> SL(2,Z/nZ) for some positive integer n (which is called the level of the congruence subgroup). Since they are kernels, my instances are normal subgroups.!

!ModularGroup commentStamp: '<historical>' prior: 0!
The special linear group SL(2,Z).!

!DirichletGroup commentStamp: '<historical>' prior: 0!
Groups of Dirichlet characters modulo n, whose elements are group homomorphisms from (Z/nZ)* to C* (see DirichletCharacter).!

!MoebiusGroup commentStamp: '<historical>' prior: 0!
My instances are groups of Moebius transformations acting on some space, like the Riemann sphere P^1(C) or the upper half plane H or the extended upper half plane H U P^1(Q).!

!UpperHalfPlane commentStamp: '<historical>' prior: 0!
This is the upper half complex plane H, i.e. the set of complex numbers with imaginary part > 0.

This serves as a model of the hyperbolic plane, defining lines as:
1. the intersection of the half-plane H and Euclidean lines orthogonal to the real axis, or
2. the intersection of the half-plane H and Euclidean half-circles with center on the real axis.

The group of hyperbolic isometries, as well as conformal automorphisms, is the general Moebius group. This is the group of Moebius transformations, i.e. transformations of the form z |-> (az + b) / (cz + d) with ad - cd ~= 0 (or without loss of generality ad - cd = 1).!

!DirichletCharacter commentStamp: '<historical>' prior: 0!
Characters on the units of Z/nZ, i.e. group homomorphisms (Z/nZ)* -> C* from the unit group of the ring Z/nZ to the multiplicative group of nonzero complex numbers.!

!LinearFractionalMap commentStamp: '<historical>' prior: 0!
Transformations of the form
	z |-> az + b / (cz + d)
where ad - cb ~= 0.!

!ModularForm commentStamp: '<historical>' prior: 0!
A modular form of weight k is a holomorphic function f:H -> C from the upper half plane to the complex plane, satisfying for a,b,c,d coefficients of a matrix in SL(2,R)
	f((az + b) / (cz + d)) = (cz + d)^k f(z)
where k is a fixed integer or half-integer. Modular forms of weight 0 are called modular functions, and are invariant under the action of SL(2,R). The factor (cz + d)^k, which is 1 in the case of modular functions, is called the automorphic factor.

Modular forms are functions having an infinite group of symmetries and many beautiful properties. They are one of the five basic operations in arithmetic, together with addition, substraction, multiplication and division.

A modular form f(z) can be seen as power series on a new variable q = e^(2 pi i z). This is its Fourier expansion or q-expansion. The coefficients a(n) of the q-expansion are usually interesting arithmetic functions.

Given a group G, the set of modular forms of a given weight k form a finite-dimensional vector space M_k(G). See ModularFormSpace. The set of all modular forms of all weights is noted M_*(G) and it's a ring. All modular functions form a field.!

!FareySymbol commentStamp: '<historical>' prior: 0!
Farey symbols give information about finite index subgroups of the modular group (such as index, generators, etc) and about the associated modular curve (such as genus and rank of the fundamental group).

References
	R. S. Kulkarni, "An arithmetic-geometric method in the study of the subgroups of the modular group", American Journal of Mathematics 113 (1991), no. 6, 1053-1133.
	Chris A. Kurth, Ling Long, "Computations with finite index subgroups of PSL(2,Z) using Farey symbols".
	John Voight, "Computational methods for modular and Shimura curves", lecture videos in youtube.!

!Gamma0CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 07:37:35'!
printOn: aStream
	aStream nextPutAll: '—', '0' sub,'('; print: self level; nextPut: $)! !

!Gamma1CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 07:37:47'!
printOn: aStream
	aStream nextPutAll: '—', '1' sub,'('; print: self level; nextPut: $)! !

!PrincipalCongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 07:40:36'!
printOn: aStream
	aStream nextPut: $—.
	self level ~= 1 ifTrue: [aStream nextPut: $(; print: self level; nextPut: $)]! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 07:38:23'!
printOn: aStream
	aStream nextPut: $—! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:13:12'!
printOn: aStream
	aStream nextPut: $(; print: ƒ; nextPut: $/; print: self modulus; nextPutAll: ')', '*' super! !

!UpperHalfPlane methodsFor: 'printing' stamp: 'len 5/23/2020 07:42:50'!
printOn: aStream
	aStream nextPut: $H! !

!LinearFractionalMap methodsFor: 'printing' stamp: 'len 5/23/2020 09:31:47'!
printOn: aStream
	| z denominator numerator |
	z _ († polynomialsIn: #(z)) x.
	c isZero ifTrue: [aStream print: z * a + b / d. ^ self].
	numerator _ z * a + b.
	(a = 0 or: [b = 0])
		ifTrue: [aStream print: numerator]
		ifFalse: [aStream nextPut: $(; print: numerator; nextPut: $)].
	aStream nextPut: $/.
	denominator _ z * c + d.
	(c = 0 or: [d = 0])
		ifTrue: [aStream print: denominator]
		ifFalse: [aStream nextPut: $(; print: denominator; nextPut: $)]! !

!EisensteinSeries methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 20:41:01'!
printOn: aStream
	aStream nextPutAll: 'G', weight printString sub! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:30:43'!
printOn: aStream
	| n |
	n _ self size.
	aStream print: Infinity negative.
	1 to: n do: [:i|
		aStream nextPutAll: ' (', (self pairingAt: i) asString, ') '; print: (self numeratorAt: i); nextPut: $/; print: (self denominatorAt: i)].
	aStream nextPutAll: ' (', (self pairingAt: n+1) asString, ') '; print: Infinity positive! !

!ArithmeticGroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:03:47'!
action
	^ GroupAction from: (self, †) to: † evaluatingWithArguments: [:f :z| f value: z]! !

!ArithmeticGroup methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 12:20:53'!
ambient
	^ ModularGroup new! !

!ArithmeticGroup methodsFor: 'as yet unclassified' stamp: 'len 12/21/2016 16:04:00'!
identity
	^ ModularMap I! !

!ArithmeticGroup methodsFor: 'as yet unclassified' stamp: 'len 12/21/2016 16:02:29'!
orderOf: aModularMap
	| trace |
	aModularMap isIdentity ifTrue: [^ 1].
	"aModularMap negated isIdentity ifTrue: [^ 2]."
	trace _ aModularMap trace.
	(trace <= -2 or: [trace >= 2]) ifTrue: [^ Infinity positive].
	trace == 1 ifTrue: [^ 6].
	trace == -1 ifTrue: [^ 3].
	"trace == 0"
	^ 4! !

!ArithmeticGroup methodsFor: 'as yet unclassified' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new! !

!ArithmeticGroup methodsFor: 'as yet unclassified' stamp: 'len 12/21/2016 16:01:59'!
upperHalfPlaneAction
	^ GroupAction from: (self, HH) to: HH evaluating: [:f :z| f value: z]! !

!CongruenceSubgroup methodsFor: 'accessing' stamp: 'len 12/28/2016 11:29:40'!
level
	^ self propertyAt: #level! !

!CongruenceSubgroup methodsFor: 'accessing-private' stamp: 'len 12/28/2016 11:29:48'!
level: anInteger
	self propertyAt: #level put: anInteger! !

!CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 23:14:57'!
indexOf: aSubgroup
	^ (aSubgroup isKindOf: CongruenceSubgroup) ifTrue: [aSubgroup index / self index] ifFalse: [super indexOf: aSubgroup]! !

!CongruenceSubgroup methodsFor: 'operations' stamp: 'len 12/11/2016 07:51:01'!
curve
	"Answer the modular curve associated to the receiver. This is the orbit space of H/G (the upper half plane modulo the receiver) compactified canonically by adding the cusps."
	^ self notYetImplemented! !

!CongruenceSubgroup methodsFor: 'operations' stamp: 'len 12/7/2016 12:06:58'!
cusps
	"Answer the set of cusps of the receiver, i.e. the set of orbits of P^1(Q) under the action of the group. This set is finite."
	^ self notYetImplemented! !

!CongruenceSubgroup methodsFor: 'operations' stamp: 'len 12/11/2016 07:51:21'!
genus
	^ self curve genus
"(1 + projectiveIndex) / 12 - nu2 / 4 - nu3 / 4 - ncusps/2"! !

!CongruenceSubgroup methodsFor: 'operations' stamp: 'len 6/1/2020 15:10:39'!
widthOf: aCusp
	"Algorithm 1.19 (Width of a cusp) in Stein's 'Modular Forms, A Computational Approach'."
	| gamma |
	aCusp isInfinite
		ifTrue:
			[gamma _ self identity]
		ifFalse:
			[| a b xgcd |
			a _ aCusp numerator.
			b _ aCusp denominator.
			xgcd _ a xgcd: b.
			gamma _ Matrix rows: {(a, b). ((xgcd at: 2), (xgcd at: 3))}].
	"..."! !

!CongruenceSubgroup methodsFor: 'testing' stamp: 'len 6/2/2018 17:59:42'!
isEven
	self flag: #fix. "this makes sense for the homogeneous group only, ~ SL(2,Z)"
	^ self includes: self identity negated! !

!CongruenceSubgroup methodsFor: 'testing' stamp: 'len 6/2/2018 17:59:51'!
isOdd
	self flag: #fix. "this makes sense for the homogeneous group only, ~ SL(2,Z)"
	^ self even not! !

!CongruenceSubgroup class methodsFor: 'instance creation' stamp: 'len 12/7/2016 11:56:57'!
new: anInteger
	^ self new level: anInteger! !

!Gamma0CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 12:24:21'!
even
	^ true! !

!Gamma0CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:38:59'!
index
	"Answer the index of the receiver in the modular group."
	| answer |
	answer _ 1.
	self level factorization withOccurrencesDo: [:p :e| answer _ answer * (p ^ e + (p ^ (e - 1)))].
	^ answer
	! !

!Gamma0CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 12:19:31'!
numberOfCusps
	| n |
	n _ self level.
	^ n divisors sum: [:d| (d gcd: n // d) phi]! !

!Gamma1CongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 12:28:30'!
even
	^ self level = 1 or: [self level = 2]! !

!PrincipalCongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 23:02:54'!
* aGroup
	(aGroup isKindOf: PrincipalCongruenceSubgroup)
		ifFalse: [^ super * aGroup].
	^ self class new: (self level gcd: aGroup level)! !

!PrincipalCongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 5/10/2019 04:33:25'!
/\ aGroup
	"Answer the intersection of the receiver and the argument."
	(aGroup isKindOf: PrincipalCongruenceSubgroup)
		ifFalse: [^ super /\ aGroup].
	^ self class new: (self level lcm: aGroup level)! !

!PrincipalCongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:42:28'!
index
	"Answer the index of the receiver in the modular group."
	| answer |
	answer _ 1.
	self level factorization withOccurrencesDo: [:p :e| answer _ answer * (p ^ (e*3 - 2) * (p*p - 1))].
	^ answer! !

!PrincipalCongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:42:32'!
numberOfCusps
	| count |
	self level = 1 ifTrue: [^ 1].
	self level = 2 ifTrue: [^ 3].
	count _ 1.
	self level factorization withOccurrencesDo: [:p :e| count _ count * (p ^ (e*2) - (p ^ (e*2 - 2)))].
	^ count // 2! !

!PrincipalCongruenceSubgroup methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 11:34:29'!
numberOfIrregularCusps
	^ 0! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 12/24/2016 12:06:20'!
S
	^ ModularMap S! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 12/24/2016 12:06:24'!
T
	^ ModularMap T! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 12/24/2016 12:08:05'!
T: n
	^ ModularMap a: 1 b: n c: 0 d: 1! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 12/21/2016 16:06:26'!
generators
	^ {ModularMap T. ModularMap S}! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 12/11/2016 07:53:49'!
level
	^ 1! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 12/11/2016 07:53:46'!
level: anInteger
	^ self shouldNotImplement! !

!ModularGroup methodsFor: 'as yet unclassified' stamp: 'len 8/9/2020 07:46:51'!
reduce: aPoint
	"Given a point in the upper half plane, return an element of the receiver that maps the point to a representative in the standard fundamental domain.
	Reference: Henri Cohen, 'A Course in Computational Algebraic Number Theory', Algorithm 7.4.2."
	| tau A n m |
	tau _ aPoint.
	A _ self identity.
	[n _ tau real rounded.
	tau _ tau - n.
	A _ (self T: n negated) · A.
	m _ tau * tau conjugate.
	m >= 1 ifTrue: [^ {tau. A}].
	tau _ tau conjugate negated / m.
	A _ self S · A] repeat! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 11:30:39'!
codomain
	"Answer the codomain of the characters."
	^ self propertyAt: #codomain ifAbsentPut: [CyclotomicField new: self domain exponent]! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 11:30:49'!
codomain: aCodomain
	self propertyAt: #codomain put: aCodomain! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:38:30'!
decomposition
	| answer |
	answer _ OrderedCollection new.
	self modulus factorization withOccurrencesDo:
		[:p :e| answer add: (self class new: p^e to: self codomain)].
	^ answer! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 12/28/2016 11:30:54'!
domain
	^ self propertyAt: #domain! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 13:34:31'!
hash
	^ self domain hash hashMultiply + self codomain hash! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 12/9/2016 15:49:11'!
modulus
	^ self domain modulus! !

!DirichletGroup methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:13:12'!
modulus: anInteger
	self propertyAt: #domain put: (ƒ / anInteger) units! !

!DirichletGroup class methodsFor: 'instance creation' stamp: 'len 12/9/2016 15:51:30'!
new: anInteger
	^ self new modulus: anInteger! !

!DirichletGroup class methodsFor: 'instance creation' stamp: 'len 12/10/2016 13:26:28'!
new: anInteger to: aCodomain
	^ self new modulus: anInteger; codomain: aCodomain! !

!UpperHalfPlane methodsFor: 'testing' stamp: 'len 12/7/2016 12:11:09'!
includes: anObject
	^ anObject isComplex and: [anObject imaginary > 0]! !

!UpperHalfPlane class methodsFor: 'initialization' stamp: 'len 12/6/2016 22:07:33'!
initialize
	super initialize.
	Smalltalk at: #HH put: self new! !

!DirichletCharacter methodsFor: 'as yet unclassified' stamp: 'len 8/5/2021 08:38:19'!
conductor
	| factors p answer |
	(self modulus = 1 or: [self isTrivial]) ifTrue: [^ 1].
	(factors _ self modulus factorization) asSet size > 1
		ifTrue: [^ self decomposition product: [:each| each conductor]].
	p _ factors anyOne.
"	e _ factors size."
	answer _ p ^ ((self size abs: p) + 1).
"	p = 2 and: [e > 2 and: [ ..."
	^ answer! !

!DirichletCharacter methodsFor: 'as yet unclassified' stamp: 'len 8/3/2019 08:58:00'!
even
	^ (self value: self domain !! -1) = self codomain one! !

!DirichletCharacter methodsFor: 'as yet unclassified' stamp: 'len 12/9/2016 12:28:30'!
isPrimitive
	^ self conductor = self modulus! !

!DirichletCharacter methodsFor: 'as yet unclassified' stamp: 'len 12/9/2016 12:21:35'!
modulus
	^ self domain modulus! !

!DirichletCharacter methodsFor: 'as yet unclassified' stamp: 'len 8/3/2019 08:58:14'!
odd
	^ (self value: self domain !! -1) = self codomain one negated! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 12/9/2016 10:07:23'!
a
	^ a! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 12/9/2016 10:07:26'!
b
	^ b! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 12/9/2016 10:07:29'!
c
	^ c! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 5/23/2020 09:31:47'!
codomain
	^ †! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 12/9/2016 10:07:31'!
d
	^ d! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 5/23/2020 09:31:47'!
domain
	^ †! !

!LinearFractionalMap methodsFor: 'accessing' stamp: 'len 12/9/2016 16:30:21'!
identity
	^ self class I! !

!LinearFractionalMap methodsFor: 'accessing-private' stamp: 'len 12/9/2016 10:08:43'!
a: aNumber
	a _ aNumber! !

!LinearFractionalMap methodsFor: 'accessing-private' stamp: 'len 12/9/2016 10:08:45'!
b: aNumber
	b _ aNumber! !

!LinearFractionalMap methodsFor: 'accessing-private' stamp: 'len 12/9/2016 10:08:48'!
c: aNumber
	c _ aNumber! !

!LinearFractionalMap methodsFor: 'accessing-private' stamp: 'len 12/9/2016 10:08:50'!
d: aNumber
	d _ aNumber! !

!LinearFractionalMap methodsFor: 'comparing' stamp: 'len 12/10/2016 11:51:23'!
= aMoebiusMap
	aMoebiusMap species = self species ifFalse: [^ super = aMoebiusMap].
	^ a = aMoebiusMap a and: [b = aMoebiusMap b and: [c = aMoebiusMap c and: [d = aMoebiusMap d]]]! !

!LinearFractionalMap methodsFor: 'comparing' stamp: 'len 12/10/2016 11:51:45'!
hash
	^ ((a hash + b hash) hashMultiply + c hash) hashMultiply + d hash! !

!LinearFractionalMap methodsFor: 'converting' stamp: 'len 6/1/2020 12:38:46'!
asMatrix
	^ (self domain scalars^2) endomorphisms !! {a. b. c. d}! !

!LinearFractionalMap methodsFor: 'converting' stamp: 'len 12/28/2016 10:24:10'!
asRationalFunction
	| z |
	z _ (self domain scalars polynomialsIn: #(z)) fractions x.
	^ z * a + b / (z * c + d)! !

!LinearFractionalMap methodsFor: 'operations' stamp: 'len 3/13/2020 06:14:59'!
· aModularMap
	self species = aModularMap species ifFalse: [^ super · aModularMap].
	^ self class
		a: a * aModularMap a + (b * aModularMap c)
		b: a * aModularMap b + (b * aModularMap d)
		c: c * aModularMap a + (d * aModularMap c)
		d: c * aModularMap b + (d * aModularMap d)! !

!LinearFractionalMap methodsFor: 'operations' stamp: 'len 12/9/2016 10:14:36'!
inverse
	^ self class a: d b: b negated c: c negated d: a! !

!LinearFractionalMap methodsFor: 'operations' stamp: 'len 12/9/2016 21:50:06'!
trace
	^ a + d! !

!LinearFractionalMap methodsFor: 'operations' stamp: 'len 12/15/2016 17:46:03'!
traceSquared
	^ (a + d) squared! !

!LinearFractionalMap methodsFor: 'operations' stamp: 'len 12/9/2016 10:07:06'!
value: anObject
	^ anObject * a + b / (anObject * c + d)! !

!LinearFractionalMap methodsFor: 'testing' stamp: 'len 12/14/2016 10:31:41'!
isCircular
	^ self trace = 0! !

!LinearFractionalMap methodsFor: 'testing' stamp: 'len 12/15/2016 17:48:21'!
isElliptic
	^ self traceSquared < 4! !

!LinearFractionalMap methodsFor: 'testing' stamp: 'len 12/15/2016 17:48:27'!
isHyperbolic
	^ self traceSquared > 4! !

!LinearFractionalMap methodsFor: 'testing' stamp: 'len 12/10/2016 12:04:59'!
isIdentity
	^ a = 1 and: [b = 0 and: [c = 0 and: [d = 1]]]! !

!LinearFractionalMap methodsFor: 'testing' stamp: 'len 12/15/2016 17:48:33'!
isLoxodromic
	| t |
	^ (t _ self traceSquared) < 0 or: [t > 4]! !

!LinearFractionalMap methodsFor: 'testing' stamp: 'len 12/15/2016 17:48:39'!
isParabolic
	^ self traceSquared = 4! !

!LinearFractionalMap class methodsFor: 'instance creation' stamp: 'len 12/9/2016 16:29:49'!
I
	^ self a: 1 b: 0 c: 0 d: 1! !

!LinearFractionalMap class methodsFor: 'instance creation' stamp: 'len 12/9/2016 16:29:54'!
R
	^ self a: 0 b: 1 c: -1 d: 1! !

!LinearFractionalMap class methodsFor: 'instance creation' stamp: 'len 12/9/2016 16:30:03'!
S
	^ self a: 0 b: -1 c: 1 d: 0! !

!LinearFractionalMap class methodsFor: 'instance creation' stamp: 'len 12/9/2016 16:30:11'!
T
	^ self a: 1 b: 1 c: 0 d: 1! !

!LinearFractionalMap class methodsFor: 'instance creation' stamp: 'len 12/9/2016 10:08:59'!
a: a b: b c: c d: d
	^ self new a: a; b: b; c: c; d: d! !

!LinearFractionalMap class methodsFor: 'instance creation' stamp: 'len 12/10/2016 12:37:18'!
matrix: aMatrix
	^ self a: (aMatrix at: 1@1) b: (aMatrix at: 1@2) c: (aMatrix at: 2@1) d: (aMatrix at: 2@2)! !

!ModularMap methodsFor: 'as yet unclassified' stamp: 'len 6/1/2020 13:05:39'!
asMatrix
	^ (ƒ^2) endomorphisms !! {a. b. c. d}! !

!ModularMap methodsFor: 'as yet unclassified' stamp: 'len 12/14/2016 10:26:46'!
isElliptic
	^ self trace abs < 2! !

!ModularMap methodsFor: 'as yet unclassified' stamp: 'len 12/14/2016 10:26:54'!
isHyperbolic
	^ self trace abs > 2! !

!ModularMap methodsFor: 'as yet unclassified' stamp: 'len 12/14/2016 10:27:12'!
isParabolic
	^ self trace abs = 2! !

!ModularForm methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:31:47'!
codomain
	^ †! !

!ModularForm methodsFor: 'as yet unclassified' stamp: 'len 12/11/2016 08:26:35'!
domain
	^ HH! !

!ModularForm methodsFor: 'as yet unclassified' stamp: 'len 12/8/2016 22:05:02'!
expansion
	"Answer the q-expansion (Fourier expansion) of the receiver."
	self notYetImplemented! !

!ModularForm methodsFor: 'as yet unclassified' stamp: 'len 12/8/2016 22:04:11'!
group
	^ self notYetImplemented! !

!ModularForm methodsFor: 'as yet unclassified' stamp: 'len 12/8/2016 22:04:15'!
level
	^ self notYetImplemented! !

!ModularForm methodsFor: 'as yet unclassified' stamp: 'len 12/8/2016 22:04:14'!
weight
	^ self notYetImplemented! !

!EisensteinSeries methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 20:40:21'!
weight
	^ weight! !

!EisensteinSeries methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 20:42:07'!
weight: anInteger
	(anInteger even and: [anInteger >= 4]) ifFalse: [^ self error: 'the weight must be even >= 4'].
	weight _ anInteger! !

!EisensteinSeries class methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 20:42:26'!
new: anInteger
	^ self new weight: anInteger! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/19/2016 22:32:21'!
cusps
	^ self notYetImplemented! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 20:42:22'!
denominatorAt: i
	i == 0 ifTrue: [^ 0].
	i == (fractions size + 1) ifTrue: [^ 0].
	^ (fractions at: i) denominator! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:21:44'!
e2
	"Answer the number of inequivalent order-2 elliptic points."
	^ pairings count: [:each| each == #even]! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:21:53'!
e3
	"Answer the number of inequivalent order-3 elliptic points."
	^ pairings count: [:each| each == #odd]! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:04:58'!
example
	group _ PrincipalCongruenceSubgroup new: 2.
	fractions _ {0. 1. 2}.
	pairings _ {1. 2. 2. 1}! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/21/2016 16:09:10'!
generators
	^ (1 to: fractions size) collect: [:i| self pairingMapAt: i]! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:25:43'!
genus
	^ self rank - self numberOfCusps / 2! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:27:27'!
index
	"Answer the index of the group of the receiver as subgroup of the modular group."
	^ (self size - 1) * 3 + self e3! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 20:41:51'!
numeratorAt: i
	i == 0 ifTrue: [^ -1].
	i == (fractions size + 1) ifTrue: [^ 1].
	^ (fractions at: i) numerator! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:12:26'!
pairingAt: i
	"Answer the pairing between i-1 and i. Each pairing is either a positive integer (a free pairing), #even or #odd."
	^ pairings at: i! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 3/6/2017 17:01:38'!
pairingMapAt: i
	| a b a1 b1 k ak bk ak1 bk1 |
	a _ self numeratorAt: i-1.
	b _ self denominatorAt: i-1.
	a1 _ self numeratorAt: i.
	b1 _ self denominatorAt: i.
	k _ self pairingAt: i.
	k == #even ifTrue: [^ ModularMap a: a1*b1 + (a*b) b: a*a negated - (a1*a1) c: b*b + (b1*b1) d: a1*b1 negated - (a*b)].
	k == #odd ifTrue: [^ ModularMap a: a1*b1 + (a*b1) + (a*b) b: a*a negated - (a*a1) - (a1*a1) c: b*b + (b*b1) + (b1*b1) d: a1*b1 negated - (a1*b) - (a*b)].
	ak _ self numeratorAt: k.
	bk _ self denominatorAt: k.
	ak1 _ self numeratorAt: k+1.
	bk1 _ self denominatorAt: k+1.
	^ ModularMap a: ak1*b1 + (ak*b) b: ak*ak negated - (ak1*a1) c: bk*b + (bk1*b1) d: a1*bk1 negated - (a*bk)! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:23:12'!
r
	"Answer the rank of the fundamental group of the uncompactified modular curve G\H, where G denotes de group of the receiver and H is the upper half plane."
	^ pairings count: [:each| each isInteger]! !

!FareySymbol methodsFor: 'as yet unclassified' stamp: 'len 12/20/2016 21:12:07'!
size
	^ fractions size! !

!ModularGroupTest methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 12:56:12'!
testGamma0Index
	self assert: (1 to: 19 :: collect: [:each| Gamma0CongruenceSubgroup new: each :: index]) = #(1 3 4 6 6 12 8 12 12 18 12 24 14 24 24 24 18 36 20).
	self assert: (Gamma0CongruenceSubgroup new: 32041) index = 32220! !

!ModularGroupTest methodsFor: 'as yet unclassified' stamp: 'len 12/10/2016 12:23:06'!
testGamma0NumberOfCusps
	| p |
	self assert: (1 to: 19 :: collect: [:each| Gamma0CongruenceSubgroup new: each :: numberOfCusps]) =  #(1 2 2 3 2 4 2 4 4 4 2 6 2 4 4 6 2 8 2).
	p _ 1.
	100 timesRepeat: [self assert: (Gamma0CongruenceSubgroup new: (p _ p nextPrime)) numberOfCusps = 2]! !

!ModularGroupTest methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 23:02:48'!
testGammaIndex
	self assert: (1 to: 19 :: collect: [:each| PrincipalCongruenceSubgroup new: each :: index]) =  #(1 6 24 48 120 144 336 384 648 720 1320 1152 2184 2016 2880 3072 4896 3888 6840)! !

!ModularGroupTest methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 23:16:41'!
testIndexOf
	| G H |
	G _ PrincipalCongruenceSubgroup new: 2.
	H _ PrincipalCongruenceSubgroup new: 8.
	self assert: (G indexOf: H) = 32 "according to GAP manual chap. 2, Gamma(2) has index 12"! !

!ModularSymbol methodsFor: 'as yet unclassified' stamp: 'len 12/21/2016 20:48:11'!
apply: aMorphism
	^ self class alpha: (aMorphism value: alpha) beta: (aMorphism value: beta)! !
UpperHalfPlane initialize!
