'From Cuis 6.0 [latest update: #5898] on 2 October 2023 at 7:47:37 pm'!
'Description '!
!provides: 'Completions' 1 1!
SystemOrganization addCategory: #Completions!
SystemOrganization addCategory: #'Completions-Tests'!


!classDefinition: #AdicIntegerRingTest category: #'Completions-Tests'!
AbstractDVRTest subclass: #AdicIntegerRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions-Tests'!
!classDefinition: 'AdicIntegerRingTest class' category: #'Completions-Tests'!
AdicIntegerRingTest class
	instanceVariableNames: ''!

!classDefinition: #RationalPowerSeriesRingTest category: #'Completions-Tests'!
AbstractDVRTest subclass: #RationalPowerSeriesRingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions-Tests'!
!classDefinition: 'RationalPowerSeriesRingTest class' category: #'Completions-Tests'!
RationalPowerSeriesRingTest class
	instanceVariableNames: ''!

!classDefinition: #AdicField category: #Completions!
FractionRing subclass: #AdicField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'AdicField class' category: #Completions!
AdicField class
	instanceVariableNames: ''!

!classDefinition: #AdicRationalField category: #Completions!
AdicField subclass: #AdicRationalField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'AdicRationalField class' category: #Completions!
AdicRationalField class
	instanceVariableNames: ''!

!classDefinition: #LaurentSeriesField category: #Completions!
AdicField subclass: #LaurentSeriesField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'LaurentSeriesField class' category: #Completions!
LaurentSeriesField class
	instanceVariableNames: ''!

!classDefinition: #AdicRing category: #Completions!
Ring subclass: #AdicRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'AdicRing class' category: #Completions!
AdicRing class
	instanceVariableNames: ''!

!classDefinition: #AdicIntegerRing category: #Completions!
AdicRing subclass: #AdicIntegerRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'AdicIntegerRing class' category: #Completions!
AdicIntegerRing class
	instanceVariableNames: ''!

!classDefinition: #PowerSeriesRing category: #Completions!
AdicRing subclass: #PowerSeriesRing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'PowerSeriesRing class' category: #Completions!
PowerSeriesRing class
	instanceVariableNames: ''!

!classDefinition: #ComplexField category: #Completions!
Ring subclass: #ComplexField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'ComplexField class' category: #Completions!
ComplexField class
	instanceVariableNames: ''!

!classDefinition: #RealField category: #Completions!
Ring subclass: #RealField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'RealField class' category: #Completions!
RealField class
	instanceVariableNames: ''!

!classDefinition: #Adic category: #Completions!
RingElement subclass: #Adic
	instanceVariableNames: 'parent representative precision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'Adic class' category: #Completions!
Adic class
	instanceVariableNames: ''!

!classDefinition: #AdicInteger category: #Completions!
Adic subclass: #AdicInteger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'AdicInteger class' category: #Completions!
AdicInteger class
	instanceVariableNames: ''!

!classDefinition: #PowerSeries category: #Completions!
Adic subclass: #PowerSeries
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'PowerSeries class' category: #Completions!
PowerSeries class
	instanceVariableNames: ''!

!classDefinition: #AdicFraction category: #Completions!
RingElement subclass: #AdicFraction
	instanceVariableNames: 'valuation unitPart'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Completions'!
!classDefinition: 'AdicFraction class' category: #Completions!
AdicFraction class
	instanceVariableNames: ''!


!AdicField commentStamp: '<historical>' prior: 0!
Field of fractions of a formal (or adic) completion ring. Elements are AdicFractions. See als AdicRing.!

!AdicRationalField commentStamp: '<historical>' prior: 0!
Field of p-adic numbers ℚₚ. This is the fraction field of the ring of p-adic integers ℤₚ. See also AdicIntegerRing.!

!LaurentSeriesField commentStamp: '<historical>' prior: 0!
Field K((x)) of formal Laurent series over a field K in one indetermiate x. See also PowerSeriesRing.!

!AdicRing commentStamp: '<historical>' prior: 0!
Formal (or adic) completion of a commutative ring R at a maximal ideal m. This is the inverse limit of the quotient rings R / mⁿ.!

!AdicIntegerRing commentStamp: '<historical>' prior: 0!
Ring of p-adic integers ℤₚ. Elements are AdicIntegers. See also AdicNumberField and PrimePowerRing.

For each prime number p, the ring of p-adic integers ℤₚ is the formal completion of the ring ℤ at the prime ideal ⟨p⟩. Geometrically this means that ℤₚ is the ring of functions on a formal neighbourhood of p inside Spec(ℤ). Algebaically it means that the elements in ℤₚ look like formal power series where the formal variable is the prime number p.!

!PowerSeriesRing commentStamp: '<historical>' prior: 0!
Ring R⟦x₁,...,xₙ⟧ of formal power series over a commutative ring R. See PowerSeries.

This is the adic completion of the polynomial ring R[x₁,..,xₙ] at the maximal ideal ⟨x₁,..,xₙ⟩.!

!ComplexField commentStamp: '<historical>' prior: 0!
The singletion global instance ℂ is the field of complex numbers. This is an approximate field, it's the the actual complex numbers but floating point approximations.!

!RealField commentStamp: '<historical>' prior: 0!
The singletion global instance ℝ is the field of real numbers. This is an approximate field, it's the the actual real numbers but floating point approximations.!

!Adic commentStamp: '<historical>' prior: 0!
Elements of the formal (or adic) completion of a commutative ring at a maximal ideal. See AdicRing.!

!AdicInteger commentStamp: '<historical>' prior: 0!
Elements of the p-adic completion of ℤ, i.e. p-adic integers ℤₚ. See AdicIntegerRing.!

!PowerSeries commentStamp: 'len 6/7/2020 17:53:07' prior: 0!
Formal power series with coefficients over a commutative ring. See PowerSeriesRing.!

!AdicFraction commentStamp: '<historical>' prior: 0!
Elements of the field of fractions of a formal (or adic) completion. See AdicField.!

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 17:55:37'!
setUp
	super setUp.
	R := ℤ adic: (10 atRandom: random) nextPrime! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:12:34'!
testAdditionExample1
	| ℤ₅ a b c |
	ℤ₅ := ℤ adic: 5.
	a := ℤ₅ !! 123.
	b := ℤ₅ !! -445.
	c := ℤ₅ !! (123 - 445).
	self assert: a+b = c.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20).
	c := a+b.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20)! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:12:13'!
testAdditionExample2
	| ℤ₇ a b c |
	ℤ₇ := ℤ adic: 7.
	a := ℤ₇ !! 676.
	b := ℤ₇ !! 123.
	c := ℤ₇ !! (676+123).
	self assert: a+b = c! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:13:01'!
testAdditionExample3
	| ℤ₇ a b c |
	ℤ₇ := ℤ adic: 7.
	a := ℤ₇ !! -676.
	b := ℤ₇ !! 123.
	c := ℤ₇ !! (-676+123).
	self assert: a+b = c! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:13:21'!
testAdditiveInverseExample
	| ℤ₇ a b |
	ℤ₇ := ℤ adic: 7.
	a := ℤ₇ !! -676.
	b := ℤ₇ !! 676.
	self assert: a = b negated.
	self assert: a negated negated = a! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:15:46'!
testMultiplicationBug
	| ℤ₅ zero |
	ℤ₅ := ℤ adic: 5.
	zero := ℤ₅ zero.
	self assert: zero * zero = zero! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:14:40'!
testMultiplicationExample1
	| ℤ₅ a b c |
	ℤ₅ := ℤ adic: 5.
	a := ℤ₅ !! 123.
	b := ℤ₅ !! -445.
	c := ℤ₅ !! (123 * -445).
	self assert: a * b = c.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 19)! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:15:02'!
testMultiplicationExample2
	| ℤ₇ a b c |
	ℤ₇ := ℤ adic: 7.
	a := ℤ₇ !! 676.
	b := ℤ₇ !! 123.
	c := ℤ₇ !! (676 * 123).
	self assert: a * b = c! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:15:25'!
testMultiplicationExample3
	| ℤ₅ a b c p |
	ℤ₅ := ℤ adic: 5.
	p := ℤ₅ uniformizer.
	a := ℤ₅ !! 123.
	b := p^8 + 1.
	c := ℤ₅ !! (123 * (5^8 + 1)).
	self assert: a * b = c.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20)! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:16:25'!
testNormAndValuationExamples
	| ℤ₇ x |
	ℤ₇ := ℤ adic: 7.
	x := ℤ₇ !! 13.
	self assert: x valuation = 0.
	self assert: x abs = 1.
	x := x * (7*7).
	self assert: x valuation = 2.
	self assert: x abs = (1/(7*7)).
	x := ℤ₇ !! 0.
	self assert: x valuation = Infinity positive.
	self assert: x abs = 0! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:16:50'!
testPrecisionExamples
	"Some tests to match Magma."
	| ℤ₅ p a |
	ℤ₅ := ℤ adic: 5.
	p := ℤ₅ uniformizer.
	self assert: p precision = 20.
	a := p*p negated.
	self assert: {a precision. a absolutePrecision. a relativePrecision} = #(21 21 19).
	a := ℤ₅ !! -25.
	self assert: {a precision. a absolutePrecision. a relativePrecision} = #(20 20 18)! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:17:23'!
testProjectionToPrimePowerRingAndLiftingExamples
	| ℤ₇ a x |
	ℤ₇ := ℤ adic: 7.
	a := (-1024 to: 1024) atRandom.
	x := ℤ₇ !! a.
	self assert: ((ℤ/(7^5)) !! x) lift = a.
	self assert: ((ℤ/(7^(5 to: 10) atRandom)) !! x) lift = a! !

!AdicIntegerRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:17:49'!
testSubstractionExample
	| ℤ₇ a b c |
	ℤ₇ := ℤ adic: 7.
	a := ℤ₇ !! 676.
	b := ℤ₇ !! 123.
	c := ℤ₇ !! (676 - 123).
	self assert: a - b = c! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:20:29'!
cos
	"The power series expansion of cos(x)."
	^ R coefficients: (ℕ to: ℚ map: [:n| n even ifTrue: [0] ifFalse: [|k| k := n-1/2. (-1)^k / (2*k) factorial]])! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:20:36'!
exp
	"The power series expansion of exp(x)."
	^ R coefficients: (ℕ to: ℚ map: [:n| 1 / n factorial])! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 18:02:32'!
setUp
	super setUp.
	R := ℚ powerSeries! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:20:57'!
sin
	"The power series expansion of sin(x)."
	^ R coefficients: (ℕ to: ℚ map: [:n| n odd ifTrue: [0] ifFalse: [|k| k := n/2-1. (-1)^k / (2*k+1) factorial]])! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:21:14'!
testAdditionExamples
	| a b c |
	a := self cos.
	b := self sin.
	c := a+b.
	self assert: c-a = b.
	self assert: c-b = a.
	self assert: (c + c negated) isZero.
	self assert: {c precision. c absolutePrecision. c relativePrecision} = #(20 20 20)! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:21:24'!
testAdditiveInverseExamples
	| a zero |
	a := self cos.
	self assert: a negated negated = a.
	self assert: (a + a negated) isZero.
	zero := a - a.
	self assert: zero isZero.
	self assert: zero negated = zero! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:22:09'!
testMultiplicativeInverseExamples
	| a one |
	a := self cos.
	one := a parent one.
	self assert: a * a inverse = one.
	self assert: one inverse = one! !

!RationalPowerSeriesRingTest methodsFor: 'as yet unclassified' stamp: 'len 5/31/2023 23:22:32'!
testSinSquaredPlusCosSquaredExample
	| cos sin |
	sin := self sin.
	cos := self cos.
	self assert: cos squared + sin squared = cos parent one! !

!AdicField methodsFor: 'accessing' stamp: 'len 5/30/2023 20:12:27'!
integers
	^ self numerators! !

!AdicField methodsFor: 'morphisms' stamp: 'len 6/2/2023 14:59:05'!
embed: anElement
	self assert: (self numerators includes: anElement).
	^ AdicFraction valuation: anElement valuation unit: anElement unitPart! !

!AdicField methodsFor: 'testing' stamp: 'len 6/1/2023 15:45:30'!
includes: anElement
	^ (anElement isKindOf: AdicFraction) and: [self numerators includes: anElement unitPart]! !

!AdicField methodsFor: 'testing' stamp: 'len 5/9/2023 14:52:07'!
isApproximate
	^ true! !

!AdicField methodsFor: 'testing' stamp: 'len 5/9/2023 14:52:32'!
isField
	^ true! !

!AdicRationalField methodsFor: 'accessing' stamp: 'len 9/8/2023 17:14:31'!
base
	^ self numerators base! !

!AdicRationalField methodsFor: 'accessing' stamp: 'len 5/30/2023 20:11:13'!
power: anInteger
	^ self numerators power: anInteger! !

!AdicRationalField methodsFor: 'accessing' stamp: 'len 6/2/2019 05:24:21'!
primeSubfield
	^ self! !

!AdicRationalField methodsFor: 'printing' stamp: 'len 9/8/2023 17:14:23'!
printOn: aStream
	aStream print: ℚ; nextPutAll: self base printString sub! !

!AdicRationalField methodsFor: 'testing' stamp: 'len 6/4/2019 04:52:35'!
isLocalField
	^ true! !

!LaurentSeriesField methodsFor: 'printing' stamp: 'len 5/30/2023 20:09:20'!
printOn: aStream
	aStream print: self scalars; nextPutAll: '(('.
	self numerators printIndeterminatesOn: aStream.
	aStream nextPutAll: '))'! !

!LaurentSeriesField methodsFor: 'testing' stamp: 'len 7/15/2021 16:05:06'!
isLocalField
	^ self scalars isFiniteField! !

!AdicRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:35:05'!
baseIdeal
	^ self subclassResponsibility! !

!AdicRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:36:10'!
cover
	^ self baseIdeal scalars! !

!AdicRing methodsFor: 'accessing' stamp: 'len 5/9/2023 15:21:49'!
defaultPrecision
	"Answer the default precision used when coercing elements from other rings into the receiver."
	^ self propertyAt: #defaultPrecision ifAbsent: [20]! !

!AdicRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:18:36'!
dimension
	"Answer the Krull dimension of the receiver."
	(self cover isNoetherian and: [self cover isLocal]) ifTrue: [^ self cover dimension].
	^ super dimension! !

!AdicRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:36:23'!
maximalIdeal
	"Answer the unique maximal ideal in the receiver."
	^ self * (self baseIdeal generators collect: [:each| self embed: each])! !

!AdicRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:13:55'!
one
	^ self embed: self cover one! !

!AdicRing methodsFor: 'accessing' stamp: 'len 5/9/2023 15:22:16'!
precision
	"Answer the precision bound for the receiver."
	^ self propertyAt: #precision ifAbsent: [Infinity positive]! !

!AdicRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:14:00'!
zero
	^ self embed: self cover zero! !

!AdicRing methodsFor: 'comparing' stamp: 'len 9/8/2023 17:36:05'!
= anObject
	self == anObject ifTrue: [^ true].
	^ (anObject isKindOf: AdicRing) and: [anObject baseIdeal = self baseIdeal]! !

!AdicRing methodsFor: 'comparing' stamp: 'len 9/6/2023 17:18:08'!
> aRing
	self cover > aRing ifTrue: [^ true].
	^ super > aRing! !

!AdicRing methodsFor: 'comparing' stamp: 'len 9/6/2023 17:18:12'!
>= aRing
	super >= aRing ifTrue: [^ true].
	self cover >= aRing ifTrue: [^ true].
	^ false! !

!AdicRing methodsFor: 'comparing' stamp: 'len 9/8/2023 17:36:14'!
hash
	^ self baseIdeal hash + self cover hash! !

!AdicRing methodsFor: 'elements' stamp: 'len 9/6/2023 17:18:17'!
O: anInteger
	anInteger > 0 ifFalse: [self error: 'precision must be positive'].
	^ self representative: self cover zero precision: anInteger! !

!AdicRing methodsFor: 'elements' stamp: 'len 5/9/2023 15:39:09'!
project: anElement precision: anInteger
	^ self representative: ((self jet: anInteger) project: anElement) representative precision: anInteger! !

!AdicRing methodsFor: 'elements' stamp: 'len 5/9/2023 15:35:20'!
representative: aPolynomial precision: anInteger
	anInteger < 0 ifTrue: [^ self error: 'negative precision is not allowed'].
	^ self elementClass representative: aPolynomial precision: anInteger parent: self! !

!AdicRing methodsFor: 'morphisms' stamp: 'len 5/9/2023 15:41:15'!
embed: anInteger
	^ self project: anInteger precision: self defaultPrecision! !

!AdicRing methodsFor: 'morphisms' stamp: 'len 9/6/2023 17:18:46'!
from: aDomain
	aDomain = self cover ifTrue: [^ self addMap: ((self cover into: self map: [:x| self embed: x]) name: 'ι')].
	^ super from: aDomain! !

!AdicRing methodsFor: 'operations' stamp: 'len 9/6/2023 17:19:06'!
hilbertSeriesAt: t
	"Answer the Hilbert series of the receiver."
	(self cover isNoetherian and: [self cover isLocal]) ifTrue: [^ self cover hilbertSeriesAt: t].
	^ super hilbertSeriesAt: t! !

!AdicRing methodsFor: 'operations' stamp: 'len 9/8/2023 17:36:18'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the quotient ring isomorphic to the anInteger-th truncation of the receiver."
	anInteger > 0 ifFalse: [^ self error: 'jet degree must be >= 1'].
	^ self cover / (self baseIdeal ^ anInteger)! !

!AdicRing methodsFor: 'testing' stamp: 'len 5/9/2023 15:25:48'!
includes: anObject
	^ anObject parent = self! !

!AdicRing methodsFor: 'testing' stamp: 'len 5/9/2023 15:16:25'!
isApproximate
	^ true! !

!AdicRing methodsFor: 'testing' stamp: 'len 5/9/2023 15:16:50'!
isCommutative
	^ true! !

!AdicRing methodsFor: 'testing' stamp: 'len 6/22/2019 03:11:07'!
isCompletion
	^ true! !

!AdicRing methodsFor: 'private' stamp: 'len 5/9/2023 15:17:55'!
defaultPrecision: anInteger
	self precision isFinite ifTrue: [self error: 'precision is already bounded'].
	self propertyAt: #defaultPrecision put: anInteger! !

!AdicRing methodsFor: 'private' stamp: 'len 5/9/2023 15:34:44'!
elementClass
	^ Adic! !

!AdicRing methodsFor: 'private' stamp: 'len 5/9/2023 15:18:10'!
precision: anInteger
	self defaultPrecision: anInteger.
	self propertyAt: #precision put: anInteger! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:14:51'!
base
	^ self propertyAt: #base! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:35:17'!
baseIdeal
	^ ℤ * self base! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 12/17/2016 20:31:49'!
characteristic
	^ 0! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:13:07'!
cover
	^ ℤ! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 8/11/2019 11:41:41'!
maximalIdeal
	"Answer the unique maximal ideal in the receiver."
	^ self * self uniformizer! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 8/11/2019 11:18:48'!
one
	^ self embed: 1! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 6/2/2019 05:24:00'!
primeSubring
	^ self! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:16:10'!
residueField
	"Answer the prime field ℤ/pℤ isomorphic to the residue field of the receiver, equipped with the canonical projection."
	^ self propertyAt: #residueField ifAbsentPut:
		[| answer |
		(answer := PrimeField new: self base)
			addMap: (self onto: answer map: [:x| answer !! x representative]); "TODO: add kernel and section"
			yourself]! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:16:15'!
uniformizer
	"A uniformizer (or uniformizing parameter, uniformizer element or prime element) is any irreducible element of a DVR, or equivalently a generator of the maximal ideal. All nonzero ideals are generated by some power of the uniformizer."
	^ self !! self base! !

!AdicIntegerRing methodsFor: 'accessing' stamp: 'len 8/11/2019 11:18:51'!
zero
	^ self embed: 0! !

!AdicIntegerRing methodsFor: 'comparing' stamp: 'len 9/8/2023 17:10:56'!
= anObject
	^ self == anObject or: [self class = anObject class and: [self base = anObject base and: [self precision = anObject precision]]]! !

!AdicIntegerRing methodsFor: 'comparing' stamp: 'len 9/8/2023 17:11:17'!
hash
	^ self base hash! !

!AdicIntegerRing methodsFor: 'elements' stamp: 'len 4/9/2020 18:02:57'!
adapt: anElement
	anElement isInteger ifTrue: [^ self embed: anElement].
	anElement isFraction ifTrue: [^ (self embed: anElement numerator) / (self embed: anElement denominator)]. "fix: only germs with valuation >= 0"
	^ super adapt: anElement! !

!AdicIntegerRing methodsFor: 'elements' stamp: 'len 8/11/2019 09:55:11'!
project: anInteger precision: N
	^ self representative: anInteger \\ (self power: N) precision: N! !

!AdicIntegerRing methodsFor: 'operations' stamp: 'len 9/6/2023 17:25:02'!
fractions
	^ self propertyAt: #fractions ifAbsentPut: [AdicRationalField on: self]! !

!AdicIntegerRing methodsFor: 'operations' stamp: 'len 9/8/2023 17:11:23'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the quotient ring isomorphic to the anInteger-th truncation of the receiver."
	^ PrimePowerRing new: self base to: anInteger! !

!AdicIntegerRing methodsFor: 'printing' stamp: 'len 9/8/2023 17:16:03'!
printOn: aStream
	aStream print: ℤ; nextPutAll: self base printString sub! !

!AdicIntegerRing methodsFor: 'random' stamp: 'len 9/8/2023 17:11:05'!
atRandom: aRandom
	^ self atRandom: aRandom bits: (self base log: 2) * self defaultPrecision! !

!AdicIntegerRing methodsFor: 'random' stamp: 'len 5/23/2020 08:55:40'!
atRandom: aRandom bits: bitSize
	| n |
	n _ ℤ atRandom: aRandom bits: bitSize.
	n = 0 ifTrue: [n _ 1].
	^ self !! n! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 6/3/2022 05:59:17'!
hasCanonicalAssociates
	^ true! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 6/2/2019 05:03:27'!
isBounded
	"Answer true if the precision of the receiver is bounded."
	^ self precision isFinite! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 6/4/2019 17:48:35'!
isDVR
	"Answer true if the receiver is a discrete valuation ring, i.e. a local PID that is not a field."
	^ true! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 12/31/2021 11:52:00'!
isEuclidean
	"Answer true if the receiver is a Euclidean ring, i.e. it has Euclidean division with remainder."
	^ true! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 12/7/2015 01:29'!
isIntegralDomain
	^ true! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 8/11/2019 11:40:53'!
isLocal
	^ true! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 8/10/2019 04:52:48'!
isPID
	^ true! !

!AdicIntegerRing methodsFor: 'testing' stamp: 'len 6/2/2019 05:03:44'!
isUnbounded
	"Answer true if the precision of the receiver is unbounded."
	^ self precision isInfinite! !

!AdicIntegerRing methodsFor: 'private' stamp: 'len 9/8/2023 17:15:31'!
base: aPrime
	self propertyAt: #base put: aPrime! !

!AdicIntegerRing methodsFor: 'private' stamp: 'len 5/9/2023 15:34:16'!
elementClass
	^ AdicInteger! !

!AdicIntegerRing methodsFor: 'private' stamp: 'len 9/8/2023 17:15:55'!
power: anInteger
	"Answer a power of the representative of the uniformizer, i.e. the integer p^n."
	^ self base ^ anInteger! !

!AdicIntegerRing class methodsFor: 'instance creation' stamp: 'len 9/8/2023 17:15:22'!
at: aPrime
	^ self new base: aPrime! !

!AdicIntegerRing class methodsFor: 'instance creation' stamp: 'len 6/6/2019 12:03:02'!
at: aPrime precision: anInteger
	^ (self at: aPrime) precision: anInteger! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 9/8/2023 17:35:34'!
baseIdeal
	^ self cover * self cover generators! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 10/28/2016 14:17'!
characteristic
	^ self scalars characteristic! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:13:23'!
cover
	^ self propertyAt: #cover! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:15:28'!
dimension
	"Answer the Krull dimension of the receiver. This is the length of the longest ascending chain of prime ideals."
	self scalars isNoetherian ifTrue: [^ self scalars dimension + self cover rank].
	^ super dimension! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:16:10'!
maximalIdeal
	self scalars isField ifTrue: [^ self * self cover generators].
	^ self * self cover generators + self scalars maximalIdeal! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 8/9/2019 11:40:05'!
residueField
	"Answer the coefficients field, isomorphic to the residue field of the receiver, equipped with the canonical projection."
	^ self scalars residueField! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:16:37'!
scalars
	^ self cover scalars! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new: 1! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 1/2/2022 10:08:34'!
uniformizer
	"A uniformizer (or uniformizing parameter, uniformizer element or prime element) is any irreducible element of a DVR, or equivalently a generator of the maximal ideal. All nonzero ideals are generated by some power of the uniformizer."
	self isDVR ifFalse: [DomainError signal: 'not a DVR'].
	^ self x! !

!PowerSeriesRing methodsFor: 'accessing' stamp: 'len 9/6/2023 17:16:47'!
zero
	^ self embed: self cover zero! !

!PowerSeriesRing methodsFor: 'comparing' stamp: 'len 9/6/2023 17:14:12'!
= anObject
	^ self == anObject or: [self class = anObject class and: [self cover = anObject cover and: [self precision = anObject precision]]]! !

!PowerSeriesRing methodsFor: 'comparing' stamp: 'len 9/6/2023 17:15:38'!
hash
	^ self cover hash! !

!PowerSeriesRing methodsFor: 'elements' stamp: 'len 9/6/2023 17:14:22'!
adapt: anElement
	(self cover includes: anElement) ifTrue: [^ self embed: anElement].
	(self scalars includes: anElement) ifTrue: [^ self  embed: (self cover constant: anElement)].
	^ super adapt: anElement! !

!PowerSeriesRing methodsFor: 'elements' stamp: 'len 9/6/2023 17:15:08'!
coefficients: aSequence
	self isUnivariate ifFalse: [^ self error: 'not univariate'].
	^ self embed: (self cover coefficients: (aSequence first: self defaultPrecision))! !

!PowerSeriesRing methodsFor: 'elements' stamp: 'len 9/6/2023 17:15:22'!
constant: anElement
	^ self embed: (self cover constant: anElement)! !

!PowerSeriesRing methodsFor: 'elements' stamp: 'len 5/9/2023 15:37:28'!
project: aPolynomial precision: anInteger
	^ self representative: (aPolynomial truncatedTo: anInteger) precision: anInteger! !

!PowerSeriesRing methodsFor: 'elements' stamp: 'len 3/21/2021 13:33:25'!
x
	^ self x: 1! !

!PowerSeriesRing methodsFor: 'elements' stamp: 'len 9/6/2023 17:16:42'!
x: anInteger
	^ self embed: (self cover x: anInteger)! !

!PowerSeriesRing methodsFor: 'morphisms' stamp: 'len 6/4/2023 02:18:58'!
to: aCodomain
	aCodomain = self residueField ifTrue: [^ self addMap: ((self onto: aCodomain map: [:f| aCodomain !! f independentCoefficient]) name: 'π')].
	^ super to: aCodomain! !

!PowerSeriesRing methodsFor: 'operations' stamp: 'len 5/9/2023 14:51:08'!
fractions
	^ self propertyAt: #fractions ifAbsentPut:
		[self scalars isField
			ifTrue: [LaurentSeriesField on: self]
			ifFalse: [LaurentSeriesField on: (self over: self scalars fractions)]]! !

!PowerSeriesRing methodsFor: 'operations' stamp: 'len 9/8/2023 17:36:32'!
jet: anInteger
	"Answer the anInteger-th jet of the receiver, that is the quotient ring isomorphic to the anInteger-th truncation of the receiver."
	anInteger > 0 ifFalse: [^ self error: 'jet degree must be >= 1'].
	^ JetRing new: anInteger baseIdeal: self baseIdeal! !

!PowerSeriesRing methodsFor: 'operations' stamp: 'len 9/6/2023 17:16:20'!
over: aRing
	^ self class on: (self cover over: aRing)! !

!PowerSeriesRing methodsFor: 'printing' stamp: 'len 9/6/2023 17:19:32'!
printIndeterminatesOn: aStream
	self cover printIndeterminatesOn: aStream! !

!PowerSeriesRing methodsFor: 'printing' stamp: 'len 9/9/2022 20:16:34'!
printOn: aStream
	aStream print: self scalars; nextPut: $⟦.
	self printIndeterminatesOn: aStream.
	aStream nextPut: $⟧! !

!PowerSeriesRing methodsFor: 'random' stamp: 'len 9/6/2023 17:14:28'!
atRandom: aRandom bits: bitSize
	^ self embed: (self cover atRandom: aRandom bits: bitSize maxDegree: self defaultPrecision - 1)! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 9/6/2023 17:15:33'!
hasCanonicalAssociates
	^ self cover hasCanonicalAssociates! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 8/10/2019 04:05:53'!
isDVR
	"Answer true if the receiver is a discrete valuation ring, i.e. a local PID that is not a field."
	^ self scalars isField and: [self isUnivariate]! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 5/31/2023 22:58:22'!
isEuclidean
	"Answer true if the receiver is a Euclidean ring, i.e. it has Euclidean division with remainder."
	^ self isDVR! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 9/6/2023 17:15:44'!
isIntegralDomain
	^ self cover isIntegralDomain! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 8/9/2019 11:49:32'!
isLocal
	^ self scalars isLocal! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 9/6/2023 17:15:49'!
isMultivariate
	^ self cover isMultivariate! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 5/8/2023 22:52:11'!
isNoetherian
	"A ring is called Noetherian if every ideal is finitely generated."
	^ self scalars isNoetherian! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 9/6/2023 17:15:53'!
isPID
	^ self cover isPID! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 9/6/2023 17:15:58'!
isTrivial
	^ self cover isTrivial! !

!PowerSeriesRing methodsFor: 'testing' stamp: 'len 9/6/2023 17:16:02'!
isUnivariate
	^ self cover isUnivariate! !

!PowerSeriesRing methodsFor: 'private' stamp: 'len 9/6/2023 17:14:40'!
cover: aPolynomialRing
	self propertyAt: #cover put: aPolynomialRing! !

!PowerSeriesRing methodsFor: 'private' stamp: 'len 5/9/2023 15:34:32'!
elementClass
	^ PowerSeries! !

!PowerSeriesRing methodsFor: 'private' stamp: 'len 9/6/2023 17:16:27'!
power: anInteger
	"Answer a power of the representative of the uniformizer, i.e. the polynomial x^n."
	self isDVR ifFalse: [DomainError signal: 'not a DVR'].
	^ self cover x: 1 to: anInteger! !

!PowerSeriesRing class methodsFor: 'instance creation' stamp: 'len 9/6/2023 17:14:51'!
on: aPolynomialRing
	^ self new cover: aPolynomialRing! !

!ComplexField methodsFor: 'accessing' stamp: 'len 8/6/2016 03:44'!
characteristic
	^ 0! !

!ComplexField methodsFor: 'accessing' stamp: 'len 8/6/2016 03:45'!
one
	^ 1! !

!ComplexField methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new: 1! !

!ComplexField methodsFor: 'accessing' stamp: 'len 8/6/2016 03:45'!
zero
	^ 0! !

!ComplexField methodsFor: 'comparing' stamp: 'len 1/20/2018 19:06:56'!
= anObject
	^ anObject class == self class! !

!ComplexField methodsFor: 'comparing' stamp: 'len 1/20/2018 19:07:25'!
hash
	^ self class hash! !

!ComplexField methodsFor: 'computing-polynomials' stamp: 'len 6/25/2023 11:27:57'!
rootsOf: aPolynomial tolerance: tolerance do: aBlock
	"Given a squarefree polynomial with real or complex coefficients, this algorithm finds
	approximations with absolute error < tolerance for all the complex roots of the polynomial.
	This is Newton's modified algorithm with quadratic convergence, based on [Coh96, Algorithm 3.6.6]."
	| X P Q P2 Q2 n x v m c dx x1 v1 m1 realCoefficients |
	X := aPolynomial parent x.
	P := aPolynomial.
	Q := P.
	P2 := P derivative.
	Q2 := P2.
	n := P degree.
	realCoefficients := P coefficients allSatisfy: [:each| (each isKindOf: Number) or: [each imaginary isZero]].
	["Initialize root finding:"
	x := 1.3 + 0.314159 i. "some initial value"
	v := Q value: x.
	m := v norm2.
	"Initialize recursion:"
	c := 0. dx := v / (Q2 value: x).
	[dx abs < tolerance]
		whileFalse:
			[x1 := x - dx.
			v1 := Q value: x1.
			m1 := v1 norm2.
			m1 < m
				ifTrue: [x := x1. v := v1. m := m1. c := 0. dx := v / (Q2 value: x)]
				ifFalse: [c := c + 1. dx := dx / 4. c > 20 ifTrue: [self error: 'failed']]].
	"Polish root:"
	2 timesRepeat: [x := x - ((P value: x) / (P2 value: x))].
	"Divide:"
	((realCoefficients not or: [n = 1]) or: [x imaginary abs < tolerance "###"])
		ifTrue: "found one root"
			[x imaginary abs < tolerance ifTrue: [x := x real]. "###"
			aBlock value: x.
			Q := Q // (X - x).
			Q2 := Q derivative. "###"
			n := n - 1]
		ifFalse: "found a root and (if the root is not real) also found its conjugate"
			[aBlock value: x; value: x conjugate.
			Q := Q // (X^2 - (X * (x real * 2)) + x abs squared).
			Q2 := Q derivative. "###"
			n := n - 2].
	n > 0] whileTrue! !

!ComplexField methodsFor: 'computing-polynomials' stamp: 'len 6/25/2023 11:30:06'!
rootsOf: aPolynomial withMultiplicitiesDo: aBlock
	aPolynomial integerPrimitivePart factorization withOccurrencesDo: [:f :e|
		f degree = 1
			ifTrue:
				[aBlock value: (f independentCoefficient negated / f leadingCoefficient) value: e]
			ifFalse:
				[self rootsOf: (f over: self) tolerance: Float epsilon do: [:each| aBlock value: each value: e]]]! !

!ComplexField methodsFor: 'elements' stamp: 'len 5/28/2023 20:55:04'!
newFrom: anObject
	anObject isNumber ifTrue: [^ anObject + 0 i].
	^ super newFrom: anObject! !

!ComplexField methodsFor: 'enumerating' stamp: 'len 8/6/2016 03:50'!
do: aBlock
	^ self shouldNotImplement! !

!ComplexField methodsFor: 'morphisms' stamp: 'len 5/28/2023 20:53:37'!
from: aDomain
	(aDomain = ℚ or: [aDomain = ℝ]) ifTrue: [^ aDomain to: self map: [:x| x + 0 i]].
	^ super from: aDomain! !

!ComplexField methodsFor: 'printing' stamp: 'len 5/23/2020 07:34:10'!
printOn: aStream
	aStream nextPut: $ℂ! !

!ComplexField methodsFor: 'testing' stamp: 'len 8/5/2016 21:50'!
includes: anObject
	^ anObject isComplex! !

!ComplexField methodsFor: 'testing' stamp: 'len 8/12/2016 04:48'!
isAlgebraicallyClosed
	^ true! !

!ComplexField methodsFor: 'testing' stamp: 'len 8/7/2019 07:45:58'!
isApproximate
	^ true! !

!ComplexField methodsFor: 'testing' stamp: 'len 4/28/2021 09:37:09'!
isCommutative
	^ true! !

!ComplexField methodsFor: 'testing' stamp: 'len 8/5/2016 21:55'!
isComplex
	^ true! !

!ComplexField methodsFor: 'testing' stamp: 'len 10/18/2016 20:43'!
isField
	^ true! !

!ComplexField methodsFor: 'testing' stamp: 'len 7/21/2021 15:43:40'!
isInvolutive
	^ true! !

!ComplexField methodsFor: 'testing' stamp: 'len 1/26/2018 18:57:44'!
isLocalField
	^ true! !

!RealField methodsFor: 'accessing' stamp: 'len 8/6/2016 03:45'!
characteristic
	^ 0! !

!RealField methodsFor: 'accessing' stamp: 'len 1/19/2017 16:29:14'!
one
	^ 1! !

!RealField methodsFor: 'accessing' stamp: 'len 12/29/2021 11:55:57'!
size
	^ Cardinal new: 1! !

!RealField methodsFor: 'accessing' stamp: 'len 1/19/2017 16:29:18'!
zero
	^ 0! !

!RealField methodsFor: 'comparing' stamp: 'len 7/4/2022 13:30:15'!
= anObject
	^ self class = anObject class! !

!RealField methodsFor: 'comparing' stamp: 'len 1/20/2018 19:00:05'!
hash
	^ self class hash! !

!RealField methodsFor: 'computing-polynomials' stamp: 'len 6/25/2023 11:31:56'!
rootsOf: aPolynomial withMultiplicitiesDo: aBlock
	aPolynomial integerPrimitivePart factorization withOccurrencesDo: [:f :e|
		f degree = 1
			ifTrue:
				[aBlock value: (f independentCoefficient negated / f leadingCoefficient) value: e]
			ifFalse:
				[ℂ rootsOf: (f over: ℂ) tolerance: Float epsilon do: [:each| each isReal ifTrue: [aBlock value: each value: e]]]]! !

!RealField methodsFor: 'enumerating' stamp: 'len 8/6/2016 03:50'!
do: aBlock
	^ self shouldNotImplement! !

!RealField methodsFor: 'morphisms' stamp: 'len 3/25/2018 19:32:22'!
embed: anObject
	(anObject isInteger or: [anObject isFraction]) ifTrue: [^ anObject].
	^ super embed: anObject! !

!RealField methodsFor: 'morphisms' stamp: 'len 5/28/2023 20:52:25'!
from: aDomain
	aDomain = ℚ ifTrue: [^ ℚ to: self map: [:x| x]].
	^ super from: aDomain! !

!RealField methodsFor: 'operations' stamp: 'len 5/23/2020 09:04:02'!
algebraicClosure
	^ ℂ! !

!RealField methodsFor: 'printing' stamp: 'len 5/23/2020 07:41:48'!
printOn: aStream
	aStream nextPut: $ℝ! !

!RealField methodsFor: 'testing' stamp: 'len 8/5/2016 21:50'!
includes: anObject
	^ anObject isReal! !

!RealField methodsFor: 'testing' stamp: 'len 8/7/2019 07:45:43'!
isApproximate
	^ true! !

!RealField methodsFor: 'testing' stamp: 'len 4/28/2021 09:39:21'!
isCommutative
	^ true! !

!RealField methodsFor: 'testing' stamp: 'len 10/18/2016 20:44'!
isField
	^ true! !

!RealField methodsFor: 'testing' stamp: 'len 1/20/2018 19:01:50'!
isFinite
	^ false! !

!RealField methodsFor: 'testing' stamp: 'len 1/26/2018 18:57:34'!
isLocalField
	^ true! !

!RealField methodsFor: 'testing' stamp: 'len 8/9/2020 08:00:00'!
isReal
	"Answer true if the receiver is a subring of the real field."
	^ true! !

!Adic methodsFor: 'accessing' stamp: 'len 5/9/2023 15:03:32'!
absolutePrecision
	^ precision! !

!Adic methodsFor: 'accessing' stamp: 'len 5/9/2023 15:04:26'!
parent
	^ parent! !

!Adic methodsFor: 'accessing' stamp: 'len 5/9/2023 15:04:18'!
precision
	^ precision! !

!Adic methodsFor: 'accessing' stamp: 'len 5/9/2023 15:03:58'!
relativePrecision
	representative isZero ifTrue: [^ 0].
	^ self absolutePrecision - self valuation! !

!Adic methodsFor: 'accessing' stamp: 'len 5/9/2023 15:04:08'!
representative
	^ representative! !

!Adic methodsFor: 'accessing' stamp: 'len 6/2/2023 14:57:39'!
unitPart
	| v |
	self isZero ifTrue: [^ parent one].
	(v := self valuation) = 0 ifTrue: [^ self].
	^ parent representative: representative // (parent power: v) precision: precision - v! !

!Adic methodsFor: 'comparing' stamp: 'len 6/1/2023 12:55:27'!
= anElement
	"Warning: this 'equality' is not transitive."
	^ self class = anElement class and: [self isWeaklyEqualTo: anElement]! !

!Adic methodsFor: 'comparing' stamp: 'len 6/1/2023 12:44:42'!
hash
	^ 0! !

!Adic methodsFor: 'operations' stamp: 'len 6/1/2023 11:07:20'!
* anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #*].
	self isZero ifTrue: [^ self]. "should take into account precision?"
	anElement isZero ifTrue: [^ anElement].
	^ parent project: representative * anElement representative precision: (precision + anElement valuation min: anElement precision + self valuation)! !

!Adic methodsFor: 'operations' stamp: 'len 6/1/2023 11:05:18'!
+ anElement
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	^ parent project: representative + anElement representative precision: (precision min: anElement precision)! !

!Adic methodsFor: 'operations' stamp: 'len 6/2/2023 14:33:42'!
// anElement
	"Answer the quotient of the Euclidean division of the receiver by the argument."
	| v |
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #//].
	anElement isZero ifTrue: [^ DivisibilityError new signalReceiver: self selector: #// argument: anElement].
	self isZero ifTrue: [^ self].
	(v := self valuation - anElement valuation) < 0 ifTrue: [^ self zero].
	^ self normalization inverse * anElement normalization * (parent power: v)! !

!Adic methodsFor: 'operations' stamp: 'len 6/1/2023 12:49:35'!
<< anInteger
	^ self >> anInteger negated! !

!Adic methodsFor: 'operations' stamp: 'len 6/2/2023 14:32:40'!
>> anInteger
	anInteger >= 0 ifTrue: [^ parent representative: representative * (parent power: anInteger) precision: precision + anInteger].
	^ parent representative: representative // (parent power: anInteger negated) precision: precision! !

!Adic methodsFor: 'operations' stamp: 'len 5/9/2023 15:08:12'!
O: anInteger
	"Answer a new p-adic element with precision decreased to anInteger."
	precision <= anInteger ifTrue: [^ self].
	anInteger > 0 ifFalse: [self error: 'precision must be positive'].
	^ parent project: representative precision: anInteger! !

!Adic methodsFor: 'operations' stamp: 'len 6/25/2023 10:01:50'!
factorizationDo: aBlock
	"This is correct for DVRs, for example ℤₚ, because they are UFDs with unique irreducible element up to multiplication by units."
	self isZero ifTrue: [^ self error: 'factorization of zero'].
	aBlock value: parent uniformizer value: self valuation! !

!Adic methodsFor: 'operations' stamp: 'len 6/1/2023 21:21:01'!
gcd: anElement
	| v N |
	self isZero ifTrue: [^ anElement normalized].
	anElement isZero ifTrue: [^ self normalized].
	v := self valuation min: anElement valuation.
	N := precision min: anElement precision.
	v >= N ifTrue: [parent zero O: N].
	^ parent representative: (parent power: v) precision: N! !

!Adic methodsFor: 'operations' stamp: 'len 5/9/2023 15:08:48'!
negated
	^ parent project: representative negated precision: precision! !

!Adic methodsFor: 'operations' stamp: 'len 6/2/2023 14:59:17'!
normalization
	"Answer a unit u such that 'self * u' is a unique choice of associate. See >>normalized."
	^ self unitPart inverse! !

!Adic methodsFor: 'testing' stamp: 'len 6/2/2023 14:52:39'!
isSquarefree
	self valuation >= 2 ifTrue: [^ false].
"	self isWeaklyZero ifTrue: [^ self error: 'not enough precision']."
	^ true! !

!Adic methodsFor: 'testing' stamp: 'len 6/2/2023 14:51:56'!
isUnit
	"Answer true if the receiver is invertible."
	^ self valuation = 0! !

!Adic methodsFor: 'testing' stamp: 'len 5/9/2023 15:09:40'!
isZero
	^ representative isZero! !

!Adic methodsFor: 'private' stamp: 'len 5/9/2023 15:12:21'!
representative: anInteger precision: N parent: anAdicRing
	representative _ anInteger.
	precision _ N.
	parent _ anAdicRing! !

!Adic class methodsFor: 'instance creation' stamp: 'len 5/9/2023 15:12:42'!
representative: anInteger precision: N parent: anAdicRing
	^ self new representative: anInteger precision: N parent: anAdicRing! !

!AdicInteger methodsFor: 'accessing' stamp: 'len 8/12/2019 11:25:18'!
centeredRepresentative
	| m |
	m _ parent power: precision.
	^ representative <= (m >> 1) ifTrue: [representative] ifFalse: [representative - m]! !

!AdicInteger methodsFor: 'accessing' stamp: 'len 9/8/2023 17:16:56'!
coefficients
	| p q |
	p := parent base.
	q := representative.
	^ Sequence streaming: [| r | r := q \\ p. q := q // p. r]! !

!AdicInteger methodsFor: 'accessing' stamp: 'len 9/8/2023 17:17:50'!
unitPart
	| p q v |
	self isZero ifTrue: [^ parent one].
	p := parent base.
	q := representative.
	v := 0.
	[p | q] whileTrue: [q := q // p. v := v + 1].
	^ parent representative: q precision: precision - v! !

!AdicInteger methodsFor: 'accessing' stamp: 'len 9/8/2023 17:17:55'!
valuation
	^ representative valuation: parent base! !

!AdicInteger methodsFor: 'comparing' stamp: 'len 6/2/2023 01:14:59'!
isWeaklyEqualTo: anElement
	"Answer true if the receiver is 'weakly equal' to the argument.
	Note that this relation is not transitive."
	anElement precision < precision ifTrue: [^ anElement isWeaklyEqualTo: self].
	precision = anElement precision ifTrue: [^ representative = anElement representative].
	^ representative = (anElement representative \\ (parent power: precision))! !

!AdicInteger methodsFor: 'operations' stamp: 'len 9/8/2023 17:16:50'!
abs
	"Answer the p-adic absolute value (or norm) of the receiver."
	^ self isZero ifTrue: [0] ifFalse: [parent base ^ self valuation negated]! !

!AdicInteger methodsFor: 'operations' stamp: 'len 5/29/2022 19:39:19'!
inverse
	representative = 1 ifTrue: [^ self].
	^ parent
		representative:
			((representative reciprocalModulo: (parent power: precision))
				ifNil: [^ DivisibilityError new signalReceiver: self one selector: #/ argument: self])
		precision: precision! !

!AdicInteger methodsFor: 'printing' stamp: 'len 9/8/2023 17:17:32'!
printExpansionOn: aStream shifted: v
	| coefficients first |
	self isZero ifTrue: [aStream nextPut: $0. ^ self].
	precision isFinite ifFalse:
		[aStream print: representative.
		v ~= 0 ifTrue: [aStream nextPut: $·; print: parent base. v ~= 1 ifTrue: [aStream nextPutAll: v printString super]].
		^ self].
	first := true.
	coefficients := self coefficients.
	v to: v + precision + 1 do: [:i|
		| x |
		(x := coefficients at: i-v+1) isZero
			ifFalse:
				[first ifFalse: [aStream nextPutAll: ' + '].
				(x ~= 1 or: [i = 0]) ifTrue: [aStream print: x. i ~= 0 ifTrue: [aStream nextPut: $·]].
				i ~= 0 ifTrue: [aStream print: parent base. i ~= 1 ifTrue: [aStream nextPutAll: i printString super]].
				first := false]].
	aStream nextPutAll: ' + ', 'O' italic; nextPut: $(; print: parent base; nextPutAll: (precision + v) printString super; nextPut: $)! !

!AdicInteger methodsFor: 'printing' stamp: 'len 8/11/2019 08:45:01'!
printOn: aStream
	self printOn: aStream shifted: 0! !

!AdicInteger methodsFor: 'printing' stamp: 'len 9/8/2023 17:17:45'!
printOn: aStream shifted: anInteger
	| v u |
	self isZero
		ifTrue: [aStream nextPut: $0]
		ifFalse:
			[v := self valuation + anInteger.
			u := self unitPart centeredRepresentative.
			v = 0
				ifTrue: [aStream print: u]
				ifFalse:
					[u abs ~= 1
						ifTrue: [aStream print: u; nextPut: $·]
						ifFalse: [u = -1 ifTrue: [aStream nextPut: $-]].
					aStream print: parent base.
					v ~= 1 ifTrue: [aStream nextPutAll: v printString super]]].
	precision isInfinite ifTrue: [^ self].
	aStream nextPutAll: ' + ', 'O' italic; nextPut: $(; print: parent base; nextPutAll: (precision - anInteger) printString super; nextPut: $)! !

!AdicInteger methodsFor: 'testing' stamp: 'len 5/4/2020 09:30:54'!
isPrime
	^ self isZero or: [self valuation = 1] "optimize"! !

!AdicInteger methodsFor: 'testing' stamp: 'len 9/8/2023 17:17:01'!
isUnit
	"Answer true if the receiver is invertible."
	^ (parent base | representative) not! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 8/11/2019 12:18:04'!
coefficients
	^ representative coefficients! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 8/12/2019 16:28:06'!
independentCoefficient
	^ representative independentCoefficient! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 6/7/2020 14:29:59'!
leadingCoefficient
	^ representative initialCoefficient! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 6/7/2020 14:30:20'!
leadingMonomial
	^ representative initialMonomial! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 6/7/2020 14:30:30'!
leadingTerm
	^ representative initialTerm! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 6/2/2023 15:05:10'!
unitPart
	self flag: #deprecated.
	true ifTrue: [^ super unitPart].
	
	self isUnit ifTrue: [^ self].
	self isZero ifTrue: [^ self parent one].
	parent isUnivariate
		ifTrue:
			[| v |
			v := representative order.
			^ parent representative: representative >> v precision: precision - v].
	^ self notYetImplemented! !

!PowerSeries methodsFor: 'accessing' stamp: 'len 6/12/2023 16:24:49'!
valuation
	^ representative order! !

!PowerSeries methodsFor: 'comparing' stamp: 'len 6/2/2023 01:21:59'!
isWeaklyEqualTo: anElement
	"Answer true if the receiver is 'weakly equal' to the argument.
	Note that this relation is not transitive."
	anElement precision < precision ifTrue: [^ anElement isWeaklyEqualTo: self].
	precision = anElement precision ifTrue: [^ representative = anElement representative].
	^ representative = (anElement representative truncatedTo: precision)! !

!PowerSeries methodsFor: 'operations' stamp: 'len 6/7/2020 14:52:16'!
* anElement
	| N |
	anElement isInteger ifTrue: [^ parent representative: representative * anElement precision: precision].
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #*].
	self isZero ifTrue: [^ self].
	anElement isZero ifTrue: [^ anElement].
	N _ precision + anElement valuation min: anElement precision + self valuation.
	^ parent representative: (representative multipliedBy: anElement representative truncatedTo: N) precision: N! !

!PowerSeries methodsFor: 'operations' stamp: 'len 8/10/2019 06:04:29'!
+ anElement
	| N |
	(parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	N _ precision min: anElement precision.
	^ parent representative: (representative truncatedTo: N) + (anElement representative truncatedTo: N) precision: N! !

!PowerSeries methodsFor: 'operations' stamp: 'len 6/15/2023 13:19:13'!
annihilator
	"Answer a generator of the ideal of all elements that multiplied by the receiver are zero.
	A power series f in R[[X]] is a zero divisor iff there is nonzero r in R with fr = 0 (see [Fie71])."
	| r |
	self parent scalars isPIR ifFalse: [^ self annihilatorIdeal generator].
	r := self parent scalars one.
	self coefficients do: [:each| r := r lcm: each annihilator].
	^ self parent constant: r! !

!PowerSeries methodsFor: 'operations' stamp: 'len 10/5/2022 18:45:16'!
annihilatorIdeal
	"Answer the ideal of all elements that multiplied by the receiver are zero.
	A power series f in R[[X]] is a zero divisor iff there is nonzero r in R with fr = 0 (see [Fie71])."
	| answer |
	self parent scalars isNoetherian ifFalse: [^ super annihilatorIdeal].
	answer _ self parent scalars asIdeal.
	self coefficients do: [:each| each isZero ifFalse: [(answer _ answer ∩ each annihilatorIdeal) isTrivial ifTrue: [^ answer]]].
	^ answer apply: self parent scalars → self parent! !

!PowerSeries methodsFor: 'operations' stamp: 'len 8/12/2019 16:57:35'!
derivative
	^ parent representative: representative derivative precision: precision - 1! !

!PowerSeries methodsFor: 'operations' stamp: 'len 8/12/2019 16:59:13'!
derivativeIn: anIndeterminate
	^ parent representative: (representative derivativeIn: anIndeterminate) precision: precision - 1! !

!PowerSeries methodsFor: 'operations' stamp: 'len 5/4/2020 10:06:02'!
inverse
	"Naive but general power series inversion algorithm."
	| answer g g1 u |
	u _ self independentCoefficient inverse.
	g _ self one.
	g1 _ g - (self * u).
	answer _ self one.
	1 to: self precision do: [:i|
		answer _ answer + (g _ g * g1)].
	^ answer * u! !

!PowerSeries methodsFor: 'operations' stamp: 'len 6/2/2023 14:58:49'!
normalizationInverse
	"Answer a unit u such that 'self * u inverse' is a unique choice of associate."
	self flag: #deprecated.
	true ifTrue: [^ super normalizationInverse].
	
	self isUnit ifTrue: [^ self].
	self isZero ifTrue: [^ self parent one].
	parent isUnivariate
		ifTrue:
			[| v |
			v := representative order.
			^ parent representative: representative >> v precision: precision - v].
	^ self notYetImplemented! !

!PowerSeries methodsFor: 'operations' stamp: 'len 8/9/2019 12:41:12'!
value: anObject
	^ representative value: anObject! !

!PowerSeries methodsFor: 'printing' stamp: 'len 9/6/2023 17:17:23'!
printOn: aStream
	aStream print: (parent cover orderedBy: #llex) !! representative.
	precision isInfinite ifTrue: [^ self].
	aStream nextPutAll: ' + ', 'O' italic, '('.
	parent isUnivariate
		ifTrue: [aStream print: parent cover x ^ precision]
		ifFalse: [aStream print: parent cover * parent cover generators; nextPutAll: precision printString super].
	aStream nextPut: $)! !

!PowerSeries methodsFor: 'printing' stamp: 'len 9/6/2023 17:18:01'!
printOn: aStream shifted: anInteger
	(parent cover orderedBy: #llex) !! representative printOn: aStream shifted: {anInteger}.
	precision isInfinite ifTrue: [^ self].
	aStream nextPutAll: ' + ', 'O' italic, '('.
	parent cover x ^ precision printOn: aStream shifted: anInteger.
	aStream nextPut: $)! !

!PowerSeries methodsFor: 'testing' stamp: 'len 5/24/2022 11:58:11'!
isNilpotent
	"Answer true if self^n = 0 for some integer n >= 1. See [Fie71]."
	self parent scalars isNoetherian ifFalse: [^ super isNilpotent].
	^ self coefficients allSatisfy: [:each| each isNilpotent]! !

!PowerSeries methodsFor: 'testing' stamp: 'len 5/4/2020 09:38:15'!
isUnit
	"Answer true if the receiver is invertible."
	self parent scalars isField ifTrue: [^ self independentCoefficient isUnit].
	^ super isUnit! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 9/8/2023 17:18:01'!
abs
	"Answer the p-adic absolute value (or norm) of the receiver."
	self flag: #fixme. "this works only for p-adics, should implement for Laurent series"
	^ self isWeaklyZero ifTrue: [0] ifFalse: [self parent base ^ valuation negated]! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 14:58:05'!
absolutePrecision
	^ unitPart absolutePrecision! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 15:01:47'!
denominator
	^ valuation < 0 ifTrue: [unitPart one << valuation] ifFalse: [unitPart one]! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 15:01:31'!
numerator
	^ valuation <= 0 ifTrue: [unitPart] ifFalse: [unitPart >> valuation]! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 14:58:47'!
parent
	^ unitPart parent fractions! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 14:58:58'!
relativePrecision
	unitPart isZero ifTrue: [^ 0].
	^ self absolutePrecision - valuation! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 14:59:05'!
unitPart
	^ unitPart! !

!AdicFraction methodsFor: 'accessing' stamp: 'len 5/9/2023 14:59:12'!
valuation
	^ valuation! !

!AdicFraction methodsFor: 'comparing' stamp: 'len 5/9/2023 14:57:14'!
= anObject
	^ anObject class = self class and: [valuation = anObject valuation and: [unitPart = anObject unitPart]]! !

!AdicFraction methodsFor: 'comparing' stamp: 'len 6/1/2023 21:50:21'!
hash
	^ 0! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/2/2023 14:45:03'!
* anElement
"	anElement isInteger ifTrue: [^ (self class valuation: valuation unit: unitPart * anElement) canonicalize]."
	(self parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #*].
	self isWeaklyZero ifTrue: [^ self].
	anElement isWeaklyZero ifTrue: [^ anElement].
	^ self class valuation: valuation + anElement valuation unit: unitPart * anElement unitPart! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/2/2023 16:56:30'!
+ anElement
	| v u |
	(self parent includes: anElement) ifFalse: [^ self adapt: anElement andSend: #+].
	self isWeaklyZero ifTrue: [^ anElement].
	anElement isWeaklyZero ifTrue: [^ self].
	valuation = anElement valuation ifTrue:
		[u := unitPart + anElement unitPart.
		^ self class valuation: valuation + u valuation unit: u unitPart].
	^ (v := valuation - anElement valuation) > 0
		ifTrue: [self class valuation: anElement valuation unit: unitPart >> v + anElement unitPart]
		ifFalse: [self class valuation: valuation unit: unitPart + (anElement unitPart >> v negated)]! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/2/2023 17:03:09'!
<< anInteger
	^ self >> anInteger negated! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/2/2023 17:02:56'!
>> anInteger
	^ self class valuation: valuation + anInteger unit: unitPart! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/1/2023 11:25:42'!
colift: anElement
	"Answer x such that xa = b, where a is the receiver and b is the argument. Answer nil if there's no solution.
	This is the right division of b by a, written b / a."
	anElement isWeaklyZero ifTrue: [^ anElement].
	^ self class
		valuation: valuation - anElement valuation
		unit: unitPart * anElement unitPart inverse! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/1/2023 11:25:47'!
inverse
	self isWeaklyZero ifTrue: [^ DivisibilityError new signalReceiver: self one selector: #/ argument: self].
	self relativePrecision <= 0 ifTrue: [^ self zero].
	^ self class valuation: valuation negated unit: unitPart inverse! !

!AdicFraction methodsFor: 'operations' stamp: 'len 6/2/2023 14:36:08'!
negated
	self isWeaklyZero ifTrue: [^ self].
	^ self class valuation: valuation unit: unitPart negated! !

!AdicFraction methodsFor: 'printing' stamp: 'len 6/1/2023 21:59:27'!
printOn: aStream
	self isWeaklyZero ifTrue: [aStream print: 0. ^ self].
	unitPart printOn: aStream shifted: valuation! !

!AdicFraction methodsFor: 'testing' stamp: 'len 5/9/2023 14:59:47'!
isIntegral
	^ self valuation >= 0! !

!AdicFraction methodsFor: 'testing' stamp: 'len 6/2/2023 14:36:20'!
isWeaklyZero
	^ valuation isPositiveInfinity! !

!AdicFraction methodsFor: 'testing' stamp: 'len 5/9/2023 15:00:02'!
isZero
	self isWeaklyZero ifFalse: [^ false].
	^ self shouldNotImplement! !

!AdicFraction methodsFor: 'private' stamp: 'len 6/2/2023 18:41:58'!
valuation: anInteger unit: anAdic
	valuation := anInteger.
	unitPart := anAdic.
	self assert: unitPart isUnit.
	valuation isPositiveInfinity ifTrue: [unitPart := unitPart one]! !

!AdicFraction class methodsFor: 'instance creation' stamp: 'len 5/9/2023 14:55:20'!
valuation: anInteger unit: anAdic
	^ self new valuation: anInteger unit: anAdic! !

!Float methodsFor: '*Completions' stamp: 'len 5/5/2023 16:13:06'!
, aTuple
	^ ℝ !!!! {self} , aTuple! !

!Float methodsFor: '*Completions' stamp: 'len 5/23/2020 10:13:14'!
parent
	^ ℝ! !

!Complex methodsFor: '*Completions' stamp: 'len 5/23/2020 10:12:18'!
parent
	^ ℂ! !

!Ring methodsFor: '*Completions' stamp: 'len 12/18/2021 11:45:36'!
completion
	"If the receiver is a local ring A with maximal ideal I, answer the I-adic completion of A, i.e. the inverse limit of A / I^n."
	self assert: self isLocal.
	self isField ifTrue: [^ self]. "because the maximal ideal is {0}"
	^ self completionAt: self maximalIdeal! !

!Ring methodsFor: '*Completions' stamp: 'len 12/18/2021 11:41:16'!
completionAt: aMaximalIdeal
	"If the receiver is a commutative ring A and the argument is the maximal ideal I, answer the I-adic completion of A, i.e. the inverse limit of A / I^n."
	| P |
	P _ (self polynomialsIn: aMaximalIdeal generators size) completion.
	^ P / (aMaximalIdeal generators withIndexCollect: [:each :i| (P x: i) - each])! !

!Ring methodsFor: '*Completions' stamp: 'len 6/2/2019 16:57:27'!
powerSeries
	^ self polynomials completion! !

!Ring methodsFor: '*Completions' stamp: 'len 8/9/2019 11:52:35'!
powerSeriesIn: anIntegerOrCollection
	"Answer the ring of multivariate formal power series over the receiver in the given indeterminates."
	^ (self polynomialsIn: anIntegerOrCollection) completion! !

!RationalField methodsFor: '*Completions' stamp: 'len 11/20/2022 11:25:09'!
adic: aPrime
	^ (ℤ adic: aPrime) fractions! !

!RationalField methodsFor: '*Completions' stamp: 'len 11/20/2022 11:25:16'!
adic: aPrime precision: anInteger
	^ (ℤ adic: aPrime precision: anInteger) fractions! !

!RationalField methodsFor: '*Completions' stamp: 'len 5/23/2020 09:03:52'!
algebraicClosure
	^ ℂ! !

!IntegerRing methodsFor: '*Completions' stamp: 'len 5/9/2023 14:46:19'!
adic: aPrime
	^ AdicIntegerRing at: aPrime! !

!IntegerRing methodsFor: '*Completions' stamp: 'len 5/9/2023 14:46:19'!
adic: aPrime precision: anInteger
	^ AdicIntegerRing at: aPrime precision: anInteger! !

!LocalRing methodsFor: '*Completions' stamp: 'len 9/8/2023 17:32:39'!
completion
	self numerators = ℤ ifTrue: [^ ℤ adic: self base].
	^ super completion! !

!PolynomialRing methodsFor: '*Completions' stamp: 'len 5/9/2023 17:31:06'!
completion
	"Answer the ring of power series, i.e. the completion of the receiver at the maximal ideal generated by the indeterminates."
	^ PowerSeriesRing on: self! !

!Sequence methodsFor: '*Completions' stamp: 'len 9/16/2022 08:04:16'!
bellSeries: p
	^ self codomain powerSeries coefficients: (self species to: self codomain map: [:n| self at: p^(n-1)])! !

!Sequence methodsFor: '*Completions' stamp: 'len 9/16/2022 08:05:02'!
exponentialSeries
	"Answer the exponential generating function of the receiver."
	^ self codomain powerSeries coefficients: (self species to: self codomain map: [:n| (self at: n) / (n-1) factorial])! !

!Sequence methodsFor: '*Completions' stamp: 'len 12/29/2015 04:26'!
generatingSeries
	"Answer the ordinary generating function of the receiver."
	^ self ordinaryGeneratingSeries! !

!Sequence methodsFor: '*Completions' stamp: 'len 6/2/2019 17:00:31'!
ordinaryGeneratingSeries
	"Answer the ordinary generating function of the receiver."
	^ self codomain powerSeries coefficients: self! !
