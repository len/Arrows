'From Cuis 6.0 [latest update: #5898] on 2 October 2023 at 9:07:56 pm'!
'Description '!
!provides: 'Sequences' 1 1!
SystemOrganization addCategory: #Sequences!
SystemOrganization addCategory: 'Sequences-Tests'!


!classDefinition: #ContinuedFraction category: #Sequences!
Number subclass: #ContinuedFraction
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sequences'!
!classDefinition: 'ContinuedFraction class' category: #Sequences!
ContinuedFraction class
	instanceVariableNames: ''!

!classDefinition: #ContinuedFractionsTest category: 'Sequences-Tests'!
TestCase subclass: #ContinuedFractionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sequences-Tests'!
!classDefinition: 'ContinuedFractionsTest class' category: 'Sequences-Tests'!
ContinuedFractionsTest class
	instanceVariableNames: ''!

!classDefinition: #SequenceTest category: 'Sequences-Tests'!
TestCase subclass: #SequenceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sequences-Tests'!
!classDefinition: 'SequenceTest class' category: 'Sequences-Tests'!
SequenceTest class
	instanceVariableNames: ''!

!classDefinition: #CFiniteSequence category: #Sequences!
Sequence subclass: #CFiniteSequence
	instanceVariableNames: 'coefficients initialValues'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sequences'!
!classDefinition: 'CFiniteSequence class' category: #Sequences!
CFiniteSequence class
	instanceVariableNames: ''!

!classDefinition: #DirichletSeries category: #Sequences!
Object subclass: #DirichletSeries
	instanceVariableNames: 'coefficients'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sequences'!
!classDefinition: 'DirichletSeries class' category: #Sequences!
DirichletSeries class
	instanceVariableNames: ''!


!ContinuedFraction commentStamp: '<historical>' prior: 0!
Finite or infinite continued fractions.

The finite case correspondes to rational numbers, for example:
  36/17 as: ContinuedFraction

But continued fractions can be infinite:
  ContinuedFraction e

Given a Float, and assuming the number is actually rational, its continued fraction expansion can be used to guess the rational number by truncating it when the convergents start to get too big (in bitsize terms), or when the coefficients of the continued fraction get too big:
  (2.117647 as: ContinuedFraction) convergents
!

!CFiniteSequence commentStamp: '<historical>' prior: 0!
Constant-recursive (or C-recursive or C-finite) sequences, i.e. sequences satisfying homogeneous linear recurrences with constant coefficients:
	a(n) = c_1 a(n-1) + ... + c_d a(n-d)

These sequences are closed under:
- addition a(n)+b(n)
- (Hadamard or termwise) multiplication a(n)*b(n)
- convolution (Cauchy multiplication)
- interlacing {a(1),b(1),a(2),b(2),...}
- scalar multiplication a(n)*c
- (left) shift a(n+k), k>0
- indefinite sumation a(1)+a(2)+...+a(n)
- multisection a(mn+r), 0 <= r < m.

Power series whose coefficients are linear recursive sequences are closed under:
- addition
- multiplication
- derivation

Example:
	CFiniteSequence fibonacci.
	CFiniteSequence fibonacci generatingFunction

Example:
	| f |
	f _ CFiniteSequence fibonacci * CFiniteSequence pellNumbers.
	f minimalPolynomial
!

!DirichletSeries commentStamp: '<historical>' prior: 0!
Formal Dirichlet series, i.e. formal series in an indeterminate x of the form: sum a(n) * n^{-x} for n >= 1.

Examples:
	DirichletSeries coefficients: Sequence moebius.
	Sequence liouville dirichletSeries.

Structure:
 coefficients	Sequence -- the coefficients of the Dirichlet series
!

!ContinuedFraction methodsFor: 'accessing' stamp: 'len 12/2/2015 00:07'!
coefficients
	^ coefficients! !

!ContinuedFraction methodsFor: 'arithmetic' stamp: 'len 12/3/2015 02:17'!
negated
	^ self class coefficients: coefficients negated! !

!ContinuedFraction methodsFor: 'arithmetic' stamp: 'len 12/4/2015 01:45'!
reciprocal
	^ (coefficients at: 1) = 0
		ifTrue: [self class coefficients: (coefficients copyFrom: 2 to: coefficients size)]
		ifFalse: [self class coefficients: 0, coefficients]! !

!ContinuedFraction methodsFor: 'convergents' stamp: 'len 3/27/2016 04:46'!
convergentAt: anInteger
	| n x |
	coefficients size > 0 ifFalse: [^ Infinity positive].
	n _ coefficients size min: anInteger.
	x _ coefficients at: n.
	(1 to: n-1) reverseDo: [:i| x _ x reciprocal + (coefficients at: i)].
	^ x! !

!ContinuedFraction methodsFor: 'convergents' stamp: 'len 9/16/2022 07:37:52'!
convergents
	^ Sequence to: ℚ map: [:i| self convergentAt: i]! !

!ContinuedFraction methodsFor: 'convergents' stamp: 'len 3/27/2016 04:51'!
convergentsDo: aBlock
	| p1 p2 q1 q2 |
	p2 _ 0.
	p1 _ 1.
	q2 _ 1.
	q1 _ 0.
	coefficients withIndexDo: [:ai :i| | pi qi | pi _ ai*p1 + p2. qi _ ai*q1 + q2. aBlock value: pi/qi. p2 _ p1. p1 _ pi. q2 _ q1. q1 _ qi]! !

!ContinuedFraction methodsFor: 'convergents' stamp: 'len 3/27/2016 04:43'!
convergentsUpTo: n
	| answer |
	answer _ OrderedCollection new: n.
	self convergentsDo: [:each| answer size < n ifFalse: [^ answer]. answer add: each]! !

!ContinuedFraction methodsFor: 'converting' stamp: 'len 10/18/2016 14:05'!
asFraction
	self isInfinite ifTrue: [self error: 'infinite continued fraction'].
	^ self convergentAt: coefficients size! !

!ContinuedFraction methodsFor: 'initialization' stamp: 'len 5/5/2023 17:32:12'!
coefficients: aSequence
	coefficients _ (aSequence isKindOf: Sequence) ifTrue: [aSequence] ifFalse: [aSequence asArray]! !

!ContinuedFraction methodsFor: 'initialization' stamp: 'len 6/1/2020 13:47:40'!
fromFraction: aFraction
	| x a |
	aFraction isInfinity ifTrue: [coefficients _ (ℤ^0) zero. ^ self].
	coefficients _ OrderedCollection new.
	x _ aFraction.
	a _ x floor.
	coefficients add: a.
	[x _ x - a.
	x ~= 0]
		whileTrue:
			[x _ x reciprocal.
			a _ x floor.
			coefficients add: a].
	coefficients _ ℤ^coefficients size !! coefficients asArray! !

!ContinuedFraction methodsFor: 'printing' stamp: 'len 10/18/2016 13:49'!
printOn: aStream base: base
	| separator |
	aStream nextPut: $[.
	separator _ $;.
	(1 to: (coefficients size min: 10)) do: [:i| (coefficients at: i) printOn: aStream base: base] separatedBy: [aStream nextPut: separator. separator _ $,].
	coefficients size isInfinite ifTrue: [aStream nextPutAll: '...'].
	aStream nextPut: $]! !

!ContinuedFraction methodsFor: 'testing' stamp: 'len 10/18/2016 14:05'!
isFinite
	^ coefficients size isInfinite not! !

!ContinuedFraction methodsFor: 'testing' stamp: 'len 10/18/2016 13:50'!
isInfinite
	^ self isFinite not! !

!ContinuedFraction class methodsFor: 'examples' stamp: 'len 9/16/2022 07:42:36'!
e
	^ self coefficients: (Sequence map: [:n| n=1 ifTrue: [2] ifFalse: [n \\ 3 = 0 ifTrue: [2*n/3] ifFalse: [1]]])! !

!ContinuedFraction class methodsFor: 'examples' stamp: 'len 12/4/2015 01:14'!
phi
	"Answer the golden ratio."
	^ self coefficients: (Sequence constant: 1)! !

!ContinuedFraction class methodsFor: 'instance creation' stamp: 'len 5/8/2023 16:32:54'!
coefficients: aSequence
	^ self new coefficients: aSequence! !

!ContinuedFraction class methodsFor: 'instance creation' stamp: 'len 3/27/2016 05:07'!
fromFloat: aFloat
	^ self fromFraction: aFloat asFraction! !

!ContinuedFraction class methodsFor: 'instance creation' stamp: 'len 12/1/2015 23:04'!
fromFraction: aFraction
	^ self new fromFraction: aFraction! !

!ContinuedFraction class methodsFor: 'instance creation' stamp: 'len 4/30/2016 04:34'!
newFrom: aNumber
	aNumber isFloat ifTrue: [^ self fromFloat: aNumber].
	aNumber isFraction ifTrue: [^ self fromFraction: aNumber].
	^ super newFrom: aNumber! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:06:32'!
testContinuedFractions
	| x |
	x := ContinuedFraction fromFraction: 415/93.
	self assert: x coefficients = (4, 2, 6, 7).
	self assert: x asFraction = (415/93)! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:06:38'!
testContinuedFractions2
	-2 to: 2 by: 1/2 do:
		[:x| self assert: (ContinuedFraction fromFraction: x) asFraction = x]! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:06:46'!
testContinuedFractionsE
	| x e |
	e := ContinuedFraction e.
	x := e convergentAt: 20.
	self assert: (x asFloat - 1 exp) abs < 0.0000001! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:06:52'!
testContinuedFractionsGoldenRatio
	| x phi |
	phi := ContinuedFraction phi.
	x := phi convergentAt: 20.
	self assert: (x asFloat - 1.6180339887) abs < 0.000001! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:06:58'!
testContinuedFractionsGoldenRatio2
	| phi denominators |
	phi := ContinuedFraction phi.
	denominators := phi convergents collect: [:each| each denominator].
	self assert: (denominators equals: Sequence fibonacci << 1 upTo: 100)! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:07:03'!
testContinuedFractionsNegated
	| x |
	x := ContinuedFraction fromFraction: 415/93.
	self assert: x negated asFraction = (-415/93)! !

!ContinuedFractionsTest methodsFor: 'as yet unclassified' stamp: 'len 10/2/2023 21:07:10'!
testContinuedFractionsReciprocal
	| x |
	x := ContinuedFraction fromFraction: 415/93.
	self assert: x reciprocal asFraction = (93/415)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2019 18:09:49'!
testBellNumbers
	self assert: (Sequence bellNumbers beginsWith: #(1 1 2 5 15 52 203 877)).
	"The inverse binomial transform is the shift operator for the Bell numbers:"
	self assert: (Sequence bellNumbers binomialInverseTransform beginsWith: #(1 2 5 15 52 203 877)).! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 12/5/2015 01:13'!
testDirichletConvolution
	self assert: ((Sequence phi dirichlet: Sequence one) beginsWith: #(1 2 3 4 5)).
	self assert: ((Sequence moebius dirichlet: Sequence one) beginsWith: #(1 0 0 0 0))! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2016 04:14'!
testLinearRecursiveSequenceBug
	| F |
	F _ CFiniteSequence fibonacci.
	self assert: ((F - F) at: 1) = 0! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 08:27:26'!
testRecursive
	| f g |
	f _ Sequence fibonacci.
	g _ CFiniteSequence to: ℤ map: f maxOrder: 5.
	self assert: (f equals: g upTo: 20).
	self assert: g order = 2.
	self assert: g characteristicPolynomial coefficients asArray = #(-1 -1 1)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2016 04:13'!
testRecursive2
	| a b c |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence lucasNumbers.
	c _ a + (b*2) - (b+b).
	self assert: a = c! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:24'!
testRecursiveConvolution
	| a b c a2 b2 c2 |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence lucasNumbers.
	a2 _ Sequence map: [:i| a at: i].
	b2 _ Sequence map: [:i| b at: i].

	c _ a convolution: b.
	c2 _ a2 convolution: b2.
	
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 08:27:32'!
testRecursiveEquals
	| a b c |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence to: ℤ map: [:i| a at: i] maxOrder: 10.
	c _ CFiniteSequence lucasNumbers.
	self assert: a = b.
	self assert: a ~= c! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:29'!
testRecursiveIndefiniteSumation
	| a c a2 c2 |
	a _ CFiniteSequence fibonacci.
	a2 _ Sequence map: [:i| a at: i].

	c _ a sum.
	c2 _ a2 sum.
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 10/31/2016 04:13'!
testRecursiveMinimal
	| a f |
	a _ CFiniteSequence initialValues: #(0 1 1) coefficients: #(0 1 1).
	f _ CFiniteSequence fibonacci.
	self assert: a isMinimal not.
	self assert: a order = 3.
	self assert: a minimal order = 2.
	self assert: a minimalPolynomial = f characteristicPolynomial! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:34'!
testRecursiveProduct
	| a b c c2 |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence pellNumbers.
	c _ a * b.
	c2 _ Sequence map: [:i| (a at: i) * (b at: i)].
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20).
	self assert: c order = 4! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:40'!
testRecursiveScalarMultiplication
	| a c a2 c2 |
	a _ CFiniteSequence fibonacci.
	a2 _ Sequence map: [:i| a at: i].

	c _ a * 7.
	c2 _ a2 * 7.
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:45'!
testRecursiveShiftLeft
	| a c a2 c2 |
	a _ CFiniteSequence fibonacci.
	a2 _ Sequence map: [:i| a at: i].

	c _ a << 5.
	c2 _ a2 << 5.
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!SequenceTest methodsFor: 'as yet unclassified' stamp: 'len 9/16/2022 07:43:50'!
testRecursiveSum
	| a b c c2 |
	a _ CFiniteSequence fibonacci.
	b _ CFiniteSequence lucasNumbers.
	c _ a + b.
	c2 _ Sequence map: [:i| (a at: i) + (b at: i)].
	self assert: (c isKindOf: CFiniteSequence).
	self assert: (c equals: c2 upTo: 20)! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 6/4/2019 05:39:53'!
at: n ifAbsent: aBlock
	self order = 0 ifTrue: [^ self codomain zero].
	n <= initialValues size ifTrue: [^ initialValues at: n].
	^ (self matrix ^ (n - self order) * initialValues) last
"	answer _ 0.
	1 to: self order do: [:i |
		answer _ (self at: n - self order + i - 1) * (coefficients at: i) + answer].
	^ answer"! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 12/27/2015 01:41'!
characteristicPolynomial
	^ self codomain polynomials coefficients: (coefficients negated copyWith: self codomain one)! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 12/27/2015 03:49'!
coefficients
	^ coefficients! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 12/27/2015 03:49'!
initialValues
	^ initialValues! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 8/9/2020 07:50:39'!
matrix
	^ self propertyAt: #matrix ifAbsentPut: [self characteristicPolynomial companion transpose]! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 12/27/2015 02:08'!
minimalPolynomial
	^ self minimal characteristicPolynomial! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 12/28/2015 23:45'!
order
	^ coefficients size! !

!CFiniteSequence methodsFor: 'accessing' stamp: 'len 12/27/2015 01:44'!
startIndex
	^ 1! !

!CFiniteSequence methodsFor: 'accessing-private' stamp: 'len 12/3/2015 23:02'!
coefficients: anArray
	coefficients _ anArray! !

!CFiniteSequence methodsFor: 'accessing-private' stamp: 'len 6/1/2020 15:06:14'!
initialValues: anArray
	initialValues _ anArray.
	initialValues isTuple ifFalse: [initialValues _ self codomain^initialValues size !! initialValues]! !

!CFiniteSequence methodsFor: 'comparing' stamp: 'len 5/23/2020 10:04:19'!
= aSequence
	| f g |
	(aSequence isKindOf: CFiniteSequence)
		ifFalse: [^ super = aSequence].
	f _ self minimal.
	g _ aSequence minimal.
	^ f coefficients = g coefficients and: [f initialValues = g initialValues]! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:27:41'!
* anObject
	(anObject isKindOf: self class)
		ifTrue: [^ self class to: self codomain map: super * anObject maxOrder: self order * anObject order].
	(self codomain includes: anObject)
		ifTrue: [^ self class to: self codomain map: super * anObject maxOrder: self order].
	^ super * anObject! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:27:46'!
+ aSequence
	(aSequence isKindOf: self class) ifFalse: [^ super + aSequence].
	^ self class to: self codomain map: super + aSequence maxOrder: self order + aSequence order! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:27:59'!
>> anInteger
	anInteger > 0 ifTrue: [^ super >> anInteger].
	^ self class to: self codomain map: super >> anInteger maxOrder: self order squared! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:28:10'!
convolution: aSequence
	(aSequence isKindOf: self class) ifFalse: [^ super convolution: aSequence].
	^ self class to: self codomain map: (super convolution: aSequence) maxOrder: self order * aSequence order! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:28:17'!
minimal
	^ self propertyAt: #minimal ifAbsentPut: [self class to: self codomain map: self maxOrder: self order]! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:28:21'!
negated
	^ self class to: self codomain map: super negated maxOrder: self order! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:28:26'!
squared
	| n |
	n _ self order.
	^ self class to: self codomain map: super squared maxOrder: n*(n-1)/2! !

!CFiniteSequence methodsFor: 'operations' stamp: 'len 9/16/2022 08:28:31'!
sum
	"Answer the indefinite sum of the receiver."
	^ self class to: self codomain map: super sum maxOrder: self order squared! !

!CFiniteSequence methodsFor: 'printing' stamp: 'len 12/27/2015 01:25'!
printLimit
	^ self order + 10 min: super printLimit! !

!CFiniteSequence methodsFor: 'series' stamp: 'len 1/1/2022 09:34:05'!
generatingFunction
	| r s |
	"Answer the generating function of the receiver, which is a rational function that equals its ordinary generating series."
	s _ self characteristicPolynomial reversed.
	r _ self codomain polynomials coefficients:
		((0 to: self order - 1) collect: [:j|
			(self at: j+1) - ((1 to: j) sum: [:i| (s at: i) * (self at: j-i+1)])]).
	^ r / s! !

!CFiniteSequence methodsFor: 'testing' stamp: 'len 12/27/2015 02:08'!
isMinimal
	^ self minimalPolynomial = self characteristicPolynomial! !

!CFiniteSequence methodsFor: 'testing' stamp: 'len 4/11/2016 23:10'!
isZero
	^ self minimal order = 0! !

!CFiniteSequence class methodsFor: 'examples' stamp: 'len 5/23/2020 10:04:12'!
fibonacci
	"The Fibonacci sequence defined as a C-finite sequence."
	^ CFiniteSequence initialValues: #(0 1) coefficients: #(1 1)! !

!CFiniteSequence class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
geometric: aNumber
	(aNumber isInteger or: [aNumber isFraction])
		ifFalse: [^ DomainError signal: 'argument is not rational'].
	^ CFiniteSequence to: ℚ initialValues: #(1) coefficients: {aNumber}! !

!CFiniteSequence class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
id
	"The identity sequence id(n) := n, defined by a recurrence."
	^ CFiniteSequence to: ℚ initialValues: #(1 2) coefficients: #(-1 2)! !

!CFiniteSequence class methodsFor: 'examples' stamp: 'len 2/22/2017 08:36:33'!
jacobsthalNumbers
	"The Jacobsthal numbers."
	^ CFiniteSequence initialValues: #(0 1) coefficients: #(2 1)! !

!CFiniteSequence class methodsFor: 'examples' stamp: 'len 2/22/2017 08:36:42'!
lucasNumbers
	"The Lucas numbers."
	^ CFiniteSequence initialValues: #(2 1) coefficients: #(1 1)! !

!CFiniteSequence class methodsFor: 'examples' stamp: 'len 2/22/2017 08:36:49'!
pellNumbers
	"The Pell numbers."
	^ CFiniteSequence initialValues: #(0 1) coefficients: #(1 2)! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 5/23/2020 09:34:55'!
initialValues: anArray coefficients: anotherArray
	^ self to: ℤ initialValues: anArray coefficients: anotherArray! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 12/27/2015 19:42'!
to: codomain constant: anObject
	^ self to: codomain initialValues: {anObject} coefficients: {anObject one}! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 3/30/2016 03:54'!
to: codomain initialValues: anArray characteristicPolynomial: aPolynomial
	^ self to: codomain initialValues: anArray coefficients: aPolynomial tail negated coefficients! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 11/5/2016 10:27'!
to: aCodomain initialValues: anArray coefficients: anotherArray
	^ self new codomain: aCodomain; initialValues: anArray; coefficients: anotherArray! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 12/28/2016 12:19:00'!
to: codomain initialValues: anArray minimalPolynomial: aPolynomial
	| answer |
	answer _ self to: codomain initialValues: anArray characteristicPolynomial: aPolynomial.
	answer propertyAt: #minimal put: answer.
	^ answer! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 6/4/2019 07:19:55'!
to: aCodomain initialValues: initialArray periodic: periodArray
	| coefficients |
	coefficients _ Array new: initialArray size + periodArray size.
	coefficients atAllPut: periodArray first zero.
	coefficients at: initialArray size + 1 put: periodArray first one.
	^ self to: aCodomain initialValues: initialArray asArray, periodArray coefficients: coefficients! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 9/16/2022 08:27:15'!
to: codomain map: aBlock maxOrder: n
	| initialValues bm f |
	initialValues _ (1 to: 2*n) collect: aBlock.
	bm _ ModifiedBerlekampMassey new
		field: (codomain isField ifTrue: [codomain] ifFalse: [codomain fractions]);
		input: initialValues.
	f _ bm minimalPolynomial.
	^ self to: codomain initialValues: (initialValues copyFrom: 1 to: f degree) minimalPolynomial:f! !

!CFiniteSequence class methodsFor: 'instance creation' stamp: 'len 12/27/2015 03:41'!
to: codomain periodic: anArray
	| coefficients |
	coefficients _ Array new: anArray size.
	coefficients atAllPut: anArray first zero.
	coefficients at: 1 put: anArray first one.
	^ self to: codomain initialValues: anArray coefficients: coefficients! !

!DirichletSeries methodsFor: 'printing' stamp: 'len 6/4/2019 05:32:31'!
printOn: aStream
	| c n first |
	first _ true.
	1 to: coefficients printLimit do: [:i|
		c _ coefficients at: i.
		c isZero
			ifFalse:
				[n _ i.
				c printString first = $-
					ifTrue: [first ifFalse: [aStream nextPutAll: ' - '].
								c _ c negated]
					ifFalse: [first ifFalse: [aStream nextPutAll: ' + ']].
				c printOn: aStream. first _ false.
				n > 1 ifTrue: [aStream nextPut: $/; print: n; nextPutAll: 'x' super]]].
	aStream nextPutAll: ' + ...'! !

!DirichletSeries methodsFor: 'accessing' stamp: 'len 12/28/2015 19:26'!
coefficients
	^ coefficients! !

!DirichletSeries methodsFor: 'accessing' stamp: 'len 12/28/2015 19:26'!
coefficients: aSequence
	coefficients _ aSequence! !

!DirichletSeries methodsFor: 'accessing' stamp: 'len 12/28/2015 19:26'!
indeterminate
	^ 1! !

!DirichletSeries methodsFor: 'accessing' stamp: 'len 12/28/2015 19:26'!
scalars
	^ coefficients codomain! !

!DirichletSeries methodsFor: 'arithmetic' stamp: 'len 12/28/2015 19:28'!
* anObject
	(self scalars includes: anObject)
		ifTrue: [^ self class coefficients: coefficients * anObject].
	(anObject isKindOf: self class)
		ifTrue: [^ self class coefficients: (coefficients dirichlet: anObject coefficients)].
	^ DomainError signal! !

!DirichletSeries methodsFor: 'arithmetic' stamp: 'len 12/28/2015 19:29'!
+ anObject
	^ self class coefficients: coefficients + anObject coefficients! !

!DirichletSeries methodsFor: 'arithmetic' stamp: 'len 12/28/2015 19:29'!
- anObject
	^ self + anObject negated! !

!DirichletSeries methodsFor: 'arithmetic' stamp: 'len 12/28/2015 19:30'!
/ anObject
	^ self * anObject reciprocal! !

!DirichletSeries methodsFor: 'arithmetic' stamp: 'len 12/28/2015 19:30'!
negated
	^ self class coefficients: coefficients negated! !

!DirichletSeries methodsFor: 'arithmetic' stamp: 'len 12/28/2015 19:30'!
squared
	^ self * self! !

!DirichletSeries methodsFor: 'comparing' stamp: 'len 12/28/2015 19:46'!
= anObject
	^ (anObject isKindOf: DirichletSeries) and: [self coefficients = anObject coefficients]! !

!DirichletSeries methodsFor: 'comparing' stamp: 'len 12/28/2015 19:46'!
hash
	^ coefficients hash! !

!DirichletSeries methodsFor: 'operations' stamp: 'len 6/4/2019 05:32:07'!
value: aNumber upTo: n
	| answer |
	answer _ aNumber zero.
	1 to: n do: [:i|
		| c |
		c _ coefficients at: i.
		answer _ answer + (c / (i ^ aNumber))].
	^ answer! !

!DirichletSeries class methodsFor: 'examples' stamp: 'len 10/31/2016 04:14'!
zeta
	^ self coefficients: (CFiniteSequence constant: 1)! !

!DirichletSeries class methodsFor: 'instance creation' stamp: 'len 12/28/2015 19:54'!
coefficients: aSequence
	^ self new coefficients: aSequence! !
