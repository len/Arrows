'From Cuis 6.0 [latest update: #5438] on 10 December 2022 at 11:43:17 am'!
'Description '!
!provides: 'Algebraic Numbers' 1 1!
SystemOrganization addCategory: #'Algebraic Numbers'!


!classDefinition: #RealAlgebraicNumber category: #'Algebraic Numbers'!
Number subclass: #RealAlgebraicNumber
	instanceVariableNames: 'minimalPolynomial bounds rightSign'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'RealAlgebraicNumber class' category: #'Algebraic Numbers'!
RealAlgebraicNumber class
	instanceVariableNames: ''!

!classDefinition: #AlgebraicNumbersTest category: #'Algebraic Numbers'!
TestCase subclass: #AlgebraicNumbersTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'AlgebraicNumbersTest class' category: #'Algebraic Numbers'!
AlgebraicNumbersTest class
	instanceVariableNames: ''!

!classDefinition: #ComplexAlgebraicNumber category: #'Algebraic Numbers'!
Object subclass: #ComplexAlgebraicNumber
	instanceVariableNames: 'minimalPolynomial approximation radius'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'ComplexAlgebraicNumber class' category: #'Algebraic Numbers'!
ComplexAlgebraicNumber class
	instanceVariableNames: ''!

!classDefinition: #ComplexBox category: #'Algebraic Numbers'!
Object subclass: #ComplexBox
	instanceVariableNames: 'real imaginary'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'ComplexBox class' category: #'Algebraic Numbers'!
ComplexBox class
	instanceVariableNames: ''!

!classDefinition: #ComplexDisk category: #'Algebraic Numbers'!
Object subclass: #ComplexDisk
	instanceVariableNames: 'center radius'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'ComplexDisk class' category: #'Algebraic Numbers'!
ComplexDisk class
	instanceVariableNames: ''!

!classDefinition: #ComplexRootIsolator category: #'Algebraic Numbers'!
Object subclass: #ComplexRootIsolator
	instanceVariableNames: 'polynomial derivatives'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'ComplexRootIsolator class' category: #'Algebraic Numbers'!
ComplexRootIsolator class
	instanceVariableNames: ''!

!classDefinition: #NumericalComplexRootIsolator category: #'Algebraic Numbers'!
Object subclass: #NumericalComplexRootIsolator
	instanceVariableNames: 'polynomial tolerance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'NumericalComplexRootIsolator class' category: #'Algebraic Numbers'!
NumericalComplexRootIsolator class
	instanceVariableNames: ''!

!classDefinition: #RealInterval category: #'Algebraic Numbers'!
Object subclass: #RealInterval
	instanceVariableNames: 'left right rightOpen leftOpen'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'RealInterval class' category: #'Algebraic Numbers'!
RealInterval class
	instanceVariableNames: ''!

!classDefinition: #RealRootIsolator category: #'Algebraic Numbers'!
Object subclass: #RealRootIsolator
	instanceVariableNames: 'polynomial sturm fourier roots'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'RealRootIsolator class' category: #'Algebraic Numbers'!
RealRootIsolator class
	instanceVariableNames: ''!

!classDefinition: #AlgebraicNumbersBenchmark category: #'Algebraic Numbers'!
Benchmark subclass: #AlgebraicNumbersBenchmark
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Algebraic Numbers'!
!classDefinition: 'AlgebraicNumbersBenchmark class' category: #'Algebraic Numbers'!
AlgebraicNumbersBenchmark class
	instanceVariableNames: ''!


!RealAlgebraicNumber commentStamp: '<historical>' prior: 0!
Real algebraic numbers, i.e. real zeros of polynomials with integer (or rational) coefficients, in 'minimal polynomial' representation. See also ComplexAlgebraicNumber.!

!ComplexAlgebraicNumber commentStamp: '<historical>' prior: 0!
Complex algebraic numbers, i.e. zeros of polynomials with integer (or rational) coefficients, in 'minimal polynomial' representation. See also RealAlgebraicNumber.!

!ComplexBox commentStamp: '<historical>' prior: 0!
Boxes in the complex plane, defined by two real intervals (for the real and imaginary parts).!

!ComplexDisk commentStamp: '<historical>' prior: 0!
Closed disks in the complex plane, defined by a center point and a radius.!

!ComplexRootIsolator commentStamp: '<historical>' prior: 0!
This is the CEVAL algorithm [1] for isolating the complex roots of a squarefree polynomial.

[SY09] Michael Sagraloff, Chee K. Yap, "An Efficient and Exact Subdivision Algorithm for Isolating Complex Roots of a Polynomial and its Complexity Analysis" (2009)!

!NumericalComplexRootIsolator commentStamp: '<historical>' prior: 0!
This algorithm separates complex roots of polynomials with real or complex coefficients. This is Newton's modified algorithm [1], and the convergence is quadratic. The polynomial is assumed to be squarefree.

Input: a squarefree polynomial of degree n with real or complex coefficients, and a desired minimum absolute error (tolerance);
Output: a collection of the n complex roots of the polynomial.

See [Coh96] Algorithm 3.6.6.!

!RealInterval commentStamp: '<historical>' prior: 0!
Real intervals in the real line. Each of my instances can have finite or infinite and open or closed bounds.

For some examples try this:
	RealInterval open: 1 closed: 2.
	RealInterval open: 0 open: Infinity positive.
	RealInterval closed: -1 closed: 1.
	RealInterval open: Infinity negative open: Infinity positive.
!

!AlgebraicNumbersBenchmark commentStamp: '<historical>' prior: 0!
AlgebraicNumbersBenchmark new run

testComplexAlgebraicNumberAddition	1.1 per second. (25 May 2018)
testComplexAlgebraicNumberDivision	63.9 per second.
testComplexAlgebraicNumberProduct	68.3 per second.
testComplexAlgebraicNumberSquare	2,080 per second.
testRealAlgebraicNumberAddition	0.936 per second.
testRealAlgebraicNumberDivision	61.7 per second.
testRealAlgebraicNumberProduct	67.1 per second.
testRealAlgebraicNumberSquare	2,010 per second.

testComplexAlgebraicNumberAddition	0.907 per second. (18 May 2018)
testComplexAlgebraicNumberDivision	61 per second.
testComplexAlgebraicNumberProduct	64.7 per second.
testComplexAlgebraicNumberSquare	1,760 per second.
testRealAlgebraicNumberAddition	0.997 per second.
testRealAlgebraicNumberDivision	58.2 per second.
testRealAlgebraicNumberProduct	62.6 per second.
testRealAlgebraicNumberSquare	1,730 per second.

testComplexAlgebraicNumberAddition	0.84 per second.
testComplexAlgebraicNumberDivision	58.4 per second.
testComplexAlgebraicNumberProduct	61.6 per second.
testComplexAlgebraicNumberSquare	1,770 per second.
testRealAlgebraicNumberAddition	0.952 per second.
testRealAlgebraicNumberDivision	48.3 per second.
testRealAlgebraicNumberProduct	56.2 per second.
testRealAlgebraicNumberSquare	1,460 per second.

testComplexAlgebraicNumberAddition	1.08 per second. (29 Apr 2018)
testComplexAlgebraicNumberDivision	57.6 per second.
testComplexAlgebraicNumberProduct	61.2 per second.
testComplexAlgebraicNumberSquare	1,660 per second.
testRealAlgebraicNumberAddition	0.78 per second.
testRealAlgebraicNumberDivision	55.8 per second.
testRealAlgebraicNumberProduct	59.1 per second.
testRealAlgebraicNumberSquare	1,610 per second.' 
!

!RealAlgebraicNumber methodsFor: 'accessing' stamp: 'len 4/29/2018 18:45:52'!
denominator
	"The denominator of an algebraic number is the smallest positive integer that multiplied by the algebraic number is an algebraic integer."
	^ minimalPolynomial coefficients inject: 1 into: [:result :each| result lcm: each denominator]! !

!RealAlgebraicNumber methodsFor: 'accessing' stamp: 'len 4/29/2018 17:32:57'!
minimalPolynomial
	^ minimalPolynomial! !

!RealAlgebraicNumber methodsFor: 'accessing' stamp: 'len 5/23/2020 09:31:27'!
parent
	^ ℝ! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 6/4/2016 22:22'!
absoluteError
	"Answer the maximun current error in the rational approximation of the receiver."
	^ bounds length / 2! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 6/4/2016 21:23'!
approximation
	"Answer the current rational approximation of the receiver."
	^ bounds midpoint! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 1/16/2016 06:27'!
bounds
	^ bounds! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 6/4/2016 23:06'!
refine
	"Refine the rational interval approximation of the receiver."
	self refineAt: self approximation! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 4/29/2018 18:37:00'!
refineAt: aFraction
	"Refine the rational interval approximation of the receiver to one of both (leftBound, aFraction] or (aFraction, rightBound]."
	| sign |
	self rightSign = (sign _ (minimalPolynomial value: aFraction) sign)
		ifTrue: [self rightBound: aFraction; rightSign: sign]
		ifFalse: [self leftBound: aFraction].
	sign = 0 ifTrue: [self leftBound: aFraction. self rightBound: aFraction]! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 1/16/2016 07:10'!
refineTo: aNumber
	"Refine the receiver up to get an approximation error < aNumber."
	[self absoluteError >= aNumber] whileTrue: [self refine]! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 1/17/2016 07:44'!
refineWith: aRealAlgebraicNumber
	"Refine the rational interval approximation of either the receiver or the argument, depending on which is less precise at the moment."
	self relativeError > aRealAlgebraicNumber relativeError
		ifTrue: [self refine]
		ifFalse: [aRealAlgebraicNumber refine]! !

!RealAlgebraicNumber methodsFor: 'approximating' stamp: 'len 6/4/2016 23:10'!
relativeError
	"Answer the relative error in the current rational approximation."
	self isZero ifTrue: [^ 0].
	^ self absoluteError / self approximation abs! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/9/2022 08:34:45'!
* aNumber
	| f b |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToRealAlgebraic: self andSend: #*].
	aNumber isZero ifTrue: [^ aNumber].
	self isZero ifTrue: [^ self].
	f _ RealRootIsolator on:
		(minimalPolynomial zeroProduct: aNumber minimalPolynomial) squarefreePart.
	[b _ self bounds * aNumber bounds.
	f moreThanOneRootBetween: b left and: b right]
		whileTrue:
			[self refineWith: aNumber].
	^ self class polynomial: f polynomial bounds: b! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/9/2022 08:34:48'!
+ aNumber
	| f b |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToRealAlgebraic: self andSend: #+].
	aNumber isZero ifTrue: [^ self].
	self isZero ifTrue: [^ aNumber].
	f _ RealRootIsolator on:
		(minimalPolynomial zeroAddition: aNumber minimalPolynomial) squarefreePart.
	[b _ self bounds + aNumber bounds.
	f moreThanOneRootBetween: b left and: b right]
		whileTrue:
			[self refineWith: aNumber].
	^ self class polynomial: f polynomial bounds: b! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 11/9/2015 14:28'!
- anObject
	^ self + anObject negated! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 11/9/2015 14:29'!
/ anObject
	^ self * anObject reciprocal! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 5/29/2022 19:39:19'!
inverse
	"Answer the multiplicative inverse of the receiver."
	self isZero ifTrue: [^ DivisibilityError new signalReceiver: self one selector: #/ argument: self].
	^ self class
		minimalPolynomial: minimalPolynomial zeroReciprocal
		bounds: bounds reciprocal! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 4/29/2018 17:27:06'!
negated
	"Answer the additive inverse of the receiver."
	self isZero ifTrue: [^ self].
	^ self class
		minimalPolynomial: minimalPolynomial zeroNegation
		bounds: bounds negated! !

!RealAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 8/12/2019 11:01:53'!
reciprocal
	"Answer the multiplicative inverse of the receiver."
	^ self inverse! !

!RealAlgebraicNumber methodsFor: 'comparing' stamp: 'len 6/4/2016 21:32'!
< anObject
	"Answer whether the receiver is less than the argument."

	anObject class = self class ifFalse: [^ (self - anObject) sign = -1].
	(self = anObject) ifTrue: [^ false].
	[self bounds intersects: anObject bounds] whileTrue: [self refineWith: anObject].
	^ self rightBound < anObject leftBound! !

!RealAlgebraicNumber methodsFor: 'comparing' stamp: 'len 4/29/2018 18:34:58'!
= anObject
	anObject isNumber ifFalse: [^ false].
	self == anObject ifTrue: [^ true].
	anObject class = self class
		ifFalse: [^ anObject adaptToRealAlgebraic: self andSend: #=].
	^ minimalPolynomial = anObject minimalPolynomial and: [bounds intersects: anObject bounds]! !

!RealAlgebraicNumber methodsFor: 'comparing' stamp: 'len 12/13/97 03:56'!
hash
	"Answer the hash value for the receiver."

	^ self truncated hash! !

!RealAlgebraicNumber methodsFor: 'converting' stamp: 'len 4/29/2018 18:39:36'!
adaptToAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (ComplexAlgebraicNumber polynomial: minimalPolynomial approximation: bounds midpoint radius: bounds length / 2)! !

!RealAlgebraicNumber methodsFor: 'converting' stamp: 'len 11/19/2015 21:39'!
adaptToFloat: rcvr andSend: selector
	^ rcvr perform: selector with: self asFloat! !

!RealAlgebraicNumber methodsFor: 'converting' stamp: 'len 11/19/2015 21:40'!
adaptToFraction: rcvr andSend: selector
	^ (self class fromRational: rcvr) perform: selector with: self! !

!RealAlgebraicNumber methodsFor: 'converting' stamp: 'len 3/3/2021 07:32:16'!
adaptToInteger: rcvr andSend: selector
	^ (self class fromRational: rcvr asRational) perform: selector with: self! !

!RealAlgebraicNumber methodsFor: 'converting' stamp: 'len 6/4/2016 23:06'!
asFloat
	"Answer a Float approximation of the receiver."

	self refineTo: 0.000001.
	^ self approximation asFloat! !

!RealAlgebraicNumber methodsFor: 'converting' stamp: 'len 5/23/2020 09:33:01'!
asRational
	"Convert the receiver to Fraction or Integer."
	| alpha |
	self leftBound = self rightBound ifTrue: [^ self leftBound].
	minimalPolynomial degree < 2
		ifTrue:
			[alpha _ minimalPolynomial independentCoefficient negated / minimalPolynomial leadingCoefficient.
			self bounds: (RealInterval open: alpha closed: alpha).
			^ alpha].
	(minimalPolynomial rootsIn: ℚ)
		do: [:each|
			(each between: self leftBound and: self rightBound)
				ifTrue: [self bounds: (RealInterval open: each closed: each). ^ each]].
	self error: 'the number is irrational'! !

!RealAlgebraicNumber methodsFor: 'initialization' stamp: 'len 12/17/2021 14:44:26'!
minimalPolynomial: aPolynomial bounds: aRealInterval
	minimalPolynomial _ aPolynomial normalized.
	bounds _ aRealInterval.
	self normalize! !

!RealAlgebraicNumber methodsFor: 'mathematical functions' stamp: 'len 8/8/97 20:54'!
sign
	"Answer 1, -1 or 0, depending on the sign of the receiver."

	^ self leftBound sign! !

!RealAlgebraicNumber methodsFor: 'mathematical functions' stamp: 'len 4/29/2018 18:38:08'!
squareRoot
	self isRational ifFalse: [^ self notYetImplemented].
	self negative ifTrue: [^ self negated squareRoot i].
	self isZero ifTrue: [^ self].
	^ self class
		polynomial: minimalPolynomial zeroSquareRoot "is it minimal?"
 		bounds: (RealInterval open: 0 open: (self asRational max: 1))! !

!RealAlgebraicNumber methodsFor: 'mathematical functions' stamp: 'len 4/29/2018 18:39:12'!
squared
	| x odd even |
	x _ minimalPolynomial parent x.
	odd _ minimalPolynomial odd.
	even _ minimalPolynomial even.
	self flag: #fix.
	^ self class "this could be rational"
		polynomial: odd squared * x - even squared
		bounds: self bounds squared! !

!RealAlgebraicNumber methodsFor: 'printing' stamp: 'len 4/29/2018 18:39:50'!
printOn: aStream base: base
	minimalPolynomial degree < 4 "too slow" ifTrue: [self isRational ifTrue: [self asRational printOn: aStream base: base. ^ self]].
"	polynomial degree = 2 ifTrue: [^ self printQuadraticOn: aStream base: base]."
	(self asFloat roundTo: 0.00001) printOn: aStream base: base.
	aStream nextPutAll: '..'

	"aStream
		nextPutAll: self class name;
		space;
		nextPut: $(;
		print: self polynomial;
		nextPutAll: '; ~ ';
		print: self asFloat;
		nextPut: $)"! !

!RealAlgebraicNumber methodsFor: 'testing' stamp: 'len 12/5/2017 20:12:14'!
isAlgebraic
	^ true! !

!RealAlgebraicNumber methodsFor: 'testing' stamp: 'len 5/27/2016 02:06'!
isAlgebraicInteger
	"An algebraic number is an 'algebraic integer' if it's the root of a monic polynomial in Z[x], in particular if it's minimal polynomial has integer coefficients."
	^ self minimalPolynomial coefficients allSatisfy: [:each| each isInteger]! !

!RealAlgebraicNumber methodsFor: 'testing' stamp: 'len 5/23/2020 09:33:01'!
isRational
	"Answer true if the receiver is a rational number."
	minimalPolynomial degree < 2 ifTrue: [^ true].
	self rightBound = self leftBound ifTrue: [^ true].
	(minimalPolynomial rootsIn: ℚ)
		do: [:each|
			(self bounds includes: each)
				ifTrue: [self bounds: (RealInterval closed: each closed: each). ^ true]].
	^ false! !

!RealAlgebraicNumber methodsFor: 'testing' stamp: 'len 11/19/2015 21:33'!
isZero
	^ self leftBound = 0 and: [self rightBound = 0]
"	^ self isRational and: [self asRational = 0]"! !

!RealAlgebraicNumber methodsFor: 'truncation and round off' stamp: 'len 1/17/2016 02:20'!
truncated
	"Answer the integer nearest the receiver toward zero."
	| t |
	self absoluteError truncated > 0 ifTrue: [self refineTo: 9/10].
	^ (t _ self rightBound truncated) = self rightBound
		ifTrue: [self refineAt: t.
				self rightBound truncated]
		ifFalse: [t]! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 1/17/2016 02:57'!
bounds: aRealInterval
	bounds _ aRealInterval.
	rightSign _ nil! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 1/16/2016 06:38'!
leftBound
	^ bounds left! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 1/16/2016 07:05'!
leftBound: aFraction
	bounds _ RealInterval open: aFraction closed: bounds right! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 7/20/2019 09:17:44'!
normalize
	"Change the representation of the receiver such that if the receiver is not 0 then 0 is not in the rational approximation interval."
	| p sign |
	self leftBound sign = self rightBound sign ifTrue: [^ self].
	minimalPolynomial independentCoefficient isZero
		ifTrue: [^ self leftBound: 0; rightBound: 0].
	"for every real root r not 0: |r| > 1 / (1 + f normInfinite), from Cauchy and Landau inequalities"
	p _ 1 / (1 + minimalPolynomial normInfinite asFloat).
	p negated < self leftBound
		ifTrue: [^ self leftBound: p].
	p > self rightBound
		ifTrue: [^ self rightBound: p negated].
	sign _ (minimalPolynomial value: p negated) sign.
	sign ~= (minimalPolynomial value: self leftBound) sign
		ifTrue: [self rightBound: p negated; rightSign: sign]
		ifFalse: [(minimalPolynomial value: p) sign ~= self rightSign
				ifTrue: [self leftBound: p]
				ifFalse: [self leftBound: 0; rightBound: 0]].
	self leftBound > self rightBound ifTrue: [self error: 'interval normalization error']! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 1/16/2016 06:38'!
rightBound
	^ bounds right! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 1/17/2016 02:57'!
rightBound: aFraction
	bounds _ RealInterval open: bounds left closed: aFraction.
	rightSign _ nil! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 4/29/2018 18:37:11'!
rightSign
	rightSign isNil ifTrue: [rightSign _ (minimalPolynomial value: self rightBound) sign].
	^ rightSign! !

!RealAlgebraicNumber methodsFor: 'private' stamp: 'len 1/17/2016 02:59'!
rightSign: anInteger
	rightSign _ anInteger! !

!RealAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
phi
	"Answer the Phidias number or Golden Ratio."
	| x |
	x _ ℚ polynomials x.
	^ self minimalPolynomial: x^2 - x - 1 bounds: (RealInterval open: 1.0 open: 2.0)! !

!RealAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
sqrt2
	"Answer an instance of the receiver representing the square root of 2."
	^ self
		minimalPolynomial: ℚ polynomials x squared - 2
 		bounds: (RealInterval open: 1.0 open: 2.0)! !

!RealAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
sqrt: aNumber
	"Answer a square root of the rational argument."
	aNumber positive ifFalse: [^ DomainError signal: 'negative square root'].
	^ self
		minimalPolynomial: ℚ polynomials x squared - aNumber
 		bounds: (RealInterval open: 0.0 open: (aNumber max: 1.0))! !

!RealAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 5/23/2020 09:33:01'!
fromRational: aRational
	"Answer an instance of the receiver representing the rational number anIntegerOrFraction."
	| x |
	x _ ℚ polynomials x.
	^ self
		minimalPolynomial: x * aRational denominator - aRational numerator
 		bounds: (RealInterval open: aRational closed: aRational)! !

!RealAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 4/29/2018 17:30:10'!
minimalPolynomial: aPolynomial bounds: aRealInterval
	"Answer an instance of the receiver representing the unique zero of aPolynomial in the given rational interval."
	^ self new minimalPolynomial: aPolynomial bounds: aRealInterval! !

!RealAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 8/5/2021 08:42:43'!
polynomial: aPolynomial bounds: aRealInterval
	"Answer an instance of the receiver representing the unique zero of aPolynomial in the given interval."
	| approximation |
	approximation _ aRealInterval midpoint.
	^ self minimalPolynomial: (aPolynomial factorization detectMin: [:f| (f value: approximation) abs]) bounds: aRealInterval! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:39:40'!
testArithmetic
	| sqrt2 sqrt3 sqrt5 sqrt6 |
	sqrt2 _ ComplexAlgebraicNumber sqrt2.
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	sqrt6 _ ComplexAlgebraicNumber sqrt: 6.
	self assert: sqrt2 squared = 2.
	self assert: (sqrt2 + sqrt3) * sqrt2 = (2 + sqrt6).
	self assert: (sqrt3 / sqrt5) squared reciprocal = (5/3).
"	self assert: (((5 root: 3) + sqrt2 - sqrt2) ^ 3) asRational = 5"! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:39:54'!
testDivision
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	self assert: sqrt3 / sqrt5 / sqrt3 / sqrt5 = (1/5)! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 7/4/2022 13:22:08'!
testMinimalPolynomial
	"Example from 'Algebraic Number Theory: A Computational Approach' by William Stein (29 pp)."
	| x alpha |
	x _ ℚ polynomials x.
	alpha _ 1 + 5 squareRoot / 2.
	self assert: alpha minimalPolynomial = (x^2 - x - 1)! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 23:50:51'!
testMultiplicationByi
	| a |
	a _ ComplexAlgebraicNumber sqrt2.
	self assert: a i i = a negated! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 04:54:12'!
testNorm
	| a |
	a _ ComplexAlgebraicNumber phi.
	self assert: a norm = a conjugates product! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:40:10'!
testProduct
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	self assert: sqrt3 * sqrt5 * sqrt3 * sqrt5 = 15! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 18:49:57'!
testRealArithmetic
	| sqrt2 sqrt3 sqrt5 sqrt6 |
	sqrt2 _ RealAlgebraicNumber sqrt2.
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	sqrt6 _ RealAlgebraicNumber sqrt: 6.
	self assert: sqrt2 squared asRational = 2.
	self assert: (sqrt2 + sqrt3) * sqrt2 = (2 + sqrt6).
	self assert: (sqrt3 / sqrt5) squared reciprocal = (5/3).
"	self assert: (((five root: 3) + sqrt2 - sqrt2) ^ 3) asRational = 5"! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:59'!
testRealDivision
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	self assert: (sqrt3 / sqrt5 / sqrt3 / sqrt5) asRational = (1/5)! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 7/4/2022 12:03:58'!
testRealIntervalIntersects
	| I J |
	I _ RealInterval open: -1 closed: 0.
	J _ RealInterval open: 0 closed: 1.
	self assert: (I intersects: J) not.
	J _ RealInterval closed: 0 open: 1.
	self assert: (I intersects: J).
	J _ RealInterval closed: -2 closed: -1.
	self assert: (I intersects: J) not! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:33:01'!
testRealNormalizationBug
	| f interval x |
	x _ ℚ polynomials x.
	f _ x^4 * 1576875 - (x^2 * 877250) + 43923.
	interval _ RealInterval open: (-260139/1345600) closed: (81631/134560).
	RealAlgebraicNumber polynomial: f bounds: interval! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:59'!
testRealProduct
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	self assert: (sqrt3 * sqrt5 * sqrt3 * sqrt5) asRational = 15! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 21:59'!
testRealSquare
	| sqrt2 |
	sqrt2 _ RealAlgebraicNumber sqrt2.
	self assert: sqrt2 squared asRational = 2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 18:48:17'!
testRealSquareRoot
	| a |
	a _ RealAlgebraicNumber sqrt2.
	self assert: a squareRoot squared = a! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 6/4/2016 22:00'!
testRealSum
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ RealAlgebraicNumber sqrt: 2.
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	self assert: sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5 = sqrt2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 05:40:24'!
testSquare
	| sqrt2 |
	sqrt2 _ ComplexAlgebraicNumber sqrt2.
	self assert: sqrt2 squared = 2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 18:49:16'!
testSquareRoot
	| a |
	a _ ComplexAlgebraicNumber sqrt2.
	self assert: a squareRoot squared = a! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 1/20/2018 19:18:17'!
testSum
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ ComplexAlgebraicNumber sqrt: 2.
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	self assert: sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5 = sqrt2! !

!AlgebraicNumbersTest methodsFor: 'as yet unclassified' stamp: 'len 4/30/2018 04:53:56'!
testTrace
	| a |
	a _ ComplexAlgebraicNumber phi.
	self assert: a trace = a conjugates sum! !

!ComplexAlgebraicNumber methodsFor: 'printing' stamp: 'len 1/11/2016 06:44'!
printOn: aStream
	self printOn: aStream base: 10! !

!ComplexAlgebraicNumber methodsFor: 'printing' stamp: 'len 4/30/2018 00:27:26'!
printOn: aStream base: base
	| real imaginary isImaginary |
	minimalPolynomial degree = 2 ifTrue: [^ self printQuadraticOn: aStream base: base].
	self refineTo: self printPrecision.
	real _ approximation real asFloat roundTo: self printPrecision.
	imaginary _ approximation imaginary asFloat roundTo: self printPrecision.
	(isImaginary _ self isImaginary)
		ifFalse: [real printOn: aStream base: base].
	self isReal
		ifFalse:
			[isImaginary ifFalse: [aStream nextPutAll: (0 <= imaginary ifTrue: [' + '] ifFalse: [imaginary _ imaginary negated. ' - '])].
			imaginary printOn: aStream base: base.
			aStream nextPutAll: 'i' italic].
	aStream nextPutAll: '..'! !

!ComplexAlgebraicNumber methodsFor: 'printing' stamp: 'len 1/11/2018 21:07:33'!
printPrecision
"	self absoluteError < 1 ifTrue: [^ self absoluteError asFloat].
	^ 0.001"
	^ 0.00001! !

!ComplexAlgebraicNumber methodsFor: 'printing' stamp: 'len 1/9/2022 08:33:51'!
printQuadraticOn: aStream base: base
	| a b c discriminant z y x1 x2 sign |
	a _ (minimalPolynomial at: 2) asFraction.
	b _ (minimalPolynomial at: 1) asFraction.
	c _ (minimalPolynomial at: 0) asFraction.
	discriminant _ b squared - (4*a*c).
	z _ discriminant squarefreePart.
	y _ (discriminant / z) sqrt.
	x1 _ b negated + (discriminant negative ifTrue: [discriminant negated sqrt i] ifFalse: [discriminant sqrt]) / (2*a).
	x2 _ b negated - (discriminant negative ifTrue: [discriminant negated sqrt i] ifFalse: [discriminant sqrt]) / (2*a).
	(x1 - approximation) abs <= radius
		ifTrue: [sign _ ' + ']
		ifFalse: [(x2 - approximation) abs <= radius ifTrue: [sign _ ' - '. y _ y negated] ifFalse: [self halt]].
	b = 0 ifFalse:
		[b/(-2*a) printOn: aStream base: base.
		aStream nextPutAll: sign.
		y _ y abs].
	y negative ifTrue: [aStream nextPut: $-. y _ y abs].
	y/(2*a) = 1 ifFalse: [y/(2*a) printOn: aStream base: base].
	z abs = 1 ifFalse:
		[aStream nextPut: $Γ.
		z abs printOn: aStream base: base].
	z negative ifTrue: [aStream nextPutAll: 'i' italic]! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 5/23/2020 09:31:47'!
conjugates
	"The conjugates of an algebraic number with minimal polynomial of degree n are all its n-th roots."
	^ (minimalPolynomial rootsIn: ℂ) "copyWithout: self"! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 5/2/2018 16:37:43'!
degree
	^ minimalPolynomial degree! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 4/29/2018 18:46:05'!
denominator
	"The denominator of an algebraic number is the smallest positive integer that multiplied by the algebraic number is an algebraic integer."
	^ minimalPolynomial coefficients inject: 1 into: [:result :each| result lcm: each denominator]! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 5/2/2018 16:33:35'!
height
	^ self minimalPolynomial height! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 4/28/2018 07:07:34'!
minimalPolynomial
	^ minimalPolynomial! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 6/4/2016 21:18'!
one
	^ 1! !

!ComplexAlgebraicNumber methodsFor: 'accessing' stamp: 'len 6/4/2016 21:18'!
zero
	^ 0! !

!ComplexAlgebraicNumber methodsFor: 'accessing-private' stamp: 'len 1/15/2016 02:07'!
approximation: aNumber radius: anotherNumber
	approximation _ aNumber.
	radius _ anotherNumber! !

!ComplexAlgebraicNumber methodsFor: 'accessing-private' stamp: 'len 7/17/2019 10:53:56'!
minimalPolynomial: aPolynomial
	minimalPolynomial _ aPolynomial integerPrimitivePart "<- or should it be monic?"! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 6/5/2016 19:35'!
absoluteError
	^ radius! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 3/24/2018 21:01:14'!
approximation
	"Answre a rational or float current approximation of the receiver."
	^ approximation! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 6/5/2016 20:59'!
bounds
	"Answer a disk in the complex plane where the receiver is located, isolated from all other roots of its minimal polynomial."
	^ ComplexDisk center: approximation radius: radius! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 5/2/2018 16:47:24'!
refine
	| P Q P2 Q2 m x v c dx y v1 m1 |
	radius _ radius / 2.0.
	P _ minimalPolynomial. "should it be monic?"
	Q _ P.
	P2 _ P derivative.
	Q2 _ P2.
	x _ approximation.
	v _ Q value: x.
	m _ v norm2.
	c _ 0. dx _ v / (Q2 value: x).
	[dx abs >= radius]
		whileTrue:
			[y _ x - dx.
			v1 _ Q value: y.
			m1 _ v1 norm2.
			m1 < m
				ifTrue: [x _ y. v _ v1. m _ m1. c _ 0. dx _ v / (Q2 value: x)]
				ifFalse: [c _ c + 1. dx _ dx / 4. c > 20 ifTrue: [self error: 'failed']]].
	"Polish root:"
	2 timesRepeat: [x _ x - ((P value: x) / (P2 value: x))].
	approximation _ x! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 12/5/2017 20:05:25'!
refineTo: aNumber
	"Refine the receiver up to get an approximation error < aNumber."
	[self absoluteError > aNumber] whileTrue: [self refine]! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 3/24/2018 21:00:20'!
refineWith: aComplexAlgebraicNumber
	"Refine the rational interval approximation of either the receiver or the argument, depending on which is less precise at the moment."
	self relativeError > aComplexAlgebraicNumber relativeError
		ifTrue: [self refine]
		ifFalse: [aComplexAlgebraicNumber refine]! !

!ComplexAlgebraicNumber methodsFor: 'approximating' stamp: 'len 4/29/2018 20:17:35'!
relativeError
	"Answer the relative error in the current rational approximation."
	self isZero ifTrue: [^ 0].
	^ self absoluteError / self approximation abs! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/9/2022 08:33:39'!
* aNumber
	| isolator x r |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToAlgebraic: self andSend: #*].
	aNumber isZero ifTrue: [^ aNumber].
	self isZero ifTrue: [^ self].
	isolator _ ComplexRootIsolator on: (minimalPolynomial zeroProduct: aNumber minimalPolynomial) squarefreePart.
	[x _ approximation * aNumber approximation.
	r _ self absoluteError * aNumber approximation norm2 + (approximation norm2 * aNumber absoluteError).
	isolator atMostOneRootAround: x radius: r]
		whileFalse:
			[self refineWith: aNumber].
	^ self class polynomial: isolator polynomial approximation: x radius: r! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/9/2022 08:33:47'!
+ aNumber
	| isolator x r |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToAlgebraic: self andSend: #+].
	aNumber isZero ifTrue: [^ self].
	self isZero ifTrue: [^ aNumber].
	isolator _ ComplexRootIsolator on: (minimalPolynomial zeroAddition: aNumber minimalPolynomial) squarefreePart.
	[x _ approximation + aNumber approximation.
	r _ self absoluteError + aNumber absoluteError.
	isolator atMostOneRootAround: x radius: r]
		whileFalse:
			[self refineWith: aNumber].
	^ self class polynomial: isolator polynomial approximation: x radius: r! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/12/2016 03:38'!
- anObject
	^ self + anObject negated! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 5/9/2020 20:35:07'!
/ aNumber
	aNumber class = self class
		ifFalse: [^ aNumber adaptToAlgebraic: self andSend: #+].
	^ self * aNumber inverse! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/13/2016 04:01'!
i
	^ self * self class i! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 8/12/2019 11:02:07'!
inverse
	"Answer the multiplicative inverse of the receiver."
	| isolator x r |
	self isZero ifTrue: [^ ZeroDivide signal].
	isolator _ ComplexRootIsolator on: minimalPolynomial zeroReciprocal.
	[x _ approximation reciprocal.
	r _ self absoluteError / approximation norm2 * x norm2.
	isolator atMostOneRootAround: x radius: r]
		whileFalse:
			[self refine].
	^ self class minimalPolynomial: isolator polynomial approximation: x radius: r! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 4/28/2018 07:10:56'!
negated
	"Answer the additive inverse of the receiver."
	^ self class
		minimalPolynomial: minimalPolynomial zeroNegation
		approximation: approximation negated
		radius: radius! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 1/9/2022 08:33:55'!
squareRoot
	| isolator x r |
	self isZero ifTrue: [^ self].
	isolator _ ComplexRootIsolator on: minimalPolynomial zeroSquareRoot squarefreePart.
	[x _ approximation sqrt.
	r _ self absoluteError sqrt.
	isolator atMostOneRootAround: x radius: r]
		whileFalse:
			[self refine].
	^ self class polynomial: isolator polynomial approximation: x radius: r! !

!ComplexAlgebraicNumber methodsFor: 'arithmetic' stamp: 'len 5/4/2018 19:10:55'!
squared
	| x odd even |
	x _ minimalPolynomial parent x.
	odd _ minimalPolynomial odd.
	even _ minimalPolynomial even.
	^ self class
		polynomial: odd squared * x - even squared
		approximation: approximation squared
		radius: radius * approximation norm2 * 2! !

!ComplexAlgebraicNumber methodsFor: 'comparing' stamp: 'len 4/28/2018 16:48:28'!
= anObject
	self == anObject ifTrue: [^ true].
	anObject isNumber ifFalse: [^ false].
	anObject class = self class
		ifFalse: [^ anObject adaptToAlgebraic: self andSend: #=].
	^ minimalPolynomial = anObject minimalPolynomial and: [self bounds intersects: anObject bounds]! !

!ComplexAlgebraicNumber methodsFor: 'comparing' stamp: 'len 4/29/2018 20:09:45'!
hash
	^ minimalPolynomial hash! !

!ComplexAlgebraicNumber methodsFor: 'converting' stamp: 'len 1/13/2016 03:41'!
adaptToFloat: rcvr andSend: selector
	^ rcvr perform: selector with: self asFloat! !

!ComplexAlgebraicNumber methodsFor: 'converting' stamp: 'len 1/13/2016 03:42'!
adaptToFraction: rcvr andSend: selector
	^ (self class fromRational: rcvr) perform: selector with: self! !

!ComplexAlgebraicNumber methodsFor: 'converting' stamp: 'len 1/13/2016 03:42'!
adaptToInteger: rcvr andSend: selector
	^ (self class fromRational: rcvr) perform: selector with: self! !

!ComplexAlgebraicNumber methodsFor: 'converting' stamp: 'len 1/13/2016 03:59'!
asComplexFloat
	"Answer a Float approximation of the receiver."
	self refineTo: 0.000001.
	^ approximation asFloat! !

!ComplexAlgebraicNumber methodsFor: 'converting' stamp: 'len 6/4/2016 22:16'!
asFloat
	"Answer a Float approximation of the receiver."
	self refineTo: 0.000001.
	^ self approximation asFloat! !

!ComplexAlgebraicNumber methodsFor: 'converting' stamp: 'len 5/23/2020 09:33:01'!
asRational
	"Convert the receiver to Fraction or Integer."
	| alpha |
	radius = 0 ifTrue: [^ approximation]. "could be float!!"
	minimalPolynomial degree < 2
		ifTrue:
			[alpha _ minimalPolynomial independentCoefficient negated / minimalPolynomial leadingCoefficient.
			approximation _ alpha. radius _ 0.
			^ alpha].
	(minimalPolynomial rootsIn: ℚ)
		do: [:each|
			(each - approximation) abs < radius
				ifTrue: [approximation _ each. radius _ 0. ^ each]].
	self error: 'the number is irrational'! !

!ComplexAlgebraicNumber methodsFor: 'operations' stamp: 'len 4/30/2018 04:46:52'!
norm
	^ minimalPolynomial independentCoefficient / minimalPolynomial leadingCoefficient * (minimalPolynomial degree even ifTrue: [1] ifFalse: [-1])

"alternatively:
	^ self conjugates product"! !

!ComplexAlgebraicNumber methodsFor: 'operations' stamp: 'len 1/1/2022 09:27:54'!
trace
	"Answer the absolute trace of the receiver."
	^ (minimalPolynomial at: minimalPolynomial degree - 1) negated

"alternatively:
	^ self conjugates sum"! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 1/13/2016 03:33'!
isAlgebraic
	^ true! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 5/2/2018 17:16:29'!
isAlgebraicInteger
	"An algebraic number is an 'algebraic integer' if it's the root of a monic polynomial in Z[x], in particular if it's minimal polynomial has integer coefficients."
	^ minimalPolynomial isMonic "<- defining the minimal polynomial as integer primitive"

"alternatively, defining the minimal polynomial as monic:
	^ minimalPolynomial coefficients allSatisfy: [:each| each isInteger]
"! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 4/29/2018 23:49:18'!
isImaginary
	^ approximation real abs < radius! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 1/13/2016 03:33'!
isNumber
	^ true! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 5/23/2020 09:33:01'!
isRational
	minimalPolynomial degree < 2 ifTrue: [^ true].
	radius = 0 ifTrue: [^ true].
	(minimalPolynomial rootsIn: ℚ)
		do: [:each|
			(each - approximation) abs < radius
				ifTrue: [approximation _ each. radius _ 0. ^ true]].
	^ false! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 4/29/2018 23:49:23'!
isReal
	^ approximation imaginary abs < radius "and: [((RealRootIsolator on: minimalPolynomial) numberOfRootsBetween: approximation real - radius and: approximation real + radius) > 0]"! !

!ComplexAlgebraicNumber methodsFor: 'testing' stamp: 'len 4/28/2018 07:10:14'!
isZero
	^ approximation abs < radius and: [(minimalPolynomial value: 0) = 0]! !

!ComplexAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
i
	| x |
	x _ ℚ polynomials x.
	^ self minimalPolynomial: x^2 + 1 approximation: 1 i radius: 0! !

!ComplexAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
phi
	"Answer the Phidias number or Golden Ratio."
	| x |
	x _ ℚ polynomials x.
	^ self minimalPolynomial: x^2 - x - 1 approximation: 2.0 radius: 1! !

!ComplexAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
sqrt2
	"Answer an instance of the receiver representing the square root of 2."

	^ self
		minimalPolynomial: ℚ polynomials x squared - 2
 		approximation: 1.5
		radius: 0.1! !

!ComplexAlgebraicNumber class methodsFor: 'examples' stamp: 'len 5/23/2020 09:33:01'!
sqrt: anIntegerOrFraction
	"Answer a square root of the rational argument."
	^ self
		minimalPolynomial: ℚ polynomials x squared - anIntegerOrFraction
 		approximation: (anIntegerOrFraction negative ifTrue: [anIntegerOrFraction negated sqrt i] ifFalse: [anIntegerOrFraction sqrt]) radius: 0.00001! !

!ComplexAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 1/15/2016 02:04'!
approximation: approximateValue radius: approximationError maxDegree: n
	"Answer an algebraic number with the given approximate value, approximation error, with minimal polynomial of degree at most n."
	^ self notYetImplemented! !

!ComplexAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 5/23/2020 09:33:01'!
fromRational: anIntegerOrFraction
	"Answer an instance of the receiver representing the rational number anIntegerOrFraction."
	| q x |
	q _ anIntegerOrFraction asFraction.
	x _ ℚ polynomials x.
	^ self
		minimalPolynomial: x * q denominator - q numerator
 		approximation: anIntegerOrFraction
		radius: 0! !

!ComplexAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 1/15/2016 02:05'!
minimalPolynomial: aPolynomial approximation: aComplex radius: aNumber
	^ self new
		minimalPolynomial: aPolynomial;
		approximation: aComplex radius: aNumber! !

!ComplexAlgebraicNumber class methodsFor: 'instance creation' stamp: 'len 8/5/2021 08:37:14'!
polynomial: aPolynomial approximation: aComplex radius: aNumber
	| minimalPolynomial |
	minimalPolynomial _ aPolynomial factorization detectMin: [:f| (f value: aComplex) abs].
	minimalPolynomial degree = 1 ifTrue: [^ minimalPolynomial independentCoefficient negated / minimalPolynomial leadingCoefficient].
	^ self minimalPolynomial: minimalPolynomial approximation: aComplex radius: aNumber! !

!ComplexBox methodsFor: 'printing' stamp: 'len 1/11/2016 02:57'!
printOn: aStream
	aStream print: real; nextPutAll: ' + '; print: imaginary; nextPut: $i! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 1/16/2016 05:56'!
area
	^ self measure! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 1/16/2016 05:59'!
closure
	^ self class real: real closure imaginary: imaginary closure! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:58'!
imaginary
	^ imaginary! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:55'!
measure
	^ real measure * imaginary measure! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 1/15/2016 07:21'!
midpoint
	^ Complex real: real midpoint imaginary: imaginary midpoint! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 6/5/2016 06:20'!
quarters
	| imaginaries reals |
	reals _ self real halves.
	imaginaries _ self imaginary halves.
	^ {self class real: (reals at: 1) imaginary: (imaginaries at: 1).
		self class real: (reals at: 1) imaginary: (imaginaries at: 2).
		self class real: (reals at: 2) imaginary: (imaginaries at: 1).
		self class real: (reals at: 2) imaginary: (imaginaries at: 2)}! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 6/5/2016 04:08'!
radius2
	^ real length squared + imaginary length squared / 4! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 1/11/2016 02:58'!
real
	^ real! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 11/23/2022 21:31:17'!
sample
	"Answer a sample point in the receiver."
	| sampleImaginary sampleReal |
	sampleReal _ real sample.
	sampleImaginary _ imaginary sample.
	^ ℂ^sampleReal size fill: [:i| Complex real: (sampleReal at: i) imaginary: (sampleImaginary at: i)]! !

!ComplexBox methodsFor: 'accessing' stamp: 'len 6/5/2016 21:42'!
width
	^ real length min: imaginary length! !

!ComplexBox methodsFor: 'accessing-private' stamp: 'len 1/11/2016 02:53'!
imaginary: aRealInterval
	imaginary _ aRealInterval! !

!ComplexBox methodsFor: 'accessing-private' stamp: 'len 1/11/2016 02:53'!
real: aRealInterval
	real _ aRealInterval! !

!ComplexBox methodsFor: 'testing' stamp: 'len 1/16/2016 05:50'!
closureIncludes: aComplex
	^ (real closureIncludes: aComplex real) and: [imaginary closureIncludes: aComplex imaginary]! !

!ComplexBox methodsFor: 'testing' stamp: 'len 1/11/2016 02:54'!
includes: aComplex
	^ (real includes: aComplex real) and: [imaginary includes: aComplex imaginary]! !

!ComplexBox methodsFor: 'testing' stamp: 'len 1/16/2016 05:52'!
intersects: aComplexBox
	^ (real intersects: aComplexBox real) and: [imaginary intersects: aComplexBox imaginary]! !

!ComplexBox methodsFor: 'testing' stamp: 'len 1/11/2016 03:40'!
isBounded
	^ real isBounded and: [imaginary isBounded]! !

!ComplexBox methodsFor: 'testing' stamp: 'len 1/11/2016 03:40'!
isClosed
	^ real isClosed and: [imaginary isClosed]! !

!ComplexBox methodsFor: 'testing' stamp: 'len 1/11/2016 03:41'!
isOpen
	^ real isOpen and: [imaginary isOpen]! !

!ComplexBox class methodsFor: 'instance creation' stamp: 'len 6/4/2016 21:53'!
center: aComplex radius: aNumber
	^ self
		real: (RealInterval closed: aComplex real - aNumber closed: aComplex real + aNumber)
		imaginary: (RealInterval closed: aComplex imaginary - aNumber closed: aComplex imaginary + aNumber)! !

!ComplexBox class methodsFor: 'instance creation' stamp: 'len 1/11/2016 03:43'!
real: aRealInterval imaginary: anotherRealInterval
	^ self new real: aRealInterval; imaginary: anotherRealInterval! !

!ComplexBox class methodsFor: 'instance creation' stamp: 'len 6/5/2016 04:07'!
with: aComplex with: anotherComplex
	^ self
		real: (RealInterval with: aComplex real with: anotherComplex real)
		imaginary: (RealInterval with: aComplex imaginary with: anotherComplex imaginary)! !

!ComplexDisk methodsFor: 'printing' stamp: 'len 6/5/2016 20:42'!
printOn: aStream
	aStream nextPutAll: 'D('; print: self center; nextPut: $,; print: self radius; nextPut: $)! !

!ComplexDisk methodsFor: 'accessing' stamp: 'len 1/13/2016 22:30'!
center
	^ center! !

!ComplexDisk methodsFor: 'accessing' stamp: 'len 6/30/2016 04:43'!
enclosingBox
	"Answer a ComplexBox that encloses the receiver."
	^ ComplexBox center: self center radius: self radius! !

!ComplexDisk methodsFor: 'accessing' stamp: 'len 1/15/2016 07:21'!
midpoint
	^ self center! !

!ComplexDisk methodsFor: 'accessing' stamp: 'len 6/5/2016 20:42'!
radius
	^ radius! !

!ComplexDisk methodsFor: 'accessing-private' stamp: 'len 6/5/2016 20:47'!
center: aComplex
	center _ aComplex! !

!ComplexDisk methodsFor: 'accessing-private' stamp: 'len 6/5/2016 20:47'!
radius: aNumber
	radius _ aNumber! !

!ComplexDisk methodsFor: 'arithmetic' stamp: 'len 6/5/2016 20:46'!
* aDisk
	^ self class center: aDisk center * self center radius: self radius * aDisk radius! !

!ComplexDisk methodsFor: 'arithmetic' stamp: 'len 6/30/2016 04:37'!
+ aDisk
	^ self class center: aDisk center + self center radius: self radius + aDisk radius! !

!ComplexDisk methodsFor: 'testing' stamp: 'len 6/5/2016 20:44'!
includes: anObject
	^ (anObject - center) norm2 <= radius squared! !

!ComplexDisk methodsFor: 'testing' stamp: 'len 6/5/2016 04:13'!
intersects: aBall
	self flag: #fix.
	^ (aBall center - center) norm2 <= (self radius + aBall radius) squared! !

!ComplexDisk class methodsFor: 'instance creation' stamp: 'len 6/5/2016 20:47'!
center: aComplex radius: aNumber
	^ self new center: aComplex; radius: aNumber! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 3/6/2017 16:56:02'!
T2: K at: m radius: r
	| t |
	t _ 0.
	1 to: self derivatives size - 1 do: [:j|
		| f |
		f _ derivatives at: j+1.
		t _ (f value: m) abs * (r ^ j) / j factorial + t].
	^ (derivatives first value: m) abs > (K * t)! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/24/2018 17:53:58'!
T: K at: m radius: r
	| t |
	t _ 0.
	1 to: self derivatives size do: [:j|
		| f |
		f _ derivatives at: j.
		t _ (f value: m) abs * (r ^ j) / j factorial + t].
	^ (polynomial value: m) abs > (K * t)! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 12/4/2017 13:22:20'!
atMostOneRootAround: center radius: radius
	"If this holds, there's at most one root in the givem disk."
	^ self T2: 3/2 at: center radius: radius
"	^ (self rootsIn: aBox) size > 1"! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/11/2018 21:50:32'!
bigBounds
	"Compute and answer a first estimate of a box containing all the complex roots."
	| bound |
	bound _ self polynomial norm1 / self polynomial leadingCoefficient.
	bound _ bound max: 1.
	^ ComplexBox center: 0.0 radius: bound asFloat! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/24/2018 17:54:29'!
derivatives
	| f |
	derivatives ifNotNil: [^ derivatives].
	f _ polynomial.
	derivatives _ OrderedCollection new: f degree.
	f degree timesRepeat: [derivatives add: (f _ f derivative)].
	^ derivatives! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/24/2018 17:54:34'!
eightPointTest: m width: w
	| N NE E SE S SW W NW points values imaginaryCrossings realCrossings |
	N _ 1 i.
	NE _ 20/29 + (21/29) i.
	E _ 1.
	SE _ 20/29 - (21/29) i.
	S _ -1 i.
	SW _ -20/29 - (21/29) i.
	W _ -1.
	NW _ -20/29 + (21/29) i.
	points _ {N. NE. E. SE. S. SW. W. NW} collect: [:p| w*p + m].
	values _ points collect: [:p| polynomial value: p].
	realCrossings _ (1 to: values size) select: [:j| (values at: j) real isZero or: [(values at: j) real * (values atWrap: j+1) real < 0]].
	realCrossings size = 2 ifFalse: [^ false].
	imaginaryCrossings _ (1 to: values size) select: [:j| (values at: j) imaginary isZero or: [(values at: j) imaginary * (values atWrap: j+1) imaginary < 0]].
	imaginaryCrossings size = 2 ifFalse: [^ false].
	realCrossings first < imaginaryCrossings first
		ifTrue: [^ realCrossings second < imaginaryCrossings second].
	^ realCrossings first > imaginaryCrossings first
		and: [realCrossings second > imaginaryCrossings second]! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/24/2018 18:20:26'!
polynomial
	^ polynomial! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 7/17/2019 10:54:00'!
polynomial: aPolynomial
	polynomial _ aPolynomial integerPrimitivePart! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/16/2018 16:06:13'!
roots
	"Separate the roots in disks each of them containing one isolated root."
	^ self rootsInBox: self bigBounds! !

!ComplexRootIsolator methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 16:56:39'!
rootsInBox: aBox
	"Separate the roots in disks each of them containing one isolated root."
	| queue disks |
	queue _ OrderedCollection with: aBox.
	disks _ OrderedCollection new.
	[queue isEmpty]
		whileFalse:
			[| B m delta |
			B _ queue removeFirst.
			m _ B midpoint.
			delta _ B width * 3 / 4. "> B radius, and a dyadic fraction"
			(self T: 1 at: m radius: delta)
				"if holds, then D(m,delta) constains no roots"
				ifFalse:
					[((self T2: 6 at: m radius: 4*delta) and: [self T2: 3/2 at: m radius: 8*delta])
						"if the second holds, then D(m,8*delta) contains at most one root"
						"the first is required so that if two disks overlap, we can discard any one of them"
						ifTrue:
							[(self eightPointTest: m width: B width)
								ifTrue:
									[| D i D2 |
									D _ ComplexDisk center: m radius: 4*delta.
									(i _ (1 to: disks size) detect: [:j| (D2 _ disks at: j) intersects: D] ifNone: [])
									 isNil
										ifTrue: [disks add: D]
										ifFalse: [D2 radius < D radius ifTrue: [disks at: i put: D]]]]
						ifFalse:
							[queue addAll: B quarters]]].
	^ disks! !

!ComplexRootIsolator class methodsFor: 'instance creation' stamp: 'len 4/24/2018 18:01:29'!
on: aPolynomial
	^ self new polynomial: aPolynomial! !

!ComplexRootIsolator class methodsFor: 'root finding' stamp: 'len 12/10/2022 11:20:34'!
allRootsOf: aPolynomial
	| answer |
	answer _ Bag new.
	aPolynomial integerPrimitivePart factorization withOccurrencesDo: [:f :e|
		f degree = 1
			ifTrue:
				[answer add: (f independentCoefficient negated / f leadingCoefficient) withOccurrences: e]
			ifFalse:
				[(ComplexRootIsolator on: f) roots do: [:each|
	 				answer add: (ComplexAlgebraicNumber minimalPolynomial: f approximation: each center radius: each radius) withOccurrences: e]]].
	^ answer! !

!NumericalComplexRootIsolator methodsFor: 'accessing' stamp: 'len 5/10/2019 08:30:57'!
rootNear: aComplex
"	[self roots do: [:each| (each approximation - aComplex) abs < epsilon ifTrue: [^ each]].
	self roots do: [:each| each refine]] repeat"
	^ self roots detectMin: [:each| (each approximation - aComplex) abs]! !

!NumericalComplexRootIsolator methodsFor: 'accessing' stamp: 'len 1/15/2016 05:57'!
tolerance
	^ tolerance! !

!NumericalComplexRootIsolator methodsFor: 'accessing' stamp: 'len 1/15/2016 05:53'!
tolerance: aNumber
	tolerance _ aNumber! !

!NumericalComplexRootIsolator methodsFor: 'initialization' stamp: 'len 1/15/2016 06:09'!
initialize
	super initialize.
	tolerance _ 0.1! !

!NumericalComplexRootIsolator methodsFor: 'private' stamp: 'len 1/13/2016 03:01'!
initialValue
	^ 1.3 + 0.314159 i! !

!NumericalComplexRootIsolator methodsFor: 'private' stamp: 'len 8/9/2020 07:46:58'!
roots
	| roots X P Q P2 Q2 n x v m c dx x1 v1 m1 realCoefficients |
	X _ polynomial parent x.
	P _ polynomial.
	Q _ P.
	P2 _ P derivative.
	Q2 _ P2.
	n _ P degree.
	roots _ OrderedCollection new: n.
	realCoefficients _ P coefficients allSatisfy: [:each| (each isKindOf: Number) or: [each imaginary isZero]].
	["Initialize root finding:"
	x _ self initialValue.
	v _ Q value: x.
	m _ v norm2.
	"Initialize recursion:"
	c _ 0. dx _ v / (Q2 value: x).
	[dx abs < tolerance]
		whileFalse:
			[x1 _ x - dx.
			v1 _ Q value: x1.
			m1 _ v1 norm2.
			m1 < m
				ifTrue: [x _ x1. v _ v1. m _ m1. c _ 0. dx _ v / (Q2 value: x)]
				ifFalse: [c _ c + 1. dx _ dx / 4. c > 20 ifTrue: [self error: 'failed']]].
	"Polish root:"
	2 timesRepeat: [x _ x - ((P value: x) / (P2 value: x))].
	"Divide:"
	((realCoefficients not or: [n = 1]) or: [x imaginary abs < tolerance "###"])
		ifTrue: "found one root"
			[x imaginary abs < tolerance ifTrue: [x _ x real]. "###"
			roots add: (ComplexDisk center: x radius: tolerance).
			Q _ Q // (X - x).
			Q2 _ Q derivative. "###"
			n _ n - 1]
		ifFalse: "found a root and (if the root is not real) also found its conjugate"
			[roots
				add: (ComplexDisk center: x radius: tolerance);
				add: (ComplexDisk center: x conjugate radius: tolerance).
			Q _ Q // (X^2 - (X * (x real * 2)) + x abs squared).
			Q2 _ Q derivative. "###"
			n _ n - 2].
	n > 0] whileTrue.
	^ roots! !

!NumericalComplexRootIsolator class methodsFor: 'instance creation' stamp: 'len 1/9/2022 08:38:42'!
polynomial: aPolynomial
	^ self squarefreePolynomial: aPolynomial // (aPolynomial gcd: aPolynomial derivative)! !

!NumericalComplexRootIsolator class methodsFor: 'instance creation' stamp: 'len 1/9/2022 08:38:08'!
squarefreePolynomial: aPolynomial
	^ super polynomial: aPolynomial! !

!RealInterval methodsFor: 'printing' stamp: 'len 12/13/97 03:23'!
printOn: aStream
	"Print a representation of the receiver in the stream aStream."

	aStream
		nextPut: (self isLeftOpen ifTrue: [ $( ] ifFalse: [ $[ ]);
		print: self left;
		nextPutAll: ', ';
		print: self right;
		nextPut: (self isRightOpen ifTrue: [ $) ] ifFalse: [ $] ])! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/19/97 22:10'!
dimension
	"Answer the dimension of the receiver."

	self isEmpty ifTrue: [^ Infinity negative].
	self left = self right ifTrue: [^ 0].
	^ 1! !

!RealInterval methodsFor: 'accessing' stamp: 'len 6/5/2016 06:26'!
halves
	| m |
	m _ self midpoint.
	^ {self copy right: m; rightOpen: false . self copy left: m; leftOpen: true}! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/13/97 03:22'!
inf
	"Answer the infimun of the receiver."

	^ self left! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/13/97 03:21'!
left
	"Answer the left bound of the receiver."

	^ left! !

!RealInterval methodsFor: 'accessing' stamp: 'len 1/16/2016 05:56'!
length
	^ self measure! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/13/97 03:24'!
max
	"Answer the maximum of the receiver."

	self isRightOpen ifTrue: [^ self error: 'there''s not maximum'].
	^ self right! !

!RealInterval methodsFor: 'accessing' stamp: 'len 1/11/2016 02:43'!
measure
	^ self right - self left! !

!RealInterval methodsFor: 'accessing' stamp: 'len 1/15/2016 07:19'!
midpoint
	^ left + right / 2! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/13/97 03:23'!
min
	"Answer the minimum of the receiver."

	self isLeftOpen ifTrue: [^ self error: 'there''s not minimun'].
	^ self left! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/13/97 03:21'!
right
	"Answer the right bound of the receiver."

	^ right! !

!RealInterval methodsFor: 'accessing' stamp: 'len 11/5/2016 09:58'!
sample
	"Answer a sample point in the receiver."

	self isEmpty ifTrue: [^ self error: 'empty interval'].
	self isLeftClosed ifTrue: [^ self left].
	self isRightClosed ifTrue: [^ self right].
	(self isLeftBounded and: [self isRightBounded])
		ifTrue: [^ self left + self right / 2].
	self isLeftBounded ifTrue: [^ self left + 1].
	self isRightBounded ifTrue: [^ self right - 1].
	^ 0! !

!RealInterval methodsFor: 'accessing' stamp: 'len 12/13/97 03:24'!
sup
	"Answer the supremum of the receiver."

	^ self right! !

!RealInterval methodsFor: 'accessing-private' stamp: 'len 12/13/97 03:18'!
left: aNumber
	left _ aNumber! !

!RealInterval methodsFor: 'accessing-private' stamp: 'len 12/13/97 02:30'!
leftOpen: aBoolean
	leftOpen _ aBoolean! !

!RealInterval methodsFor: 'accessing-private' stamp: 'len 12/13/97 03:21'!
right: aNumber
	right _ aNumber! !

!RealInterval methodsFor: 'accessing-private' stamp: 'len 12/13/97 02:32'!
rightOpen: aBoolean
	rightOpen _ aBoolean! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 12/13/97 03:19'!
* anInterval
	"Answer the product of the receiver by the argument."

	| l1l2 l1r2 r1l2 r1r2 |
	l1l2 _ self left * anInterval left.
	l1r2 _ self left * anInterval right.
	r1l2 _ self right * anInterval left.
	r1r2 _ self right * anInterval right.
	^ self copy
		left: ((l1l2 min: l1r2) min: (r1l2 min: r1r2));
		right: ((l1l2 max: l1r2) max: (r1l2 max: r1r2))! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 12/13/97 03:20'!
+ anInterval
	"Answer the sum of the receiver and the argument."

	^ self copy
		left: self left + anInterval left;
		right: self right + anInterval right! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 12/13/97 03:36'!
- anInterval
	"Answer the difference of the receiver and the argument."

	^ self + anInterval negated! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 12/13/97 02:14'!
/ anInterval
	"Answer the quotient of the receiver by the argument."

	^ self * anInterval reciprocal! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 12/13/97 03:20'!
negated
	"Answer the additive inverse of the receiver."

	^ self copy
		right: self left negated;
		left: self right negated;
		leftOpen: self isRightOpen;
		rightOpen: self isLeftOpen! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 1/16/2016 05:48'!
reciprocal
	"Answer the multiplicative inverse of the receiver."
	(self closureIncludes: 0) ifTrue: [^ ZeroDivide signal].
	^ self copy
		left: self right reciprocal;
		right: self left reciprocal! !

!RealInterval methodsFor: 'arithmetic' stamp: 'len 1/16/2016 06:58'!
squared
	^ self * self! !

!RealInterval methodsFor: 'comparing' stamp: 'len 1/16/2016 05:57'!
= anInterval
	^ left = anInterval left and: [right = anInterval right and: [self isLeftOpen = anInterval isLeftOpen and: [self isRightOpen = anInterval isRightOpen]]]! !

!RealInterval methodsFor: 'comparing' stamp: 'len 6/28/2016 09:30'!
hash
	^ self left hash hashMultiply + self right hash + self isRightOpen hash + self isLeftOpen hash hashMultiply! !

!RealInterval methodsFor: 'constants' stamp: 'len 12/13/97 03:20'!
identity
	"Answer the identity element for the receiver."

	^ self copy left: 1; right: 1! !

!RealInterval methodsFor: 'constants' stamp: 'len 12/13/97 03:20'!
null
	"Answer the null element for the receiver."

	^ self copy left: 0; right: 0! !

!RealInterval methodsFor: 'converting' stamp: 'len 12/13/97 02:31'!
close
	"Answer a closed inverval with the same bounds that the receiver."

	self isClosed ifFalse: [^ self copy leftOpen: false; rightOpen: false]! !

!RealInterval methodsFor: 'converting' stamp: 'len 12/13/97 02:40'!
closeLeft
	"Answer a closed left interval with the same bounds that the receiver."

	self isLeftClosed ifFalse: [^ self copy leftOpen: false]! !

!RealInterval methodsFor: 'converting' stamp: 'len 12/13/97 02:35'!
closeRight
	"Answer a closed right interval with the same bounds that the receiver."

	self isRightClosed ifFalse: [^ self copy rightOpen: false]! !

!RealInterval methodsFor: 'converting' stamp: 'len 12/13/97 02:32'!
open
	"Answer an open inverval with the same bounds that the receiver."

	self isOpen ifFalse: [^ self copy leftOpen: true; rightOpen: true]! !

!RealInterval methodsFor: 'converting' stamp: 'len 12/13/97 02:38'!
openLeft
	"Answer an open left interval with the same bounds that the receiver."

	self isLeftOpen ifFalse: [^ self copy leftOpen: true]! !

!RealInterval methodsFor: 'converting' stamp: 'len 12/13/97 02:39'!
openRight
	"Answer an open right interval with the same bounds that the receiver."

	self isRightOpen ifFalse: [^ self copy rightOpen: true]! !

!RealInterval methodsFor: 'initialization' stamp: 'len 12/13/97 03:18'!
setLeftBound: leftBound rightBound: rightBound leftOpen: leftOpenBool rightOpen: rightOpenBool
	left _ leftBound.
	right _ rightBound.
	"If the interval is left-closed or right-closed and two points are
	the same, then the interval is a single point... so it's closed."
	((leftOpenBool & rightOpenBool) not and: [left = right])
		ifTrue: [leftOpen _ false.
				rightOpen _ false]
		ifFalse: [leftOpen _ leftOpenBool.
				rightOpen _ rightOpenBool]! !

!RealInterval methodsFor: 'operations' stamp: 'len 1/16/2016 05:59'!
closure
	^ self class closed: left closed: right! !

!RealInterval methodsFor: 'operations' stamp: 'len 6/2/2016 07:29'!
interior
	^ self class open: left open: right! !

!RealInterval methodsFor: 'testing' stamp: 'len 3/28/2018 18:05:40'!
closureIncludes: anObject
	anObject isTuple ifTrue: [^ anObject size = 1 and: [self includes: anObject first]].
	^ anObject between: left and: right! !

!RealInterval methodsFor: 'testing' stamp: 'len 3/28/2018 18:05:25'!
includes: anObject
	anObject isTuple ifTrue: [^ anObject size = 1 and: [self includes: (anObject at: 1)]].
	(self left > anObject or: [self right < anObject])
		ifTrue: [^ false].
	(self left < anObject and: [self right > anObject])
		ifTrue: [^ true].
	((self isLeftClosed and: [anObject = self left])
	or: [self isRightClosed and: [anObject = self right]])
		ifTrue: [^ true].
	^ false! !

!RealInterval methodsFor: 'testing' stamp: 'len 6/4/2016 21:43'!
intersects: anInterval
	"Answer true if the receiver overlaps the argument."
	self right < anInterval left ifTrue: [^ false].
	self left > anInterval right ifTrue: [^ false].
	self right = anInterval left ifTrue: [^ self isRightClosed and: [anInterval isLeftClosed]].
	self left = anInterval right ifTrue: [^ self isLeftClosed and: [anInterval isRightClosed]].
	^ true! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 03:29'!
isBounded
	"Answer true if the receiver is bounded."

	^ self isLeftBounded and: [self isRightBounded]! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:36'!
isClosed
	"Answer true if the receiver is closed."

	^ self isLeftClosed and: [self isRightClosed]! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 03:23'!
isEmpty
	"Answer true if the receiver is empty."

	^ self isLeftBounded
		ifTrue: [self right < self left]
		ifFalse: [self left > self right]! !

!RealInterval methodsFor: 'testing' stamp: 'len 6/2/2016 05:16'!
isFinite
	^ left = right and: [leftOpen not | rightOpen not]! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:14'!
isHalfOpen
	"Answer true if the receiver is hald-open."

	^ self isClosed not and: [self isOpen not]! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:23'!
isLeftBounded
	"Answer true if the receiver is left bounded."

	^ self left isInfinity not! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:33'!
isLeftClosed
	"Answer true if the receiver is left closed."

	^ self isLeftOpen not! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:36'!
isLeftOpen
	"Answer true if the receiver is left open."

	^ leftOpen! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:38'!
isOpen
	"Answer true if the receiver is open."

	^ self isLeftOpen and: [self isRightOpen]! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:24'!
isRightBounded
	"Answer true if the receiver is right bounded."

	^ self right isInfinity not! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:35'!
isRightClosed
	"Answer true if the receiver is right closed."

	^ self isRightOpen not! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 02:37'!
isRightOpen
	"Answer true if the receiver is open right."

	^ rightOpen! !

!RealInterval methodsFor: 'testing' stamp: 'len 12/13/97 03:23'!
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self left > 0 ifTrue: [^ 1].
	self right < 0 ifTrue: [^ -1].
	^ 0! !

!RealInterval class methodsFor: 'instance creation' stamp: 'len 12/13/97 02:29'!
closed: left closed: right
	"Answer a new [left, right] interval."

	^ self new setLeftBound: left rightBound: right leftOpen: false rightOpen: false! !

!RealInterval class methodsFor: 'instance creation' stamp: 'len 12/13/97 02:30'!
closed: left open: right
	"Answer a new [left, right) interval."

	^ self new setLeftBound: left rightBound: right leftOpen: false rightOpen: true! !

!RealInterval class methodsFor: 'instance creation' stamp: 'len 12/13/97 02:30'!
open: left closed: right
	"Answer a new (left, right] interval."

	^ self new setLeftBound: left rightBound: right leftOpen: true rightOpen: false! !

!RealInterval class methodsFor: 'instance creation' stamp: 'len 12/13/97 02:30'!
open: left open: right
	"Answer a new (left, right) interval."

	^ self new setLeftBound: left rightBound: right leftOpen: true rightOpen: true! !

!RealRootIsolator methodsFor: 'accessing' stamp: 'len 11/18/97 00:38'!
fourier
	"Answer the Budan-Fourier sequence for the polynomial of the receiver."

	fourier isNil ifTrue: [self computeFourier].
	^ fourier! !

!RealRootIsolator methodsFor: 'accessing' stamp: 'len 4/24/2018 18:20:19'!
polynomial
	^ polynomial! !

!RealRootIsolator methodsFor: 'accessing' stamp: 'len 6/5/2016 04:47'!
roots
	"Answer a collection with the roots of the polynomial."
	roots isNil ifTrue: [self separateRoots].
	^ roots! !

!RealRootIsolator methodsFor: 'accessing' stamp: 'len 8/23/97 18:10'!
sturm
	"Answer a sturm sequence for the polynomial of the receiver."

	sturm isNil ifTrue: [self computeSturm].
	^ sturm! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 4/24/2018 17:59:22'!
lowerBoundNumberOfRootsBetween: leftBound and: rightBound
	"Answer a lower bound for the number of real roots in the open interval (leftBound, rightBound)."
	| answer last sign |
	answer _ 0.
	last _ 0.
	1 to: polynomial degree * 2 do: [:each|
		sign _ (polynomial value: (rightBound - leftBound) / each) sign.
		(last ~= sign and: [sign ~= 0]) ifTrue: [answer _ answer + 1].
		last _ sign].
	^ answer! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 1/16/2016 08:33'!
moreThanOneRootBetween: leftBound and: rightBound
	"Answer true if there are only one root in the interval (leftBound, rightBound]."

	| upper lower |
	sturm isNil "if the Sturm chain was not yet computed, first try some bounds and heuristics"
		ifTrue:
			[(upper _ self upperBoundNumberOfRootsBetween: leftBound and: rightBound) < 2
				ifTrue: [^ false].
			(upper even and: [self heuristicDetectOneRootBetween: leftBound and: rightBound])
				ifTrue: [^ true].
			lower _ self lowerBoundNumberOfRootsBetween: leftBound and: rightBound.
			lower > 1 ifTrue: [^ true].
			(upper even and: [lower > 0]) ifTrue: [^ true]].

	^ (self numberOfRootsBetween: leftBound and: rightBound) > 1! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 4/24/2018 17:59:16'!
numberOfNegativeRoots
	"Answer the number of negative roots."
	| fix |
	fix _ (polynomial value: 0) = 0 ifTrue: [1] ifFalse: [0].
	^ (self numberOfRootsBetween: self bigBounds min and: 0) - fix! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 1/15/2016 02:41'!
numberOfPositiveRoots
	"Answer the number of positive roots."
	^ self numberOfRootsBetween: 0 and: self bigBounds max! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 1/15/2016 02:41'!
numberOfRoots
	"Answer the number of real roots of the receiver."

	| interval |
	interval _ self bigBounds.
	^ self numberOfRootsBetween: interval min and: interval max! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 1/16/2016 08:03'!
numberOfRootsBetween: leftBound and: rightBound
	"Answer the number of real roots in the left open interval (leftBound, rightBound]."

	| leftSignChanges rightSignChanges a b |
	a _ leftBound asFraction.
	b _ rightBound asFraction.
	leftSignChanges _ 0.
	(self sturm collect: [ :each | (each value: a) sign])
		inject: 0 into: [ :lastSign :each |
			(lastSign ~= each and: [each ~= 0])
				ifTrue: [leftSignChanges _ leftSignChanges + 1].
			each].

	rightSignChanges _ 0.
	(self sturm collect: [ :each | (each value: b) sign])
		inject: 0 into: [ :lastSign :each |
			(lastSign ~= each and: [each ~= 0])
				ifTrue: [rightSignChanges _ rightSignChanges + 1].
			each].

	^ leftSignChanges - rightSignChanges! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 1/15/2016 02:42'!
upperBoundNumberOfRoots
	"Answer an upper bound for the number of real roots of the receiver."
	| interval |
	interval _ self bigBounds.
	^ self upperBoundNumberOfRootsBetween: interval min and: interval max! !

!RealRootIsolator methodsFor: 'couting roots' stamp: 'len 11/24/2015 06:10'!
upperBoundNumberOfRootsBetween: leftBound and: rightBound
	"Answer an upper bound for the number of real roots
	in the open interval (leftBound, rightBound)."

	| leftSignChanges rightSignChanges |

	leftSignChanges _ 0.
	(self fourier collect: [ :each | (each value: leftBound) sign])
		inject: 0 into: [ :lastSign :each |
			(lastSign ~= each and: [each ~= 0])
				ifTrue: [leftSignChanges _ leftSignChanges + 1].
			each].

	rightSignChanges _ 0.
	(self fourier collect: [ :each | (each value: rightBound) sign])
		inject: 0 into: [ :lastSign :each |
			(lastSign ~= each and: [each ~= 0])
				ifTrue: [rightSignChanges _ rightSignChanges + 1].
			each].

	^ leftSignChanges - rightSignChanges! !

!RealRootIsolator methodsFor: 'initialization' stamp: 'len 4/24/2018 18:02:05'!
polynomial: aPolynomial
	polynomial _ aPolynomial! !

!RealRootIsolator methodsFor: 'operations' stamp: 'len 4/29/2018 18:27:49'!
separateRoots
	"Separate the roots in intervals each of them including an isolated root."

	| bounds |
	roots _ SortedCollection sortBlock: [ :one :other | one left <= other left].
	bounds _ self bigBounds.
	self separateRootsBetween: bounds min and: bounds max! !

!RealRootIsolator methodsFor: 'operations' stamp: 'len 4/29/2018 18:28:56'!
separateRootsBetween: leftBound and: rightBound
	"Separate the roots in the real interval given by the arguments."
	| r mid |
	r _ self numberOfRootsBetween: leftBound and: rightBound.
	r = 0 ifTrue: [^ self].
	r = 1 ifTrue: [^ roots add: (RealInterval closed: leftBound closed: rightBound)].

	mid _ leftBound + rightBound / 2.0. "2 for exact rationals or 2.0 to use floats."
	self separateRootsBetween: leftBound and: mid.
	self separateRootsBetween: mid and: rightBound! !

!RealRootIsolator methodsFor: 'private' stamp: 'len 7/21/2019 08:52:22'!
bigBounds
	"Compute and answer a first estimate of an interval containing all the real roots."
	| bound |
	bound _ polynomial norm1 asFloat / polynomial leadingCoefficient asFloat.
	bound _ bound max: 1.0.
	^ RealInterval closed: bound negated closed: bound! !

!RealRootIsolator methodsFor: 'private' stamp: 'len 4/24/2018 17:59:04'!
computeFourier
	"Compute the Budan-Fourier sequence for the polynomial of the receiver."
	| f |
	f _ polynomial.
	fourier _ (OrderedCollection new: f degree + 1) add: f; yourself.
	f degree timesRepeat: [fourier add: (f _ f derivative)]! !

!RealRootIsolator methodsFor: 'private' stamp: 'len 4/24/2018 17:59:08'!
computeSturm
	"Compute a sturmian sequence for the receiver."
	| f g r |
	f _ polynomial.
	g _ f derivative.
	sturm _ (OrderedCollection new: f degree + 1) add: f; add: g; yourself.
	[g degree > 0]
		whileTrue:
			[r _ sturm add: (f \\ g) negated.
			f _ g.
			g _ r]! !

!RealRootIsolator methodsFor: 'private' stamp: 'len 4/24/2018 17:59:00'!
heuristicDetectOneRootBetween: leftBound and: rightBound
	| first |
	first _ polynomial value: leftBound.
	2 to: 10 do: [:each|
		first ~= (polynomial value: (rightBound - leftBound) / each) sign
			ifTrue: [^ true]].
	^ false! !

!RealRootIsolator class methodsFor: 'instance creation' stamp: 'len 4/24/2018 18:01:20'!
on: aPolynomial
	^ self new polynomial: aPolynomial! !

!RealRootIsolator class methodsFor: 'root finding' stamp: 'len 12/10/2022 11:19:06'!
allRootsOf: aPolynomial
	| answer |
	answer _ Bag new.
	aPolynomial integerPrimitivePart factorization withOccurrencesDo: [:f :e|
		f degree = 1
			ifTrue:
				[answer add: (f independentCoefficient negated / f leadingCoefficient) withOccurrences: e]
			ifFalse:
				[(RealRootIsolator on: f) roots do: [:each|
	 				answer add: (RealAlgebraicNumber minimalPolynomial: f bounds: each) withOccurrences: e]]].
	^ answer! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:55'!
testComplexAlgebraicNumberAddition
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ ComplexAlgebraicNumber sqrt: 2.
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:34:35'!
testComplexAlgebraicNumberDivision
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 / sqrt5 / sqrt3 / sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:34:48'!
testComplexAlgebraicNumberProduct
	| sqrt3 sqrt5 |
	sqrt3 _ ComplexAlgebraicNumber sqrt: 3.
	sqrt5 _ ComplexAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 * sqrt5 * sqrt3 * sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:28'!
testComplexAlgebraicNumberSquare
	| sqrt2 |
	sqrt2 _ ComplexAlgebraicNumber sqrt2.
	10 timesRepeat: [sqrt2 squared]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:01'!
testRealAlgebraicNumberAddition
	| sqrt2 sqrt3 sqrt5 |
	sqrt2 _ RealAlgebraicNumber sqrt: 2.
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 + sqrt5 + sqrt2 - sqrt3 - sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:06'!
testRealAlgebraicNumberDivision
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 / sqrt5 / sqrt3 / sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:11'!
testRealAlgebraicNumberProduct
	| sqrt3 sqrt5 |
	sqrt3 _ RealAlgebraicNumber sqrt: 3.
	sqrt5 _ RealAlgebraicNumber sqrt: 5.
	10 timesRepeat: [sqrt3 * sqrt5 * sqrt3 * sqrt5]! !

!AlgebraicNumbersBenchmark methodsFor: 'as yet unclassified' stamp: 'len 4/29/2018 20:33:15'!
testRealAlgebraicNumberSquare
	| sqrt2 |
	sqrt2 _ RealAlgebraicNumber sqrt2.
	10 timesRepeat: [sqrt2 squared]! !

!Fraction methodsFor: '*algebraic numbers' stamp: 'len 1/20/2018 19:17:36'!
adaptToAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (ComplexAlgebraicNumber fromRational: self)! !

!Fraction methodsFor: '*algebraic numbers' stamp: 'len 1/13/2016 03:55'!
adaptToRealAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (RealAlgebraicNumber fromRational: self)! !

!Fraction methodsFor: '*algebraic numbers' stamp: 'len 1/20/2018 19:17:29'!
squareRoot
	self positive ifTrue: [^ RealAlgebraicNumber sqrt: self].
	^ ComplexAlgebraicNumber sqrt: self! !

!Integer methodsFor: '*algebraic numbers' stamp: 'len 1/20/2018 19:17:45'!
adaptToAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (ComplexAlgebraicNumber fromRational: self)! !

!Integer methodsFor: '*algebraic numbers' stamp: 'len 3/3/2021 07:32:03'!
adaptToRealAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (RealAlgebraicNumber fromRational: self asRational)! !

!Integer methodsFor: '*algebraic numbers' stamp: 'len 1/20/2018 19:17:41'!
squareRoot
	self positive ifTrue: [^ RealAlgebraicNumber sqrt: self].
	^ ComplexAlgebraicNumber sqrt: self! !

!Rational methodsFor: '*algebraic numbers' stamp: 'len 7/21/2019 08:56:58'!
adaptToAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (ComplexAlgebraicNumber fromRational: self)! !

!Rational methodsFor: '*algebraic numbers' stamp: 'len 7/21/2019 08:57:06'!
adaptToRealAlgebraic: rcvr andSend: selector
	^ rcvr perform: selector with: (RealAlgebraicNumber fromRational: self)! !

!Rational methodsFor: '*algebraic numbers' stamp: 'len 7/19/2019 07:28:52'!
squareRoot
	self positive ifTrue: [^ RealAlgebraicNumber sqrt: self].
	^ ComplexAlgebraicNumber sqrt: self! !
