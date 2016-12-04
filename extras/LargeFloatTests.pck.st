'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 4 December 2016 at 9:58:45 pm'!
'Description Please enter a description for this package'!
!provides: 'LargeFloatTests' 1 0!
!classDefinition: #LargeFloatTest category: #LargeFloatTests!
TestCase subclass: #LargeFloatTest
	instanceVariableNames: 'zero one two half minusOne minusTwo huge'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LargeFloatTests'!
!classDefinition: 'LargeFloatTest class' category: #LargeFloatTests!
LargeFloatTest class
	instanceVariableNames: ''!


!LargeFloatTest commentStamp: 'nice 4/29/2006 01:28' prior: 0!
Test to check FloatingPoint numbers with arbitrary precision!

!LargeFloatTest methodsFor: 'private' stamp: 'nice 2/7/2015 02:41'!
checkDoublePrecision: y forFunction: func precision: n
	"Check that doubling the precision, then rounding would lead to the same result"
	
	| anLargeFloat singlePrecisionResult |
	anLargeFloat _ y asLargeFloatPrecision: n.
	singlePrecisionResult _ anLargeFloat perform: func.
	self checkThatEvaluatingFunction: func toDoublePrecisionOf: anLargeFloat equals: singlePrecisionResult.
	^singlePrecisionResult! !

!LargeFloatTest methodsFor: 'private' stamp: 'nice 2/7/2015 01:49'!
checkDoublePrecisionSerie: serie forFunction: func 
	^self checkDoublePrecisionSerie: serie forFunction: func precision: Float precision! !

!LargeFloatTest methodsFor: 'private' stamp: 'nice 2/7/2015 02:14'!
checkDoublePrecisionSerie: serie forFunction: func precision: n
	serie do: [:y | self checkDoublePrecision: y forFunction: func precision: n]! !

!LargeFloatTest methodsFor: 'private' stamp: 'nice 2/7/2015 02:43'!
checkDoublePrecisionSerieVsFloat: serie forFunction: func 
	^serie reject: [:y |
		| farb |
		farb _ self checkDoublePrecision: y forFunction: func precision: Float precision.
		[(y asFloat perform: func) = farb] on: ZeroDivide do: [false]]! !

!LargeFloatTest methodsFor: 'private' stamp: 'nice 2/7/2015 02:44'!
checkThatEvaluatingFunction: func toDoublePrecisionOf: anLargeFloat equals: singlePrecisionResult
	"Check that doubling the precision, then rounding would lead to the same result"
	
	| n doublePrecision doublePrecisionResult lowBits |
	n _ anLargeFloat precision.
	doublePrecision _ anLargeFloat asLargeFloatPrecision: n * 2.
	doublePrecisionResult _ doublePrecision perform: func.
	
	"Note: the test must be guarded against double rounding error condition.
	For example, suppose the single precision is 4 bits, double precision 8 bits.
	If exact result is 1.001 | 0111 | 1001...
	Then the nearest double is rounded to upper 1.001 | 1000
	Then the nearest single to the double is rounded to upper 1.010
	But the nearest single to the exact result should have been 1.001
	To avoid this, we have to check if the second rounding is an exact tie"
	doublePrecisionResult normalize.
	lowBits _ doublePrecisionResult mantissa bitAnd: 1<<n-1.
	lowBits = (1<<(n-1))
		ifTrue:
			["double precision is ambiguous - retry with quadruple..."
			^self checkThatEvaluatingFunction: func toQuadruplePrecisionOf: anLargeFloat equals: singlePrecisionResult].
	self assert: ((doublePrecisionResult asLargeFloatPrecision: n)- singlePrecisionResult) isZero
	
! !

!LargeFloatTest methodsFor: 'private' stamp: 'nice 2/7/2015 02:43'!
checkThatEvaluatingFunction: func toQuadruplePrecisionOf: anLargeFloat equals: singlePrecisionResult
	"Check that quadrupling the precision, then rounding would lead to the same result"
	
	| n quadruplePrecision quadruplePrecisionResult lowBits |
	n _ anLargeFloat precision.
	quadruplePrecision _ anLargeFloat asLargeFloatPrecision: n * 4.
	quadruplePrecisionResult _ quadruplePrecision perform: func.
	
	"Guard against double rounding error condition (exact tie)"
	quadruplePrecisionResult normalize.
	lowBits _ quadruplePrecisionResult mantissa bitAnd: 1<<(3*n)-1.
	lowBits = (1<<(3*n-1))
		ifTrue:
			["quadruple precision is ambiguous - give up..."
			^self].
	self assert: ((quadruplePrecisionResult asLargeFloatPrecision: n)- singlePrecisionResult) isZero.! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 11/13/2010 19:16'!
hyperbolicSerie
	^#(-3.0e0  -0.1e0  0.0e0  1.0e-20  1.0e-10  0.99e0 1.0e0  2.5e0  3.0e0  10.25e0) , (Array with: (3/10) asFloat with: (22/7) asFloat)! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 11/13/2010 19:17'!
inverseTrigonometricSerie
	^((-20 to: 20) collect: [:e | (e / 20) asFloat]) , ((-6 to: 6) collect: [:e | (e / 7) asFloat])! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 12/27/2010 19:38'!
largeTrigonometricSerie
	^#(1.0e15 1.1e21 1.2e28 1.0e32 1.1e34 -1.23e51 1.345e67 1.777e151 1.211e308)! !

!LargeFloatTest methodsFor: 'setup' stamp: 'nice 4/29/2006 01:43'!
setUp
	zero _ 0 asLargeFloatPrecision: 53.
	one _ 1 asLargeFloatPrecision: 53.
	two _ 2 asLargeFloatPrecision: 53.
	half _ 1/2 asLargeFloatPrecision: 53.
	minusOne _ -1 asLargeFloatPrecision: 53.
	minusTwo _ -2 asLargeFloatPrecision: 53.
	huge _ (10 raisedTo: 100) asLargeFloatPrecision: 53.! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 4/23/2011 00:59'!
testArCosh
	<timeout: 5 "seconds">
	| serie |
	serie _ ((1 to: 10) , #(1.0001 100 1000 1.0e20)) collect: [:e | e asFloat].
	self checkDoublePrecisionSerie: serie forFunction: #arCosh! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 3/3/2012 20:45'!
testArCoshDomainError
	self should: [(1/2 asLargeFloatPrecision: 24) arCosh] raise: DomainError.! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 4/23/2011 00:59'!
testArSinh
	<timeout: 10 "seconds">
	| serie |
	serie _ ((-5 to: 10) , #(1.0e-20 1.0e-10  0.9999 1.0001 100 1000 1.0e20)) collect: [:e | e asFloat].
	self checkDoublePrecisionSerie: serie forFunction: #arSinh! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 4/23/2011 01:00'!
testArTanh
	<timeout: 20 "seconds">
	| serie |
	serie _ ((-19 to: 19) collect: [:e | (e / 20) asFloat]) , ((-6 to: 6) collect: [:e | (e / 7) asFloat]) , #(1.0e-20 1.0e-10 0.99 0.9999 0.999999).
	self checkDoublePrecisionSerie: serie forFunction: #arTanh! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 3/3/2012 20:45'!
testArTanhDomainError
	self should: [(2 asLargeFloatPrecision: 24) arTanh] raise: DomainError.
	self should: [(-3 asLargeFloatPrecision: 24) arTanh] raise: DomainError.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 11/13/2010 19:21'!
testArcCos
	<timeout: 10 "seconds">
	| badArcCos |
	badArcCos _ self checkDoublePrecisionSerieVsFloat: self inverseTrigonometricSerie forFunction: #arcCos.
	badArcCos isEmpty ifFalse: [Transcript cr; show: 'bad arcCos for ' , badArcCos printString]! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 3/3/2012 20:45'!
testArcCosDomainError
	self should: [(2 asLargeFloatPrecision: 24) arcCos] raise: DomainError.
	self should: [(-3 asLargeFloatPrecision: 24) arcCos] raise: DomainError.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 11/13/2010 19:21'!
testArcSin
	<timeout: 10 "seconds">
	| badArcSin |
	badArcSin _ self checkDoublePrecisionSerieVsFloat: self inverseTrigonometricSerie forFunction: #arcSin.
	badArcSin isEmpty ifFalse: [Transcript cr; show: 'bad arcSin for ' , badArcSin printString]! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 3/3/2012 20:45'!
testArcSinDomainError
	self should: [(2 asLargeFloatPrecision: 24) arcSin] raise: DomainError.
	self should: [(-3 asLargeFloatPrecision: 24) arcSin] raise: DomainError.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 11/13/2010 19:20'!
testArcTan
	<timeout: 10 "seconds">
	| badArcTan serie |
	serie _ ((-50 to: 50) collect: [:e | (e / 10) asFloat]).
	badArcTan _ self checkDoublePrecisionSerieVsFloat: serie forFunction: #arcTan.
	badArcTan isEmpty ifFalse: [Transcript cr; show: 'bad arcTan for ' , badArcTan printString]! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 8/30/2011 03:05'!
testArcTan2
	<timeout: 30 "seconds">
	-5 to: 5 by: 4/10 do: [:y |
		| yf yd |
		yf _ y asLargeFloatPrecision: Float precision.
		yd _ yf asLargeFloatPrecision: Float precision * 2.
		-5 to: 5 by: 4/10 do: [:x |
			| xf xd  |
			xf _ x asLargeFloatPrecision: Float precision.
			xd _ xf asLargeFloatPrecision: Float precision * 2.
			self assert: ((yd arcTan: xd) asFloat - (yf arcTan: xf) asFloat) isZero]].! !

!LargeFloatTest methodsFor: 'testing-converting' stamp: 'nice 3/3/2012 20:40'!
testAsFloat
	self assert: (half asLargeFloatPrecision: Float precision) asFloat = 0.5e0.
	self assert: (half asLargeFloatPrecision: Float precision * 2) asFloat = 0.5e0.! !

!LargeFloatTest methodsFor: 'testing-converting' stamp: 'nice 3/3/2012 20:40'!
testAsFloatWithUnderflow
	| fmin fminA |
	fmin _ Float fmin.
	fminA _ fmin asLargeFloatPrecision: one precision.
	Float emin - Float precision + 1 to: Float emin + 1 do: [:n |
		self assert: ((one timesTwoPower: n) + fminA) asFloat = ((1.0e0 timesTwoPower: n) + fmin)].! !

!LargeFloatTest methodsFor: 'testing-converting' stamp: 'nice 7/26/2013 23:27'!
testAsMinimalDecimalFraction
	| emax emin leadingOne significands |
	significands _ 0 to: 1<<10-1.
	leadingOne _ 1<<10.
	emin _ -14.
	emax _ 15.
	
	"Test all normal finite half precision float"
	emin to: emax do: [:e | 
		significands do: [:s |
			| f |
			f _ (leadingOne + s asLargeFloatPrecision: 11) timesTwoPower: e - 10.
			self assert: (f asMinimalDecimalFraction asLargeFloatPrecision: 11) = f]].
	
	"Test all subnormal finite half precision float"
	significands do: [:s |
		| f |
		f _ (s asLargeFloatPrecision: s highBit) timesTwoPower: emin - 10.
		self assert: (f asMinimalDecimalFraction asLargeFloatPrecision: s highBit) = f].! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'len 12/4/2016 21:01:48'!
testCoercingDivide
	(Array with: 1/2 with: 0.5e0) do: [:heteroHalf |
		self assert: one / heteroHalf = two.
		self assert: (one / heteroHalf) class = one class.
		self assert: (one / heteroHalf) precision = one precision.
		self assert: heteroHalf / one = half.
		self assert: (heteroHalf / one) class = one class.
		self assert: (heteroHalf / one) precision = one precision].

	self assert: one / 2 = half.
	self assert: (one / 2) class = one class.
	self assert: (one / 2) precision = one precision.
	self assert: -2 / two = minusOne.
	self assert: (-2 / two) class = two class.
	self assert: (-2 / two) precision = two precision.! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'len 12/4/2016 21:02:17'!
testCoercingEqual
	self assert: half = (1/2).
	self assert: (1/2) = half.
	self deny: half = (1/3).
	self deny: (1/3) = half.

	self assert: two = 2.
	self assert: -2 = minusTwo.
	self deny: -3 = two.
	self deny: two = 3.

	self assert: half = (0.5e0).
	self assert: (0.5e0) = half.
	self deny: half = (0.33e0).
	self deny: (0.33e0) = half.
! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'len 12/4/2016 21:02:29'!
testCoercingLessThan
	self deny: half < (1/2).
	self assert: (1/3) < half.
	self assert: minusOne < (1/2).
	self deny: (1/3) < minusTwo.

	self assert: two < 3.
	self deny: two < 2.
	self deny: two < 1.
	self deny: two < -1.
	self assert:  minusTwo < -1.
	self assert:  minusTwo < 1.
	self deny: minusTwo < -2.
	self deny: minusTwo < -3.

	self deny: half < (0.5e0).
	self deny: half < (0.33e0).
	self assert: half < (0.66e0).
	self deny: (0.5e0) < half.
	self assert: (0.33e0) < half.
	self deny: (0.66e0) < half.
! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'len 12/4/2016 21:02:42'!
testCoercingMultiply
	(Array with: 1/2 with: 0.5e0) do: [:heteroHalf |
		self assert: two * heteroHalf = one.
		self assert: (two * heteroHalf) class = half class.
		self assert: (two * heteroHalf) precision = half precision.
		self assert: heteroHalf * two = one.
		self assert: (heteroHalf * two) class = half class.
		self assert: (heteroHalf * two) precision = half precision].

	self assert: minusOne * 2 = minusTwo.
	self assert: (minusOne * 2) class = minusOne class.
	self assert: (minusOne * 2) precision = minusOne precision.
	self assert: 2 * one = two.
	self assert: (2 * one) class = one class.
	self assert: (2 * one) precision = one precision.! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'len 12/4/2016 21:02:49'!
testCoercingSubtract
	(Array with: 1/2 with: 0.5e0) do: [:heteroHalf |
		self assert: half - heteroHalf = zero.
		self assert: (half - heteroHalf) class = half class.
		self assert: (half - heteroHalf) precision = half precision.
		self assert: heteroHalf - half = zero.
		self assert: (heteroHalf - half) class = half class.
		self assert: (heteroHalf - half) precision = half precision].

	self assert: one - 1 = zero.
	self assert: (one - 1) class = minusOne class.
	self assert: (one - 1) precision = minusOne precision.
	self assert: -2 - minusTwo = zero.
	self assert: (-2 - minusTwo) class = minusTwo class.
	self assert: (-2 - minusTwo) precision = minusTwo precision.! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'len 12/4/2016 21:02:54'!
testCoercingSum
	(Array with: 1/2 with: 0.5e0) do: [:heteroHalf |
		self assert: half + heteroHalf = one.
		self assert: (half + heteroHalf) class = half class.
		self assert: (half + heteroHalf) precision = half precision.
		self assert: heteroHalf + half = one.
		self assert: (heteroHalf + half) class = half class.
		self assert: (heteroHalf + half) precision = half precision].

	self assert: minusOne + 1 = zero.
	self assert: (minusOne + 1) class = minusOne class.
	self assert: (minusOne + 1) precision = minusOne precision.
	self assert: 2 + minusTwo = zero.
	self assert: (2 + minusTwo) class = minusTwo class.
	self assert: (2 + minusTwo) precision = minusTwo precision.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 8/30/2011 03:03'!
testCos
	<timeout: 30 "seconds">
	| badCos |
	badCos _ self checkDoublePrecisionSerieVsFloat: self trigonometricSerie forFunction: #cos.
	badCos isEmpty ifFalse: [Transcript cr; show: 'bad cos for angles (degrees) ' , (badCos collect: [:i | i radiansToDegrees rounded]) printString]! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 4/23/2011 01:00'!
testCosh
	<timeout: 10 "seconds">
	self checkDoublePrecisionSerie: self hyperbolicSerie forFunction: #cosh! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 11/13/2010 09:49'!
testDivide
	| serie |
	serie _ {1. 2. 3. 5. 6. 7. 9. 10. 11. 12. 19. 243. 10 raisedTo: Float precision + 1. Float precision factorial. Float pi.}.
	serie do: [:num |
		| nf na |
		nf _ num asFloat.
		na _ num asLargeFloatPrecision: Float precision.
		serie do:[:den |
			| df da ff fa |
			df _ den asFloat.
			da _ den asLargeFloatPrecision: Float precision.
			ff _ nf / df.
			fa _ na / da.
			self assert: ff = fa]].! !

!LargeFloatTest methodsFor: 'testing-compare' stamp: 'nice 3/18/2014 23:10'!
testEqual
	self assert: zero = zero.
	self assert: one = one.
	self assert: one = one copy.
	self assert: one = (one asLargeFloatPrecision: one precision * 2).

	self deny: zero = one.
	self deny: minusOne = one.

	self assert: zero = 0.
	self assert: 0 = zero.
	self assert: zero = 0.0.
	self assert: 0.0 = zero.

	self deny: two = (1/2).
	self deny: (1/2) = two.
	self deny: zero = 1.0.
	self deny: 0.0 = one.

	self deny: one = nil.
	self deny: nil = one.! !

!LargeFloatTest methodsFor: 'testing-functions' stamp: 'nice 11/13/2010 21:17'!
testExp
	<timeout: 10 "seconds">
	| badExp serie |
	serie _ ((-20 to: 20) collect: [:e |e asFloat]).
	badExp _ self checkDoublePrecisionSerieVsFloat: serie forFunction: #exp.
	badExp isEmpty ifFalse: [Transcript cr; show: 'bad exp for ' , badExp printString]! !

!LargeFloatTest methodsFor: 'testing-functions' stamp: 'WernerKassens 4/11/2016 19:24'!
testExpLn
	|n|
	self assert: (1 asLargeFloatPrecision: 53) exp asFloat = 1 asFloat exp.
	n _ 5 exp.
	self assert: ((5 asLargeFloatPrecision: 53) exp - n)abs <= n ulp.
	"self assert: (5 asLargeFloatPrecision: 53) exp asFloat = 5 asFloat exp."
	self assert: ((5 asLargeFloatPrecision: 53) exp ln asFloat - n ln)abs <= 5.0 ulp.
	"this test was skipped. changed that & loosened 2. test,
	 since '5 exp' seems to round up instead of down here,
	 which results in an error of almost one ulp in '5 exp'"! !

!LargeFloatTest methodsFor: 'testing-compare' stamp: 'nice 3/3/2012 20:44'!
testGreaterThan
	
	self assert: zero < one.
	self deny: one > two.
	self deny: two > huge.
	self deny: minusOne > one.
	self deny: minusTwo > minusOne.
	self deny: minusTwo > huge.
	
	self assert: huge > one.
	self assert: huge > zero.
	self assert: huge > minusOne.
	self assert: one > minusOne.
	self assert: minusOne > minusTwo.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 4/30/2006 02:49'!
testIEEEArithmeticVersusFloat
	| floats ops ref new |
	floats _ #(1.0 2.0 3.0 5.0 10.0 2r1.0e52 2r1.0e53 2r1.0e54 0.5 0.25 2r1.0e-52 2r1.0e-53 2r1.0e-54 1.0e60 0.1 1.1e-30 1.0e-60) copyWith: Float pi.
	ops _ #(#+ #- #* #/ #= #< #> ).
	ops
		do: [:op | floats
				do: [:f1 | floats
						do: [:f2 | 
							ref _ f1 perform: op with: f2.
							new _ (f1 asLargeFloatPrecision: 53)
										perform: op
										with: (f2 asLargeFloatPrecision: 53).
							self assert: new = ref]]]! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'len 12/4/2016 21:05:32'!
testIEEEArithmeticVersusIntegerAndFraction
	"check that results are the same as IEEE 754 accelerated hardware
	WARNING: this cannot be the case for denormalized numbers (gradual underflow)
	because our exponent is unlimited"

	| floats ops ref new intAndFractions |
	floats _ #(1.0e0 2.0e0 3.0e0 5.0e0 10.0e0) 
				, (#(52 53 54 -52 -53 -54) collect: [:e | 1.0e0 timesTwoPower: e]) 
					, #(0.5e0 0.25e0 1.0e60 0.1e0 1.1e-30 1.0e-60) copyWith: Float pi.
	intAndFractions _ #(1 3 5 10 12345678901234567890 -1 -22 -3) copyWith: 7/9.
	intAndFractions _ intAndFractions , (intAndFractions collect: [:e | e reciprocal]).
	
	ops _ 1/10 = 0.1
		ifTrue: [#(#+ #- #* #/)]
		ifFalse: [#(#+ #- #* #/ #= #< #>)]. "BEWARE: LargeFloat compare exactly, Float don't, unless http://bugs.squeak.org/view.php?id=3374"
	ops do: 
			[:op | 
			floats do: 
					[:f1 | 
					intAndFractions do: 
							[:f2 | 
							ref _ f1 perform: op with: f2 asFloat.
							new _ (f1 asLargeFloatPrecision: 53) perform: op
										with: (f2 asLargeFloatPrecision: 53).
							self assert: new = ref.
							new _ f1 perform: op
										with: (f2 asLargeFloatPrecision: 53).
							self assert: new = ref.
							
							ref _ f1 perform: op with: f2.
							new _ (f1 asLargeFloatPrecision: 53) perform: op
										with: f2.
							self assert: new = ref.
							
							ref _ f2 asFloat perform: op with: f1.
							new _ (f2 asLargeFloatPrecision: 53) perform: op
										with: (f1 asLargeFloatPrecision: 53).
							self assert: new = ref.
							new _ (f2 asLargeFloatPrecision: 53) perform: op with: f1.
							self assert: new = ref.
							
							ref _ f2 perform: op with: f1.
							new _ f2 perform: op
										with: (f1 asLargeFloatPrecision: 53).
							self assert: new = ref]]]! !

!LargeFloatTest methodsFor: 'testing-coercing' stamp: 'WernerKassens 4/11/2016 15:58'!
testInfinityAndNaN
	| inf nan |
	inf _ Float infinity.
	nan _ Float nan.
	self assert: inf + two equals: inf.
	self assert: half + inf negated equals: inf negated.	
	self assert: (nan + minusOne)  isNaN .
	self assert: inf - huge equals: inf.
	self assert: half - inf equals: inf negated.
	self assert: minusTwo - inf negated equals: inf.
	self assert: (one - nan) isNaN.
	self assert: inf * two equals: inf.
	self assert: minusOne * inf equals: inf negated.
	self assert: inf negated * minusOne equals: inf.
	self assert: (huge * nan) isNaN.
	self assert: inf negated / minusTwo equals: inf.
	self assert: zero / inf negated equals: 0.	
	self assert: one / inf equals: 0.
	self should: [inf / zero] raise: ZeroDivide.	
	self assert: (nan  / two) isNaN.	
	self assert: (inf raisedTo: huge) equals: inf.
	self assert: (huge raisedTo: inf) equals: inf.
	self assert: (nan raisedTo: two) isNaN.
	self assert: (two raisedTo: nan) isNaN.
	self deny: nan <= one.
	self deny: zero >= nan.
	self assert: one < inf.
	self assert: zero ~= nan.
	self deny: nan = one.! !

!LargeFloatTest methodsFor: 'testing-compare' stamp: 'nice 4/29/2006 01:44'!
testIsZero
	self assert: zero isZero.
	self deny: one isZero.
	self deny: minusTwo isZero.! !

!LargeFloatTest methodsFor: 'testing-compare' stamp: 'nice 4/29/2006 01:45'!
testLessThan
	
	self assert: zero < one.
	self assert: one < two.
	self assert: two < huge.
	self assert: minusOne < one.
	self assert: minusTwo < minusOne.
	self assert: minusTwo < huge.
	
	self deny: huge < one.
	self deny: huge < zero.
	self deny: huge < minusOne.
	self deny: one < minusOne.
	self deny: minusOne < minusTwo.! !

!LargeFloatTest methodsFor: 'testing-functions' stamp: 'nice 11/13/2010 21:17'!
testLn
	<timeout: 10 "seconds">
	| badLn serie |
	serie _ ((1 to: 100) collect: [:e |e asFloat]).
	badLn _ self checkDoublePrecisionSerieVsFloat: serie forFunction: #ln.
	badLn isEmpty ifFalse: [Transcript cr; show: 'bad ln for ' , badLn printString]! !

!LargeFloatTest methodsFor: 'testing-functions' stamp: 'nice 3/3/2012 20:44'!
testLnDomainError
	self should: [(-2 asLargeFloatPrecision: 24) ln] raise: DomainError.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 4/29/2006 03:21'!
testMultiply
	self assert: zero * zero = zero.
	self assert: zero * minusOne = zero.
	self assert: huge * zero = zero.
	self assert: one * zero = zero.
	
	self assert: one * two = two.
	self assert: minusOne * one = minusOne.
	self assert: minusOne * minusTwo = two.
	
	self assert: half * two = one.
	
	"check rounding"
	self assert: huge * one = huge.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 4/29/2006 02:29'!
testNegated
	self assert: zero negated = zero.
	self assert: one negated = minusOne.
	self assert: minusTwo negated = two.
	self assert: huge negated negated = huge.
! !

!LargeFloatTest methodsFor: 'testing-compare' stamp: 'nice 4/5/2008 02:13'!
testNegative
	
	self deny: zero negative.
	self deny: two negative.
	self assert: minusTwo negative.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 2/10/2007 01:03'!
testPi
	"check computation of pi"

	self assert: (1 asLargeFloatPrecision: 53) pi = Float pi.! !

!LargeFloatTest methodsFor: 'testing-compare' stamp: 'nice 4/5/2008 02:13'!
testPositive
	
	self assert: zero positive.
	self assert: one positive.
	self deny: minusOne positive.! !

!LargeFloatTest methodsFor: 'testing-converting' stamp: 'nice 3/18/2014 23:24'!
testPrintAndEvaluate
	<timeout: 50 "seconds">
	| emax emin leadingOne significands |
	significands _ 0 to: 1<<10-1.
	leadingOne _ 1<<10.
	emin _ -14.
	emax _ 15.
	
	"Test all normal finite half precision float"
	emin to: emax do: [:e | 
		significands do: [:s |
			| f |
			f _ (leadingOne + s asLargeFloatPrecision: 11) timesTwoPower: e - 10.
			self assert: (Compiler evaluate: f storeString) = f.
			self assert: (Compiler evaluate: f printString) = f.]].
	
	"Test all subnormal finite half precision float"
	significands do: [:s |
		| f |
		f _ (s asLargeFloatPrecision: s highBit) timesTwoPower: emin - 10.
		self assert: (Compiler evaluate: f storeString) = f.
		self assert: (Compiler evaluate: f printString) = f].! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 2/7/2015 01:38'!
testRaisedToNegativeInteger
	| n |
	n _ 11.
	1 to: 1<<n-1 do: [:i |
		self assert: ((i asLargeFloatPrecision: n) raisedToInteger: -49)
			equals: ((i raisedToInteger: -49) asLargeFloatPrecision: n) ].! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 2/7/2015 01:37'!
testRaisedToPositiveInteger
	| n |
	n _ 11.
	1 to: 1<<n-1 do: [:i |
		self assert: ((i asLargeFloatPrecision: n) raisedToInteger: 49)
			equals: ((i raisedToInteger: 49) asLargeFloatPrecision: n) ].! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 7/18/2014 13:29'!
testReciprocal
	| b |
	b _ 1 << (Float precision - 1).
	1 to: 10000 do: [:i |
		| a |
		a _ i asLargeFloatPrecision: Float precision.
		self assert: a reciprocal = i asFloat reciprocal.
		self assert: (a+b) reciprocal = (i+b) asFloat reciprocal.
		self assert: a negated reciprocal = i asFloat negated reciprocal.]! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 2/7/2007 23:15'!
testRoundToNearestEven
	"Check that IEEE default rounding mode is honoured,
	that is rounding to nearest even"
		
	self assert: ((one timesTwoPower: 52)+(0+(1/4))) asTrueFraction = ((1 bitShift: 52)+0).
	self assert: ((one timesTwoPower: 52)+(0+(1/2))) asTrueFraction = ((1 bitShift: 52)+0).
	self assert: ((one timesTwoPower: 52)+(0+(3/4))) asTrueFraction = ((1 bitShift: 52)+1).
	self assert: ((one timesTwoPower: 52)+(1+(1/4))) asTrueFraction = ((1 bitShift: 52)+1).
	self assert: ((one timesTwoPower: 52)+(1+(1/2))) asTrueFraction = ((1 bitShift: 52)+2).
	self assert: ((one timesTwoPower: 52)+(1+(3/4))) asTrueFraction = ((1 bitShift: 52)+2).! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 2/7/2007 23:16'!
testRoundToNearestEvenAgainstIEEEDouble
	"Check that IEEE default rounding mode is honoured"

	#(1 2 3 5 6 7) do: 
			[:i | 
			self assert: ((one timesTwoPower: 52) + (i / 4)) asTrueFraction 
						= ((1 asFloat timesTwoPower: 52) + (i / 4)) asTrueFraction.
			self assert: ((one timesTwoPower: 52) - (i / 4)) asTrueFraction 
						= ((1 asFloat timesTwoPower: 52) - (i / 4)) asTrueFraction]! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 8/30/2011 03:04'!
testSin
	<timeout: 30 "seconds">
	| badSin |
	badSin _ self checkDoublePrecisionSerieVsFloat: self trigonometricSerie forFunction: #sin.
	badSin isEmpty ifFalse: [Transcript cr; show: 'bad sin for angles (degrees) ' , (badSin collect: [:i | i radiansToDegrees rounded]) printString]! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 9/27/2011 20:18'!
testSincos
	<timeout: 30 "seconds">
	self trigonometricSerie do: [:aFloat |
		| x sc s c |
		x _ aFloat asLargeFloatPrecision: 53.
		sc _ x sincos.
		s _ x sin.
		c _ x cos.
		self assert: sc size = 2.

		self assert: sc first = s.
		self assert: sc last = c]! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 4/23/2011 01:00'!
testSinh
	<timeout: 10 "seconds">
	self checkDoublePrecisionSerie: self hyperbolicSerie forFunction: #sinh! !

!LargeFloatTest methodsFor: 'testing-functions' stamp: 'nice 11/14/2010 14:05'!
testSqrt
	<timeout: 10 "seconds">
	| badSqrt serie |
	"knowing that (10**3) < (2**10), 100 bits are enough for representing 10**30 exactly"
	self assert: ((10 raisedTo: 30) asLargeFloatPrecision: 100) sqrt = (10 raisedTo: 15).

	serie _ ((0 to: 20) collect: [:e | e asFloat]) , ((2 to: 20) collect: [:e | e reciprocal asFloat]).
	badSqrt _ self checkDoublePrecisionSerieVsFloat: serie forFunction: #sqrt.
	badSqrt isEmpty ifFalse: [Transcript cr; show: 'bad sqrt for ' , badSqrt printString]! !

!LargeFloatTest methodsFor: 'testing-functions' stamp: 'nice 3/3/2012 20:44'!
testSqrtDomainError
	self should: [(-2 asLargeFloatPrecision: 24) sqrt] raise: DomainError.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 11/13/2010 09:51'!
testSubtract
	self assert: zero - zero = zero.
	self assert: zero - minusOne = one.
	self assert: huge - zero = huge.
	self assert: one - zero = one.
	
	self assert: one - minusOne = two.
	self assert: minusOne - minusTwo = one.
	self assert: minusOne - one = minusTwo.
	
	"check rounding"
	self assert: huge - one = huge.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 4/29/2006 01:49'!
testSum
	self assert: zero + zero = zero.
	self assert: zero + minusOne = minusOne.
	self assert: huge + zero = huge.
	self assert: one + zero = one.
	
	self assert: one + minusOne = zero.
	self assert: minusOne + two = one.
	self assert: one + minusTwo = minusOne.
	
	"check rounding"
	self assert: huge + one = huge.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 8/30/2011 03:04'!
testTan
	<timeout: 30 "seconds">
	| badTan |
	badTan _ self checkDoublePrecisionSerieVsFloat: self trigonometricSerie forFunction: #tan.
	badTan isEmpty ifFalse: [Transcript cr; show: 'bad tan for angles (degrees) ' , (badTan collect: [:i | i radiansToDegrees rounded]) printString]! !

!LargeFloatTest methodsFor: 'testing-hyperbolic' stamp: 'nice 4/23/2011 01:01'!
testTanh
	<timeout: 10 "seconds">
	self checkDoublePrecisionSerie: self hyperbolicSerie forFunction: #tanh! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 12/27/2010 19:38'!
testVeryLargeCos
	<timeout: 10 "seconds">
	self checkDoublePrecisionSerie: self largeTrigonometricSerie forFunction: #cos.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 12/27/2010 19:38'!
testVeryLargeSin
	<timeout: 10 "seconds">
	self checkDoublePrecisionSerie: self largeTrigonometricSerie forFunction: #sin.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 12/27/2010 19:38'!
testVeryLargeTan
	<timeout: 10 "seconds">
	self checkDoublePrecisionSerie: self largeTrigonometricSerie forFunction: #tan.! !

!LargeFloatTest methodsFor: 'testing-arithmetic' stamp: 'nice 11/13/2010 21:18'!
testZeroOne

	self assert: (312 asLargeFloatPrecision: 53) one = 1.
	self assert: (312 asLargeFloatPrecision: 24) zero isZero.

	self assert: (312 asLargeFloatPrecision: 53) one asInteger = 1.
	self assert: (312 asLargeFloatPrecision: 24) zero asInteger isZero.! !

!LargeFloatTest methodsFor: 'testing-trigonometry' stamp: 'nice 11/13/2010 15:57'!
trigonometricSerie
	^(-720 to: 720) collect: [:i | i asFloat degreesToRadians]! !
