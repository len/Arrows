'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 4 December 2016 at 9:58:48 pm'!
'Description Please enter a description for this package'!
!provides: 'LargeFloat' 1 0!
!classDefinition: #LargeFloat category: #LargeFloat!
Number subclass: #LargeFloat
	instanceVariableNames: 'mantissa biasedExponent precision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LargeFloat'!
!classDefinition: 'LargeFloat class' category: #LargeFloat!
LargeFloat class
	instanceVariableNames: ''!


!LargeFloat commentStamp: 'nice 2/7/2007 23:08' prior: 0!
I store floating point numbers in base 2 with some arbitrary precision (arbitrary number of bits).
I do inexact arithmetic like Float.
But I am very slow due to emulated (Large) Integer arithmetic... (compared to IEEE 754 hardwired)

Unlike Float, mantissa is not normalized under the form 1.mmmmmm
It is just stored as an integer.
The sign is stored in the mantissa.
biasedExponent is the power of two that multiply the mantissa to form the number. there is no limitation of exponent (overflow or underflow), unless you succeed in exhausting the VM memory...

Like Float, my arithmetic operations are inexact. They will round to nearest precision LargeFloat.

If two different precisions are used in arithmetic, the result is expressed in the higher precision.

Default operating mode is rounding, but might be one of the other possibility (truncate floor ceiling).

Instance Variables:
	mantissa	<Integer>	the bits of mantissa including sign
	biasedExponent	<Integer>	the times two power to multiply the mantissa (floating binary scale)
	precision	<Magnitude>	number of bits to be stored in mantissa when i am normalized
!

!Number methodsFor: '*LargeFloat' stamp: 'len 12/4/2016 20:55:42'!
adaptToLargeFloat: rcvr andSend: selector
	^ (self isInfinite or: [self isNaN])
		ifTrue: [rcvr asFloat perform: selector with: self]
		ifFalse: [rcvr perform: selector with: (self asLargeFloatPrecision: rcvr precision)]! !

!Float methodsFor: '*LargeFloat' stamp: 'len 12/4/2016 14:22:46'!
asLargeFloatPrecision: anInteger
	self isZero ifTrue: [^ 0 asLargeFloatPrecision: anInteger].
	^ self partValues: [:sign :exponent :mantissa| LargeFloat mantissa: mantissa * sign exponent: (exponent max: -1022) - 52 precision: anInteger]! !

!Fraction methodsFor: '*LargeFloat' stamp: 'nice 7/25/2013 23:08'!
asLargeFloatPrecision: n
	"Answer a Floating point with arbitrary precision
	close to the receiver."

	"Note: form below would not be the closest approximation
	^ (numerator asLargeFloatPrecision: n)
		inPlaceDivideBy: (denominator asLargeFloatPrecision: n)"

	| a b mantissa exponent nBits ha hb hm hasTruncatedBits |
	a _ numerator abs.
	b _ denominator abs.
	ha _ a highBit.
	hb _ b highBit.

	"If both numerator and denominator are represented exactly in floating point number,
	then fastest thing to do is to use hardwired float division"
	nBits _ n + 1.
	(ha < nBits and: [hb < nBits]) 
		ifTrue: 
			[^(numerator asLargeFloatPrecision: n) 
				inPlaceDivideBy: (denominator asLargeFloatPrecision: n)].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n+1 bits.
	First guess is rough, the mantissa might have n+2 bits."
	exponent _ ha - hb - nBits.
	exponent > 0 
		ifTrue: [b _ b bitShift: exponent]
		ifFalse: [a _ a bitShift: exponent negated].
	mantissa _ a quo: b.
	hasTruncatedBits _ a > (mantissa * b).
	hm _ mantissa highBit.

	"Remove excess bits in the mantissa."
	hm > nBits 
		ifTrue: 
			[exponent _ exponent + hm - nBits.
			hasTruncatedBits _ hasTruncatedBits or: [mantissa anyBitOfMagnitudeFrom: 1 to: hm - nBits].
			mantissa _ mantissa bitShift: nBits - hm].

	"Check if mantissa must be rounded upward.
	The case of tie (mantissa odd & hasTruncatedBits not)
	will be handled by Integer>>asLargeFloatPrecision:."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa _ mantissa + 1].

	"build the LargeFloat from mantissa and exponent"
	^(self positive 
		ifTrue: [mantissa asLargeFloatPrecision: n]
		ifFalse: [mantissa negated asLargeFloatPrecision: n]) 
			inPlaceTimesTwoPower: exponent! !

!Integer methodsFor: '*LargeFloat' stamp: 'len 12/4/2016 14:23:42'!
asLargeFloatPrecision: anInteger
	^ LargeFloat mantissa: self exponent: 0 precision: anInteger! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 7/30/2013 01:45'!
* aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToLargeFloat: self andSend: #*].
	n _ precision max: aNumber precision.
	^ (self asLargeFloatPrecision: n)
		copy inPlaceMultiplyBy: (aNumber asLargeFloatPrecision: n)! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 7/30/2013 01:45'!
+ aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToLargeFloat: self andSend: #+].
	n _ precision max: aNumber precision.
	^ (self asLargeFloatPrecision: n)
		copy inPlaceAdd: (aNumber asLargeFloatPrecision: n)! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 7/30/2013 01:45'!
- aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToLargeFloat: self andSend: #-].
	n _ precision max: aNumber precision.
	^ (self asLargeFloatPrecision: n)
		copy inPlaceSubtract: (aNumber asLargeFloatPrecision: n)! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 7/30/2013 01:45'!
/ aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber adaptToLargeFloat: self andSend: #/].
	n _ precision max: aNumber precision.
	^ (self asLargeFloatPrecision: n)
		copy inPlaceDivideBy: (aNumber asLargeFloatPrecision: n)! !

!LargeFloat methodsFor: 'comparing' stamp: 'len 12/4/2016 20:56:10'!
< aNumber
	aNumber class = self class ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self digitCompare: aNumber) > 0]
						ifFalse: [^ (self digitCompare: aNumber) < 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToLargeFloat: self andSend: #<! !

!LargeFloat methodsFor: 'comparing' stamp: 'len 12/4/2016 20:56:17'!
= aNumber
	aNumber isNumber ifFalse: [^ false].
	aNumber class = self class ifTrue:
		[aNumber negative == self negative
			ifTrue: [^ (self digitCompare: aNumber) = 0]
			ifFalse: [^ false]].
	^ aNumber adaptToLargeFloat: self andSend: #=! !

!LargeFloat methodsFor: 'comparing' stamp: 'len 12/4/2016 20:56:24'!
> aNumber
	aNumber class = self class ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) < 0]
						ifFalse: [^(self digitCompare: aNumber) > 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToLargeFloat: self andSend: #>! !

!LargeFloat methodsFor: 'printing' stamp: 'nice 7/25/2013 22:48'!
absPrintExactlyOn: aStream base: base
	"Print my value on a stream in the given base. 
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version guarantees that the printed representation exactly represents my value
	by using exact integer arithmetic."

	| fBase significand exp baseExpEstimate r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount shead slowbit |
	fBase _ base asFloat.
	self normalize.
	significand _ mantissa abs.
	roundingIncludesLimits _ significand even.
	exp _ biasedExponent.
	baseExpEstimate _ (self exponent * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[significand isPowerOfTwo
				ifTrue:
					[r _ significand bitShift: 2 + exp.
					s _ 4.
					mPlus _ 2 * (mMinus _ 1 bitShift: exp)]
				ifFalse:
					[r _ significand bitShift: 1 + exp.
					s _ 2.
					mPlus _ mMinus _ 1 bitShift: exp]]
		ifFalse:
			[significand isPowerOfTwo
				ifTrue:
					[r _ significand bitShift: 2.
					s _ 1 bitShift: 2 - exp.
					mPlus _ 2.
					mMinus _ 1]
				ifFalse:
					[r _ significand bitShift: 1.
					s _ 1 bitShift: 1 - exp.
					mPlus _ mMinus _ 1]].
	baseExpEstimate >= 0
		ifTrue: [s _ s * (base raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale _ base raisedToInteger: baseExpEstimate negated.
			r _ r * scale.
			mPlus _ mPlus * scale.
			mMinus _ mMinus * scale].
	((r + mPlus >= s) and: [roundingIncludesLimits or: [r + mPlus > s]])
		ifTrue: [baseExpEstimate _ baseExpEstimate + 1]
		ifFalse:
			[r _ r * base.
			mPlus _ mPlus * base.
			mMinus _ mMinus * base].
	(fixedFormat _ baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount _ baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount _ 1]. 
	slowbit _ 1 - s lowBit .
	shead _ s bitShift: slowbit.
	[d _ (r bitShift: slowbit) // shead.
	r _ r - (d * s).
	(tc1 _ (r <= mMinus) and: [roundingIncludesLimits or: [r < mMinus]]) |
	(tc2 _ (r + mPlus >= s) and: [roundingIncludesLimits or: [r + mPlus > s]])] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r _ r * base.
		mPlus _ mPlus * base.
		mMinus _ mMinus * base.
		decPointCount _ decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[(tc1 not or: [r * 2 >= s]) ifTrue: [d _ d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]! !

!LargeFloat methodsFor: 'converting' stamp: 'WernerKassens 4/11/2016 09:27'!
adaptToFloat: rcvr andCompare: selector
	^ (rcvr isInfinite or: [ rcvr isNaN ])
		ifTrue: [ rcvr perform: selector with: self asFloat ]
		ifFalse: [ (rcvr asLargeFloatPrecision: 53) perform: selector with: self ]! !

!LargeFloat methodsFor: 'converting' stamp: 'WernerKassens 4/11/2016 09:28'!
adaptToFloat: rcvr andSend: selector
	^ (rcvr isInfinite or: [ rcvr isNaN ])
		ifTrue: [ rcvr perform: selector with: self asFloat ]
		ifFalse: [ (rcvr asLargeFloatPrecision: 53) perform: selector with: self ]! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 4/29/2006 01:18'!
adaptToFraction: rcvr andCompare: selector 
	"If I am involved in comparison with a Fraction, convert myself to a
	Fraction. This way, no bit is lost and comparison is exact."

	^ rcvr perform: selector with: self asTrueFraction! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 4/29/2006 00:09'!
adaptToFraction: rcvr andSend: selector
	^(rcvr asLargeFloatPrecision: precision) perform: selector with: self! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 4/29/2006 01:19'!
adaptToInteger: rcvr andCompare: selector 
	"If I am involved in comparison with an Integer, convert myself to a
	Fraction. This way, no bit is lost and comparison is exact."

	^ rcvr perform: selector with: self asTrueFraction! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 4/29/2006 00:09'!
adaptToInteger: rcvr andSend: selector
	^(rcvr asLargeFloatPrecision: precision) perform: selector with: self! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/10/2007 00:57'!
agm: aNumber 
	"Answer the arithmetic geometric mean of self and aNumber"

	| a b am gm |
	a _ self.
	b _ aNumber.
	
	[am _ a + b timesTwoPower: -1.	"am is arithmetic mean"
	gm _ (a * b) sqrt.	"gm is geometric mean"
	a = am or: [b = gm]] 
			whileFalse: 
				[a _ am.
				b _ gm].
	^am! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 02:43'!
arCosh
	"Evaluate the area hyperbolic cosine of the receiver."

	| arCosh x one y two |
	x _ self asLargeFloatPrecision: 16 + precision.
	one _ x one.
	x < one ifTrue: [DomainError signal: 'cannot compute arCosh of a number less than 1'].
	x = one ifTrue: [^self zero].
	y _ x - one.
	y < one
		ifTrue:
			[y exponent * -4 >= precision
				ifTrue: [arCosh _ (y powerExpansionArCoshp1Precision: y precision) * (y timesTwoPower: 1) sqrt]
				ifFalse:
					[two _ one timesTwoPower: 1.
					arCosh _ ((y * (y + two)) sqrt + y + one) ln]]
		ifFalse: [arCosh _ ((x squared - one) sqrt + x) ln].
	^arCosh asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 01:53'!
arSinh
	"Evaluate the area hyperbolic sine of the receiver."

	| arSinh x one |
	self isZero ifTrue: [^self].
	self exponent negated > precision ifTrue: [^self].
	x _ self asLargeFloatPrecision: 16 + precision.
	x inPlaceAbs.
	self exponent * -4 >= precision
		ifTrue: [arSinh _ x powerExpansionArSinhPrecision: x precision]
		ifFalse:
			[one _ x one.
			arSinh _ ((x squared + one) sqrt + x) ln].
	self negative ifTrue: [arSinh inPlaceNegated].
	^arSinh asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 00:24'!
arTanh
	"Evaluate the area hyperbolic tangent of the receiver."

	| arTanh x one |
	self isZero ifTrue: [^self].
	x _ self asLargeFloatPrecision: 16 + precision.
	x inPlaceAbs.
	one _ x one.
	x >= one ifTrue: [DomainError signal: 'cannot evaluate arTanh of number of magnitude >= 1'].
	self exponent * -4 >= precision
		ifTrue: [arTanh _ x powerExpansionArTanhPrecision: x precision]
		ifFalse:
			[arTanh _ ((one + x) / (one - x)) ln.
			arTanh inPlaceTimesTwoPower: -1].
	self negative ifTrue: [arTanh inPlaceNegated].
	^arTanh asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/8/2015 20:57'!
arcCos
	"Evaluate the arc cosine of the receiver."

	| arcCos x one |
	self isZero ifTrue: [^(self pi timesTwoPower: -1)].
	x _ self asLargeFloatPrecision: 16 + precision.
	x inPlaceAbs.
	one _ x one.
	x > one ifTrue: [DomainError signal: 'cannot compute arcCos of a number greater than 1'].
	arcCos _ x = one
		ifTrue: [self zero]
		ifFalse: [((one - x squared) sqrt / x) arcTan].
	self negative ifTrue: [arcCos _ x pi - arcCos].
	^arcCos asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 01:57'!
arcSin
	"Evaluate the arc sine of the receiver."

	| arcSin x one |
	self isZero ifTrue: [^self].
	x _ self asLargeFloatPrecision: 16 + precision.
	x inPlaceAbs.
	one _ x one.
	x > one ifTrue: [DomainError signal: 'cannot compute arcSin of a number greater than 1'].
	arcSin _ x = one
		ifTrue: [self pi timesTwoPower: -1]
		ifFalse: [self exponent * -4 >= precision
			ifTrue: [x powerExpansionArcSinPrecision: x precision]
			ifFalse: [(x / (one - x squared) sqrt) arcTan]].
	self negative ifTrue: [arcSin inPlaceNegated].
	^arcSin asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/8/2015 23:25'!
arcTan
	"Evaluate the arc tangent of the receiver."

	| x arcTan one power |
	self isZero ifTrue: [^self].
	self > 1
		ifTrue:
			[x _ self asLargeFloatPrecision: precision * 2 + 2.
			x inPlaceAbs.
			arcTan _ (x pi timesTwoPower: -1) - x reciprocal arcTan]
		ifFalse:
			[power _ ((precision bitShift: -1) + self exponent max: 4) highBit.
			x _ self asLargeFloatPrecision: precision + (1 bitShift: 1 + power).
			x inPlaceAbs.
			one _ x one.
			power timesRepeat: [x _ x / (one + (one + x squared) sqrt)].
			arcTan _ x powerExpansionArcTanPrecision: x precision + 6.
			arcTan inPlaceTimesTwoPower: power].
	self negative ifTrue: [arcTan inPlaceNegated].
	^arcTan asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 11/12/2010 22:21'!
arcTan: denominator
	"Evaluate the four quadrant arc tangent of the argument denominator (x) and the receiver (y)."

	^self isZero
		ifTrue: [denominator sign positive
			ifTrue: [ (self + denominator) zero ]
			ifFalse: [ self positive
				ifTrue: [ (self + denominator) pi ]
				ifFalse: [ (self + denominator) pi negated ]]]
		ifFalse: [denominator isZero
			ifTrue: [self positive
				ifTrue: [ (self + denominator) pi timesTwoPower: -1 ]
				ifFalse: [ (self + denominator) pi negated timesTwoPower: -1 ]]
			ifFalse:
				[ | precision arcTan |
				precision _ (self + denominator) precision.
				arcTan _ ((self asLargeFloatPrecision: precision * 2) / (denominator asLargeFloatPrecision: precision * 2)) arcTan.
				^(denominator > 0
					ifTrue: [ arcTan ]
					ifFalse: [ self > 0
						ifTrue: [ arcTan + arcTan pi ]
						ifFalse: [ arcTan - arcTan pi ]]) asLargeFloatPrecision: precision]]! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 3/19/2014 01:09'!
asFloat
	"Convert to a IEEE 754 double precision floating point."
	
	precision > Float precision ifTrue: [^(self copy setPrecisionTo: Float precision) asFloat].
	^mantissa asFloat timesTwoPower: biasedExponent! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 8/30/2011 01:20'!
asFraction
	^self asTrueFraction! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 4/28/2006 23:19'!
asLargeFloatPrecision: n 
	^ precision = n
		ifTrue: [self]
		ifFalse: [self copy setPrecisionTo: n]! !

!LargeFloat methodsFor: 'printing' stamp: 'nice 3/16/2014 22:55'!
asMinimalDecimalFraction
	"Answer the shortest decimal Fraction that will equal self when converted back asFloat.
	A decimal Fraction has only powers of 2 and 5 as decnominator.
	For example, 0.1 asMinimalDecimalFraction = (1/10)."

	| significand exp baseExpEstimate r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount shead slowbit numerator denominator |
	self isZero ifTrue: [^0].
	self negative ifTrue: [^self negated asMinimalDecimalFraction negated].
	self normalize.
	significand _ mantissa abs.
	roundingIncludesLimits _ significand even.
	exp _ biasedExponent.
	baseExpEstimate _ (self exponent * 10.0 reciprocalLogBase2 - 1.0e-10) ceiling.
	numerator _ 0.
	denominator _ 0.
	exp >= 0
		ifTrue:
			[significand isPowerOfTwo
				ifTrue:
					[r _ significand bitShift: 2 + exp.
					s _ 4.
					mPlus _ 2 * (mMinus _ 1 bitShift: exp)]
				ifFalse:
					[r _ significand bitShift: 1 + exp.
					s _ 2.
					mPlus _ mMinus _ 1 bitShift: exp]]
		ifFalse:
			[significand isPowerOfTwo
				ifTrue:
					[r _ significand bitShift: 2.
					s _ 1 bitShift: 2 - exp.
					mPlus _ 2.
					mMinus _ 1]
				ifFalse:
					[r _ significand bitShift: 1.
					s _ 1 bitShift: 1 - exp.
					mPlus _ mMinus _ 1]].
	baseExpEstimate >= 0
		ifTrue: [s _ s * (10 raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale _ 10 raisedToInteger: baseExpEstimate negated.
			r _ r * scale.
			mPlus _ mPlus * scale.
			mMinus _ mMinus * scale].
	((r + mPlus >= s) and: [roundingIncludesLimits or: [r + mPlus > s]])
		ifTrue: [baseExpEstimate _ baseExpEstimate + 1]
		ifFalse:
			[r _ r * 10.
			mPlus _ mPlus * 10.
			mMinus _ mMinus * 10].
	(fixedFormat _ baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount _ baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [denominator _ 10 raisedTo: baseExpEstimate negated]]
		ifFalse:
			[decPointCount _ 1]. 
	slowbit _ 1 - s lowBit .
	shead _ s bitShift: slowbit.
	[d _ (r bitShift: slowbit) // shead.
	r _ r - (d * s).
	(tc1 _ (r <= mMinus) and: [roundingIncludesLimits or: [r < mMinus]]) |
	(tc2 _ (r + mPlus >= s) and: [roundingIncludesLimits or: [r + mPlus > s]])] whileFalse:
		[numerator _ 10 * numerator + d.
		denominator _ 10 * denominator.
		r _ r * 10.
		mPlus _ mPlus * 10.
		mMinus _ mMinus * 10.
		decPointCount _ decPointCount - 1.
		decPointCount = 0 ifTrue: [denominator _ 1]].
	tc2 ifTrue:
		[(tc1 not or: [r * 2 >= s]) ifTrue: [d _ d + 1]].
	numerator _ 10 * numerator + d.
	denominator _ 10 * denominator.
	decPointCount > 0
		ifTrue:
			[numerator _ (10 raisedTo: decPointCount - 1) * numerator].
			fixedFormat ifFalse:
				[(baseExpEstimate - 1) > 0
					ifTrue: [numerator _ (10 raisedTo: baseExpEstimate - 1) * numerator]
					ifFalse: [denominator _ (10 raisedTo: 1 - baseExpEstimate) * (denominator max: 1)]].
			denominator < 2 ifTrue: [^numerator].
	^numerator / denominator! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 2/7/2007 22:21'!
asTrueFraction

	"First remove lowBits from mantissa.
	This can save a useless division and prevent gcd: cost"
	self reduce.
	
	^ biasedExponent >= 0
		ifTrue: [self shift: mantissa by: biasedExponent]
		ifFalse: [
			"Now avoid a painfull GCD: algorihm.
			mantissa is odd and cannot be reduced by a power of two.
				mantissa / (1 bitShift: exponent negated)"
			^ Fraction numerator: mantissa denominator: (1 bitShift: biasedExponent negated)]! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 2/7/2007 22:23'!
biasedExponent
	^biasedExponent! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'len 12/4/2016 20:44:10'!
cos
	"Evaluate the cosine of the receiver"
	
	| pi halfPi quarterPi x neg |
	x _ self moduloNegPiToPi.
	x inPlaceAbs.
	pi _ self piDoublePrecision.
	halfPi _ pi timesTwoPower: -1.
	(neg _ x > halfPi) ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi _ halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			x _ self sin: x]
		ifFalse: [x := self cos: x].
	neg ifTrue: [x inPlaceNegated].
	^x asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'private' stamp: 'len 12/4/2016 20:39:49'!
cos: x
	"Evaluate the cosine of x by recursive cos(2x) formula and power series expansion.
	Note that it is better to use this method with x <= pi/4."
	
	| one cos fraction power |
	x isZero ifTrue: [^x one].
	power _ ((precision bitShift: -1) + x exponent max: 0) highBit.
	fraction _ x timesTwoPower: power negated.
	cos _ fraction powerExpansionCosPrecision: precision + (1 bitShift: 1 + power).
	one _ x one.
	power timesRepeat:
		["Evaluate cos(2x)=2 cos(x)^2-1"
		cos inPlaceMultiplyBy: cos; inPlaceTimesTwoPower: 1; inPlaceSubtract: one].
	^cos! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 00:38'!
cosh
	| e x |
	self isZero ifTrue: [^self one].
	self exponent negated > precision ifTrue: [^self one].
	x _ self asLargeFloatPrecision: precision + 16.
	self exponent * -4 >= precision
		ifTrue: [^(x powerExpansionCoshPrecision: x precision) asLargeFloatPrecision: precision].
	e _ x exp.
	^e
		inPlaceAdd: e reciprocal;
		inPlaceTimesTwoPower: -1;
		asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'accessing' stamp: 'SergeStinckwich 6/28/2015 21:30'!
decimalPrecision
	^ precision * (2 log: 10)! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 14:48'!
digitCompare: b 
	"both are positive or negative.
	answer +1 if i am of greater magnitude, -1 if i am of smaller magnitude, 0 if equal magnitude"
	
	| compare |
	self isZero
		ifTrue: [b isZero
				ifTrue: [^ 0]
				ifFalse: [^ -1]].
	b isZero
		ifTrue: [^ 1].
	compare _ (self exponent - b exponent) sign.
	^ compare = 0
		ifTrue: [(self abs - b abs) sign]
		ifFalse: [compare]! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/10/2007 22:58'!
exp
	"Answer the exponential of the receiver."

	| ln2 x q r ri res n maxIter p one two |
	one _ self one.
	two _ one timesTwoPower: 1.
	"Use following decomposition:
		x exp = (2 ln * q + r) exp.
		x exp = (2**q * r exp)"
	ln2 _ two ln.
	x _ self / ln2.
	q _ x truncated.
	r _ (x - q) * ln2.

	"now compute r exp by power series expansion
	we compute (r/(2**p)) exp ** (2**p) in order to have faster convergence"
	p _ 10 min: precision // 2.
	r _ r timesTwoPower: p negated.
	ri _ one asLargeFloatPrecision: precision + 16.
	res _ ri copy.
	n _ 0.
	maxIter _ 1 + ((precision + 16) / p) ceiling.
	[n <= maxIter] whileTrue: 
			[n _ n + 1.
			ri inPlaceMultiplyBy: r / n.
			res inPlaceAdd: ri].
	p timesRepeat: [res inPlaceMultiplyBy: res].
	res inPlaceTimesTwoPower: q.

	"now use a Newton iteration to refine the result
	res = res * (self - res ln + 1)"
	[| oldres delta |
	oldres _ res.
	res _ res asLargeFloatPrecision: res precision + 32.
	res inPlaceMultiplyBy: self - res ln + 1.
	delta _ (res - oldres) exponent.
	delta = 0 or: [delta <= (res exponent - precision - 8)]] 
			whileFalse.
	
	^res asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 2/7/2007 22:24'!
exponent
	"anwser the floating point like exponent e,
	of self normalized as
	1.mmmmmm * (2 raisedTo: e)"
	
	self isZero ifTrue: [^0].
	^biasedExponent + self precisionInMantissa - 1! !

!LargeFloat methodsFor: 'comparing' stamp: 'nice 4/29/2006 01:07'!
hash
	"Hash is reimplemented because = is implemented."
	
	^ self asTrueFraction hash! !

!LargeFloat methodsFor: 'private' stamp: 'nice 11/1/2010 18:41'!
inPlaceAbs
	mantissa _ mantissa abs! !

!LargeFloat methodsFor: 'private' stamp: 'nice 4/5/2008 02:01'!
inPlaceAdd: b 
	| delta |
	b isZero ifTrue: [^self round].
	self isZero 
		ifTrue: 
			[mantissa _ b mantissa.
			biasedExponent _ b biasedExponent]
		ifFalse: 
			[biasedExponent = b biasedExponent 
				ifTrue: [mantissa _ mantissa + b mantissa]
				ifFalse: 
					["check for early truncation. beware, keep 2 bits for rounding"

					delta _ self exponent - b exponent.
					delta - 2 > (precision max: self precisionInMantissa) 
						ifFalse: 
							[delta negated - 2 > (precision max: b precisionInMantissa) 
								ifTrue: 
									[mantissa _ b mantissa.
									biasedExponent _ b biasedExponent]
								ifFalse: 
									[delta _ biasedExponent - b biasedExponent.
									delta > 0 
										ifTrue: 
											[mantissa _ (self shift: mantissa by: delta) + b mantissa.
											biasedExponent _ biasedExponent - delta]
										ifFalse: [mantissa _ mantissa + (self shift: b mantissa by: delta negated)]]]]].
	self round! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:34'!
inPlaceAddNoRound: b 
	| delta |
	b isZero ifTrue: [^self].
	self isZero 
		ifTrue: 
			[mantissa _ b mantissa.
			biasedExponent _ b biasedExponent]
		ifFalse: 
			[delta _ biasedExponent - b biasedExponent.
			delta isZero 
				ifTrue: [mantissa _ mantissa + b mantissa]
				ifFalse: 
					[delta > 0 
						ifTrue: 
							[mantissa _ (self shift: mantissa by: delta) + b mantissa.
							biasedExponent _ biasedExponent - delta]
						ifFalse: [mantissa _ mantissa + (self shift: b mantissa by: delta negated)]]]! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:44'!
inPlaceCopy: b 
	"copy another arbitrary precision float into self"

	mantissa _ b mantissa.
	biasedExponent _ b biasedExponent.
	precision _ b precision! !

!LargeFloat methodsFor: 'private' stamp: 'nice 8/29/2011 15:26'!
inPlaceDivideBy: y 
	"Reference: Accelerating Correctly Rounded
	Floating-Point Division when the Divisor
	Is Known in Advance - Nicolas Brisebarre,
	Jean-Michel Muller, Member, IEEE, and
	Saurabh Kumar Raina -
	http://perso.ens-lyon.fr/jean-michel.muller/DivIEEETC-aug04.pdf"

	| zh x q |
	zh _ y reciprocal reduce.
	x _ self copy.
	self inPlaceMultiplyBy: zh.
	q _ self copy.
	"r _ "self inPlaceMultiplyBy: y negated andAccumulate: x.
	"q' _ "self inPlaceMultiplyBy: zh andAccumulate: q.

	"ALGO 4
	| zh r zl |
	zh _ b reciprocal.
	r _ b negated inPlaceMultiplyBy: zh andAccumulate: (1 asLargeFloatPrecision: precision).
	zl _ (b asLargeFloatPrecision: precision + 1) reciprocal inPlaceMultiplyBy: r.
	self inPlaceMultiplyBy: zh andAccumulate: (zl inPlaceMultiplyBy: self)"! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:44'!
inPlaceMultiplyBy: b
	self inPlaceMultiplyNoRoundBy: b.
	self round! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:42'!
inPlaceMultiplyBy: b andAccumulate: c 
	"only do rounding after the two operations.
	This is the traditional muladd operation in aritmetic units"
	
	self inPlaceMultiplyNoRoundBy: b.
	self inPlaceAdd: c! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:39'!
inPlaceMultiplyNoRoundBy: b
	mantissa _ mantissa * b mantissa.
	biasedExponent _ biasedExponent + b biasedExponent.! !

!LargeFloat methodsFor: 'private' stamp: 'nice 4/29/2006 02:26'!
inPlaceNegated
	mantissa _ mantissa negated! !

!LargeFloat methodsFor: 'private' stamp: 'nice 7/18/2014 13:22'!
inPlaceReciprocal
	| ma h |
	self isZero ifTrue: [(ZeroDivide dividend: self) signal].
	ma _ mantissa abs.
	h _ ma highBit.
	mantissa _ (1 bitShift: h + precision) + ma quo: (self shift: mantissa by: 1).
	biasedExponent _ biasedExponent negated - h - precision + 1.
	self round
	
	"Implementation notes: if m is a power of 2, reciprocal is trivial.
	Else, we have 2^h > m >2^(h-1)
	thus 1 < 2^h/m < 2.
	thus 2^(n-1) < 2^(h+n-1)/m < 2^n
	We thus have to evaluate (2^(h+n-1)/m) rounded
	Tie is away from zero because there are always trailing bits (inexact op)
	(num/den) rounded is also ((num/den)+(sign/2)) truncated
	or (num*2)+(sign*den) quo: den*2
	That's finally what we evaluate"! !

!LargeFloat methodsFor: 'private' stamp: 'nice 4/22/2011 02:29'!
inPlaceSqrt
	"Replace the receiver by its square root."

	| guess guessSquared delta shift |
	self negative 
		ifTrue: 
			[^ DomainError signal: 'sqrt undefined for number less than zero.'].
	self isZero ifTrue: [^self].

	shift _ 2 * precision - mantissa highBit.
	biasedExponent _ biasedExponent - shift.
	biasedExponent odd
		ifTrue:
			[shift _ shift + 1.
			biasedExponent _ biasedExponent - 1].
	mantissa _ mantissa bitShift: shift.
	guess _ mantissa bitShift: mantissa highBit + 1 // 2.
	[
		guessSquared _ guess * guess.
		delta _ guessSquared - mantissa quo: (guess bitShift: 1).
		delta = 0 ] whileFalse:
			[ guess _ guess - delta ].
	guessSquared = mantissa
		ifFalse:
			[(guessSquared - guess - mantissa) negative ifFalse: [guess _ guess - 1]].
	mantissa _ guess.
	biasedExponent _ biasedExponent quo: 2.
	self round	! !

!LargeFloat methodsFor: 'private' stamp: 'nice 4/5/2008 02:01'!
inPlaceSubtract: b 
	| delta |
	b isZero ifTrue: [^self round].
	self isZero 
		ifTrue: 
			[mantissa _ b mantissa negated.
			biasedExponent _ b biasedExponent]
		ifFalse: 
			[biasedExponent = b biasedExponent
				ifTrue: [mantissa _ mantissa - b mantissa]
				ifFalse: 
					["check for early truncation. beware, keep 2 bits for rounding"

					delta _ self exponent - b exponent.
					delta - 2 > (precision max: self precisionInMantissa) 
						ifFalse: 
							[delta negated - 2 > (precision max: b precisionInMantissa) 
								ifTrue: 
									[mantissa _ b mantissa negated.
									biasedExponent _ b biasedExponent]
								ifFalse: 
									[delta _ biasedExponent - b biasedExponent.
									delta >= 0 
										ifTrue: 
											[mantissa _ (self shift: mantissa by: delta) - b mantissa.
											biasedExponent _ biasedExponent - delta]
										ifFalse: [mantissa _ mantissa - (self shift: b mantissa by: delta negated)]]]]].
	self round! !

!LargeFloat methodsFor: 'private' stamp: 'nice 4/4/2008 22:25'!
inPlaceSubtractNoRound: b 
	| delta |
	b isZero ifTrue: [^self].
	self isZero 
		ifTrue: 
			[mantissa _ b mantissa negated.
			biasedExponent _ b biasedExponent]
		ifFalse: 
			[delta _ biasedExponent - b biasedExponent.
			delta isZero 
				ifTrue: [mantissa _ mantissa - b mantissa]
				ifFalse: 
					[delta >= 0 
						ifTrue: 
							[mantissa _ (self shift: mantissa by: delta) - b mantissa.
							biasedExponent _ biasedExponent - delta]
						ifFalse: [mantissa _ mantissa - (self shift: b mantissa by: delta negated)]]]! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:16'!
inPlaceTimesTwoPower: n 
	self isZero
		ifFalse: [biasedExponent _ biasedExponent + n]! !

!LargeFloat methodsFor: 'testing' stamp: 'nice 5/21/2014 02:49'!
isAnExactFloat
	^self exponent <= Float emax
		and: [Float emin - Float precision < self exponent
		and: [precision <= Float precision or: [mantissa isAnExactFloat]]]! !

!LargeFloat methodsFor: 'testing' stamp: 'nice 4/29/2006 00:56'!
isZero
	^mantissa isZero! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 00:24'!
ln
	"Answer the neperian logarithm of the receiver."

	| x4 one two p res selfHighRes prec e |
	self <= self zero ifTrue: [DomainError signal: 'ln is only defined for x > 0.0'].
	
	one _ self one.
	self = one ifTrue: [^self zero].
	two _ one timesTwoPower: 1.

	"Use Salamin algorithm (approximation is good if x is big enough)
		x ln = Pi  / (2 * (1 agm: 4/x) ).
	If x not big enough, compute (x timesTwoPower: p) ln - (2 ln * p)
	if x is close to 1, better use a power expansion"
	prec _ precision + 16.
	e _ self exponent.
	e < 0 ifTrue: [e _ -1 - e].
	e > prec
		ifTrue: [p _ 0]
		ifFalse:
			[p _ prec - e.
			prec _ prec + p highBit].
	selfHighRes _ self asLargeFloatPrecision: prec.
	(selfHighRes - one) exponent * -4 >= precision ifTrue: [^(selfHighRes powerExpansionLnPrecision: prec) asLargeFloatPrecision: precision].
	self < one ifTrue: [selfHighRes inPlaceReciprocal].	"Use ln(1/x) => - ln(x)"
	x4 _ (4 asLargeFloatPrecision: prec) 
				inPlaceDivideBy: selfHighRes;
				inPlaceTimesTwoPower: p negated.
	res _ selfHighRes pi / (one agm: x4) timesTwoPower: -1.
	res _ selfHighRes = two 
		ifTrue: [res / (p + 1)]
		ifFalse: [p = 0 ifTrue: [res] ifFalse: [res - ((two asLargeFloatPrecision: prec) ln * p)]].
	self < one ifTrue: [res inPlaceNegated].
	^res asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 4/28/2006 22:38'!
mantissa
	^mantissa! !

!LargeFloat methodsFor: 'initialization' stamp: 'nice 2/7/2007 22:15'!
mantissa: m exponent: e precision: n 
	mantissa _ m.
	biasedExponent _ e.
	precision _ n.
	self round! !

!LargeFloat methodsFor: 'private' stamp: 'nice 11/1/2010 18:40'!
moduloNegPiToPi
	"answer a copy of the receiver modulo 2*pi, with doubled precision"

	| x pi twoPi quo |
	x _ (self asLargeFloatPrecision: precision * 2).
	self negative ifTrue: [x inPlaceNegated].
	pi _ x pi.
	twoPi _ pi timesTwoPower: 1.
	x > pi ifTrue:
		[quo _ x + pi quo: twoPi.
		quo highBitOfMagnitude > precision ifTrue:
			[x _ (self abs asLargeFloatPrecision: precision + quo highBitOfMagnitude).
			pi _ x pi.
			twoPi _ pi timesTwoPower: 1.
			quo _ x + pi quo: twoPi].
		x inPlaceSubtract: twoPi * quo.
		self negative ifTrue: [x inPlaceNegated]].
	^x asLargeFloatPrecision: precision * 2! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 2/7/2015 00:40'!
naiveRaisedToInteger: n
	"Very naive algorithm: use full precision.
	Use only for small n"
	| m e |
	m _ mantissa raisedToInteger: n. 
	e _ biasedExponent * n.
	^(m asLargeFloatPrecision: precision) timesTwoPower: e
	! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 4/29/2006 02:26'!
negated
	^self copy inPlaceNegated! !

!LargeFloat methodsFor: 'testing' stamp: 'nice 4/29/2006 03:15'!
negative
	^mantissa negative! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 7/30/2013 01:12'!
nextToward: aNumber 
	"answer the nearest floating point number to self with same precision than self,
	toward the direction of aNumber argument.
	If the nearest one falls on the other side of aNumber, than answer a Number"

	| next |

	"if self is greater, decrease self, but never under aNumber"
	self > aNumber 
		ifTrue: 
			[next _ self predecessor.
			^next >= aNumber 
				ifTrue: [next]
				ifFalse: [aNumber]].

	"if self is smaller, increase self, but never above aNumber"
	self < aNumber 
		ifTrue: [next _ self successor.
			^next <= aNumber 
				ifTrue: [next]
				ifFalse: [aNumber]].

	"if we are equal, return self"
	^self! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/7/2007 22:46'!
normalize
	"normalize the receiver.
	a normalized floating point is either 0, or has mantissa highBit = precision"
	
	| delta |
	mantissa isZero
		ifTrue: [biasedExponent _ 0]
		ifFalse: 
			[self round.
			delta _ self precisionInMantissa - precision.
			delta < 0 
				ifTrue: 
					[mantissa _ self shift: mantissa by: delta negated.
					biasedExponent _ biasedExponent + delta]]! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 2/10/2007 00:56'!
one
	^self class 
		mantissa: 1
		exponent: 0
		precision: precision! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 4/4/2008 22:24'!
pi
	"answer the value of pi rounded to precision.
	Note: use the Brent-Salamin Arithmetic Geometric Mean algorithm"

	| a b c k pi oldpi oldExpo expo |
	a _ self one asLargeFloatPrecision: precision + 16.
	b _ (a timesTwoPower: 1) sqrt reciprocal.
	c _ a timesTwoPower: -1.
	k _ 1.
	oldpi _ Float pi.
	oldExpo _ 2.
	
	[| am gm a2 |
	am _ a + b timesTwoPower: -1.
	gm _ (a * b) sqrt.
	a _ am.
	b _ gm.
	a2 _ a squared.
	c inPlaceSubtract: (a2 - b squared timesTwoPower: k).
	pi _ (a2 timesTwoPower: 1) / c.
	expo _ (oldpi - pi) exponent.
	expo isZero or: [expo > oldExpo or: [expo < (-1 - precision)]]] 
			whileFalse: 
				[oldpi _ pi.
				oldExpo _ expo.
				k _ k + 1].
	^pi asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'len 12/4/2016 21:54:12'!
piDoublePrecision
	^ (self class mantissa: 0 exponent: 0 precision: precision + 1 * 2) pi! !

!LargeFloat methodsFor: 'testing' stamp: 'nice 4/29/2006 03:15'!
positive
	^mantissa positive! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 02:58'!
powerExpansionArCoshp1Precision: precBits
	"Evaluate arcosh(x+1)/sqrt(2*x) for the receiver x by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| one two count count2 sum term term1 term2 |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	count2 _ one copy.
	sum _ one copy.
	term1 _ one copy.
	term2 _ one copy.
	
	[term1 inPlaceMultiplyBy: self.
	term1 inPlaceNegated.
	term2 inPlaceMultiplyBy: count2.
	term2 inPlaceMultiplyBy: count2.
	term2 inPlaceDivideBy: count.
	count inPlaceAdd: one.
	count2 inPlaceAdd: two.
	term2 inPlaceDivideBy: count2.
	term2 inPlaceTimesTwoPower: -2.
	term _ term1 * term2.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 01:49'!
powerExpansionArSinhPrecision: precBits
	"Evaluate the area hypebolic sine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| one x2 two count sum term |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	sum _ one copy.
	term _ one copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceMultiplyBy: count.
	term inPlaceDivideBy: count + one.
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term / count.
	term exponent + precBits < sum exponent] whileFalse.
	sum inPlaceMultiplyBy: self.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 01:13'!
powerExpansionArTanhPrecision: precBits
	"Evaluate the area hyperbolic tangent of the receiver by power series expansion.
	arTanh (x) = x (1 + x^2/3 + x^4/5 + ... ) for -1 < x < 1
	The algorithm is interesting when the receiver is close to zero"
	
	| one x2 two count sum term |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	sum _ one copy.
	term _ one copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	count inPlaceAdd: two.
	sum inPlaceAdd: term / count.
	term exponent + precBits < sum exponent] whileFalse.
	sum inPlaceMultiplyBy: self.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 01:50'!
powerExpansionArcSinPrecision: precBits
	"Evaluate the arc sine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| one x2 two count sum term |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	sum _ one copy.
	term _ one copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceMultiplyBy: count.
	term inPlaceDivideBy: count + one.
	count inPlaceAdd: two.
	sum inPlaceAdd: term / count.
	term exponent + precBits < sum exponent] whileFalse.
	sum inPlaceMultiplyBy: self.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 01:16'!
powerExpansionArcTanPrecision: precBits
	"Evaluate the arc tangent of the receiver by power series expansion.
	arcTan (x) = x (1 - x^2/3 + x^4/5 - ... ) for -1 < x < 1
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term two x2 |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	sum _ one copy.
	term _ one copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term / count.
	term exponent + precBits < sum exponent] whileFalse.
	sum inPlaceMultiplyBy: self.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 00:40'!
powerExpansionCosPrecision: precBits
	"Evaluate the cosine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term two x2 |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	sum _ one copy.
	term _ one copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceDivideBy: count * (count + one).
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 00:41'!
powerExpansionCoshPrecision: precBits
	"Evaluate the hyperbolic cosine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term two x2 |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ one copy.
	sum _ one copy.
	term _ one copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceDivideBy: count * (count + one).
	count inPlaceAdd: two.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 00:16'!
powerExpansionLnPrecision: precBits
	"Evaluate the neperian logarithm of the receiver by power series expansion.
	For quadratic convergence, use:
	ln ((1+y)/(1-y)) = 2 y (1 + y^2/3 + y^4/5 + ... ) = 2 ar tanh( y )
	(1+y)/(1-y) = self => y = (self-1)/(self+1)
	This algorithm is interesting when the receiver is close to 1"
	
	| one |
	one _ self one.
	^((self - one)/(self + one) powerExpansionArTanhPrecision: precBits) timesTwoPower: 1! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 00:41'!
powerExpansionSinCosPrecision: precBits
	"Evaluate the sine and cosine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sin cos term |
	one _ self one.
	count _ one copy.
	cos _ one copy.
	sin _ self copy.
	term _ self copy.
	
	[count inPlaceAdd: one.
	term
		inPlaceMultiplyBy: self;
		inPlaceDivideBy: count;
		inPlaceNegated.
	cos inPlaceAdd: term.

	count inPlaceAdd: one.
	term
		inPlaceMultiplyBy: self;
		inPlaceDivideBy: count.
	sin inPlaceAdd: term.
	
	term exponent + precBits < sin exponent] whileFalse.
	^Array with: sin with: cos! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 00:41'!
powerExpansionSinPrecision: precBits
	"Evaluate the sine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term two x2 |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ two copy.
	sum _ self copy.
	term _ self copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceDivideBy: count * (count + one).
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 00:41'!
powerExpansionSinhPrecision: precBits
	"Evaluate the hyperbolic sine of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term two x2 |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ two copy.
	sum _ self copy.
	term _ self copy.
	x2 _ self squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceDivideBy: count * (count + one).
	count inPlaceAdd: two.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 03:48'!
powerExpansionTanPrecision: precBits
	"Evaluate the tangent of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term pow two x2 seidel |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ two copy.
	sum _ one copy.
	pow _ one copy.
	x2 _ self squared.
	seidel _ OrderedCollection new: 256.
	seidel add: 1.
	
	[pow inPlaceMultiplyBy: x2.
	pow inPlaceDivideBy: count * (count + one).
	count inPlaceAdd: two.
	2 to: seidel size do: [:i | seidel at: i put: (seidel at: i-1) + (seidel at: i)].
	seidel addLast: seidel last.
	seidel size to: 2 by: -1 do: [:i | seidel at: i - 1 put: (seidel at: i-1) + (seidel at: i)].
	seidel addFirst: seidel first.
	term _ pow * seidel first.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	sum inPlaceMultiplyBy: self.
	^sum! !

!LargeFloat methodsFor: 'private' stamp: 'nice 2/9/2015 03:46'!
powerExpansionTanhPrecision: precBits
	"Evaluate the hyperbolic tangent of the receiver by power series expansion.
	The algorithm is interesting when the receiver is close to zero"
	
	| count one sum term pow two x2 seidel |
	one _ self one.
	two _ one timesTwoPower: 1.
	count _ two copy.
	sum _ one copy.
	pow _ one copy.
	x2 _ self squared.
	seidel _ OrderedCollection new: 256.
	seidel add: 1.
	
	[pow inPlaceMultiplyBy: x2.
	pow inPlaceDivideBy: count * (count + one).
	pow inPlaceNegated.
	count inPlaceAdd: two.
	2 to: seidel size do: [:i | seidel at: i put: (seidel at: i-1) + (seidel at: i)].
	seidel addLast: seidel last.
	seidel size to: 2 by: -1 do: [:i | seidel at: i - 1 put: (seidel at: i-1) + (seidel at: i)].
	seidel addFirst: seidel first.
	term _ pow * seidel first.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	sum inPlaceMultiplyBy: self.
	^sum! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 11/13/2010 00:12'!
precision
	^precision! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 2/10/2007 22:27'!
precisionInMantissa
	"this is equal to precision only if we are normalized.
	If we are reduced (low bits being zero are removed), then it will be less.
	If we haven't been rounded/truncated then it will be more"

	^mantissa highBitOfMagnitude! !

!LargeFloat methodsFor: 'truncation and round off' stamp: 'nice 7/30/2013 01:01'!
predecessor
	mantissa = 0 ifTrue: [^self].
	mantissa negative ifTrue: [^self negated successor negated].
	^mantissa isPowerOfTwo
		ifTrue: [self - (self ulp timesTwoPower: -1)]
		ifFalse: [self - self ulp]! !

!LargeFloat methodsFor: 'printing' stamp: 'nice 2/10/2007 01:31'!
printOn: aStream
	^self printOn: aStream base: 10! !

!LargeFloat methodsFor: 'printing' stamp: 'len 12/4/2016 10:10:40'!
printOn: aStream base: base 
	self negative ifTrue: [aStream nextPut: $-].
	self absPrintExactlyOn: aStream base: base! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 2/7/2015 01:34'!
raisedToInteger: anInteger 
	| bitProbe highPrecisionSelf n result |
	n _ anInteger abs.
	(n < 5 or: [n * precision < 512])
		ifTrue: [^ self naiveRaisedToInteger: anInteger].
	bitProbe _ 1 bitShift: n highBit - 1.
	highPrecisionSelf _ self asLargeFloatPrecision: n highBit * 2 + precision + 2.
	result _ highPrecisionSelf one.
	
	[(n bitAnd: bitProbe) = 0 ifFalse: [result _ result * highPrecisionSelf].
	(bitProbe _ bitProbe bitShift: -1) > 0]
		whileTrue: [result _ result squared].
		
	^ (anInteger negative
		ifTrue: [result reciprocal]
		ifFalse: [result])
		asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 4/29/2006 00:46'!
reciprocal
	^self copy inPlaceReciprocal! !

!LargeFloat methodsFor: 'private' stamp: 'nice 3/16/2014 23:43'!
reduce
	"remove trailing zero bits from mantissa so that we can do arithmetic on smaller integer
	(that will un-normalize self)"

	| trailing |
	trailing _ mantissa abs lowBit - 1.
	trailing > 0
		ifFalse: [ ^ self ].
	mantissa _ self shift: mantissa by: trailing negated.
	biasedExponent _ biasedExponent + trailing! !

!LargeFloat methodsFor: 'private' stamp: 'nice 3/16/2014 23:47'!
round
	"apply algorithm round to nearest even used by IEEE arithmetic"

	"inexact _ ma lowBit <= excess."

	| excess ma carry |
	mantissa isZero
		ifTrue: [ 
			biasedExponent _ 0.
			^ self ].
	ma _ mantissa abs.
	excess _ ma highBit - precision.
	excess > 0
		ifFalse: [ ^ self ].
	carry _ ma bitAt: excess.
	mantissa _ self shift: mantissa by: excess negated.
	biasedExponent _ biasedExponent + excess.
	(carry = 1 and: [ mantissa odd or: [ ma lowBit < excess ] ])
		ifFalse: [ ^ self ].
	mantissa _ mantissa + mantissa sign.
	self truncate! !

!LargeFloat methodsFor: 'initialization' stamp: 'nice 4/28/2006 23:02'!
setPrecisionTo: n 
	precision _ n.
	self round! !

!LargeFloat methodsFor: 'private' stamp: 'nice 4/30/2006 02:12'!
shift: m by: d
	"shift mantissa m absolute value by some d bits, then restore sign"
	
	^m negative
		ifTrue: [(m negated bitShift: d) negated]
		ifFalse: [m bitShift: d]! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 4/29/2006 00:56'!
sign
	^mantissa sign! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 7/30/2013 01:13'!
significand
	^self timesTwoPower: self exponent negated! !

!LargeFloat methodsFor: 'accessing' stamp: 'nice 4/29/2006 00:56'!
significandAsInteger
	self normalize.
	^mantissa abs! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'len 12/4/2016 20:45:10'!
sin
	"Evaluate the sine of the receiver"

	| pi halfPi quarterPi x neg |
	x _ self moduloNegPiToPi.
	neg _ x negative.
	x inPlaceAbs.
	pi _ self piDoublePrecision.
	halfPi _ pi timesTwoPower: -1.
	x > halfPi ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi _ halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			x _ self cos: x]
		ifFalse: [x := self sin: x].
	neg ifTrue: [x inPlaceNegated].
	^x asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'private' stamp: 'len 12/4/2016 20:40:40'!
sin: x
	"Evaluate the sine of x by sin(5x) formula and power series expansion."
	
	| sin sin2 sin4 fifth five |
	x isZero ifTrue: [^x zero].
	five _ 5 asLargeFloatPrecision: x precision.
	fifth _ x / five.
	sin _ fifth powerExpansionSinPrecision: precision + 8.
	sin2 _ sin squared.
	sin2 inPlaceTimesTwoPower: 2.
	sin4 _ sin2 squared.
	sin2 inPlaceMultiplyBy: five.
	^sin4
		inPlaceSubtract: sin2;
		inPlaceAdd: five;
		inPlaceMultiplyBy: sin;
		yourself! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'len 12/4/2016 20:46:20'!
sincos
	"Evaluate the sine and cosine of the receiver"

	| pi halfPi quarterPi x sincos sinneg cosneg |
	x _ self moduloNegPiToPi.
	sinneg _ x negative.
	x inPlaceAbs.
	pi _ self piDoublePrecision.
	halfPi _ pi timesTwoPower: -1.
	(cosneg _ x > halfPi) ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi _ halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			sincos _ (self sincos: x) reversed]
		ifFalse:
			[sincos _ self sincos: x].
	sinneg ifTrue: [sincos first inPlaceNegated].
	cosneg ifTrue: [sincos last inPlaceNegated].
	^sincos collect: [:e| e asLargeFloatPrecision: precision]! !

!LargeFloat methodsFor: 'private' stamp: 'len 12/4/2016 20:41:38'!
sincos: x
	"Evaluate the sine and cosine of x by recursive sin(2x) and cos(2x) formula and power series expansion.
	Note that it is better to use this method with x <= pi/4."
	
	| one sin cos sincos fraction power |
	x isZero ifTrue: [^Array with: x zero with: x one].
	power _ ((precision bitShift: -1) + x exponent max: 0) highBit.
	fraction _ x timesTwoPower: power negated.
	sincos _ fraction powerExpansionSinCosPrecision: precision + (1 bitShift: 1 + power).
	sin _ sincos first.
	cos _ sincos last.
	one _ x one.
	power timesRepeat:
		["Evaluate sin(2x)=2 sin(x) cos(x)"
		sin inPlaceMultiplyBy: cos; inPlaceTimesTwoPower: 1.
		"Evaluate cos(2x)=2 cos(x)^2-1"
		cos inPlaceMultiplyBy: cos; inPlaceTimesTwoPower: 1; inPlaceSubtract: one].
	^sincos! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 00:38'!
sinh
	| e x |
	self isZero ifTrue: [^self].
	self exponent negated > precision ifTrue: [^self].
	x _ self asLargeFloatPrecision: precision + 16.
	self exponent * -4 >= precision
		ifTrue: [^(x powerExpansionSinhPrecision: x precision) asLargeFloatPrecision: precision].
	e _ x exp.
	^e
		inPlaceSubtract: e reciprocal;
		inPlaceTimesTwoPower: -1;
		asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 4/22/2011 02:29'!
sqrt
	"Answer the square root of the receiver."

	| decimalPlaces n norm guess previousGuess one stopIteration |
	self negative 
		ifTrue: 
			[^ DomainError signal: 'sqrt undefined for number less than zero.'].
	self isZero ifTrue: [^self].

	"use additional bits"
	decimalPlaces _ precision + 16.
	n _ self asLargeFloatPrecision: decimalPlaces.
	
	"constants"
	one _ n one.

	"normalize n"
	norm _ n exponent quo: 2.
	n _ n timesTwoPower: norm * -2.

	"Initial guess for sqrt(1/n)"
	previousGuess _ self class 
				mantissa: 3
				exponent: -2 - (n exponent quo: 2)
				precision: decimalPlaces.
	guess _ previousGuess copy.

	"use iterations x(k+1) _ x*( 1 +  (1-x*x*n)/2) to guess sqrt(1/n)"
	
	[guess inPlaceMultiplyNoRoundBy: guess.
	guess inPlaceMultiplyBy: n.
	guess inPlaceNegated.
	guess inPlaceAddNoRound: one.

	"stop when no evolution of precision + 12 first bits"
	stopIteration _ guess isZero or: [guess exponent < (decimalPlaces - 4) negated].
	guess inPlaceTimesTwoPower: -1.
	guess inPlaceAddNoRound: one.
	guess inPlaceMultiplyNoRoundBy: previousGuess.
	guess negative ifTrue: [guess inPlaceNegated].

	guess isZero or: [stopIteration]] 
			whileFalse: 
				[guess round.
				previousGuess inPlaceCopy: guess].

	"multiply by n and un-normalize"
	guess inPlaceMultiplyBy: n.
	guess inPlaceTimesTwoPower: norm.
	^guess asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 11/1/2010 19:52'!
squared
	| result |
	result _ self copy.
	result inPlaceMultiplyBy: self.
	^result! !

!LargeFloat methodsFor: 'printing' stamp: 'nice 2/10/2007 01:15'!
storeOn: aStream
	aStream nextPut: $(; nextPutAll: self class name.
	aStream space; nextPutAll: 'mantissa:'; space; print: mantissa.
	aStream space; nextPutAll: 'exponent:'; space; print: biasedExponent.
	aStream space; nextPutAll: 'precision:'; space; print: precision.
	aStream nextPut: $)! !

!LargeFloat methodsFor: 'truncation and round off' stamp: 'nice 7/30/2013 01:01'!
successor
	mantissa = 0 ifTrue: [^self].
	mantissa negative ifTrue: [^self negated predecessor negated].
	^self + self ulp! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'len 12/4/2016 21:05:43'!
tan
	"Evaluate the tangent of the receiver"

	| pi halfPi quarterPi x sincos neg tan |
	x _ self moduloNegPiToPi.
	neg _ x negative.
	x inPlaceAbs.
	pi _ self piDoublePrecision.
	halfPi _ pi timesTwoPower: -1.
	(x > halfPi)
		ifTrue:
			[x inPlaceSubtract: pi; inPlaceNegated.
			neg := neg not].
	x exponent * -4 >= precision
		ifTrue: [tan _ x powerExpansionTanPrecision: x precision]
		ifFalse:
			[quarterPi _ halfPi timesTwoPower: -1.
			x > quarterPi
				ifTrue:
					[x inPlaceSubtract: halfPi; inPlaceNegated.
					sincos _ (self sincos: x) reversed]
				ifFalse:
					[sincos _ self sincos: x].
			sincos first inPlaceDivideBy: sincos last.
			tan := sincos first].
	neg ifTrue: [tan inPlaceNegated].
	^tan asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'mathematical functions' stamp: 'nice 2/9/2015 04:20'!
tanh
	| e x ep one |
	self isZero ifTrue: [^self].
	self exponent negated > precision ifTrue: [^self].
	x _ self asLargeFloatPrecision: precision + 16.
	self exponent * -4 >= precision
		ifTrue: [^(x powerExpansionTanhPrecision: x precision) asLargeFloatPrecision: precision].
	e _ x exp.
	one :=x one.
	e inPlaceMultiplyBy: e.
	ep _ e + one.
	^e
		inPlaceSubtract: one;
		inPlaceDivideBy: ep;
		asLargeFloatPrecision: precision! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 4/29/2006 02:43'!
timesTwoPower: n 
	^ self isZero
		ifTrue: [self]
		ifFalse: [self copy inPlaceTimesTwoPower: n]! !

!LargeFloat methodsFor: 'private' stamp: 'nice 3/16/2014 23:46'!
truncate
	"remove trailing bits if they exceed our allocated number of bits"

	| excess |
	excess _ self precisionInMantissa - precision.
	excess > 0
		ifFalse: [ ^ self ].
	mantissa _ self shift: mantissa by: excess negated.
	biasedExponent _ biasedExponent + excess! !

!LargeFloat methodsFor: 'converting' stamp: 'nice 2/7/2007 22:53'!
truncated
	"answer the integer that is nearest to self in the interval between zero and self"
	
	^biasedExponent negated > self precisionInMantissa 
		ifTrue: [0]
		ifFalse: [self shift: mantissa by: biasedExponent]! !

!LargeFloat methodsFor: 'truncation and round off' stamp: 'nice 2/6/2015 23:28'!
ulp
	mantissa = 0 ifTrue: [^self].
	^self one timesTwoPower: self exponent - precision + 1! !

!LargeFloat methodsFor: 'arithmetic' stamp: 'nice 2/10/2007 00:56'!
zero
	^self class 
		mantissa: 0
		exponent: 0
		precision: precision! !

!LargeFloat class methodsFor: 'instance creation' stamp: 'nice 2/7/2007 22:40'!
mantissa: mantisInteger exponent: expoInteger precision: nbitsInteger 
	^self basicNew 
		mantissa: mantisInteger
		exponent: expoInteger
		precision: nbitsInteger! !
