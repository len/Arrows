'From Cuis 6.0 [latest update: #5434] on 5 August 2022 at 9:24:04 am'!
'Description '!
!provides: 'Transfinites' 1 2!
SystemOrganization addCategory: 'Transfinites'!


!classDefinition: #Cardinal category: 'Transfinites'!
Magnitude subclass: #Cardinal
	instanceVariableNames: 'order'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Transfinites'!
!classDefinition: 'Cardinal class' category: 'Transfinites'!
Cardinal class
	instanceVariableNames: ''!

!classDefinition: #Infinity category: 'Transfinites'!
Magnitude subclass: #Infinity
	instanceVariableNames: 'sign'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Transfinites'!
!classDefinition: 'Infinity class' category: 'Transfinites'!
Infinity class
	instanceVariableNames: ''!

!classDefinition: #Ordinal category: 'Transfinites'!
Magnitude subclass: #Ordinal
	instanceVariableNames: 'terms'
	classVariableNames: 'Zero'
	poolDictionaries: ''
	category: 'Transfinites'!
!classDefinition: 'Ordinal class' category: 'Transfinites'!
Ordinal class
	instanceVariableNames: ''!


!Cardinal commentStamp: '<historical>' prior: 0!
Aleph numbers, i.e. cardinalities of infinite sets. Aleph-naught is the cardinality of the natural numbers. Assuming the continuum hypothesis, aleph-one is taken to be the cardinality of the real numbers.!

!Infinity commentStamp: '<historical>' prior: 0!
The real +infinity or -infinity.

For some examples try these:
	Infinity positive * Infinity negative.
	Infinity positive * 2.
	Infinity positive - 7811234871239847.
	Infinity negative / -199.
	Infinity positive reciprocal.
	Infinity positive > Infinity negative.
	Infinity negative < -19238479182374598172349871234.
	Infinity negative > 0.
	Infinity negative min: Infinity positive.

The following are examples of undeterminations (they produce an error):
	Infinity positive + Infinity negative.
	Infinity positive * 0.
	Infinity positive / Infinity positive.
	Infinity positive raisedToInteger: 0.
!

!Cardinal methodsFor: 'printing' stamp: 'len 5/23/2020 07:28:29'!
printOn: aStream
	aStream nextPut: $ℵ; nextPutAll: order printString sub! !

!Cardinal methodsFor: 'accessing' stamp: 'len 4/22/2016 05:06'!
order
	^ order! !

!Cardinal methodsFor: 'arithmetic' stamp: 'len 4/22/2016 02:39'!
* anObject
	anObject = 0 ifTrue: [^ self errorUndetermined].
	^ self max: anObject! !

!Cardinal methodsFor: 'arithmetic' stamp: 'len 1/22/2017 08:12:39'!
^ aNumber
	(aNumber isInteger and: [aNumber > 0]) ifTrue: [^ self].
	DomainError signal: 'only integer positive exponents allowed'! !

!Cardinal methodsFor: 'comparing' stamp: 'len 4/22/2016 05:06'!
< anObject
	anObject class = self class ifTrue: [^ order < anObject order].
	^ false! !

!Cardinal methodsFor: 'comparing' stamp: 'len 4/22/2016 05:06'!
= anObject
	^ self class == anObject class and: [order = anObject order]! !

!Cardinal methodsFor: 'comparing' stamp: 'len 6/28/2016 09:04'!
hash
	^ self class hash + order hash! !

!Cardinal methodsFor: 'converting' stamp: 'len 4/22/2016 05:11'!
adaptToNumber: rcvr andSend: selector
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #/ ifTrue:[^ rcvr isNumber ifTrue: [0] ifFalse: [self errorUndetermined]].
	selector == #> ifTrue:[^false].
	selector == #>= ifTrue:[^false].
	selector == #< ifTrue:[^true].
	selector == #<= ifTrue:[^true].
	^super adaptToNumber: rcvr andSend: selector! !

!Cardinal methodsFor: 'initialization' stamp: 'len 4/22/2016 05:06'!
order: anInteger
	order _ anInteger! !

!Cardinal methodsFor: 'testing' stamp: 'len 4/22/2016 05:06'!
isCountable
	^ order = 0! !

!Cardinal methodsFor: 'testing' stamp: 'len 4/22/2016 04:54'!
isInfinite
	^ true! !

!Cardinal methodsFor: 'private' stamp: 'len 4/16/2022 12:17:51'!
errorUndetermined
	^ self error: 'undetermined'! !

!Cardinal class methodsFor: 'instance creation' stamp: 'len 4/22/2016 02:37'!
new
	^ self new: 0! !

!Cardinal class methodsFor: 'instance creation' stamp: 'len 4/22/2016 05:07'!
new: anInteger
	^ self basicNew order: anInteger! !

!Infinity methodsFor: 'printing' stamp: 'len 5/23/2020 07:36:16'!
printOn: aStream
	self negative ifTrue: [aStream nextPut: $-].
	aStream nextPut: $∞! !

!Infinity methodsFor: 'accessing' stamp: 'len 10/9/97 20:17'!
sign
	"Answer the sign of the receiver."

	^ sign! !

!Infinity methodsFor: 'accessing-private' stamp: 'len 10/9/97 20:16'!
sign: anInteger
	sign _ anInteger! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 11/14/97 14:49'!
* anObject
	"Answer the product of the receiver by the argument."

	anObject = 0 ifTrue: [^ self errorUndetermined].
	^ self class sign: self sign * anObject sign! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 12/13/97 02:26'!
+ anObject
	"Answer the sum of the receiver and the argument."

	(anObject isInfinity and: [self sign ~= anObject sign])
		ifTrue: [^ self errorUndetermined].
	^ self! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 11/12/97 21:30'!
- anObject
	"Answer the difference between the receiver and the argument."

	^ self + anObject negated! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 10/9/97 20:24'!
/ anObject
	"Answer the division of the receiver by the argument."

	^ self * anObject reciprocal! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 1/22/2017 08:15:34'!
^ aNumber
	^ self raisedTo: aNumber! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 11/12/97 21:31'!
negated
	"Answer a copy of the receiver with the sign changed."

	^ self class sign: self sign negated! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 1/16/2016 06:08'!
raisedTo: aNumber
	aNumber isInteger ifTrue: [^ self raisedToInteger: aNumber].
	^ DomainError signal! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 11/9/2015 23:18'!
raisedToInteger: anInteger
	anInteger = 0 ifTrue: [^ self errorUndetermined].
	anInteger negative ifTrue: [^ 0].
	^ anInteger odd ifTrue: [self] ifFalse: [self negated]! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 11/12/97 21:32'!
reciprocal
	"Answer zero. (1 / self)"

	^ 0! !

!Infinity methodsFor: 'arithmetic' stamp: 'len 10/17/97 04:09'!
squared
	"Answer the square of the receiver."

	^ self class positive! !

!Infinity methodsFor: 'comparing' stamp: 'len 11/9/2015 15:19'!
< anObject
	anObject isInfinity ifTrue: [^ self sign < anObject sign].
	^ self negative! !

!Infinity methodsFor: 'comparing' stamp: 'len 11/9/2015 15:19'!
<= aMagnitude
	^ self < aMagnitude! !

!Infinity methodsFor: 'comparing' stamp: 'len 11/9/2015 15:19'!
= anObject
	^ self class == anObject class and: [self sign = anObject sign]! !

!Infinity methodsFor: 'comparing' stamp: 'len 11/9/2015 15:19'!
> anObject
	anObject isInfinity ifTrue: [^ self sign > anObject sign].
	^ self positive! !

!Infinity methodsFor: 'comparing' stamp: 'len 11/9/2015 15:19'!
>= aMagnitude
	^ self > aMagnitude! !

!Infinity methodsFor: 'comparing' stamp: 'len 6/28/2016 09:19'!
hash
	^ self sign hash + self class hash! !

!Infinity methodsFor: 'converting' stamp: 'len 11/11/2015 05:00'!
adaptToNumber: rcvr andSend: selector
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #- ifTrue:[^self negated + rcvr].
	selector == #/ ifTrue:[^ rcvr isInfinite ifTrue: [self errorUndetermined] ifFalse: [0]].
	selector == #> ifTrue:[^self < rcvr].
	selector == #>= ifTrue:[^self <= rcvr].
	selector == #< ifTrue:[^self > rcvr].
	selector == #<= ifTrue:[^self >= rcvr].
	^super adaptToNumber: rcvr andSend: selector! !

!Infinity methodsFor: 'testing' stamp: 'len 10/18/2016 14:00'!
isInfinite
	^ true! !

!Infinity methodsFor: 'testing' stamp: 'len 11/19/2015 17:00'!
isInfinity
	^ true! !

!Infinity methodsFor: 'testing' stamp: 'len 12/6/2016 13:19:59'!
isZero
	^ false! !

!Infinity methodsFor: 'testing' stamp: 'len 11/14/97 14:50'!
negative
	"Answer true if the receiver is negative."

	^ self sign negative! !

!Infinity methodsFor: 'testing' stamp: 'len 11/14/97 14:50'!
positive
	"Answer true if the receiver is positive."

	^ self sign positive! !

!Infinity methodsFor: 'testing' stamp: 'len 11/14/97 14:51'!
strictlyPositive
	"Answer true if the receiver is strictly positive."

	^ self positive! !

!Infinity methodsFor: 'private' stamp: 'len 10/9/97 20:29'!
errorUndetermined
	^ self error: 'undetermined'! !

!Infinity class methodsFor: 'instance creation' stamp: 'len 10/17/97 04:07'!
negative
	"Answer a new instance of the receiver representing -infinity."

	^ self sign: -1! !

!Infinity class methodsFor: 'instance creation' stamp: 'len 10/17/97 04:07'!
positive
	"Answer a new instance of the receiver representing +infinity."

	^ self sign: 1! !

!Infinity class methodsFor: 'instance creation' stamp: 'len 11/23/2015 02:41'!
projective
	"Answer a new instance of the receiver representing the projective (unsigned) infinity."

	^ self sign: 0! !

!Infinity class methodsFor: 'instance creation' stamp: 'len 10/9/97 20:27'!
sign: anInteger
	"Answer a new instance of the receiver with sign anInteger."

	^ self new sign: anInteger! !

!Ordinal methodsFor: 'printing' stamp: 'len 12/31/2021 11:26:42'!
printOn: aStream
	self isFinite ifTrue: [aStream print: self asInteger. ^ self].
	terms do: [:each|
		each key isZero
			ifTrue: [aStream print: each value]
			ifFalse:
				[aStream nextPut: $ω.
				each key isOne ifFalse: [aStream nextPutAll: each key printString super].
				each value > 1 ifTrue: [aStream print: each value]]]
		separatedBy: [aStream nextPut: $+]! !

!Ordinal methodsFor: 'accessing' stamp: 'len 12/31/2021 11:23:44'!
degree
	^ self isZero ifTrue: [self] ifFalse: [terms first key]! !

!Ordinal methodsFor: 'accessing' stamp: 'len 12/29/2021 11:58:21'!
size
	^ self isFinite ifTrue: [self asInteger] ifFalse: [Cardinal new: self]! !

!Ordinal methodsFor: 'comparing' stamp: 'len 12/31/2021 11:23:28'!
< anOrdinal
	1 to: (terms size min: anOrdinal terms size) do: [:i|
		| a b |
		a _ terms at: i.
		b _ anOrdinal terms at: i.
		(a key < b key or: [a key = b key and: [a value < b value]]) ifTrue: [^ true]].
	^ terms size < anOrdinal terms size! !

!Ordinal methodsFor: 'comparing' stamp: 'len 12/31/2021 11:23:34'!
= anOrdinal
	^ self class = anOrdinal class and: [terms = anOrdinal terms]! !

!Ordinal methodsFor: 'comparing' stamp: 'len 12/31/2021 11:23:48'!
hash
	^ terms hash! !

!Ordinal methodsFor: 'converting' stamp: 'len 12/28/2021 12:51:49'!
adaptToInteger: rcvr andSend: selector
	^ rcvr asOrdinal perform: selector with: self! !

!Ordinal methodsFor: 'converting' stamp: 'len 12/31/2021 11:23:39'!
asInteger
	self isZero ifTrue: [^ 0].
	self isFinite ifFalse: [^ self error: 'transfinite ordinal'].
	^ terms first value! !

!Ordinal methodsFor: 'converting' stamp: 'len 1/4/2022 10:55:45'!
asOrdinal
	^ self! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/31/2021 11:22:19'!
* anOrdinal
	"Answer the ordinal product of the receiver and the argument.
	This operation is not commutative."
	self isZero ifTrue: [^ self].
	anOrdinal isInteger ifTrue: [^ self * anOrdinal asOrdinal].
	anOrdinal isLimit ifTrue: [^ self class terms: (anOrdinal terms collect: [:each| self degree + each key -> each value])].
	^ self class terms:
		(anOrdinal terms allButLast collect: [:each| self degree + each key -> each value]),
		{self degree -> (terms first value * anOrdinal terms last value)},
		terms allButFirst

"	anOrdinal isFinite ifTrue: [^ self timesCoefficient: anOrdinal asInteger].
	answer _ 0 asOrdinal.
	anOrdinal parts do: [:each|
		answer _ answer + ((self timesExponent: each key) timesCoefficient: each value)].
	^ answer"! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/31/2021 11:23:07'!
+ anOrdinal
	"Answer the ordinal sum of the receiver and the argument.
	This operation is not commutative."
	| i |
	anOrdinal isInteger ifTrue: [^ self + anOrdinal asOrdinal].
	self isZero ifTrue: [^ anOrdinal].
	anOrdinal isZero ifTrue: [^ self].
	(i _ terms findLast: [:one| one key >= anOrdinal degree]) = 0 ifTrue: [^ anOrdinal].
	^ (terms at: i) key = anOrdinal degree
		ifTrue: [self class terms: (terms first: i-1), {anOrdinal degree -> ((terms at: i) value + anOrdinal terms first value)}, anOrdinal terms allButFirst]
		ifFalse: [self class terms: (terms first: i), anOrdinal terms]

"	(self isFinite and: [anOrdinal isFinite]) ifTrue: [^ (self asInteger + anOrdinal asInteger) asOrdinal].
	^ self class parts: (self truncatedAt: anOrdinal degree) parts, anOrdinal parts"! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/31/2021 11:27:52'!
^ anOrdinal
	anOrdinal isInteger ifTrue: [^ self ^ anOrdinal asOrdinal].
	self isOmega ifTrue: [^ self class terms: {anOrdinal -> 1}].
	^ self notYetImplemented! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/28/2021 12:50:46'!
× anOrdinal
	"Answer the Jacobsthal multiplication of the receiver with the argument."
	anOrdinal isInteger ifTrue: [^ self × anOrdinal asOrdinal].
	^ self notYetImplemented! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/28/2021 12:40:02'!
⊕ anOrdinal
	"Answer the natural (or Hessenberg) addition of the receiver with the argument."
	anOrdinal isInteger ifTrue: [^ self ⊕ anOrdinal asOrdinal].
	^ self notYetImplemented! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/28/2021 12:40:15'!
⊗ anOrdinal
	"Answer the natural (or Hessenberg) multiplication of the receiver with the argument."
	anOrdinal isInteger ifTrue: [^ self ⊗ anOrdinal asOrdinal].
	^ self notYetImplemented! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/31/2021 11:27:57'!
exp
	^ self class terms: {self -> 1}! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/31/2021 11:26:35'!
log
	(terms size = 1 and: [terms first value = 1]) ifTrue: [^ terms first key].
	^ self notYetImplemented! !

!Ordinal methodsFor: 'operations' stamp: 'len 12/31/2021 11:26:52'!
truncatedAt: anOrdinal
	^ self class terms: (terms first: (terms findLast: [:x| x key >= anOrdinal]))! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/28/2021 12:33:38'!
includes: anObject
	anObject isInteger ifTrue: [^ anObject >= 0 and: [self isFinite not or: [anObject < self asInteger]]].
	^ anObject class = self class and: [anObject < self]! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/31/2021 11:23:58'!
isFinite
	^ terms size = 0 or: [terms size = 1 and: [terms first key isZero]]! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/29/2021 11:12:15'!
isLimit
	^ self isSuccessor not! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/31/2021 11:24:08'!
isOmega
	^ terms size = 1 and: [terms first key isOne and: [terms first value = 1]]! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/31/2021 11:26:16'!
isOne
	^ terms size = 1 and: [terms first key isZero and: [terms first value = 1]]! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/31/2021 11:26:22'!
isSuccessor
	^ self isZero not and: [terms last key isZero]! !

!Ordinal methodsFor: 'testing' stamp: 'len 12/31/2021 11:26:26'!
isZero
	^ terms isEmpty! !

!Ordinal methodsFor: 'private' stamp: 'len 12/31/2021 11:27:28'!
terms
	^ terms! !

!Ordinal methodsFor: 'private' stamp: 'len 12/31/2021 11:27:22'!
terms: anArray
	terms _ anArray! !

!Ordinal methodsFor: 'private' stamp: 'len 7/8/2022 11:11:54'!
validate
	"Verify that the receiver is in Cantor normal form:
		1) exponents in decreasing order,
		2) coefficients > 0 and in increasing order."
	1 to: terms size - 1 do: [:i|
		| a b |
		a _ terms at: i.
		b _ terms at: i+1.
		self assert: (a key > b key and: [a value > 0 and: [a value < b value]])]! !

!Ordinal class methodsFor: 'instance creation' stamp: 'len 12/31/2021 11:28:03'!
fromInteger: anInteger
	anInteger = 0 ifTrue: [^ Zero].
	anInteger > 0 ifFalse: [^ self error: 'not a positive integer'].
	^ self terms: {Zero -> anInteger}! !

!Ordinal class methodsFor: 'instance creation' stamp: 'len 12/31/2021 11:28:58'!
terms: anArray
	^ self new terms: anArray! !

!Ordinal class methodsFor: 'class initialization' stamp: 'len 12/31/2021 11:28:10'!
initialize
	super initialize.
	Zero _ self terms: #().
	Smalltalk at: #ω put: Zero exp exp! !

!Object methodsFor: '*transfinites' stamp: 'len 11/19/2015 17:01'!
isInfinity
	^ false! !

!Magnitude methodsFor: '*transfinites' stamp: 'len 10/18/2016 13:58'!
isFinite
	^ self isInfinite not! !

!Magnitude methodsFor: '*transfinites' stamp: 'len 10/18/2016 13:57'!
isInfinite
	^ false! !
Ordinal initialize!
