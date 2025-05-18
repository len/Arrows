'From Cuis7.3 [latest update: #7158] on 18 May 2025 at 8:33:25 pm'!
'Description General Algebra Subrutines. This package provides low-level support for algebra on arrays.'!
!provides: 'GAS' 1 21!
!requires: 'Collections-Extras' 1 14 nil!



!SequenceableCollection methodsFor: '*gas' stamp: 'len 1/10/2025 10:09:58'!
· aCollection
	| answer |
	self flag: #deprecated. "but used in Buchberger algorithm"
	self isEmpty ifTrue: [^ aCollection first zero].
	aCollection isEmpty ifTrue: [^ self first zero].
	answer := self first * aCollection first.
	2 to: (self size min: aCollection size) do: [:i| answer := (self at: i) * (aCollection at: i) + answer].
	^ answer! !

!SequenceableCollection methodsFor: '*gas' stamp: 'len 3/23/2016 05:04'!
at: anInteger add: anObject
	^ self at: anInteger put: (self at: anInteger) + anObject! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 16:35:18'!
* anObject
	| n |
	self species = anObject species
		ifTrue: [^ (self species new: (n _ self size min: anObject size)) replaceFrom: 1 to: n with: self startingAt: 1 times: anObject startingAt: 1].
	anObject isCollection ifTrue: [^ super * anObject].
	^ (self species new: (n _ self size)) replaceFrom: 1 to: n with: self startingAt: 1 timesScalar: anObject! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 16:37:02'!
*= anObject
	self isEmpty ifTrue: [^ self].
	anObject species = self species
		ifTrue: [	self replaceFrom: 1 to: self size with: self startingAt: 1 times: anObject startingAt: 1]
		ifFalse: [self replaceFrom: 1 to: self size with: self startingAt: 1 timesScalar: anObject]! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/11/2023 17:42:47'!
+ anObject
	| n m answer |
	anObject species = self species ifFalse:
		[anObject isCollection ifTrue: [^ super + anObject].
		^ self collect: [:each| each + anObject]].
	n _ self size.
	m _ anObject size.
	answer _ self species new: (n max: m).
	answer replaceFrom: 1 to: (n min: m) with: self startingAt: 1 plus: anObject startingAt: 1.
	m > n
		ifTrue: [answer replaceFrom: n+1 to: m with: anObject startingAt: n+1]
		ifFalse: [m < n ifTrue: [answer replaceFrom: m+1 to: n with: self startingAt: m+1]].
	^ answer! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 2/1/2024 19:56:47'!
+= anObject
	anObject species = self species ifFalse:
		[anObject isCollection ifTrue: [^ super += anObject].
		^ self replace: [:each| each + anObject]].
	self replaceFrom: 1 to: self size plus: anObject startingAt: 1! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/6/2023 16:09:15'!
, anObject
	anObject isCollection ifTrue: [^ super , anObject].
	^ self copyWith: anObject! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/11/2023 17:43:04'!
- anObject
	| n m answer |
	anObject species = self species ifFalse:
		[anObject isCollection ifTrue: [^ super - anObject].
		^ self collect: [:each| each - anObject]].
	n _ self size.
	m _ anObject size.
	answer _ self species new: (n max: m).
	answer replaceFrom: 1 to: (n min: m) with: self startingAt: 1 minus: anObject startingAt: 1.
	m > n
		ifTrue: [answer replaceFrom: n+1 to: m withNegated: anObject startingAt: n+1]
		ifFalse: [m < n ifTrue: [answer replaceFrom: m+1 to: n with: self startingAt: m+1]].
	^ answer! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 2/1/2024 20:13:26'!
-= anObject
	anObject species = self species ifFalse:
		[anObject isCollection ifTrue: [^ super -= anObject].
		^ self replace: [:each| each - anObject]].
	self replaceFrom: 1 to: self size minus: anObject startingAt: 1! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 16:42:56'!
/ anObject
	anObject isCollection ifTrue: [^ super / anObject].
	^ self collect: [:each| each / anObject]! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 16:43:26'!
// anObject
	anObject isCollection ifTrue: [^ super // anObject].
	^ self collect: [:each| each // anObject]! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 15:53:37'!
<< anInteger
	"Answer the left shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	^ self >> anInteger negated! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 13:15:55'!
<<< anInteger
	"Answer the circular left shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	^ self >>> anInteger negated! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/9/2023 18:50:04'!
>> anInteger
	"Answer the right shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	| answer zero n m |
	(anInteger = 0 or: [(m _ self size) = 0]) ifTrue: [^ self].
	answer _ self species new: m.
	zero _ self first zero.
	n _ m min: anInteger max: m negated.
	n > 0
		ifTrue:
			[answer from: 1 to: n put: zero.
			answer replaceFrom: 1+n to: m with: self startingAt: 1]
		ifFalse:
			[answer from: m+n+1 to: answer size put: zero.
			answer replaceFrom: 1 to: m+n with: self startingAt: 1-n].
	^ answer! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/9/2023 18:50:18'!
>>> anInteger
	"Answer the circular right shift of the receiver by anInteger places.
	The answer is a new array of the same size as the receiver."
	| n m |
	(anInteger = 0 or: [(m _ self size) = 0]) ifTrue: [^ self].
	n _ anInteger \\ m.
	^ (self species new: m)
		replaceFrom: 1 to: n with: self startingAt: m-n+1;
		replaceFrom: 1+n to: m with: self startingAt: 1! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 16:43:36'!
\\ anObject
	anObject isCollection ifTrue: [^ super \\ anObject].
	^ self collect: [:each| each \\ anObject]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 5/20/2023 23:58:36'!
· anArray
	| n i answer |
	answer := self first zero.
	n := self size min: anArray size.
	i := 1.
	[i <= n] whileTrue:
		[answer := (self at: i) * (anArray at: i) + answer.
		i := i + 1].
	^ answer! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 12/9/2022 07:57:27'!
autoconvolution
	self isEmpty ifTrue: [^ self].
	^ (self species new: self size * 2 - 1)
		replaceFrom: 1 withAutoconvolution: self from: 1 to: self size! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 12/9/2022 07:56:19'!
convolution: anArray
	self isEmpty ifTrue: [^ self].
	anArray isEmpty ifTrue: [^ anArray].
	^ (self species new: self size + anArray size - 1)
		replaceFrom: 1 with: self from: 1 to: self size convolution: anArray from: 1 to: anArray size! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:45:34'!
dot: anArray modulo: modulus
	| answer n i minusOne |
	n := self size min: anArray size.
	answer := 0.
	i := 1.	
	modulus squared * n sqrt < 16r40000000
		ifTrue: "delayed reduction"
			[[i <= n] whileTrue:
				[answer := (self at: i) * (anArray at: i) + answer.
				i := i + 1].
			answer := answer \\ modulus]
		ifFalse:
			[((minusOne := modulus - 1) bitAnd: modulus) = 0
				ifTrue: "modulus is power of 2"
					[[i <= n] whileTrue:
						[answer := (self at: i) * (anArray at: i) + answer bitAnd: minusOne.
						i := i + 1]]
				ifFalse:
					[[i <= n] whileTrue:
						[answer := (self at: i) * (anArray at: i) + answer \\ modulus.
						i := i + 1]]].
	^ answer! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 6/6/2023 13:16:08'!
evaluateFrom: start to: stop at: anElement
	"Evaluate the receiver (as an univariate polynomial from start to stop) at the given argument using ranged Horner's method."
	| answer i |
	answer := anElement zero.
	i := stop.
	[i >= start] whileTrue:
		[answer := anElement * answer + (self at: i).
		i := i - 1].
	^ answer! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:45:43'!
evaluateFrom: start to: stop at: anInteger modulo: modulus
	"Evaluate the receiver (as an univariate polynomial from start to stop) at the given argument using ranged Horner's method."
	| answer i minusOne |
	stop < start ifTrue: [^ 0].
	anInteger = 0 ifTrue: [^ self at: start].
	anInteger = 1 ifTrue: [^ self sumFrom: start to: stop modulo: modulus].
	answer := 0.
	i := stop.
	((minusOne := modulus - 1) bitAnd: modulus) = 0
		ifTrue: "power of 2"
			[[i >= start] whileTrue:
				[answer := anInteger * answer + (self at: i) bitAnd: minusOne.
				i := i - 1]]
		ifFalse:
			[[i >= start] whileTrue:
				[answer := anInteger * answer + (self at: i) \\ modulus.
				i := i - 1]].
	^ answer! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 6/21/2023 23:03:56'!
exponentiate: anArray
	| answer next |
	1 to: anArray size do: [:i|
		next := (self at: i) ^ (anArray at: i).
		answer := answer ifNil: [next] ifNotNil: [next * answer]].
	^ answer! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 6/21/2023 23:05:51'!
exponentiate: anArray modulo: modulus
	| answer |
	answer := 1.
	1 to: anArray size do: [:i|
		answer := answer * ((self at: i) raisedTo: (anArray at: i) modulo: modulus) \\ modulus].
	^ answer! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 5/20/2022 11:36:55'!
findFirstNonzeroFrom: start to: stop
	"Answer the index of the first nonzero entry between start and stop, or nil."
	start to: stop do: [:i| (self isZeroAt: i) ifFalse: [^ i]].
	^ nil! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 12/27/2022 11:24:59'!
findLastNonzeroFrom: start to: stop
	"Answer the index of the last nonzero entry between start and stop, or nil."
	| i |
	i _ stop.
	[i >= start] whileTrue:
		[(self isZeroAt: i) ifFalse: [^ i].
		i _ i - 1].
	^ nil! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/20/2022 11:37:09'!
firstNonzeroIndex
	"Answer the index of the first nonzero entry, or nil."
	^ self findFirstNonzeroFrom: 1 to: self size! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/20/2022 20:57:37'!
isZero
	^ (self findFirstNonzeroFrom: 1 to: self size) isNil! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 1/10/2025 10:05:32'!
isZeroAt: anInteger
	^ (self at: anInteger) isZero! !

!ArrayedCollection methodsFor: '*gas-low level-karatsuba' stamp: 'len 5/14/2023 22:52:02'!
karatsubaSplit: length startingAt: start
	^ length + 1 // 2! !

!ArrayedCollection methodsFor: '*gas-low level-karatsuba' stamp: 'len 5/14/2023 11:36:51'!
karatsubaThreshold
	^ 32! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/20/2022 11:37:29'!
lastNonzeroIndex
	"Answer the index of the last nonzero entry, or nil."
	^ self findLastNonzeroFrom: 1 to: self size! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/5/2023 16:01:45'!
negated
	^ (self species new: self size) replaceFrom: 1 to: self size withNegated: self startingAt: 1! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/23/2023 10:34:56'!
partiallyReduceBy: g
	self partiallyReduceBy: g quotient: nil! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:56:43'!
partiallyReduceBy: g quotient: q
	| n m lead |
	m := g lastNonzeroIndex ifNil: [^ self error: 'division by zero'].
	lead := g at: m.
	[(n := self lastNonzeroIndex ifNil: [0]) >= m]
		whileTrue:
			[| i c |
			i := n-m+1.
			c := ((self at: n) // lead) negated.
			c isZero ifTrue: [^ self].
			q ifNotNil: [q at: i put: (q at: i) - c].
			self replaceFrom: i to: i+m-1 plus: g startingAt: 1 timesScalar: c]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 6/2/2023 18:55:09'!
partiallyReduceBy: g quotient: q modulo: modulus
	| n m lead |
	m := g lastNonzeroIndex ifNil: [^ self error: 'division by zero'].
	lead := g at: m.
	[(n := self lastNonzeroIndex ifNil: [0]) >= m]
		whileTrue:
			[| i c |
			i := n-m+1.
			c := ((self at: n) // lead) negated.
			c = 0 ifTrue: [^ self].
			q ifNotNil: [q at: i put: (q at: i) - c \\ modulus].
			self replaceFrom: i to: i+m-1 with: self startingAt: i plus: g startingAt: 1 timesScalar: c modulo: modulus]! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 1/10/2025 10:13:15'!
permutedBy: aPermutation
	| answer |
	answer := self species new: self size.
	1 to: self size do: [:i| answer at: (aPermutation at: i) put: (self at: i)].
	^ answer! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 6/6/2023 08:47:56'!
polynomialDivisionBy: anArray
	| q r |
	q := self zeros.
	r := self copy reduceBy: anArray quotient: q.
	^ {q. r}! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 6/6/2023 08:48:24'!
polynomialPartialDivisionBy: anArray
	| q r |
	q := self zeros.
	r := self copy partiallyReduceBy: anArray quotient: q.
	^ {q. r}! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 6/6/2023 00:17:56'!
polynomialPartialQuotientBy: anArray
	| q r k m n |
	n := self size - 1.
	m := anArray size - 1.
	(n > m and: [(k := n - m * 2) < n]) ifFalse: [^ (self polynomialPartialDivisionBy: anArray) first].
	r := self copyFrom: n - k + 1 to: n + 1.
	q := (self species new: n - m + 1) atAllPut: self first zero.
	r partiallyReduceBy: (anArray copyFrom: n - k + 1 to: m + 1) quotient: q.
	^ q! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 6/6/2023 00:18:19'!
polynomialQuotientBy: anArray
	| q r k m n |
	n := self size - 1.
	m := anArray size - 1.
	(n > m and: [(k := n - m * 2) < n]) ifFalse: [^ (self polynomialDivisionBy: anArray) first].
	r := self copyFrom: n - k + 1 to: n + 1.
	q := (self species new: n - m + 1) atAllPut: self first zero.
	r reduceBy: (anArray copyFrom: n - k + 1 to: m + 1) quotient: q.
	^ q! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/23/2023 10:40:54'!
reduceBy: g
	self reduceBy: g quotient: nil! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 11/7/2023 11:21:53'!
reduceBy: g leadingInverse: u quotient: q modulo: modulus
	| n m minusU |
	m := g size "lastNonzeroIndex".
	"inv _ (g at: m) inverse."
	minusU := modulus - u.
	n := self size + 1.
	[(n := self findLastNonzeroFrom: m to: n-1) notNil]
		whileTrue:
			[| i c |
			i := n-m+1.
			c := (self at: n) * minusU \\ modulus.
			q ifNotNil: [q at: i put: (q at: i) - c \\ modulus].
			self replaceFrom: i to: n with: self startingAt: i plus: g startingAt: 1 timesScalar: c modulo: modulus]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:56:55'!
reduceBy: g quotient: q
	| n m minusU |
	m := g size "lastNonzeroIndex".
	minusU := (g at: m) inverse negated.
	n := self size + 1.
	[(n := self findLastNonzeroFrom: m to: n-1) notNil]
		whileTrue:
			[| i c |
			i := n-m+1.
			c := (self at: n) * minusU.
			q ifNotNil: [q at: i put: (q at: i) - c].
			self replaceFrom: i to: n plus: g startingAt: 1 timesScalar: c]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 1/1/2025 18:10:55'!
replaceFrom: start by: step blockSize: blockSize plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width modulo: modulus
	"Blocked matrix multiplication with accumulation.
	A choice of block size that minimizes cache misses should provide a considerable speedup. Although cache misses don't seem to be the bottleneck in Smalltalk, blocking still improves performance for some modulii because it can enable per-block delayed reduction."
	| value s t u blockBottom blockRight |
	blockSize < (16r40000000 // modulus) squared
		ifTrue: "delayed reduction"
			[0 to: height - 1 // blockSize * blockSize by: blockSize do: [:k₀|
				blockBottom := k₀ + blockSize - 1 min: width1 - 1.
				0 to: width - 1 // blockSize * blockSize by: blockSize do: [:j₀|
					blockRight := j₀ + blockSize - 1 min: width - 1.
					0 to: height - 1 do: [:i|
						s := i*step + start.
						t := i*step1 + start1.
						j₀ to: blockRight do: [:j|
							value := self at: s + j.
							u := start2 + j.
							k₀ to: blockBottom do: [:k| value := (anArray1 at: t + k) * (anArray2 at: k*step2 + u) + value].
							self at: s + j put: value \\ modulus]]]]]
		ifFalse:
			[0 to: height - 1 // blockSize * blockSize by: blockSize do: [:k₀|
				blockBottom := k₀ + blockSize - 1 min: width1 - 1.
				0 to: width - 1 // blockSize * blockSize by: blockSize do: [:j₀|
					blockRight := j₀ + blockSize - 1 min: width - 1.
					0 to: height - 1 do: [:i|
						s := i*step + start.
						t := i*step1 + start1.
						j₀ to: blockRight do: [:j|
							value := self at: s + j.
							u := start2 + j.
							k₀ to: blockBottom do: [:k| value := (anArray1 at: t + k) * (anArray2 at: k*step2 + u) + value \\ modulus].
							self at: s + j put: value]]]]]

"
n := 1000.
a := WordArray new: n*n.
p := 16r0FFFFFFF atRandom nextPrime.
b := a copy.
c := a copy.
[:aRandom| 1 to: a size do: [:i| a at: i put: (aRandom nextBits: 32) \\ p. b at: i put: (aRandom nextBits: 32) \\ p]] value: Random new.

c atAllPut: 0.
[c replaceFrom: 1 by: n blockSize: 25 plusMatrix: a startingAt: 1 by: n width: n height: n timesMatrix: b startingAt: 1 by: n width: n modulo: p] timeToRun
"! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 1/6/2025 09:40:48'!
replaceFrom: start by: step minusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width
	"C ← C - AB."
	| i j |
	i := start.
	j := start1.
	height timesRepeat:
		[| k value |
		k := start2.
		i to: i + width - 1 do: [:s|
			value := self at: s.
			j to: j + width1 - 1 do: [:t|
				| u |
				u := t - j  * step2 + k.
				value := value - ((anArray1 at: t) * (anArray2 at: u))].
			self at: s put: value.
			k := k + 1].
		i := i + step.
		j := j + step1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 1/1/2025 18:14:41'!
replaceFrom: start by: step minusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width modulo: modulus
	| i j k value minusOne |
"	self class isBits ifTrue: [^ self replaceFrom: start by: step blockSize: 128 // self bytesPerElement plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width modulo: modulus].		"
	i := start.
	j := start1.
	modulus * width1 sqrt < 16r40000000
		ifTrue: "delayed reduction"
			[height timesRepeat:
				[k := start2.
				i to: i + width - 1 do: [:s|
					value := self at: s.
					j to: j + width1 - 1 do: [:t| value := value - ((anArray1 at: t) * (anArray2 at: t - j  * step2 + k))].
					self at: s put: value \\ modulus.
					k := k + 1].
				i := i + step.
				j := j + step1]]
		ifFalse:
			[((minusOne := modulus - 1) bitAnd: modulus) = 0
				ifTrue: "power of 2"
					[height timesRepeat:
						[k := start2.
						i to: i + width - 1 do: [:s|
							value := self at: s.
							j to: j + width1 - 1 do: [:t| value := value - ((anArray1 at: t) * (anArray2 at: t - j  * step2 + k)) bitAnd: minusOne].
							self at: s put: value.
							k := k + 1].
						i := i + step.
						j := j + step1]]
				ifFalse:
					[height timesRepeat:
						[k := start2.
						i to: i + width - 1 do: [:s|
							value := self at: s.
							j to: j + width1 - 1 do: [:t| value := value - ((anArray1 at: t) * (anArray2 at: t - j  * step2 + k)) \\ modulus].
							self at: s put: value.
							k := k + 1].
						i := i + step.
						j := j + step1]]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 1/6/2025 09:43:00'!
replaceFrom: start by: step plusMatrix: anArray1 startingAt: start1 by: step1 width: width height: height
	"C ← C + A."
	| i j |
	i := start.
	j := start1.
	height timesRepeat:
		[self replaceFrom: i to: i + width - 1 plus: anArray1 startingAt: j.
		i := i + step.
		j := j + step1]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 1/6/2025 09:41:02'!
replaceFrom: start by: step plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width
	"C ← C + AB."
	| i j |
	i := start.
	j := start1.
	height timesRepeat:
		[| k value |
		k := start2.
		i to: i + width - 1 do: [:s|
			value := self at: s.
			j to: j + width1 - 1 do: [:t|
				| u |
				u := t - j  * step2 + k.
				value := (anArray1 at: t) * (anArray2 at: u) + value].
			self at: s put: value.
			k := k + 1].
		i := i + step.
		j := j + step1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 1/27/2025 14:39:33'!
replaceFrom: start by: step plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width modulo: modulus
	| i j k value minusOne |
	self class isBits ifTrue: [^ self replaceFrom: start by: step blockSize: 128 // self class bytesPerBasicElement plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height timesMatrix: anArray2 startingAt: start2 by: step2 width: width modulo: modulus].		
	i := start.
	j := start1.
	modulus * width1 sqrt < 16r40000000
		ifTrue: "delayed reduction"
			[height timesRepeat:
				[k := start2.
				i to: i + width - 1 do: [:s|
					value := self at: s.
					j to: j + width1 - 1 do: [:t| value := (anArray1 at: t) * (anArray2 at: t - j  * step2 + k) + value].
					self at: s put: value \\ modulus.
					k := k + 1].
				i := i + step.
				j := j + step1]]
		ifFalse:
			[((minusOne := modulus - 1) bitAnd: modulus) = 0
				ifTrue: "power of 2"
					[height timesRepeat:
						[k := start2.
						i to: i + width - 1 do: [:s|
							value := self at: s.
							j to: j + width1 - 1 do: [:t| value := (anArray1 at: t) * (anArray2 at: t - j  * step2 + k) + value bitAnd: minusOne].
							self at: s put: value.
							k := k + 1].
						i := i + step.
						j := j + step1]]
				ifFalse:
					[height timesRepeat:
						[k := start2.
						i to: i + width - 1 do: [:s|
							value := self at: s.
							j to: j + width1 - 1 do: [:t| value := (anArray1 at: t) * (anArray2 at: t - j  * step2 + k) + value \\ modulus].
							self at: s put: value.
							k := k + 1].
						i := i + step.
						j := j + step1]]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:51:05'!
replaceFrom: start by: step width: width preLowerTriangularMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height
	self notYetImplemented! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:51:14'!
replaceFrom: start by: step width: width preUpperTriangularMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height
	self notYetImplemented! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:47:25'!
replaceFrom: start by: step width: width solveLowerTriangularMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height
	self notYetImplemented! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:47:13'!
replaceFrom: start by: step width: width solveUpperTriangularMatrix: anArray1 startingAt: start1 by: step1 width: width1 height: height
	self notYetImplemented! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/3/2024 16:30:33'!
replaceFrom: start by: step withMatrix: anArray startingAt: start1 by: step1 width: width height: height
	| i j |
	i := start.
	j := start1.
	height timesRepeat:
		[self replaceFrom: i to: i + width - 1 with: anArray startingAt: j.
		i := i + step.
		j := j + step1]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 1/6/2025 09:44:02'!
replaceFrom: start by: step withMatrix: anArray1 startingAt: start1 by: step1 width: width height: height plusMatrix: anArray2 startingAt: start2 by: step2 timesScalar: anElement
	"C ← A + B * b."
	| i j k |
	i := start.
	j := start1.
	k := start2.
	height timesRepeat:
		[self replaceFrom: i to: i + width - 1 with: anArray1 startingAt: j plus: anArray2 startingAt: k timesScalar: anElement.
		i := i + step.
		j := j + step1.
		k := k + step2]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:27:00'!
replaceFrom: start plus: anArray1 from: start1 to: stop1 convolution: anArray2 from: start2 to: stop2
	self replaceFrom: start plus: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:46:22'!
replaceFrom: start plus: anArray1 from: start1 to: stop1 convolutionKronecker: anArray2 from: start2 to: stop2 modulo: modulus
	| n₁ n₂ minusOne ℓ₀ ℓ z₁ z₂ z j |
	self flag: #fixme. "this seems to be slower than convolutionLong"
	n₁ := stop1 - start1 + 1.
	n₂ := stop2 - start2 + 1.
	ℓ₀ := (minusOne := modulus - 1) highBit.
	ℓ := ℓ₀ * 2 + (n₁ min: n₂) highBit.
	z₁ := LargePositiveInteger new: n₁ * ℓ + 7 // 8.
	j := 1.
	start1 to: stop1 do: [:i| z₁ bits: ℓ₀ at: j xor: (anArray1 at: i). j := j + ℓ].
	z₂ := LargePositiveInteger new: n₂ * ℓ + 7 // 8.
	j := 1.
	start2 to: stop2 do: [:i| z₂ bits: ℓ₀ at: j xor: (anArray2 at: i). j := j + ℓ].
	z := z₁*z₂.
	j := 1.
	(minusOne bitAnd: modulus) = 0
		ifTrue: "modulus is power of 2"
			[start to: start + n₁ + n₂ - 2 do: [:i| self at: i put: ((self at: i) + (z bits: ℓ₀ at: j) bitAnd: minusOne). j := j + ℓ]]
		ifFalse:
			[start to: start + n₁ + n₂ - 2 do: [:i| self at: i put: (self at: i) + (z bits: ℓ at: j) \\ modulus. j := j + ℓ]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 11:00:02'!
replaceFrom: start plus: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2
	start2 to: stop2 do: [:i|
		(anArray2 isZeroAt: i) ifFalse:
			[| k |
			k := i-start2+start.
			self replaceFrom: k to: k+stop1-start1
				plus: anArray1 startingAt: start1
				timesScalar: (anArray2 at: i)]]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/2/2024 10:28:42'!
replaceFrom: start plus: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2 modulo: modulus
	start2 to: stop2 do: [:i| | x |
		(x := anArray2 at: i) = 0 ifFalse:
			[| index |
			index := i-start2+start.
			self replaceFrom: index to: index+stop1-start1
				with: self startingAt: index
				plus: anArray1 startingAt: start1 timesScalar: x
				modulo: modulus]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:27:48'!
replaceFrom: start plusAutoconvolution: anArray1 from: start1 to: stop1
	self replaceFrom: start plusAutoconvolutionLong: anArray1 from: start1 to: stop1! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:27:48'!
replaceFrom: start plusAutoconvolutionLong: anArray1 from: start1 to: stop1
	self replaceFrom: start plus: anArray1 from: start1 to: stop1 convolutionLong: anArray1 from: start1 to: stop1! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/1/2024 20:12:08'!
replaceFrom: start to: stop minus: anArray1 startingAt: start1
	^ self replaceFrom: start to: stop with: self startingAt: start minus: anArray1 startingAt: start1! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/1/2024 19:55:27'!
replaceFrom: start to: stop plus: anArray1 startingAt: start1
	^ self replaceFrom: start to: stop with: self startingAt: start plus: anArray1 startingAt: start1! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/1/2024 19:56:11'!
replaceFrom: start to: stop plus: anArray1 startingAt: start1 timesScalar: anElement
	^ self replaceFrom: start to: stop with: self startingAt: start plus: anArray1 startingAt: start1 timesScalar: anElement! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/3/2024 17:11:45'!
replaceFrom: start to: stop plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 timesVector: anArray2 startingAt: start2
	| j |
	width1 = 0 ifTrue: [^ self].
	j := start1.
	start to: stop do: [:i|
		| value |
		value := self at: i.
		j to: j + width1 - 1 do: [:k| value := (anArray1 at: k) * (anArray2 at: k - j + start2) + value].
		self at: i put: value.
		j := j + step1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 17:11:00'!
replaceFrom: start to: stop plusMatrix: anArray1 startingAt: start1 by: step1 width: width1 timesVector: anArray2 startingAt: start2 modulo: modulus
	| j value minusOne |
	width1 = 0 ifTrue: [^ self].
	j := start1.
	modulus * width1 sqrt < 16r40000000
		ifTrue: "delayed reduction"
			[start to: stop do: [:i|
				value := self at: i.
				j to: j + width1 - 1 do: [:k| value := (anArray1 at: k) * (anArray2 at: k - j + 1) + value].
				self at: i put: value \\ modulus.
				j := j + step1]]
		ifFalse:
			[((minusOne := modulus - 1) bitAnd: modulus) = 0
				ifTrue: "power of 2"
					[start to: stop do: [:i|
						value := self at: i.
						j to: j + width1 - 1 do: [:k| value := (anArray1 at: k) * (anArray2 at: k - j + 1) + value bitAnd: minusOne].
						self at: i put: value.
						j := j + step1]]
				ifFalse:
					[start to: stop do: [:i|
						value := self at: i.
						j to: j + width1 - 1 do: [:k| value := (anArray1 at: k) * (anArray2 at: k - j + 1) + value \\ modulus].
						self at: i put: value.
						j := j + step1]]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 4/25/2022 07:55:30'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i|
		self at: i put: (anArray1 at: j1) - (anArray2 at: j2).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 12/27/2022 18:02:28'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 minus: anArray2 startingAt: start2 modulo: modulus
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i| | x |
		x _ anArray2 at: j2.
		x = 0 ifTrue: [x _ anArray1 at: j1] ifFalse: [x _ (anArray1 at: j1) + modulus - x. x >= modulus ifTrue: [x _ x - modulus]].
		self at: i put: x.
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 4/25/2022 07:56:38'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i|
		self at: i put: (anArray1 at: j1) + (anArray2 at: j2).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 5/1/2022 12:20:44'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 modulo: modulus
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i| | x |
		x _ (anArray1 at: j1) + (anArray2 at: j2).
		x >= modulus ifTrue: [x _ x - modulus].
		self at: i put: x.
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 5/30/2023 17:40:32'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anElement
	| j1 j2 |
	(self == anArray1 and: [start = start1]) ifTrue: "this optimization makes reduction by a sparse modulus much faster"
		[j2 := start2.
		start to: stop do: [:i|
			| x |
			(x := anArray2 at: j2) isZero
				ifFalse: [self at: i put: (self at: i) + (x * anElement)].
			j2 := j2 + 1].
		^ self].
	j1 := start1.
	j2 := start2.
	start to: stop do: [:i|
		self at: i put: (anArray1 at: j1) + ((anArray2 at: j2) * anElement).
		j1 := j1 + 1.
		j2 := j2 + 1].
	^ self! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 5/30/2023 17:39:01'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 timesScalar: anInteger modulo: modulus
	| j1 j2 |
	anInteger = 0 ifTrue: [^ self replaceFrom: start to: stop with: anArray1 startingAt: start1].
	anInteger = 1 ifTrue: [^ self replaceFrom: start to: stop with: anArray1 startingAt: start1 plus: anArray2 startingAt: start2 modulo: modulus].
	(self == anArray1 and: [start = start1]) ifTrue: "this optimization makes reduction by a sparse modulus much faster"
		[j2 := start2.
		start to: stop do: [:i|
			| x |
			(x := anArray2 at: j2) = 0
				ifFalse: [self at: i put: (self at: i) + (x * anInteger) \\ modulus].
			j2 := j2 + 1].
		^ self].
	j1 := start1.
	j2 := start2.
	start to: stop do: [:i|
		self at: i put: (anArray1 at: j1) + ((anArray2 at: j2) * anInteger) \\ modulus.
		j1 := j1 + 1.
		j2 := j2 + 1]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 4/25/2022 07:58:39'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2
	| j1 j2 |
	j1 _ start1.
	j2 _ start2.
	start to: stop do: [:i|
		self at: i put: (anArray1 at: j1) * (anArray2 at: j2).
		j1 _ j1 + 1.
		j2 _ j2 + 1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:46:42'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 times: anArray2 startingAt: start2 modulo: modulus
	| j1 j2 minusOne |
	j1 := start1.
	j2 := start2.
	((minusOne := modulus - 1) bitAnd: modulus) = 0
		ifTrue: "modulus is power of 2"
			[start to: stop do: [:i|
				self at: i put: ((anArray1 at: j1) * (anArray2 at: j2) bitAnd: minusOne).
				j1 := j1 + 1.
				j2 := j2 + 1]]
		ifFalse:
			[start to: stop do: [:i|
				self at: i put: (anArray1 at: j1) * (anArray2 at: j2) \\ modulus.
				j1 := j1 + 1.
				j2 := j2 + 1]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 6/11/2023 23:20:44'!
replaceFrom: start to: stop with: anArray1 startingAt: start1 timesScalar: anElement
	| j |
	j := start1.
	start to: stop do: [:i|
		self at: i put: (anArray1 at: j) * anElement.
		j := j + 1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:46:54'!
replaceFrom: start to: stop with: anArray startingAt: srcStart timesScalar: anInteger modulo: modulus
	| j minusOne |
	anInteger = 0 ifTrue: [^ self from: start to: stop put: 0].
	anInteger = 1 ifTrue: [^ self replaceFrom: start to: stop with: anArray startingAt: srcStart].
	j := srcStart.
	((minusOne := modulus - 1) bitAnd: modulus) = 0
		ifTrue: "modulus is power of 2"
			[start to: stop do: [:i|
				self at: i put: ((anArray at: j) * anInteger bitAnd: minusOne).
				j := j + 1]]
		ifFalse:
			[start to: stop do: [:i|
				self at: i put: (anArray at: j) * anInteger \\ modulus.
				j := j + 1]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 3/14/2025 08:07:46'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart
	| j |
	j := srcStart.
	start to: stop do: [:i|
		self at: i put: (anArray at: j) negated.
		j := j + 1]! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 5/1/2022 12:24:57'!
replaceFrom: start to: stop withNegated: anArray startingAt: srcStart modulo: modulus
	| j |
	j _ srcStart.
	start to: stop do: [:i| | x |
		x _ anArray at: j.
		x = 0 ifFalse: [x _ modulus - x].
		self at: i put: x.
		j _ j + 1]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 6/13/2023 22:39:00'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolution: anArray2 from: start2 to: stop2
	| s1 s2 k tempArray tempSize depth m |
	s1 := stop1 - start1 + 1.
	s2 := stop2 - start2 + 1.
	"Note: this assumes commutativity. It is fine, because the convolution is only used for multiplication of polynomials, and the coefficients ring is always commutative."
	s1 < s2 ifTrue: [^ self replaceFrom: start with: anArray2 from: start2 to: stop2 convolution: anArray1 from: start1 to: stop1].
	k := self karatsubaThreshold.
	s1 < k ifTrue: [^ self replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2].
	tempSize := 0.
	depth := 0.
	[m := s1 + 1 // 2.
	tempSize := m * 2 - 1 + tempSize.
	s1 := m.
	depth := depth + 1.
	s1 >= k] whileTrue.
	tempArray := self species new: tempSize.
	self replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionKaratsuba: anArray2 from: start2 to: stop2 temporary: tempArray startingAt: 1! !

!ArrayedCollection methodsFor: '*gas-low level-karatsuba' stamp: 'len 2/1/2024 20:14:07'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionKaratsuba: anArray2 from: start2 to: stop2 temporary: tempArray startingAt: tempStart
	| s1 s2 m m2 r1 r2 nextTempStart |
	s1 := stop1 - start1 + 1.
	s2 := stop2 - start2 + 1.
	(s1 > 0 and: [s2 > 0]) ifFalse: [^ self].
	s1 < s2 ifTrue: [^ self replaceFrom: start with: anArray2 from: start2 to: stop2 convolutionKaratsuba: anArray1 from: start1 to: stop1 temporary: tempArray startingAt: tempStart].
	"basecase:"
	s1 < self karatsubaThreshold ifTrue: [^ self replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2].
	m := self karatsubaSplit: s1 startingAt: start1.
	m2 := m * 2.
	m < s2 ifFalse: "degenerate case:"
		[nextTempStart := tempStart + m + s2 - 1.
		"recursively compute  f_hi * g into high part of result:"
		self replaceFrom: start + m with: anArray1 from: start1 + m to: stop1 convolutionKaratsuba: anArray2 from: start2 to: stop2 temporary: tempArray startingAt: nextTempStart.
		"recursively compute f_lo * g into temporary space:"
		tempArray replaceFrom: tempStart with: anArray1 from: start1 to: start1 + m - 1 convolutionKaratsuba: anArray2 from: start2 to: stop2 temporary: tempArray startingAt: nextTempStart.
		"add temporary into result:"
		self replaceFrom: start to: start + m - 1 with: tempArray startingAt: tempStart.
		^ self replaceFrom: start + m to: start + m + s2 - 2 plus: tempArray startingAt: tempStart + m	].
	nextTempStart := tempStart + m2 - 1.
	r1 := s1 - m.
	r2 := s2 - m.
	"compute t1 = f_lo + f_hi:"
	self replaceFrom: start to: start + r1 - 1 with: anArray1 startingAt: start1 plus: anArray1 startingAt: start1 + m.
	self replaceFrom: start + r1 to: start + m - 1 with: anArray1 startingAt: start1 + r1.
	"compute t2 = g_lo + g_hi:"
	self replaceFrom: start + m to: start + m + r2 - 1 with: anArray2 startingAt: start2 plus: anArray2 startingAt: start2 + m.
	self replaceFrom: start + m + r2 to: start + m2 - 1 with: anArray2 startingAt: start2 + r2.
	"recursively compute t3 = t1 * t2:"
	tempArray replaceFrom: tempStart with: self from: start to: start + m - 1 convolutionKaratsuba: self from: start + m to: start + m2 - 1 temporary: tempArray startingAt: nextTempStart.
	"recursively compute f_hi * g_hi into high part of result and substract from t3:"
	self replaceFrom: start + m2 with: anArray1 from: start1 + m to: stop1 convolutionKaratsuba: anArray2 from: start2 + m to: stop2 temporary: tempArray startingAt: nextTempStart.
	tempArray replaceFrom: tempStart to: tempStart + r1 + r2 - 2 minus: self startingAt: start + m2.
	"recursively compute f_lo * g_lo into low part of result and substract from t3:"
	self replaceFrom: start with: anArray1 from: start1 to: start1 + m - 1 convolutionKaratsuba: anArray2 from: start2 to: start2 + m - 1 temporary: tempArray startingAt: nextTempStart.
	tempArray replaceFrom: tempStart to: tempStart + m2 - 2 minus: self startingAt: start.
	self at: start + m2 - 1 put: (anArray1 at: start1) zero.
	"finally add t3 * x^m to result:"
	self replaceFrom: start + m to: start + m + m2 - 2 plus: tempArray startingAt: tempStart! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:47:04'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionKronecker: anArray2 from: start2 to: stop2 modulo: modulus
	| n₁ n₂ minusOne ℓ₀ ℓ z₁ z₂ z j |
	n₁ := stop1 - start1 + 1.
	n₂ := stop2 - start2 + 1.
	ℓ₀ := (minusOne := modulus - 1) highBit.
	ℓ := ℓ₀ * 2 + (n₁ min: n₂) highBit.
	z₁ := LargePositiveInteger new: n₁ * ℓ + 7 // 8.
	j := 1.
	start1 to: stop1 do: [:i| z₁ bits: ℓ₀ at: j xor: (anArray1 at: i). j := j + ℓ].
	z₂ := LargePositiveInteger new: n₂ * ℓ + 7 // 8.
	j := 1.
	start2 to: stop2 do: [:i| z₂ bits: ℓ₀ at: j xor: (anArray2 at: i). j := j + ℓ].
	z := z₁*z₂.
	j := 1.
	(minusOne bitAnd: modulus) = 0
		ifTrue: "modulus is power of 2"
			[start to: start + n₁ + n₂ - 2 do: [:i| self at: i put: (z bits: ℓ₀ at: j). j := j + ℓ]]
		ifFalse:
			[start to: start + n₁ + n₂ - 2 do: [:i| self at: i put: (z bits: ℓ at: j) \\ modulus. j := j + ℓ]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:27:00'!
replaceFrom: start with: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2
	self from: start to: start + stop1 - start1 + stop2 - start2 put: (anArray1 at: start1) zero.
	self replaceFrom: start plus: anArray1 from: start1 to: stop1 convolutionLong: anArray2 from: start2 to: stop2! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 12/9/2022 07:47:04'!
replaceFrom: start withAutoconvolution: anArray1 from: start1 to: stop1
	| s k tempArray tempSize depth m |
	s _ stop1 - start1 + 1.
	k _ self karatsubaThreshold.
	s < k ifTrue: [^ self replaceFrom: start withAutoconvolutionLong: anArray1 from: start1 to: stop1].
	tempSize _ 0.
	depth _ 0.
	[m _ s + 1 // 2.
	tempSize _ m * 2 - 1 + tempSize.
	s _ m.
	depth _ depth + 1.
	s >= k] whileTrue.
	tempArray _ self species new: tempSize.
	self replaceFrom: start withAutoconvolutionKaratsuba: anArray1 from: start1 to: stop1 temporary: tempArray startingAt: 1! !

!ArrayedCollection methodsFor: '*gas-low level-karatsuba' stamp: 'len 2/1/2024 20:14:37'!
replaceFrom: start withAutoconvolutionKaratsuba: anArray1 from: start1 to: stop1 temporary: tempArray startingAt: tempStart
	| s m m2 r nextTempStart |
	s := stop1 - start1 + 1.
	s > 0 ifFalse: [^ self].
	"basecase:"
	s < self karatsubaThreshold ifTrue: [^ self replaceFrom: start withAutoconvolutionLong: anArray1 from: start1 to: stop1].
	m := self karatsubaSplit: s startingAt: start1.
	m2 := m * 2.
	nextTempStart := tempStart + m2 - 1.
	r := s - m.
	"compute t1 = f_lo + f_hi:"
	self replaceFrom: start to: start + r - 1 with: anArray1 startingAt: start1 plus: anArray1 startingAt: start1 + m.
	self replaceFrom: start + r to: start + m - 1 with: anArray1 startingAt: start1 + r.
	self replaceFrom: start + m to: start + m2 - 1 with: self startingAt: start.
	"recursively compute t2 = t1^2:"
	tempArray replaceFrom: tempStart withAutoconvolutionKaratsuba: self from: start to: start + m - 1 temporary: tempArray startingAt: nextTempStart.
	"recursively compute f_hi^2 into high part of result and substract from t2:"
	self replaceFrom: start + m2 withAutoconvolutionKaratsuba: anArray1 from: start1 + m to: stop1 temporary: tempArray startingAt: nextTempStart.
	tempArray replaceFrom: tempStart to: tempStart + (r*2) - 2 minus: self startingAt: start + m2.
	"recursively compute f_lo^2 into low part of result and substract from t2:"
	self replaceFrom: start withAutoconvolutionKaratsuba: anArray1 from: start1 to: start1 + m - 1 temporary: tempArray startingAt: nextTempStart.
	tempArray replaceFrom: tempStart to: tempStart + m2 - 2 minus: self startingAt: start.
	self at: start + m2 - 1 put: (anArray1 at: start1) zero.
	"finally add t3 * x^m to result:"
	self replaceFrom: start + m to: start + m + m2 - 2 plus: tempArray startingAt: tempStart! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:47:14'!
replaceFrom: start withAutoconvolutionKronecker: anArray1 from: start1 to: stop1 modulo: modulus
	| n minusOne ℓ₀ ℓ z₁ z j |
	n := stop1 - start1 + 1.
	ℓ₀ := (minusOne := modulus - 1) highBit.
	ℓ := ℓ₀ * 2 + n highBit.
	z₁ := LargePositiveInteger new: n * ℓ + 7 // 8.
	j := 1.
	start1 to: stop1 do: [:i| z₁ bits: ℓ₀ at: j xor: (anArray1 at: i). j := j + ℓ].
	z := z₁ squared.
	j := 1.
	(minusOne bitAnd: modulus) = 0
		ifTrue: "modulus is power of 2"
			[start to: start + (n * 2) - 2 do: [:i| self at: i put: (z bits: ℓ₀ at: j). j := j + ℓ]]
		ifFalse:
			[start to: start + (n * 2) - 2 do: [:i| self at: i put: (z bits: ℓ at: j) \\ modulus. j := j + ℓ]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/2/2024 10:27:48'!
replaceFrom: start withAutoconvolutionLong: anArray1 from: start1 to: stop1
	self from: start to: stop1 - start1 * 2 + start put: (anArray1 at: start1) zero.
	self replaceFrom: start plusAutoconvolutionLong: anArray1 from: start1 to: stop1! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 5/18/2023 17:34:08'!
replaceWithMatrixTranspose: anArray width: width
	| n height |
	(n := anArray size) = 0 ifTrue: [^ self].
	height := n // width.
	1 to: height do: [:i| | j0 |
		j0 :=  i-1*width.
		1 to: width do: [:j|
			self at: j-1*height + i put: (anArray at: j0 + j)]]

"	blockSize _ 16.
	1 to: height by: blockSize do: [:i|
		1 to: width by: blockSize do: [:j|
			i to: i + blockSize - 1 do: [:k|
				j to: j + blockSize - 1 do: [:l|
					self at: l-1*height + k put: (anArray at: k-1*width + l)]]]]"! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 6/6/2023 13:36:22'!
sumFrom: start to: stop
	| answer i |
	answer := self first zero.
	i := start.
	[i <= stop] whileTrue:
		[answer := answer + (self at: i).
		i := i + 1].
	^ answer! !

!ArrayedCollection methodsFor: '*gas-low level-modular' stamp: 'len 2/3/2024 16:47:25'!
sumFrom: start to: stop modulo: modulus
	| answer i minusOne |
	answer := 0.
	i := start.
	modulus * (stop - start + 1) sqrt < 16r40000000
		ifTrue: "delayed reduction"
			[[i <= stop] whileTrue:
				[answer := (self at: i) + answer.
				i := i + 1].
			answer := answer \\ modulus]
		ifFalse:
			[((minusOne := modulus - 1) bitAnd: modulus) = 0
				ifTrue: "modulus is power of 2"
					[[i <= stop] whileTrue:
						[answer := (self at: i) + answer bitAnd: minusOne.
						i := i + 1]]
				ifFalse:
					[[i <= stop] whileTrue:
						[answer := (self at: i) + answer.
						answer >= modulus ifTrue: [answer := answer - modulus].
						i := i + 1]]].
	^ answer! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/9/2023 21:11:38'!
support
	^ Iterator on: self performing: #supportDo:! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/9/2023 13:50:37'!
supportDo: aBlock
	1 to: self size do: [:i| (self isZeroAt: i) ifFalse: [aBlock value: i]]! !

!ArrayedCollection methodsFor: '*gas-low level' stamp: 'len 2/1/2024 19:46:01'!
swapFrom: start to: stop with: anArray startingAt: start2
	| n tmp |
	self flag: #fixme. "this generates lots of garbage, avoid the temporary space, or do it in small blocks"
	(n := stop - start + 1) > 0 ifFalse: [^ self].
	tmp := self species new: n.
	tmp replaceFrom: 1 to: n with: anArray startingAt: start2.
	anArray replaceFrom: start2 to: start2 + n - 1 with: self startingAt: start.
	self replaceFrom: start to: stop with: tmp startingAt: 1! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/9/2023 17:43:10'!
weight
	"Answer the Hamming weight of the receiver, i.e. the number of entries that are not zero."
	^ (1 to: self size) count: [:each| (self isZeroAt: each) not]! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 5/16/2025 14:35:03'!
withSupportDo: aBlock
	self supportDo: [:i| aBlock value: (self at: i) value: i]! !

!ArrayedCollection methodsFor: '*gas' stamp: 'len 6/6/2023 08:47:31'!
zeros
	self isEmpty ifTrue: [^ self].
	^ (self species new: self size) atAllPut: self first zero! !

!FloatArray methodsFor: '*gas' stamp: 'len 5/13/2019 21:32:12'!
· anObject
	^ self dot: anObject! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/15/2023 12:56:47'!
bitAt: anInteger
	^ (self at: (anInteger - 1 bitShift: -5) + 1) bitAt: (anInteger - 1 bitAnd: 31) + 1! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/15/2023 12:57:08'!
bitAt: anInteger put: aBit
	| index |
	index _ (anInteger - 1 bitShift: -5) + 1.
	self at: index put: ((self at: index) bitAt: (anInteger - 1 bitAnd: 31) + 1 put: aBit).
	^ aBit! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 1/5/2024 11:15:33'!
bitAt: anInteger xor: aBit
	| index |
	aBit = 0 ifTrue: [^ self].
	aBit = 1 ifFalse: [^ self error: 'bit value should be 0 or 1'].
	index := (anInteger - 1 bitShift: -5) + 1.
	self at: index put: ((self at: index) bitXor: (1 bitShift: (anInteger - 1 bitAnd: 31))).
	^ aBit! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/15/2023 18:00:28'!
bitCount
	^ self sum: [:each| each bitCount32] ifEmpty: [0]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/27/2023 15:59:38'!
bitParityParallel
	"Compute the parity of each of the words of the receiver, and return the result in a single integer."
	self notYetImplemented! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/22/2023 18:04:14'!
bitSupport
	^ Iterator on: self performing: #bitSupportDo:! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/28/2023 20:13:21'!
bitSupportDo: aBlock
	| i j word |
	i := 1.
	self do: [:each|
		word := each.
		j := i.
		[word = 0] whileFalse:
			[word odd ifTrue: [aBlock value: j].
			j := j + 1.
			word := word bitShift: -1].
		i := i + 32]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/22/2023 14:19:56'!
bitTranspose32x32
	"Transpose a 32x32 bit matrix in-place.
	From Hacker's Delight [War02]."
	| m j k t |
	m := 16r0000FFFF.
	j := 16.
	[j ~= 0] whileTrue:
		[k := 0.
		[k < 32] whileTrue:
			[t := ((self at: k+1) bitXor: (self at: k+j+1) >> j) bitAnd: m.
			self at: k+1 put: ((self at: k+1) bitXor: t).
			self at: k+j+1 put: ((self at: k+j+1) bitXor: (t << j bitAnd: 16rFFFFFFFF)).
			k := k + j + 1 bitAnd: (j bitInvert bitAnd: 16rFFFFFFFF)].
		j := j >> 1.
		m := m bitXor: (m << j bitAnd: 16rFFFFFFFF)]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/17/2023 12:15:45'!
bits: length at: index
	"Read the desired number of bits from the receiver starting at the given bit index.
	Assume length <= 32."
	| max i₀ i r bits |
	i₀ _ index - 1.
	i _ (i₀ bitShift: -5) + 1.
	i <= (max _ self size) ifFalse: [^ 0].
	bits _ self at: i.
	r _ i*32 - i₀. "r ∈ [1..32]"
	r = 32 ifTrue: [length = 32 ifTrue: [^ bits]] ifFalse: [bits _ bits bitShift: r - 32].
	(length > r and: [i < max]) ifFalse: [^ bits bitAnd: (1 bitShift: length) - 1].
	bits _ bits bitXor: (((self at: i+1) bitAnd: (1 bitShift: 32 - r) - 1) bitShift: r).
	^ bits bitAnd: (1 bitShift: length) - 1! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/17/2023 17:28:47'!
bits: length at: index put: bits
	"Write the desired number of bits to the receiver starting at the given bit index.
	Assume length <= 32."
	| i₀ i r mask |
	self flag: #fixme. "if length = 32 and the index is aligned to 32 no need to use mask or read previous value"
"	self assert: bits highBit <= length.
	self assert: (length between: 1 and: 32)."
	i₀ _ index - 1.
	i _ (i₀ bitShift: -5) + 1.
	r _ i*32 - i₀. "r ∈ [1..32]"
	mask _ (1 bitShift: (r min: length)) - 1 bitShift: 32 - r.
	self at: i put: (((self at: i) bitAnd: mask bitInvert) bitXor: ((bits bitShift: 32 - r) bitAnd: mask)).
	r < length ifFalse: [^ bits].
	mask _ (1 bitShift: length - r) - 1.
	i _ i + 1.
	self at: i put: (((self at: i) bitAnd: mask bitInvert) bitXor: ((bits bitShift: r negated) bitAnd: mask)).
	^ bits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 5/25/2023 13:43:29'!
bits: length at: index xor: bits
	"XOR the desired number of bits over the receiver starting at the given bit index.
	Assume length <= 32."
	| i₀ i r mask |
"	self assert: bits highBit <= length.
	self assert: (length between: 1 and: 32)."
	i₀ := index - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i*32 - i₀. "r ∈ [1..32]"
	mask := (1 bitShift: (r min: length)) - 1 bitShift: 32 - r.
	self at: i put: ((self at: i) bitXor: ((bits bitShift: 32 - r) bitAnd: mask)).
	r < length ifFalse: [^ bits].
	mask := (1 bitShift: length - r) - 1.
	i := i + 1.
	self at: i put: ((self at: i) bitXor: ((bits bitShift: r negated) bitAnd: mask)).
	^ bits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 09:48:04'!
clearBitsFrom: start to: stop
	"Set to 0 the bits from start to stop."
	| n i₀ i r q |
	(n := stop - start + 1) > 0 ifFalse: [^ self].
	i₀ := start - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i*32 - i₀ min: n. "r ∈ [1..32], number of high bits of the first word included in the range"
	r = 32 ifFalse:
		[self bits: r at: start put: 0.
		n := n - r. i := i + 1].
	n >= 32 ifTrue:
		[q := n // 32.
		self from: i to: i + q - 1 put: 0.
		i := i + q.
		n := n - (q * 32)].
	n > 0 ifTrue:
		[self bits: n at: i-1*32 + 1 put: 0]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 11:00:45'!
highBitFrom: start to: stop
	"Answer the index of the last 1 bit between start and stop, or nil."
	| n i₀ i r q bits |
	(n := stop - start + 1) > 0 ifFalse: [^ nil].
	i₀ := stop - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i₀ - (i-1*32) + 1 min: n. "r ∈ [1..32], number of low bits of the last word that are included in the range"
	r = 32 ifFalse:
		[(bits := self bits: r at: stop - r + 1) = 0 ifFalse: [^ stop - r + bits highBit].
		n := n - r. i := i - 1].
	n >= 32 ifTrue:
		[q := n // 32.
		i to: i - q + 1 by: -1 do: [:j| (bits := self at: j) = 0 ifFalse: [^ j-1*32 + bits highBit]].
		i := i - q.
		n := n - (q * 32)].
	n > 0 ifTrue:
		[(bits := self bits: n at: start) = 0 ifFalse: [^ start - 1 + bits highBit]].
	^ nil! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 09:40:41'!
lowBitFrom: start to: stop
	"Answer the index of the first 1 bit between start and stop, or nil."
	| n i₀ i r q bits |
	(n := stop - start + 1) > 0 ifFalse: [^ nil].
	i₀ := start - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i*32 - i₀ min: n. "r ∈ [1..32], number of high bits of the first word included in the range"
	r = 32 ifFalse:
		[(bits := self bits: r at: start) = 0 ifFalse: [^ start + bits lowBit - 1].
		n := n - r. i := i + 1].
	n >= 32 ifTrue:
		[q := n // 32.
		i to: i + q - 1 do: [:j| (bits := self at: j) = 0 ifFalse: [^ j-1*32 + bits lowBit]].
		i := i + q.
		n := n - (q * 32)].
	n > 0 ifTrue:
		[(bits := self bits: n at: i-1*32 + 1) = 0 ifFalse: [^ i-1*32 + bits lowBit]].
	^ nil! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 1/9/2024 10:30:16'!
replaceBitsFrom: start by: step withMatrix: anArray1 startingAt: start1 by: step1 width: width height: height
	| i j bitBlt |
	bitBlt := (BitBlt toForm: (Form extent: self size * 32 @ 1 depth: -1 bits: self))
		sourceForm: (Form extent: anArray1 size * 32 @ 1 depth: -1 bits: anArray1);
		combinationRule: Form over;
		width: width; height: 1.
	i := start - 1.
	j := start1 - 1.
	height timesRepeat:
		[bitBlt sourceX: j; destX: i; copyBits.
		i := i + step.
		j := j + step1]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 1/9/2024 10:28:06'!
replaceBitsFrom: start to: stop with: anArray1 startingAt: start1	
	stop >= start ifFalse: [^ self].
"	self assert: (start > 0 and: [stop <= (self size * 32)])."
	"Since WordArray>>replaceFrom:to:with:startingAt: is faster than BitBlt,
	when the indices are aligned it is better to fallback to the old method:"
	start - start1 \\ 32 = 0 ifTrue: [^ self replaceBitsFromOld: start to: stop with: anArray1 startingAt: start1	].
"	self assert: start1 + stop - start <= (anArray1 size * 32)."
	(BitBlt toForm: (Form extent: self size * 32 @ 1 depth: -1 bits: self))
		sourceForm: (Form extent: anArray1 size * 32 @ 1 depth: -1 bits: anArray1);
		combinationRule: Form over;
		sourceX: start1 - 1; sourceY: 0;
		destX: start - 1; destY: 0;
		width: stop - start + 1; height: 1;
		copyBits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 09:21:34'!
replaceBitsFrom: start to: stop with: anArray1 startingAt: start1 and: anArray2 startingAt: start2	
	| dst src1 src2 |
	stop >= start ifFalse: [^ self].
	self flag: #fixme. "TODO: fine tune this"
	stop - start > 64 ifFalse: [^ self replaceBitsFromOld: start to: stop with: anArray1 startingAt: start1 and: anArray2 startingAt: start2	].	
"	self assert: (start > 0 and: [stop <= (self size * 32)]).
	self assert: start1 + stop - start <= (anArray1 size * 32).
	self assert: start2 + stop - start <= (anArray2 size * 32)."
	dst := Form extent: self size * 32 @ 1 depth: -1 bits: self.
	(self == anArray1 and: [start = start1]) ifTrue:
		[src2 := Form extent: anArray2 size * 32 @ 1 depth: -1 bits: anArray2.
		(BitBlt toForm: dst)
			sourceForm: src2;
			combinationRule: Form and; "dst := dst and src2"
			sourceX: start2 - 1; sourceY: 0;
			destX: start - 1; destY: 0;
			width: stop - start + 1; height: 1;
			copyBits.
		^ self].
	src1 := Form extent: anArray1 size * 32 @ 1 depth: -1 bits: anArray1.
	(self == anArray2 and: [start = start2]) ifTrue:
		[(BitBlt toForm: dst)
			sourceForm: src1;
			combinationRule: Form and; "dst := dst and src1"
			sourceX: start1 - 1; sourceY: 0;
			destX: start - 1; destY: 0;
			width: stop - start + 1; height: 1;
			copyBits.
		^ self].
	src2 := Form extent: anArray2 size * 32 @ 1 depth: -1 bits: anArray2.
	(BitBlt toForm: dst)
		sourceForm: src1;
		combinationRule: Form over; "dst := src1"
		sourceX: start1 - 1; sourceY: 0;
		destX: start - 1; destY: 0;
		width: stop - start + 1; height: 1;
		copyBits;
		sourceForm: src2;
		combinationRule: Form and; "dst := dst and src2"
		sourceX: start2 - 1;
		copyBits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 15:35:07'!
replaceBitsFrom: start to: stop with: anArray1 startingAt: start1 xor: anArray2 startingAt: start2	
	| dst src1 src2 |
	stop >= start ifFalse: [^ self].
	self flag: #fixme. "TODO: fine tune this"
	stop - start > 64 ifFalse: [^ self replaceBitsFromOld: start to: stop with: anArray1 startingAt: start1 xor: anArray2 startingAt: start2	].	
"	self assert: (start > 0 and: [stop <= (self size * 32)]).
	self assert: start1 + stop - start <= (anArray1 size * 32).
	self assert: start2 + stop - start <= (anArray2 size * 32)."
	dst := Form extent: self size * 32 @ 1 depth: -1 bits: self.
	(self == anArray1 and: [start = start1]) ifTrue:
		[src2 := Form extent: anArray2 size * 32 @ 1 depth: -1 bits: anArray2.
		(BitBlt toForm: dst)
			sourceForm: src2;
			combinationRule: Form reverse; "dst := dst xor src2"
			sourceX: start2 - 1; sourceY: 0;
			destX: start - 1; destY: 0;
			width: stop - start + 1; height: 1;
			copyBits.
		^ self].
	src1 := Form extent: anArray1 size * 32 @ 1 depth: -1 bits: anArray1.
	(self == anArray2 and: [start = start2]) ifTrue:
		[(BitBlt toForm: dst)
			sourceForm: src1;
			combinationRule: Form reverse; "dst := dst xor src1"
			sourceX: start1 - 1; sourceY: 0;
			destX: start - 1; destY: 0;
			width: stop - start + 1; height: 1;
			copyBits.
		^ self].
	src2 := Form extent: anArray2 size * 32 @ 1 depth: -1 bits: anArray2.
	(BitBlt toForm: dst)
		sourceForm: src1;
		combinationRule: Form over; "dst := src1"
		sourceX: start1 - 1; sourceY: 0;
		destX: start - 1; destY: 0;
		width: stop - start + 1; height: 1;
		copyBits;
		sourceForm: src2;
		combinationRule: Form reverse; "dst := dst xor src2"
		sourceX: start2 - 1;
		copyBits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 1/5/2024 15:40:47'!
replaceBitsFromNew: start to: stop with: replacement startingAt: repStart	
	stop >= start ifFalse: [^ self].
	"Since WordArray>>replaceFrom:to:with:startingAt: is faster than BitBlt,
	when the indices are aligned it is better to fallback to the old method:"
"	start - repStart \\ 32 = 0 ifTrue: [^ self replaceBitsFromOld: start to: stop with: replacement startingAt: repStart	]."
	self assert: repStart + stop - start <= (replacement size * 32).
	(BitBlt toForm: (Form extent: self size * 32 @ 1 depth: -1 bits: self))
		sourceForm: (Form extent: replacement size * 32 @ 1 depth: -1 bits: replacement);
		combinationRule: Form over;
		sourceX: repStart - 1; sourceY: 0;
		destX: start - 1; destY: 0;
		height: 1; width: stop - start + 1;
		copyBits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/28/2023 20:42:57'!
replaceBitsFromNew: start to: stop with: anArray1 startingAt: start1 and: anArray2 startingAt: start2	
	| dst src1 src2 |
	stop >= start ifFalse: [^ self].
	self assert: start1 + stop - start <= (anArray1 size * 32).
	self assert: start2 + stop - start <= (anArray2 size * 32).
	dst := Form extent: self size * 32 @ 1 depth: -1 bits: self.
	src1 := Form extent: anArray1 size * 32 @ 1 depth: -1 bits: anArray1.
	src2 := Form extent: anArray2 size * 32 @ 1 depth: -1 bits: anArray2.
	(BitBlt toForm: dst)
		sourceForm: src1;
		combinationRule: Form over; "dst := src1"
		sourceX: start1 - 1; sourceY: 0;
		destX: start - 1; destY: 0;
		height: 1; width: stop - start + 1;
		copyBits;
		sourceForm: src2;
		combinationRule: Form and; "dst := dst and src2"
		sourceX: start2 - 1;
		copyBits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/28/2023 20:42:26'!
replaceBitsFromNew: start to: stop with: anArray1 startingAt: start1 xor: anArray2 startingAt: start2	
	| dst src1 src2 |
	stop >= start ifFalse: [^ self].
	self assert: start1 + stop - start <= (anArray1 size * 32).
	self assert: start2 + stop - start <= (anArray2 size * 32).
	dst := Form extent: self size * 32 @ 1 depth: -1 bits: self.
	src1 := Form extent: anArray1 size * 32 @ 1 depth: -1 bits: anArray1.
	src2 := Form extent: anArray2 size * 32 @ 1 depth: -1 bits: anArray2.
	(BitBlt toForm: dst)
		sourceForm: src1;
		combinationRule: Form over; "dst := src1"
		sourceX: start1 - 1; sourceY: 0;
		destX: start - 1; destY: 0;
		height: 1; width: stop - start + 1;
		copyBits;
		sourceForm: src2;
		combinationRule: Form reverse; "dst := dst xor src2"
		sourceX: start2 - 1;
		copyBits! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 1/9/2024 10:46:36'!
replaceBitsFromOld: start to: stop with: anArray1 startingAt: start1	
	| n i₀ i r j q |
	(n := stop - start + 1) > 0 ifFalse: [^ self].
	i₀ := start - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i*32 - i₀ min: n. "r ∈ [1..32], number of high bits of the first word included in the range"
	j := start1.
	r = 32 ifFalse:
		[self bits: r at: start put: (anArray1 bits: r at: j).
		n := n - r. j := j + r. i := i + 1].
	n > 0 ifFalse: [^ self].
	(n >= 32 and: [(j bitAnd: 31) = 1])
		ifTrue: "j is aligned"
			[q := n // 32.
			self replaceFrom: i to: i + q - 1 with: anArray1 startingAt: j - 1 // 32 + 1.
			i := i + q. q := q * 32. n := n - q. j := j + q]
		ifFalse:
			[[n >= 32] whileTrue:
				[self at: i put: (anArray1 bits: 32 at: j).
				n := n - 32. j := j + 32. i := i + 1]].
	n > 0 ifTrue:
		[self bits: n at: i-1*32 + 1 put: (anArray1 bits: n at: j)]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 09:41:14'!
replaceBitsFromOld: start to: stop with: anArray1 startingAt: start1 and: anArray2 startingAt: start2	
	| n i₀ i r j k |
	(n := stop - start + 1) > 0 ifFalse: [^ self].
	i₀ := start - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i*32 - i₀ min: n. "r ∈ [1..32], number of high bits of the first word included in the range"
	j := start1.
	k := start2.
	r = 32 ifFalse:
		[self bits: r at: start put: ((anArray1 bits: r at: j) bitAnd: (anArray2 bits: r at: k)).
		n := n - r. j := j + r. k := k + r. i := i + 1].
	n > 0 ifFalse: [^ self].
	(j bitAnd: 31) = 1
		ifTrue:
			[(k bitAnd: 31) = 1
				ifTrue: "both j and k are aligned"
					[j := j - 1 // 32 + 1. k := k - 1 // 32 + 1.
					[n >= 32] whileTrue:
						[self at: i put: ((anArray1 at: j) bitAnd: (anArray2 at: k)).
						n := n - 32. j := j + 1. k := k + 1. i := i + 1].
					j := j - 1 * 32 + 1. k := k - 1 * 32 + 1]
				ifFalse: "only j is aligned"
					[j := j - 1 // 32 + 1.
					[n >= 32] whileTrue:
						[self at: i put: ((anArray1 at: j) bitAnd: (anArray2 bits: 32 at: k)).
						n := n - 32. j := j + 1. k := k + 32. i := i + 1].
					j := j - 1 * 32 + 1]]
		ifFalse:
			[(k bitAnd: 31) = 1
				ifTrue: "only k is aligned"
					[k := k - 1 // 32 + 1.
					[n >= 32] whileTrue:
						[self at: i put: ((anArray1 bits: 32 at: j) bitAnd: (anArray2 at: k)).
						n := n - 32. j := j + 32. k := k + 1. i := i + 1].
					k := k - 1 * 32 + 1]
				ifFalse: "none is aligned"
					[[n >= 32] whileTrue:
					[self at: i put: ((anArray1 bits: 32 at: j) bitAnd: (anArray2 bits: 32 at: k)).
					n := n - 32. j := j + 32. k := k + 32. i := i + 1]]].
	n > 0 ifTrue:
		[self bits: n at: i-1*32 + 1 put: ((anArray1 bits: n at: j) bitAnd: (anArray2 bits: n at: k))]! !

!WordArray methodsFor: '*gas-low level' stamp: 'len 12/29/2023 09:41:20'!
replaceBitsFromOld: start to: stop with: anArray1 startingAt: start1 xor: anArray2 startingAt: start2	
	| n i₀ r i j k |
	(n := stop - start + 1) > 0 ifFalse: [^ self].
	i₀ := start - 1.
	i := (i₀ bitShift: -5) + 1.
	r := i*32 - i₀ min: n. "r ∈ [1..32], number of high bits of the first word included in the range"
	j := start1.
	k := start2.
	r = 32 ifFalse:
		[self bits: r at: start put: ((anArray1 bits: r at: j) bitXor: (anArray2 bits: r at: k)).
		n := n - r. j := j + r. k := k + r. i := i + 1].
	n > 0 ifFalse: [^ self].
	(j bitAnd: 31) = 1
		ifTrue:
			[(k bitAnd: 31) = 1
				ifTrue: "both j and k are aligned"
					[j := j - 1 // 32 + 1. k := k - 1 // 32 + 1.
					[n >= 32] whileTrue:
						[self at: i put: ((anArray1 at: j) bitXor: (anArray2 at: k)).
						n := n - 32. j := j + 1. k := k + 1. i := i + 1].
					j := j - 1 * 32 + 1. k := k - 1 * 32 + 1]
				ifFalse: "only j is aligned"
					[j := j - 1 // 32 + 1.
					[n >= 32] whileTrue:
						[self at: i put: ((anArray1 at: j) bitXor: (anArray2 bits: 32 at: k)).
						n := n - 32. j := j + 1. k := k + 32. i := i + 1].
					j := j - 1 * 32 + 1]]
		ifFalse:
			[(k bitAnd: 31) = 1
				ifTrue: "only k is aligned"
					[k := k - 1 // 32 + 1.
					[n >= 32] whileTrue:
						[self at: i put: ((anArray1 bits: 32 at: j) bitXor: (anArray2 at: k)).
						n := n - 32. j := j + 32. k := k + 1. i := i + 1].
					k := k - 1 * 32 + 1]
				ifFalse: "none is aligned"
					[[n >= 32] whileTrue:
					[self at: i put: ((anArray1 bits: 32 at: j) bitXor: (anArray2 bits: 32 at: k)).
					n := n - 32. j := j + 32. k := k + 32. i := i + 1]]].
	n > 0 ifTrue:
		[self bits: n at: i-1*32 + 1 put: ((anArray1 bits: n at: j) bitXor: (anArray2 bits: n at: k))]! !

!Number methodsFor: '*gas' stamp: 'len 2/4/2024 19:49:11'!
one
	^ self class one! !

!Number methodsFor: '*gas' stamp: 'len 2/4/2024 19:49:06'!
zero
	^ self class zero! !
