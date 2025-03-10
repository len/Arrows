'From Cuis6.3 [latest update: #6222] on 26 December 2024 at 10:38:37 am'!
'Description This package includes some extra methods for existing collections and some new collection classes.'!
!provides: 'Collections-Extras' 1 23!
SystemOrganization addCategory: #'Collections-Extras'!


!classDefinition: #Iterator category: #'Collections-Extras'!
Collection subclass: #Iterator
	instanceVariableNames: 'block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Extras'!
!classDefinition: 'Iterator class' category: #'Collections-Extras'!
Iterator class
	instanceVariableNames: ''!

!classDefinition: #SortedDictionary category: #'Collections-Extras'!
Collection subclass: #SortedDictionary
	instanceVariableNames: 'associations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Extras'!
!classDefinition: 'SortedDictionary class' category: #'Collections-Extras'!
SortedDictionary class
	instanceVariableNames: ''!


!Iterator commentStamp: '<historical>' prior: 0!
An Iterator is a read-only collection that evaluates a block to yield the elements of the collection.!

!SortedDictionary commentStamp: 'len 8/21/2022 08:24:28' prior: 0!
A SortedDictionary is a dictionary that is kept sorted by an ordering in its keys. The keys must implement #compare:.!

!Iterator methodsFor: 'accessing' stamp: 'len 4/12/2021 12:31:20'!
first
	self do: [:each| ^ each].
	self errorEmptyCollection! !

!Iterator methodsFor: 'accessing'!
identityIndexOf: anElement 
	"Answer the identity index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self identityIndexOf: anElement ifAbsent: [0]! !

!Iterator methodsFor: 'accessing'!
identityIndexOf: anElement ifAbsent: exceptionBlock 
	"Answer the identity index of anElement within the receiver.  If the receiver does
	not contain anElement, answer the result of evaluating the exceptionBlock."

	| index |
	index := 1.
	self do: [ :el | el == anElement ifTrue: [^index].  index := index + 1].
	^exceptionBlock value! !

!Iterator methodsFor: 'accessing'!
indexOf: anElement 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]! !

!Iterator methodsFor: 'accessing'!
indexOf: anElement ifAbsent: exceptionBlock 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer the result of evaluating the exceptionBlock."

	| index |
	index := 1.
	self do: [ :el | el = anElement ifTrue: [^index].  index := index + 1].
	^exceptionBlock value! !

!Iterator methodsFor: 'adding'!
add: anObject
	"Iterators are read-only"
	self shouldNotImplement! !

!Iterator methodsFor: 'converting' stamp: 'len 6/20/2023 11:13:17'!
asOrderedCollection
	"The default implementation sends size to the receiver, and that would make the iteration run twice.
	In this way the new collection might have to grow during computation, but the iteration is performed only once."
	| answer |
	answer := OrderedCollection new.
	self do: [:each| answer add: each].
	^ answer! !

!Iterator methodsFor: 'copying' stamp: 'len 11/30/2024 09:40:22'!
first: n
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."
	| answer |
	answer := self species new: n.
	n = 0 ifTrue: [^ answer].
	self do: [:each|
		answer add: each.
		answer size = n ifTrue: [^ answer]].
	^ self error: 'not enough elements'! !

!Iterator methodsFor: 'enumerating'!
do: aBlock
	block value: aBlock! !

!Iterator methodsFor: 'enumerating'!
findFirst: aBlock
	"Answer the index of the first element of the receiver
	for which aBlock evaluates as true."

	| index |
	index := 1.
	self do: [ :el | (aBlock value: el) ifTrue: [^index].  index := index + 1].
	^0! !

!Iterator methodsFor: 'enumerating'!
findLast: aBlock
	"Answer the index of the last element of the receiver
	for which aBlock evaluates as true."

	| index last |
	index := 1.
	last := 0.
	self do: [ :el | (aBlock value: el) ifTrue: [last := index].  index := index + 1].
	^last! !

!Iterator methodsFor: 'enumerating'!
keysAndValuesDo: aBlock  
	"Evaluate aBlock with each of the receiver's key/value pairs
	(e.g. indexes and elements) as the arguments."

	| index |
	index := 1.
	self do: [:el | aBlock value: index value: el.  index := index + 1]! !

!Iterator methodsFor: 'printing' stamp: 'len 11/11/2024 14:48:08'!
printLimit
	^ 20! !

!Iterator methodsFor: 'printing' stamp: 'len 11/11/2024 14:48:00'!
printOn: aStream
	| count |
	self isEmpty ifTrue: [aStream nextPut: $∅. ^ self].
	count := 0.
	self do: [:element| aStream print: element] separatedBy: [(count := count + 1) > self printLimit ifTrue: [aStream nextPutAll: '...'. ^ self] ifFalse: [aStream nextPutAll: ', ']]! !

!Iterator methodsFor: 'removing'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Iterators are read-only."
	self shouldNotImplement! !

!Iterator methodsFor: 'testing' stamp: 'len 12/19/2023 18:28:19'!
isPair
	"Answer true if the receiver contains exactly two elements."
	^ self sizeEquals: 2! !

!Iterator methodsFor: 'testing' stamp: 'len 12/19/2023 18:28:14'!
isSingleton
	"Answer true if the receiver contains exactly one element."
	^ self sizeEquals: 1! !

!Iterator methodsFor: 'testing' stamp: 'len 12/19/2023 18:26:41'!
sizeEquals: anInteger
	"Answer true if the receiver contains exactly anInteger elements."
	| count |
	count := 0.
	self do: [:each| (count := count + 1) > anInteger ifTrue: [^ false]].
	^ count = anInteger! !

!Iterator methodsFor: 'private'!
block: aBlock
	block := aBlock! !

!Iterator methodsFor: 'private'!
species
	^OrderedCollection! !

!Iterator class methodsFor: 'instance creation' stamp: 'len 2/28/2016 20:32'!
on: aBlock
	^ self new block: aBlock! !

!Iterator class methodsFor: 'instance creation' stamp: 'len 2/14/2016 21:45'!
on: anObject performing: aSymbol
	^ self new block: [:aBlock| anObject perform: aSymbol with: aBlock]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:27:07'!
allButFirst
	^ self allButFirst: 1! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:26:22'!
allButFirst: anInteger
	^ self class associations: (associations allButFirst: anInteger)! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:26:58'!
allButLast
	^ self allButLast: 1! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:26:34'!
allButLast: anInteger
	^ self class associations: (associations allButLast: anInteger)! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:37:26'!
associationAt: key 
	"Answer the association at key.  If key is not found, create an error message."

	^self associationAt: key ifAbsent: [self errorKeyNotFound]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:56'!
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	^ associations findBinary: [:each| key compare: each key] do: [:each| each] ifNone: [:high :low| aBlock value]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/26/2022 22:34:15'!
associations
	^ associations! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 18:43:45'!
at: key
	^ self at: key ifAbsent: [self errorKeyNotFound]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:11'!
at: key ifAbsent: exceptionBlock
	^ associations findBinary: [:each| key compare: each key] do: [:each| each value] ifNone: [:high :low| exceptionBlock value]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:41:36'!
at: key ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	^ presentBlock value: (self at: key ifAbsent: [^ nil])! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:43:08'!
at: key ifPresent: presentBlock ifAbsent: absentBlock
	^ presentBlock value: (self at: key ifAbsent: [^ absentBlock value])! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:26'!
at: key put: value
	associations isEmpty ifTrue: [value isZero ifFalse: [associations add: (Association key: key value: value)]. ^ self].
	associations
		findBinaryIndex: [:each| key compare: each key]
		do: [:index| value isZero ifTrue: [associations removeIndex: index] ifFalse: [(associations at: index) value: value]]
		ifNone: [:low :high| value isZero ifFalse: [associations add: (Association key: key value: value) beforeIndex: high]].
	^ value! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:29:55'!
atRandom: aGenerator
	^ (associations atRandom: aGenerator) value! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:47:35'!
first
	^ associations first value! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:48:14'!
firstKey
	^ associations first key! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:47:42'!
last
	^ associations last value! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:48:07'!
lastKey
	^ associations last key! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/26/2022 22:28:01'!
removeKey: key 
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key."

	^self removeKey: key ifAbsent: [self errorKeyNotFound]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:41'!
removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	associations
		findBinaryIndex: [:each| key compare: each key]
		do: [:index| associations removeIndex: index]
		ifNone: [:low :high| aBlock value]! !

!SortedDictionary methodsFor: 'accessing' stamp: 'len 4/26/2022 22:33:39'!
size
	^ associations size! !

!SortedDictionary methodsFor: 'arithmetic' stamp: 'len 12/17/2024 16:38:19'!
* aNumber
	^ self collectNonZero: [:each| each * aNumber]! !

!SortedDictionary methodsFor: 'arithmetic' stamp: 'len 8/21/2022 08:25:30'!
+ aSortedDictionary
	| newAssociations 	stream1 stream2 |
	newAssociations _ OrderedCollection new: associations size + aSortedDictionary associations size.
	newAssociations resetTo: 1.
	stream1 _ associations readStream.
	stream2 _ aSortedDictionary associations readStream.
	[stream1 atEnd or: [stream2 atEnd]]
		whileFalse:
			[| cmp |
			cmp _ stream1 peek key compare: stream2 peek key.
			cmp < 0
				ifTrue: [newAssociations add: stream1 next copy]
				ifFalse: [cmp > 0
					ifTrue: [newAssociations add: stream2 next copy]
					ifFalse:
						[| a b newValue |
						a _ stream1 next.
						b _ stream2 next.
						(newValue _ a value + b value) isZero
							ifFalse: [newAssociations add: (a copy value: newValue)]]]].
	[stream1 atEnd] whileFalse: [newAssociations add: stream1 next copy].
	[stream2 atEnd] whileFalse: [newAssociations add: stream2 next copy].
	^ self class associations: newAssociations! !

!SortedDictionary methodsFor: 'arithmetic' stamp: 'len 8/21/2022 08:25:44'!
- aSortedDictionary
	| newAssociations 	stream1 stream2 |
	newAssociations _ OrderedCollection new: associations size + aSortedDictionary associations size.
	newAssociations resetTo: 1.
	stream1 _ associations readStream.
	stream2 _ aSortedDictionary associations readStream.
	[stream1 atEnd or: [stream2 atEnd]]
		whileFalse:
			[| cmp |
			cmp _ stream1 peek key compare: stream2 peek key.
			cmp < 0
				ifTrue: [newAssociations add: stream1 next copy]
				ifFalse: [cmp > 0
					ifTrue:
						[| b |
						b _ stream2 next.
						newAssociations add: (b copy value: b value negated)]
					ifFalse:
						[| a b newValue |
						a _ stream1 next.
						b _ stream2 next.
						(newValue _ a value - b value) isZero
							ifFalse: [newAssociations add: (a copy value: newValue)]]]].
	[stream1 atEnd] whileFalse: [newAssociations add: stream1 next copy].
	[stream2 atEnd] whileFalse: [| b | b _ stream2 next. newAssociations add: (b copy value: b value negated)].
	^ self class associations: newAssociations! !

!SortedDictionary methodsFor: 'arithmetic' stamp: 'len 4/27/2022 13:47:27'!
/ aNumber
	^ self collect: [:each| each / aNumber]! !

!SortedDictionary methodsFor: 'comparing' stamp: 'len 4/26/2022 22:32:45'!
= anObject
	^ self class = anObject class and: [associations = anObject associations]! !

!SortedDictionary methodsFor: 'comparing' stamp: 'len 4/26/2022 22:33:17'!
hash
	^ associations hash! !

!SortedDictionary methodsFor: 'copying' stamp: 'len 11/22/2022 15:53:25'!
postCopy
	associations _ associations collect: [:each| each copy]! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 4/27/2022 13:14:43'!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's key/value associations."
	associations do: aBlock! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 12/17/2024 16:38:08'!
collect: aBlock 
	^ self collectNonZero: aBlock! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 12/17/2024 16:37:50'!
collectNonZero: aBlock 
	| newAssociations |
	newAssociations := OrderedCollection new: associations size.
	newAssociations resetTo: 1.
	associations do: [:each| | newValue |
		(newValue := aBlock value: each value) isZero
			ifFalse: [newAssociations add: (each copy value: newValue)]].
	^ self class associations: newAssociations! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 4/26/2022 22:24:33'!
do: aBlock
	self valuesDo: aBlock! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 4/26/2022 22:33:22'!
keysDo: aBlock
	associations do: [:each| aBlock value: each key]! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 4/27/2022 13:14:14'!
select: aBlock 
	"Evaluate aBlock with each of my values as the argument. Collect into a new
	dictionary only those associations for which aBlock evaluates to true."
	| newAssociations |
	newAssociations _ OrderedCollection new.
	newAssociations resetTo: 1.
	newAssociations do: [:each| (aBlock value: each value) ifTrue: [newAssociations add: each copy]].
	^ self class associations: newAssociations! !

!SortedDictionary methodsFor: 'enumerating' stamp: 'len 4/26/2022 22:33:44'!
valuesDo: aBlock
	associations do: [:each| aBlock value: each value]! !

!SortedDictionary methodsFor: 'testing' stamp: 'len 4/27/2022 07:49:03'!
isEmpty
	^ associations size = 0! !

!SortedDictionary methodsFor: 'private' stamp: 'len 4/27/2022 19:15:39'!
associations: aCollection
	associations _ aCollection! !

!SortedDictionary methodsFor: 'private' stamp: 'len 4/27/2022 07:36:58'!
errorKeyNotFound
	self error: 'key not found'! !

!SortedDictionary class methodsFor: 'instance creation' stamp: 'len 4/26/2022 22:35:43'!
associations: aCollection
	^ self basicNew associations: aCollection! !

!SortedDictionary class methodsFor: 'instance creation' stamp: 'len 4/26/2022 22:36:03'!
new
	^ self associations: OrderedCollection new! !

!SortedDictionary class methodsFor: 'instance creation' stamp: 'len 4/26/2022 22:36:12'!
new: anInteger
	^ self associations: (OrderedCollection new: anInteger)! !

!Collection methodsFor: '*collections-extras' stamp: 'len 10/20/2023 16:59:19'!
\ aCollection
	"Answer the subcollection of elements of the receiver that are not in aCollection."
	^ self reject: [:each| aCollection includes: each]! !

!Collection methodsFor: '*collections-extras' stamp: 'len 2/6/2024 22:28:37'!
^ anInteger
	"Answer the receiver concatenated with itself anInteger times."
	anInteger = 1 ifTrue: [^ self].
	anInteger = 0 ifTrue: [^ self copyBlank].
	anInteger < 0 ifTrue: [^ self error: 'negative exponent'].
	^ anInteger even ifTrue: [self , self ^ (anInteger // 2)] ifFalse: [self , self ^ (anInteger // 2) , self]! !

!Collection methodsFor: '*collections-extras' stamp: 'len 4/30/2024 10:51:22'!
any
	^ self anyIfNone: [self errorEmptyCollection]! !

!Collection methodsFor: '*collections-extras' stamp: 'len 2/18/2003 21:40'!
anyIfNone: aBlock
	"Answer a representative sample of the receiver. This method can
	be helpful when needing to preinfer the nature of the contents of 
	semi-homogeneous collections."

	self do: [:each | ^ each].
	^aBlock value! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/3/2024 08:34:53'!
atAll: aCollection put: anObject 
	"Put anObject at every index specified by the elements of aCollection."

	aCollection do: [:index | self at: index put: anObject].
	^ anObject! !

!Collection methodsFor: '*collections-extras' stamp: 'len 2/6/2024 22:28:37'!
copyBlank
	^ self species new! !

!Collection methodsFor: '*collections-extras' stamp: 'len 5/1/2022 07:51:04'!
count: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count _ 0.
	self do: [:each| (aBlock value: each) ifTrue: [count _ count + 1]].
	^ count! !

!Collection methodsFor: '*collections-extras' stamp: 'len 2/19/2017 10:40:52'!
intersects: aCollection
	^ self anySatisfy: [:any| aCollection includes: any]! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/19/2023 18:28:29'!
isPair
	"Answer true if the receiver contains exactly two elements."
	^ self size = 2! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/19/2023 18:28:36'!
isSingleton
	"Answer true if the receiver contains exactly one element."
	^ self size = 1! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/25/2024 09:01:09'!
times: aCollection collect: aBlock
	"Collect and return the result of evaluating aBlock with all pairs (x,y) for x in the receiver and y in aCollection."
	| answer |
	answer := OrderedCollection new: self size * aCollection size.
	self do: [:x| aCollection do: [:y| answer add: (aBlock value: x value: y)]].
	^ answer! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/25/2024 09:01:34'!
times: aCollection do: aBlock
	"Evaluate aBlock with all pairs (x,y) for x in the receiver and y in aCollection."
	self do: [:x| aCollection do: [:y| aBlock value: x value: y]]! !

!Collection methodsFor: '*collections-extras' stamp: 'len 8/12/2022 13:14:15'!
∩ aCollection
	^ self intersection: aCollection! !

!Collection methodsFor: '*collections-extras' stamp: 'len 8/12/2022 13:14:27'!
∪ aCollection
	^ self union: aCollection! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/10/2023 10:36:24'!
⊆ aCollection
	^ aCollection includesAllOf: self! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/10/2023 10:36:24'!
⊇ aCollection
	^ aCollection ⊆ self! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/10/2023 10:36:24'!
⊊ aCollection
	^ self ~= aCollection and: [self ⊆ aCollection]! !

!Collection methodsFor: '*collections-extras' stamp: 'len 12/4/2023 09:55:40'!
⊋ aCollection
	^ aCollection ⊊ self! !

!Collection class methodsFor: '*collections-extras'!
accumulate: iteratorBlock
	"Return a new instance of myself by evaluating the iteratorBlock.
	Examples:
		| x oc set |
		x := #( 1 2 3 4 1 ).
		oc := OrderedCollection accumulate: [ :incBlock | x do: incBlock].
		Transcript cr; show: oc printString.
		set := Set accumulate: [ :incBlock | x do: incBlock].
		Transcript cr; show: set printString"

	| collection | 
	collection := self new.
	iteratorBlock value: [ :element | collection add: element].
	^collection! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 5/28/2023 19:13:41'!
<= aCollection
	"Answer true if the receiver is before aCollection in lexicographic order."
	1 to: (self size min: aCollection size) do: [:i|
		| a b |
		a := self at: i.
		b := aCollection at: i.
		a > b ifTrue: [^ false].
		a = b ifFalse: [^ true]].
	^ self size <= aCollection size! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 2/21/2017 18:59:40'!
combinations: r
	^ Iterator on: [:aBlock| self combinations: r atATimeDo: [:each| aBlock value: each copy]]! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 3/3/2016 23:07'!
copyWithoutIndex: index 
	| answer |
	answer _ self species new: self size - 1.
	answer replaceFrom: 1 to: index - 1 with: self startingAt: 1.
	answer replaceFrom: index to: answer size with: self startingAt: index + 1.
	^ answer! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 6/4/2023 17:45:41'!
indexOf: anElement ifPresent: aBlock
	^ aBlock value: (self indexOf: anElement ifAbsent: [^ 0])! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 2/21/2017 19:00:52'!
permutations
	^ Iterator on: [:aBlock| self permutationsDo: [:each| aBlock value: each copy]]! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 11/18/2016 08:58:16'!
sum
	| answer |
	answer _ self at: 1.
	2 to: self size do: [:i| answer _ answer + (self at: i)].
	^ answer! !

!SequenceableCollection methodsFor: '*collections-extras' stamp: 'len 12/10/2023 10:36:24'!
⊆ aCollection
	"Answer true if the receiver is a subcollection of aCollection."
	(aCollection isKindOf: self species) ifFalse: [^ super ⊆ aCollection].
	self size <= aCollection size ifFalse: [^ false].
	^ (aCollection indexOfSubCollection: self startingAt: 1) > 0! !

!ArrayedCollection methodsFor: '*collections-extras' stamp: 'len 12/25/2024 09:03:14'!
times: aCollection collect: aBlock
	"Collect and return the result of evaluating aBlock with all pairs (x,y) for x in the receiver and y in aCollection."
	| answer i |
	answer := self copyBlank: self size * aCollection size.
	i := 0.
	self do: [:x| aCollection do: [:y| answer at: (i := i + 1) put: (aBlock value: x value: y)]].
	^ answer! !

!OrderedCollection class methodsFor: '*collections-extras' stamp: 'len 2/9/2024 10:00:49'!
new: anInteger
	^self basicNew setCollection: (Array new: anInteger)! !

!Text methodsFor: '*collections-extras' stamp: 'len 2/7/2024 15:42:27'!
copyBlank: anInteger
	^ Text fromString: (string class new: anInteger)! !

!Bag methodsFor: '*collections-extras' stamp: 'len 5/1/2022 10:25:40'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into a collection that is like the receiver and nswer the new collection.
	NOTE: This method assumes aBlock is a function."
	| answer |
	answer _ self species new.
	self withOccurrencesDo: [:each :times| answer add: (aBlock value: each) withOccurrences: times].
	^ answer! !

!Bag methodsFor: '*collections-extras' stamp: 'len 5/1/2022 10:22:53'!
count: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."
	| count |
	count _ 0.
	self withOccurrencesDo: [:each :times| (aBlock value: each) ifTrue: [count _ count + times]].
	^ count! !

!Bag methodsFor: '*collections-extras' stamp: 'len 12/10/2016 11:26:24'!
withOccurrencesDo: aBlock
	contents associationsDo: [:each| aBlock value: each key value: each value]! !

!Set methodsFor: '*collections-extras' stamp: 'len 4/27/2016 02:28'!
add: newObject ifAbsent: aBlock
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject. aBlock value].
	^ newObject! !

!Set methodsFor: '*collections-extras' stamp: 'len 5/12/2016 23:24'!
add: newObject ifPresent: aBlock
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: newObject.
	(array at: index) isNil
		ifTrue: [self atNewIndex: index put: newObject]
		ifFalse: [aBlock value].
	^ newObject! !

!Set methodsFor: '*collections-extras' stamp: 'len 12/23/2021 13:58:57'!
at: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"
	| index |
	^(index _ self scanFor: anObject) = 0 ifFalse: [array at: index]! !

!Set methodsFor: '*collections-extras' stamp: 'len 12/23/2021 13:59:04'!
at: anObject ifAbsent: exceptionBlock
	| index |
	^ ((index _ self scanFor: anObject) = 0 ifFalse: [array at: index]) ifNil: exceptionBlock! !

!Set methodsFor: '*collections-extras' stamp: 'len 12/23/2021 13:59:10'!
at: anObject ifAbsentPut: aBlock
	"Answer an object in the receiver that is equal to anObject, or evaluate aBlock to get one and add it to the receiver,
	Relies heavily on hash properties, it is important that the following holds: aBlock value = anObject."
	| index |
	anObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: anObject.
	^ (array at: index) ifNil: [self atNewIndex: index put: aBlock value]! !

!Set class methodsFor: '*collections-extras' stamp: 'len 2/9/2024 10:03:42'!
new: anInteger
	^self newWithRoomForMoreThan: anInteger! !
