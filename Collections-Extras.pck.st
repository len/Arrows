'From Cuis 6.0 [latest update: #5053] on 30 April 2022 at 12:43:51 pm'!
'Description '!
!provides: 'Collections-Extras' 1 2!
SystemOrganization addCategory: 'Collections-Extras'!


!classDefinition: #Iterator category: 'Collections-Extras'!
Collection subclass: #Iterator
	instanceVariableNames: 'block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Extras'!
!classDefinition: 'Iterator class' category: 'Collections-Extras'!
Iterator class
	instanceVariableNames: ''!

!classDefinition: #SparseDictionary category: 'Collections-Extras'!
Collection subclass: #SparseDictionary
	instanceVariableNames: 'associations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Extras'!
!classDefinition: 'SparseDictionary class' category: 'Collections-Extras'!
SparseDictionary class
	instanceVariableNames: ''!


!Iterator commentStamp: '<historical>' prior: 0!
An Iterator is a read-only collection that evaluates a block to yield the elements of the collection.!

!Iterator methodsFor: 'printing' stamp: 'len 5/10/2020 10:16:19'!
printOn: aStream
	| count |
	aStream nextPut: ${.
	count _ 0.
	self do: [:element| aStream print: element] separatedBy: [(count _ count + 1) > 10 ifTrue: [aStream nextPutAll: '...)'. ^ self] ifFalse: [aStream nextPutAll: ', ']].
	aStream nextPut: $}! !

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

!Iterator methodsFor: 'copying' stamp: 'len 10/27/2016 15:39'!
first: n
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."
	| answer |
	answer _ self species new: n.
	self do: [:each|
		answer size = n ifTrue: [^ answer].
		answer add: each].
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

!Iterator methodsFor: 'removing'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Iterators are read-only."
	self shouldNotImplement! !

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

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:27:07'!
allButFirst
	^ self allButFirst: 1! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:26:22'!
allButFirst: anInteger
	^ self class associations: (associations allButFirst: anInteger)! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:26:58'!
allButLast
	^ self allButLast: 1! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 13:26:34'!
allButLast: anInteger
	^ self class associations: (associations allButLast: anInteger)! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:37:26'!
associationAt: key 
	"Answer the association at key.  If key is not found, create an error message."

	^self associationAt: key ifAbsent: [self errorKeyNotFound]! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:56'!
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	^ associations findBinary: [:each| key compare: each key] do: [:each| each] ifNone: [:high :low| aBlock value]! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/26/2022 22:34:15'!
associations
	^ associations! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 18:43:45'!
at: key
	^ self at: key ifAbsent: [self errorKeyNotFound]! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:11'!
at: key ifAbsent: exceptionBlock
	^ associations findBinary: [:each| key compare: each key] do: [:each| each value] ifNone: [:high :low| exceptionBlock value]! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:41:36'!
at: key ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	^ presentBlock value: (self at: key ifAbsent: [^ nil])! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:43:08'!
at: key ifPresent: presentBlock ifAbsent: absentBlock
	^ presentBlock value: (self at: key ifAbsent: [^ absentBlock value])! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:26'!
at: key put: value
	associations isEmpty ifTrue: [value isZero ifFalse: [associations add: (Association key: key value: value)]. ^ self].
	associations
		findBinaryIndex: [:each| key compare: each key]
		do: [:index| value isZero ifTrue: [associations removeIndex: index] ifFalse: [(associations at: index) value: value]]
		ifNone: [:low :high| value isZero ifFalse: [associations add: (Association key: key value: value) beforeIndex: high]].
	^ value! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:29:55'!
atRandom: aGenerator
	^ (associations atRandom: aGenerator) value! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:47:35'!
first
	^ associations first value! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:48:14'!
firstKey
	^ associations first key! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:47:42'!
last
	^ associations last value! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 07:48:07'!
lastKey
	^ associations last key! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/26/2022 22:28:01'!
removeKey: key 
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key."

	^self removeKey: key ifAbsent: [self errorKeyNotFound]! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/27/2022 19:07:41'!
removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	associations
		findBinaryIndex: [:each| key compare: each key]
		do: [:index| associations removeIndex: index]
		ifNone: [:low :high| aBlock value]! !

!SparseDictionary methodsFor: 'accessing' stamp: 'len 4/26/2022 22:33:39'!
size
	^ associations size! !

!SparseDictionary methodsFor: 'arithmetic' stamp: 'len 4/27/2022 13:49:29'!
* aNumber
	^ self collect: [:each| each * aNumber]! !

!SparseDictionary methodsFor: 'arithmetic' stamp: 'len 4/27/2022 19:08:53'!
+ aSparseDictionary
	| newAssociations 	stream1 stream2 |
	newAssociations _ OrderedCollection new: associations size + aSparseDictionary associations size.
	newAssociations resetTo: 1.
	stream1 _ associations readStream.
	stream2 _ aSparseDictionary associations readStream.
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

!SparseDictionary methodsFor: 'arithmetic' stamp: 'len 4/27/2022 19:37:28'!
- aSparseDictionary
	| newAssociations 	stream1 stream2 |
	newAssociations _ OrderedCollection new: associations size + aSparseDictionary associations size.
	newAssociations resetTo: 1.
	stream1 _ associations readStream.
	stream2 _ aSparseDictionary associations readStream.
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

!SparseDictionary methodsFor: 'arithmetic' stamp: 'len 4/27/2022 13:47:27'!
/ aNumber
	^ self collect: [:each| each / aNumber]! !

!SparseDictionary methodsFor: 'arithmetic' stamp: 'len 4/27/2022 16:53:29'!
negated
	^ self collect: [:each| each negated]! !

!SparseDictionary methodsFor: 'comparing' stamp: 'len 4/26/2022 22:32:45'!
= anObject
	^ self class = anObject class and: [associations = anObject associations]! !

!SparseDictionary methodsFor: 'comparing' stamp: 'len 4/26/2022 22:33:17'!
hash
	^ associations hash! !

!SparseDictionary methodsFor: 'copying' stamp: 'len 4/27/2022 13:12:08'!
copy
	^ self class associations: (associations collect: [:each| each copy])! !

!SparseDictionary methodsFor: 'enumerating' stamp: 'len 4/27/2022 13:14:43'!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's key/value associations."
	associations do: aBlock! !

!SparseDictionary methodsFor: 'enumerating' stamp: 'len 4/27/2022 13:18:07'!
collect: aBlock 
	| newAssociations |
	newAssociations _ OrderedCollection new: associations size.
	newAssociations resetTo: 1.
	associations do: [:each| | newValue |
		(newValue _ aBlock value: each value) isZero
			ifFalse: [newAssociations add: (each copy value: newValue)]].
	^ self class associations: newAssociations! !

!SparseDictionary methodsFor: 'enumerating' stamp: 'len 4/26/2022 22:24:33'!
do: aBlock
	self valuesDo: aBlock! !

!SparseDictionary methodsFor: 'enumerating' stamp: 'len 4/26/2022 22:33:22'!
keysDo: aBlock
	associations do: [:each| aBlock value: each key]! !

!SparseDictionary methodsFor: 'enumerating' stamp: 'len 4/27/2022 13:14:14'!
select: aBlock 
	"Evaluate aBlock with each of my values as the argument. Collect into a new
	dictionary only those associations for which aBlock evaluates to true."
	| newAssociations |
	newAssociations _ OrderedCollection new.
	newAssociations resetTo: 1.
	newAssociations do: [:each| (aBlock value: each value) ifTrue: [newAssociations add: each copy]].
	^ self class associations: newAssociations! !

!SparseDictionary methodsFor: 'enumerating' stamp: 'len 4/26/2022 22:33:44'!
valuesDo: aBlock
	associations do: [:each| aBlock value: each value]! !

!SparseDictionary methodsFor: 'testing' stamp: 'len 4/27/2022 07:49:03'!
isEmpty
	^ associations size = 0! !

!SparseDictionary methodsFor: 'private' stamp: 'len 4/27/2022 19:15:39'!
associations: aCollection
	associations _ aCollection! !

!SparseDictionary methodsFor: 'private' stamp: 'len 4/27/2022 07:36:58'!
errorKeyNotFound
	self error: 'key not found'! !

!SparseDictionary class methodsFor: 'instance creation' stamp: 'len 4/26/2022 22:35:43'!
associations: aCollection
	^ self basicNew associations: aCollection! !

!SparseDictionary class methodsFor: 'instance creation' stamp: 'len 4/26/2022 22:36:03'!
new
	^ self associations: OrderedCollection new! !

!SparseDictionary class methodsFor: 'instance creation' stamp: 'len 4/26/2022 22:36:12'!
new: anInteger
	^ self associations: (OrderedCollection new: anInteger)! !

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
