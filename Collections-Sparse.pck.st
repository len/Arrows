'From Cuis 6.0 [latest update: #5053] on 27 April 2022 at 8:02:57 pm'!
'Description '!
!provides: 'Collections-Sparse' 1 1!
SystemOrganization addCategory: #'Collections-Sparse'!


!classDefinition: #SparseArray category: #'Collections-Sparse'!
ArrayedCollection subclass: #SparseArray
	instanceVariableNames: 'contents size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sparse'!
!classDefinition: 'SparseArray class' category: #'Collections-Sparse'!
SparseArray class
	instanceVariableNames: ''!

!classDefinition: #SparseDictionary category: #'Collections-Sparse'!
Collection subclass: #SparseDictionary
	instanceVariableNames: 'associations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sparse'!
!classDefinition: 'SparseDictionary class' category: #'Collections-Sparse'!
SparseDictionary class
	instanceVariableNames: ''!


!SparseArray methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 16:04:54'!
at: anInteger
	(anInteger between: 1 and: size) ifFalse: [^ self errorOutOfBounds].
	^ contents at: anInteger ifAbsent: []! !

!SparseArray methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 16:05:10'!
at: anInteger put: anObject
	(anInteger between: 1 and: size) ifFalse: [^ self errorOutOfBounds].
	^ contents at: anInteger put: anObject! !

!SparseArray methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 16:03:24'!
contents
	^ contents! !

!SparseArray methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 13:52:43'!
size
	^ size! !

!SparseArray methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 16:03:42'!
size: anInteger contents: aSparseDictionary
	size _ anInteger.
	contents _ aSparseDictionary! !

!SparseArray class methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 16:02:35'!
new: anInteger
	^ self new: anInteger contents: SparseDictionary new! !

!SparseArray class methodsFor: 'as yet unclassified' stamp: 'len 4/27/2022 16:03:06'!
new: anInteger contents: aSparseDictionary
	^ self basicNew size: anInteger contents: SparseDictionary new! !

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
