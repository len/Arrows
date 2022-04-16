'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 2:13:04 pm'!
'Description '!
!provides: 'Tableaux' 1 1!
!requires: 'Algebra' 1 0 nil!
SystemOrganization addCategory: #Tableaux!


!classDefinition: #YoungTableau category: #Tableaux!
Collection subclass: #YoungTableau
	instanceVariableNames: 'rows'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tableaux'!
!classDefinition: 'YoungTableau class' category: #Tableaux!
YoungTableau class
	instanceVariableNames: ''!

!classDefinition: #SemistandardTableaux category: #Tableaux!
Structure subclass: #SemistandardTableaux
	instanceVariableNames: 'shape alphabet'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tableaux'!
!classDefinition: 'SemistandardTableaux class' category: #Tableaux!
SemistandardTableaux class
	instanceVariableNames: ''!

!classDefinition: #StandardTableaux category: #Tableaux!
SemistandardTableaux subclass: #StandardTableaux
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tableaux'!
!classDefinition: 'StandardTableaux class' category: #Tableaux!
StandardTableaux class
	instanceVariableNames: ''!

!classDefinition: #YoungDiagram category: #Tableaux!
Object subclass: #YoungDiagram
	instanceVariableNames: 'shape'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tableaux'!
!classDefinition: 'YoungDiagram class' category: #Tableaux!
YoungDiagram class
	instanceVariableNames: ''!


!YoungTableau methodsFor: 'printing' stamp: 'len 5/17/2020 12:36:42'!
printOn: aStream
	self rows do: [:row|
		aStream nextPut: $[.
		row do: [:each| aStream print: each] separatedBy: [aStream nextPut: $|].
		aStream nextPut: $]] separatedBy: [aStream newLine; space]! !

!YoungDiagram methodsFor: 'as yet unclassified' stamp: 'len 5/17/2020 12:36:48'!
printOn: aStream
	self shape do: [:k|
		aStream nextPut: $[.
		(1 to: k) do: [:ignore| aStream space] separatedBy: [aStream nextPut: $|].
		aStream nextPut: $]] separatedBy: [aStream newLine; space]! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 8/1/2016 02:52'!
at: aPoint
	^ (rows at: aPoint x) at: aPoint y! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 8/1/2016 02:52'!
at: aPoint put: anObject
	^ (rows at: aPoint x) at: aPoint y put: anObject! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 8/1/2016 03:19'!
diagram
	^ YoungDiagram shape: self shape! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 4/5/2018 15:20:19'!
height
	^ rows size! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 7/12/2016 00:07'!
rank
	^ rows sum: [:each| each size]! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 7/11/2016 21:47'!
rows
	^ rows! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 7/11/2016 22:34'!
shape
	^ rows collect: [:each| each size]! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 7/12/2016 00:06'!
weight
	| answer |
	answer _ Array new: self rank.
	answer atAllPut: 0.
	self do: [:each| answer at: each put: (answer at: each) + 1].
	^ answer! !

!YoungTableau methodsFor: 'accessing' stamp: 'len 4/5/2018 15:20:10'!
width
	^ rows first size! !

!YoungTableau methodsFor: 'arithmetic' stamp: 'len 4/5/2018 15:43:58'!
negated
	self height = 1 ifTrue: [^ self].
	^ self copy at: 1@1 put: (self at: 1@2); at: 1@2 put: (self at: 1@1); yourself! !

!YoungTableau methodsFor: 'arithmetic' stamp: 'len 7/11/2016 21:56'!
sign
	self notYetImplemented! !

!YoungTableau methodsFor: 'copying' stamp: 'len 7/11/2016 22:07'!
copy
	^ self class rows: (rows collect: [:each| each copy])! !

!YoungTableau methodsFor: 'enumerating' stamp: 'len 7/11/2016 22:11'!
do: aBlock
	rows do: [:each| each do: aBlock]! !

!YoungTableau methodsFor: 'enumerating' stamp: 'len 8/1/2016 02:54'!
withIndexDo: aBlock
	rows withIndexDo: [:row :i| row withIndexDo: [:each :j| aBlock value: each value: i@j]]! !

!YoungTableau methodsFor: 'initialization' stamp: 'len 7/11/2016 21:37'!
rows: anArray
	rows _ anArray! !

!YoungTableau methodsFor: 'testing' stamp: 'len 8/1/2016 02:36'!
isSemistandard
	self withIndexDo: [:each :index|
		(index x > 1 and: [each < (self at: index x - 1 @ index y)])
			ifTrue: [^ false].
		(index y > 1 and: [each < (self at: index x @ (index y - 1))])
			ifTrue: [^ false]].
	^ true! !

!YoungTableau methodsFor: 'testing' stamp: 'len 7/12/2016 00:15'!
isStandard
	"if it is semistandard and content = (1,1,..,1)"
	self withIndexDo: [:each :index|
		(index x > 1 and: [each <= (self at: index x - 1 @ index y)])
			ifTrue: [^ false].
		(index y > 1 and: [each <= (self at: index x @ (index y - 1))])
			ifTrue: [^ false]].
	^ true! !

!YoungTableau class methodsFor: 'instance creation' stamp: 'len 7/11/2016 21:47'!
rows: anArray
	^ self basicNew rows: anArray! !

!YoungTableau class methodsFor: 'instance creation' stamp: 'len 7/11/2016 22:33'!
shape: anArray
	^ self rows: (anArray collect: [:each| Array new: each])! !

!SemistandardTableaux methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 02:31'!
alphabet
	^ alphabet! !

!SemistandardTableaux methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 02:32'!
alphabet: anArray
	alphabet _ anArray! !

!SemistandardTableaux methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:25'!
diagram
	^ YoungDiagram shape: shape! !

!SemistandardTableaux methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:29'!
shape
	^ shape! !

!SemistandardTableaux methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:29'!
shape: aPartition
	shape _ aPartition! !

!SemistandardTableaux class methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:28'!
alphabet: anArray shape: aPartition
	^ self new alphabet: anArray; shape: aPartition! !

!SemistandardTableaux class methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:28'!
new: anInteger shape: aPartition
	^ self alphabet: (1 to: anInteger) shape: aPartition! !

!StandardTableaux methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:24'!
size
	| answer Y |
	answer _ alphabet size factorial.
	Y _ self diagram.
	Y do: [:each|
		answer _ answer / (Y hookLengthAt: each)].
	^ answer
! !

!YoungDiagram methodsFor: 'as yet unclassified' stamp: 'len 2/21/2017 18:40:23'!
conjugate
	"Answer the Young diagram of the conjugate partition."
	^ self class shape: ((1 to: shape size) collect: [:k| shape findLast: [:i| i >= k]])! !

!YoungDiagram methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:17'!
hookLengthAt: aPoint
	| answer |
	answer _ (shape at: aPoint x) - aPoint y. "boxes to the right"
	aPoint x + 1 to: shape size do: [:i|
		(shape at: i) >= aPoint y
			ifFalse: [^ answer].
		answer _ answer + 1].
	^ answer! !

!YoungDiagram methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:12'!
shape
	^ shape! !

!YoungDiagram methodsFor: 'as yet unclassified' stamp: 'len 8/1/2016 03:12'!
shape: aPartition
	shape _ aPartition! !

!YoungDiagram class methodsFor: 'instance creation' stamp: 'len 8/1/2016 03:13'!
shape: aPartition
	^ self new shape: aPartition! !
