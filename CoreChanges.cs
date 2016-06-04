'From Cuis 4.2 of 25 July 2013 [latest update: #2752] on 4 June 2016 at 5:02:20.264713 am'!

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/30/2016 21:32'!
doesNotExist
	"
	Character doesNotExist
	"
	^ self value: 16r83! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/30/2016 21:33'!
exists
	"
	Character exists
	"
	^ self value: 16r82! !


!Collection methodsFor: 'enumerating' stamp: 'len 2/17/2016 06:27'!
symmetricDifference: aSet 
	^ (self difference: aSet) union: (aSet difference: self)! !

!Collection methodsFor: 'statistics' stamp: 'len 4/29/2016 21:58'!
argmax: aBlock
	"Answer the argument (element of the receiver) that maximizes aBlock."
	^ self detectMax: aBlock! !

!Collection methodsFor: 'statistics' stamp: 'len 4/29/2016 21:58'!
argmin: aBlock
	"Answer the argument (element of the receiver) that mminimizes aBlock."
	^ self detectMin: aBlock! !

!Collection methodsFor: 'statistics' stamp: 'len 2/24/2016 06:13'!
max: aBlock
	| answer |
	self emptyCheck.
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer! !

!Collection methodsFor: 'statistics' stamp: 'len 2/24/2016 06:13'!
min: aBlock
	| answer |
	self emptyCheck.
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer min: (aBlock value: each)]].
	^ answer! !

!Collection methodsFor: 'statistics' stamp: 'len 5/27/2016 21:24'!
product
	"Compute the product of all the elements in the receiver"

	^self collect: [ :each | each ] andFold: [ :a :b | a * b]! !

!Collection methodsFor: 'statistics' stamp: 'len 5/27/2016 21:26'!
product: aBlock
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 1 is a good initial value for the product."
	^self collect: aBlock andFold: [ :a :b | a * b ]! !


!Collection class methodsFor: '*squeakenh-misc-instance creation'!
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

!Collection class methodsFor: '*squeakenh-misc-instance creation'!
accumulate: iteratorSymbol on: collection
	"Return a new instance of myself by sending the iteratorSymbol to the collection.
	Examples:
		| x oc set |
		x := #( 1 2 3 4 1 ).
		oc := OrderedCollection accumulate: #do: on: x.
		Transcript cr; show: oc printString.
		set := Set accumulate: #do: on: x.
		Transcript cr; show: set printString"

	| newCollection | 
	newCollection := self new.
	collection perform: iteratorSymbol with: [ :element | newCollection add: element].
	^newCollection! !


!Bag methodsFor: '*squeakenh-misc-frequencies'!
frequencyDistribution
	"Return a dictionary mapping each different thing in the bag to its percentage of the total."
	| total d |
	total := contents inject: 0 into: [ :subTotal :next | subTotal + next].
	d := Dictionary new.
	contents associationsDo: [ :assoc |
		d at: assoc key put: (assoc value/total) asFloat].
	^d! !

!Bag methodsFor: '*squeakenh-misc-removing'!
remove: oldObject howMany: count
	"Remove count occurrences of oldObject from the receiver.  
	Answer the number of remaining elements."

	^self remove: oldObject howMany: count ifAbsent: [self notFoundError]! !

!Bag methodsFor: '*squeakenh-misc-removing'!
remove: oldObject howMany: count ifAbsent: exceptionBlock
	"Remove count occurrences of oldObject from the receiver.  Answer the number of remaining elements.  If the receiver contains less than count occurrences of oldObject, evalaute the exception block."

	^(self includes: oldObject)
		ifTrue: [| currentCount |
				(currentCount := contents at: oldObject) < count
					ifTrue: [exceptionBlock value]
					ifFalse: [currentCount = count
								ifTrue: [contents removeKey: oldObject.
										0]
								ifFalse: [currentCount := currentCount - count.
										contents at: oldObject put: currentCount.
										currentCount]]]
		ifFalse: [exceptionBlock value]! !

!Bag methodsFor: '*squeakenh-misc-removing'!
removeAllOccurrences: obj ifNone: aBlock
	"Remove all occurrences of obj; evaluate aBlock if there were none."
	^contents removeKey: obj ifAbsent: aBlock! !

!Bag methodsFor: '*squeakenh-misc-removing'!
removeOne: oldObject
	"Remove oldObject as one of the receiver's elements.  If several of the elements are equal to oldObject, only one is removed.  Answer the number of remaining elements."

	^self remove: oldObject howMany: 1! !

!Bag methodsFor: '*squeakenh-misc-removing'!
removeOne: oldObject ifAbsent: exceptionBlock
	"Remove oldObject as one of the receiver's elements.  If several of the elements are equal to oldObject, only one is removed.  Answer the number of remaining elements.  If the receiver does not contain oldObject, evalaute the exception block."

	^self remove: oldObject howMany: 1 ifAbsent: exceptionBlock! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'len 3/23/2016 05:04'!
at: anInteger add: anObject
	^ self at: anInteger put: (self at: anInteger) + anObject! !

!SequenceableCollection methodsFor: 'copying' stamp: 'len 3/3/2016 23:07'!
copyWithoutIndex: index 
	| answer |
	answer _ self species new: self size - 1.
	answer replaceFrom: 1 to: index - 1 with: self startingAt: 1.
	answer replaceFrom: index to: answer size with: self startingAt: index + 1.
	^ answer! !

!SequenceableCollection methodsFor: 'copying' stamp: 'len 4/18/2016 22:08'!
shuffledBy: aGenerator
	"To answer a mutable collection when receiver is, for example, an Interval."
	^ (self collect: [ :each | each ]) shuffleBy: aGenerator! !


!String methodsFor: 'testing' stamp: 'len 3/22/2016 21:11'!
isAlphaNumeric
	^ self allSatisfy: [:each| each isAlphaNumeric]! !


!Number methodsFor: 'mathematical functions' stamp: 'len 2/11/2016 21:21'!
lg
	^ self log: 2! !


!Integer methodsFor: 'arithmetic' stamp: 'len 2/27/2016 04:34'!
karatsuba2: anInteger
	| high1 high2 low1 low2 m m2 mask z0 z1 z2 |
	m _ self highBit max: anInteger highBit.
	m < 1600 ifTrue: [^ self * anInteger].
	m2 _ m // 2 alignedTo: 256.
	mask _ (1 bitShift: m2) - 1.
	low1 _ self bitAnd: mask. high1 _ self bitShift: m2 negated.
	low2 _ anInteger bitAnd: mask. high2 _ anInteger bitShift: m2 negated.
	z0 _ low1 karatsuba2: low2.
	z1 _ low1 + high1 karatsuba2: low2 + high2.
	z2 _ high1 karatsuba2: high2.
	^ (z2 bitShift: m2*2) + (z1 - z2 - z0 bitShift: m2) + z0! !

!Integer methodsFor: 'arithmetic' stamp: 'len 2/27/2016 05:12'!
karatsuba: anInteger
	"Karatsuba-Ofman algorithm. Integer multiplication in O(n^log(3)) ~ O(n^1.58)."
	| high1 high2 low1 low2 z0 z1 z2 middle middleBytes n |
	n _ self highBit min: anInteger highBit.
	n < 2000 ifTrue: [^ self * anInteger].
	middle _ n // 2 alignedTo: 256.
	middleBytes _ middle // 8.
	low1 _ self digitCopyFrom: 1 to: middleBytes.
	high1 _ self bitShift: middle negated.
	low2 _ anInteger digitCopyFrom: 1 to: middleBytes.
	high2 _ anInteger bitShift: middle negated.
	z0 _ low1 karatsuba: low2.
	z1 _ low1 + high1 karatsuba: low2 + high2.
	z2 _ high1 karatsuba: high2.
	^ (z2 bitShift: middle*2) + (z1 - z2 - z0 bitShift: middle) + z0! !


!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'len 2/27/2016 04:16'!
digitCopyFrom: start to: stop
	| n answer |
	n _ stop - start + 1.
	answer _ LargePositiveInteger new: n.
	answer replaceFrom: 1 to: n with: self startingAt: start.
	^ answer! !


!PseudoClass methodsFor: 'methods' stamp: 'len 4/4/2016 03:55'!
removeSelectorIfInBaseSystem: selector
	self sourceCode removeKey: selector ifAbsent: [^ self].
	self organization removeElement: selector! !


!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:40'!
blockLevelOne

	^ #(
		leftParenthesis1
		rightParenthesis1
		blockStart1
		blockEnd1
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:40'!
blockLevelThree

	^ #(
		leftParenthesis3
		rightParenthesis3
		blockStart3
		blockEnd3
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:40'!
blockLevelTwo

	^ #(
		leftParenthesis2
		rightParenthesis2
		blockStart2
		blockEnd2
	)
! !


!Workspace methodsFor: 'binding' stamp: 'len 11/23/2015 03:28'!
bindingOf: aString
	mustDeclareVariables ifTrue: [^ nil].
	(bindings includesKey: aString) ifFalse: [
		"aString first isUppercase
			ifTrue: [^nil]
			ifFalse: ["bindings at: aString put: nil]"]".
	^bindings associationAt: aString! !

!Workspace methodsFor: 'binding' stamp: 'len 11/23/2015 04:05'!
initializeBindings
	bindings _ Dictionary new.
	bindings at: #bindings put: bindings! !


!Categorizer methodsFor: 'accessing' stamp: 'len 3/5/2016 20:53'!
listAtCategoryNumber: anInteger 
	"Answer the array of elements stored at the position indexed by anInteger.  Answer nil if anInteger is larger than the number of categories."

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ nil].
	firstIndex _ self firstIndexOfCategoryNumber: anInteger.
	lastIndex _  self lastIndexOfCategoryNumber: anInteger.
	^(elementArray copyFrom: firstIndex to: lastIndex) sort! !


!Collection methodsFor: 'accessing' stamp: 'len 2/23/2016 23:02'!
anyOne
	"Answer any element in the receiver."

	self do: [:each | ^ each].
	self errorEmptyCollection! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:43'!
sum
	"Compute the sum of all the elements in the receiver"

	^self collect: [ :each | each ] andFold: [ :a :b | a + b]! !

!Collection methodsFor: 'statistics' stamp: 'jmv 3/25/2015 09:44'!
sum: aBlock
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	^self collect: aBlock andFold: [ :a :b | a + b ]! !


!String class methodsFor: 'initialization' stamp: 'len 3/3/2016 04:48'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 124 126) do: [ :c | "\|~"
		newOrder at: c + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first asciiValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second asciiValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 124 126) do: [ :c | "\|~"
		newOrder at: c + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first asciiValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase asciiValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character allCharacters collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character allCharacters collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character allCharacters collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !


!Set methodsFor: 'private' stamp: 'len 2/26/2016 21:29'!
atNewIndex: index put: anObject
	array at: index put: anObject.
	tally _ tally + 1.
	self fullCheck.
	^ anObject! !


!Complex methodsFor: 'printing' stamp: 'len 3/6/2016 21:07'!
printOn: aStream
	real printOn: aStream.
"	imaginary isZero ifTrue: [^ self]."
	aStream nextPut: Character space.
	0 <= imaginary
		ifTrue: [aStream nextPut: $+]
		ifFalse: [aStream nextPut: $-].
	aStream nextPut: Character space.
	imaginary abs printOn: aStream.
	aStream nextPut: Character space.
	aStream nextPut: $i
! !


!TextEditor methodsFor: 'new selection' stamp: 'jmv 9/19/2011 16:56'!
afterSelectionInsertAndSelect: aString
	"This is a user command, and generates undo"

	self insertAndSelect: aString at: self stopIndex ! !

!TextEditor methodsFor: 'new selection' stamp: 'len 4/3/2016 07:41'!
insertAndSelect: aString at: anInteger
	"This is a user command, and generates undo"

	| newText |
	newText _ (aString is: #Text) ifTrue: [aString] ifFalse: [Text string: aString attributes: emphasisHere].
	self deselectAndPlaceCursorAt: anInteger.
	self replaceSelectionWith: newText.
	self selectFrom: anInteger to: anInteger + newText size - 1! !


!SystemWindow methodsFor: 'drawing' stamp: 'len 5/27/2016 21:56'!
drawOn: aCanvas

	| titleColor roundCorners |

	titleColor _ self widgetsColor.
	self isTopWindow
		ifTrue: [ titleColor _ titleColor lighter ].

	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			"Round corners. Optional title gradient."
			self drawRoundedFrameOn: aCanvas color: titleColor ]
		ifFalse: [
			"No round corners. No title gradient."
			self drawClassicFrameOn: aCanvas color: titleColor ].
	Theme current minimalWindows
		ifFalse: [self drawLabelOn: aCanvas]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'len 5/27/2016 21:52'!
buildButtonPane

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'save'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'new'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'delete/merge'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'browse'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'add requirement'.
	buttonRow := LayoutMorph newRow.
	buttonRow
		doAdoptWidgetsColor;
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow ! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 8/27/2015 14:39'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground textHeigth |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	textHeigth := AbstractFont default height.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: textHeigth;
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: textHeigth;
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: textHeigth;
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.
		
	description := TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary := TextModelMorph
		textProvider: model
		textGetter: #summary.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: self buildButtonPane proportionalHeight: 0.1;
		addAdjusterAndMorph: summary proportionalHeight: 0.18;
		addAdjusterAndMorph: description proportionalHeight: 0.22;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.2.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'len 5/27/2016 21:51'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete'.
	deleteReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newColumn.
	buttonLayout addMorph: deleteReqButton 
					layoutSpec: (LayoutSpec 
										proportionalWidth: 1.0 
										proportionalHeight: 1.0
										minorDirectionPadding: #top);
					color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.9 
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.1 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right);
		color: Color transparent;
		yourself
		! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2014 16:21'!
initialExtent

	^540@400! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'DM 8/22/2015 12:34'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.47;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!ParkMiller88Random methodsFor: 'private' stamp: 'len 5/20/2016 02:23'!
initialize
	"Output stabilization is the user's responsibility"

	[
		seed _ (Time localMillisecondClock + self identityHash) hashMultiply \\ self m.
		seed = 0  "zero seeds are unacceptable"
	] whileTrue.
	seed _ seed asFloat! !


!Theme methodsFor: 'private - shout mappings' stamp: 'len 3/5/2016 22:41'!
generateShoutConfig

	| styles colors |
	
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined}.
		{self literals . colors at: #pseudoVariables}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages}.
		{self blockLevelOne . colors at: #blockLevelOne}.
		{self blockLevelTwo . colors at: #blockLevelTwo}.
		{self blockLevelThree . colors at: #blockLevelThree}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self tempBar . colors at: #tempBar}.
		{self methodTags . colors at: #methodTags . #bold}.
		{self globals . colors at: #defaults . #bold}.
		{self incompleteMessages . colors at: #incompleteMessages . #underlined}.
		{self argumentTypes . colors at: #arguments . self italic}.
		{self symbols . colors at: #messages . #bold}.
		{self pattern . nil . #bold}.
		{self ansiAssignment . nil . #bold}.
		{self assignment . nil . #(#bold #withST80Glyphs)}.
		{self return . nil . #(#bold #withST80Glyphs)}.
		{self tempVars . colors at: #tempVars ". self italic"}.
		{self blockTemps . colors at: #tempBar . self italic}
	} do: [ :style |
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements _ style asOrderedCollection.
					elements at: 1 put: category.
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	styles addAll: {
		{#unfinishedString . colors at: #undefined . #normal}.
		{#undefinedIdentifier . colors at: #undefined .#bold}.
		{#unfinishedComment . colors at: #pseudoVariables . self italic}.
		{#comment . colors at: #methodTags . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !

!methodMoveToSomePackage: SmallInteger #bitCount!
SmallInteger removeSelectorIfInBaseSystem: #bitCount!
!methodMoveToSomePackage: FloatArray #dotProduct:!
FloatArray removeSelectorIfInBaseSystem: #dotProduct:!
!methodRemoval: Character class #thereDoesNotExists!
Character class removeSelector: #thereDoesNotExists!
!methodRemoval: Character class #thereExists!
Character class removeSelector: #thereExists!
!methodMoveToSomePackage: BlockClosure #count!
BlockClosure removeSelectorIfInBaseSystem: #count!

!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('gui' openLabel:)
('initialization' initialize)
('shout styling' shouldStyle shoutAboutToStyle: toggleStyling toggleStylingLabel)
('testing')
('user interface support' autoCompleterClass editorClass textStylerClass)
('variable declarations' mustDeclareVariableWording toggleVariableDeclarationMode)
!


!CodeFileBrowser class reorganize!
('class initialization' unload)
('instance creation' browseCode: browsePackage: fileReaderServicesForFile:suffix: serviceBrowseCode serviceBrowsePackage)
!


!BlockClosure reorganize!
('accessing' argumentCount copiedValueAt: home isBlock method numArgs numCopiedValues outerContext receiver size startpc)
('controlling' repeat repeatWithGCIf: whileFalse whileFalse: whileNil: whileNotNil: whileTrue whileTrue:)
('debugger access' sender)
('error handing' numArgsError:)
('evaluating' bench durationToRun ifError: simulateValueWithArguments:caller: timeToRun timeToRunWithoutGC value value: value:value: value:value:value: value:value:value:value: valueAt: valueNoContextSwitch valueNoContextSwitch: valueWithArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('events-support' asMinimalRepresentation isReceiverOrAnyArgumentGarbage)
('exceptions' assert ensure: ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do: valueUninterruptably)
('initialization' outerContext:startpc:numArgs:copiedValues:)
('objects from disk' objectForDataStream:)
('printing' decompile printOn: printStack:)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessWith:)
('services' timeProfile)
('testing' hasMethodReturn isClosure isDead isTrivialClosure)
('private' asContextWithSender: copyForSaving grabProcessor grabProcessorFor:onTimeout: grabProcessorOnlyFor: reentrant valueUnpreemptively)
('*Mathematics' asBlockOrFunction count)
!


!Categorizer reorganize!
('accessing' addCategory: addCategory:before: allMethodSelectors categories categories: categoryOfElement: changeFromCategorySpecs: changeFromString: classify:under: classify:under:suppressIfDefault: classifyAll:under: elementCategoryDict isEmptyCategoryNumber: listAtCategoryNamed: listAtCategoryNumber: moveCategoryBottom: moveCategoryDown: moveCategoryTop: moveCategoryUp: numberOfCategoryOfElement: removeCategory: removeElement: removeEmptyCategories renameCategory:toBe: sortCategories)
('copying' postCopy)
('fileIn/Out' scanFrom:)
('printing' printOn:)
('testing' hasAnyCategoriesSuchThat:)
('private' elementArray firstIndexOfCategoryNumber: lastIndexOfCategoryNumber: setDefaultList:)
!


!Collection reorganize!
('*Mathematics' anyIfNone: copyEmpty copyEmpty: gather: plot:)
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // raisedTo: \\)
('comparing' hash)
('converting' asArray asBag asByteArray asCharacterSet asFloatArray asIdentitySet asIntegerArray asOrderedCollection asSet asSortedArray asSortedCollection asSortedCollection: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: symmetricDifference: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('sorting' sorted:)
('statistics' argmax: argmin: average max max: mean median min min: product product: range sampleStandardDeviation sampleVariance standardDeviation sum sum: variance)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('private' emptyCheck errorCollectionToSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species toBraceStack:)
!


!Collection class reorganize!
('*squeakenh-misc-instance creation' accumulate: accumulate:on:)
('instance creation' newFrom: with: with:with: with:with:with: with:with:with:with: with:with:with:with:with: with:with:with:with:with:with: withAll:)
!


!Bag reorganize!
('*squeakenh-misc-frequencies' frequencyDistribution)
('*squeakenh-misc-removing' remove:howMany: remove:howMany:ifAbsent: removeAllOccurrences:ifNone: removeOne: removeOne:ifAbsent:)
('accessing' at: at:put: cumulativeCounts size sortedCounts sortedElements)
('adding' add: add:withOccurrences: addAll:)
('comparing' =)
('converting' asBag)
('copying' postCopy)
('enumerating' asSet detect:ifNone: do: select:)
('math functions' sum)
('removing' remove:ifAbsent:)
('testing' includes: occurrencesOf:)
('private' contents setContents:)
!


!SequenceableCollection reorganize!
('*Mathematics' convolution:)
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: anyOne at:add: at:ifAbsent: at:incrementBy: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth fillWith: first first: fourth from:to:put: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: integerAt: integerAt:put: last last: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: third)
('comparing' = hasEqualElements: hash)
('converting' asArray asByteArray asColorArray concatenation isSequenceable printStringWithNewline readStream reverse reversed writeStream @)
('copying' , copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: copyWithoutIndex: forceTo:paddingStartWith: forceTo:paddingWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeDo: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:reverseDo: withIndexCollect: withIndexDo:)
('gui' do:displayingProgress:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: includes: isSequenceableCollection)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:in:after:do: copyReplaceAll:with:asTokens: errorFirstObject: errorOutOfBounds permutationsStartingAt:do:)
!


!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= adaptToNumber:andSend: dot: negated normalize sum \\=)
('comparing' = hash)
('converting' asFloatArray)
('inspecting' inspectorClass)
('interpolating' interpolatedValueAt:)
('math functions' derivative)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('testing' is:)
('private' replaceFrom:to:with:startingAt:)
('*Mathematics')
!


!String reorganize!
('*fileman-core-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('*fileman-core-actions' fileContents fileContents:)
('*fileman-core-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('*fileman-core-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('arithmetic' * + - / // \\)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asCamelCase asCharacter asDate asFileName asHex asIdentifier: asInteger asLegalSelector asLowercase asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: base64Decoded base64Encoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector initialIntegerOrNil keywords prefixAndSuffix: romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withFirstCharacterDownshifted withLineEndings: withNewLines withoutSuffix: withoutTrailingBlanks)
('copying' , copyReplaceTokens:with: join: padded:to:with:)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('formatting' format:)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('printing' isLiteral print printOn: storeOn:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer isAlphaNumeric isString)
('uCompletion' separateKeywords)
('user interface' edit editLabel:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('text conversion helpers' bold italic sub super under)
!

String initialize!

!String class reorganize!
('*fileman-core-constants' pathSeparators)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('initialization' initialize)
('instance creation' addUnicodeCodePoint:to:hex: crString crlfString fromString: fromUtf8: fromUtf8:hex:trimLastNull: lfString newLineString readFrom: tab value:)
('primitives' findFirstInString:inSet:startingAt: indexOfAscii:inString:startingAt: stringHash:initialHash: translate:from:to:table:)
!


!Set reorganize!
('accessing' atRandom: like: size)
('adding' add: add:withOccurrences:)
('converting' asSet)
('copying' postCopy)
('enumerating' collect: do: union:)
('explorer' hasContentsInExplorer)
('inspecting' inspectorClass)
('objects from disk' comeFullyUpOnReload:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' = includes: occurrencesOf:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: rehash scanFor: swap:with: withArray:)
('*Mathematics' add:ifAbsent: add:ifPresent:)
!


!Complex reorganize!
('*Mathematics' ** norm norm2 one zero)
('accessing' argument imaginary magnitude phase real)
('arithmetic' * + - / abs absSecure arg conjugated divideFastAndSecureBy: divideSecureBy: i negated reciprocal squaredNorm)
('comparing' = hash)
('converting' adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: asComplex)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: cos cosh exp ln log: raisedTo: raisedToInteger: sin sinh sqrt squared tan tanh)
('printing' printOn:)
('testing' isComplex isNumber isZero)
('private' real:imaginary:)
!


!TextEditor reorganize!
('accessing' currentAttributes currentCharacterStyleOrNil currentParagraphStyle lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: setSearch: text)
('accessing-selection' hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('as yet unclassified' totalTextHeight visibleHeight)
('attributes' changeEmphasisOrAlignment offerColorMenu offerFontMenu)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: cancelEdits: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('initialization' initialize resetState textComposition:)
('menu' getMenu)
('menu messages' acceptContents align cancelEdits chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model:)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling' scrollBy:)
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll selectAll:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: storeSelectionInComposition unapplyAttribute: zapMultiSelection)
!


!Number reorganize!
('*Mathematics' ** , adaptToQuaternion:andSend: asQuaternion conjugated isAlgebraic isRational norm norm2 one zero)
('arithmetic' * + - / // \\ abs arg div: mod: negated quo: reciprocal rem:)
('comparing' closeTo:)
('converting' @ adaptToCollection:andSend: adaptToComplex:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asComplex asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours i milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: magnitude nthRoot: raisedTo: raisedToInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: storeOn: storeOn:base: storeStringBase:)
('testing' even isDivisibleBy: isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart reduce roundDownTo: roundTo: roundUpTo: rounded truncateTo: truncated)
!


!Integer reorganize!
('*Mathematics' % Stirling: adaptToAlgebraic:andSend: adaptToRealAlgebraic:andSend: bitSize count: denominator descendentPower: divisors divisorsDo: factors factors2 isRational jacobi: jacobiOld: kronecker: moebius numberOfDivisors numerator pollard primeSignature primitiveRoot squareRoot stirling: | take:q:)
('arithmetic' * + - / // alignedTo: crossSumBase: karatsuba2: karatsuba: quo: reciprocalModulo: \\\)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash)
('converting' adaptToFraction:andSend: asCharacter asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat)
('enumerating' timesRepeat:)
('inspecting' explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial gcd: ifMultipleOf2And5Do:otherwise: lcm: ln log nthRoot: nthRootTruncated: productTo: raisedTo:modulo: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isInteger isPowerOfTwo isPrime isProbablyPrime nextPrime)
('tiles')
('truncation and round off' atRandom atRandom: ceiling floor normalize rounded truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!


!LargePositiveInteger reorganize!
('arithmetic' * + - / // abs digitCopyFrom:to: negated quo: \\ \\\)
('bit manipulation' bitAt: bitReverse: hashMultiply highBit highBitOfMagnitude)
('comparing' < <= > >= hash)
('converting' asFloat normalize withAtLeastNDigits:)
('enumerating' timesRepeat:)
('mathematical functions' mightBeASquare sqrt)
('printing' printOn:base: printOn:base:nDigits:)
('system primitives' digitAt: digitAt:put: digitLength replaceFrom:to:with:startingAt:)
('testing' negative positive sign strictlyPositive)
!


!SmallInteger reorganize!
('arithmetic' * + - / // gcd: quo: \\)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed hashMultiply lowBit)
('comparing' < <= = > >= identityHash ~=)
('converting' asCharacter asFloat)
('copying' shallowCopy)
('mathematical functions' sqrt)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength instVarAt: nextInstance nextObject)
('testing' even odd)
('private')
('*Mathematics' bitCount)
!


!ChangeSorterWindow reorganize!
('GUI building' buildMorphicWindow initialExtent windowColor)
('menu building' changeSetMenu classListMenu messageMenu)
('menu commands' browseMethodConflicts browseVersions)
('keyboard shortcuts' changeSetListKey:from: classListKey:from: messageListKey:from:)
!


!PseudoClass reorganize!
('accessing' allCallsOn allInstVarNames allSuperclasses compilerClass fullName instVarNames name name: organization realClass theNonMetaClass)
('categories' removeCategory: removedCategoryName whichCategoryIncludesSelector:)
('class' classComment: classPool comment comment: definition definition: metaClass renameTo: sharedPools)
('compatibility' browseClassVarRefs category chooseInstVarThenDo:)
('errors' classNotDefined)
('fileIn/fileOut' fileIn fileInCategory: fileInDefinition fileInMethod: fileInMethods fileInMethods: fileOut fileOutCategory: fileOutDefinitionOn: fileOutMethod: fileOutMethods:on: fileOutMethodsOn: fileOutOn:)
('methods' addMethodChange: closuresInfoAt: compiledMethodAt:ifAbsent: methodChange: removeMethod: removeSelector: removeSelectorIfInBaseSystem: selectors sourceCode sourceCodeAt: sourceCodeAt:put: sourceCodeTemplate stampAt:)
('printing' literalScannedAs:notifying: printOn:)
('removing' removeAllUnmodified removeUnmodifiedMethods:)
('testing' exists hasChanges hasComment hasDefinition hasMetaclass isMeta nameExists needsInitialize)
('testing method dictionary' bindingOf: includesSelector:)
('private' allSubclassesWithLevelDo:startingLevel: confirmRemovalOf: evaluate: makeSureClassExists: makeSureSuperClassExists: parserClass)
!


!TextStream reorganize!
('accessing' nextPutAll:)
('private' applyAttribute:beginningAt: nextPutAllString:withAttributes: withAttribute:do: withAttributes:do:)
!


!Theme reorganize!
('accessing' decorateMenu:)
('as yet unclassified' windowClosed: windowOpen:)
('colors' background buttonColorFrom: buttonLabel focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarColor scrollbarSliderShadowColor text textCursor textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon helpIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('initialization' initialize)
('keyboard shortcuts' keyStroke:morph:)
('menu colors' menu menuHighlight menuText menuTitleBar)
('menus' allIcons basicIcons menuDecorations miscellaneousIcons noIcons)
('other options' buttonGradientBottomFactor buttonGradientHeight buttonGradientTopFactor buttonPaneHeight embossedButtonLabels embossedTitles fullScreenDeskMargin layoutAdjusterThickness minimalWindows roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarThickness steButtons titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useButtonGradient useTaskbar useUniformColors useWindowTitleGradient)
('shout' shout)
('tool colors' browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton listHighlightFocused: listMultiHighlightFocused: listSelectedRowText listUnselectedRowText textPane)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelOne blockLevelSeven blockLevelSix blockLevelThree blockLevelTwo blockTemps defaults firstBlockLevel generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
!

