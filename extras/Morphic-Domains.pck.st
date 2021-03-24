'From Cuis 5.0 [latest update: #4190] on 24 March 2021 at 9:32:37 am'!
'Description '!
!provides: 'Morphic-Domains' 1 1!
!requires: 'RayTracer' 1 4 nil!
SystemOrganization addCategory: #'Morphic-Domains'!


!classDefinition: #ComplexPlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #ComplexPlotMorph
	instanceVariableNames: 'function image box palette'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ComplexPlotMorph class' category: #'Morphic-Domains'!
ComplexPlotMorph class
	instanceVariableNames: ''!

!classDefinition: #FunctionPlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #FunctionPlotMorph
	instanceVariableNames: 'backgroundColor fillColor textColor function xMin xMax yMin yMax evaluationPoints values firstMousePosition lastMousePosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'FunctionPlotMorph class' category: #'Morphic-Domains'!
FunctionPlotMorph class
	instanceVariableNames: ''!

!classDefinition: #HistogramMorph category: #'Morphic-Domains'!
KernelMorph subclass: #HistogramMorph
	instanceVariableNames: 'backgroundColor textColor tallies extraBinsForClampedValues lowBinCenter highBinCenter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'HistogramMorph class' category: #'Morphic-Domains'!
HistogramMorph class
	instanceVariableNames: ''!

!classDefinition: #ImplicitFunctionPlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #ImplicitFunctionPlotMorph
	instanceVariableNames: 'function image box'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ImplicitFunctionPlotMorph class' category: #'Morphic-Domains'!
ImplicitFunctionPlotMorph class
	instanceVariableNames: ''!

!classDefinition: #ImplicitSurfacePlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #ImplicitSurfacePlotMorph
	instanceVariableNames: 'polynomial rotation scaling surface scene camera image resolutionRatio lastMousePosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ImplicitSurfacePlotMorph class' category: #'Morphic-Domains'!
ImplicitSurfacePlotMorph class
	instanceVariableNames: ''!

!classDefinition: #MultiplicationTableMorph category: #'Morphic-Domains'!
KernelMorph subclass: #MultiplicationTableMorph
	instanceVariableNames: 'elements colors operation'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'MultiplicationTableMorph class' category: #'Morphic-Domains'!
MultiplicationTableMorph class
	instanceVariableNames: ''!

!classDefinition: #ScalarPlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #ScalarPlotMorph
	instanceVariableNames: 'function palette xMin xMax yMin yMax form'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ScalarPlotMorph class' category: #'Morphic-Domains'!
ScalarPlotMorph class
	instanceVariableNames: ''!

!classDefinition: #ScatterPlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #ScatterPlotMorph
	instanceVariableNames: 'points transformation max min positionBlock colorBlock sizeBlock lastMousePosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ScatterPlotMorph class' category: #'Morphic-Domains'!
ScatterPlotMorph class
	instanceVariableNames: ''!

!classDefinition: #UnitDiskPlotMorph category: #'Morphic-Domains'!
KernelMorph subclass: #UnitDiskPlotMorph
	instanceVariableNames: 'function image palette inverseMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'UnitDiskPlotMorph class' category: #'Morphic-Domains'!
UnitDiskPlotMorph class
	instanceVariableNames: ''!

!classDefinition: #ImplicitSurface category: #'Morphic-Domains'!
RayTraceObject subclass: #ImplicitSurface
	instanceVariableNames: 'polynomial gradient radiusSquared interiorTexture'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ImplicitSurface class' category: #'Morphic-Domains'!
ImplicitSurface class
	instanceVariableNames: ''!

!classDefinition: #AutomaticPalette category: #'Morphic-Domains'!
Object subclass: #AutomaticPalette
	instanceVariableNames: 'map'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'AutomaticPalette class' category: #'Morphic-Domains'!
AutomaticPalette class
	instanceVariableNames: ''!

!classDefinition: #ComplexPalette category: #'Morphic-Domains'!
Object subclass: #ComplexPalette
	instanceVariableNames: 'luminanceBlock hueBlock zeroColor infinityColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'ComplexPalette class' category: #'Morphic-Domains'!
ComplexPalette class
	instanceVariableNames: ''!

!classDefinition: #DiscretePalette category: #'Morphic-Domains'!
Object subclass: #DiscretePalette
	instanceVariableNames: 'colors map'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'DiscretePalette class' category: #'Morphic-Domains'!
DiscretePalette class
	instanceVariableNames: ''!

!classDefinition: #RealPalette category: #'Morphic-Domains'!
Object subclass: #RealPalette
	instanceVariableNames: 'colors min max'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Domains'!
!classDefinition: 'RealPalette class' category: #'Morphic-Domains'!
RealPalette class
	instanceVariableNames: ''!


!HistogramMorph commentStamp: '<historical>' prior: 0!
Histograms to be used for statistical purposes. Useful, for example, for many image processing algorithms. See http://en.wikipedia.org/wiki/Histogram .
Counts are currently held in a WordArray, and limited to about 4 billion counts (32 bit unsigned integers)

A good number of bins can be n^(1/2) or n^(1/3), where n is the total number of samples. See Wikipedia article for rationale.!

!HistogramMorph methodsFor: 'copying' stamp: 'jmv 4/10/2015 16:16'!
postCopy
	tallies _ tallies copy! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 14:39:49'!
box: aRectangle
	box _ aRectangle.
	image _ nil.
	self redrawNeeded.
	self matchBoxRatio.! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 16:53:54'!
buildImage
	image _ Form extent: self morphExtent depth: Display depth.
	1 to: image height do: [:j|
		| y |
		y _ j asFloat / image height * box height - box bottom.
		1 to: image width do: [:i|
			| x z c |
			x _ i asFloat / image width * box width - box right.
			z _ [function value: x + y i] on: ZeroDivide do: [Infinity projective].
			image colorAt: i@(image height - j + 1) put: (palette at: z)]]! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 09:56:06'!
drawOn: aCanvas
	image isNil ifTrue: [self buildImage].
	aCanvas image: image at: 0@0! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 09:56:01'!
function: aFunction
	function _ aFunction.
	image _ nil.
	self redrawNeeded! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 1/23/2017 09:46:10'!
initialize
	super initialize.
	extent _ 300@300.
	color _ Color lightGray.
	box _ -2@0 extent: 4@2.
	palette _ ComplexPalette default! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 14:39:59'!
matchBoxRatio
	| ratio |
	((ratio _ box width / box height asFloat) - (extent x / extent y)) abs < 1
		ifFalse:
			[(ratio >= 1 and: [extent x >= extent y])
				ifTrue: [self morphExtent: extent x @ (extent x / ratio)]
				ifFalse: [self morphExtent: extent y * ratio @ extent y]]! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 14:40:16'!
morphExtent: aPoint
	image _ nil.
	super morphExtent: aPoint! !

!ComplexPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 16:54:28'!
palette: aComplexPalette
	palette _ aComplexPalette! !

!ComplexPlotMorph class methodsFor: 'examples' stamp: 'len 8/9/2020 07:46:17'!
exampleBlaschkeProduct
	"This is a phase plot of the Blaschke product defined by 40 randomly choosen numbers in the unit disk."
	| random A f |
	random _ Random new.
	A _ (1 to: 40) collect: [:each| Complex abs: random next arg: Float twoPi * random next].
	f _ † carrier to: † carrier evaluating: [:z| A product: [:a| a abs / a * (a - z) / (1 - (a conjugate * z))]].
	^ ComplexPlotMorph new box: (-1 @ -1 corner: 1 @ 1); palette: ComplexPalette phase; function: f! !

!ComplexPlotMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:47'!
exampleReciprocalSinus
	| f |
	f _ † carrier to: † carrier evaluating: [:z| (1/z) sin].
	^ ComplexPlotMorph new box: (-2 @ -1 corner: 2 @ 1); palette: ComplexPalette default; function: f! !

!ComplexPlotMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:47'!
exampleRiemannZetaFunction
	| f |
	f _ † carrier to: † carrier evaluating: [:z| (1 to: 100) sum: [:n| n^z negated]].
	^ ComplexPlotMorph new morphExtent: 300@300; box: (-5@-5 corner: 5@5); palette: (ComplexPalette grid: 1@5); function: f! !

!ComplexPlotMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:47'!
exampleSinus
	| f |
	f _ † carrier to: † carrier evaluating: [:z| z sin].
	^ ComplexPlotMorph new box: (-2*Float pi @ -1.5 corner: 2*Float pi @ 1.5); palette: (ComplexPalette grid: 1@10); function: f! !

!FunctionPlotMorph methodsFor: 'accessing' stamp: 'len 1/2/2016 06:03'!
evaluationPoints
	evaluationPoints isNil ifTrue: [self computeEvaluationPoints].
	^ evaluationPoints! !

!FunctionPlotMorph methodsFor: 'accessing' stamp: 'len 4/27/2016 21:36'!
evaluationPoints: aCollection
	xMin _ aCollection first.
	xMax _ aCollection last.
	evaluationPoints _ aCollection.
	values _ nil! !

!FunctionPlotMorph methodsFor: 'accessing' stamp: 'len 1/6/2016 02:53'!
function: aFunction
	function _ aFunction.
	yMin _ nil. yMax _ nil! !

!FunctionPlotMorph methodsFor: 'accessing' stamp: 'len 4/21/2016 05:29'!
yMin: min yMax: max
	yMin _ min.
	yMax _ max! !

!FunctionPlotMorph methodsFor: 'drawing' stamp: 'len 4/28/2016 05:42'!
drawOn: aCanvas
	| rect p lastP font |
	values isNil ifTrue: [self computeValues].
	rect _ self morphLocalBounds.
	aCanvas fillRectangle: rect color: backgroundColor.
	aCanvas line: (self localToMorphic:0@yMin)-(0@1) to: (self localToMorphic: 0 @ yMax)+(0@1) width: 1 color: textColor.
	aCanvas line: (self localToMorphic: xMin@0)+(1@0) to: (self localToMorphic: xMax@0)-(1@0) width: 1 color: textColor.

	lastP _ nil.
	self evaluationPoints with: values do: [:x :value|
		| y |
		y _ value ifNil: [0].
		(y between: yMin and: yMax)
			ifTrue:
				[p _ (self xToMorphic: x)@(self yToMorphic: y).
				p _ p rounded.
				lastP notNil
					ifTrue:
						[| y0 |
						aCanvas line: lastP to: p width: 1 color: color.
						y0 _ (self yToMorphic: 0) rounded.
						y > 0
							ifTrue: [aCanvas fillRectangle: (lastP corner: (p x @ (y0 min: rect bottom))) color: fillColor]
							ifFalse: [aCanvas fillRectangle: (lastP x @ (y0 max: rect top) corner: p) color: fillColor]].
					lastP _ p]
			ifFalse:
				[lastP _ nil]].

	font _ AbstractFont default. " Preferences standardButtonFont."
	aCanvas drawString: (yMax roundTo: 0.01) printString at: rect left + 2 @ (rect top + 2) font: font color: textColor.
	aCanvas drawString: (yMin roundTo: 0.01)  printString at: rect left + 2 @ (rect bottom - (font height * 2) - 2) font: font color: textColor.
	aCanvas drawString: (xMax roundTo: 0.01) printString at: rect right - (font widthOfString: (xMax  roundTo: 0.01) printString) - 2 @ (rect bottom - font height - 2) font: font color: textColor.
	aCanvas drawString: (xMin roundTo: 0.01) printString at: rect left + 2 + 20 @ (rect bottom - font height - 2) font: font color: textColor! !

!FunctionPlotMorph methodsFor: 'drawing' stamp: 'len 4/28/2016 05:40'!
localToMorphic: aPoint
	^(self xToMorphic: aPoint x) @ (self yToMorphic: aPoint y)! !

!FunctionPlotMorph methodsFor: 'drawing' stamp: 'len 4/27/2016 22:04'!
morphicToLocal: aPoint
	| bounds |
	bounds _ self morphLocalBounds.
	^ aPoint x - bounds left / bounds width * (xMax - xMin) + xMin @ (aPoint y - bounds top / bounds height * (yMax - yMin) + yMin)! !

!FunctionPlotMorph methodsFor: 'drawing' stamp: 'len 12/4/2015 05:00'!
xToMorphic: x
	^x - xMin / (xMax - xMin ) * extent x! !

!FunctionPlotMorph methodsFor: 'drawing' stamp: 'len 12/4/2015 05:00'!
yToMorphic: y
	^yMax - y / (yMax - yMin ) * extent y! !

!FunctionPlotMorph methodsFor: 'event handling' stamp: 'len 4/27/2016 21:11'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !

!FunctionPlotMorph methodsFor: 'event handling' stamp: 'len 4/27/2016 21:57'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	firstMousePosition _ lastMousePosition _ localEventPosition! !

!FunctionPlotMorph methodsFor: 'event handling' stamp: 'len 6/24/2016 21:36'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self flag: #fix.
"	mouseMoveEventHandler _ nil"! !

!FunctionPlotMorph methodsFor: 'event handling' stamp: 'len 4/27/2016 22:16'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	firstMousePosition isNil
		ifTrue: [firstMousePosition _ localEventPosition]
		ifFalse:
			[aMouseMoveEvent shiftPressed
				ifTrue:
					[| bounds anchor delta |
					bounds _ self morphLocalBounds.
					delta _ localEventPosition - lastMousePosition / (bounds width max: bounds height).
					self scaleBy: 2.0 ^ delta x negated @ (2.0 ^ delta y) anchor: firstMousePosition]
				ifFalse:
					[| bounds delta |
					bounds _ self morphLocalBounds.
					delta _ localEventPosition - lastMousePosition / (bounds width max: bounds height).
					self scrollBy: delta]].
	lastMousePosition _ localEventPosition! !

!FunctionPlotMorph methodsFor: 'event handling' stamp: 'len 4/27/2016 22:21'!
scaleBy: aPoint anchor: anchorPoint
	| p |
	p _ self morphicToLocal: anchorPoint.
	self evaluationPoints: (self evaluationPoints collect: [:each| each - p x * aPoint x + p x]).
	self redrawNeeded! !

!FunctionPlotMorph methodsFor: 'event handling' stamp: 'len 4/27/2016 21:33'!
scrollBy: aPoint
	| dx dy |
	dx _ xMin - xMax * aPoint x.
	self evaluationPoints: (self evaluationPoints collect: [:each| each + dx]).
	dy _ yMax - yMin * aPoint y.
	yMin _ yMin + dy.
	yMax _ yMax + dy.
	self redrawNeeded! !

!FunctionPlotMorph methodsFor: 'initialization' stamp: 'len 4/24/2016 02:15'!
initialize
	super initialize.
	extent _ 280@175.
	color _ Color lightGray.
	backgroundColor _ Color transparent.
	fillColor _ Color transparent.
	textColor _ Color gray alpha: 0.5! !

!FunctionPlotMorph methodsFor: 'private' stamp: 'len 1/2/2016 06:07'!
computeEvaluationPoints
	evaluationPoints _ xMin to: xMax by: 1/100.0! !

!FunctionPlotMorph methodsFor: 'private' stamp: 'len 4/27/2016 22:23'!
computeValues
	values _ self evaluationPoints collect: [:x| [function value: x] on: Error do: [0]].
	yMin isNil
		ifTrue:
			[yMin _ values min.
			yMax _ values max]! !

!FunctionPlotMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example1
	"Plotting the sine function."
	| sin |
	sin _ … carrier to: … carrier evaluating: [:x| x sin].
	^ FunctionPlotMorph new evaluationPoints: (0.0 to: Float pi * 2 count: 100); function: sin; color: (Color red alpha: 0.5)! !

!FunctionPlotMorph class methodsFor: 'examples' stamp: 'len 5/1/2020 18:42:40'!
example2
	"Plotting Euler's totient function."
	^ FunctionPlotMorph new
		color: (Color lightBlue alpha: 0.5);
		function: Sequence phi;
		evaluationPoints: (1 to: 150)! !

!FunctionPlotMorph class methodsFor: 'examples' stamp: 'len 5/1/2020 18:43:16'!
example3
	"Plotting the Calkin-Wilf sequence.
	It enumerates the positive rational numbers by a breadth-first traversal of the Calkin-Wilf tree."
	^ FunctionPlotMorph new
		evaluationPoints: (1 to: 150);
		function: Sequence calkinWilf;
		color: (Color yellow alpha: 0.5)! !

!FunctionPlotMorph class methodsFor: 'examples' stamp: 'len 5/1/2020 18:43:28'!
example4
	"Plotting the primes."
	^ FunctionPlotMorph new
		evaluationPoints: (1 to: 5);
		function: Sequence primes;
		yMin: 0 yMax: 12;
		color: (Color orange alpha: 0.5)! !

!HistogramMorph methodsFor: 'accessing' stamp: 'jmv 4/10/2015 16:11'!
countAt: value
	^tallies at: (self indexFor: value)! !

!HistogramMorph methodsFor: 'as yet unclassified' stamp: 'len 4/24/2016 05:38'!
handlesMouseDown: evt
	^ true! !

!HistogramMorph methodsFor: 'as yet unclassified' stamp: 'len 4/24/2016 05:35'!
handlesMouseOver: evt
	^ true! !

!HistogramMorph methodsFor: 'as yet unclassified' stamp: 'len 4/25/2016 07:46'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	| i |
	i _ (localEventPosition x * tallies size / self morphLocalBounds width) truncated + 1.
	i _ (i min: tallies size) max: 1.
	self showBalloon: i printString, ' -> ', (tallies at: i) printString! !

!HistogramMorph methodsFor: 'building' stamp: 'jmv 4/10/2015 16:12'!
accumulate

	2 to: tallies size do: [ :i |
		tallies at: i put: (tallies at: i) + (tallies at: i-1) ]! !

!HistogramMorph methodsFor: 'building' stamp: 'jmv 4/10/2015 16:13'!
accumulated

	^self copy accumulate! !

!HistogramMorph methodsFor: 'building' stamp: 'jmv 4/6/2015 11:37'!
addValue: aNumber

	| i |
	i _ self indexFor: aNumber.
	tallies at: i put: (tallies at: i) + 1! !

!HistogramMorph methodsFor: 'building' stamp: 'len 12/27/2015 22:45'!
inverted
	"For accumulated histograms, compute de inverse cummulative function.
	This is not an exact inverse, and is so far an experiment.
	Some tests to ensure good properties, and maybe more work, are in order. "
	| answer answerTallies n i k |
	n _ self regularTalliesCount.
	answerTallies _ WordArray new: n.
	answer _ HistogramMorph basicNew
		tallies: answerTallies
		lowBinCenter: 0
		highBinCenter: n
		extraBinsForClampedValues: false.
	i _ 1.
	k _ 1.0 * n / tallies last.
	1 to: n do: [ :j |
		[ (tallies at: i) * k < j ] whileTrue: [ i _ i + 1 ].
		answerTallies at: j put: i
		].
	^answer! !

!HistogramMorph methodsFor: 'drawing' stamp: 'len 4/27/2016 08:18'!
drawOn: aCanvas
	| rect x dx y min max font |
	rect _ self morphLocalBounds.
	aCanvas fillRectangle: rect color: backgroundColor.
	min _ 0. max _ tallies max.
	max = min ifTrue: [^ self].
	dx _ rect width asFloat / tallies size.
	x _ rect left.
	tallies do: [:v|
		| bar |
		y _ (max-v) / (max-min) * rect height asFloat.
		y _ y min: rect height.
		y _ y max: 0.
		bar _ x truncated @ y corner: ((x _ x + dx) ceiling @ rect bottom).
		aCanvas fillRectangle: bar color: color].
	
	font _ AbstractFont default. " Preferences standardButtonFont."
	aCanvas drawString: max printString at: rect left + 2 @ (rect top + 2) font: font color: textColor.
	aCanvas drawString: min printString at: rect left + 2 @ (rect bottom - font height - 2) font: font color: textColor
! !

!HistogramMorph methodsFor: 'initialization' stamp: 'len 4/27/2016 07:28'!
initialize
	super initialize.
	extent _ 280@175.
	color _ Color darkGray.
	backgroundColor _ Color transparent.
	textColor _ Color gray alpha: 0.5! !

!HistogramMorph methodsFor: 'private' stamp: 'len 4/25/2016 07:39'!
indexFor: aValue
	| slotIndex |
	slotIndex _ ((aValue - lowBinCenter / (highBinCenter -lowBinCenter )) * (tallies size-1)) rounded + 1.
	^slotIndex min: tallies size max: 1.! !

!HistogramMorph methodsFor: 'private' stamp: 'jmv 4/6/2015 11:18'!
regularTalliesCount

	^ extraBinsForClampedValues
		ifTrue: [ tallies size - 2 ]
		ifFalse: [ tallies size ]! !

!HistogramMorph methodsFor: 'private' stamp: 'len 4/25/2016 07:29'!
tallies: anArray lowBinCenter: aNumber highBinCenter: anotherNumber extraBinsForClampedValues: aBoolean
	tallies _ anArray.
	lowBinCenter _ aNumber.
	highBinCenter _ anotherNumber.
	extraBinsForClampedValues _ aBoolean.
	tallies size < 300 ifTrue: [extent _ (extent x / tallies size) truncated + 1 * tallies size @ extent y]! !

!HistogramMorph class methodsFor: 'examples' stamp: 'len 4/23/2016 04:02'!
example
	"
	HistogramMorph example
	"
	| r h |
	r _ Random new.
	h _ HistogramMorph new: 500 min: 0 max: 1.
	100000 timesRepeat: [h addValue: r next].
	h openInHand! !

!HistogramMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:34:55'!
example2
	"
	HistogramMorph example2
	"
	| h S n |
	S _ ƒ ^ 4.
	n _ 40.
	h _ HistogramMorph new: 500 min: 0 max: n.
	100000 timesRepeat: [h addValue: (S atRandom: n) bitSize].
	h openInHand! !

!HistogramMorph class methodsFor: 'examples' stamp: 'len 3/6/2017 16:33:28'!
example3
	"primes conspiracy"
	| h p r |
	h _ HistogramMorph new: 22 min: 1 max: 100.
	p _ 2. r _ p \\ 10.
	100 timesRepeat:
		[| p2 r2 |
		p2 _ p nextPrime. r2 _ p2 \\ 10.
		h addValue: (r * 10 + r2).
		r _ r2. p _ p2].
	^ h! !

!HistogramMorph class methodsFor: 'instance creation' stamp: 'len 4/23/2016 04:02'!
new: binCount lowBinCenter: lowBinCenter highBinCenter: highBinCenter
	"lowBinCenter is the center value of the lowest bin.
	highBinCenter is the center value of the highest bin.
	
	Values lower than the lower bound of the first bin go in the fist bin,
	and values higher or equal than the upper bound of the last bin go in the last bin.
		(i.e., out of range values are simply clamped)
		
	For example, the following will give a histogram where first and last bins have half the counts as all the rest:
		| h r |
		h _ Histogram binCount: 10 lowBinCenter: 0 highBinCenter: 100.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next * 100  ].
		h plot
		
	Maybe what we wanted was:
		| h r |
		h _ Histogram binCount: 10 lowBinCenter: 5 highBinCenter: 95.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  * 100 ].
		h plot
	
	Or more simply
		| h r |
		h _ Histogram binCount: 10 minValue: 0 maxValue: 100.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  * 100 ].
		h plot
"

	^self new
		tallies: (WordArray new: binCount)
		lowBinCenter: lowBinCenter
		highBinCenter: highBinCenter
		extraBinsForClampedValues: false! !

!HistogramMorph class methodsFor: 'instance creation' stamp: 'len 4/23/2016 04:03'!
new: binCount min: min max: max
	"min is the lower bound of the lowest bin.
	max is the upper bound of the highest bin.
	
	Values lower than min go in the fist bin, and values higher or equal than max go in the last bin.
		(i.e., out of range values are simply clamped)

		| h r |
		h _ Histogram binCount: 10 minValue: 0.0 maxValue: 1.0.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  ].
		h plot

	"

	^self new: binCount min: min max: max extraBinsForClampedValues: false! !

!HistogramMorph class methodsFor: 'instance creation' stamp: 'len 4/23/2016 04:01'!
new: regularBinCount min: min max: max extraBinsForClampedValues: aBoolean
	"min is the lower bound of the lowest bin.
	max is the upper bound of the highest bin.
	
	If aBoolean is false, values lower than min go in the fist bin, and values higher or equal than max go in the last bin.
		(i.e., out of range values are simply clamped)
	If aBoolean, then extra bins are added at each end for these 'out of range' values

		| h r |
		h _ Histogram binCount: 10 minValue: 0.0 maxValue: 1.0 extraBinsForClampedValues: true.
		r _ Random new.
		10000 timesRepeat: [ h addValue: r next  ].
		h plot
	"

	| tallies highBinCenter lowBinCenter halfBinWidth |
	halfBinWidth _ max - min / regularBinCount / 2.0.
	aBoolean
		ifTrue: [
			tallies _ WordArray new: regularBinCount + 2.
			lowBinCenter _ min - halfBinWidth.
			highBinCenter _ max + halfBinWidth ]
		ifFalse: [
			tallies _ WordArray new: regularBinCount.
			lowBinCenter _ min + halfBinWidth.
			highBinCenter _ max - halfBinWidth ].
	^self new
		tallies: tallies
		lowBinCenter: lowBinCenter
		highBinCenter: highBinCenter
		extraBinsForClampedValues: aBoolean! !

!HistogramMorph class methodsFor: 'instance creation' stamp: 'len 8/14/2016 00:10'!
new: binCount samples: aCollection
	| answer |
	answer _ self new: binCount min: aCollection min max: aCollection max.
	aCollection do: [:each| answer addValue: each].
	^ answer! !

!ImplicitFunctionPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 10/24/2016 21:38'!
box: aRectangle
	box _ aRectangle.
	image _ nil.
	self redrawNeeded! !

!ImplicitFunctionPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 10/24/2016 21:40'!
buildImage
	| lastRow |
	image _ Form extent: self morphExtent depth: Display depth.
	lastRow _ nil.
	1 to: image height do: [:j|
		| y row lastSign |
		y _ j asFloat / image height * box height - box bottom.
		row _ Array new: image width.
		lastSign _ nil.
		1 to: image width do: [:i|
			| x sign |
			x _ i asFloat / image width * box width - box right.
			sign _ (function value: (x,y)) sign.
			((lastSign notNil and: [lastSign ~= sign])
				or: [lastRow notNil and: [(lastRow at: i) ~= sign]])
					ifTrue: [image colorAt: i@(image height - j + 1) put: color].
			row at: i put: sign.
			lastSign _ sign].
		lastRow _ row]! !

!ImplicitFunctionPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 10/24/2016 21:19'!
drawOn: aCanvas
	image isNil ifTrue: [self buildImage].
	aCanvas image: image at: 0@0! !

!ImplicitFunctionPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 10/24/2016 21:38'!
function: aFunction
	function _ aFunction.
	image _ nil.
	self redrawNeeded! !

!ImplicitFunctionPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 10/24/2016 21:47'!
initialize
	super initialize.
	extent _ 150@150.
	color _ Color lightGray.
	box _ -1@-1 extent: 2@2! !

!ImplicitFunctionPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 10/24/2016 20:55'!
morphExtent: aPoint
	image _ nil.
	super morphExtent: aPoint! !

!ImplicitFunctionPlotMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example1
	"Circle of radious 1."
	| function |
	function _ (…^2) carrier to: … carrier evaluatingWithArguments: [:x :y| x^2 + (y^2) - 1].
	^ ImplicitFunctionPlotMorph new function: function! !

!ImplicitFunctionPlotMorph class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example2
	"Descartes folium."
	| function |
	function _ (…^2) carrier to: … carrier evaluatingWithArguments: [:x :y| x^3 + (y^3) - (3*x*y)].
	^ ImplicitFunctionPlotMorph new function: function! !

!ImplicitSurfacePlotMorph methodsFor: 'accessing' stamp: 'len 1/22/2017 07:44:33'!
color: aColor
	super color: aColor.
	surface color: aColor! !

!ImplicitSurfacePlotMorph methodsFor: 'accessing' stamp: 'len 1/22/2017 07:44:42'!
interiorColor: aColor
	surface interiorColor: aColor! !

!ImplicitSurfacePlotMorph methodsFor: 'accessing' stamp: 'len 1/22/2017 20:34:55'!
polynomial: aPolynomial
	polynomial _ aPolynomial.
	rotation _ Float64Quaternion identity.
	scaling _ 1.0.
	surface _ ImplicitSurface new polynomial: aPolynomial.
	surface color: color.
	camera _ Camera new
				location: 0 @ 0 @ -2.5;
				direction: 0 @ 0 @ 1;
				up: 0 @ 1 @ 0;
				right: 1 @ 0 @ 0.
	scene _ Scene new
				addLightSource: (LightSource new
							location: 15 @ 5 @ -20;
							color: Color lightYellow muchLighter);
"				addLightSource: (LightSource new
							location: -15 @ -5 @ 0;
							color: Color cyan);"
				addObject: surface;
"				addObject: (Plane new
							normal: 0 @ 1 @ 0;
							offset: 1;
							texture: (TextureWithFinish new
										pigment: (CheckerBoardPigment new
													center: 0 @ 0.1 @ 0;
													color1: Color gray;
													color2: Color white;
													squareSize: 1);
										finish: (CompoundFinish new
													addFinish: (PhongLightingModelFinish new
																ambient: 0.2;
																specular: 0;
																specularPower: 10);
													addFinish: (ReflectionFinish new reflection: 0.3))));
"				yourself.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'drawing' stamp: 'len 1/20/2017 23:05:27'!
drawOn: aCanvas
	image isNil ifTrue: [^ super drawOn: aCanvas].
	aCanvas image: (image magnifyBy: resolutionRatio) at: 0@0! !

!ImplicitSurfacePlotMorph methodsFor: 'event handling testing' stamp: 'len 1/21/2017 18:24:36'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !

!ImplicitSurfacePlotMorph methodsFor: 'events' stamp: 'len 1/21/2017 18:24:48'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	lastMousePosition _ localEventPosition! !

!ImplicitSurfacePlotMorph methodsFor: 'events' stamp: 'len 1/22/2017 09:57:36'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	resolutionRatio _ self normalResolutionRatio.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'events' stamp: 'len 1/22/2017 09:57:49'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	lastMousePosition isNil
		ifFalse:
			[aMouseMoveEvent shiftPressed
				ifTrue:
					[| bounds d |
					bounds _ self morphLocalBounds.
					d _ (localEventPosition dist: lastMousePosition) / (bounds width min: bounds height) * 2.0.
					localEventPosition y >= lastMousePosition y ifTrue: [d _ d negated].
					resolutionRatio _ self fastResolutionRatio.
					self scaleSurfaceBy: 2^d]
				ifFalse: [resolutionRatio _ self fastResolutionRatio.
					self rotateSurfaceBy: localEventPosition - lastMousePosition]].
	lastMousePosition _ localEventPosition! !

!ImplicitSurfacePlotMorph methodsFor: 'geometry' stamp: 'len 1/20/2017 21:55:28'!
morphExtent: aPoint
	image _ nil.
	super morphExtent: aPoint! !

!ImplicitSurfacePlotMorph methodsFor: 'initialization' stamp: 'len 5/16/2018 05:24:03'!
initialize
	super initialize.
	self morphExtent: 300@300.
	resolutionRatio _ self normalResolutionRatio.
	self polynomial: AffineVariety clebschCubic asHypersurface polynomial! !

!ImplicitSurfacePlotMorph methodsFor: 'surface geometry' stamp: 'len 1/22/2017 09:14:31'!
rotateSurfaceBy: aPoint
	| xSpeed ySpeed |
	xSpeed _ Float pi / self morphLocalBounds width.
	ySpeed _ Float pi / self morphLocalBounds height.
	rotation _ ((Float64Quaternion radiansAngle: aPoint x * xSpeed axis: 0@1@0) * (Float64Quaternion radiansAngle: aPoint y * ySpeed axis: 1@0@0)) * rotation.
	self updateTransformation! !

!ImplicitSurfacePlotMorph methodsFor: 'surface geometry' stamp: 'len 1/22/2017 09:18:41'!
scaleSurfaceBy: aNumber
	scaling _ scaling * aNumber.
	self updateTransformation! !

!ImplicitSurfacePlotMorph methodsFor: 'surface geometry' stamp: 'len 1/22/2017 09:13:51'!
updateTransformation
	| M v |
	M _ rotation asMatrix3x3.
	v _ polynomial parent generate: [:x :y :z|
		{x * M a11 + (y * M a12) + (z * M a13) * scaling.
		x * M a21 + (y * M a22) + (z * M a23) * scaling.
		x * M a31 + (y * M a32) + (z * M a33) * scaling}].
	surface polynomial: (polynomial value: v).
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/22/2017 09:57:17'!
fastResolutionRatio
	^ 8! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/22/2017 20:02:12'!
normalResolutionRatio
	^ 2! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/20/2017 23:05:05'!
render
	camera imageSize: extent // resolutionRatio.
	image _ camera renderScene: scene.
	self redrawNeeded! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/23/2017 10:13:53'!
renderBest
	resolutionRatio _ 1.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/20/2017 22:31:58'!
setCameraBack: distance
	camera
		location: 0 @ 0 @ distance negated;
		direction: 0 @ 0 @ 1;
		up: 0 @ 1 @ 0;
		right: 1 @ 0 @ 0.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/20/2017 22:32:04'!
setCameraLocation: aPoint
	camera location: aPoint.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/20/2017 22:32:11'!
setCameraRight: distance
	camera
		location: distance @ 0 @ 0;
		direction: -1 @ 0 @ 0;
		up: 0 @ 1 @ 0;
		right: 0 @ 0 @ 1.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/20/2017 22:32:28'!
setCameraTop: distance
	camera
		location: 0 @ distance @ 0;
		direction: 0 @ -1 @ 0;
		up: 0 @ 0 @ 1;
		right: 1 @ 0 @ 0.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/20/2017 22:32:34'!
setLightSourceAt: aPoint
	scene lightSources first location: aPoint.
	self render! !

!ImplicitSurfacePlotMorph methodsFor: 'private' stamp: 'len 1/21/2017 19:11:40'!
setRadius: aNumber
	surface radius: aNumber.
	self render! !

!MultiplicationTableMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 05:20'!
drawOn: aCanvas
	| h w |
	w _ extent x / elements size.
	h _ extent y / elements size.
	w > 10
		ifTrue:
			[1 to: elements size do: [:i|
				1 to: elements size do: [:j|
					aCanvas fillRectangle: ((i-1*w @ (j-1*h)) truncated corner: (i*w-1 @ (j*h-1)) truncated) color: (colors at: (operation value: {elements at: i. elements at: j}))]]]
		ifFalse:
			[1 to: elements size do: [:i|
				1 to: elements size do: [:j|
					aCanvas fillRectangle: ((i-1*w @ (j-1*h)) truncated corner: (i*w @ (j*h)) truncated) color: (colors at: (operation value: {elements at: i. elements at: j}))]]]
		! !

!MultiplicationTableMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 01:47'!
elements: anArray
	elements _ anArray asArray! !

!MultiplicationTableMorph methodsFor: 'as yet unclassified' stamp: 'len 8/16/2016 04:03'!
initialize
	super initialize.
	self morphExtent: 150@150.
	colors _ AutomaticPalette new! !

!MultiplicationTableMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 04:28'!
morphExtent: aPoint
	| max |
	max _ aPoint x max: aPoint y.
	super morphExtent: max @ max! !

!MultiplicationTableMorph methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 01:47'!
operation: aBinaryOperation
	operation _ aBinaryOperation! !

!MultiplicationTableMorph class methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 01:48'!
elements: anArray operation: aBinaryOperation
	^ self new elements: anArray; operation: aBinaryOperation! !

!ScalarPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/17/2016 23:44'!
drawOn: aCanvas
	| r |
	r _ 1.
	0 to: extent x - r by: r do: [:x|
		0 to: extent y - r by: r do: [:y|
			aCanvas fillRectangle: (x@y extent: r@r) color: (palette at: (function value: (self morphToDomain: x@y)))]]
! !

!ScalarPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/17/2016 23:23'!
function: aFunction
	function _ aFunction! !

!ScalarPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 16:39:13'!
initialize
	super initialize.
	extent _ 100@100.
	function _ [:v| v x cos * v y sin].
	xMin _ -4. xMax _ 4.
	yMin _ -4. yMax _ 4.
	palette _ RealPalette default min: -1.0; max: 1.0! !

!ScalarPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/17/2016 23:30'!
morphToDomain: aPoint
	^ (aPoint x * (xMax - xMin) / extent x + xMin, (aPoint y * (yMax - yMin) / extent y + yMin))! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:31:27'!
applyRotationBy: aPoint
	| bounds xSpeed ySpeed rotation |
	self dimension < 3 ifTrue: [^ self].
	bounds _ self morphLocalBounds.
	xSpeed _ Float pi / bounds width.
	ySpeed _ Float pi / bounds height.
	rotation _ (…^self dimension) endomorphisms evaluating: [:p|
		| xz yz |
		xz _ p x @ p z.
		xz _ xz rotatedBy: aPoint x * xSpeed.
		yz _ p y @ xz y.
		yz _ yz rotatedBy: aPoint y * ySpeed.
		(xz x, yz x, yz y), (p coordinates copyFrom: 4 to: p coordinates size)].
	transformation _ rotation · (transformation ifNil: [rotation identity]).
	self redrawNeeded! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/16/2016 02:00'!
applyScaleBy: aNumber
	transformation _ transformation * aNumber.
	self redrawNeeded! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/21/2016 07:13'!
colorBy: aBlock
	colorBlock _ aBlock.
	self redrawNeeded! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/15/2016 18:57'!
dimension
	^ points anyOne size! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 5/28/2020 18:25:49'!
drawOn: aCanvas
	| bounds range boxColor w |
	bounds _ self morphLocalBounds insetBy: 1.
	range _ max - min.
	"draw box:"
	self dimension >= 3 ifTrue:
		[boxColor _ Color white alpha: 0.2.
		self drawPolyline: {(-1,-1,-1). (-1,1,-1). (-1,1,1). (1,1,1). (1,-1,1). (1,-1,-1). (-1,-1,-1). (-1,-1,1). (-1,1,1)} color: boxColor on: aCanvas.
		self drawPolyline: {(1,-1,1). (-1,-1,1)} color: boxColor on: aCanvas.
		self drawPolyline: {(1,1,-1). (1,1,1)} color: boxColor on: aCanvas.
		self drawPolyline: {(1,-1,-1). (1,1,-1). (-1,1,-1)} color: boxColor on: aCanvas].
	
	"draw points:"
	w _ extent x min: extent y.
	points withIndexDo: [:each :k|
		| p xy z r shade c |
		p _ positionBlock value: each.
		p _ p collect: [:xi| xi - min / range - 0.5 * 2].
		transformation ifNotNil: [p _ (transformation value: p)].
		xy _ p x / 2 + 0.5 * w @ (p y / 2 + 0.5 * w).
		z _ p size >= 3 ifTrue: [(p z / 2 + 0.5 max: 0.0) min: 1.0] ifFalse: [1.0].
		r _ z * (sizeBlock value: each) :: rounded + 1.
		shade _ z / 2 + 0.5.
		c _ colorBlock value: each.
		c _ c mixed: shade with: Color black.
		c _ c alpha: c alpha * (shade / 2 + 0.5).
		(bounds containsPoint: xy) ifTrue:
			[aCanvas fillRectangle: (Rectangle center: xy extent: r@r) color: c]]! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/19/2016 18:39'!
drawPolyline: anArray color: aColor on: aCanvas
	| w lastPoint |
	w _ extent x min: extent y.
	lastPoint _ nil.
	anArray do: [:each|
		| p xy |
		p _ transformation ifNil: [each] ifNotNil: [transformation value: each].
		xy _ p x / 2 + 0.5 * w @ (p y / 2 + 0.5 * w).
		lastPoint ifNotNil: [aCanvas line: lastPoint to: xy width: 1 color: aColor].
		lastPoint _ xy]! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/13/2016 04:51'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 16:39:19'!
initialize
	| palette |
	super initialize.
	color _ Color white.
	extent _ 250@250.
	transformation _ nil.
	positionBlock _ [:aTuple| aTuple copyFrom: 1 to: (aTuple size min: 3)].
	palette _ RealPalette default.
	colorBlock _ [:aTuple| palette at: (positionBlock value: aTuple) norm2 / (max - min) squared].
	sizeBlock _ [:aTuple| 3]! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/13/2016 04:51'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	lastMousePosition _ localEventPosition! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/16/2016 01:59'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	lastMousePosition isNil
		ifFalse:
			[aMouseMoveEvent shiftPressed
				ifTrue:
					[| bounds d |
					bounds _ self morphLocalBounds.
					d _ (localEventPosition dist: lastMousePosition) / (bounds width min: bounds height).
					localEventPosition y >= lastMousePosition y ifTrue: [d _ d negated].
					self applyScaleBy: 2.0 ^ d]
				ifFalse: [self applyRotationBy: lastMousePosition - localEventPosition]].
	lastMousePosition _ localEventPosition! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:31:27'!
points: anArray
	points _ anArray.
	max _ points max: [:each| each max].
	min _ points min: [:each| each min].
	transformation _ …^self dimension :: id.
	self redrawNeeded! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/21/2016 07:13'!
positionBy: aBlock
	positionBlock _ aBlock.
	self redrawNeeded! !

!ScatterPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 8/21/2016 07:13'!
sizeBy: aBlock
	sizeBlock _ aBlock.
	self redrawNeeded! !

!ScatterPlotMorph class methodsFor: 'examples' stamp: 'len 5/1/2020 18:45:16'!
example1
	"Plotting prime gaps."
	| palette |
	palette _ RealPalette default.
	^ (self timeDelayPlot: (Sequence primes difference first: 1000))
			colorBy: [:each| palette at: each bitSize / 40.0]! !

!ScatterPlotMorph class methodsFor: 'examples' stamp: 'len 5/1/2020 18:45:00'!
example2
	"Plotting random samples."
	| samples |
	samples _ 1 to: 1000 :: collect: [:i| Random new next].
	^ ScatterPlotMorph timeDelayPlot: samples! !

!ScatterPlotMorph class methodsFor: 'examples' stamp: 'len 8/19/2016 05:22'!
example3
	| samples |
	samples _ 1 to: 1000 :: collect: [:i| | r | r _ Random new. 100 timesRepeat: [r next]. r next].
	^ self timeDelayPlot: samples! !

!ScatterPlotMorph class methodsFor: 'instance creation' stamp: 'len 8/18/2016 06:38'!
timeDelayPlot: anArray
	^ self timeDelayPlot: anArray lag: 1! !

!ScatterPlotMorph class methodsFor: 'instance creation' stamp: 'len 6/1/2020 13:39:42'!
timeDelayPlot: anArray dimension: m lag: r
	^ self new
		points: ((m*r+1 to: anArray size) collect: [:i|
			…^m filling: [:j| anArray at: i-(j-1*r)]])! !

!ScatterPlotMorph class methodsFor: 'instance creation' stamp: 'len 8/18/2016 06:39'!
timeDelayPlot: anArray lag: r
	^ self timeDelayPlot: anArray dimension: 3 lag: r! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 18:43:01'!
buildImage
	image _ Form extent: self morphExtent depth: Display depth.
	1 to: image height do: [:j|
		| y |
		y _ j asFloat / image height * 2 - 1.
		1 to: image width do: [:i|
			| x z c |
			x _ i asFloat / image width * 2 - 1.
			(x squared + y squared) sqrt < 1
				ifTrue:
					[| w |
					z _ x + y i.
					w _ [function value: (inverseMap value: z)] on: ZeroDivide do: [Infinity projective].
					image colorAt: i@j put: (palette at: w)]]]! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/9/2016 10:27:16'!
drawOn: aCanvas
	image isNil ifTrue: [self buildImage].
	aCanvas image: image at: 0@0! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 10:11:27'!
function: aFunction
	function _ aFunction.
"	function domain = HH
		ifTrue: [self mapUpperHalfPlaneToUnitDisk]
		ifFalse: [function domain = † ifTrue: [self mapPlaneToUnitDisk]]."
	image _ nil.
	self redrawNeeded! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 18:38:39'!
initialize
	super initialize.
	extent _ 300@300.
	color _ Color lightGray.
	palette _ ComplexPalette default.
	inverseMap _ [:z| z] "by default use the identity map"! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 18:36:26'!
mapPlaneToUnitDisk
	inverseMap _ [:z| z / (1 - z abs squared)]! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/15/2016 18:37:30'!
mapUpperHalfPlaneToUnitDisk
	inverseMap _ [:z| 1 + z / (1 - z) * 1 i]! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/9/2016 10:28:38'!
morphExtent: aPoint
	| side |
	image _ nil.
	side _ aPoint x min: aPoint y.
	super morphExtent: side @ side! !

!UnitDiskPlotMorph methodsFor: 'as yet unclassified' stamp: 'len 12/9/2016 10:27:55'!
palette: aComplexPalette
	palette _ aComplexPalette! !

!UnitDiskPlotMorph class methodsFor: 'examples' stamp: 'len 8/9/2020 07:47:16'!
exampleBlaschkeProduct
	"This is a phase plot of the Blaschke product defined by 40 randomly choosen numbers in the unit disk, plotted through a conformal mapping of the complex plane into the unit disk."
	| random A f |
	random _ Random new.
	A _ (1 to: 40) collect: [:each| Complex abs: random next arg: Float twoPi * random next].
	f _ † carrier to: † carrier evaluating: [:z| A product: [:a| a abs / a * (a - z) / (1 - (a conjugate * z))]].
	^ UnitDiskPlotMorph new mapPlaneToUnitDisk; palette: ComplexPalette phase; function: f! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/21/2017 22:33:19'!
color: aColor
	self texture: (self defaultTextureForColor: aColor)! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/21/2017 22:33:30'!
interiorColor: aColor
	self interiorTexture: (self defaultTextureForColor: aColor)! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/21/2017 22:33:42'!
interiorTexture
	^ interiorTexture ifNil: [texture]! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/21/2017 17:26:55'!
interiorTexture: aTexture
	interiorTexture _ aTexture! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/21/2017 19:08:58'!
polynomial
	^ polynomial! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/20/2017 21:07:10'!
polynomial: aPolynomial
	polynomial _ aPolynomial.
	gradient _ polynomial gradient! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/21/2017 18:35:29'!
radius
	^ radiusSquared sqrt! !

!ImplicitSurface methodsFor: 'accessing' stamp: 'len 1/20/2017 17:56:18'!
radius: aNumber
	radiusSquared _ aNumber squared! !

!ImplicitSurface methodsFor: 'initializing' stamp: 'len 5/23/2020 10:10:00'!
initialize
"	self polynomial: (… polynomialsIn: #(x y z) :: generate: [:x :y :z| x**2 + (y**2) + (z**2) - 2])"
"	self polynomial: (… polynomialsIn: #(x y z) :: generate: [:x :y :z| x**2 + (z**2) - (y**2)])."
	self polynomial: AffineVariety clebschCubic asHypersurface polynomial.
"	self polynomial: („ polynomialsIn: #(x y z) :: generate: [:x :y :z| x squared + y squared - ((1 - z) * (z**4))])."
	self radius: 1.0! !

!ImplicitSurface methodsFor: 'tracing' stamp: 'len 1/21/2017 19:35:11'!
normalAt: aPoint
	^ (Float32Vector3 x: ((gradient at: 1) value: aPoint) y: ((gradient at: 2) value: aPoint) z: ((gradient at: 3) value: aPoint)) normalize! !

!ImplicitSurface methodsFor: 'tracing' stamp: 'len 5/23/2020 09:31:27'!
writeIntersectionsWith: aRay into: aCollection
	| t0 origin ox oy oz direction dx dy dz t f root intersection p |
	(t0 _ self intersectionWithSphere: aRay) ifNil: [^ self].
	origin _ aRay pointAtDistance: t0.
	ox _ origin x.
	oy _ origin y.
	oz _ origin z.
	direction _ aRay direction.
	dx _ direction x.
	dy _ direction y.
	dz _ direction z.
	t _ … polynomials x.
	f _ polynomial value: {t*dx + ox. t*dy + oy. t*dz + oz}.
	(root _ self solve: f max: radiusSquared sqrt*2) ifNil: [^ self].
	intersection _ Intersection for: self ray: aRay distance: root+t0.
	p _ intersection intersectionPoint. "this is expensive, can we do without computing this intersection point?"
	(p dot: p) < (radiusSquared - 0.00001) ifTrue: [aCollection add: intersection]! !

!ImplicitSurface methodsFor: 'tracing-private' stamp: 'len 1/20/2017 21:25:30'!
intersectionWithSphere: aRay
	| a b c determinant t1 t2 |
	a _ aRay directionLengthSquared.
	b _ (aRay origin dot: aRay direction) * 2.0.
	c _ (aRay origin dot: aRay origin) - radiusSquared.
	determinant _ b * b - (4.0 * a * c).
	determinant < 0.0 ifTrue: [^ nil].
	determinant _ determinant sqrt.
	a _ a * 2.0.
	t1 _ (determinant - b) / a.
	t2 _ (determinant negated - b) / a.
	t1 negative ifTrue: [^ t2 positive ifTrue: [t2]].
	t2 negative ifTrue: [^ t1 positive ifTrue: [t1]].
	^ t1 min: t2! !

!ImplicitSurface methodsFor: 'tracing-private' stamp: 'len 1/20/2017 08:07:40'!
signChanges: a
	| lastSign count |
	lastSign _ a at: 1.
	count _ 0.
	2 to: a size do: [:i| | ai sign | (ai _ a at: i) abs < Float epsilon ifFalse: [(sign _ ai sign) == lastSign ifFalse: [count _ count + 1. lastSign _ sign]]].
	^ count! !

!ImplicitSurface methodsFor: 'tracing-private' stamp: 'len 1/22/2017 19:56:38'!
solve: f max: max
	| left right n a b c r0 r1 count |
	f isConstant ifTrue: [^ nil].
"	(f independentCoefficient < Float epsilon) ifTrue: [^ 0.0]."
	a _ f coefficients asArray reversed collect: [:each| each asFloat].
	n _ a size.
"	(self signChanges: a) > 0 ifFalse: [^ nil]." "no positive roots"
	b _ Array new: n.
	c _ Array new: n-1.
	left _ 0.000001.
	[right _ left + 0.1. (f value: left) * (f value: right) <= 0.0] whileFalse: [left _ right. left > max ifTrue: [^ nil]].
	r0 _ left.
	b at: 1 put: (a at: 1).
	c at: 1 put: (b at: 1).
	count _ 1.
	[2 to: n do: [:i|
		b at: i put: (a at: i) + (r0 * (b at: i-1)).
		i < n ifTrue: [c at: i put: (b at: i) + (r0 * (c at: i-1))]].
	r1 _ r0 - ((b at: n) / (c at: n-1)).
	(r1 - r0) abs < 10.0e-6 ifTrue: [^ r1].
	r0 _ r1.
	(count _ count + 1) > 100 ifTrue: [^ nil]] repeat! !

!ImplicitSurface methodsFor: 'private' stamp: 'len 1/22/2017 20:39:02'!
defaultTextureForColor: aColor
	^ TextureWithFinish new
			pigment: (FlatColorPigment new color: aColor);
			finish: (PhongLightingModelFinish new
					ambient: 0.35;
					specular: 0.8;
					specularPower: 20)

"			finish: (CompoundFinish new
				addFinish: (PhongLightingModelFinish new
					ambient: 0.35;
					specular: 0.8;
					specularPower: 20);
				addFinish: (ReflectionFinish new reflection: 1);
				yourself)"! !

!ImplicitSurface class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example1
	^ self new
		polynomial: ((… polynomialsIn: #(x y z)) generate: [:x :y :z| x^2 + (y^2) + (z^3) + (x*y*z*2) - 1]);
		radius: 1.0! !

!ImplicitSurface class methodsFor: 'examples' stamp: 'len 5/23/2020 09:31:27'!
example2
	^ self new
		polynomial: ((… polynomialsIn: #(x y z)) generate: [:x :y :z| x^2 - ((y^2)*(z^2))]);
		radius: 5.0! !

!AutomaticPalette methodsFor: 'as yet unclassified' stamp: 'len 12/18/2016 23:10:48'!
at: anObject
	| i n |
	i _ map at: anObject ifAbsentPut: [map size].
	n _ map size.
	^ Color h: i * 360.0 / (n + 1) + 90.0 \\ 360.0 s: 0.65 v: 0.95! !

!AutomaticPalette methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 04:44'!
initialize
	map _ Dictionary new! !

!ComplexPalette methodsFor: 'accessing' stamp: 'len 12/9/2016 16:39:28'!
at: aNumber
	aNumber isInfinite ifTrue: [^ infinityColor].
	aNumber isZero ifTrue: [^ zeroColor].
	^ self
		colorHue: ([hueBlock value: aNumber] on: Error do: [^ infinityColor])
		saturation: 1.0
		luminance: ([luminanceBlock value: aNumber] on: Error do: [^ infinityColor])! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/7/2016 22:25:44'!
setGrid: scalePoint
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
	luminanceBlock _ [:z| (z isZero ifTrue: [1.0] ifFalse: [(z arg / Float twoPi + 0.5 * scalePoint y + 0.999 \\ 1.0) * (z abs ln * scalePoint x \\ 1.0)]) * 0.4 + 0.2].
	zeroColor _ Color white.
	infinityColor _ Color black! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/7/2016 16:20:20'!
setLog
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
	luminanceBlock _ [:z| z isZero ifTrue: [1.0] ifFalse: [z abs min: 0.6 max: 0.0]].
	zeroColor _ Color white.
	infinityColor _ Color black! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/7/2016 22:15:26'!
setLogReversed
"	hueBlock _ [:z| (z arg + Float pi) radiansToDegrees \\ 360]."
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
	luminanceBlock _ [:z| z isZero ifTrue: [0.0] ifFalse: [1.0 - (z abs min: 0.6 max: 0.0)]].
	zeroColor _ Color black.
	infinityColor _ Color white! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/18/2016 11:01:21'!
setMagnitude
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
	luminanceBlock _ [:z| 2.0 ^ z abs negated].
	zeroColor _ Color white.
	infinityColor _ Color black! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/7/2016 21:53:50'!
setPhase
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
	luminanceBlock _ [:z| 0.5].
	zeroColor _ Color white.
	infinityColor _ Color black! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/7/2016 22:15:20'!
setReversed
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
"	hueBlock _ [:z| (z arg + Float pi) radiansToDegrees \\ 360]."
	luminanceBlock _ [:z| 1.0 - (2.0 ^ z abs negated)].
	zeroColor _ Color black.
	infinityColor _ Color white! !

!ComplexPalette methodsFor: 'initialization' stamp: 'len 12/7/2016 16:22:40'!
setScaled: scale
	hueBlock _ [:z| z arg radiansToDegrees \\ 360].
	luminanceBlock _ [:z| 2.0 ^ (z abs * scale negated)].
	zeroColor _ Color white.
	infinityColor _ Color black! !

!ComplexPalette methodsFor: 'private' stamp: 'len 12/6/2016 16:43:01'!
colorHue: hue saturation: saturation luminance: luma
	| chroma x hf i r1 g1 b1 m | 

	chroma _ (1 - (2*luma - 1) abs) * saturation.
	hf _ hue \\ 360.
	i _ hf // 60.				"integer part of hue"
	x _ (hf \\ 60) / 60.0.	"fractional part of hue"
	x _ x \\ 2.
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ luma - (chroma / 2) "(0.299*r1) - (0.587*g1) - (0.114*b1)".
	r1 _ r1 + m min: 1.0 max: 0.0.
	g1 _ g1 + m min: 1.0 max: 0.0.
	b1 _ b1 + m min: 1.0 max: 0.0.
	^ Color r: r1 g: g1 b: b1! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/18/2016 11:00:55'!
default
	^ self phase! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/7/2016 22:06:31'!
grid: scalePoint
	"This is a palette that colors each complex value z assigning a hue depending on its phase arg(z), and luminance depending on both phase and modulus |z|. The result is that of showing a grid of shadows over the Riemann sphere similar to a conformal grid mapping. The argument scalePoint determines the number of meridians and separation between parallels."
	^ self basicNew setGrid: scalePoint! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/7/2016 16:18:14'!
log
	^ self basicNew setLog! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/7/2016 22:11:32'!
logReversed
	^ self basicNew setLogReversed! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/18/2016 11:00:45'!
magnitude
	^ self basicNew setMagnitude! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/6/2016 16:51:43'!
new
	^ self default! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/7/2016 21:59:06'!
phase
	"This is a palette that colors each complex value z assigning a hue depending only on its phase arg(z), and constant luminance. The modulus |z| is not depicted."
	^ self basicNew setPhase! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/7/2016 22:11:44'!
reversed
	^ self basicNew setReversed! !

!ComplexPalette class methodsFor: 'as yet unclassified' stamp: 'len 12/7/2016 16:22:58'!
scaled: aNumber
	^ self basicNew setScaled: aNumber! !

!DiscretePalette methodsFor: 'as yet unclassified' stamp: 'len 2/18/2016 23:38'!
at: anObject
	^ map at: anObject ifAbsentPut: [self newColor]! !

!DiscretePalette methodsFor: 'as yet unclassified' stamp: 'len 2/18/2016 23:42'!
colors: anArray
	colors _ anArray.
	map _ Dictionary new! !

!DiscretePalette methodsFor: 'as yet unclassified' stamp: 'len 7/31/2016 04:32'!
newColor
	| a b answer |
	map size < colors size ifTrue: [^ colors at: map size + 1].
	[a _ colors atRandom.
	[b _ colors atRandom. a = b] whileTrue.
	answer _ a mixed: 0.5 with: b.
	colors includes: answer] whileTrue.
	^ answer! !

!DiscretePalette class methodsFor: 'as yet unclassified' stamp: 'len 2/18/2016 23:42'!
colors: anArray
	^ self new colors: anArray! !

!DiscretePalette class methodsFor: 'as yet unclassified' stamp: 'len 5/2/2016 07:36'!
default
	^ self colors: (#("brightGreen" periwinkle seaGreen hotPink lightYellow cyan lightPink lightOrange  "lightGreen" "lightRed"  "mustard" ) collect: [:each| Color exactColorNamed: each]) "shuffled"! !

!RealPalette methodsFor: 'as yet unclassified' stamp: 'len 8/17/2016 23:35'!
at: aNumber
	| v |
	v _ (aNumber max: min) min: max.
	^ colors at: (colors size - 1 * (v - min) / (max - min)) truncated + 1! !

!RealPalette methodsFor: 'as yet unclassified' stamp: 'len 2/26/2016 06:43'!
colors: anArray
	colors _ anArray! !

!RealPalette methodsFor: 'as yet unclassified' stamp: 'len 8/17/2016 23:33'!
max: aNumber
	max _ aNumber! !

!RealPalette methodsFor: 'as yet unclassified' stamp: 'len 8/17/2016 23:33'!
min: aNumber
	min _ aNumber! !

!RealPalette class methodsFor: 'as yet unclassified' stamp: 'len 5/23/2020 09:31:27'!
default
	| blue green red |
	red _ … polynomials interpolate: {"0 -> 0.0." 40 -> 0. 65 -> 0.8. "90 -> 0." 100 -> 0. 150 -> 0.5. 200 -> 0.9. 215 -> 1. 255 -> 0.95}.
	green _ … polynomials interpolate: {0 -> 0.2. 50 -> 0.48. 100 -> 0.68. 150 -> 0.75. 200 -> 0.70. 255 -> 1}.
	blue _ … polynomials interpolate: {0 -> 0.5. 35 -> 0.9. 85 -> 0.8. 115 -> 0.7. 150 -> 0.5. 200 -> 0.3. 255 -> 0}.
	^ self new colors: ((0 to: 255) collect: [:x| Color fromArray: ({x < 80 ifTrue: [0] ifFalse: [red value: x]. green value: x. blue value: x} collect: [:y| y abs min: 1.0])]); min: 0.0; max: 1.0! !

!RealPalette class methodsFor: 'as yet unclassified' stamp: 'len 8/18/2016 07:30'!
grayscale
	^ (self new: 100 interpolating: {Color darkGray darker. Color white}) min: 0.0; max: 1.0! !

!RealPalette class methodsFor: 'as yet unclassified' stamp: 'len 8/18/2016 07:21'!
new: anInteger interpolating: anArray
	^ self new colors: ((0 to: anInteger-1) collect: [:i|
			| mu |
			mu _ i * anArray size / (anInteger-1).
			(anArray atPin: mu floor + 1) mixed: mu - mu floor with: (anArray atPin: mu ceiling + 1)])! !

!BlockClosure methodsFor: '*Morphic-Domains' stamp: 'len 3/1/2017 17:00:08'!
times: anInteger histogram: binCount
	^ HistogramMorph new: binCount samples: ((1 to: anInteger) collect: [:each| self value])! !

!Collection methodsFor: '*Morphic-Domains' stamp: 'len 4/25/2016 07:52'!
plot: aFunction
	^ FunctionPlotMorph new evaluationPoints: self; function: aFunction! !

!SequenceableCollection methodsFor: '*Morphic-Domains' stamp: 'len 5/23/2020 10:05:45'!
plot
	^ FunctionPlotMorph new evaluationPoints: (1 to: self size); function: (ƒ carrier to: … carrier evaluating: [:n| self at: n])! !

!Monoid methodsFor: '*Morphic-Domains' stamp: 'len 4/26/2020 06:22:44'!
multiplicationTable
	| elements identity |
	identity _ self identity.
	elements _ self elements asArray asSortedCollection:
		[:a :b| a = identity or: [a printString <= b printString]].
	^ MultiplicationTableMorph elements: elements operation: [:x :y| x·y]! !

!Group methodsFor: '*Morphic-Domains' stamp: 'len 4/29/2016 07:14'!
cayleyGraphMorph
	^ self cayleyGraphMorph: self generators! !

!Group methodsFor: '*Morphic-Domains' stamp: 'len 12/19/2016 12:23:54'!
cayleyGraphMorph: generators
	"Answer the colored Cayley graph of the receiver for the given set of generators.
	The set of generators is usually assumed to be symmetric (contains all inverses too) and to not contain the identity."
	| G answer colors |
	G _ self cayleyGraph: generators.
	answer _ G asMorph.
	colors _ AutomaticPalette new.
	answer nodesDo: [:each| each color: Color transparent; radius: 0].
	generators do: [:each| colors at: each].
	generators do: [:each| (answer nodeAt: each) color: (colors at: each); radius: 10].
	answer edgesAndLabelsDo: [:each :label| each hideLabel; color: (colors at: label)].
	(answer nodeAt: self identity) color: Color white; shape: #square; radius: 10.
	^ answer! !

!Group methodsFor: '*Morphic-Domains' stamp: 'len 7/29/2016 06:30'!
cycleGraphMorph
	| answer |
	answer _ self cycleGraph asMorph.
"	answer hideLabels."
	(answer nodeAt: self identity) color: Color white; shape: #square.
	^ answer! !

!Group methodsFor: '*Morphic-Domains' stamp: 'len 4/26/2020 07:15:12'!
multiplicationTable
	| elements identity |
	identity _ self identity.
	elements _ self elements asArray asSortedCollection:
		[:a :b| a = identity or: [a printString <= b printString]].
	^ MultiplicationTableMorph elements: elements operation: [:x :y| x·y]! !

!Ring methodsFor: '*Morphic-Domains' stamp: 'len 4/26/2020 07:59:06'!
additionTable
	^ MultiplicationTableMorph elements: self elements asArray operation: [:x :y| x+y]! !

!Ring methodsFor: '*Morphic-Domains' stamp: 'len 4/26/2020 07:59:18'!
multiplicationTable
	^ MultiplicationTableMorph elements: self elements asArray operation: [:x :y| x*y]! !

!AffineHypersurface methodsFor: '*Morphic-Domains' stamp: 'len 1/22/2017 09:31:15'!
plot
	(self scalars isField and: [self scalars isReal and: [self ambient dimension = 3]])
		ifFalse: [^ DomainError signal].
	^ ImplicitSurfacePlotMorph new polynomial: self polynomial! !

!AffinePlaneCurve methodsFor: '*Morphic-Domains' stamp: 'len 10/24/2016 21:43'!
plot
	^ self plot: (-5@-5 extent: 10@10)! !

!AffinePlaneCurve methodsFor: '*Morphic-Domains' stamp: 'len 10/24/2016 21:43'!
plot: aRectangle
	^ ImplicitFunctionPlotMorph new function: self polynomial; box: aRectangle! !

!Map methodsFor: '*Morphic-Domains' stamp: 'len 4/24/2016 02:04'!
plot: aCollection
	^ FunctionPlotMorph new evaluationPoints: aCollection; function: self! !

!GroupAction methodsFor: '*Morphic-Domains' stamp: 'len 5/18/2016 21:23'!
actionGraphMorph
	^ self actionGraphMorph: self group generators! !

!GroupAction methodsFor: '*Morphic-Domains' stamp: 'len 12/19/2016 12:24:08'!
actionGraphMorph: generators
	| G answer colors |
	G _ self actionGraph: generators.
	answer _ G asMorph.
	colors _ DiscretePalette default.
	answer edgesAndLabelsDo: [:each :label| each hideLabel; color: (colors at: label)].
	answer nodesDo: [:each| each color: Color transparent; radius: 0].
	^ answer! !
