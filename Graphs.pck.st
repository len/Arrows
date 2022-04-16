'From Cuis 6.0 [latest update: #5053] on 16 April 2022 at 1:26:24 pm'!
'Description '!
!provides: 'Graphs' 1 0!
!requires: 'Collections-Extras' 1 1 nil!
!requires: 'Collections-Iterator' 1 1 nil!
SystemOrganization addCategory: #Graphs!


!classDefinition: #MutualAssociation category: #Graphs!
Association subclass: #MutualAssociation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'MutualAssociation class' category: #Graphs!
MutualAssociation class
	instanceVariableNames: ''!

!classDefinition: #Graph category: #Graphs!
Collection subclass: #Graph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'Graph class' category: #Graphs!
Graph class
	instanceVariableNames: ''!

!classDefinition: #Digraph category: #Graphs!
Graph subclass: #Digraph
	instanceVariableNames: 'nodes nodeCreator type'
	classVariableNames: 'InitializationBlocks'
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'Digraph class' category: #Graphs!
Digraph class
	instanceVariableNames: ''!

!classDefinition: #RootedDigraph category: #Graphs!
Digraph subclass: #RootedDigraph
	instanceVariableNames: 'roots'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'RootedDigraph class' category: #Graphs!
RootedDigraph class
	instanceVariableNames: ''!

!classDefinition: #UndirectedGraph category: #Graphs!
Graph subclass: #UndirectedGraph
	instanceVariableNames: 'digraph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'UndirectedGraph class' category: #Graphs!
UndirectedGraph class
	instanceVariableNames: ''!

!classDefinition: #DAGFrontier category: #Graphs!
Object subclass: #DAGFrontier
	instanceVariableNames: 'frontier bag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'DAGFrontier class' category: #Graphs!
DAGFrontier class
	instanceVariableNames: ''!

!classDefinition: #Dijkstra category: #Graphs!
Object subclass: #Dijkstra
	instanceVariableNames: 'graph source predecessor distance'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'Dijkstra class' category: #Graphs!
Dijkstra class
	instanceVariableNames: ''!

!classDefinition: #FloydWarshall category: #Graphs!
Object subclass: #FloydWarshall
	instanceVariableNames: 'graph distances'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'FloydWarshall class' category: #Graphs!
FloydWarshall class
	instanceVariableNames: ''!

!classDefinition: #GraphNode category: #Graphs!
Object subclass: #GraphNode
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'GraphNode class' category: #Graphs!
GraphNode class
	instanceVariableNames: ''!

!classDefinition: #ExplicitGraphNode category: #Graphs!
GraphNode subclass: #ExplicitGraphNode
	instanceVariableNames: 'inNeighbors outNeighbors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'ExplicitGraphNode class' category: #Graphs!
ExplicitGraphNode class
	instanceVariableNames: ''!

!classDefinition: #LabeledExplicitGraphNode category: #Graphs!
ExplicitGraphNode subclass: #LabeledExplicitGraphNode
	instanceVariableNames: 'label'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'LabeledExplicitGraphNode class' category: #Graphs!
LabeledExplicitGraphNode class
	instanceVariableNames: ''!

!classDefinition: #ImplicitGraphNode category: #Graphs!
GraphNode subclass: #ImplicitGraphNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'ImplicitGraphNode class' category: #Graphs!
ImplicitGraphNode class
	instanceVariableNames: ''!

!classDefinition: #ImplicitCollectionGraphNode category: #Graphs!
ImplicitGraphNode subclass: #ImplicitCollectionGraphNode
	instanceVariableNames: 'collectionBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'ImplicitCollectionGraphNode class' category: #Graphs!
ImplicitCollectionGraphNode class
	instanceVariableNames: ''!

!classDefinition: #ImplicitIteratorGraphNode category: #Graphs!
ImplicitGraphNode subclass: #ImplicitIteratorGraphNode
	instanceVariableNames: 'iterator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphs'!
!classDefinition: 'ImplicitIteratorGraphNode class' category: #Graphs!
ImplicitIteratorGraphNode class
	instanceVariableNames: ''!


!Graph commentStamp: '<historical>' prior: 0!
Abstract class for (directed or undirected) graphs. See subclasses.!

!Digraph commentStamp: '<historical>' prior: 0!
Directed graphs (digraphs). The vertices (values) can be arbitrary objects, and the edges are Associations v1 -> v2. It uses a sparse representation, implemented as a collection of nodes (GraphNodes) where each node knows its neighbors.

Some properties of the graph are encoded in the class of its nodes (e.g. whether the graph is ordered, has a fixed arity, or the edges are labeled). The 'creation block' is initialized so that appropriate nodes are made.

Structure:
 nodes 			Set of GraphNodes
 nodeCreator 	a block which is evaluated to create a new node
 type 			a Symbol indicating what the default node type is!

!RootedDigraph commentStamp: '<historical>' prior: 0!
Directed graphs with some distinguished vertices that we call 'roots'.

Structure:
 roots 		Set of root objects
!

!UndirectedGraph commentStamp: '<historical>' prior: 0!
Undirected graphs. The vertices (values) can be arbitrary objects, and the edges are MutualAssociations v1 <-> v2.

Internally, graphs are represented as symmetric digraphs (i.e., whenever the edge v1 -> v2 is present, v2 -> v1 is also present).

Structure:
 digraph 		The associated directed graph (which is always symmetric)!

!DAGFrontier commentStamp: '<historical>' prior: 0!
A DAGFrontier is used to iterate a directed acyclic graph (DAG) respecting the dependencies between nodes.  A client does not create it directly, but sends the message "frontier" to the DAG in question.

At any moment it holds a "frontier" of nodes that can be accessed.  When any node in the frontier has been processed, it can be removed from the frontier, and the frontier will be updated if necessary (in situ).

Instance variables:
frontier		<Set> of nodes on the frontier, empty if the whole DAG has been enumerated.
bag		<Bag> of nodes, used to calculate updates to the frontier.

Clients of this class must *not* modify the frontier set, only access it.
!

!Dijkstra commentStamp: '<historical>' prior: 0!
This is Dijkstra's algorithm for computing distances and shortest paths in a graph from a fixed starting node (source). If the graph is edge-labeled, the labels are used as weights.

Pre-Input: a graph G = (V, E), and a node (source);
Input: a target node;
Output: the shortest distance from the source node to the target node, or a shortest path.

The initial precomputation takes time O(|V|^2). Afterwards, it's possible to compute disntances in time O(log |V|) and shortest paths of length k in O(k log |V|). The O(log |V|) comes from the dictionary lookup operation, a lookup in a hash table.!

!FloydWarshall commentStamp: '<historical>' prior: 0!
This is the Floyd-Warshall algorithm for solving the all-pairs shortest path problem (for weighted graphs). It computes distances and shortest paths in a graph. If the graph is edge-labeled, the labels are used as weights.

Pre-Input: a graph G = (V, E);
Input: a pair of nodes (source and target);
Output: the shortest distance from the source node to the target node, or a shortest path.

The initial precomputation takes time O(|V|^3). Afterwards, it's possible to compute distances in time O(log |V|) and shortest paths of length k in O(k log |V|). The O(log |V|) comes from the dictionary lookup operation, a lookup in a hash table.

While (after initial precomputation) Dijkstra's algorithm is able to efficiently compute distances from a fixed source node, Floyd-Warshall's computes distances from any two arbitrary nodes. However, for sparse graphs with non-negative edge weights a better choice is to use Dijkstra's algorithm with binary heaps, which gives a time complexity of O(|V| |E| log |V|).!

!GraphNode commentStamp: '<historical>' prior: 0!
Abstract class for nodes that are held in a graph.

Each node holds on to a corresponding object that is the value of that node.

Subclasses add state/behavior to represent edges in the graph.!

!ExplicitGraphNode commentStamp: '<historical>' prior: 0!
An explicit nodes stores the edges in a collection, an instance variable. The type of the collection varies as to whether the graph is ordered, of fixed arity, etc.

To change the edge collection, addNeighbor: and removeNeighbor: messages are supplied.
!

!LabeledExplicitGraphNode commentStamp: '<historical>' prior: 0!
A labeled explicit node associates a label with each neighbor, and a label with the node itself; i.e. nodes and edges are labeled.

When adding neighbors, the edge label must be given.!

!ImplicitGraphNode commentStamp: '<historical>' prior: 0!
An implicit node has the edge structure represented within its value, and therefore forwards messages to the value to access edges.

To change the edge collection, you must send messages to the value.

Subclasses decide as to whether the value can yield a collection of edges more efficiently than iterating over them.!

!ImplicitCollectionGraphNode commentStamp: '<historical>' prior: 0!
Implicit graphs nodes that access the collection of edges by evaluating a block.!

!ImplicitIteratorGraphNode commentStamp: '<historical>' prior: 0!
Implicit graph nodes that iterate over the collection of edges by evaluating the iterator.!

!MutualAssociation methodsFor: 'printing' stamp: 'len 7/13/2016 07:31'!
printOn: aStream
	aStream
		print: key;
		nextPutAll: ' <-> ';
		print: value! !

!GraphNode methodsFor: 'printing' stamp: 'len 2/14/2003 23:38'!
printOn: aStream
	aStream nextPut: $[; print: value; nextPut: $]! !

!MutualAssociation methodsFor: 'comparing' stamp: 'len 7/13/2016 07:55'!
= anAssociation
	(anAssociation isKindOf: Association)
		ifFalse: [^ false].
	^ key = anAssociation key
		ifTrue: [value = anAssociation value]
		ifFalse: [value = anAssociation key and: [key = anAssociation value]]! !

!MutualAssociation methodsFor: 'comparing' stamp: 'len 7/13/2016 07:31'!
hash
	^ key hash bitXor: value hash! !

!MutualAssociation class methodsFor: 'instance creation' stamp: 'len 7/23/2016 06:39'!
newFrom: anAssociation
	^ self with: anAssociation key with: anAssociation value! !

!MutualAssociation class methodsFor: 'instance creation' stamp: 'len 7/13/2016 07:33'!
with: anObject with: anotherObject
	^ self key: anObject value: anotherObject! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 21:53'!
degree
	self isEmpty ifTrue: [^ 0].
	^ self nodes max: [:each| each degree]! !

!Graph methodsFor: 'accessing' stamp: 'len 7/14/2016 08:36'!
density
	"Answer a measure of the graph density (vs sparsity), a number between 0 and 1.
	A graph is dense if the number of edges is close to the maximum (for the given number of vertices).
	pre: assume the graph is simple."
	| V E |
	V _ self size.
	E _ self numberOfEdges.
	^ self isDirected ifTrue: [2*E/(V*(V-1))] ifFalse: [E/(V*(V-1))]! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 21:53'!
edges
	^ Iterator on: self performing: #edgesDo:! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 22:24'!
nodeAt: anObject
	^ self nodeAt: anObject ifAbsent: [self errorNotFound: anObject]! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 22:24'!
nodeAt: anObject ifAbsent: exceptionBlock
	^ self nodes at: anObject ifAbsent: exceptionBlock! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 21:53'!
numberOfEdges
	"This is commonly known as the size of the graph, but the size message returns the order (number of vertices), as this fits in better with Smalltalk usage."
	| count |
	count _ 0.
	self nodesDo: [:each| count _ count + each neighbors size].
	^ count! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 21:54'!
order
	"The size of a graph G=(V,E) is the number of vertices |V|, contrary to the more common convention of defining it as |E|."
	^ self size! !

!Graph methodsFor: 'accessing' stamp: 'len 7/13/2016 21:54'!
size
	"The size of a graph G=(V,E) is the number of vertices |V|, contrary to the more common convention of defining it as |E|."
	^ self nodes size! !

!Graph methodsFor: 'accessing' stamp: 'len 7/23/2016 04:32'!
values
	"Answer the set of vertices of the receievr ('values', as opposed to 'nodes' which are GraphNodes)."
	^ self nodes collect: [:each| each value]! !

!Graph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 21:55'!
addEdge: edge 
	"Add an edge (represented by an Association)."

	self addEdgeFrom: edge key to: edge value.
	^ edge! !

!Graph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 21:55'!
addEdge: edge label: label
	"Add an edge (represented by an Association), with given label.
	pre: (self nodeAt: edge key) isLabeled"

	self addEdgeFrom: edge key to: edge value label: label.
	^edge! !

!Graph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 23:54'!
addEdges: aCollection
	aCollection do: [:each| self addEdge: each].
	^ aCollection! !

!Graph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 22:17'!
removeEdge: anEdge
	"Remove the given edge.
	pre: the edge must be in the graph."
	self removeEdge: anEdge ifAbsent: [self errorNotFound: anEdge]! !

!Graph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 22:16'!
removeEdge: edge ifAbsent: exceptionBlock 
	"Remove the given edge, evaluate exceptionBlock if it doesn't exist.
	pre: both the source and target of the edge must be in the graph."
	^self removeEdgeFrom: edge key to: edge value ifAbsent: exceptionBlock! !

!Graph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 22:17'!
removeEdgeFrom: src to: dest
	"Remove the given edge.
	pre: the edge must be in the graph."
	^self removeEdgeFrom: src to: dest ifAbsent: [self errorNotFound: src -> dest]! !

!Graph methodsFor: 'comparing' stamp: 'len 8/12/2016 21:18'!
< aGraph
	"Answer true if the receiver is a proper subgraph of aGraph."
	(aGraph isKindOf: self species) ifFalse: [^ aGraph >= self].
	self size < aGraph size ifFalse: [^ false].
	self nodesDo: [:each|
		| node |
		(aGraph includes: each value) ifFalse: [^ false].
		node _ aGraph nodeAt: each value.
		each neighborsDo: [:n| (node hasEdgeTo: n value) ifFalse: [^ false]]].
	^ true! !

!Graph methodsFor: 'comparing' stamp: 'len 8/12/2016 21:45'!
<= aGraph
	"Answer true if the receiver is a subgraph of aGraph."
	(aGraph isKindOf: self species) ifFalse: [^ aGraph >= self].
	self size <= aGraph size ifFalse: [^ false].
	self nodesDo: [:each|
		| node |
		(aGraph includes: each value) ifFalse: [^ false].
		node _ aGraph nodeAt: each value.
		each neighborsDo: [:n| (node hasEdgeTo: n value) ifFalse: [^ false]]].
	^ true! !

!Graph methodsFor: 'comparing' stamp: 'len 7/21/2016 00:07'!
= aGraph
	^ (aGraph isKindOf: self species) and: [self nodes = aGraph nodes and: [self edges asBag = aGraph edges asBag]]! !

!Graph methodsFor: 'comparing' stamp: 'len 8/12/2016 21:47'!
> aGraph
	"Answer true if aGraph is a proper subgraph of the receiver."
	^ aGraph < self! !

!Graph methodsFor: 'comparing' stamp: 'len 8/12/2016 21:47'!
>= aGraph
	"Answer true if aGraph is a subgraph of the receiver."
	^ aGraph <= self! !

!Graph methodsFor: 'comparing' stamp: 'len 7/13/2016 21:58'!
hash
	^ self nodes hash! !

!Graph methodsFor: 'converting' stamp: 'len 7/21/2016 02:12'!
asStandard
	^ self asStandard: self values asArray! !

!Graph methodsFor: 'converting' stamp: 'len 7/21/2016 02:12'!
asStandard: verticesArray
	^ self collect: [:each| verticesArray indexOf: each]! !

!Graph methodsFor: 'enumerating' stamp: 'len 7/13/2016 22:03'!
collect: aBlock
	"Answer a new graph like the receiver but with vertices values mapped by aBlock."
	| answer |
	answer _ self copyEmpty.
	self nodesDo: [:each| answer add: (aBlock value: each value)].
	self edgesDo: [:each| answer addEdgeFrom: (aBlock value: each key) to: (aBlock value: each value)].
	^ answer! !

!Graph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:50'!
collect: aBlock labels: labelBlock
	"Answer a new graph like the receiver but with vertices values mapped by aBlock."
	| answer |
	answer _ self copyEmpty.
	self do: [:each| answer add: (aBlock value: each)].
	self edgesAndLabelsDo: [:each :label| answer addEdgeFrom: (aBlock value: each key) to: (aBlock value: each value) label: (labelBlock value: label)].
	^ answer! !

!Graph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:34'!
do: aBlock
	"Iterate over the vertices of the receiver (the values, not GraphNodes)."
	self nodesDo: [:each| aBlock value: each value]! !

!Graph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:54'!
select: aBlock
	"Answer the subgraph of the receiver whose vertices satisfy aBlock."
	| answer |
	answer _ self copyEmpty.
	self do: [:each| (aBlock value: each) ifTrue: [answer add: each]].
	self edgesDo: [:each|
		((aBlock value: each key) and: [aBlock value: each value])
			ifTrue: [answer addEdgeFrom: each key to: each value]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 2/17/2017 07:41:42'!
* aGraph
	"Answer the graph with all edges that connect the vertices of the receiver with the vertices of the argument. This is a commutative operation (for unlabeled graphs)."
	^ self join: aGraph! !

!Graph methodsFor: 'operations' stamp: 'len 5/10/2019 04:36:17'!
+ aGraphOrEdge
	"Answer the dijoint union of the receiver with the argument. Assume they are disjoint."
	(aGraphOrEdge isKindOf: Association)
		ifFalse: [^ self \/ aGraphOrEdge].
	(self hasEdge: aGraphOrEdge)
		ifFalse: [^ self copy addEdge: aGraphOrEdge; yourself]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:04'!
- anEdge
	"Answer the graph obtained by removing anEdge."
	(self hasEdge: anEdge)
		ifTrue: [^ self copy removeEdge: anEdge; yourself]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:05'!
/ anEdge
	"Answer the graph obtained by contracting the given edge a -> b, by removing the edge and collapsing a with b in a single node."

	| answer newNode found |
	(self hasEdge: anEdge) ifFalse: [^ self].
	answer _ self copyEmpty.
	newNode _ anEdge.
	answer add: newNode.
	self nodesDo: [:each| (each = anEdge key or: [each = anEdge value]) ifFalse: [answer add: each]].
	found _ false.
	self edgesDo: [:each|
		(each = anEdge and: [found not])
			ifTrue: [found _ true]
			ifFalse:
				[| e |
				e _ each.
				(e key = anEdge key or: [e key = anEdge value])
					ifTrue: [e _ newNode -> e value].
				(e value = anEdge key or: [e value = anEdge value])
					ifTrue: [e _ e key -> newNode].
				answer addEdge: e]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 5/10/2019 04:27:18'!
/\ aGraph
	"Answer the graph whose vertices and edges is the intersection of the vertices and edges of the receiver and the argument."
	| answer |
	answer _ self copyEmpty.
	aGraph nodesDo: [:each|
		(self nodeAt: each ifAbsent: [])
			ifNotNil: [:node|
				answer add: each.
				each neighborsDo: [:n|
					(node hasEdgeTo: n)
						ifTrue: [answer addEdgeFrom: each to: n]]]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 5/10/2019 04:27:27'!
\/ aGraph
	"Answer the graph whose vertices and edges are the union of the vertices and edges of the receiver and the argument."
	| answer |
	answer _ self copy.
	aGraph do: [:each| answer add: each].
	aGraph edgesDo: [:each| answer addEdgeFrom: each key to: each value].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 12/30/2017 16:16:58'!
× aGraph
	"Answer the cartesian product of the receiver with the argument. This is a commutative and associative operation (for unlabeled graphs)."
	| G |
	G _ self copyEmpty.
	self do: [:x| aGraph do: [:y| G add: (x, y)]].
	self do: [:x| aGraph edgesDo: [:e| G addEdgeFrom: (x, e key) to: (x, e value)]].
	self edgesDo: [:e| aGraph do: [:y| G addEdgeFrom: (e key, y) to: (e value, y)]].
	^ G! !

!Graph methodsFor: 'operations' stamp: 'len 5/14/2019 20:49:36'!
‘ aGraph
	"Answer the tensor product (or direct graph product, categorical graph product, cardinal graph product, Kronecker graph product) of the receiver with the argument. This is a commutative operation (for unlabeled graphs)."
	| G |
	G _ self class ordered.
	self do: [:x| aGraph do: [:y| G add: (x, y)]].
	self edgesDo: [:e1|
		aGraph edgesDo: [:e2|
			G addEdgeFrom: (e1 key, e2 key) to: (e1 value, e2 value)]].
	^ G! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:13'!
breadthFirstPath2From: origin to: target with: visitedNodes 
	| queue node nodesLevels |
	nodesLevels _ Dictionary new.
	nodesLevels at: origin value put: 0.
	queue _ OrderedCollection with: origin.
	visitedNodes add: origin.
	[queue isEmpty]
		whileFalse: [node _ queue removeFirst.
			node neighborsDo:
				[:each | (visitedNodes includes: each)
						ifFalse: [queue addLast: each.
							visitedNodes add: each.
							nodesLevels at: each value put: (nodesLevels at: node value)
									+ 1.
							each = target
								ifTrue: [^ nodesLevels at: each value]]]].
	"No path From origin to target, i.e. the two subgraph are not conected"
	^ 0! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:13'!
breadthFirstPathFrom: source to: target 
	| visitedNodes |
	(source isLeaf
			or: [target isLeaf])
		ifTrue: [^ 0]
		ifFalse: [visitedNodes _ Set new.
			^ self
				breadthFirstPath2From: source
				to: target
				with: visitedNodes]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:13'!
breadthFirstPathFrom: origin to: target with: visitedNodes 
	| queue node nodesLevels |
	nodesLevels _ Bag new.
	queue _ OrderedCollection with: origin.
	visitedNodes add: origin.
	[queue isEmpty]
		whileFalse: [node _ queue removeFirst.
			node neighborsDo:
				[:each | (visitedNodes includes: each)
						ifFalse: [queue addLast: each.
							visitedNodes add: each.
							nodesLevels add: each withOccurrences: (nodesLevels occurrencesOf: node)
									+ 1.
							each = target
								ifTrue: [^ nodesLevels occurrencesOf: each]]]].
	"No path From origin to target, i.e. the two subgraph are not conected"
	^ 0! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:06'!
center
	"Answer the center of the receiver, i.e. the subset of vertices with maximal eccentricity.
	Using Dijsktra shortest path algorithm this computation requires time O(|V|^3)."
	| radius |
	radius _ self radius.
	^ self values select: [:each| (self eccentricityOf: each) = radius]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:06'!
circumference
	"Answer the length of the longest cycle."
	^ self notYetImplemented! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:06'!
components
	"Answer the Strongly Connected Components of the receiver."
	^ Set accumulate: [:aBlock | self componentsDo: aBlock]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:07'!
diameter
	^ self nodes max: [:each| self eccentricityOf: each]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:05'!
distanceFrom: source to: target
	^ (Dijkstra graph: self source: source) distanceTo: target! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:05'!
eccentricityOf: anObject
	^ (Dijkstra graph: self source: anObject) eccentricity! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:07'!
girth
	"Answer the length of the shortest cycle."
	^ self notYetImplemented! !

!Graph methodsFor: 'operations' stamp: 'len 7/20/2016 23:19'!
join: aGraph
	"Answer the graph with all edges that connect the vertices of the receiver with the vertices of the argument. This is a commutative operation (for unlabeled graphs)."
	| answer |
	answer _ self copyEmpty.
	self edgesDo: [:each| (aGraph includes: each value) ifTrue: [answer addEdge: each]].
	aGraph edgesDo: [:each| (self includes: each key) ifTrue: [answer addEdge: each]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 7/20/2016 23:54'!
line
	"Answer the line graph of the receiver, i.e. the graph L(G) such that:
	 - each edge of G is a vertex of L(G);
	 - if two edges of G share a common endpoint, the correspondng vertices in L(G) are connected." 
	| answer |
	self flag: #fix.
	answer _ self copyEmpty.
	self fullEdgesDo: [:each| "each contains GraphNodes, not values"
		each value neighborsDo: [:n|
			answer addEdgeFrom: each to: (Association key: each value value value: n value)]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 2/7/2022 09:18:12'!
maxmimumDegree
	^ self nodes inject: Float negativeInfinity into: [:maximum :each| maximum max: each degree]! !

!Graph methodsFor: 'operations' stamp: 'len 2/7/2022 09:18:23'!
minimumDegree
	^ self nodes inject: Float infinity into: [:minimum :each| minimum min: each degree]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:08'!
multiplicity
	^ self edges max: [:each| self multiplicityFrom: each key to: each value]! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:08'!
multiplicityFrom: source to: target
	^ (self nodeAt: source) neighbors occurrencesOf: target! !

!Graph methodsFor: 'operations' stamp: 'len 7/20/2016 23:22'!
neighborhoodOf: node
	"Answer the subgraph of everything that is reachable from the given vertex."
	^ self subgraphInducedBy: (self nodeAt: node) neighbors! !

!Graph methodsFor: 'operations' stamp: 'len 2/7/2022 10:08:11'!
quotient
	| Q |
	self isLabeled ifFalse: [self error: 'not a labeled graph'].
	Q _ self copyEmpty.
	self nodesDo: [:each| Q add: each label].
	self fullEdgesAndLabelsDo: [:each :label| Q addEdgeFrom: each key label to: each value label label: label].
	^ Q! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:08'!
radius
	^ self nodes min: [:each| self eccentricityOf: each]
"	^ (FloydWarshall graph: self) radius"! !

!Graph methodsFor: 'operations' stamp: 'len 7/20/2016 23:47'!
reflexive
	"Answer the reflexive closure of the receiver."
	| answer |
	answer _ self copy.
	answer nodesDo: [:each | each hasLoop ifFalse: [answer addEdgeFrom: each to: each]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:05'!
shortestPathFrom: source to: target
	^ (Dijkstra graph: self source: source) shortestPathTo: target! !

!Graph methodsFor: 'operations' stamp: 'len 6/2/2020 12:14:00'!
subgraphInducedBy: aSet
	"Answer the subgraph induced by aSet of vertices."
	| answer |
	answer _ self copy.
	self edgesDo: [:each|
		((aSet includes: each key) and: [aSet includes: each value])
			ifTrue: [answer addEdge: each]].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 23:06'!
symmetric
	"Answer the symmetric closure of the receiver."
	| answer |
	answer _ self copy.
	self edgesDo: [:each | answer addEdge: (Association key: each value value: each key)].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 7/13/2016 22:12'!
topologicalSort
	"Answer a topological sort of the receiver. This is a linear ordering of its vertices such that for each edge a -> b, a comes before to b in the ordering.
	This is Kahn's algorithm."
	| answer G S n |
	answer _ OrderedCollection new.
	G _ self copy.
	S _ G nodes select: [:each| each inDegree = 0].
	[S notEmpty]
		whileTrue:
			[n _ S anyOne.
			S remove: n.
			answer add: n.
			n neighbors do: [:m|
				n removeNeighbor: m.
				m inDegree = 0 ifTrue: [S add: m]]].
	G isEdgeless ifFalse: [^ self error: 'the graph has at least one cycle'].
	^ answer! !

!Graph methodsFor: 'operations' stamp: 'len 8/9/2020 07:48:33'!
transpose
	"Answer the transpose of the receiver. This is the graphs where edges a->b are inverted as b->a."
	| answer |
	answer _ self copyEmpty.
	self edgesDo: [:each | answer addEdge: (Association key: each value value: each key)].
	^ answer! !

!Graph methodsFor: 'random' stamp: 'len 7/13/2016 23:14'!
edgeAtRandom
	^ Random withDefaultDo: [:aRandom| self edgeAtRandom: aRandom]! !

!Graph methodsFor: 'random' stamp: 'len 7/21/2016 01:53'!
shuffled
	^ Random withDefaultDo: [:aRandom| self shuffledBy: aRandom]! !

!Graph methodsFor: 'statistics' stamp: 'len 2/7/2022 18:28:31'!
averageDegree
	"The average degree of the graph."
	"^ 2 * self numberOfEdges / self size"
	^ self nodes average: [:node | node degree] ifEmpty: [0]! !

!Graph methodsFor: 'statistics' stamp: 'len 7/13/2016 23:11'!
averagePathLength
	"The average path length of the graph."
	| sum n progress max |
	self flag: #bug. "bug with implicit graph since nodes are labelled with integer"
	sum := 0.
	n := 2.
	max := (self order * (self order - 1) / 2) asFloat.
	"p _ 1 / self order."
	progress := ProgressMorph label: 'Average path length progress'.
	progress submorphs first color: Color paleGreen lighter lighter lighter.
	progress submorphs first borderWidth: 1.
	progress openInWorld.
	self
		nodesDo: [:node1 | 
			n
				to: self order
				do: [:node2 | sum := sum
								+ (self
										breadthFirstPathFrom: node1
										to: (self nodeAt: node2))].
			n := n + 1.
			progress incrDone: self order - n / max].
	progress delete.
	^ (sum / max) asFloat! !

!Graph methodsFor: 'statistics' stamp: 'len 7/13/2016 23:11'!
averagePathLength2
	"The average path length of the graph."
	| sum n max |
	sum := 0.
	n := 2.
	max := (self order * (self order - 1) / 2) asFloat.
	self
		nodesDo: [:node1 | 
			n
				to: self order
				do: [:node2 | sum := sum
								+ (self
										breadthFirstPathFrom: node1
										to: (self nodeAt: node2))].
			n := n + 1.].
	^ (sum / max) asFloat! !

!Graph methodsFor: 'statistics' stamp: 'len 7/13/2016 23:11'!
averagePathLengthAcuteAt
	"The aproximate average path length of the graph."
	| sum n progress p max nodesList |
	sum _ 0.
	n _ 2.
	max _ 0.
	progress _ ProgressMorph label: 'Average path length progress'.
	progress submorphs first color: Color paleGreen lighter lighter lighter.
	progress submorphs first borderWidth: 1.
	progress openInWorld.
	nodesList _ OrderedCollection new.
	(0.1 * self order) rounded
		timesRepeat: [nodesList
				add: self nodes atRandom].
	p _ 1 / nodesList size.
	nodesList
		do: [:node1 | 
			n
				to: nodesList size
				do: [:node2 | 
					max _ max + 1.
					sum _ sum
								+ (self
										breadthFirstPathFrom: node1
										to: (self nodeAt: node2))].
			n _ n + 1.
			progress incrDone: p].
	progress delete.
	^ (sum / max) asFloat! !

!Graph methodsFor: 'statistics' stamp: 'len 7/13/2016 23:11'!
averagePathLengthAcuteAt: percentage
	"The aproximate average path length of the graph."
	| sum n max nodesList |
	sum _ 0.
	n _ 2.
	max _ (self order * (self order - 1) / 2) asFloat.
	"p _ 1 / self order. 
	progress _ ProgressMorph label: 'Average path length progress'. 
	progress submorphs first color: Color paleGreen lighter lighter lighter. 
	progress submorphs first borderWidth: 1. 
	progress openInWorld."
	nodesList := OrderedCollection new.
	(percentage * self order) timesRepeat: [nodesList add: self nodes atRandom].
	nodesList
				do: [:node1 | 
					n
						to: nodesList size
						do: [:node2 | sum _ sum
										+ (self
												breadthFirstPathFrom: node1
												to: (self nodeAt: node2))].
					n _ n + 1].
	"progress delete."
	^ (sum / max) asFloat! !

!Graph methodsFor: 'statistics' stamp: 'len 7/13/2016 23:11'!
clusteringCoefficient
	| sum |
	sum _ 0.
	self nodesDo: [:node | node degree > 1 ifTrue: [sum _ sum + node clusteringCoefficient]].
	^ (sum / self order) asFloat! !

!Graph methodsFor: 'statistics' stamp: 'len 7/13/2016 23:11'!
degreeDistribution
	"The distribution degree of the graph."
	| b |
	b _ Bag new: self size.
	self nodesDo: [:each| b add: each degree].
	^ b frequencyDistribution! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:31'!
hasEdge: edge
	^ self hasEdgeFrom: edge key to: edge value! !

!Graph methodsFor: 'testing' stamp: 'len 7/29/2016 06:34'!
hasEdgeFrom: a to: b
	^ (self nodeAt: a ifAbsent: [^ false]) hasEdgeTo: b! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:31'!
hasLoop
	self nodesDo: [:each| each hasLoop ifTrue: [^ true]].
	^ false! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:32'!
includes: anObject
	"Answer whether anObject is one of the vertices of the receiver."
	^ self nodes includes: anObject! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:32'!
isChain
	"Answer whether the graph is a chain - path graph."
	^ (self select: [:each| each isAnExtremity]) size + (self select:[:each| each isMiddle]) size = self order! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:32'!
isComplete
	self nodesDo: [:x| self nodesDo: [:y| (x hasEdgeTo: y) ifFalse: [^ false]]].
	^ true! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:33'!
isConnected
	| count |
	count _ 0.
	self componentsDo: [:each| count _ count + 1. count > 1 ifTrue: [^ false]].
	^ true! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:33'!
isCubic
	^ self nodes allSatisfy: [:each| each degree = 3]! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:33'!
isCyclic
	[self topologicalSort] on: Error do: [^ true].
	^ false
"	| remainingNodes |
	remainingNodes _ self nodes copy.
	[remainingNodes isEmpty]
		whileFalse:
			[Transcript show: 'pick'; newLine.
			remainingNodes anyOne walkPre: [:each|
					Transcript show: each printString; newLine.
					remainingNodes remove: each ifAbsent: [^ true]] post: [:ignore]].
	Transcript show: 'done'; newLine.
	^ false"! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:33'!
isEdgeless
	^ self numberOfEdges = 0! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:33'!
isEmpty
	"Answer whether the receiver contains any elements."

	^ self nodes isEmpty! !

!Graph methodsFor: 'testing' stamp: 'len 7/29/2016 08:12'!
isEulerian
	"Answer true if the receiver has an Eurlerian path."
	| count |
	self flag: #fix. "it should be 'has an Eulerian circuit', and isSemiEulerian must be implemented."
	count _ 0.
	self nodesDo: [:each| each degree odd ifTrue: [(count _ count + 1) > 2 ifTrue: [^ false]]].
	^ count = 0 or: [count = 2]! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:33'!
isHamiltonian
	"Answer true if the receiver has a Hamiltonian cycle (or Hamiltonian circuit, vertex tour, or graph cycle), which is a cycle that visits each node once except for the start/end node that is visited twice."
	^ self closure isComplete "Bondy-Chvatai theorem"! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:34'!
isReflexive
	^ self nodes allSatisfy: [:each| each isReflexive]! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:34'!
isRegular
	| n |
	self isEmpty ifTrue: [^ true].
	n _ self nodes anyOne degree.
	^ self nodes allSatisfy: [:each| each degree = n]! !

!Graph methodsFor: 'testing' stamp: 'len 7/29/2016 08:11'!
isSemiEulerian
	"Answer true if the receiver has an Eurlerian path but not an Eulerian circuit."
	self notYetImplemented! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:34'!
isSimple
	"A graph is simple if doesn't countain multiple edges with the same endpoints."
	^ self nodes allSatisfy: [:each| each isSimple]! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:34'!
isTraceable
	"Answer true if the receiver has a Hamiltonian path (or traceable path), which is a path that visits each vertex exactly once."
	^ self notYetImplemented! !

!Graph methodsFor: 'testing' stamp: 'len 7/13/2016 22:37'!
isUndirected
	^ self isDirected not! !

!Graph class methodsFor: 'examples' stamp: 'len 7/20/2016 19:17'!
C: n
	"Answer the cycle of n vertices 1 -> 2 -> ... -> n -> 1."
	| C |
	C _ self unordered.
	1 to: n-1 do: [:i| C addEdge: i->(i+1)].
	n >= 1 ifTrue: [C addEdge: n -> 1]. "the extremal case of n = 1 is just a loop"
	^ C! !

!Graph class methodsFor: 'examples' stamp: 'len 7/14/2016 00:12'!
K: n
	"Answer the n-complete graph."
	| K |
	K _ self unordered.
	1 to: n do: [:i| K add: i].
	1 to: n do: [:i| 1 to: i do: [:j| i ~= j ifTrue: [K addEdge: i->j]]].
	^ K! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:40'!
N: n
	"Answer the null graph with n vertices {1..n}."
	^ self unordered addAll: (1 to: n); yourself! !

!Graph class methodsFor: 'examples' stamp: 'len 7/20/2016 19:16'!
P: n
	"Answer the path of n vertices 1 -> 2 -> ... -> n."
	| P |
	n > 1 ifFalse: [self error: 'path graph only definted for >= 2 vertices'].
	P _ self unordered.
	1 to: n-1 do: [:i| P addEdge: i->(i+1)].
	^ P! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:40'!
S: n
	"Answer the star graph of n vertices."
	| S |
	S _ self unordered.
	2 to: n do: [:i| S addEdge: 1 -> i].
	^ S! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:40'!
W: n
	"Answer the wheel graph of n vertices."
	| W |
	W _ self unordered.
	2 to: n do: [:i| W addEdge: 1 -> i; addEdge: i -> (i=n ifTrue: [2] ifFalse: [i+1])].
	^ W! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:40'!
cube
	^ self cube: 3! !

!Graph class methodsFor: 'examples' stamp: 'len 12/30/2017 16:17:15'!
cube: n
	"Answer an n-cube."
	| G H |
	G _ self unordered addEdge: 1 -> 2; yourself.
	H _ G.
	n - 1 timesRepeat: [H _ H × G].
	^ H! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:40'!
desargues
	"Answer the Desargues graph."
	^ self petersen: 10 order: 3! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:40'!
dodecahedron
	^ self petersen: 10 order: 2! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:41'!
durer
	"Answer the Durer graph."
	^ self petersen: 6 order: 2! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:46'!
exampleImplicitGraph
	"Graph exampleImplicitGraph. BUG : There is a problem when you don't  
	test classes without subclasses, maybe a problem in the block creation  
	for ImplicitGraphNode (cf. Graph>>initialize)"
	| tree |
	tree _ self implicitCollection: [:class | class subclasses].
	Magnitude withAllSubclasses
		do: [:class| "class subclasses isEmpty ifFalse: ["tree add: class"]"].
	^ tree! !

!Graph class methodsFor: 'examples' stamp: 'len 2/7/2022 13:21:46'!
exampleImplicitGraph2
	| forest |
	forest := self implicitCollection: [:class | class ~= ProtoObject ifTrue: [{class superclass}] ifFalse:[#()]].
	(Smalltalk organization listAtCategoryNamed: 'Collections-Graphs') do: [:className| forest add: (Smalltalk at: className)].
	^ forest! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:47'!
exampleImplicitGraph3
	"Graph exampleImplicitGraph3"
	| tree |
	tree _ self implicitIteratorBlock: [:class| [:aBlock| class subclasses do: aBlock]].
	RootedDigraph withAllSuperclasses do: [:class| tree add: class].
	^tree! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 23:54'!
icosahedron
	^ self unordered addEdges:
		{1->2. 2->3. 3->1.
		4->5. 5->6. 6->7. 7->8. 8->9. 9->4.
		1->9. 1->4. 1->5. 2->5. 2->6. 2->7. 3->7. 3->8. 3->9.
		10->11. 11->12. 12->10.
		10->4. 10->5. 10->6. 11->6. 11->7. 11->8. 12->8. 12->9. 12->4}; yourself! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:41'!
moebiusKantor
	"Answer the Moebius-Kantor graph."
	^ self petersen: 8 order: 3! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:41'!
nauru
	"Answer the Nauru graph."
	^ self petersen: 12 order: 5! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 23:54'!
octahedron
	^ self unordered addEdges: {1->2. 2->3. 3->1. 1->4. 2->4. 2->5. 3->5. 3->6. 1->6. 4->5. 5->6. 6->4}; yourself! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 23:54'!
petersen
	^ self unordered addEdges:
		{1->2. 2->3. 3->4. 4->5. 5->1.
		1->6. 2->7. 3->8. 4->9. 5->10.
		6->8. 6->9. 7->9. 7->10. 8->10}; yourself! !

!Graph class methodsFor: 'examples' stamp: 'len 2/7/2022 10:08:01'!
petersen: n order: k
	"Answer the generalized Petersen graph G(n,k)."
	| G |
	k < (n/2) ifFalse: [^ self error: 'k must be >= (n/2)'].
	G _ self unordered.
	0 to: n-1 do: [:i|
		G addEdges: {i->(i+1\\n). i->(i+n). i+n->(i+k\\n+n)}].
	^ G ! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 22:41'!
prism: n
	"Answer an n-prism."
	^ self petersen: n order: 1! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 23:55'!
square
	^ self unordered addEdges: {1 -> 2. 2 -> 3. 3 -> 4. 4 -> 1}; yourself! !

!Graph class methodsFor: 'examples' stamp: 'len 7/13/2016 23:55'!
triangle
	^ self unordered addEdges: {1->2. 2->3. 3->1}; yourself! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 09:58'!
arity: n
	"Create a new ordered graph with fixed arity."
	^ UndirectedGraph digraph: (Digraph arity: n)! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 09:58'!
arityLabeled: n
	"Create a new labeled, ordered graph with fixed arity."
	^ UndirectedGraph digraph: (Digraph arityLabeled: n)! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:01'!
binary
	"Create an ordered graph with arity of 2 (ie each vertex has exactly two connecting edges)."
	^ UndirectedGraph digraph: Digraph binary! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:01'!
binaryLabeled
	"Create an labeled, ordered graph with arity of 2 (ie each vertex has exactly two connecting edges)."
	^ UndirectedGraph digraph: Digraph binaryLabeled! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:01'!
implicitCollection: collectionBlock
	"Create a new graph, using the structure implicit in existing objects.
	Each node is the graph is accessed by evaluating collectionBlock to yield a collection of neighbouring nodes."
	^ UndirectedGraph digraph: (Digraph implicitCollection: collectionBlock)! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:01'!
implicitIteratorBlock: iteratorBlock
	"Create a new graph, using the structure implicit in existing objects.
	Each node is the graph is accessed by evaluating iteratorBlock to iterate over a collection of neighbouring nodes."
	^ UndirectedGraph digraph: (Digraph implicitIteratorBlock: iteratorBlock)! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:01'!
ordered
	"Create a new ordered graph."
	^ UndirectedGraph digraph: Digraph ordered! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:02'!
orderedLabeled
	"Create a new labeled, ordered graph."
	^ UndirectedGraph digraph: Digraph orderedLabeled! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:02'!
unordered
	"Create a new unordered graph."
	^ UndirectedGraph digraph: Digraph unordered! !

!Graph class methodsFor: 'instance creation' stamp: 'len 11/16/2016 10:02'!
unorderedLabeled
	"Create a new labeled, unordered graph."
	^ UndirectedGraph digraph: Digraph unorderedLabeled! !

!Graph class methodsFor: 'private' stamp: 'len 11/11/2016 21:03'!
new
	^ self shouldNotImplement! !

!Digraph methodsFor: 'accessing' stamp: 'len 2/14/2016 21:42'!
nodes
	^ nodes! !

!Digraph methodsFor: 'accessing' stamp: 'len 2/14/2016 21:55'!
type
	^ type! !

!Digraph methodsFor: 'adding/removing' stamp: 'len 7/11/2016 06:56'!
add: anObject
	"Create a new GraphNode for the value (if necessary), and add it to the graph. You shouldn't be calling this unless you're sure what you're doing. Better to add nodes by adding edges."
	^ nodes at: anObject ifAbsentPut: [nodeCreator value: ((anObject isKindOf: GraphNode) ifTrue: [anObject value] ifFalse: [anObject])]! !

!Digraph methodsFor: 'adding/removing' stamp: 'len 4/29/2016 22:29'!
addEdgeFrom: source to: target 
	| sourceNode targetNode |
	sourceNode _ self add: source.
	targetNode _ self add: target.
	sourceNode addNeighbor: targetNode! !

!Digraph methodsFor: 'adding/removing' stamp: 'len 5/25/2016 04:05'!
addEdgeFrom: source to: target label: label
	"pre: (self nodeAt: edge key) isLabeled"
	| sourceNode targetNode |
	sourceNode _ self add: source.
	targetNode _ self add: target.
	sourceNode addNeighbor: targetNode label: label! !

!Digraph methodsFor: 'adding/removing' stamp: 'timestamp) (no'!
remove: aGraphNode ifAbsent: exceptionBlock 
	"Remove the given node, evaluate exceptionBlock if it's not part of the graph.
	pre: the node shouldn't be the target of an edge."
	^nodes remove: aGraphNode ifAbsent: exceptionBlock! !

!Digraph methodsFor: 'adding/removing' stamp: 'len 5/25/2016 04:03'!
removeEdgeFrom: source to: target ifAbsent: exceptionBlock 
	"Remove the given edge, evaluate exceptionBlock if it doesn't exist. 
	pre: both the source and target of the edge must be in the graph."
	| sourceNode targetNode |
	sourceNode _ self nodeAt: source.
	targetNode _ self nodeAt: target.
	(sourceNode hasEdgeTo: targetNode)
		ifTrue: [sourceNode removeNeighbor: targetNode]
		ifFalse: [^ exceptionBlock value]! !

!Digraph methodsFor: 'converting' stamp: 'len 5/25/2016 03:15'!
asRooted
	| answer |
	answer _ RootedDigraph unordered.
	answer addAll: self.
	self edgesDo: [:each| answer addEdge: each].
	answer findRoots.
	^ answer! !

!Digraph methodsFor: 'converting' stamp: 'len 11/16/2016 09:54'!
asUndirected
	^ UndirectedGraph digraph: self symmetric! !

!Digraph methodsFor: 'copying' stamp: 'len 7/13/2016 22:56'!
copy
	^ self collect: [:each| each]! !

!Digraph methodsFor: 'copying' stamp: 'len 2/16/2016 22:19'!
copyEmpty
	"Return a graph of the same type with no nodes or edges."
	^self isImplicit
		ifTrue: [self class ordered]
		ifFalse: [super copy initialize]! !

!Digraph methodsFor: 'enumerating' stamp: 'len 7/13/2016 22:25'!
componentsDo: aBlock
	"Iterate over the Strongly Connected Components of the receiver."
	| component remainingNodes node |
	remainingNodes _ self nodes copy.
	[remainingNodes isEmpty]
		whileFalse:
			[node _ remainingNodes anyOne.
			component _ self copyEmpty.
			node markDo: [:each|
				component nodes add: each.
				remainingNodes remove: each ifAbsent: []].
			aBlock value: component]! !

!Digraph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:39'!
edgesAndLabelsDo: aBinaryBlock
	self nodesDo: [:node| node neighborsAndLabelsDo: [:n :label| aBinaryBlock value: (Association key: node value value: n value) value: label]]! !

!Digraph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:39'!
edgesDo: aBlock
	self nodesDo: [:node| node neighborsDo: [:n| aBlock value: (Association key: node value value: n value)]]! !

!Digraph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:39'!
fullEdgesAndLabelsDo: aBinaryBlock
	self nodesDo: [:node| node neighborsAndLabelsDo: [:n :label| aBinaryBlock value: (Association key: node value: n) value: label]]! !

!Digraph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:39'!
fullEdgesDo: aBlock
	self nodesDo: [:node| node neighborsDo: [:n| aBlock value: (Association key: node value: n)]]! !

!Digraph methodsFor: 'enumerating' stamp: 'len 7/20/2016 23:42'!
nodesDo: aBlock
	self isImplicit
		ifTrue: [nodes asOrderedCollection do: aBlock]
		ifFalse: [nodes do: aBlock]! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/14/2016 22:14'!
arity: n
	type _ #(#arity).
	nodeCreator _ (InitializationBlocks at: #arity) value: n! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/20/2016 06:37'!
arityLabeled: n
	type _ #(#arity #labeled).
	nodeCreator _ (InitializationBlocks at: #arityLabeled) value: n! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/14/2016 22:15'!
binary
	type _ #(#binary).
	nodeCreator _ InitializationBlocks at: #binary! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/20/2016 06:38'!
binaryLabeled
	type _ #(#binary #labeled).
	nodeCreator _ InitializationBlocks at: #binaryLabeled! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/14/2016 22:15'!
implicitCollection: collectionBlock
	type _ #(#implicitCollection).
	nodeCreator _ (InitializationBlocks at: #implicitCollection) value: self value: collectionBlock! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/14/2016 22:15'!
implicitIteratorBlock: iteratorBlock
	type _ #(#implicitIteratorBlock).
	nodeCreator _ (InitializationBlocks at: #implicitIteratorBlock) value: self value: iteratorBlock! !

!Digraph methodsFor: 'initialization' stamp: 'len 12/23/2021 14:00:18'!
initialize
	nodes _ Set new! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/14/2016 22:15'!
ordered
	type _ #(#ordered).
	nodeCreator _ InitializationBlocks at: #ordered! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/20/2016 06:37'!
orderedLabeled
	type _ #(#ordered #labeled).
	nodeCreator _ InitializationBlocks at: #orderedLabeled! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/14/2016 22:16'!
unordered
	type _ #(#unordered).
	nodeCreator _ InitializationBlocks at: #unordered! !

!Digraph methodsFor: 'initialization' stamp: 'len 2/20/2016 06:35'!
unorderedLabeled
	type _ #(#unordered #labeled).
	nodeCreator _ InitializationBlocks at: #unorderedLabeled! !

!Digraph methodsFor: 'operations' stamp: 'len 2/19/2017 17:57:54'!
complement
	"Answer the complement graph, i.e. the graph where two vertices are connected iff they are *not* connected in the receiver."
	| answer |
	answer _ self copyEmpty.
	self nodesDo: [:x|
		| neighbors |
		answer add: x.
		neighbors _ x neighbors.
		self nodesDo: [:y| (neighbors includes: y) ifFalse: [answer addEdgeFrom: x to: y]]].
	^ answer! !

!Digraph methodsFor: 'random' stamp: 'len 7/11/2016 05:32'!
atRandom: aRandom
	^ (nodes atRandom: aRandom) value! !

!Digraph methodsFor: 'random' stamp: 'len 7/11/2016 05:59'!
edgeAtRandom: aRandom
	| count |
	3 timesRepeat:
		[| source |
		(source _ nodes atRandom: aRandom) neighbors
			ifNotEmpty: [:neighbors| ^ source -> (neighbors atRandom: aRandom)]].
	count _ self numberOfEdges atRandom.
	self edgesDo: [:each| (count _ count - 1) > 0 ifFalse: [^ each]].
	self error: 'inconsitency problem, edge not found'! !

!Digraph methodsFor: 'testing' stamp: 'len 7/13/2016 20:56'!
isOrdered
	^ self type includes: #ordered! !

!Digraph methodsFor: 'testing' stamp: 'len 7/23/2016 07:29'!
isOriented
	self nodesDo: [:each|
		each neighborsDo: [:neighbor|
			(neighbor hasEdgeTo: each) ifTrue: [^ false]]].
	^ true! !

!Digraph methodsFor: 'testing' stamp: 'len 2/17/2016 03:38'!
isSymmetric
	^ nodes allSatisfy: [:each| each isSymmetric]! !

!Digraph methodsFor: 'testing' stamp: 'len 2/17/2016 03:37'!
isTransitive
	^ nodes allSatisfy: [:each| each isTransitive]! !

!Digraph methodsFor: 'testing' stamp: 'len 7/13/2016 20:56'!
isUnordered
	^ self type includes: #unordered! !

!Digraph methodsFor: 'testing' stamp: 'len 2/17/2016 02:42'!
isWeaklyConnected
	"A graph is said to be 'weakly connected' when its symmetric closure is connected."
	^ self symmetric isConnected! !

!Digraph methodsFor: 'private' stamp: 'len 7/13/2016 22:37'!
isDirected
	^ true! !

!Digraph methodsFor: 'private' stamp: 'timestamp) (no'!
isImplicit
	"Is the node creator for implicit nodes?"
	^(type includes: #implicitCollection) or: [type includes: #implicitIteratorBlock]! !

!Digraph methodsFor: 'private' stamp: 'len 2/20/2016 06:38'!
isLabeled
	"Is the node creator for labeled nodes?"
	^type includes: #labeled! !

!Digraph methodsFor: 'private' stamp: 'timestamp) (no'!
nodeCreator: n
	nodeCreator := n! !

!Digraph class methodsFor: 'initialization' stamp: 'len 7/14/2016 03:05'!
initialize
	"Digraph initialize"
	InitializationBlocks	_ Dictionary new.
	InitializationBlocks
		at: #binary
		put: [:value | ExplicitGraphNode binary: value];
		at: #arity
		put: [:n | [:value | ExplicitGraphNode on: value arity: n]];
		at: #ordered
		put: [:value | ExplicitGraphNode ordered: value];
		at: #unordered
		put: [:value | ExplicitGraphNode newOn: value];
		at: #arityLabeled
		put: [:n | [:value | LabeledExplicitGraphNode on: value arity: n]];
		at: #binaryLabeled
		put: [:value | LabeledExplicitGraphNode binary: value];
		at: #orderedLabeled
		put: [:value | LabeledExplicitGraphNode ordered: value];
		at: #unorderedLabeled
		put: [:value | LabeledExplicitGraphNode newOn: value];
		at: #implicitCollection
		put: [:graph :collectionBlock | [:value | ImplicitGraphNode
				on: value
				collectionBlock: [(collectionBlock value: value)
						collect: [:node | graph add: node]]]];
		at: #implicitIteratorBlock
		put: [:graph :iteratorBlock | [:value | 
			| iterator | 
			iterator := iteratorBlock value: value.
			ImplicitGraphNode
				on: value
				iteratorBlock: [:aBlock | iterator
						value: [:node | aBlock
								value: (graph nodes add: node)]]]]! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:50'!
arity: n
	"Create a new ordered graph with fixed arity."
	^ self basicNew initialize arity: n! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:50'!
arityLabeled: n
	"Create a new labeled, ordered graph with fixed arity."
	^ self basicNew initialize arityLabeled: n! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
binary
	"Create an ordered graph with arity of 2 (ie each vertex has exactly two connecting edges)."
	^ self basicNew initialize binary! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
binaryLabeled
	"Create an labeled, ordered graph with arity of 2 (ie each vertex has exactly two connecting edges)."
	^ self basicNew initialize binaryLabeled! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
implicitCollection: collectionBlock
	"Create a new graph, using the structure implicit in existing objects.
	Each node is the graph is accessed by evaluating collectionBlock to yield a collection of neighbouring nodes."
	^ self basicNew initialize implicitCollection: collectionBlock! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
implicitIteratorBlock: iteratorBlock
	"Create a new graph, using the structure implicit in existing objects.
	Each node is the graph is accessed by evaluating iteratorBlock to iterate over a collection of neighbouring nodes."
	^ self basicNew initialize implicitIteratorBlock: iteratorBlock! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
ordered
	"Create a new ordered graph."
	^ self basicNew initialize ordered! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
orderedLabeled
	"Create a new labeled, ordered graph."
	^ self basicNew initialize orderedLabeled! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
unordered
	"Create a new unordered graph."
	^ self basicNew initialize unordered! !

!Digraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:51'!
unorderedLabeled
	"Create a new labeled, unordered graph."
	^ self basicNew initialize unorderedLabeled! !

!RootedDigraph methodsFor: 'accessing' stamp: 'len 7/21/2016 00:04'!
root
	"pre: roots size = 1"
	^ roots anyOne! !

!RootedDigraph methodsFor: 'accessing' stamp: 'len 2/26/2016 23:21'!
rootNode
	"return the GraphNode for the root.
	pre: roots size = 1"
	^self nodeAt: roots anyOne! !

!RootedDigraph methodsFor: 'accessing' stamp: 'len 2/26/2016 23:22'!
rootNodes
	"Return the GraphNodes for the roots."
	^roots collect: [ :root | self nodeAt: root]! !

!RootedDigraph methodsFor: 'accessing' stamp: 'len 7/21/2016 00:03'!
roots
	^ roots! !

!RootedDigraph methodsFor: 'adding' stamp: 'len 2/19/2016 21:17'!
addEdgeFrom: source to: target
	super addEdgeFrom: source to: target.
	roots remove: target ifAbsent: []! !

!RootedDigraph methodsFor: 'adding' stamp: 'len 2/19/2016 21:17'!
addEdgeFrom: source to: target label: label
	super addEdgeFrom: source to: target label: label.
	roots remove: target ifAbsent: []! !

!RootedDigraph methodsFor: 'adding' stamp: 'len 7/11/2016 06:23'!
addRoot: anObject
	"Add anObject as a root of the graph."
	| node |
	node _ self add: anObject.
	roots add: node value.
	^ node! !

!RootedDigraph methodsFor: 'adding' stamp: 'timestamp) (no'!
buildFromRoots: rootCollection
	"Build the graph with given roots.
	The nodes should all be ImplicitGraphNodes, or have outgoing edges attached."
	self roots: rootCollection.
	self markDo: [ :junk ]! !

!RootedDigraph methodsFor: 'adding' stamp: 'len 2/26/2016 05:10'!
roots: rootNodes children: aBlock
	"Build the graph with given rootNodes.
	Evaluate aBlock for each node to obtain a Collection of its children."
	^self roots: rootNodes childrenGenerator:
		[ :node | [ :iterationBlock | (aBlock value: node) do: iterationBlock]]! !

!RootedDigraph methodsFor: 'adding' stamp: 'timestamp) (no'!
roots: rootNodes children: aBlock label: labelBlock
	"Build the graph with given rootNodes.
	Evaluate aBlock for each node to obtain a Collection of its children."
	^self
		roots: rootNodes 
		childrenGenerator: [ :node | [ :iterationBlock | (aBlock value: node) do: iterationBlock]]
		label: labelBlock! !

!RootedDigraph methodsFor: 'adding' stamp: 'timestamp) (no'!
roots: rootNodes childrenGenerator: aBlock
	"Build the graph with given rootNodes.
	Evaluate aBlock for each node to obtain an iterator over a Collection of its children."
	^self roots: rootNodes childrenGenerator: aBlock label: nil! !

!RootedDigraph methodsFor: 'adding' stamp: 'len 2/26/2016 20:55'!
roots: rootNodes childrenGenerator: aBlock label: labelBlock
	"Build the graph with given rootNodes.
	Evaluate aBlock for each node to obtain an iterator over a Collection of its children.
	Evaluate labelBlock (if not nil) for each node and child to obtain a label for the edge."
	| todo |
	self roots: rootNodes.
	todo := rootNodes asSet copy.
	[todo isEmpty] whileFalse:
		[| node |
		node := todo anyOne.
		(aBlock value: node) value:
			[ :child |
				(self includes: child) ifFalse: [todo add: child].
				labelBlock isNil
					ifTrue: [self addEdgeFrom: node to: child]
					ifFalse: [self addEdgeFrom: node to: child label: (labelBlock value: node value: child)]].
			todo remove: node].
	^self! !

!RootedDigraph methodsFor: 'adding' stamp: 'len 2/26/2016 05:03'!
roots: rootNodes childrenLabelGenerator: aBlock
	"Build the graph with given rootNodes.
	Evaluate aBlock for each node to obtain an iterator over a Collection of its children and labels."
	| todo |
	self roots: rootNodes.
	todo := rootNodes asSet copy.
	[todo isEmpty] whileFalse:
		[| node |
		node := todo anyOne.
		(aBlock value: node) value:
			[ :child :label |
				(self includes: child) ifFalse: [todo add: child].
				self addEdgeFrom: node to: child label: label].
			todo remove: node].
	^self! !

!RootedDigraph methodsFor: 'adding' stamp: 'timestamp) (no'!
roots: rootNodes childrenMsg: msg
	"Build the graph with given rootNodes.
	Send msg to each node to obtain a Collection of its children."
	^self roots: rootNodes childrenGenerator:
		[ :node | [ :iterationBlock | (node perform: msg) do: iterationBlock]]! !

!RootedDigraph methodsFor: 'copying' stamp: 'len 2/19/2016 21:38'!
copyEmpty
	"Return a new graph of the same type, with the same roots, but no edges."
	^ super copyEmpty roots: roots copy asSet! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'timestamp) (no'!
breadthFirstDo: aBlock
	"Evaluate aBlock for each set of nodes at a given depth in the graph.
	A node is at depth n if the longest path to that node from a root has n steps.
	pre: self isCyclic not"

	| df current |
	df := self frontier.
	[current := df frontier copy.
	current isEmpty] whileFalse:
		[aBlock value: current.
		df removeAll: current]! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'len 7/21/2016 00:02'!
collect: aBlock
	"Answer a new graph like the receiver but with vertices values mapped by aBlock.
	Note that aBlock is evaluated on the nodes, not the values."
	| answer |
	answer _ super copyEmpty.
	answer roots: (roots collect: aBlock).
	self do: [:each| answer add: (aBlock value: each)].
	self edgesDo: [:each| answer addEdgeFrom: (aBlock value: each key) to: (aBlock value: each value)].
	^ answer! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'len 7/21/2016 00:02'!
collect: aBlock labels: labelBlock
	"Answer a new graph like the receiver but with vertices values mapped by aBlock.
	Note that aBlock is evaluated on the nodes, not the values."
	| answer |
	answer _ super copyEmpty.
	self rootNodes do: [:each| answer addRoot: (aBlock value: each)].
	self do: [:each| answer add: (aBlock value: each)].
	self edgesAndLabelsDo: [:each :label| answer addEdgeFrom: (aBlock value: each key) to: (aBlock value: each value) label: (labelBlock value: label)].
	^ answer! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'len 2/17/2016 01:20'!
frontier
	"Return a DAGFrontier object, capable of iterating over this DAG."
	^ DAGFrontier on: self! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'len 11/30/2016 14:44:37'!
markDo: aBlock
	"Visit each node in the graph once, applying aBlock.
	A node is only visited after at least one of its predecessors, but not necessarily after all the predecessors."
	| todo visited |
	todo _ self rootNodes as: Set.
	visited _ Set new.
	[todo isEmpty] whileFalse:
		[| node |
		node _ todo anyOne.
		visited add: node.
		aBlock value: node.
		node neighborsDo:
			[ :child |
				(visited includes: child)
					ifFalse: [todo add: child]].
		todo remove: node]! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'len 6/24/2016 17:11'!
preOrderDo: aBlock 
	"Walk the graph, evaluating aBlock for each node.  Evaluate each node before its successors, but in no particular order when there is a choice.
	pre: self isCyclic not"

	| frontier remaining |
	frontier := self rootNodes.
	"Build a Bag of non-root nodes, each node once in the Bag for every predecessor."
	remaining := Bag new.
	nodes do: [:node | remaining addAll: node neighbors].
	
	[| next |
	next := frontier anyIfNone: [^self].
	aBlock value: next.
	frontier remove: next.
	next neighborsDo: 
			[:neighbor | 
			(remaining remove: neighbor) = 0 ifTrue: [frontier add: neighbor]]] 
			repeat! !

!RootedDigraph methodsFor: 'enumerating' stamp: 'len 2/14/2003 20:06'!
walkPre: preBlock post: postBlock
	"Recursively walk the tree(s).  Apply preBlock to each node, then walk the subtree below node, then apply postBlock to the node."
	self rootNodes do: [ :root | root walkPre: preBlock post: postBlock]! !

!RootedDigraph methodsFor: 'initialization' stamp: 'len 2/26/2016 23:01'!
findRoots
	"Find all nodes that are not the targets of an edge and make them the roots."
	roots _ self values.
	self nodesDo: [:node| node neighborsDo: [:n| roots remove: n value ifAbsent: []]]! !

!RootedDigraph methodsFor: 'initialization' stamp: 'timestamp) (no'!
initialize
	super initialize.
	roots := Set new! !

!RootedDigraph methodsFor: 'initialization' stamp: 'len 4/29/2016 22:28'!
roots: aCollection
	roots _ aCollection asSet.
	roots do: [:each| self add: each]! !

!RootedDigraph methodsFor: 'operations' stamp: 'timestamp) (no'!
depthList
	"Partitions the DAG, returning a SequenceableCollection of Sets of nodes in the graph.  Element n in the SequenceableCollection is the Set of nodes in the graph reachable in n-1 steps by the longest route from a root."
	^OrderedCollection accumulate: [ :incBlock | self breadthFirstDo: incBlock]! !

!RootedDigraph methodsFor: 'operations' stamp: 'timestamp) (no'!
reduce
	"Remove all redundant edges to form the transitive reduction.

	Based on Algorithm 4 in 'An Algorithm for Finding a Minimum Equivalent Graph of a Digraph', by D. M. Moyles and G. L. Thompson, JACM 16(3), July 1969, pp455-60 (with obvious simplifications for acyclic graphs)."
	
	self rootNodes do: [ :root | self reduceStep: root with: OrderedCollection new]! !

!RootedDigraph methodsFor: 'operations' stamp: 'len 11/30/2016 14:44:48'!
reduceSlow
	"Remove all redundant edges to form the transitive reduction.

	Based on Algorithm 4 in 'An Algorithm for Finding a Minimum Equivalent Graph of a Digraph', by D. M. Moyles and G. L. Thompson, JACM 16(3), July 1969, pp455-60 (with obvious simplifications for acyclic graphs).
	
	This version sometimes builds huge sets!!"
	
	| seqSet |
	seqSet _ Set accumulate: [ :incBlock |
				self rootNodes do: [ :root | incBlock value: (Array with: root)]].

	[seqSet isEmpty] whileFalse:
		[| newSeqSet |
"Transcript show: seqSet size printString ; cr."
		newSeqSet _ Set new.
		seqSet do: [ :seq || tail |
			tail _ seq last.
			tail neighborsDo: [ :neighbor |
				seq do: [ :node |
					(node ~= tail and: [node hasEdgeTo: neighbor])
						ifTrue: [
"Transcript show: node printString, ' -> ', neighbor printString ; cr."
								self removeEdgeFrom: node to: neighbor]].
				newSeqSet add: (seq copyWith: neighbor)]].
		seqSet _ newSeqSet]! !

!RootedDigraph methodsFor: 'operations' stamp: 'len 5/7/2016 04:13'!
reduceStep: node with: sofar 
	node degree = 0 ifTrue: [^self].
	node neighbors copy do: 
			[:succ | 
			sofar 
				do: [:done | (done hasEdgeTo: succ) ifTrue: [self removeEdgeFrom: done to: succ]].
			sofar addLast: node.
			self reduceStep: succ with: sofar.
			sofar removeLast]! !

!RootedDigraph methodsFor: 'removing' stamp: 'len 5/7/2016 04:13'!
removeAllFrom: aNode 
	"Remove the subtree below aNode (and associated edges), excluding aNode itself.
	Note that nodes are removed from the graph without being sent any messages."

	aNode walkPre: [:junk | ]
		post: 
			[:child | 
			child == aNode 
				ifTrue: [aNode neighbors copy do: [:n | aNode removeNeighbor: n]]
				ifFalse: [self remove: child]]! !

!RootedDigraph methodsFor: 'testing' stamp: 'len 6/24/2016 17:11'!
isCyclic
	"Is the graph cyclic?"

	| frontier remaining |
	frontier := self rootNodes.
	"Build a Bag of non-root nodes, each node once in the Bag for every predecessor."
	remaining := Bag new.
	nodes do: [:node | remaining addAll: node neighbors].
	
	[| next |
	next := frontier anyIfNone: [^remaining isEmpty not].
	frontier remove: next.
	next neighborsDo: 
			[:neighbor | 
			(remaining remove: neighbor) = 0 ifTrue: [frontier add: neighbor]]] 
			repeat! !

!RootedDigraph methodsFor: 'testing' stamp: 'timestamp) (no'!
isEmpty
	"Answer whether the receiver contains any elements."

	^roots isEmpty! !

!RootedDigraph methodsFor: 'testing' stamp: 'timestamp) (no'!
isTree
	"Is the graph a tree (more accurately, a forest)?
	It is if every node has at most one predecessor."
	| visited |
	visited := Set new.
	nodes do: [ :node |
		node neighborsDo: [ :neighbor |
			(visited includes: neighbor) ifTrue: [^false].
			visited add: neighbor]].
	^true! !

!RootedDigraph methodsFor: 'testing' stamp: 'len 5/5/2016 01:50'!
test
	^ #[1 2 3 4]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'timestamp) (no'!
exampleDAG
	"RootedGraph exampleDAG."
	| d |
	d := self unordered.
	d addRoot: #r1.
	d addRoot: #r2.
	d addEdge: #r1 -> #n1.
	d addEdge: #r1 -> #n2.
	d addEdge: #r2 -> #n2.
	d addEdge: #n1 -> #n3.
	d addEdge: #n2 -> #n3.
	^d! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/12/2003 22:59'!
exampleForest
	"RootedGraph exampleForest"
	^self unordered roots: (Set with: Number with: Stream) children: [ :class | class subclasses]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'timestamp) (no'!
exampleImplicitLargeTree
	"RootedGraph exampleImplicitLargeTree"
	^(self
		implicitCollection:
			[ :class |
				class == Class
					ifTrue: [#()]
					ifFalse: [class subclasses asSortedCollection: [ :x :y | x name < y name]]]) 		buildFromRoots: (Array with: Object)! !

!RootedDigraph class methodsFor: 'examples' stamp: 'timestamp) (no'!
exampleImplicitSmallTree
	"RootedGraph exampleImplicitSmallTree"
	^(self implicitCollection: [ :class | class subclasses]) buildFromRoots: (Set with: Number)! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/12/2003 22:58'!
exampleLargeTree
	"RootedGraph exampleLargeTree"
	^self ordered roots: (Array with: Object) children:
		[ :class | class == Class
					ifTrue: [#()]
					ifFalse: [class subclasses asSortedCollection: [ :x :y | x name < y name]]]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/20/2016 06:36'!
exampleMediumLabelledTree
	"RootedGraph exampleMediumLabelledTree"
	^self unorderedLabeled
		roots: (Set with: Collection)
		children: [ :class | class subclasses]
		label: [ :src :trg |
				trg isVariable
					ifTrue: [trg isBits ifTrue: [#variableByte] ifFalse: [#variable]]
					ifFalse: [#normal]]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/25/2016 20:44'!
exampleMediumTree
	^self unordered roots: (Set with: Collection) children: [ :class | class subclasses]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/14/2003 23:43'!
exampleMediumTree2
	^self unordered roots: (Set with: Collection) children: [ :class | class subclasses]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/20/2016 06:36'!
exampleSmallLabelledTree
	"RootedGraph exampleSmallLabelledTree"
	^self unorderedLabeled
		roots: (Set with: Number)
		children: [ :class | class subclasses]
		label: [ :src :trg | trg category]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/20/2016 06:36'!
exampleSmallLabelledTree2
	"RootedGraph exampleSmallLabelledTree2"
	^self unorderedLabeled
		roots: (Set with: Magnitude)
		children: [ :class | class subclasses]
		label: [ :src :trg |
				trg isVariable
					ifTrue: [trg isBits ifTrue: [#variableByte] ifFalse: [#variable]]
					ifFalse: [#normal]]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/12/2003 22:42'!
exampleSmallTree
	"RootedGraph exampleSmallTree"
	^self unordered roots: (Set with: Number) children: [ :class | class subclasses]! !

!RootedDigraph class methodsFor: 'examples' stamp: 'len 2/18/2003 22:09'!
exampleSmallTree2
	"RootedGraph exampleSmallTree2"
	^self unordered roots: (Set with: Magnitude) childrenMsg: #subclasses! !

!UndirectedGraph methodsFor: 'accessing' stamp: 'len 7/13/2016 22:53'!
nodes
	^ digraph nodes! !

!UndirectedGraph methodsFor: 'accessing' stamp: 'len 7/13/2016 21:45'!
numberOfEdges
	^ digraph numberOfEdges / 2! !

!UndirectedGraph methodsFor: 'accessing' stamp: 'len 7/16/2016 08:25'!
type
	^ digraph type! !

!UndirectedGraph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 22:52'!
add: anObject
	^ digraph add: anObject! !

!UndirectedGraph methodsFor: 'adding/removing' stamp: 'len 5/25/2016 04:07'!
addEdgeFrom: source to: target 
	| sourceNode targetNode |
	sourceNode _ self add: source.
	targetNode _ self add: target.
	sourceNode addNeighbor: targetNode.
	targetNode addNeighbor: sourceNode! !

!UndirectedGraph methodsFor: 'adding/removing' stamp: 'len 5/25/2016 04:06'!
addEdgeFrom: source to: target label: label
	"pre: (self nodeAt: edge key) isLabeled"
	| sourceNode targetNode |
	sourceNode _ self add: source.
	targetNode _ self add: target.
	sourceNode addNeighbor: targetNode label: label.
	targetNode addNeighbor: sourceNode label: label! !

!UndirectedGraph methodsFor: 'adding/removing' stamp: 'len 7/13/2016 22:59'!
remove: aGraphNode ifAbsent: exceptionBlock 
	"Remove the given node, evaluate exceptionBlock if it's not part of the graph.
	pre: the node shouldn't be the target of an edge."
	^ digraph remove: aGraphNode ifAbsent: exceptionBlock! !

!UndirectedGraph methodsFor: 'adding/removing' stamp: 'len 5/25/2016 04:04'!
removeEdgeFrom: source to: target ifAbsent: exceptionBlock 
	"Remove the given edge, evaluate exceptionBlock if it doesn't exist. 
	pre: both the source and target of the edge must be in the graph."
	| sourceNode targetNode |
	sourceNode _ self nodeAt: source.
	targetNode _ self nodeAt: target.
	(sourceNode hasEdgeTo: targetNode)
		ifTrue:
			[sourceNode removeNeighbor: targetNode.
			targetNode removeNeighbor: sourceNode]
		ifFalse: [^ exceptionBlock value]! !

!UndirectedGraph methodsFor: 'converting' stamp: 'len 2/15/2017 15:53:55'!
asDirected
	^ digraph! !

!UndirectedGraph methodsFor: 'copying' stamp: 'len 7/13/2016 22:57'!
copy
	^ self class digraph: digraph copy! !

!UndirectedGraph methodsFor: 'copying' stamp: 'len 7/13/2016 22:30'!
copyEmpty
	^ self class digraph: digraph copyEmpty! !

!UndirectedGraph methodsFor: 'enumerating' stamp: 'len 7/13/2016 22:58'!
componentsDo: aBlock
	digraph componentsDo: [:each| aBlock value: (self class digraph: each)]! !

!UndirectedGraph methodsFor: 'enumerating' stamp: 'len 11/30/2016 14:45:13'!
edgesAndLabelsDo: aBinaryBlock
	| visited |
	visited _ Set new.
	digraph edgesAndLabelsDo: [:each :label|
		(visited includes: each value -> each key)
			ifFalse: [aBinaryBlock value: (each as: MutualAssociation) value: label].
		visited add: each]! !

!UndirectedGraph methodsFor: 'enumerating' stamp: 'len 11/30/2016 14:45:17'!
edgesDo: aBlock
	| visited |
	visited _ Set new.
	digraph edgesDo: [:each|
		(visited includes: each value -> each key)
			ifFalse: [aBlock value: (each as: MutualAssociation)].
		visited add: each]! !

!UndirectedGraph methodsFor: 'enumerating' stamp: 'len 11/30/2016 14:45:20'!
fullEdgesAndLabelsDo: aBinaryBlock
	| visited |
	visited _ Set new.
	digraph fullEdgesAndLabelsDo: [:each :label|
		(visited includes: each value -> each key)
			ifFalse: [aBinaryBlock value: (each as: MutualAssociation) value: label].
		visited add: each]! !

!UndirectedGraph methodsFor: 'enumerating' stamp: 'len 11/30/2016 14:45:25'!
fullEdgesDo: aBlock
	| visited |
	visited _ Set new.
	digraph fullEdgesDo: [:each|
		(visited includes: each value -> each key)
			ifFalse: [aBlock value: (each as: MutualAssociation)].
		visited add: each]! !

!UndirectedGraph methodsFor: 'enumerating' stamp: 'len 7/13/2016 22:53'!
nodesDo: aBlock
	digraph nodesDo: aBlock! !

!UndirectedGraph methodsFor: 'initialization' stamp: 'len 7/13/2016 22:29'!
digraph: aDigraph
	digraph _ aDigraph! !

!UndirectedGraph methodsFor: 'operations' stamp: 'len 7/21/2016 02:44'!
complement
	^ self class digraph: digraph complement! !

!UndirectedGraph methodsFor: 'operations' stamp: 'len 7/13/2016 22:13'!
symmetric
	^ self! !

!UndirectedGraph methodsFor: 'operations' stamp: 'len 8/9/2020 07:49:51'!
transpose
	^ self! !

!UndirectedGraph methodsFor: 'random' stamp: 'len 7/13/2016 23:14'!
atRandom: aRandom
	^ digraph atRandom: aRandom! !

!UndirectedGraph methodsFor: 'random' stamp: 'len 7/13/2016 23:15'!
edgeAtRandom: aRandom
	^ (digraph edgeAtRandom: aRandom) as: MutualAssociation! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/13/2016 22:37'!
isDirected
	^ false! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/21/2016 02:13'!
isImplicit
	^ digraph isImplicit! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/21/2016 02:13'!
isLabeled
	^ digraph isLabeled! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/21/2016 02:13'!
isOrdered
	^ digraph isOrdered! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/13/2016 22:35'!
isSymmetric
	^ true! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/13/2016 22:35'!
isTransitive
	^ true! !

!UndirectedGraph methodsFor: 'testing' stamp: 'len 7/13/2016 22:36'!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	^(self includes: anObject) ifTrue: [1] ifFalse: [0]! !

!UndirectedGraph class methodsFor: 'instance creation' stamp: 'len 7/13/2016 22:27'!
digraph: aDigraph
	^ self basicNew digraph: aDigraph! !

!DAGFrontier methodsFor: 'accessing' stamp: 'timestamp) (no'!
frontier
	^frontier! !

!DAGFrontier methodsFor: 'advancing' stamp: 'len 6/24/2016 17:11'!
remove: frontierObject
	frontier remove: frontierObject.
	frontierObject neighborsDo:
		[ :neighbor |
			(bag remove: neighbor) = 0
				ifTrue: [frontier add: neighbor]].
	^frontierObject! !

!DAGFrontier methodsFor: 'advancing' stamp: 'timestamp) (no'!
removeAll: collection
	collection do: [ :n | self remove: n]! !

!DAGFrontier methodsFor: 'initialization' stamp: 'timestamp) (no'!
frontier: f bag: remaining
	frontier := f.
	bag := remaining! !

!DAGFrontier class methodsFor: 'instance creation' stamp: 'len 2/17/2016 01:20'!
on: rootedGraph
	"Create a frontier object to enumerate the graph."
	| remaining frontier |
	"Build a Bag of non-root nodes, each node once in the Bag for every predecessor."
	frontier _ rootedGraph rootNodes asOrderedCollection.
	remaining _ Bag new.
	rootedGraph nodesDo: [:node| node neighborsDo: [:n| remaining add: n]].
	^ self new frontier: frontier bag: remaining! !

!Dijkstra methodsFor: 'accessing' stamp: 'len 2/15/2016 06:47'!
distanceTo: anObject
	^ distance at: anObject! !

!Dijkstra methodsFor: 'accessing' stamp: 'len 2/13/2003 21:56'!
eccentricity
	"Answer the eccentricity of the source vertex.
	The eccentricity of a vertex is the length of the longest minimal path from that vertex to some vertex in the graph. You can think of the eccentricity of a vertex as the longest distance in the graph from there to somewhere."

	^ distance max! !

!Dijkstra methodsFor: 'accessing' stamp: 'len 2/26/2016 23:18'!
shortestPathTo: anObject
	| answer node predecessorNode |
	anObject = source ifTrue: [^ #()].
	answer _ OrderedCollection new.
	node _ graph nodeAt: anObject.
	[answer add: node value.
	predecessorNode _ predecessor at: node.
	predecessorNode = source]
		whileFalse: [node _ predecessorNode].
	^ answer reversed! !

!Dijkstra methodsFor: 'initialization' stamp: 'len 2/15/2003 18:52'!
graph: aGraph source: aNode
	graph _ aGraph.
	source _ aNode.
	self run! !

!Dijkstra methodsFor: 'private' stamp: 'len 2/7/2022 14:19:08'!
run
	| queue u estimate |
	predecessor _ Dictionary new.
	distance _ Dictionary new.
	graph nodesDo: [:each |
		distance at: each put: Float infinity.
		predecessor at: each put: nil].
	distance at: source put: 0.
	queue _ Heap sortBlock: [:a :b| (distance at: a) <= (distance at: b)].
	queue addAll: graph.
	[queue isEmpty]
		whileFalse:
			[u _ queue removeFirst.
			(graph nodeAt: u) neighborsAndLabelsDo: [:each :weight|
				estimate _ (distance at: u) + 1 "(weight ifNil: [1])".
				(distance at: each) > estimate
					ifTrue:
						[distance at: each put: estimate.
						predecessor at: each put: u.
						queue reSort]]]! !

!Dijkstra class methodsFor: 'instance creation' stamp: 'len 2/14/2016 22:38'!
graph: aGraph source: aNode
	^ self new graph: aGraph source: aNode! !

!FloydWarshall methodsFor: 'accessing' stamp: 'len 2/17/2016 07:24'!
diameter
	^ distances max! !

!FloydWarshall methodsFor: 'accessing' stamp: 'len 2/17/2016 07:21'!
distanceFrom: source to: target
	^ distances at: {source. target}! !

!FloydWarshall methodsFor: 'accessing' stamp: 'len 2/17/2016 07:24'!
radius
	^ distances min! !

!FloydWarshall methodsFor: 'initialization' stamp: 'len 2/17/2016 07:04'!
graph: aGraph
	graph _ aGraph.
	self run! !

!FloydWarshall methodsFor: 'private' stamp: 'len 2/17/2016 07:18'!
run
	distances _ Dictionary new.
	graph nodesDo: [:x| distances at: {x.x} put: 0].
	graph edgesAndLabelsDo: [:each :label|
		distances at: {each key. each value} put: (label ifNil: [1])].
	graph nodesDo: [:k|
		graph nodesDo: [:i|
			graph nodesDo: [:j|
				| d |
				(d _ (distances at: {i.k}) + (distances at: {k.j})) > (distances at: {i.j})
				 	ifTrue: [distances at: {i.j} put: d]]]]! !

!FloydWarshall class methodsFor: 'instance creation' stamp: 'len 2/17/2016 07:22'!
graph: aGraph
	^ self new graph: aGraph! !

!GraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
degree
	"How many edges does this node have?"
	self subclassResponsibility! !

!GraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:14'!
neighbors
	"Return a collection of nodes connected to outgoing edges."
	self subclassResponsibility! !

!GraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
neighborsAndLabelsDo: aBlock
	"Evaluate aBlock for each node connected to an outgoing edge, and the label on that edge (nil if no label)."
	"The default definition assumes no labels are present."
	self neighborsDo: [ :n | aBlock value: n value: nil]! !

!GraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
neighborsDo: aBlock
	"Evaluate aBlock for each node connected to an outgoing edge."
	self subclassResponsibility! !

!GraphNode methodsFor: 'accessing value' stamp: 'timestamp) (no'!
value
	^value! !

!GraphNode methodsFor: 'accessing value' stamp: 'len 2/14/2016 22:31'!
value: anObject
	value _ anObject! !

!GraphNode methodsFor: 'comparing' stamp: 'timestamp) (no'!
= anObject
	"Comparison is delegated to the value of the node.
	Argument and receiver are reversed to dereference through other GraphNodes."
	^anObject = value! !

!GraphNode methodsFor: 'comparing' stamp: 'timestamp) (no'!
hash
	^value hash! !

!GraphNode methodsFor: 'enumerating' stamp: 'len 11/30/2016 14:43:32'!
markDo: aBlock
	"Visit each node in the graph once, applying aBlock.
	A node is only visited after at least one of its predecessors, but not necessarily after all the predecessors."
	| todo visited |
	todo _ Set with: self.
	visited _ Set new.
	[todo isEmpty] whileFalse:
		[| node |
		node _ todo anyOne.
		visited add: node.
		aBlock value: node.
		node neighborsDo:
			[ :child |
				(visited includes: child)
					ifFalse: [todo add: child]].
		todo remove: node]! !

!GraphNode methodsFor: 'enumerating' stamp: 'len 2/14/2003 20:05'!
walkPre: preBlock post: postBlock
	"Recursively walk the subtree rooted at me.  Apply preBlock to each node, then walk the subtree below node, then apply postBlock to the node."
	preBlock value: self.
	self neighborsDo: [ :child | child walkPre: preBlock post: postBlock].
	postBlock value: self! !

!GraphNode methodsFor: 'operations' stamp: 'len 7/20/2016 08:48'!
span
	"Answer the spanning tree of the receiver."
	| tree |
	tree _ RootedDigraph ordered.
	tree roots: (Set with: self).
	self markDo: [:each| tree add: each].
	^ tree! !

!GraphNode methodsFor: 'statistics' stamp: 'len 5/7/2016 04:09'!
clusteringCoefficient
	| links k |
	links _ Set new.
	self neighborsDo: [:n1 |
		self neighborsDo: [:n2 |
			(n1 ~= n2 and: [n1 hasEdgeTo: n2])
				ifTrue: [links add: {n1 value. n2 value}]]].
	k _ self neighbors size.
	^ links size / (k * (k - 1))! !

!GraphNode methodsFor: 'testing edges' stamp: 'timestamp) (no'!
hasEdgeTo: anObject
	"Is there an edge from this node to anObject?"
	self subclassResponsibility! !

!GraphNode methodsFor: 'testing edges' stamp: 'len 2/26/2016 20:20'!
hasLoop
	^ self hasEdgeTo: self! !

!GraphNode methodsFor: 'testing edges' stamp: 'timestamp) (no'!
isLeaf
	^self degree = 0! !

!GraphNode methodsFor: 'testing edges' stamp: 'len 2/16/2016 03:39'!
isReflexive
	^ self hasEdgeTo: self! !

!GraphNode methodsFor: 'testing edges' stamp: 'len 5/7/2016 04:09'!
isSimple
	^ self neighbors asSet size = self neighbors size and: [self hasLoop not]! !

!GraphNode methodsFor: 'testing edges' stamp: 'len 2/16/2016 03:40'!
isSymmetric
	self neighborsDo: [:each| (each hasEdgeTo: self) ifFalse: [^ false]].
	^ true! !

!GraphNode methodsFor: 'testing edges' stamp: 'len 2/17/2016 03:37'!
isTransitive
	self neighborsDo: [:each|
		each neighborsDo: [:other| (other hasEdgeTo: self) ifFalse: [^ false]]].
	^ true! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:03'!
degree
	"If I am in my neighborhood, don't count the loop edge"

	^ (outNeighbors includes: self) 
		ifTrue: [outNeighbors size - 1]
		ifFalse: [outNeighbors size]! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
inDegree
	"If I am in my neighborhood, don't count the loop edge"
	(inNeighbors includes: self)
		ifTrue: [^ inNeighbors size - 1]
		ifFalse: [^ inNeighbors size]
! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'Samir Saidani 9/25/2003 14:14'!
inNeighbors
	^inNeighbors! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:07'!
neighbors
	^outNeighbors! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:07'!
neighborsDo: aBlock 
	outNeighbors do: aBlock! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 2/15/2016 01:54'!
strictInNeighbors
	(inNeighbors includes: self) ifTrue: [^ inNeighbors copyWithout: self].
	^ inNeighbors! !

!ExplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:15'!
strictNeighbors
	(outNeighbors includes: self) ifTrue: [^ outNeighbors copyWithout: self].
	^ outNeighbors! !

!ExplicitGraphNode methodsFor: 'changing edges' stamp: 'len 5/9/2016 17:10'!
addNeighbor: aNode 
	outNeighbors add: aNode.
	aNode addInNeighborNoCheck: self.
	^ aNode
"	self flag: #fix.
	(outNeighbors includes: aNode)
		ifFalse: 
			[outNeighbors add: aNode.
			aNode addInNeighborNoCheck: self].
	^aNode"! !

!ExplicitGraphNode methodsFor: 'changing edges' stamp: 'len 5/7/2016 04:19'!
addNeighbor: aNode at: index 
	"pre: self isOrdered"
	outNeighbors at: index put: aNode.
	aNode addInNeighborNoCheck: self.
	^aNode! !

!ExplicitGraphNode methodsFor: 'changing edges' stamp: 'len 2/26/2016 22:46'!
removeInNeighbor: aNode 
	^ inNeighbors remove: aNode ifAbsent: [aNode]! !

!ExplicitGraphNode methodsFor: 'changing edges' stamp: 'len 2/26/2016 22:54'!
removeNeighbor: aNode
	aNode removeInNeighbor: self.
	^outNeighbors remove: aNode! !

!ExplicitGraphNode methodsFor: 'copying' stamp: 'len 2/28/2016 17:02'!
copy
	^ super copy setNeighbors: outNeighbors copy; setInNeighbors: inNeighbors copy! !

!ExplicitGraphNode methodsFor: 'initialization' stamp: 'len 7/14/2016 00:12'!
initializeFixed: arity 
	outNeighbors _ Array new: arity.
	inNeighbors _ Bag new! !

!ExplicitGraphNode methodsFor: 'initialization' stamp: 'len 7/14/2016 00:12'!
initializeOrdered
	outNeighbors _ OrderedCollection new.
	inNeighbors _ Bag new! !

!ExplicitGraphNode methodsFor: 'initialization' stamp: 'len 7/14/2016 00:12'!
initializeUnordered
	outNeighbors _ Bag new.
	inNeighbors _ Bag new! !

!ExplicitGraphNode methodsFor: 'testing' stamp: 'sam 2/9/2004 15:30'!
isAnExtremity
	^ self isFinal | self isInitial | self isExtremal! !

!ExplicitGraphNode methodsFor: 'testing' stamp: 'len 5/7/2016 04:06'!
isExtremal
	"o<-->..."
	^ self degree = 1 and: [self inNeighbors asSet = self neighbors asSet]! !

!ExplicitGraphNode methodsFor: 'testing' stamp: 'len 5/7/2016 04:04'!
isFinal
" o<--..."
	^ self inDegree = 1 and: [self degree = 0]! !

!ExplicitGraphNode methodsFor: 'testing' stamp: 'len 5/7/2016 04:05'!
isInitial
	"o-->..."
	^ self inDegree = 0 and: [self degree = 1]! !

!ExplicitGraphNode methodsFor: 'testing' stamp: 'len 5/10/2019 04:36:13'!
isMiddle
	"..--o--..."
	^ (self inDegree + self degree between: 2 and: 4)
		and: [(self inNeighbors \/ self neighbors) size = 2]! !

!ExplicitGraphNode methodsFor: 'testing' stamp: 'timestamp) (no'!
isOrdered
	^outNeighbors isSequenceable! !

!ExplicitGraphNode methodsFor: 'testing edges' stamp: 'timestamp) (no'!
hasEdgeTo: anObject 
	^outNeighbors includes: anObject! !

!ExplicitGraphNode methodsFor: 'private' stamp: 'len 2/26/2016 22:44'!
addInNeighborNoCheck: aNode
	^ inNeighbors add: aNode! !

!ExplicitGraphNode methodsFor: 'private' stamp: 'len 2/28/2016 17:02'!
setInNeighbors: aCollection
	inNeighbors _ aCollection! !

!ExplicitGraphNode methodsFor: 'private' stamp: 'len 2/28/2016 17:02'!
setNeighbors: aCollection
	outNeighbors _ aCollection! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'timestamp) (no'!
arity: n
	"Create a new ordered node with fixed arity n."
	^super new initializeFixed: n! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'timestamp) (no'!
binary
	"Create a new binary node."
	^super new initializeFixed: 2! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 2/12/2003 21:40'!
binary: value
	"Create a new binary node, and given value."
	^super new initializeFixed: 2; value: value! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'timestamp) (no'!
new
	"Create a new unordered node."
	^super new initializeUnordered! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 2/12/2003 21:40'!
newOn: value
	"Create a new unordered node, and given value."
	^super new initializeUnordered; value: value! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 2/12/2003 21:40'!
on: value arity: n
	"Create a new ordered node with fixed arity n, and given value."
	^super new initializeFixed: n; value: value! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'timestamp) (no'!
ordered
	"Create a new ordered node."
	^super new initializeOrdered! !

!ExplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 2/12/2003 21:40'!
ordered: value
	"Create a new ordered node, and given value."
	^super new initializeOrdered; value: value! !

!LabeledExplicitGraphNode methodsFor: 'accessing' stamp: 'len 2/19/2016 18:49'!
label
	^ label! !

!LabeledExplicitGraphNode methodsFor: 'accessing' stamp: 'len 2/19/2016 18:49'!
label: anObject
	label _ anObject! !

!LabeledExplicitGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
labelOfEdgeTo: anObject 
	"Return the label associated with the edge to anObject; otherwise signal an error."

	^(outNeighbors detect: [:assoc | assoc key = anObject]) value! !

!LabeledExplicitGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
labelOfEdgeTo: anObject ifNone: aBlock 
	"Return the label associated with the edge to anObject; return the value of the block is no such edge exists."

	^(outNeighbors detect: [:assoc | assoc key = anObject]
		ifNone: [^aBlock value]) value! !

!LabeledExplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:11'!
neighbors
	^outNeighbors collect: [:assoc | assoc key]! !

!LabeledExplicitGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
neighborsAndLabelsDo: aBlock 
	"Evaluate aBlock for each node connected to an outgoing edge, and the label on that edge (nil if no label)."

	^outNeighbors do: [:assoc | aBlock value: assoc key value: assoc value]! !

!LabeledExplicitGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
neighborsDo: aBlock 
	^outNeighbors do: [:assoc | aBlock value: assoc key]! !

!LabeledExplicitGraphNode methodsFor: 'changing edges' stamp: 'timestamp) (no'!
addNeighbor: aNode
	self shouldNotImplement! !

!LabeledExplicitGraphNode methodsFor: 'changing edges' stamp: 'timestamp) (no'!
addNeighbor: aNode at: index
	self shouldNotImplement! !

!LabeledExplicitGraphNode methodsFor: 'changing edges' stamp: 'len 6/12/2019 11:07:56'!
addNeighbor: aNode at: index label: anObject
	"pre: self isOrdered"
	outNeighbors at: index put: aNode -> anObject.
	^ aNode! !

!LabeledExplicitGraphNode methodsFor: 'changing edges' stamp: 'len 6/12/2019 11:07:48'!
addNeighbor: aNode label: anObject
	self flag: #fix.
	(outNeighbors includes: aNode) "inefficient"
		ifFalse: 
			[outNeighbors add: aNode -> anObject.
			aNode addInNeighborNoCheck: self].
	^aNode! !

!LabeledExplicitGraphNode methodsFor: 'changing edges' stamp: 'timestamp) (no'!
removeNeighbor: aNode 
	outNeighbors remove: (outNeighbors detect: [:assoc | assoc key = aNode])! !

!LabeledExplicitGraphNode methodsFor: 'comparing' stamp: 'len 7/25/2016 08:25'!
= anObject
	anObject class = self class ifTrue: [^ value = anObject value and: [label = anObject label]].
	^ super = anObject! !

!LabeledExplicitGraphNode methodsFor: 'testing edges' stamp: 'timestamp) (no'!
hasEdgeTo: anObject 
	^outNeighbors anySatisfy: [:assoc | assoc key = anObject]! !

!ImplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:09'!
neighbors
	^OrderedCollection accumulate: [:incBlock | self neighborsDo: incBlock]! !

!ImplicitGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:09'!
neighborsDo: aBlock 
	self neighbors do: aBlock! !

!ImplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 4/1/2018 15:37:55'!
on: aValue collectionBlock: aBlock
	^ImplicitCollectionGraphNode on: aValue collectionBlock: aBlock! !

!ImplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 4/1/2018 15:37:09'!
on: aValue iterator: anIterator
	^ImplicitIteratorGraphNode on: aValue iterator: anIterator! !

!ImplicitGraphNode class methodsFor: 'instance creation' stamp: 'len 4/1/2018 15:37:19'!
on: aValue iteratorBlock: iteratorBlock
	^ImplicitIteratorGraphNode on: aValue iteratorBlock: iteratorBlock! !

!ImplicitCollectionGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:10'!
degree
	^self neighbors size! !

!ImplicitCollectionGraphNode methodsFor: 'accessing edges' stamp: 'len 5/7/2016 04:10'!
neighbors
	^collectionBlock value! !

!ImplicitCollectionGraphNode methodsFor: 'initialisation' stamp: 'len 2/28/2016 20:32'!
collectionBlock: aBlock
	collectionBlock _ aBlock! !

!ImplicitCollectionGraphNode methodsFor: 'testing edges' stamp: 'len 5/7/2016 04:10'!
hasEdgeTo: anObject 
	^self neighbors includes: anObject! !

!ImplicitCollectionGraphNode class methodsFor: 'instance creation' stamp: 'len 2/15/2016 05:20'!
on: aValue collectionBlock: aBlock 
	^ self new value: aValue; collectionBlock: aBlock! !

!ImplicitIteratorGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
degree
	^iterator size! !

!ImplicitIteratorGraphNode methodsFor: 'accessing edges' stamp: 'timestamp) (no'!
neighborsDo: aBlock
	iterator do: aBlock! !

!ImplicitIteratorGraphNode methodsFor: 'initialisation' stamp: 'len 2/28/2016 20:32'!
iterator: anIterator
	iterator _ anIterator! !

!ImplicitIteratorGraphNode methodsFor: 'testing edges' stamp: 'timestamp) (no'!
hasEdgeTo: anObject
	^iterator includes: anObject! !

!ImplicitIteratorGraphNode class methodsFor: 'instance creation' stamp: 'timestamp) (no'!
on: aValue iterator: anIterator
	^(self new) value: aValue ; iterator: anIterator! !

!ImplicitIteratorGraphNode class methodsFor: 'instance creation' stamp: 'timestamp) (no'!
on: aValue iteratorBlock: iteratorBlock
	^self on: aValue iterator: (Iterator on: iteratorBlock)! !
Digraph initialize!
