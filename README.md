# Arrows: a Computer Algebra System in Smalltalk
Arrows is a very general, extensible and mathematically rigorous environment where one can construct mathematical objects (such as groups, rings, modules, etc) and operare with them, their morphisms and their elements.

For an introduction to the system see [Arrows: a Computer Algebra System in Smalltalk](https://openreview.net/pdf?id=oIozVmVool), published in November 2022 in FAST Workshop on Smalltalk Related Technologies.

### Smalltalk
The system is based on Smalltalk-80, specifically [Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev). It is multiplatform and runs with the [OpenSmalltalk virtual machine](https://github.com/OpenSmalltalk/opensmalltalk-vm).

As many systems based on Smalltalk, it comes complete with an environment for interactive programming, blurring the distinction between user and programmer. It contains a complete development environment that allows you to browse classes, inspect objects, debug, and change anything 'live' while it is running.

### Notation
The notation is intended to be as close as possible to standard mathematical notation, yet adhering completly to Smalltalk syntax. For this we use unicode in Smalltalk code. Special characters can be input by typing backslash followed by the character name, similar to LaTeX. For example, ℤ can be input as \\Z, α as \\alpha and ⊗ as \\otimes.

### Citing Arrows
If you use Arrows in a non-trivial part of your research please consider citing it as follows:

	@manual{Arrows,
	  key = "Arrows",
	  author = "Luciano Notarfrancesco",
	  organization = "The Arrows~Team",
	  title = "{The Arrows Computer Algebra System}",
	  year = 2022,
	  url = "\url{https://github.com/len/arrows}",
	}

