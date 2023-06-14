# Arrows: a Computer Algebra System in Smalltalk
Arrows is a very general, extensible and mathematically rigorous environment where one can construct mathematical objects (such as groups, rings, modules, etc) and operate with them, their morphisms and their elements.

For an introduction to the system see [Arrows: a Computer Algebra System in Smalltalk](https://len.github.io/Arrows.pdf), published in November 2022 in FAST Workshop on Smalltalk Related Technologies.

### Smalltalk
The system is based on Smalltalk-80, specifically [Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev). It is multiplatform and runs with the [OpenSmalltalk virtual machine](https://github.com/OpenSmalltalk/opensmalltalk-vm).

As many systems based on Smalltalk, it comes complete with an environment for interactive programming, blurring the distinction between user and programmer. It contains a complete development environment that allows you to browse classes, inspect objects, debug, and change anything 'live' while it is running.

### Notation
The notation is intended to be as close as possible to standard mathematical notation, yet adhering completly to Smalltalk syntax. For this we use unicode in Smalltalk code. Special characters can be input by typing backslash followed by the character name, similar to LaTeX. For example, ℤ can be input as \\Z, α as \\alpha and ⊗ as \\otimes.

### Setup
You need to [setup Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/GettingStarted.md) first. Once you have Cuis running in your system you can install the Arrows packages in a new Cuis image, or if you want to try it more easily you can just download the [latest Arrows image](https://len.github.io/arrows-20230615.zip) and unzip it in the directory where you have already setup Cuis. Currently it only supports 64-bit processors, so you will need to download the 64-bit virtual machine to run it.

### Citing Arrows
If you use Arrows in a non-trivial part of your research please consider citing it as follows:

	@manual{Arrows,
	  key = "Arrows",
	  author = "Luciano Notarfrancesco",
	  organization = "The Arrows~Team",
	  title = "{The Arrows Computer Algebra System}",
	  year = 2023,
	  url = "\url{https://github.com/len/arrows}",
	}

