FARCE
=====

FeAtureConstraintsExtraction (FARCE) can extract constraints from build-time errors and the effects of feature selection on the code structure.
It relies on information extracted by [TypeChef](https://github.com/snadi/TypeChef). FARCE can also compare constraints and Boolean formulas.


Usage
=====

You can use FARCE to analyze any C project which uses conditional compilation through #ifdef directives. Here is a general guideline on how you can setup a subject system to be analyzed by FARCE. To make things easier, we have included some of the scripts you might need in the scripts/ directory.

- Download the source code of the system to be analyzed.
- Build this system with a couple of different configurations to make sure you have all required header files and scripts on your machine.
- Analyze the build system to extract file presence conditions. If the system uses Kbuild, you can use [KBuildMiner](http://code.google.com/p/variability/source/browse/KBuildMiner/) to automatically analyze it. Otherwise, you can do a combination of manual and automated analysis to extract the file pcs.
- Checkout FARCE, and run "sbt mkrun" to generate a run.sh script with the right paths etc.
- Checkout TypeChef and run "sbt mkrun" to generate typechef.sh script or you can use an assembled jar file.
- Run farce/run.sh gsd.farce.filepcs.ProcessFileList <file with list of file pcs> <source directory> \[Prefix for feature names\]
- Make sure you have the required header files needed for the analysis. You can use the following [system headers](http://www.informatik.uni-marburg.de/~kaestner/tmp/includes-redhat.tar.bz2). If you use something else, make sure to adjust the paths in redhat.properties file.
- The jcpp.sh script makes it easier to run TypeChef, and the analyze.sh script can be used as a guiding template to what you need to run TypeChef properly.
- After you run TypeChef, you can run the farceAnalysis.sh to extract constraints from TypeChef's output files. The property keys you will need are in the template.properties file. You can change them as you want. If you want to use the same structure we did, you can use the createOutput script to create the necessary directory structure.
- If you want to also run the comparisons against an existing feature model, then you need the following
	* You need to extract the full formula representing the constraints in the variability model. Check [LVAT](http://code.google.com/p/linux-variability-analysis-tools/) for systems using Kconfig.
	* You need to add your system to gsd.farce.utilities.SystemsConfigs.scala to describe some of its properties, and update the functions in gsd.farce.utilities.Utilities accordingly
	* Use the compareModels.sh script to compare the extracted constraints to the model and vice versa
- You can check samples of systems which we have already analyzed [uClibc](https://bitbucket.org/snadi/farce-uclibc/), [BusyBox](https://bitbucket.org/snadi/farce-busyboxanalysis), [eCos](https://bitbucket.org/snadi/farce-ecosanalysis), and [Linux](https://bitbucket.org/snadi/farce-linuxanalysis). 
- Most of the code in FARCE has some documentation to explain what its doing. We will be updating this description soon to give a general overview of the code structure.

Project Members
===============
- [Sarah Nadi](http://swag.uwaterloo.ca/~snadi)
- [Thorsten Berger](http://www.itu.dk/people/thbe/)
- [Christian Kästner](http://cs.cmu.edu/~ckaestne)
- [Krzysztof Czarnecki](http://gsd.uwaterloo.ca/kczarnec)


Contact Us
==========
If you have any questions or clarifications, please contact one of us.
