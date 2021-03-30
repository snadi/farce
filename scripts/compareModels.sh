farceCommand=../../farce/run.sh

system=<system name>
prefix=$prefix

################################ Feature Model vs Code ############################################
#model vs code comparison: hierarchy edges and cross tree edges
$farceCommand gsd.farce.comparisons.FeatureModelComparer $system output/FeatureModel/hierarchy.constraints > output/Comparison/FeatureModelVsCode/hierarchySingle.csv  2> output/Comparison/FeatureModelVsCode/hierarchySingleStats.txt 


$farceCommand gsd.farce.comparisons.FeatureModelComparer $system output/FeatureModel/crosstree.constraints > output/Comparison/FeatureModelVsCode/crosstreeSingle.csv  2> output/Comparison/FeatureModelVsCode/crosstreeSingleStats.txt 


############################# Code vs Feature Model ###################################################

$farceCommand gsd.farce.comparisons.CodeToVMComparer $system output/Linker/defUseConstraints.txt $prefix > output/Comparison/CodeVsFeatureModel/DefUse.csv 2> output/Comparison/CodeVsFeatureModel/DefUseStats.txt

$farceCommand gsd.farce.comparisons.CodeToVMComparer $system output/FeatureEffect/nestedIfDefConstraints.txt $prefix > output/Comparison/CodeVsFeatureModel/NestedIfDefs.csv 2> output/Comparison/CodeVsFeatureModel/NestedIfDefStats.txt

$farceCommand gsd.farce.comparisons.CodeToVMComparer $system output/TypeErrors/typeErrorConstraints.txt $prefix > output/Comparison/CodeVsFeatureModel/TypeErrors.csv 2> output/Comparison/CodeVsFeatureModel/TypeErrorsStats.txt

$farceCommand gsd.farce.comparisons.CodeToVMComparer $system output/ParserErrors/parserErrorConstraints.txt $prefix > output/Comparison/CodeVsFeatureModel/ParserErrors.csv 2> output/Comparison/CodeVsFeatureModel/ParserErrorsStats.txt

$farceCommand gsd.farce.comparisons.CodeToVMComparer $system output/PreprocessorErrors/allErrors.txt $prefix  > output/Comparison/CodeVsFeatureModel/Errors.csv 2> output/Comparison/CodeVsFeatureModel/ErrorsStats.txt

$farceCommand gsd.farce.comparisons.CodeToVMComparer $system output/FilePcs/filePcConstraints.txt $prefix  > output/Comparison/CodeVsFeatureModel/filePcs.csv 2> output/Comparison/CodeVsFeatureModel/filePcStats.txt
