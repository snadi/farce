package gsd.farce.utilities

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 18/09/13
 * Time: 10:43 AM
 * To change this template use File | Settings | File Templates.
 */
object PropertyKeys {
  //Feature model
  def FEATURE_MODEL_IMPL_GRAPH = "featureModelImplGraph"
  def FEATURE_MODEL_MUTEX_GRAPH = "featureModelMutexGraph"
  def FEATURE_MODEL_FILE = "featureModelFile"


  //Type system model
  def TYPE_MODEL_IMPL_GRAPH = "typeModelImplGraph"
  def TYPE_MODEL_MUTEX_GRAPH = "typeModelMutexGraph"
  def TYPE_FORMULA_FILE = "typeFormulaFile"
  def TYPE_MODEL_DIMACS="typeModelDimacs"
  def TYPE_MODEL_INITIAL_GRAPH="typeModelInitialImplGraph"
  def TYPE_CONSTRAINTS_FILE ="typeConstraintsFile"

  //parser errors model

  def PARSER_MODEL_MUTEX_GRAPH = "parserModelMutexGraph"
  def PARSER_MODEL_IMPL_GRAPH = "parserModelImplGraph"
  def PARSER_FORMULA_FILE = "parserErrorsFormulaFile"
  def PARSER_CONSTRAINTS_FILE ="parserConstraintsFile"

  //linker model
  def DEFUSE_MODEL_IMPL_GRAPH = "defUseModelImplGraph"
  def DEFUSE_MODEL_MUTEX_GRAPH = "defUseModelMutexGraph"
  def DEFUSE_FORMULA_FILE="defUseFormulaFile"
  def DEFUSE_DIMACS_FILE="defUseDimacsFile"
  def DEFUSE_CONSTRAINTS_FILE="defUseConstraintsFile"

  //nested ifdef model
  def NESTED_IFDEF_IMPL_GRAPH = "nestedIfDefImplGraph"
  def NESTED_IFDEF_MUTEX_GRAPH = "nestedIfDefMutexGraph"
  def NESTED_IFDEF_FORMULA_FILE = "nestedIfDefFormulaFile"
  def NESTED_IFDEF_IMPLICATIONS = "nestedIfDefImplications"
  def NESTED_IFDEF_INITIAL_GRAPH = "nestedIfDefInitialGraph"
  def NESTED_IFDEF_INITIAL_MUTEX_GRAPH = "nestedIfDefInitialMutexGraph"
  def NESTED_IFDEF_DIMACS = "nestedIfDefDimacs"
  def NESTED_IFDEF_KEYMAP="nestedIfDefKeyMap"
  def NESTED_IFDEF_CONSTRAINTS_FILE="nestedIfDefConstraintsFile"
  def FEATURE_EFFECT_COMPARE_LIST = "featureEffectCompareList"
  def FEATURE_EFFECT_FEATURE_BLACKLIST = "fBlacklist"


  //preprocessor errors
  def PREPROCESSOR_ERROR_MODEL_IMPL_GRAPH = "errorsModelImplGraph"
  def PREPROCESSOR_ERROR_MODEL_MUTEX_GRAPH = "errorsModelMutexGraph"
  def PREPROCESSOR_ERROR_FORMULA_FILE = "errorsFormulaFile"
  def PREPROCESSOR_ERROR_EXTRACTED_CONSTRAINTS = "errorsDirectives"
  def PREPROCESSOR_ERROR_CONSRAINT_FILE="preprocessorErrorConstraintsFile"

  //FILE PC (Feature effect build)
  def FILE_PC_FORMULA_FILE="filePcFormulaFile"
  def FILE_PC_CONSTRAINTS_FILE="filePcConstraintsFile"


  //big conjunction
  def BIG_CONJUNCTION_FORMULA_FILE = "bigConjunctionFormulaFile"
  def BIG_CONJUNCTION_DIMACS = "bigConjunctionDimacs"
  def BIG_CONJUNCTION_INITIAL_GRAPH = "bigConjInitialGraph"
  def BIG_CONJUNCTION_INITIAL_MUTEX_GRAPH = "bigConjInitialMutexGraph"
  def BIG_CONJUNCTION_IMPL_GRAPH = "bigConjImplGraph"
  def BIG_CONJUNCTION_MUTEX_GRAPH = "bigConjMutexGraph"
  def SPEC1_DIMACS_FILE = "spec1Dimacs"
  def SPEC2_DIMACS_FILE = "spec2Dimacs"

  //costraints
  def HIERARCHY_CONSTRAINTS_FILE="hierarchyConstraintsFile"
  def CROSSTREE_CONSTRAINTS_FILE="crossTreeConstraintsFile"
  def XOR_GROUP_FILE="xorGroupConstraintsFile"
  def OR_GROUP_FILE="orGroupConstraintsFile"
  def MUTEX_GROUP_FILE="mutexGroupConstraintsFile"
  def SIMPLE_FEATURE_MODEL="featureModelSimple"
  def CNF_CLAUSES_FILE="cnfClausesFile"

  //list of features that do not appear in code or build
  //called noDefine based on CDL naming
  def NO_DEFINE_FILE="noDefineFil"
}
