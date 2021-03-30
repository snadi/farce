package gsd.farce.utilities

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 18/09/13
 * Time: 12:48 PM
 * To change this template use File | Settings | File Templates.
 */

object BB_Git extends Config {
  def getFeatureModelFile: String = getSourceDir + "featureModel"

  def getFileListFile: String = getSourceDir + "filelist"

  def getSourceDir: String = "S:\\ARCHIVE\\kos\\share\\TypeChef\\busybox\\gitbusybox\\"

  def getInterfaceExtension = ".interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = "CONFIG_"

  def getSuffix = ""

  def getHierarchyResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_hierarchy_stats.csv"
  def getCrosstreeResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_crosstree_stats.csv"
  def getHierarchyFullResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_hierarchy_full_stats.csv"
  def getCrosstreeFullResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_crosstree_full_stats.csv"
  def getAccuracyResults =  "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/CodeVsFeatureModel/busybox_accuracyStats.csv"
}

object BB_GitServer extends Config {
  def getFeatureModelFile: String = getSourceDir + "featureModel"

  def getFileListFile: String = getSourceDir + "filelist"

  def getSourceDir: String = "gitbusybox/"

  def getInterfaceExtension = ".interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = "CONFIG_"

  def getSuffix = ""
  def getHierarchyResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_hierarchy_stats.csv"
  def getCrosstreeResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_crosstree_stats.csv"
  def getHierarchyFullResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_hierarchy_full_stats.csv"
  def getCrosstreeFullResults = "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/FeatureModelVsCode/busybox_crosstree_full_stats.csv"
  def getAccuracyResults =  "../CaseStudies/farce-BusyBoxAnalysis/output/Comparison/CodeVsFeatureModel/busybox_accuracyStats.csv"
}

object ecos extends Config {
  def getFeatureModelFile: String = "output/FeatureModel/pc_vmWare.constraints"

  def getFileListFile: String = "filelist"

  def getSourceDir: String = "ecos/"

  def getInterfaceExtension = ".interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = ""

  def getSuffix = ""

  def getHierarchyResults = "../CaseStudies/farce-eCosAnalysis/output/Comparison/FeatureModelVsCode/ecos_hierarchy_stats.csv"
  def getCrosstreeResults = "../CaseStudies/farce-eCosAnalysis/output/Comparison/FeatureModelVsCode/ecos_crosstree_stats.csv"
  def getHierarchyFullResults = "../CaseStudies/farce-eCosAnalysis/output/Comparison/FeatureModelVsCode/ecos_hierarchy_full_stats.csv"
  def getCrosstreeFullResults = "../CaseStudies/farce-eCosAnalysis/output/Comparison/FeatureModelVsCode/ecos_crosstree_full_stats.csv"
  def getAccuracyResults =  "../CaseStudies/farce-eCosAnalysis/output/Comparison/CodeVsFeatureModel/ecos_accuracyStats.csv"
}

object axTLS_1_4_8 extends Config {
  def getFeatureModelFile: String = getSourceDir + "featureModel"

  def getFileListFile: String = "filelist"

  def getSourceDir: String = "axTLS-1.4.8/"

  def getInterfaceExtension = ".interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = "CONFIG_"

  def getSuffix = ""
  def getHierarchyResults = ""
  def getCrosstreeResults = ""
  def getHierarchyFullResults = ""
  def getCrosstreeFullResults = ""
  def getAccuracyResults = ""


}

object uclibc extends Config {
  def getFeatureModelFile: String = "output/FeatureModel/uClibc-0.9.33.2.dimacs"

  def getFileListFile: String = "filelist"

  def getSourceDir: String = "uClibc/"

  def getInterfaceExtension = ".interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = "__"

  def getSuffix = "__"

  def getHierarchyResults = "../CaseStudies/farce-uClibc/output/Comparison/FeatureModelVsCode/uclibc_hierarchy_stats.csv"
  def getCrosstreeResults = "../CaseStudies/farce-uClibc/output/Comparison/FeatureModelVsCode/uclibc_crosstree_stats.csv"
  def getHierarchyFullResults = "../CaseStudies/farce-uClibc/output/Comparison/FeatureModelVsCode/uclibc_hierarchy_full_stats.csv"
  def getCrosstreeFullResults = "../CaseStudies/farce-uClibc/output/Comparison/FeatureModelVsCode/uclibc_crosstree_full_stats.csv"
  def getAccuracyResults =  "../CaseStudies/farce-uClibc/output/Comparison/CodeVsFeatureModel/uclibc_accuracyStats.csv"
}

trait Config {
  def getFeatureModelFile: String

  def getFileListFile: String

  def getSourceDir: String

  def getInterfaceExtension: String

  def getXMLExtenstion: String

  def getPrefix: String

  def getSuffix: String

  def getHierarchyResults: String
  def getCrosstreeResults: String
  def getHierarchyFullResults: String
  def getCrosstreeFullResults: String
  def getAccuracyResults: String
}

object Linux_2_6_33_3 extends Config {
  def getFeatureModelFile: String = "linux-2.6.33.3.dimacs"

  def getFileListFile: String = "linux_files.lst"

  def getSourceDir: String = "linux-2.6.33.3/"

  def getInterfaceExtension = ".interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = "CONFIG_"

  def getSuffix = ""


  def getHierarchyResults = "../CaseStudies/farce-linuxAnalysis/output/Comparison/FeatureModelVsCode/linux_hierarchy_stats.csv"
  def getCrosstreeResults = "../CaseStudies/farce-linuxAnalysis/output/Comparison/FeatureModelVsCode/linux_crosstree_stats.csv"
  def getHierarchyFullResults = "../CaseStudies/farce-linuxAnalysis/output/Comparison/FeatureModelVsCode/linux_hierarchy_full_stats.csv"
  def getCrosstreeFullResults = "../CaseStudies/farce-linuxAnalysis/output/Comparison/FeatureModelVsCode/linux_crosstree_full_stats.csv"
  def getAccuracyResults =  "../CaseStudies/farce-linuxAnalysis/output/Comparison/CodeVsFeatureModel/linux_accuracyStats.csv"
}

object BB_1_18_5 extends Config {
  def getFeatureModelFile: String = "S:\\ARCHIVE\\kos\\share\\TypeChef\\busybox\\busybox\\featureModel"

  def getFileListFile: String = "S:\\ARCHIVE\\kos\\share\\TypeChef\\busybox\\busybox\\busybox_files"

  def getSourceDir: String = "S:\\ARCHIVE\\kos\\share\\TypeChef\\busybox\\busybox-1.18.5\\"

  def getInterfaceExtension = ".c.interface"

  def getXMLExtenstion = ".c.xml"

  def getPrefix = "CONFIG_"

  def getSuffix = ""
  def getHierarchyResults = ""
  def getCrosstreeResults = ""
  def getHierarchyFullResults = ""
  def getCrosstreeFullResults = ""
  def getAccuracyResults = ""
}
