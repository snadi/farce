#!/bin/bash -e
#!/bin/bash -vxe

sourceDir= #the directory containing the source files of the system to analyze

filesToProcess() {
  local listFile=filelist #the list of files to analyze
  cat $listFile
  #awk -F: '$1 ~ /.c$/ {print gensub(/\.c$/, "", "", $1)}' < linux_2.6.33.3_pcs.txt
}

# any additional flags that might be needed. These may include features that may need to be defined/undefined for things to work properly"
flags="-U HAVE_LIBDMALLOC -DCONFIG_FIND -U CONFIG_FEATURE_WGET_LONG_OPTIONS -U ENABLE_NC_110_COMPAT -U CONFIG_EXTRA_COMPAT -D_GNU_SOURCE" 

srcPath="$sourceDir"

#these are the typechef flags.. check the TypeChef page for what each flag means. The important thing here is to pass any directories containing necessary header files
export partialPreprocFlags="--bdd -x CONFIG_ --include $sourceDir/header.h --include mheader.h -I $srcPath/include --debugInterface --writePI --recordTiming --parserstatistics --errorXML --interface --openFeat $sourceDir/openFeaturesList.txt"

filesToProcess|while read i; do
  if [ ! -f $srcPath/$i.dbg ]; then
    touch $srcPath/$i.dbg
	#call TypeChef
     ./jcpp.sh $srcPath/$i.c $flags
  else
    echo "Skipping $srcPath/$i.c"
  fi
done

