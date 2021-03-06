#window manager settings
export X_WINDOW_MANAGER=/usr/bin/startkde
export HOME=/msu/home/m1gsa00

export http_proxy="http://bender.rsma.frb.gov:3128"

#printer settings
#http://www.rsma.frb.gov/Guide/Print/Printers.html
#export my_printer=prn-m2232
#fsprpr1 for 1454 printer
#export my_printer=colorprk
#export my_printer=k16211-ofs-01
#export my_printer=k16520-ofs-01c
#export my_printer=prn-b5132-color-bw
#export my_printer=prn-b5130A-color-bw
#export my_printer=colorpr7
#export my_printer=mqpr1
#lpoptions -d prn-m2124 > /dev/null
#lpoptions -p prn-m2124 -o InputSlot=Tray3 > /dpev/null


export my_printer=b5132-ma-01c
export LPDEST=$my_printer
export PRINTER=$my_printer
export EDITOR=emacs
export GIT_EDITOR=emacs

alias lprmsu="lpr -P b5132-ma-01c"
alias myenscript="enscript -r2 -P b5132-ma-01c"

lpoptions -o InputSlot=Auto
lpoptions -o MediaType=Auto





#some useful dirs
export RES2=/msu/res2/m1gsa00
export PROJ3=/msu/res2/m1gsa00/proj3
export PROJ5=/msu/res2/m1gsa00/proj5
export MYREPOS=/msu/res1/Software/mavenRepositories
export DHOME=$PROJ3/dataHome
export THOME=$DHOME/texFiles


#setup prompt  
#(http://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html)
#export PS1="{\h,\w,\!}$"
#\
#export PS1="xxx"
#setup place for bins ( will ln -s to this dir
export RES2=/msu/res2/m1gsa00

export PS1="{\h,\w}$"

#add utility dir to path
export PATH=$HOME/bin:$PATH

#nuweb
export PATH=/msu/res1/Software/nuweb/nuweb-1.58:$PATH

#directory with swish indexes and examples
export SWISHDIR=$PROJ3/garyFiles/big/swish
alias toSwish="cd "$SWISHDIR"; cat example"



#tex dirs
#export TEXINPUTS=$THOME/texStyles/:$TEXINPUTS
#export TEXINPUTS=$TEXINPUTS:$THOME/texBibs
#export TEXINPUTS=$TEXINPUTS:$THOME/texStyles/
#export TEXINPUTS=$TEXINPUTS:$THOME/texBibs
#export BIBINPUTS=.
#export BIBINPUTS=::../../bibFiles::/msu/home/m1gsa00/git/paperProduction/bibFiles
export TEXINPUTS=
export TEXINPUTS=../../bibFiles:../../texFiles:$TEXINPUTS::/opt/texlive/2014/texmf-dist/tex/latex/natbib/:~/git/paperProduction/texFiles
export BIBINPUTS=../../bibFiles:../../texFiles:$BIBINPUTS:/msu/home/m1gsa00/git/paperProduction/bibFiles/::/opt/texlive/2014/texmf-dist/tex/latex/natbib/:~/git/paperProduction/bibFiles
export CLASSPATH=""

#mathematica
export MATHDIR=/opt/mathematica9
#export PATH=$MATHDIR:$PATH
#export MATHDIR=/opt/mathematica8
export PATH=$MATHDIR:$PATH
export JLinkLibLoc=$MATHDIR/SystemFiles/Links/JLink/SystemFiles/Libraries/Linux
#export CLASSPATH=$CLASSPATH:$MATHDIR/SystemFiles/Links/JLink/JLink.jar
export TEXINPUTS=$TEXINPUTS:$MATHDIR/SystemFiles/IncludeFiles/TeX
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MATHDIR/SystemFiles/Links/JLink/SystemFiles/Libraries/Linux-x86-64
alias math8="/opt/mathematica8/bin/math"
alias math9="/opt/mathematica9/math"
alias math10="/opt/mathematica10/math"
alias peakmath="/opt/mathematica/10.3/Executables/math"
alias peakmathematica="/opt/mathematica/10.3/Executables/mathematica"
alias mathematica8="/opt/mathematica8/bin/mathematica"
alias mathematica9="/opt/mathematica9/mathematica"
alias mathematica10="/opt/mathematica10/mathematica"
PATH=/opt/mathematica/10.3/Executables/:$PATH


#cuda
#PATH=$PATH:/gpu/cuda/bin
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/mathematica9/SystemFiles/Links/CUDALink/LibraryResources/Linux-x86-64/
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/gpu/cuda/lib
#export CUDA_LIBRARY_PATH=/gpu/cuda/lib/libcudart.so


#for fame timeiq
export timeiqdir=/opt/fame/timeiq
#export timeiqdir=/opt/fame/timeiq
export licensedir=/opt/fame/timeiq



#export CLASSPATH=$CLASSPATH:$licensedir:$timeiqdir/lib/timeiq.jar:$timeiqdir/lib/TimeIQLicense.jar:.
#for charts
#export CLASSPATH=$CLASSPATH:$timeiqdir/tools/lib/TimeIQCharts.jar:$timeiqdir/tools/lib/JimiProClasses.zip


#export CLASSPATH=$timeiqdir/lib/timeiq.jar:$timeiqdir/lib/TimeIQLicense.jar:.
#matlab
export mlDir=/opt/MATLAB/R2013a
export PATH=$mlDir/bin:$PATH


#for java
export PATH=/msu/res5/software/jdk1.8.0_121/bin:$PATH



#for maven
#export M2_REPO=$MYREPOS/tryRep
#for maven 3.0.3
#export M2_HOME=/msu/res1/Software/maven3.0.5/apache-maven-3.0.5
#export M2=$M2_HOME/bin
#export PATH=$M2_HOME/bin:$PATH
#export M2_HOME=/msu/res1/Software/apache-maven-3.3.9
#export M2=$M2_HOME/bin
#export PATH=$M2_HOME/bin:$PATH
#export JAVA_HOME=/msu/res1/Software/java/jdk1.7.0_21/
#for affineArithmetic
#export PLATFORM=intel-Linux




#for rootbeer
#export CLASSPATH=$CLASSPATH:/msu/res1/Software/functional/Rootbeer-1.0.48-alpha.jar



#for eclipse
#export PATH=/msu/scratch2/m1gsa00/eclipse:$PATH
export PATH=$HOME/RES2/eclipse/eclipse:$PATH


#for antlr 4
#export CLASSPATH=".:/msu/res1/Software/antlr/antlr-4.0-complete.jar:$CLASSPATH"
#alias antlr4='java -jar /msu/res1/Software/antlr/antlr-4.0-complete.jar'
#alias grun='java org.antlr.v4.runtime.misc.TestRig'

#for antlr 3
#export CLASSPATH=".:/msu/res1/Software/antlr/antlr-3.5-complete.jar:$CLASSPATH"
alias antlr3='java -jar /msu/res1/Software/antlr/antlr-3.5-complete.jar'
alias grun='java org.antlr.v3.runtime.misc.TestRig'



#export LD_LIBRARY_PATH=/opt/fame/timeiq/timeiqcharts/tools:
export LD_LIBRARY_PATH=/opt/fame/timeiq/lib/linux_x86/64
#extra fame
#provide paths for timeiq and timeiqCharts
export timeiqdir=/opt/fame/timeiq
export timeiqChartsDir=/opt/fame/timeiq/timeiqcharts/tools
#export CLASSPATH=$timeiqChartsDir/lib/TimeIQCharts.jar:$timeiqChartsDir/lib/JimiProClasses.zip:$timeiqdir/lib/timeiq.jar:$timeiqdir/lib/TimeIQLicense.jar:.:$timeiqChartsDir:$timeiqdir/lib/commons-codec-1.3.jar



#for ctocpp
export PATH=/msu/res1/Software/garyUsr/local/bin/:$PATH

#for javacc
export PATH=/msu/res1/Software/javaCC/javacc-5.0/bin:$PATH
#for matio
export LD_LIBRARY_PATH=/opt/icc/lib/intel64/:/msu/res1/Software/matio-1.5.1/./src/.libs/:$LD_LIBRARY_PATH




#xalan

#export CLASSPATH=/msu/res1/Software/xalan-j_2_7_1/xalan.jar:$CLASSPATH


#export CLASSPATH=/msu/res1/Software/XSLTXQUERY/SaxonHE9-7-0-2J/saxon9he.jar:$CLASSPATH

#xquery
#export CLASSPATH=/msu/res1/Software/XSLTXQUERY/SaxonHE9-7-0-2J/saxon9-xqj.jar:$CLASSPATH
#export CLASSPATH=/msu/res1/Software/XSLTXQUERY/xqj/xqjapi.jar:$CLASSPATH


# INTEL FORTRAN

#source /opt/intel/bin/ifortvars.sh intel64 
#source /opt/intel/bin/iccvars.sh intel64

export PATH=/opt/intel/composer_xe_2013.2.146/bin/intel64/:$PATH



#python 
export PATH=/msu/res1/Software/anaconda/envs/py33/bin:$PATH








#define git merge/conflict experiment dirs
export mergeConflictDir=/msu/res2/Shared_Projects/MPSCode/mergeConflictResolution
export amyDir=mergeConflictDir/amy/amySharedProject
export benDir=mergeConflictDir/ben/benSharedProject
export carlDir=mergeConflictDir/carl/carlSharedProject

#define git flow experiment dirs
export devFlowDir=/msu/res2/Shared_Projects/MPSCode/gitFlowExamples
export amyDir=mergeConflictDir/amy/amySharedDevProject
export benDir=mergeConflictDir/ben/benSharedDevProject
export carlDir=mergeConflictDir/carl/carlSharedDevProject


#add git flow to path
export PATH=/add/lib/gitflow:$PATH
#add use newer git to path
export PATH=/opt/local/bin:$PATH
#for java keep it earliest on path
#export PATH=/msu/res1/Software/java/jdk1.7.0_21/bin/:$PATH

#python 
export PATH=/msu/res1/Software/anaconda/envs/py33/bin:$PATH


export LD_LIBRARY_PATH=



#netbeans
export PATH=/msu/res5/software/netbeans-8.2/bin:$PATH




#export LD_LIBRARY_PATH=/opt/atlas/lib:/msu/res2/m1gsa00/forGitSparseAMA/sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared/:$LD_LIBRARY_PATH


#export LD_LIBRARY_PATH=/opt/fame/timeiq/lib/linux_x86/64:/msu/home/m1gsa00/git/sparseAMAParser/sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared



#export LD_LIBRARY_PATH=/msu/home/m1gsa00/git/sparseAMAParser/sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared/:$LD_LIBRARY_PATH

export LD_LIBRARY_PATH=/msu/home/m1gsa00/git/sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared/:$LD_LIBRARY_PATH

