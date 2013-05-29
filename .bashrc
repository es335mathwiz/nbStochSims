#window manager settings
export X_WINDOW_MANAGER=/usr/bin/startkde
export HOME=/msu/home/m1gsa00

#printer settings
export my_printer=msupr1
export LPDEST=$my_printer
export PRINTER=$my_printer
export EDITOR=emacs
export GIT_EDITOR=emacs

#some useful dirs
export RES2=/msu/res2/m1gsa00
export PROJ3=/msu/res2/m1gsa00/proj3
export PROJ5=/msu/res2/m1gsa00/proj5
export MYSCRTCH=/msu/scratch/m1gsa00
export DHOME=$PROJ3/dataHome
export THOME=$DHOME/texFiles


#setup prompt  
#(http://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html)
export PS1="{\h,\w,\!}$"


#setup place for bins ( will ln -s to this dir
export RES2=/msu/res2/m1gsa00


#add utility dir to path
PATH=$HOME/bin:$PATH


#directory with swish indexes and examples
export SWISHDIR=$PROJ3/garyFiles/big/swish
alias toSwish="cd "$SWISHDIR"; cat example"




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
PATH=/add/lib/gitflow:$PATH


#tex dirs
#export TEXINPUTS=.
export TEXINPUTS=$THOME/texStyles/:$TEXINPUTS
export TEXINPUTS=$TEXINPUTS:$THOME/texBibs
export BIBINPUTS=.
export BIBINPUTS=::$HOME/RES2/pdfEtc
export CLASSPATH=""

#mathematica
#export MATHDIR=/opt/mathematica9
#export PATH=$MATHDIR:$PATH
export MATHDIR=/opt/mathematica8
export PATH=$MATHDIR/bin:$PATH
export JLinkLibLoc=$MATHDIR/SystemFiles/Links/JLink/SystemFiles/Libraries/Linux
export CLASSPATH=$CLASSPATH:$MATHDIR/SystemFiles/Links/JLink/JLink.jar
export TEXINPUTS=$TEXINPUTS:$MATHDIR/SystemFiles/IncludeFiles/TeX
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MATHDIR/SystemFiles/Links/JLink/SystemFiles/Libraries/Linux-x86-64



#cuda
#PATH=$PATH:/gpu/cuda/bin
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/mathematica9/SystemFiles/Links/CUDALink/LibraryResources/Linux-x86-64/
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/gpu/cuda/lib
#export CUDA_LIBRARY_PATH=/gpu/cuda/lib/libcudart.so


#for fame timeiq
export timeiqdir=/opt/fame/timeiq
#export timeiqdir=/opt/fame/timeiq
export licensedir=/opt/fame/timeiq



export CLASSPATH=$CLASSPATH:$licensedir:$timeiqdir/lib/timeiq.jar:$timeiqdir/lib/TimeIQLicense.jar:.
#for charts
export CLASSPATH=$CLASSPATH:$timeiqdir/tools/lib/TimeIQCharts.jar:$timeiqdir/tools/lib/JimiProClasses.zip


export CLASSPATH=$timeiqdir/lib/timeiq.jar:$timeiqdir/lib/TimeIQLicense.jar:.
#matlab
export mlDir=/opt/MATLAB/R2013a
export PATH=$mlDir/bin:$PATH


#for java
export PATH=/msu/res1/Software/java/jdk1.7.0_21/bin/:$PATH



#for maven
export M2_REPO=$MYSCRTCH/tryRep
#for maven 3.0.3
export M2_HOME=/msu/res1/Software/maven3.0.5/apache-maven-3.0.5
export M2=$M2_HOME/bin
export PATH=$M2_HOME/bin:$PATH
export JAVA_HOME=/msu/res1/Software/java/jdk1.7.0_21/
#for affineArithmetic
export PLATFORM=intel-Linux




#for rootbeer
export CLASSPATH=$CLASSPATH:/msu/res1/Software/functional/Rootbeer-1.0.48-alpha.jar



#for eclipse
#export PATH=/msu/scratch2/m1gsa00/eclipse:$PATH
export PATH=$HOME/RES2/eclipse/eclipse:$PATH


#for antlr
export CLASSPATH=".:/msu/res1/Software/antlr/antlr-4.0-complete.jar:$CLASSPATH"
alias antlr4='java -jar /msu/res1/Software/antlr/antlr-4.0-complete.jar'
alias grun='java org.antlr.v4.runtime.misc.TestRig'


export LD_LIBRARY_PATH=/opt/fame/timeiq/lib/linux_x86/64

#extra fame
#provide paths for timeiq and timeiqCharts
export timeiqdir=/opt/fame/timeiq
export timeiqChartsDir=/opt/fame/timeiq/timeiqcharts/tools
export CLASSPATH=$timeiqChartsDir/lib/TimeIQCharts.jar:$timeiqChartsDir/lib/JimiProClasses.zip:$timeiqdir/lib/timeiq.jar:$timeiqdir/lib/TimeIQLicense.jar:.:$timeiqChartsDir:$timeiqdir/lib/commons-codec-1.3.jar

