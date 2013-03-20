#window manager settings
export X_WINDOW_MANAGER=/usr/bin/startkde
export HOME=/msu/home/m1gsa00

#printer settings
export my_printer=msupr1
export LPDEST=$my_printer
export PRINTER=$my_printer
export EDITOR=emacs

#some useful dirs
export RES2=/msu/res2/m1gsa00
export PROJ3=/msu/res2/m1gsa00/proj3
export PROJ5=/msu/res2/m1gsa00/proj5
export MYSCRTCH=/msu/scratch/m1gsa00


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
export mergeConflictDir /msu/res2/Shared_Projects/MPSCode/mergeConflictResolution
export amyDir  $mergeConflictDir/amy/amySharedProject
export benDir  $mergeConflictDir/ben/benSharedProject
export carlDir  $mergeConflictDir/carl/carlSharedProject

#define git flow experiment dirs
export devFlowDir /msu/res2/Shared_Projects/MPSCode/gitFlowExamples
export amyDir  $mergeConflictDir/amy/amySharedDevProject
export benDir  $mergeConflictDir/ben/benSharedDevProject
export carlDir  $mergeConflictDir/carl/carlSharedDevProject
