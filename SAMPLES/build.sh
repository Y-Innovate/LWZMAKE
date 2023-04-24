#!/bin/sh
#-----------------------------------------------------------------#
# Edit BUILD.jcl to replace @@HOMEDIR@@ with your home directory, #
# @@GITDIR@@ with this repository's root directory, and submit    #
# the edited JCL.                                                 #
#-----------------------------------------------------------------#

# Get this shell script's directory
scrdir=$(dirname $0)

# Change to the directory where this script resides (which is the repo root)
cd $scrdir

# Put the full path of this directory in escSrcdir, but first replace every forward slash
# with two backslashes and a forward slash (so /u/userdir becomes \\/u\\/userdir)
# The two backslashes are needed so $escSrcdir can be used between "" causing the double
# backslashes to be resolved to a single one making it the correct syntax for sed.
escSrcdir=`pwd | sed 's/\//\\\\\//g'`
srcdir=`pwd`

# Change to home dir to then get its full path
cd ~

# Put the full path of the home directory in escHomedir, but first replace every forward slash
# with two backslashes and a forward slash (so /u/userdir becomes \\/u\\/userdir)
# The two backslashes are needed so $escHomedir can be used between "" causing the double
# backslashes to be resolved to a single one making it the correct syntax for sed.
escHomedir=`pwd | sed 's/\//\\\\\//g'`

# Now back to the source directory
cd $srcdir

# Pipe BUILD.jcl through sed with the replace string and pipe the result into submit
cat JCL/JOBSTMT.jcl JCL/BUILD.jcl | sed -e "s/@@GITDIR@@/$escSrcdir/g" -e "s/@@HOMEDIR@@/$escHomedir/g" | submit

# And we're done
exit 0
