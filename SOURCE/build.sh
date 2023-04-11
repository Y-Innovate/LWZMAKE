#!/bin/sh
#-------------------------------------------------------------#
# Edit BUILD.jcl to replace @@GITDIR@@ with this repository's #
# root directory and submit the edited JCL.                   #
#-------------------------------------------------------------#

# Get this shell script's directory
scrdir=$(dirname $0)

# Change to the directory where this script resides (which is the repo root)
cd $scrdir

# Put the full path of this directory in pwdesc, but first replace every forward slash
# with two backslashes and a forward slash (so /u/userdir becomes \\/u\\/userdir)
# The two backslashes are needed so $pwdesc can be used between "" causing the double
# backslashes to be resolved to a single one making it the correct syntax for sed.
pwdesc=`pwd | sed 's/\//\\\\\//g'`

# Pipe BUILD.jcl through sed with the replace string and pipe the result into submit
cat JCL/BUILD.jcl | sed "s/@@GITDIR@@/$pwdesc/g" | submit

# And we're done
exit 0
