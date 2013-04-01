#!/bin/bash

# A simple script to automate the merge process. The revision number of the
# last merge is stored in last_merge_revision.txt; this script merges all
# trunk changes between the last merge and the current revision into this
# branch and then updates the revision number stored in the text file.

OLDVER=`cat last_merge_revision.txt`
svn merge -r$OLDVER:head https://parallelio.googlecode.com/svn/trunk

NEWVER=`svn info | grep Revision | cut -d " " -f 2`
echo $NEWVER > last_merge_revision.txt
