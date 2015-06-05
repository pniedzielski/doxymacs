#!/bin/sh

# File names
AUTHORS="AUTHORS.md"

# Make sure we have git installed
GIT_EXECUTABLE=`which git`
if [ "$?" -ne "0" ]; then
    echo "Sorry, cannot find git in PATH." >&2
    exit 1
fi

# Make sure we are in a git repository
$GIT_EXECUTABLE status > /dev/null 2>&1
if [ "$?" -ne "0" ]; then
    echo "Sorry, not in a git repository." >&2
    exit 2
fi

# Generate AUTHORS file
authors_text=`$GIT_EXECUTABLE log --format="%aN &lt;%aE&gt;" | sort | uniq | sed "s/^/  * /g"`
echo "Authors\n=======\n\n$authors_text" >$AUTHORS
