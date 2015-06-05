#!/bin/sh

# File names
AUTHORS="AUTHORS.md"
NEWS="NEWS.md"

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

# Generate NEWS file
tags=`$GIT_EXECUTABLE tag -l --sort="-v:refname" | grep "^v[0-9]"`
news_text="Release Notes\n============="
for tag in $tags; do
    date=`$GIT_EXECUTABLE show --quiet --format="%ci" $tag^{commit} | cut -d' ' -f1`
    annotation=`$GIT_EXECUTABLE show --quiet --format="" $tag | tail -n +4`
    news_text="$news_text\n\n## $date: $annotation\n"
done
echo "$news_text" | head -n -1 >$NEWS
