#!/bin/sh

force_commit () {
    printf "There are uncommitted changes in this branch. Are you sure you want to proceed? (y/n) "
    read choice
    if [ "$choice" = "y" ]
    then
        echo "OK, proceeding..."
    else
        echo "Not continuing..."
        exit 1
    fi
}

force_build_error () {
    printf "There are build errors. Are you sure you want to proceed? (y/n) "
    read choice
    if [ "$choice" = "y" ]
    then
        echo "OK, proceeding..."
    else
        echo "Not continuing..."
        exit 1
    fi
}

git diff-index --quiet HEAD -- || force_commit;

echo "Switching to develop branch (usually should be no-op)..."
git stash
git checkout develop

echo "Building site..."
stack exec site clean
stack exec site build || force_build_error;

echo "Pulling from master..."
git fetch --all
git checkout -b master --track origin/master || exit

echo "Copying site files..."
cp -a _site/. .

echo "Committing..."
git add -A
echo "Commit number: $(date +%s)"
git commit -m "Publish $(date +%s)"

echo "Pushing to origin..."
git push origin master:master

echo "OK, returning to develop"
git checkout develop
git branch -D master
git stash pop
