git stash
git branch -D merged.old
git branch -M merged merged.old
git checkout -B merged master
git branch | grep / | sort | while read b; do echo Merging $b; git merge $b || break; done
git stash pop
