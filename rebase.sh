git stash
git fetch origin
git checkout -B master origin/master
git branch | grep / | sort | while read b; do echo Rebasing $b; git rebase master $b; done
git branch -D merged.old
git branch -M merged merged.old
git checkout -B merged master
git branch | grep / | sort | while read b; do echo Merging $b; git merge $b; done
git push --mirror mirror
git stash pop
