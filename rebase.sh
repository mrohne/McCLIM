git stash
git fetch origin
git checkout -B master origin/master
git branch | grep / | sort | while read b; do echo Rebasing $b; git rebase master $b || break; done
git stash pop
