#! /bin/sh
chmod +x commit_hook.sh
cp commit_hook.sh .git/hooks/pre-commit
stack exec hlint -- -V || stack install hlint
stack exec weeder -- -V || stack install weeder
