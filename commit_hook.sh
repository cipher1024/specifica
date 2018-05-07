#! /bin/sh
stack exec weeder -- . || exit
stack exec hlint -- . || exit
