#!/bin/bash

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

if [[ $TRAVIS_OS_NAME == 'osx' ]]; then
    travis_retry curl -sSL https://get.haskellstack.org/ | sh
else
    travis_retry curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi
