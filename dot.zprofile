# -*- mode: shell-script -*-
# @file ~/.zprofile

## nvidia cuda/cudnn toolkit
export CUDA_PATH=/usr/local/cuda
if [ -e $CUDA_PATH ]; then
    export PATH=$CUDA_PATH/bin:$PATH
    export CFLAGS=-I$CUDA_PATH/include
    export LDFLAGS=-L$CUDA_PATH/lib64
    export LD_LIBRARY_PATH=$CUDA_PATH/lib64:$LD_LIBRARY_PATH
fi

## anyenv
if [ -e $HOME/.anyenv/bin ]; then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
fi

## emacs cask
#export PATH=$HOME/.cask/bin:$PATH
