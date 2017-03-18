# -*- mode: shell-script -*-
# @file ~/.zprofile

## nvidia cuda/cudnn
export CUDA_PATH=/usr/local/cuda
export PATH=$CUDA_PATH/bin:$PATH
export CFLAGS=-I$CUDA_PATH/include
export LD_LIBRARY_PATH=$CUDA_PATH/lib64:$CUDA_PATH/lib:$LD_LIBRARY_PATH

## anyenv
if [ -e $HOME/.anyenv/bin ]; then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
fi

## emacs cask
#export PATH=$HOME/.cask/bin:$PATH