#!/bin/sh

. reproduce/settings.sh

stack exec vl-train -- $iterations -s 1 -z $neighborDistance -c $chainSize -r $resetRate\
      -l "$learningRate" -f "$fastWeightsRate" -p "$power" -R default\
      -o "model.json" -L "train_reproduce.log"
