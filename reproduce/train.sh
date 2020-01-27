#!/bin/sh

stack exec vl-train -- 5000 -s 1 -z 0.1 -c 1 -r 0\
      -l "Lin 0.3 0" -f "Lin 0.1 0.3" -p "Cnst 1"\
      -o "model.json" -L "train_reproduce.log"
