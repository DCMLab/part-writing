#!/bin/sh

stack exec vl-train -- 2000 -s 1 -z 0.1 -c 1 -r 0\
      -l "Lin 0.2 0.1" -f "Lin 0.07 0.2" -p "Cnst 1"\
      -o "model.json" -L "train_reproduce.log" -q
