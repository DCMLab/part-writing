#!/bin/sh

. reproduce/settings.sh

times=10
chainSize=1 # faster for many runs

for i in $(seq 1 $times);
do
    read -r seed
    echo "Run $i"
    stack exec vl-train -- $iterations -s 1 -z $neighborDistance -c $chainSize -r $resetRate\
          -l "$learningRate" -f "$fastWeightsRate" -p "$power" -R "$seed"\
          -o "model_compare_$i.json" -L "train_compare_$i.log"\
          $rtsopts
done < reproduce/seeds.txt
