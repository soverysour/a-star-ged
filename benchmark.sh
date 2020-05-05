#!/bin/bash

timeoutLimit='60s'
runTimes=5

beforeFile='./input-graph-before.json'
afterFile='./input-graph-after.json'

executable='./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/ged-seq-exe/ged-seq-exe'
pythonScript='./gen-graph.py'

function performTest()
{
  nodeCount="$1"
  labelCount="$2"
  edgePercent="$3"
  labelPercent="$4"

  # Generate the input files.
  python3 "$pythonScript" --node "$nodeCount" --label "$labelCount" --edge-p "$edgePercent" --label-p "$labelPercent" > "$beforeFile"
  python3 "$pythonScript" --node "$nodeCount" --label "$labelCount" --edge-p "$edgePercent" --label-p "$labelPercent" > "$afterFile"

  printf "\n\nNOW TESTING FOR nodeCount=$nodeCount, labelCount=$labelCount, edgePercent=$edgePercent, labelPercent=$labelPercent\n\n"

  for i in `seq 1 "$runTimes"`; do
    printf "\n=== BEGIN RUN $i === \n"

    # Seq.
    result=`time timeout "$timeoutLimit" "$executable"`
    printf "\nExit status: $?"

    # Par.
    result=`time timeout "$timeoutLimit" "$executable" --run-par-with-k 1 +RTS -N1`
    printf "\nExit status: $?"

    result=`time timeout "$timeoutLimit" "$executable" --run-par-with-k 2 +RTS -N2`
    printf "\nExit status: $?"

    result=`time timeout "$timeoutLimit" "$executable" --run-par-with-k 4 +RTS -N4`
    printf "\nExit status: $?"

    result=`time timeout "$timeoutLimit" "$executable" --run-par-with-k 8 +RTS -N8`
    printf "\nExit status: $?"

    printf "\n\n=== END RUN $i === \n\n"
  done
}

for nodeCount in `seq 5 15`; do
  for labelCount in 50 100 300 500; do
    for edgePercent in 0.2 0.5 0.8 1.0; do
      for labelPercent in 0.2 0.5 0.8 1.0; do
        performTest "$nodeCount" "$labelCount" "$edgePercent" "$labelPercent"
      done
    done
  done
done
