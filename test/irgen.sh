#!/bin/bash

for case in "irgen/"*.crust; do 
    ../src/./crusty.native < $case > $case.out 2> $case.stderr
    lli $case.out > $case.out.expected 2> $case.stderr /dev/null/

    rm $case.stderr
    diff=$(diff $case.out.expected $case.expected)
    if [ -z "$diff" ]; then 
        echo "Correct Output for Test Case $case" 
    else 
        echo "Incorrect Output for Test Case $case" 
        exit 1

    fi
    rm $case.out
    rm $case.out.expected
done