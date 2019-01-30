#!/bin/bash

cd experiments/
rm -rf results
benchexec boolector -N 3
benchexec cvc4 -N 3
benchexec q3b -N 2
benchexec z3 -N 3
