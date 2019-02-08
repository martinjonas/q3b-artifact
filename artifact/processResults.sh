#!/bin/bash

cd results
table-generator --no-diff -d -o . -n data -f csv ../experiments/results/boolector.*.results.boolector.xml.bz2 ../experiments/results/cvc4.*.results.cvc4.xml.bz2 ../experiments/results/q3b.*.results.q3b.xml.bz2  ../experiments/results/z3.*.results.z3.xml.bz2
bash PROCESSRESULTS.sh
Rscript PROCESSRESULTS.R
pdflatex results.tex
cd ..
