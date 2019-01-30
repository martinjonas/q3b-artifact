sed -i 's/\t/,/g' data.table.csv

cat results_header.csv > data.csv
tail -n +4 data.table.csv >> data.csv

sed -i 's#/#,##' data.csv
#separator is #, so we do not have to escape all backslashes
