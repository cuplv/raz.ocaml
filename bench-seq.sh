# output file
FILE=bench-seq.csv

# test runner
TESTER=./eval.native

# single header
$TESTER > $FILE

#tests in their own process
for M in `seq 1 15`; do
  for T in z Z f; do
  	$TESTER -$T --seed $M --tag trial$M -i 1000000 -g 100 --nohead >> $FILE
  done
done