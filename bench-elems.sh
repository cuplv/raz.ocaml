# output file
FILE=bench-elems.csv

# test runner
TESTER=./eval.native

# single header
$TESTER > $FILE

#tests in their own process
for M in `seq 1 15`; do
  for I in `seq 1 100`; do
  let INS=I*10000
    for T in z Z f; do
      $TESTER -$T --seed $M --tag trial$M -i $INS -g 2 --nohead >> $FILE
    done
  done
done