# output file
FILE=autoeval.csv

# test runner
TESTER=./eval.native

$TESTER > $FILE
for I in `seq 1 100`; do
  let INS=I*1000
  for M in `seq 1 5`; do
    for T in z f; do
      $TESTER -$T --seed $M --tag trial$M -i $INS -g 1 --nohead >> $FILE
    done
  done
done