#!/bin/sh
set -e
/bin/echo -ne '\xff\xff\xff\xff' > separator.dat
# for i in examples/*.s
for i in loopback fib-loop fib-recur floatfib sendsample \
  mandelbrot mandelbrot-large
do
  j=`basename $i .s`
  echo "Assembling $j..."
  ./qkasm examples/$j.s > examples/$j
  echo "Combining $j and its input..."
  cat examples/$j separator.dat examples/$j.in > examples/$j.bin
done
