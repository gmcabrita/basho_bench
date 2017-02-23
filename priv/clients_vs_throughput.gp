set terminal pdfcairo font 'Verdana,10' linewidth 1 rounded fontscale 0.8

set style line 11 lc rgb '#808080' lt 1
set tics nomirror

set style line 12 lc rgb '#808080' lt 0 lw 1

set style line 1 dashtype 1 lc rgb '#E31A1C' pt 6 ps 1 lt 1 lw 1
set style line 2 dashtype 2 lc rgb '#387DB8' pt 4 ps 1 lt 1 lw 1

set output outputname
set datafile separator ','

set xlabel "Number of clients per data center"
set ylabel "Mean Throughput (ops/sec)"

set key off

set datafile missing '0'

plot inputname using 1:2 with linespoints ls 1