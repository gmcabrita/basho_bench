set terminal pdfcairo font 'Verdana,10' linewidth 1 rounded fontscale 0.8

set style line 11 lc rgb '#808080' lt 1
set tics nomirror

set style line 12 lc rgb '#808080' lt 0 lw 1

set style line 1 dashtype 1 lc rgb '#E31A1C' pt 6 ps 1 lt 1 lw 1
set style line 2 dashtype 2 lc rgb '#387DB8' pt 4 ps 1 lt 1 lw 1
set style line 3 dashtype 5 lc rgb '#A870B0' pt 10 ps 1 lt 1 lw 1
set style line 4 dashtype 4 lc rgb '#4DB04A' pt 8 ps 1 lt 1 lw 1

set output outputname
set datafile separator ','

set xlabel "Number of clients per data center"
set ylabel "Mean Throughput (ops/sec)"

set key horizontal outside box 3

set datafile missing '0'

plot inputone using 1:2 title titleone with linespoints ls 1, \
     inputtwo using 1:2 title titletwo with linespoints ls 3, \
     inputthree using 1:2 title titlethree with linespoints ls 4, \
     inputfour using 1:2 title titlefour with linespoints ls 2