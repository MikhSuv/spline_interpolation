set terminal pdfcairo enhanced color font "Helvetica,12" size 6in,4in
set output 'spline.pdf'
set xlabel 'x'
set ylabel 'y'
set grid
set key top right

plot 'original.dat' using 1:2 with points pt 7 lc rgb 'red'   title 'Исходные точки', \
     'spline.dat'   using 1:2 with lines lw 2 lc rgb 'blue'  title 'Сплайн (интерполяция)'
