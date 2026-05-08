# plot.gnu – проверка гладкости сплайна на краях
set terminal png size 800,600 enhanced font 'Arial,14'
set output 'spline_smoothness.png'
set xlabel 'x'
set ylabel 'y'
set title 'Естественный кубический сплайн: y = sin(x)'
set grid
set key top right

plot 'original.dat' using 1:2 with points pt 7 lc rgb 'red'   title 'Исходные точки', \
     'spline.dat'   using 1:2 with lines lw 2 lc rgb 'blue'  title 'Сплайн (интерполяция)', \
     'exact.dat'    using 1:2 with lines lw 1 lc rgb 'black' dashtype 2 title 'y = sin(x)'
