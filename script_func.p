set terminal pdf            #Hace que salga por png
set output 'F(x).pdf'    #Establece el noombre del archivo de salida

set autoscale               # escala los ejes automaticamente
unset log                   # quita la escala logaritmica (si hubiera)
unset label                 # quita los titulos anteriores

set xtic auto               # establece las divisiones del eje x automaticamente
set ytic auto               # establece las divisiones del eje y automaticamente
#set xrange[0:5]             # establece el rango a mostrar del eje x
#set yrange[0:5]             # establece el rango a mostrar del eje y
set grid

set title 'Función F(x)'
set xlabel "x"
set ylabel "y"

plot "Funcion.txt" using 1:2 title 'F(x)' with lines lw 2 lc 1,\
     "Raices.txt" using 1:2 title 'Raíces' with points pt 3
     
#using 1:2 son las columnas de las que se van a tomar valores para x e y creo.
#lw es line width o ancho de linea
#lc es line color o color de linea
#title es el nombre que va a tener la funcion en la leyenda
#con ",\" se separan las distintas lineas del plot (las distintas funciones a plotear)
