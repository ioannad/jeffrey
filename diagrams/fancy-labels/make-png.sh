#!/bin/bash



for i in *.tex
do 
  pdflatex "$i"
  convert -density 300 -trim "${i/.tex/}".pdf -quality 100 "${i/.tex/}".png
done

rm *.log
rm *.pdf

