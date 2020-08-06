#!/bin/bash

mkdir -p fancy-labels

for i in fancy-labels/*.tex
do 
    pdflatex -output-directory fancy-labels "$i"
    convert -density 300 -trim "${i/.tex/}".pdf -quality 100 "${i/.tex/}".png
done

rm fancy-labels/*.log
rm fancy-labels/*.aux
