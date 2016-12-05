#!/bin/bash

for i in diagrams/fancy-labels/*.tex
do 
    pdflatex -output-directory diagrams/fancy-labels "$i"
    convert -density 300 -trim "${i/.tex/}".pdf -quality 100 "${i/.tex/}".png
done

rm diagrams/fancy-labels/*.log
rm diagrams/fancy-labels/*.aux
