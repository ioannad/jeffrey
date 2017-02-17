#!/bin/bash

for i in math-snippets/*.tex
do 
    pdflatex -output-directory math-snippets "$i"
    convert -density 150 "${i/.tex/}".pdf "${i/.tex/}".png
done

rm math-snippets/*.log
rm math-snippets/*.aux
rm math-snippets/*.pdf
