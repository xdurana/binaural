#!/bin/sh

#pdflatex plos_template
#bibtex plos_template
#pdflatex plos_template
#pdflatex plos_template

latex plos_template
bibtex plos_template
latex plos_template
latex plos_template
pdflatex plos_template