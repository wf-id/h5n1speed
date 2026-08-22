build:
    Rscript -e 'devtools::clean_dll()'; Rscript -e "devtools::build()"

doc:
    Rscript -e "devtools::document()"

install:
    Rscript -e "odin::odin_package(here::here()); devtools::clean_dll(); devtools::install()"
check:
    Rscript -e "devtools::check()"

simulate:
    R CMD BATCH --vanilla "manuscript/intervention-effectiveness.R"
    R CMD BATCH --vanilla "manuscript/time-to-effective-strategy.R"

brief:
    quarto render manuscript/delay_strategy.qmd --to pdf

conceptual-tikz:
    cd dev && pdflatex -interaction=nonstopmode conceptual-sensitivity-analysis-2.tex
    rm -f dev/conceptual-sensitivity-analysis-2.aux dev/conceptual-sensitivity-analysis-2.log

readme:
    Rscript -e "rmarkdown::render('README.Rmd')"
    rm -f README.html

topng:
    magick -density 150 \
    manuscript/figures/relative-risk-asymptomatic.pdf \
    -quality 100 \
    -flatten \
    -sharpen 0x1.0 \
    manuscript/figures/relative-risk-asymptomatic.png