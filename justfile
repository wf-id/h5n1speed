build:
    Rscript -e "devtools::build()"

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

movepkg:
    scp ../rRsurveillance_0.0.2.tar.gz pegasus:/deac/bio/kortessisGrp/dewime23/rRsurveillance_0.0.2.tar.gz 

fetch:
    scp pegasus:/deac/bio/kortessisGrp/dewime23/rRSurveillance/manuscript/*.rds dev/