FROM rocker/tidyverse:3.6.3

RUN mkdir --parents /home/rstudio/.R/snippets
COPY inst/setup_IQDesktop/snippets/r.snippets \
  /home/rstudio/.R/snippets/r.snippets

RUN Rscript -e  "install.packages(c('data.table', 'R.utils', 'cOde', 'dMod', 'officer', 'beepr', 'reshape2', 'devtools', 'git2r'))"

# Proteomics analysis
# RUN Rscript -r "install.packages(c('GGally', 'RColorBrewer')"
# RUN Rscript -r "BiocManager::install('limma', update = FALSE)"
# RUN Rscript -r "BiocManager::install('MBQN' , update = FALSE)"

RUN Rscript -e "devtools::install_github('dlill/conveniencefunctions')"

RUN mkdir /home/rstudio/Promotion