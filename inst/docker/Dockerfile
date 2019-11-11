FROM rocker/tidyverse:3.6.1
RUN R -e "devtools::install_github('dkaschek/cODE'); devtools::install_github('dkaschek/dMod');"
RUN R -e "devtools::install_github('dlill/conveniencefunctions');"

CMD R


