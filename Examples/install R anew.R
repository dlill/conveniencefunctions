# sudo apt-get remove r-base-core
# add debian packages
# sudo apt-get install r-base

install.packages("devtools")
devtools::install_github("dlill/conveniencefunctions")
conveniencefunctions::update_version()
