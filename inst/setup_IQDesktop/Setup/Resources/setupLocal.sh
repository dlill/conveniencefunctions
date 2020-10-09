# ssh key
cd 
mkdir ~/.ssh 
unzip /IQDESKTOP/PROJTOOLS/IQDesktop/id_rsa.zip -d ~/.ssh 
chmod 600 ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa.pub

# git 
git config --global user.name Daniel Lill
git config --global user.email daniel.lill@intiquan.com

# clone PROJECTS

# Not done on local container



# clone PROJTOOLS
cd
mkdir PROJTOOLS
cd PROJTOOLS
git clone git@github.com:dlill/conveniencefunctions # Clone single repository in beginning to set up key
git clone git@github.com:IntiQuan/IQRmate &
git clone git@github.com:IntiQuan/IQRexamples &
git clone git@github.com:IntiQuan/MMVIsoboles &
git clone git@github.com:IntiQuan/iqrmalaria IQRmalariaGIT &
git clone git@github.com:IntiQuan/IQRtools # clone last => works as time control for the following part of the script

# Install PROJTOOLS (except IQRtools)
cd && cd PROJTOOLS && Rscript -e "devtools::install_deps('./conveniencefunctions/')" && R CMD INSTALL --no-multiarch --with-keep.source conveniencefunctions
cd && cd PROJTOOLS && Rscript -e "devtools::install_deps('./IQRmate/')" && R CMD INSTALL --no-multiarch --with-keep.source IQRmate
cd && cd PROJTOOLS/IQRmalariaGIT && Rscript -e "devtools::install_deps('./IQRmalaria/')" && R CMD INSTALL --no-multiarch --with-keep.source IQRmalaria
cd && cd PROJTOOLS && Rscript -e "devtools::install_deps('./IQRexamples/')" && R CMD INSTALL --no-multiarch --with-keep.source IQRexamples
cd && cd PROJTOOLS && Rscript -e "devtools::install_deps('./MMVIsoboles/')" && R CMD INSTALL --no-multiarch --with-keep.source MMVIsoboles

# Setup RStudio
Rscript -e 'conveniencefunctions::cf_install_rstudio()'
