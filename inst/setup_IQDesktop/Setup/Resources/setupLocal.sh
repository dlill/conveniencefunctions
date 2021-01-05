# ssh key
cd /home/daniel2
sudo chown -R daniel2 .ssh/

# git 
git config --global user.name Daniel Lill
git config --global user.email daniel.lill@intiquan.com

# clone PROJTOOLS
cd /home/daniel2
mkdir PROJTOOLS
cd PROJTOOLS
git clone git@github.com:dlill/conveniencefunctions # Clone single repository in beginning to set up key
git clone git@github.com:IntiQuan/IQRmate &
git clone git@github.com:IntiQuan/IQRexamples &
git clone git@github.com:IntiQuan/MMVIsoboles &
git clone git@github.com:IntiQuan/iqrmalaria IQRmalariaGIT &

# Install PROJTOOLS (except IQRtools)
cd /home/daniel2/PROJTOOLS && Rscript -e "devtools::install_deps('./conveniencefunctions/')" && R CMD INSTALL --no-multiarch --with-keep.source conveniencefunctions
cd /home/daniel2/PROJTOOLS && Rscript -e "devtools::install_deps('./IQRmate/')" && R CMD INSTALL --no-multiarch --with-keep.source IQRmate
cd /home/daniel2/PROJTOOLS/IQRmalaria && Rscript -e "devtools::install_deps('./IQRmalaria/')" && R CMD INSTALL --no-multiarch --with-keep.source IQRmalaria
cd /home/daniel2/PROJTOOLS && Rscript -e "devtools::install_deps('./IQRexamples/')" && R CMD INSTALL --no-multiarch --with-keep.source IQRexamples
cd /home/daniel2/PROJTOOLS && Rscript -e "devtools::install_deps('./MMVIsoboles/')" && R CMD INSTALL --no-multiarch --with-keep.source MMVIsoboles

# Setup RStudio
Rscript -e 'conveniencefunctions::cf_install_rstudio()'
