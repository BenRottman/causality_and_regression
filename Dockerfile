FROM rocker/shiny
MAINTAINER Ben Rottman (benjamin.rottman@gmail.com)

# install R package dependencies
RUN apt-get update && apt-get install -y \
    ##### ADD YOUR DEPENDENCIES ??not sure need to add all dependencies for all cran packages?
    ## clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages from CRAN
RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    ##### ADD YOUR CRAN PACKAGES
    shiny \
    MASS \
    ggplot2 \
    ggExtra \
    stringr \
    mediation \
    ##### && Rscript -e "devtools::install_github( ## ADD YOUR GITHUB PACKAGES )" \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## copy your shiny app folder below
COPY ./shiny/ /srv/shiny-server/myapp/
