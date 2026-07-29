FROM inwt/r-shiny:4.4.3

RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e "remotes::install_github('r-lib/httr2@v1.2.3')" \
    && Rscript -e "remotes::install_github('tidyverse/ellmer@v0.4.1')"

ADD . .

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    libglpk40 \
    libuv1-dev \
 && rm -rf /var/lib/apt/lists/*

# Remove corrupted pkgbuild package entirely
RUN rm -rf /usr/local/lib/R/site-library/pkgbuild

# Reinstall pkgbuild from scratch
RUN Rscript -e "install.packages('pkgbuild', repos='https://cloud.r-project.org', clean=TRUE)"

RUN installPackage

# Expose ports
EXPOSE 3838

CMD ["Rscript", "-e", "library(shiny); PEITHO::startApplication(3838)"]
