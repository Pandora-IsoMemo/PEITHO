FROM inwt/r-shiny:4.3.2

RUN echo "options(repos = c(getOption('repos'), CRAN = 'https://cloud.r-project.org', PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

# Debugging: Check the current repositories in R
RUN Rscript -e "cat('Current repos:'); print(getOption('repos'))"

ADD . .

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    libglpk40 \
    libuv1-dev \
 && rm -rf /var/lib/apt/lists/*

# Remove corrupted pkgbuild package entirely - it will be reinstalled as a dependency
RUN rm -rf /usr/local/lib/R/site-library/pkgbuild

RUN installPackage

# Expose ports
EXPOSE 3838

CMD ["Rscript", "-e", "library(shiny); PEITHO::startApplication(3838)"]
