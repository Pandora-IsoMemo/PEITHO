FROM ghcr.io/pandora-isomemo/base-image:latest
ADD . .
RUN installPackage
CMD ["Rscript", "-e", "library(PEITHO);startApplication(3838)"]
