# gomx_sediment_macrofauna
This repository is to clean and standardize data on macrofaunal composition and behavior in the Gulf of Mexico, as well as the grain size of the sediment they live in. The data has been standardized to Darwin Core standards and published to OBIS and GBIF.

 - Original Data: https://www.sciencebase.gov/catalog/item/5a709594e4b0a9a2e9d88e4e

 - Published to OBIS: https://obis.org/dataset/8eafe8eb-6215-4de8-b38d-aef644768e6f

 - Published to GBIF: https://www.gbif.org/dataset/f41e070d-14ed-4386-b3f1-9b89b584eb92/metrics

To see a rendered version of the current working notebook go to: [https://kylieh10.github.io/gomx_sediment_macrofauna/index](https://kylieh10.github.io/gomx_sediment_macrofauna/quarto_pages/index)

# Navigating the repository and files

`scripts`: Updated scripts that create the ready-to-publish data tables, aligned with Darwin Core standards, for OBIS and GBIF.

`renv` and `renv.lock`: Files that contain the packages and versions used in this project. This increases portability. More documentation on this package and how it works can be found here: https://rstudio.github.io/renv/articles/renv.html. 

`quarto_pages`: The individual quarto pages and style guides that compose the notebook documenting the standardization workflow.
