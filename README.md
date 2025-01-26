# Pastures research

A repository for my research on pastures in Brazil. I organized it as follows:

## Code 

This folder houses the main code for the analysis, in Rmarkdown and Jupyter Notebooks. They have been compartimentalized thematically, with each file serving as a standalone piece of code, that imports all necessary files and functions when needed. Replicators should, ideally, run them in the numbered order. This is not essential, since nearly all the dataframes created throughout this research have been stored in the ```Variables``` folder, but it assures they are updated properly. It also certifies the creation of important intermediary files that were too heavy for uploading to Github. *Attention: running the entire repository can (and likely will) take many hours.* Replicators interested in running only the regressions can head towards the .Rmd file named accordingly, which can run independently. The files in this folder are:

- 1_geo_data
- 2_price_data
- 3_mapbiomas_data
- 4_transportation_costs
- 5_trade_data
- 6_survey_data
- 7_intruments
- 8_agroclimatic_data
- 9_carbon_biomass
- 10_weather_data
- regressions_with_controls

## Functions

This folder houses functions created for specific tasks throughout the project. They are saved in .R files, which are called upon in the heading of each .Rmd file where they are used in the analysis. These functions: automatically save and read multiple serialized objects; clean detailed year-product-exporter-importer trade data; calculate multiple summarizing statistics with rasterized GIS data; download and process carbon biomass satellite data in parallel; calculate the structural parameters of the model; and calculate the counterfactual results for pasture recovery and carbon taxes.   

## Inputs

This folder houses the inputs that weren't imported programatically (from online sources and APIs), and were instead taken from files of varying formats. They are: BACI trade metadata, carbon biomass metadata, mapbiomas' pasture quality data, FAO's pasture suitability data, slaughterhouse locations, CEPEA prices and data on transportation costs.

## Outputs

This folder houses graphics and LaTex tables, created as outputs of the analysis.

## Playground

This folder houses code I developped for tests and experiments, as well as legacy versions of code I updated, revised or totaly rewrote, but kept as a reference if needed. 

## Variables

This folder houses intermediary files, mostly dataframes saved as serialized data (.Rds) or .csv, used for connecting the different .Rmd coding files, such that clean data can be easily imported whenever needed.
