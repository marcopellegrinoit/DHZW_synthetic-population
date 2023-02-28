![GitHub](https://img.shields.io/badge/license-GPL--3.0-blue)

# Generating a Spatially Explicit Synthetic Population of Individuals and Households

#### *Utrecht University, The Netherlands. 2022 - 2023*

#### *Author: Marco Pellegrino*

#### *Contribution: Jan de Mooij, Tabea Sonnenschein, Mehdi Dastani, Dick Ettema, Brian Logan, Judith A. Verstegen*

## Description

Repository of scripts to generate the synthetic population for the district of Den Haag Zuid-West for the year 2019.

The code makes use of the R library [GenSynthPop](https://github.com/TabeaSonnenschein/Spatial-Agent-based-Modeling-of-Urban-Health-Interventions/tree/main/GenSynthPop) by Tabea Sonnenschein.

## Usage

-   Scripts in [/src/format-datasets](/src/format-datasets) are used to convert the input CBS demographic information into more convenient data frames.
-   [***generate_synthetic_population.R***](generate-synthetic-population.R)**:** it generates a synthetic population of individuals with demographic attributes. In detail:
    -   Gender: male, female
    -   Age: integer value
    -   Migration background: Dutch, Western and non-Western
    -   Current education level: low/middle/high
    -   Education attainment: low/middle/high Note: Dutch education level based on [CBS classification](https://www.cbs.nl/nl-nl/nieuws/2019/33/verschil-levensverwachting-hoog-en-laagopgeleid-groeit/opleidingsniveau)
    -   Individual living with at least one parent: true/false
    -   Car license ownership: true/false
-   [***generate_households.R***](generate-households.R)**:** it groups agents of a synthetic population into households. Also, it generates the following household-level attributes:
    -   Standardized 10% income group
    -   Car ownership

## Project structure

-   ***Root folder***: script to generate a synthetic population and group the latter into households.
-   [***/config***](/config): configuration files. [***config.R***](config/config.R) specifies the municipality, year and filtering of the DHZW area.
-   [***/src***](/src): helper functions.
    -   [***/format-datasets***](/src/format-datasets) contains scripts to convert the raw dataset into the processed dataset ready-to-use.
-   **/data**:
    -   **/raw**: CBS data sets to be processed by the scripts in [/src](/src)
    -   **/processed**: data sets ready-to-use. Outputs of the scripts in [/src/format-datasets](/src/format-datasets)

### Other resources

-   Section [**data**](README_data.md) explains the used data sets in detail.
-   Section [**plots**](README_plots.md) contains evaluation plots and maps.
