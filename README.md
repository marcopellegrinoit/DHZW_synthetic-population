# DHZW synthetic population script

#### *Utrecht University, The Netherlands. 2022 - 2023*
#### *Author: Marco Pellegrino*
#### *Contribution: Jan de Mooij, Tabea Sonnenschein, Mehdi Dastani, Dick Ettema, Brian Logan, Judith A. Verstegen*

## Description

Repository of scripts to generate the synthetic population for the district of Den Haag Zuid-West for the year 2019.

The code makes use of the R library [GenSynthPop](https://github.com/TabeaSonnenschein/Spatial-Agent-based-Modeling-of-Urban-Health-Interventions/tree/main/GenSynthPop) by Tabea Sonnenschein.

## Usage

*   Scripts in [/format-CBS-datasets](/format-CBS-datasets) are used to convert the input CBS demographic information into more convenient data frames.
*   [_**generate\_synthetic\_population.R**_](generate-synthetic-population.R)**:** it generates a synthetic population of individuals with demographic attributes. In detail:
    *   Gender: male, female
    *   Age: integer value
    *   Migration background: Dutch, Western and non-Western
    *   Current education level: low/middle/high
    *   Education attainment: low/middle/high Note: Dutch education level based on [CBS classification](https://www.cbs.nl/nl-nl/nieuws/2019/33/verschil-levensverwachting-hoog-en-laagopgeleid-groeit/opleidingsniveau)
    *   Individual living with at least one parent: true/false
    *   Car license ownership: true/false
*   [_**generate\_households.R**_](generate-households.R)**:** it groups agents of a synthetic population into households. Also, it generates the following household-level attributes:
    *   Standardized 10% income group
    *   Car ownership

## Project structure

*   _**Root folder**_: script to generate a synthetic population and group the latter into households.
*   [_**/config**_](/config): configuration files. [_**config.R**_](config/config.R) specifies the municipality, year and filtering of the DHZW area.
*   [_**/src**_](/src): helper functions.
    *   [_**/format-CBS-datasets**_](src/format-CBS-datasets) contains scripts to convert the raw dataset into the processed dataset ready-to-use.
*   [_**/data**_](/data):
    *   [_**/raw**_](/data/raw)_**:** CBS data sets to be processed by the scripts in_ [_/src_](/src)
    *   [_**/processed**_](/data/processed)_**:** data sets ready-to-use. Outputs of the scripts in_ [_/format-CBS-datasets_](/src/format-CBS-datasets)
    *   Note: datasets are divided by municipality and year. The configuration file ([/config/config.R](config/config.R)) allows all the scripts to automatically refer to the correct data folders.

### Other resources

*   Section [**data**](README_data.md) explains the used data sets in detail.  
    Section [**plots**](README_plots.md) contains evaluation plots and maps.