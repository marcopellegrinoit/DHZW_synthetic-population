![GitHub](https://img.shields.io/badge/license-GPL--3.0-blue)

# Generating a Spatially Explicit Synthetic Population of Individuals and Households

![Utrecht University, The Netherlands](https://www.uu.nl/sites/default/files/styles/image_290_193/public/campus%20madurodam.jpg)

## Table of Contents

1.  [Description](#description)
2.  [Usage](#usage)
3.  [Project Structure](#project-structure)
4.  [Data](README_data.md)
5.  [Evaluation Plots](README_plots.md)
6.  [Contributors](#contributors)
7.  [License](#license)

## Description

Repository for the generation of a synthetic population for the district of Den Haag Zuid-West in the year 2019. This project was undertaken at Utrecht University, The Netherlands, during 2022-2023 by Marco Pellegrino and a team of contributors.

The code in this repository utilizes the R library [GenSynthPop](https://github.com/TabeaSonnenschein/Spatial-Agent-based-Modeling-of-Urban-Health-Interventions/tree/main/GenSynthPop) created by Tabea Sonnenschein.

The work has been presented at the [C-MAS workshop conference in London, 2023](https://figshare.com/articles/conference_contribution/Generating_a_Spatially_Explicit_Synthetic_Population_from_Aggregated_Data/22821914).

## Usage

The repository provides the following functionalities:

### 1\. Data Preparation

Scripts in [`/src/format-datasets`](/src/format-datasets) are used to convert the input CBS demographic information into more convenient data frames, which are stored in the `/data/processed` folder.

### 2\. Generating a Synthetic Population

The script [`generate_synthetic_population.R`](generate-synthetic-population.R) generates a synthetic population of individuals with demographic attributes, including:

*   Gender: male, female
*   Age: integer value
*   Migration background: Dutch, Western, and non-Western
*   Current education level: nothing/low/middle/high
*   Education attainment: nothing/low/middle/high _(Note: Dutch education level based on_ [_CBS classification_](https://www.cbs.nl/nl-nl/nieuws/2019/33/verschil-levensverwachting-hoog-en-laagopgeleid-groeit/opleidingsniveau)_)_
*   Children individuals living with at least one parent: true/false
*   Car license ownership: true/false

### 3\. Generating Households

The script [`generate_households.R`](generate-households.R) groups agents of the synthetic population into households and generates the following household-level attributes:

*   Standardized 10% income group
*   Car ownership

## Project Structure

*   _**Root folder**_: Contains the main scripts to generate the synthetic population and group them into households.
*   [`/src`](/src): Contains helper functions and utility scripts.
    *   [`/format-datasets`](/src/format-datasets): Contains scripts to convert the raw datasets into processed datasets ready for use.
*   `/data`:
    *   `/raw`: CBS datasets to be processed by the scripts in [`/src`](/src)
    *   `/processed`: Data sets ready-to-use, outputs of the scripts in [`/src/format-datasets`](/src/format-datasets)
*   `/evaluation`: Contains scripts to aggregate data for plots and tools to plot different distributions and comparisons.

## Contributors

This project was made possible thanks to the hard work and contributions from the following individuals:

*   Marco Pellegrino (Author)
*   Jan de Mooij
*   Tabea Sonnenschein
*   Mehdi Dastani
*   Dick Ettema
*   Brian Logan
*   Judith A. Verstegen

## License

This repository is licensed under the GNU General Public License v3.0 (GPL-3.0). For more details, see the [LICENSE](LICENSE) file.

For more information about the data sets used in this project, refer to the [Data](README_data.md) section. For evaluation plots and maps, please check the [Evaluation Plots](README_plots.md) section.