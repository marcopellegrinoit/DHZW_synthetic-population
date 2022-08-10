# DHZW synthetic population script
#### *Author: Marco Pellegrino, Utrecht University*
## Description
Repository of scripts to geneate the synthetic population for the district of Den Haag Zuid-West.

The code makes use of the library [GenSynthPop](https://github.com/TabeaSonnenschein/Spatial-Agent-based-Modeling-of-Urban-Health-Interventions/tree/main/GenSynthPop) by Tabea Sonnenschein.

## Usage
* Scripts in [/format-CBS-datasets](https://github.com/mr-marco/DHZW_synthetic-population/tree/main/format-CBS-datasets "format-CBS-datasets") are used to convert the input CBS demographic information into more convinient dataframes.
* ***generate_synthetic_population.R*:** it generates a synthetic population of individuals with demographic attributes. In detail:
	* gender: male, female
	* age: integer value
	* migration background: Dutch, Western and non-Western
	* current education level: low/middle/high
	* education attainment: low/middle/high
	Note: Dutch education level based on [CBS classification](https://www.cbs.nl/nl-nl/nieuws/2019/33/verschil-levensverwachting-hoog-en-laagopgeleid-groeit/opleidingsniveau)
* ***generate_households.R*:** it groups agents of a synthetic population into households.

## Data required
All the dataset are already included in the [/data](https://github.com/mr-marco/DHZW_synthetic-population/tree/main/data) folder. Internally, case-studies are divided into cities and years. For example, "den_haag_2019" contains data about the municipality of Den Haag in the year 2019.

### Synthetic population generation:
 - **Marginal distributions.**
	 -  Aggregation level: neighbourhoods
	 - Dataset: [marginal_distributions_84583NED.csv](https://github.com/mr-marco/DHZW_synthetic-population/blob/main/data/den_haag_2019/marginal_distributions_84583NED.csv "marginal_distributions_84583NED.csv")
	 - Source: [CBS](https://opendata.cbs.nl/statline/portal.html?_la=nl&_catalog=CBS&tableId=84583NED&_theme=236)
	 - Used for: gender, age, migration background, education attainment
 - **Gender, integer age**. Stratified.
	 -  Aggregation level: municipality
	 - Dataset: [gender_age-03759NED.csv](https://github.com/mr-marco/DHZW_synthetic-population/blob/main/data/den_haag_2019/stratified-datasets/gender_age-03759NED.csv "gender_age-03759NED.csv")
	 - Source: [CBS](https://opendata.cbs.nl/statline/portal.html?_la=nl&_catalog=CBS&tableId=03759ned&_theme=267)
	 - Used for: gender, age
- **Migration background**, gender and groupages. Stratified.
	-  Aggregation level: municipality
	 - Dataset: [gender_age_migration-84910NED.csv](https://github.com/mr-marco/DHZW_synthetic-population/blob/main/data/den_haag_2019/stratified-datasets/gender_age_migration-84910NED.csv "gender_age_migration-84910NED.csv")
	 - Source: [CBS](https://opendata.cbs.nl/statline/portal.html?_la=nl&_catalog=CBS&tableId=84910NED&_theme=267)
	 - Used for: migration background, current education
- **Current education**, migration background, gender, groupages. Stratified.
	-  Aggregation level: municipality
	 - Dataset: [edu_current-71450NED.csv](https://github.com/mr-marco/DHZW_synthetic-population/blob/main/data/den_haag_2019/stratified-datasets/edu_current-71450NED.csv "edu_current-71450NED.csv")
	 - Source: [CBS](https://opendata.cbs.nl/statline/portal.html?_la=nl&_catalog=CBS&tableId=71450ned&_theme=341)
	 - Used for: current education