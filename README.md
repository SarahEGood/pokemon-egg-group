# Pokemon Breeding Calculator
A basic tool for determining possible paths for chain breeding.

## Features
* Can calculate the optimal path between egg groups by Pokemon
* Can take into account chain breeding hidden abilities

## Installation
A live version of this app is available [here](https://apps.sarahegood.com/pokemon-egg-group).

### Run app offline

This app is compatible with R versions >= 4.3.0. Functionality may be limited if 

To run offline, can directly install this package via R using the following commands.
```R
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("SarahEGood/pokemon-egg-group", dependencies = TRUE)
```
To run this app direct from the console, navigate to the root directory of your repo and run the following command:
```R
R -e "shiny::runApp('app.R')"
```

## Data Credit
Data primarily pulled from the [National Dex Spreedsheet](https://www.reddit.com/r/pokemon/comments/z0mjwt/national_dex_spreadsheet_updated_for_gen_9/) by u/Arce_HJavrek on Reddit. Additional data and information about game mechanics taken from [Bulbapedia](https://bulbapedia.bulbagarden.net/wiki/Main_Page).