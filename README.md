# Parameter values of COVID-19 in the United States
Aidan T. Neher, Joaquin Escobar-Dodero, Amy C. Kinsley


University of Minnesota, Department of Veterinary Population Medicine


Please note: This repository documents research-in-progress. As such, it has not been peer-reviewed like a traditional journal article.  


## Introduction
This repository houses the code and data used to estimate parameter values of COVID-19 in the United States. Funding was provided by the University of Minnesota - Medical School, and details regarding the grant can be found at the following link: https://clinicalaffairs.umn.edu/umn-covid-19-research/parameter-values-covid-19-united-states. 


We use an approximate bayesian computation algorithm and a compartmental model to estimate the parameter values of COVID-19 in the United States. 


## Codebook
Parameter definitions:
* durExp=duration of exposure period
* durIa=duration of asymptomic infection
* durIs=duration of symptomatic infection
* mort=mortality rate
* durR=recovery rate
* duration of immunity (not included in this model, yet)
* x=proportion of infected that remain asymptomatic
* y=proportion of infected that are symptomatic
* p=proportion that die
* q=proportion that recover
* testp=proportion with covid that receive test (assumed to be 1, for now)

## Data Sourcing
[State population data was sourced from the United States Census Bureau](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_500989927) and [state COVID-19 case data was sourced from The COVID Tracking Project](https://covidtracking.com/data).  
