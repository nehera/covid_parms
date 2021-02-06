# Epidemiological parameters of COVID-19 in the United States at the state-level
Aidan T. Neher, Joaquin Escobar-Dodero, Amy C. Kinsley


University of Minnesota, Department of Veterinary Population Medicine, [Kinsley Lab](http://kinsleylab.umn.edu/)


Note: This repository is in-progress, so the research has not been peer-reviewed.


## Introduction
This repository houses the code and data used to estimate parameter values of COVID-19 in the United States at the state-level via a Susceptible-Exposed-Infected-Recovered (SEIR) compartmental model and an approximate bayesian computation (ABC) algorithm.


## Codebook
### SEIR Compartments
* S = Susceptible
* E = Exposed
* Ia = Infected Asymptomatic
* Is = Infected Symptomatic
* R = Recovered
* D = Dead

### ABC Parameters
* beta = transmission coefficient
* durE = duration of exposure
* durIa = duration of asymptomatic infection
* durD = duration to death
* durR = duration to recovery
* p = proportion of Is that dies
* q = proportion of Is that recovers
* x = proportion of Ia that becomes Is
* y = proportion of Ia that recovers

## Data Sourcing
[State population data was sourced from the United States Census Bureau](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_500989927) and [state COVID-19 case data was sourced from The COVID Tracking Project](https://covidtracking.com/data).  

## Acknowledgements
Funding was provided by the University of Minnesota - Medical School's [CO:VID (Collaborative Outcomes: Visionary Innovation &amp; Discovery) grants program](https://clinicalaffairs.umn.edu/umn-covid-19-research/parameter-values-covid-19-united-states.). 