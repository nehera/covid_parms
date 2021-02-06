# This script defines the differential equations for the SEIR model.

# Compartments
# S = Susceptible
# E = Exposed
# Ia = Infected Asymptomatic
# Is = Infected Symptomatic
# R = Recovered
# D = Dead

# Parameters
# beta = transmission coefficient
# durE = duration of exposure
# durIa = duration of asymptomatic infection
# durD = duration to death
# durR = duration to recovery
# p = proportion of Is that dies
# q = proportion of Is that recovers
# x = proportion of Ia that becomes Is
# y = proportion of Ia that recovers

seir <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*(Ia+Is)
    dE <- beta*S*(Ia+Is)-durE*E
    dIa <- durE*E-durIa*Ia
    dIs <- durIa*Ia*x-durD*Is*p-durR*Is*q
    dD <- durD*Is*p
    dR <- durIa*Ia*y+durR*Is*q
    return(list(c(dS,dE,dIa,dIs,dD,dR)))
  })
}