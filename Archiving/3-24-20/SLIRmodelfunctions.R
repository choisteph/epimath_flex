# Useful functions for running the SLIR model.

library(readr)
library(lhs)
library(deSolve)
library(ggplot2)
library(gridExtra)

# Convert table parameters to model parameters
paramfun = function(P){
  # Input: P should be an array/df/etc where the first column is the names and the second column is the values
  # Output: a vector of parameters for running in the compartmental model
  # Note: maybe change this so that the input is a named list? Rownames are kind of unreliable...
  row.names(P) = P[[1]]
  params = c('b' = 5*P[['R0',2]]/P[['InfPer',2]],   # these will be calculated correctly off of R0 later, also will fit this one. Screw it, b of about 1 seems to work so setting that on the defaults. That gives a doubling time of about 3 days
             'gL' = 1/(P[['IncPer',2]]), #1/(P[['IncPer',2]] - P[['InfAsympPer',2]]),
             'g2' = 1/P[['InfPer',2]],
             'fd' = P[['fMort',2]],
             'fA' = P[['fAsymp',2]],
             'hr' = 1/P[['TimetoHosp',2]] ,
             'gh' = 1/P[['StayLengthHosp',2]],
             'cr'= 1/P[['TimetoSeekCare',2]], 
             'PopSize' = P[['MIPop',2]],
             'k' = P[['MIPop',2]]*P[['fReport',2]],
             'fHosp' = P[['fHosp',2]],
             'fICU' = P[['fICU',2]],
             'fO2' = P[['fO2',2]],
             'fVent' = P[['fVent',2]],
             'L0' = P[['L0',2]],  # these are just arbitrary numbers
             'I10' = P[['I10',2]],
             'I20' = P[['I20',2]])
  # params['b'] = (P[['R0',2]]*(params['hr'] + params['g2']))/(1 - params['fA'])  # this is from the old R0 before I split up the I's
  params['b'] = (params['hr']*P[['R0',2]]*params['g2'])/((-1+params['fA'])*(-params['hr'] + params['fHosp']*params['hr'] - params['fHosp']*params['g2'])) # corrected
  # beta = (hr R \[Gamma]2)/((-1 + fA) (-hr + fHosp hr - fHosp \[Gamma]2))
  return(params)
}


# Equations
CoVode = function(t, x, params){
  S = x[1]    # Susceptible
  L1 = x[2]    # Latent comp 1 
  
  I1 = x[3]   # Mild symptomatic infectious, not care seeking
  I1c = x[4]   # Mild symptomatic infectious, care seeking
  
  I2 = x[5]   # Severe + critical symptomatic infectious, will be hospitalized
  # added this because the fraction hospitalized was way too high if we just let time to hospitalization decide things
  
  R = x[6]    # Recovered
  H = x[7]    # Should be hospitalized
  D = x[8]    # Dead
  CC = x[9]   # Cumulative care seeking
  CS = x[10]   # Cumulative symptomatic
  CH = x[11]  # Cumulative hospitalization
  
  b = params['b']    # Transmission non-superspreader
  gL = params['gL']  # Latent-->asymptomatic non-superspreader
  g2 = params['g2']   # Symptomatic-->recovered non-superspreader
  fd = params['fd']  # Case fatality rate in hospitalized
  fA = params['fA'] # Fraction asymptomatic in non-superspreaders
  fHosp = params['fHosp'] # Fraction who will be hospitalized
  hr = params['hr']   # Hospitalization rate non-superspreader
  gh = params['gh']   # Hospitalized -->recovered
  cr = params['cr']  # Rate of care seeking after symptoms appear
  g2c = g2            #Recovery rate in tested non-superspreader (should this be adjusted to account for time before testing? maybe not because of memorylessness?)
  bc = b              #transmission in tested non-superspreader
  
  lambda = b*(I1+I2) +bc*(I1c)
  
  dSdt = -lambda*S
  dL1dt = lambda*S - gL*L1
  
  dI1dt = (1-fHosp)*(1-fA)*gL*L1 - g2*I1 - cr*I1
  
  dI1cdt = cr*I1  - g2c*I1c
  
  dI2dt = fHosp*(1-fA)*gL*L1 - hr*I2
  
  dRdt = fA*gL*L1 + g2*I1 + g2c*I1c + (1-fd)*gh*H
  
  dHdt = hr*I2 - gh*H
  
  dDdt = fd*gh*H 
  
  dCCdt = cr*I1 + hr*I2
  
  dCSdt = (1-fA)*gL*L1
  
  dCHdt = hr*I2
  
  list(c(dSdt, dL1dt, dI1dt, dI1cdt, dI2dt, dRdt, dHdt, dDdt, dCCdt, dCSdt,dCHdt))
}

# for fitting the ICs - assume no deaths or recovereds at t=0 (which is not true but let's say)
x0 = function(params){
  x0 = numeric(11)
  x0[2] = params['L0']
  x0[3] = (1-params['fHosp'])*(params['I20']+params['I10'])
  x0[5] = params['fHosp']*(params['I20']+params['I10'])
  x0[1] = 1-sum(x0[c(2,3,5)])
  x0[9] = cases[1]/params['k']
  
  names(x0) = c('S', 'L1', 'I1', 'I1c','I2', 'R', 'H','D', 'CC', 'CS','CH')
  x0
}


# Prevalence of infectious individuals
ytrueprevfun = function(odeSim, params){params['PopSize']*rowSums(odeSim[,4:6])} # All Is (note odeSim[,1] is time)
# yobsprevfun = function(odeSim, params){params['k']*rowSums(odeSim[,4:6])} # Is with reporting fraction
# This no longer exactly makes sense, since reporting happens with hospitalization/careseeking

# Prevalence of infected individuals (including latent)
ytruelatprevfun = function(odeSim, params){params['PopSize']*rowSums(odeSim[,3:6])}

# Cumulative incidence of symptomatic cases
ytruecumincfun = function(odeSim, params){odeSim[,11]*params['PopSize']} 
# yobscumincfun = function(odeSim, params){odeSim[,11]*params['k']} # same note as above

# Cumulative incidence of care seeking
ytruecumcarefun = function(odeSim, params){odeSim[,10]*params['PopSize']} 
yobscumcarefun = function(odeSim, params){odeSim[,10]*params['k']} # This is what we will treat as testing

# Cumulative deaths
ytruecumdeathfun = function(odeSim, params){odeSim[,9]*params['PopSize']} 
yobscumdeathfun = function(odeSim, params){odeSim[,9]*params['k']} # This reporting fraction should not be the same as for cases! Need to think about this

# Cumulative hospitalizations
ytruecumhospfun = function(odeSim, params){odeSim[,12]*params['PopSize']} 
yobscumhospfun = function(odeSim, params){odeSim[,12]*params['k']} # This is what we will treat as testing

# The below functions are for the old model, need to update! #FIX#

# Daily incidence of symptomatic cases (observed version is care-seeking)
ytruedailyincfun = function(odeSim, params){ #calculate by differencing the cumulative incidence
  ytruecumincfun(odeSim, params)[2:length(odeSim[,1])] - ytruecumincfun(odeSim, params)[1:(length(odeSim[,1])-1)]}
yobsdailycarefun = function(odeSim, params){
  yobscumcarefun(odeSim, params)[2:length(odeSim[,1])] - yobscumcarefun(odeSim, params)[1:(length(odeSim[,1])-1)]}

# Daily deaths
ytruedailydeathfun = function(odeSim, params){
  ytruecumdeathfun(odeSim, params)[2:length(odeSim[,1])] - ytruecumdeathfun(odeSim, params)[1:(length(odeSim[,1])-1)]}
yobsdailydeathfun = function(odeSim, params){
  yobscumdeathfun(odeSim, params)[2:length(odeSim[,1])] - yobscumdeathfun(odeSim, params)[1:(length(odeSim[,1])-1)]} # This reporting fraction should not be the same as for cases

# Weekly incidence of symptomatic cases
ytrueweeklyincfun = function(odeSim, params){
  ytruecumincfun(odeSim, params)[7:length(odeSim[,1])] - ytruecumincfun(odeSim, params)[1:(length(odeSim[,1])-6)]}
yobsweeklyincfun = function(odeSim, params){
  yobscumcarefun(odeSim, params)[7:length(odeSim[,1])] - yobscumcarefun(odeSim, params)[1:(length(odeSim[,1])-6)]}

# Weekly deaths
ytrueweeklydeathfun = function(odeSim, params){
  ytruecumdeathfun(odeSim, params)[7:length(odeSim[,1])] - ytruecumdeathfun(odeSim, params)[1:(length(odeSim[,1])-6)]}
yobsweeklydeathfun = function(odeSim, params){
  yobscumdeathfun(odeSim, params)[7:length(odeSim[,1])] - yobscumdeathfun(odeSim, params)[1:(length(odeSim[,1])-6)]} # This reporting fraction should not be the same as for cases

# Hospital admits per week
ytrueweeklyhospfun = function(odeSim, params){
  ytruecumhospfun(odeSim, params)[7:length(odeSim[,1])] - ytruecumhospfun(odeSim, params)[1:(length(odeSim[,1])-6)]}
yobsweeklyhospfun = function(odeSim, params){
  yobscumhospfun(odeSim, params)[7:length(odeSim[,1])] - yobscumhospfun(odeSim, params)[1:(length(odeSim[,1])-6)]} # This reporting fraction should not be the same as for cases

# Total current patients, ICU patients, patients needing O2, patients needing vent
ycurrpatfun = function(odeSim, params){params['k']*odeSim[,8]} # curr hosp x reporting fraction
ycurrICUfun = function(odeSim, params){params['fICU']*params['k']*odeSim[,8]}
ycurrO2fun = function(odeSim, params){params['fO2']*params['k']*odeSim[,8]}
ycurrventfun = function(odeSim, params){params['fVent']*params['k']*odeSim[,8]}

## Helper functions to take the full vector to and from the vector of fitted parameters

paramshrinkbk = function(params){params[c(1,10)]}
paramexpandbk = function(miniparams,params){c(miniparams[1],params[c(2:9)], miniparams[2],params[c(11:17)])}

paramshrinkb = function(params){params[c(1)]}
paramexpandb = function(miniparams,params){c(miniparams[1],params[c(2:17)])}

paramshrinkk = function(params){params[10]}
paramexpandk = function(miniparams,params){c(params[c(1:9)], miniparams[1],params[c(11:17)])}

paramshrink = paramshrinkbk
paramexpand = paramexpandbk


## Likelihood function
likelihood = function(miniparams,fullparams,yfun,times,data){
  # print(miniparams)
  params = abs(paramexpand(miniparams,fullparams))
  
  if(x0(params)[1]<0){
    NLL = 1e20
  } else {
    # Simulate model
    xcurr = ode(x0(params), times, CoVode, params, method='ode45')
    
    # Measurement equation
    y = yfun(xcurr,params)
    
    # Negative Log Likelihood (NLL)
    # NLL =  sum(y) - sum(data*log(y)) # Poisson ML
    # note this is a slightly shortened version--there's an additive constant term missing but it 
    # makes calculation faster and won't alter the threshold. Alternatively, can do:
    # NLL = -sum(log(dpois(round(data),round(y)))) # the round is b/c Poisson is for (integer) count data
    # this can also barf if data and y are too far apart because the dpois will be ~0, which makes the log angry
    
    # ML using normally distributed measurement error (least squares)
    # NLL = -sum(log(dnorm(data,y,0.1*mean(data)))) # example WLS assuming sigma = 0.1*mean(data)
    NLL = sum((y - data)^2)/(2*(0.1*mean(data))^2)  # alternatively can do OLS but note this will mess with the thresholds
    #                             for the profile! This version of OLS is off by a scaling factor from
    #                             actual LL units. Added scaling temporarily, also note that mean data isn't a vector, 
    #                             it's just a number... We should also get this working with poisson later too...
  }
  return(NLL)
}

