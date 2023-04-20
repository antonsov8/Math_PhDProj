# File variables prefix - MCP = Math Conditional Probability

# Question ----------------------------------------------------------------

# What is the probability for a cell to divide after time 't2d' given that it is currently of age 'a'?


# Loading Libraries,Data ----------------------------------------------
library(tidyverse)
library(emg)
load("Data/PubData.RData")



# Sec 1: Creating the IMTs ------------------------------------------------

# first finding the emg coefficients
emg.mle(GAD_IMTBefD_High/2) %>% # To convert IMT to time(in hours) rather than timeframes
  coef() -> MCP_emgCoef


# next, sampling IMTs
MCP_sampleSize <- 5e7
# set.seed(10)
# map(MCP_emgCoef, remg, n=MCP_sampleSize )
# set.seed(10)
remg(n = MCP_sampleSize,
     mu = MCP_emgCoef["mu"],
     sigma = MCP_emgCoef["sigma"],
     lambda = MCP_emgCoef["lambda"]) -> MCP_IMTs

# Sec 2: Setting Age ------------------------------------------------------
MCP_age = 25 # 10 hours is when I want to see the cells



# Sec 3: Setting the t2d --------------------------------------------------

MCP_t2d = 4 # Time to division, I am interested to calculate the probability for

MCP_t = MCP_age+MCP_t2d

# Sec 4: Calculating CondProb ----------------------------------------------------

MCP_CP_dt <- 5e-3

# Theoretical 

MCP_CP_theo_demg <- demg(MCP_t, # f(t)dt/S(age) is the conditional probability 
                         
                    mu = MCP_emgCoef["mu"],
                    sigma = MCP_emgCoef["sigma"],
                    lambda = MCP_emgCoef["lambda"]) 

MCP_CP_theo_surv <- pemg(MCP_age, 
                         mu = MCP_emgCoef["mu"],
                         sigma = MCP_emgCoef["sigma"],
                         lambda = MCP_emgCoef["lambda"],
                         lower.tail = F)

MCP_CP_theo <- MCP_CP_dt * MCP_CP_theo_demg / unname(MCP_CP_theo_surv )


# Empirical

# MCP_CP_emp_favEvents <- (MCP_IMTs-MCP_age)

MCP_CP_emp_favEvents <- MCP_IMTs[(MCP_t2d < MCP_IMTs -MCP_age) &  (MCP_IMTs -MCP_age <= MCP_t2d + MCP_CP_dt)] 
MCP_CP_emp_sampleSpace <- MCP_IMTs[MCP_IMTs > MCP_age] 

MCP_CP_emp <- length(MCP_CP_emp_favEvents)/length(MCP_CP_emp_sampleSpace)


# Sec 5: Comparing --------------------------------------------------------

100*(MCP_CP_theo - MCP_CP_emp)/MCP_CP_theo

# hist(MCP_IMTs, probability = T)
# lines(x = seq(20,60), y = demg(seq(20,60), 
#                                mu = MCP_emgCoef["mu"],
#                                sigma = MCP_emgCoef["sigma"],
#                                lambda = MCP_emgCoef["lambda"]
#                                ))
  
