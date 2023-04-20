# File variables prefix - MCP = Math Conditional Probability

# Question ----------------------------------------------------------------

# What is the probability for a cell to divide after time 't2d' given that it is currently of age 'a'?


# Loading Libraries,Data ----------------------------------------------
library(tidyverse)
library(emg)
load("Data/PubData.RData")


# Sec 1: Creating the IMTs ------------------------------------------------

# first finding the emg coefficients
emg.mle(GAD_IMTBefD_High) %>%
  coef() -> MCP_emgCoef


# next, sampling IMTs
MCP_sampleSize <- 1e2
set.seed(10)
map(MCP_emgCoef, remg, n=MCP_sampleSize )
set.seed(10)
remg(n = MCP_sampleSize,
     mu = MCP_emgCoef["mu"],
     sigma = MCP_emgCoef["sigma"],
     lambda = MCP_emgCoef["lambda"])

# Sec 2: Setting Age ------------------------------------------------------


# Sec 3: Calculating CondProb ----------------------------------------------------



