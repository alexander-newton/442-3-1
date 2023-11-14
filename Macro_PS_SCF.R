## Macro Problem Set 1 for Ben Moll’s Part
## Ed Manuel, 13 November 2023

rm(list=ls())

library(tidyverse)

# Approach 1 --------------------------------------------------------------

library(lodown)
library(survey)
library(mitools)

# # examine all available SCF microdata files
# scf_cat <-
#   get_catalog( "scf" ,
#                output_dir = file.path( path.expand( "~" ) , "SCF" ) )
# 
# # Download 2022 SCF to laptop
# scf_cat <- subset( scf_cat , year == 2022 )
# scf_cat <- lodown( "scf" , scf_cat )

# read in the files 
# this looks like all the actual data
scf_imp <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2022.rds" ) )

# this looks like the weights
scf_rw <- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2022 rw.rds" ) )

# I think this basically re-weights the survey and saves as complicated object
scf_design <- 
  svrepdesign( 
    weights = ~wgt , 
    repweights = scf_rw[ , -1 ] , 
    data = imputationList( scf_imp ) , 
    scale = 1 ,
    rscales = rep( 1 / 998 , 999 ) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

library(convey)
scf_design$designs <- lapply( scf_design$designs , convey_prep )

scf_MIcombine( with( scf_design , svygini( ~ networth ) ) )

# Approach 2 --------------------------------------------------------------

# Following here: https://medium.com/@candace_60118/survey-of-consumer-finances-scf-analyzing-weighted-data-in-r-4e4789112c8a

# Also this: https://bookdown.org/jimr1603/Intermediate_R_-_R_for_Survey_Analysis/survey-specific-functions.html 

library(reldist)    

# Download the files
download.file("https://www.federalreserve.gov/econres/files/scfp2022excel.zip", "SCFP2022.zip")
unzip("SCFP2022.zip")
df <- read.csv("SCFP2022.csv")

## Q1: Make two histograms for: (a) the level of wealth (only plot up to the 95th percentile)

# Find 95th perc
q_95<- wtd.quantile(df$NETWORTH, q=0.95, weight = df$WGT)

# Make hist up to 95th perc
hist1<-df%>%
  filter(NETWORTH<q_95)%>%
  ggplot(mapping = aes(NETWORTH,weight = WGT)) + 
  geom_histogram(binwidth = 100000,
                 color = "white") +
  labs(x = "Net Worth Histogram")

hist1

## (b) the logarithm of wealth.

# Make hist up to 95th perc
hist2<-df%>%
  select(NETWORTH,WGT)%>%
  filter(NETWORTH<q_95)%>%
  filter(NETWORTH>0)%>%
  mutate(ln_networth = log(NETWORTH))%>%
  ggplot(mapping = aes(ln_networth,weight = WGT)) + 
  geom_histogram(binwidth = 1,
                 color = "white") +
  labs(x = "Log Net Worth Histogram")

hist2

## Q2: Make a table with the wealth shares of 
# (a) the bottom 50 percent, 
# (b) the next 40 percent, 
# (c) the next 9 percent, 
# (d) the top 1 percent,
# (e) the top 0.1 percent of households in the sample.

# Set up the quantile spacing needed for (a)-(e)
quantiles = rbind(c(0,0.5),
                  c(0.5,0.9),
                  c(0.9,0.99),
                  c(0.99,1),
                  c(0.999,1))

# Get total wealth to construct shares
totalwealth = sum(df$NETWORTH*df$WGT)

# Find share for each quantile
wealth_share<-list()
for (i in 1:nrow(quantiles)){
quantile_space = quantiles[i,]  
  
wealth_lb<- wtd.quantile(df$NETWORTH, q=quantile_space[1], weight = df$WGT)
wealth_ub<- wtd.quantile(df$NETWORTH, q=quantile_space[2], weight = df$WGT)

wealth_sample<-df%>%
  # extract the relevant sample
  filter(NETWORTH>wealth_lb)%>%
  filter(NETWORTH<wealth_ub)
  
wealth_share [[i]]= sum(wealth_sample$NETWORTH*wealth_sample$WGT) / totalwealth
  
}


## Q3: Plot the quantile function