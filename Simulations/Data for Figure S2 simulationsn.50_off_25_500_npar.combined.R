#originally created on: March 29, 2018
#by: Nick Sard

#ABOUT: This script was written to use a breeding matrix functions in a for loop to generate summary statistics of the
#breeding matrices themselves, as well as the estimates of the true number of parents in the dataset

#loading in libraries
library(tidyverse)
library(vegan)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/")

#load source scripts
source("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/brd.mat.functions_v4.R")

#########################
### setting variables ###
#########################

#Maximum expected number of offspring collected using specific gear
maxoff <- 50
minoff <- 50

#Minimum and maximum expected sex ratio - Based on data from Black Lake
min.sex.ratio <- 1
max.sex.ratio <- 1

#Minimum and maximum expected number of parents spawning in study area
min.parents <- 25
max.parents <- 500

#Minimum and maximum expected number of fertilized eggs per female
min.fert = 2500
max.fert = 6500

#setting minimumn and maximum bounds for mean number of mate pairs
lambda.high <- 4
lambda.low <- 4

#creating objects to store information about each simulation and parameter estimation
sim.info <- NULL
pool <- NULL

#now using a for loop to create many simulations to understand distrbutions of variables assoicated with simulations
j <- 1
j <- NULL
for(j in 1:100){
  
  print(j)
  
  ####################################################
  ### Clearing tmp varaibles before each simuation ###
  ####################################################
  n.mom <- NULL
  n.dad <- NULL
  n.par <- NULL
  n.off <- NULL
  sex.ratio <- NULL
  sex.ratio2 <- NULL
  picks <- NULL
  mat.str <- NULL
  ms1 <- NULL
  mat.sub <- NULL
  mp.lost <- NULL
  ped2 <-  NULL
  mat2 <- NULL
  ms2 <- NULL
  
  ######################################################
  ### Create a breeding matrix for entire study area ###
  ######################################################
  
  #defining the number of moms, dads, and rs distribution (poisson in this case) (20 and 5 mean, sd)
  
  #picking the number of parents from a uniform distrbution
  n.par <- round(runif(n = 1,min = min.parents, max = max.parents))
  n.par
  
  # assuming the the sex ratio of successful males to females is NOT 1:1
  sex.ratio <- round(runif(n = 1,min = min.sex.ratio,max = max.sex.ratio),1)
  sex.ratio
  
  #needed a way to pick of combination of males and females that would exactly (or as close as possible) to
  #the sex ratio that was choosen, here is my solution
  
  #enumerate all possible combinations of males and females, calculate sex ratio (sr) between them,
  #get the absoluate difference between their sr and the real sr, and pick the one with smallest difference
  picks <- expand.grid(1:n.par,1:n.par) %>%
    mutate(sr =  Var1/Var2) %>%
    mutate(npar = Var1 + Var2) %>%
    mutate(diffs = abs(sr - sex.ratio)) %>%
    mutate(diffnp = abs(npar - n.par)) %>%
    filter(diffnp == min(diffnp)) %>%
    filter(diffs == min(diffs))
  picks
  
  #sometimes is possible to pick more than one pick - e.g. when the sex ratio is 2, so I put in an if statement
  #to randomly select one of the rows in that case
  if(nrow(picks)== 1){
    n.mom <- picks$Var2
    n.dad <- picks$Var1
    sex.ratio2 <- round(picks$sr,2)
    diff <- picks$diff
  } else {
    warning(paste("There are",nrow(picks),"sex ratios that work. Picking one randomly."))
    my.row <- as.numeric(sample(x = row.names(picks),size = 1))
    picks <- picks[my.row,]
    n.mom <- picks$Var2
    n.dad <- picks$Var1
    sex.ratio2 <- round(picks$sr,2)
    diff <- picks$diff
  }
  picks
  
  #using breding matrix function to create the breeding matrix to identify who breeds with whom
  mat.str <- brd.mat(moms = n.mom,dads = n.dad,lambda.low = lambda.low,lambda.high =  lambda.high)
  head(mat.str)
  #filling in with some number of kids
  mat.str <- brd.mat.fitness(mat = mat.str,min.fert = min.fert,max.fert = max.fert,type = "uniform")
  head(mat.str)

  #getting summary stats
  ms1 <- mat.stats(mat = mat.str)
  ms1$type <- "Before"
  ms1$lost <- 0
  ms1$sim <- j
  ms1
  
  #######################################
  ### Subsamping full breeding matrix ###
  #######################################
  
  #randomly selecting the number of offspring that will be sampled in a given sampling effort
  n.off = round(runif(n = 1,min = minoff, max = maxoff))
  n.off

  #creating full pedigree with that breeding matrix, and sub-sampling each mate pair randomly
  df <- mat.sub.sample(mat = mat.str,noff = n.off)
  head(df)

  #counting how many mp lost
  mplost <- nrow(df[df$off1 == 0,])
  mat2 <- ped2mat(ped = df)
  #making into a pedigree for later
  ped2 <- mat2ped(mat = mat2)
  ped2
  
  #getting numbers of mates and rs per sex
  ms2 <- mat.stats(mat = mat2)
  ms2$type <- "After"
  ms2$lost <- mplost
  ms2$sim <- j
  ms2

  #saving information associated with each breeding matrix for graphics
  sim.info1 <- rbind(ms1,ms2)
  sim.info1$true.npar <- n.par
  sim.info1$true.noff <- n.off
  sim.info1$true.sr <- sex.ratio
  sim.info1$true.lambda <- lambda.high
  sim.info1
  head(sim.info1)
  sim.info <- rbind(sim.info,sim.info1)

  ########################################################################
  ### Application of the pedigree to species accumulation curve theory ###
  ########################################################################
  
  #making a community matrix for moms
  mom.com <- ped2 %>%
    select(-dad) %>%
    mutate(val = 1) %>%
    spread(key = "mom",value = "val",fill = 0)
  row.names(mom.com) <- mom.com$off
  mom.com$off <- NULL
  head(mom.com)
  
  #making a community matrix for dads
  dad.com <- ped2 %>%
    select(-mom) %>%
    mutate(val = 1) %>%
    spread(key = "dad",value = "val",fill = 0)
  row.names(dad.com) <- dad.com$off
  dad.com$off <- NULL
  head(dad.com)
  
  #combining the two
  #table(row.names(mom.com) == row.names(dad.com))
  all.com <- cbind(mom.com,dad.com)
  head(all.com)
  
  #getting estimates for data
  all.pool <- specpool(all.com)
  all.pool$type <- "all"
  all.pool$true_n <- ms1$n.par
  all.pool$true_off <- ms1$noff
  head(all.pool)
  
  
  #combining species accumulation estimates for graphics and evaluation
  pool1 <- all.pool
  pool1$mat_sim_iter <- j
  head(pool1)
  pool1
  #saving information associated with each breeding matrix for graphics
  pool <- rbind(pool,pool1)
}
head(sim.info)
head(pool)

#a couple quick checks
sim.info %>%
  filter(type == "Before") %>%
  summarize(ratio = n.par/true.npar) %>%
  group_by(ratio) %>% count()

sim.info %>%
  filter(type != "Before") %>%
  summarize(ratio = noff/true.noff) %>%
  group_by(ratio) %>% count()


#writing each to file for evulation via graphics
write.table(x = sim.info,file = "Output/simulation.50off.25_500_npar.sr1.combined.information.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
write.table(x = pool,file = "Output/estimation.50off.25_500_npar.sr1.combined.information.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)

#fin!