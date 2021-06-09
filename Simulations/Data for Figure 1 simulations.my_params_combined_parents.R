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

#setting a range of parameters to iterate through
n_my.off <- c(25,50,100,200,500)
n_my.par <- c(25,50,100,200,500)
n_my.mates <- c(4)
n_my.sex.ratio <- c(1)

#making the df to for loop through
my.params <- expand.grid(n_my.mates,n_my.par,n_my.off,n_my.sex.ratio)
colnames(my.params) <- c("all_mates","all_par","all_offs","all_sex.ratio")
head(my.params)

#pedigree saver
peds <- NULL
sim.info_all <- NULL
pool_all <- NULL

i <- 1
i <- NULL
for(i in 1:nrow(my.params)){
  
  #########################
  ### setting variables ###
  #########################
  
  #Maximum expected number of offspring collected using specific gear
  maxoff <- my.params$all_offs[i]
  minoff <- my.params$all_offs[i]
  
  #Minimum and maximum expected sex ratio - Based on data from Black Lake
  min.sex.ratio <- my.params$all_sex.ratio[i]
  max.sex.ratio <- my.params$all_sex.ratio[i]
  
  #Minimum and maximum expected number of parents spawning in study area
  min.parents <- my.params$all_par[i]
  max.parents <- my.params$all_par[i]
  
  #Minimum and maximum expected number of fertilized eggs per female
  min.fert = 2500
  max.fert = 6500
  
  #setting minimumn and maximum bounds for mean number of mate pairs
  min.mates <- my.params$all_mates[i]
  max.mates <- my.params$all_mates[i]
  
  #creating objects to store information about each simulation and parameter estimation
  sim.info <- NULL
  pool <- NULL
  
  #now using a for loop to create many simulations to understand distrbutions of variables assoicated with simulations
  j <- 1
  j <- NULL
  for(j in 1:100){
    
    print(paste("Parameters",i,"Simulation",j))
    
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
    
    #using breding matrix function to create the breeding matrix to identify who breeds with whom
    mat.str <- brd.mat(moms = n.mom,dads = n.dad,lambda.low = min.mates,lambda.high =  max.mates)
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
      
    df <- mat.sub.sample(mat = mat.str,noff = my.params$all_offs[i])
    mplost <- nrow(df[df$off1 == 0,])
    mat2 <- ped2mat(ped = df)
    head(mat2)
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
    head(sim.info1)
    sim.info1$true.npar <- my.params$all_par[i]
    sim.info1$true.noff <- my.params$all_offs[i]
    sim.info1$true.sr <- my.params$all_sex.ratio[i]
    sim.info1$true.lambda <- my.params$all_mates[i]
    sim.info1
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
  
  #saving for later
  sim.info_all <- rbind(sim.info_all,sim.info)
  pool_all <- rbind(pool_all,pool)
}
head(sim.info_all)
head(pool_all)
my.params

#a couple quick checks
sim.info_all %>%
  filter(type == "Before") %>%
  summarize(ratio = n.par/true.npar) %>%
  group_by(ratio) %>% count()

sim.info_all %>%
  filter(type != "Before") %>%
  summarize(ratio = noff/true.noff) %>%
  group_by(ratio) %>% count()

#writing each to file for evulation via graphics
write.table(x = sim.info_all,file = "Output/simulation.npar25_500.noff25_500.parents.combined.information.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
write.table(x = pool_all,file = "Output/estimation.npar25_500.noff25_500.parents.combined.information.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)

#fin!