#originally created on: March 29, 2018
#by: Nick Sard

#ABOUT: This script was written to use a breeding matrix functions in a for loop to generate summary statistics of the
#breeding matrices themselves, as well as the estimates of the true number of parents in the dataset

#loading in libraries
library(tidyverse)
library(vegan)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2021/Nspawners rarefaction project/Simulations/")

#load source scripts
source("C:/Users/sard/Google Drive/R/Data analysis/2021/Nspawners rarefaction project/Simulations/brd.mat.functions_v4.R")

#setting a range of parameters to iterate through
n_my.off <- c(10,100,1000)
n_my.par <- c(10,100,1000)
n_my.mates <- c(4)

#making the df to for loop through
my.params <- expand.grid(n_my.mates,n_my.par)
colnames(my.params) <- c("all_mates","all_par")
head(my.params)

#pedigree saver
peds <- NULL
sim.info <- NULL
i <- 1
i <- NULL
for(i in 1:nrow(my.params)){
  
  #########################
  ### setting variables ###
  #########################
  
  #Maximum expected number of offspring collected using specific gear
#  maxoff <- my.params$all_offs[i]
#  minoff <- my.params$all_offs[i]
  
  #Minimum and maximum expected sex ratio - Based on data from Black Lake
  min.sex.ratio <- 1
  max.sex.ratio <- 1
  
  #Minimum and maximum expected number of parents spawning in study area
  min.parents <- my.params$all_par[i]
  max.parents <- my.params$all_par[i]
  
  #Minimum and maximum expected number of fertilized eggs per female
  min.fert = 2500
  max.fert = 10000
  
  #setting minimumn and maximum bounds for mean number of mate pairs
  min.mates <- my.params$all_mates[i]
  max.mates <- my.params$all_mates[i]
  
  #now using a for loop to create many simulations to understand distrbutions of variables assoicated with simulations
  j <- 1
  j <- NULL
  for(j in 1:1){
    
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

    #convert to pedigree
    ped1 <- mat2ped(mat = mat.str)
    head(ped1)
    ped1$size <- n.par
    #saving information associated with each breeding matrix for graphics
    sim.info1 <- rbind(ms1)
    sim.info <- rbind(sim.info,sim.info1)
    
    #saving pedigree for later
    peds <- rbind(peds,ped1)
  } #end of j loop
} #end of i loop
head(sim.info)
head(peds)

#writing each to file for evulation via graphics
#write.table(x = sim.info,file = "Output/pedigree.accumulation.information.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
#write.table(x = peds,file = "Output/pedigree.accumulation.peds.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)

#now the re-sampling procedure

#first freeing some RAM
ped1 <- NULL
mat.str <- NULL

#setting a range of parameters to iterate through
n_my.off <- c(10,100,1000)
n_my.par <- c(10,100,1000)
my.reps <- 100

#making the df to for loop through
my.params <- expand.grid(n_my.par,n_my.off,my.reps)
colnames(my.params) <- c("all_par","all_off","reps")
head(my.params)

i <- 1
i <- NULL
out <- NULL
out1 <- NULL
out2 <- NULL
for(i in 1:nrow(my.params)){
  
  #first getting the pedigree I want
  
  ped1 <- peds[peds$size == my.params$all_par[i],]
  head(ped1)
  
  #now each sample size of offspring 
  #random sample that number and count the number of parents
  j <- 1
  j <- NULL
  out1 <- NULL
  for(j in 1:my.params$reps[i]){
    k<- 1
    k <- NULL
    for(k in 1:my.params$all_off[i]){
      print(paste("N_adults",my.params$all_par[i],"N_Offspring",my.params$all_off[i],"Rep",j,"Iterate",k))
        ped2 <- ped1[ped1$off %in% sample(x = ped1$off,size = k,replace = F),]
        uniq.dads <- length(unique(ped2$dad))
        uniq.moms <- length(unique(ped2$mom))
        uniq.pars <- sum(uniq.moms,uniq.dads)
        out2 <- data.frame(uniq.par = uniq.pars,
                         parents = my.params$all_par[i],
                         off_n = my.params$all_off[i],
                         k = k,
                         rep = j)
        out2
        out1 <- rbind(out1,out2)
    }
  }
  out <- rbind(out,out1)
}
head(out)
str(out)

#write.table(x = out,file = "Output/bootstrapped.nspawner.estimates.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
out <- read.table(file = "Output/bootstrapped.nspawner.estimates.txt",header = T,stringsAsFactors = F,sep = "\t")
table(out$parents,out$off_n)

tiff(filename = "Output/asymptote.figure.tiff",width = 11,height = 8,units = "in",res = 150)
out %>%
  group_by(parents,off_n,k) %>%
  summarize(avg = mean(uniq.par),
            sds = sd(uniq.par),
            count = n(),
            se = sds/sqrt(count),
            CI = 1.96*se) %>%
  mutate(off_n2 = paste("Offspring =",off_n)) %>%
  mutate(parents2 = paste("Breeding Adults =",parents)) %>%
  ggplot(aes(x=k,y=avg))+
    facet_grid(parents2~off_n2,scales = "free")+
    geom_ribbon(aes(ymin=(avg-CI),ymax=avg+CI),fill="grey",alpha=.5)+  
    geom_line(size=1)+
    scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
#    scale_y_log10()+
#    scale_x_log10()+
    theme_bw()+
    labs(x="Number of offspring sampled",y="Number of breeding adults detected")+
    theme(axis.title = element_text(size=18),
          axis.text = element_text(size=16,color = "black"),
          strip.text = element_text(size=15),
          legend.text = element_text(size=18),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
dev.off()

#fin!