#originally created on: March 17, 2020
#by: Nick Sard

#ABOUT: This script was written to test community ecology theory on estimating the true number of 
#successful spawners based on a sampled set of offspring

#loading in libraries
library(tidyverse)
library(vegan)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2021/Nspawners rarefaction project/Known datasets/Lake Sturgeon/")

#load source scripts
# - none - 

#identifying the pedigrees to process
my.files <- list.files(path = "Input/",pattern = "txt")
my.files

#using a for loop to estimate the true number of 
i <- 1
i <- NULL
pool <- NULL
for(i in 1:length(my.files)){
  
  #print statement
#  print(i)
  
  #NULLing out things
  ped <- NULL
  
  #reading dataset
  ped <- read.table(file = paste0("Input/",my.files[i]),header = T,stringsAsFactors = F)
  head(ped)
  
  #doing some column renaming
  colnames(ped) <- c("off","dad","mom","rem")
  ped$rem <- NULL
  
  #identifying the maximum number of offspring that could be possibly sampled
  noff.max <- nrow(ped)
  noff.max
  
  #identifying intervals to run though
  my.vals <- seq(from=10,to = noff.max,by = 10)
  my.vals
  
  j <- 1
  j <- NULL
  for(j in 1:length(my.vals)){
    
    k <- 1
    k <- NULL
    for(k in 1:100){
      
      print(paste(my.files[i],"offspring sampled",my.vals[j],"Replicate",k))
      
      #selecting a common ped
      ped1 <- ped %>%
        mutate(rd = runif(n = nrow(ped))) %>%
        arrange(rd) %>%
        mutate(mr = 1:nrow(ped)) %>%
        filter(mr %in% 1:my.vals[j]) %>%
        select(-rd,-mr)
      head(ped1)
      
      #sampling the comm for moms
      mom.com <-  ped1 %>%
        select(-dad) %>%
        mutate(val = 1) %>%
        spread(key = "mom",value = "val",fill = 0)
      row.names(mom.com) <- mom.com$off
      mom.com$off <- NULL
      head(mom.com)

      
      #making a community matrix for dads
      dad.com <- ped1 %>%
        select(-mom,) %>%
        mutate(val = 1) %>%
        spread(key = "dad",value = "val",fill = 0)
      row.names(dad.com) <- dad.com$off
      dad.com$off <- NULL
      head(dad.com)
      
      #combining the comms
      #row.names(mom.com) == row.names(dad.com)
      all.com <- cbind(mom.com,dad.com)
      head(all.com)
      
      #getting estimates for data
      all.pool <- specpool(all.com)
      all.pool$type <- "all"
      head(all.pool)
      
      #getting the year now
      my.year <- gsub(pattern = '_.*',replacement = "",x =  my.files[i])
      my.year
      
      #combining species accumulation estimates for graphics and evaluation
      pool1 <- all.pool
      
      #adding year to the end here
      pool1$year <- my.year
      pool1$noff.sample <- my.vals[j]
      pool1$rep <- k
      pool1
      
      #saving information associated with each breeding matrix for graphics
      pool <- rbind(pool,pool1)
    } #end of K loop
  } #end of j loop
  #making a community matrix for moms
  
} #end of i for loop
names(pool)[1] <- "observed"
names(pool)[9] <- "noff_gt"
pool$type <- "All parents"
head(pool)

#saving things for later
#write.table(x = pool,file = "Output/scdrs.bootstrap.estimates.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
pool <- read.table(file = "Output/scdrs.bootstrap.estimates.txt",header = T,sep = "\t",stringsAsFactors = F)

##getting the actual estimates
ped15 <- read.table(file = paste0("Input/2015_run3.BestConfig.txt"),header = T,stringsAsFactors = F)
#doing some column renaming
colnames(ped15) <- c("off","dad","mom","rem")
ped15$rem <- NULL

#sampling the comm for moms
mom.com <-  ped15 %>%
  select(-dad) %>%
  mutate(val = 1) %>%
  spread(key = "mom",value = "val",fill = 0)
row.names(mom.com) <- mom.com$off
mom.com$off <- NULL
head(mom.com)


#making a community matrix for dads
dad.com <- ped15 %>%
  select(-mom,) %>%
  mutate(val = 1) %>%
  spread(key = "dad",value = "val",fill = 0)
row.names(dad.com) <- dad.com$off
dad.com$off <- NULL
head(dad.com)

#combining the comms
#row.names(mom.com) == row.names(dad.com)
all.com <- cbind(mom.com,dad.com)
head(all.com)

#getting estimates for data
all.pool_15 <- specpool(all.com)
all.pool_15$type <- "all"
head(all.pool_15)


##getting the actual estimates
ped16 <- read.table(file = paste0("Input/2016_run3.BestConfig.txt"),header = T,stringsAsFactors = F)

#doing some column renaming
colnames(ped16) <- c("off","dad","mom","rem")
ped16$rem <- NULL

#sampling the comm for moms
mom.com <-  ped16 %>%
  select(-dad) %>%
  mutate(val = 1) %>%
  spread(key = "mom",value = "val",fill = 0)
row.names(mom.com) <- mom.com$off
mom.com$off <- NULL
head(mom.com)


#making a community matrix for dads
dad.com <- ped16 %>%
  select(-mom,) %>%
  mutate(val = 1) %>%
  spread(key = "dad",value = "val",fill = 0)
row.names(dad.com) <- dad.com$off
dad.com$off <- NULL
head(dad.com)

#combining the comms
#row.names(mom.com) == row.names(dad.com)
all.com <- cbind(mom.com,dad.com)
head(all.com)

#getting estimates for data
all.pool_16 <- specpool(all.com)
all.pool_16$type <- "all"
head(all.pool_16)

#combing
all.pool <- rbind(all.pool_15,all.pool_16)
all.pool

df2 <- all.pool[,c("Species","type")]
row.names(df2) <- NULL
df2$year <- c(2015,2016)
df2$type <- "All parents"
names(df2)[1] <- "val"
df2 <- rbind(df2,df2)
df2$stat <- rep(c("Chao","Jackknife"),each=2)
df2
head(pool)
pool1 <- pool %>% 
  gather(key = "stat",value = "val",chao,jack1) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife"))
head(pool1)



#getting a plot now
tiff(filename = "Output/scdrs.bootstrapped.tiff",width = 5.5,height = 4,units = "in",res = 125)
ggplot()+
  geom_point(data = pool1, aes(x=noff_gt,y=val),size=1)+
  geom_hline(data = df2,aes(yintercept = val),size=1,linetype=1,color="darkgrey")+
  geom_smooth(data = pool1, aes(x=noff_gt,y=val),se = F,size=1,color="#f0f0f0")+
  facet_grid(stat~year,scales = "free")+
  theme_bw()+
  labs(y=expression(hat(N)[S]),x=expression(N[off_s]),color="")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=11),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()

#fin!