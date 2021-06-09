#originally created on: March 17, 2020
#by: Nick Sard

#ABOUT: This script was written to test community ecology theory on estimating the true number of 
#successful spawners based on a sampled set of offspring

#loading in libraries
library(tidyverse)
library(vegan)
library(readxl)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Known datasets/Chinook salmon/")

#load source scripts
# - none - 

#identifying the pedigrees to process
my.files <- list.files(path = "Input/",pattern = "mendel")
my.files

#using a for loop to estimate the true number of 
i <- 1
i <- NULL
pool <- NULL
for(i in 1:length(my.files)){
  
  #pring statement
  print(i)
  
  #NULLing out things
  ped <- NULL
  
  #reading dataset
  ped <- read.table(file = paste0("Input/",my.files[i]),header = T,sep = "\t",stringsAsFactors = F)
  head(ped)
  
  #identifying the maximum number of offspring that could be possibly sampled
  noff.max <- nrow(ped)
  
  #identifying intervals to run though
  my.vals <- seq(from=50,to = noff.max,by = 50)
  my.vals
  j <- 1
  j <- NULL
  for(j in 1:length(my.vals)){
    
    k <- 1
    k <- NULL
    for(k in 1:100){
      
      print(paste(my.files[i],"offspring sampled",my.vals[j],"Replicate",k))
      #sampling the comm for moms
      mom.com <- ped %>%
        mutate(rd = runif(n = nrow(ped))) %>%
        arrange(rd) %>%
        mutate(mr = 1:nrow(ped)) %>%
        filter(mr %in% 1:my.vals[j]) %>%
        select(-rd,-mr) %>%
        select(-dad,-type) %>%
        mutate(val = 1) %>%
        spread(key = "mom",value = "val",fill = 0)
      row.names(mom.com) <- mom.com$off
      mom.com$off <- NULL
      head(mom.com)
      
      #getting estimates for data
      mom.pool <- specpool(mom.com)
      mom.pool$type <- "female"
      mom.pool
      
      #making a community matrix for dads
      dad.com <- ped %>%
        mutate(rd = runif(n = nrow(ped))) %>%
        arrange(rd) %>%
        mutate(mr = 1:nrow(ped)) %>%
        filter(mr %in% 1:my.vals[j]) %>%
        select(-rd,-mr) %>%
        select(-mom,-type) %>%
        mutate(val = 1) %>%
        spread(key = "dad",value = "val",fill = 0)
      row.names(dad.com) <- dad.com$off
      dad.com$off <- NULL
      head(dad.com)
      
      #getting estimates for data
      dad.pool <- specpool(dad.com)
      dad.pool$type <- "male"
      head(dad.pool)
      
      #getting the year now
      my.year <- gsub(pattern = '[a-z]',replacement = "",x =  my.files[i])
      my.year <- paste0("20",gsub(pattern = "\\.",replacement = "",x = my.year))
      
      #combining species accumulation estimates for graphics and evaluation
      pool1 <- rbind(mom.pool,dad.pool)
      
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
head(pool)

#saving things for later
#write.table(x = pool,file = "Output/bootstrap.estimates.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
pool <- read.table(file = "Output/bootstrap.estimates.txt",header = T,sep = "\t",stringsAsFactors = F)
head(pool)

#reading
df <- read.table(file = "Input/Summary of adults.txt",header = T,stringsAsFactors = F)
head(df)

#identifying the pedigrees to process
my.files <- list.files(path = "Input/",pattern = "mendel")
my.files

#using a for loop to estimate the true number of 
i <- 1
i <- NULL
out <- NULL
for(i in 1:length(my.files)){
 
  #reading dataset
  ped <- read.table(file = paste0("Input/",my.files[i]),header = T,sep = "\t",stringsAsFactors = F)
  head(ped)
  
  #getting the year now
  my.year <- gsub(pattern = '[a-z]',replacement = "",x =  my.files[i])
  my.year <- as.numeric(paste0("20",gsub(pattern = "\\.",replacement = "",x = my.year)))
  my.year
  
  #successful
  female.s <- length(unique(ped$mom))-1
  male.s <- length(unique(ped$dad))-1
  df$female.s[df$year == my.year] <- female.s
  df$male.s[df$year == my.year] <- male.s
  df
} # end

#checking stuff
head(df)
head(pool)

#filtering getting 
pool1 <- pool %>% select(jack1,noff_gt,type,year,noff.sample,rep)
row.names(pool1) <- NULL
pool1$jack1 <- round(pool1$jack1)
pool1$type <- ifelse(pool1$type == "female","Female","Male")
head(pool1)

head(df)

df1 <- df %>%
  select(-female.s,-male.s) %>%
  gather(key = "type",value = "n",female,male) %>%
  select(year,type,n)
names(df1)[3] <- "jack1" 
df1$type <- ifelse(df1$type == "female","Female","Male")
df1
df2 <- df %>%
  select(-female,-male) %>%
  gather(key = "type",value = "ns",female.s,male.s) %>%
  mutate(type = gsub(pattern = ".s",replacement = "",x = type)) %>%
  select(year,type,ns)
names(df2)[3] <- "jack1" 
df2$jack12 <- df2$jack1*2
df2$type <- ifelse(df2$type == "female","Female","Male")
df2

#based on simulations
df3 <- df %>%
  select(year,female,male) %>%
  gather(key = "sex",value = "N",-year) %>%
  mutate(N_90 = N*.9)
df3
head(df1)
head(df2)
df2 <- cbind(df2,df3[,-1:-2])
head(df2)
df2$N_4x <- df2$N*4
df2
#making a figure
p1 <- ggplot()+
  geom_point(data = pool1, aes(x=noff_gt,y=jack1))+
  geom_hline(data = df1,aes(yintercept = jack1),size=2,linetype=2,color="black")+
  geom_hline(data = df2,aes(yintercept = jack1),size=2,linetype=1,color="darkgrey")+
  geom_linerange(data = df2,aes(x = N_90,ymin=0,ymax=jack1),size=2,linetype="solid",color="#b2df8a")+
  geom_linerange(data = df2,aes(x = N_4x,ymin=0,ymax=jack1),size=2,linetype="solid",color="#33a02c")+
  geom_smooth(data = pool1, aes(x=noff_gt,y=jack1),se = F,size=2,color="#f0f0f0")+
  facet_grid(type~year,scales = "free")+
  theme_bw()+
  labs(y=expression(hat(N)[S]),x=expression(N[off_s]),color="")+
  theme(axis.text = element_text(size=12,color = "black"),
        axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        axis.title = element_text(size=16),
        strip.text = element_text(size=12))
p1
ggsave(filename = "Output/chinook.salmon.bootstrap.tiff",plot = p1,device = "tiff",dpi = 300,width = 11,height = 8,units = "in")
#fin!