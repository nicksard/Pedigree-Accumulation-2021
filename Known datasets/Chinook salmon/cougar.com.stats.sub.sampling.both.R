#originally created on: March 17, 2020
#by: Nick Sard

#ABOUT: This script was written to test community ecology theory on estimating the true number of 
#successful spawners based on a sampled set of offspring

#loading in libraries
library(tidyverse)
library(vegan)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2021/Nspawners rarefaction project/Known datasets/Chinook salmon/")

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

      #combining the comms
      #row.names(mom.com) == row.names(dad.com)
      all.com <- cbind(mom.com,dad.com)
      head(all.com)
      
      #getting estimates for data
      all.pool <- specpool(all.com)
      all.pool$type <- "all"
      head(all.pool)
      
      #getting the year now
      my.year <- gsub(pattern = '[a-z]',replacement = "",x =  my.files[i])
      my.year <- paste0("20",gsub(pattern = "\\.",replacement = "",x = my.year))
      
      #combining species accumulation estimates for graphics and evaluation
      pool1 <- rbind(all.pool)
      
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
#write.table(x = pool,file = "Output/bootstrap.both.estimates.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
pool <- read.table(file = "Output/bootstrap.both.estimates.txt",header = T,sep = "\t",stringsAsFactors = F)
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
df$total <- df$female + df$male
df$total.s <- df$female.s + df$male.s
head(df)
head(pool)

#filtering getting 
pool1 <- pool %>% 
  gather(key = "stat",value = "val",chao,jack1) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife"))
head(pool1)


#getting a plot now
tiff(filename = "Output/sfmr.bootstrapped.tiff",width = 5.5,height = 4,units = "in",res = 125)
ggplot()+
  geom_point(data = pool1, aes(x=noff_gt,y=val),size=1)+
  #  geom_hline(data = df1,aes(yintercept = chao),size=2,linetype=2,color="black")+
  geom_hline(data = df,aes(yintercept = total),size=1,linetype=3,color="darkgrey")+
  geom_hline(data = df,aes(yintercept = total.s),size=1,linetype=2,color="darkgrey")+
  geom_smooth(data = pool1, aes(x=noff_gt,y=val),se = F,size=1,color="#f0f0f0")+
  facet_grid(stat~year,scales = "free")+
  theme_bw()+
  labs(y=expression(hat(N)[S]),x=expression(N[off_s]),color="")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        axis.text.x = element_text(size=10,color = "black",angle=45,vjust=1,hjust = 1),
        strip.text = element_text(size=11),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()
#fin!
