##created by: Nick Sard
##created on: Sept. 7, 2020

#About: Created to test if male and female variation in fitness differed

#setting working directory
setwd("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/")

#loading libraries
library(tidyverse)
library(grid)
library(gridExtra)

#load source scripts
source("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/brd.mat.functions_v4.R")

#setting some parameters
nsims <- 100
npar <- c(25,50,100,250,500)
noff <- c(25,50,100,250,500)
sex.ratio <- 1
lambda <- c(4)
type <- c("uniform","decline")

#getting a list of all combinations
all.parms <- expand.grid(npar,noff,lambda,sex.ratio,type,stringsAsFactors = F)
colnames(all.parms) <- c("npar","noff","lambda","sex.ratio","type")
head(all.parms)

i <- 1
i <- NULL
out <- NULL
for(i in 1:nrow(all.parms)){
  
  for(j in 1:nsims){
    
    #simulation print
    print(paste(i,j))
    
    #dividing up npar based on sex ratio
    #enumerate all possible combinations of males and females, calculate sex ratio (sr) between them,
    #get the absolute difference between their sr and the real sr, and pick the one with smallest difference
    picks <- expand.grid(1:all.parms$npar[i],1:all.parms$npar[i]) %>%
      mutate(sr =  Var1/Var2) %>%
      mutate(npar = Var1 + Var2) %>%
      mutate(diffs = abs(sr - sex.ratio)) %>%
      mutate(diffnp = abs(npar - all.parms$npar[i])) %>%
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
    
    #creating the original breeding  matrix, subsampling it, and doing some basic stats before/after the  subsampling
    mat <- brd.mat(moms = n.mom,dads = n.dad,lambda.low = all.parms$lambda[i],lambda.high =  all.parms$lambda[i])
    mat <- brd.mat.fitness(mat = mat,min.fert = 4000,max.fert = 12000,type = all.parms$type[i])
    ms1 <- mat.stats(mat = mat)
    ped <- mat.sub.sample(mat = mat,noff = all.parms$noff[i])
    mat1 <- ped2mat(ped = ped)
    ms2 <- mat.stats(mat = mat1)
    
    #saving all this information for later
    all.ms <- rbind(ms1,ms2)
    all.ms$true.npar <- all.parms$npar[i]
    all.ms$true.noff <- all.parms$noff[i]
    all.ms$true.lambda <- all.parms$lambda[i]
    all.ms$true.sr <- all.parms$sex.ratio[i]
    all.ms$true.sckew <- all.parms$type[i]
    all.ms$mat <- c("Original","Subsampled")
    out <- rbind(out,all.ms)
  }
}
head(out)

#writing this stuff out to file
write.table(x = out,file = "Output/simulation.mat.stats.4K_12K.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)
out <- read.table(file = "Output/simulation.mat.stats.4K_12K.txt",header = T,sep = "\t",stringsAsFactors = F)
head(out)
#now going to do some graphics

out1 <- out
head(out1)
#doing some re-leveling
unique(x = out1$true.npar)
unique(x = out1$true.noff)
out1$true.npar1 <- factor(x = paste("Npar =",as.character(out1$true.npar)),levels = c("Npar = 25","Npar = 50","Npar = 100","Npar = 250","Npar = 500"))
out1$true.noff1 <- factor(x = paste("Noff =",as.character(out1$true.noff)),levels = c("Noff = 25","Noff = 50","Noff = 100","Noff = 250","Noff = 500"))
head(out1)
#looking at the original data sets right now

#checking the assumption about sampling few parents when offspring sample size is small
out1 %>%
  filter(mat != "Original") %>%
  mutate(ratio = n.par/true.npar) %>%
  ggplot(aes(x=true.npar1,y=ratio,color=true.sckew))+
  facet_wrap(~true.noff1,ncol=1,strip.position = "right")+
  geom_boxplot()+
  theme_bw()+
  labs(x="Breeding matrix size",y="Proportion of parents not sampled",color="Male\nreproductive sckew")

out1 %>% 
  mutate(sd.ratio = male.rs/female.rs) %>%
  ggplot(aes(x=true.npar1,y=sd.ratio,color=true.sckew))+
  facet_grid(mat~true.noff1)+
  geom_boxplot()+
  theme_bw()+
  labs(x="Breeding matrix size",y="Ratio of Male/Female Mean RS",color="Male\nreproductive sckew")

out1 %>% 
  mutate(sd.ratio = male.sd/female.sd) %>%
  ggplot(aes(x=true.npar1,y=sd.ratio,color=true.sckew))+
  facet_grid(mat~true.noff1)+
  geom_boxplot()+
  theme_bw()+
  labs(x="Breeding matrix size",y="Ratio of Male/Female Stdev.",color="Male\nreproductive sckew")

p1 <- out1 %>% 
  filter(mat == "Original") %>%
  select(female.rs,male.rs,true.sckew,true.npar1,true.noff1) %>%
  gather(key = sex,value= stat,-true.sckew:-true.noff1) %>%
  mutate(sex = gsub(pattern = "\\.rs",replacement = "",x = sex)) %>%
  mutate(type = paste(true.sckew,sex)) %>%
  ggplot(aes(x=sex,y=stat,color=true.sckew))+
  facet_grid(true.noff1~true.npar1)+
  geom_boxplot()+
  theme_bw()+
  labs(x="",y="Mean fitness",color="Male\nreproductive sckew")
p1
p2 <- out1 %>% 
  filter(mat == "Original") %>%
  select(female.sd,male.sd,true.sckew,true.npar1,true.noff1) %>%
  gather(key = sex,value= stat,-true.sckew:-true.noff1) %>%
  mutate(sex = gsub(pattern = "\\.sd",replacement = "",x = sex)) %>%
  mutate(type = paste(true.sckew,sex)) %>%
  ggplot(aes(x=sex,y=stat,color=true.sckew))+
  facet_grid(true.noff1~true.npar1)+
  geom_boxplot()+
  theme_bw()+
  labs(x="",y="Stdev.in fitness",color="Male\nreproductive sckew")
p2

tiff(filename = "Output/SD in fitness.tiff",width = 11,height = 8,units = "in",res = 125)
out1 %>% 
  filter(mat == "Original") %>%
  select(female.sd,male.sd,true.sckew,true.npar1,true.noff1) %>%
  gather(key = sex,value= stat,-true.sckew:-true.noff1) %>%
  mutate(sex = gsub(pattern = "\\.sd",replacement = "",x = sex)) %>%
  mutate(type = paste(true.sckew,sex)) %>%
  ggplot(aes(x=sex,y=stat,color=true.sckew))+
  facet_grid(true.noff1~true.npar1)+
  geom_boxplot()+
  theme_bw()+
  scale_color_manual(values = c("#a6cee3","#1f78b4"))+
  labs(x="",y="Stdev. in reproductive success",color="Male\nreproductive sckew")+
  theme(axis.title = element_text(size=14),
      axis.text = element_text(size=12,color = "black"),
      strip.text = element_text(size=11),
      legend.text = element_text(size=14),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position = "none")
dev.off()

p3 <- out1 %>% 
  filter(mat == "Original") %>%
  select(females.mates,male.mates,true.sckew,true.npar1,true.noff1) %>%
  gather(key = sex,value= stat,-true.sckew:-true.noff1) %>%
  mutate(sex = gsub(pattern = "\\.mates",replacement = "",x = sex)) %>%
  mutate(type = paste(true.sckew,sex)) %>%
  ggplot(aes(x=sex,y=stat,color=true.sckew))+
  facet_grid(true.noff1~true.npar1)+
  geom_boxplot()+
  theme_bw()+
  labs(x="",y="Mean mates",color="Male\nreproductive sckew")
p3 
p4 <- out1 %>% 
  filter(mat == "Original") %>%
  select(max.female.mates,max.male.mates,true.sckew,true.npar1,true.noff1) %>%
  gather(key = sex,value= stat,-true.sckew:-true.noff1) %>%
  mutate(sex = gsub(pattern = "\\.mates",replacement = "",x = sex)) %>%
  mutate(sex = gsub(pattern = "max\\.",replacement = "",x = sex)) %>%
  mutate(type = paste(true.sckew,sex)) %>%
  ggplot(aes(x=sex,y=stat,color=true.sckew))+
  facet_grid(true.noff1~true.npar1)+
  geom_boxplot()+
  theme_bw()+
  labs(x="",y="Max number of mates",color="Male\nreproductive sckew")
p4

#saving them all to PDFs
pdf("Output/simulation.summary.4k_12K.stats.pdf", onefile = TRUE)
grid.arrange(p1)
grid.arrange(p2)
grid.arrange(p3)
dev.off()
#fin!