#originally created on: May 4, 2018
#by: Nick Sard

#ABOUT: This script was written to assess if the community ecology approach to estimating the number of successful parents is
#valid

#loading in libraries
library(tidyverse)
library(vegan)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/")
#load source scripts
# - none -

#reading in data
df1 <- read.table(file = "Output/estimation.50off.25_500_npar.sr1.combined.information.txt",header = T,sep = "\t",stringsAsFactors = F)
df2 <- read.table(file = "Output/estimation.500off.25_500_npar.sr1.combined.information.txt",header = T,sep = "\t",stringsAsFactors = F)
tmp <- rbind(df1,df2)
table(tmp$n)
head(tmp)


tmp1 <- tmp %>% 
  select(n,Species,chao,jack1,type,true_n) %>%
  gather(key = "stat",value = "pred",chao:jack1)
head(tmp1)

tmp1$n <- factor(tmp1$n, levels = c("50","500"))
tmp1$type <- "Parents combined"
tmp1$stat <- ifelse(test = tmp1$stat == "chao", "Chao","Jackknife")

tiff(filename = "Output/prediction.error.combined.figure.tiff",width = 5.5,height =4,units = "in",res = 150)
ggplot(tmp1,aes(x=true_n,y=pred,fill =n))+
  facet_wrap(~stat,ncol=2,strip.position = "top")+
  geom_point(size=2,alpha=.75,shape=21)+
  geom_smooth(method = "lm",se = F,size=.5)+
  geom_abline(slope=1,size=.5,linetype=2)+
  theme_bw()+
  scale_fill_manual(values = c("#a6cee3","#1f78b4"))+
  labs(x=expression(N[S]),y=expression(hat(N)[S]),color="Method")+
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=11),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()

#fin!