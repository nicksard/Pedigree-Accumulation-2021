#originally created on: May 4, 2018
#by: Nick Sard

#ABOUT: This script was written to assess if the community ecology approach to estimating the number of successful parents is
#valid

#loading in libraries
library(tidyverse)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/")

#load source scripts
# - none -

#reading in data
tmp <- read.table(file = "Output/estimation.npar25_500.noff25_500.parents.combined.UD.information.txt",header = T,sep = "\t",stringsAsFactors = F)
tmp$n <- as.character(tmp$n)
table(tmp$n)
table(tmp$true_n)
head(tmp)

############
### Bias ###
############

head(tmp)
tmp2 <- tmp %>% 
  select(n,Species,chao,jack1,type,true_n) %>%
  gather(key = "stat",value = "pred",chao:jack1) %>%
  group_by(true_n,n,type,stat) %>%
  summarize(count = n(),
            pred.mean = mean(pred),
            true.mean = mean(true_n),
            bias = pred.mean/true.mean)
head(tmp2)
tmp2$n <- paste("Noff_s =",as.character(tmp2$n))
tmp2$type <- ifelse(test = tmp2$type == "decline",yes = "Decline","Uniform")
tmp2$stat <- ifelse(test = tmp2$stat == "chao", "Chao","Jackknife")
head(tmp2)
table(tmp2$type,tmp2$true_n)
head(tmp2)
table(tmp2$true_n)
table(tmp2$n)

#re-leveling
tmp2$n <- factor(tmp2$n,levels = c("Noff_s = 25","Noff_s = 50","Noff_s = 100","Noff_s = 200","Noff_s = 500"))
head(tmp2)
tiff(filename = "Output/Bias.UDfitness.figure.tiff",width = 4, height = 5.5,units = "in",res = 150)
ggplot(tmp2,aes(x=true_n,y=bias,color = type,group=type))+
  facet_grid(n~stat,scales = "free_x")+
  geom_hline(yintercept = 1,size=1,linetype=2,color="darkgrey")+
  geom_line(size=1)+
  geom_point(size=2)+
  labs(x=expression(N[S]),y=expression(Bias),color="Mate pair\nfitness")+
  theme_bw()+
  scale_color_manual(values = c("#a6cee3","#1f78b4"))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10,color = "black"),
        strip.text = element_text(size=10),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()
#fin!