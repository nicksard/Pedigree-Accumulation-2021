#originally created on: May 4, 2018
#by: Nick Sard

#ABOUT: This script was written to assess if the community ecology approach to estimating the number of successful parents is
#valid

#loading in libraries
library(tidyverse)
library(grid)
library(gridExtra)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2021/Nspawners rarefaction project/Simulations/")

#load source scripts
# - none -

#reading in data
tmp <- read.table(file = "Output/estimation.npar25_500.noff25_500.parents.combined.information.txt",header = T,sep = "\t",stringsAsFactors = F)
tmp$n <- as.character(tmp$n)
table(tmp$n)
table(tmp$true_n)
head(tmp)

############
### RMSE ###
############

tmp1 <- tmp %>% 
  select(n,Species,chao,jack1,type,true_n) %>%
  gather(key = "stat",value = "pred",chao:jack1) %>%
  mutate(diff = pred-true_n) %>%
  mutate(true_n_var = diff^2) %>%
  group_by(true_n,n,type,stat) %>%
  summarize(count = n(),
            total = sum(true_n_var),
            rmse = sqrt(total / count))
head(tmp1)
tmp1$n <- paste("Noff =",as.character(tmp1$n))
tmp1$stat <- ifelse(test = tmp1$stat == "chao", "Chao","Jackknife")
head(tmp1)

tmp1$type <- "Parents combined"

#re-leveling
unique(tmp1$n)
tmp1$n <- factor(tmp1$n,levels = c("Noff = 25","Noff = 50","Noff = 100","Noff = 200","Noff = 500"))

#making figure
#tiff(filename = "Output/RMSE.combined.figure.tiff",width = 4,height =5.5,units = "in",res = 150)
p1.rmse <- ggplot(tmp1,aes(x=true_n,y=rmse,color = stat))+
  facet_wrap(~n,ncol=1,strip.position = "right")+
  geom_line(size=1)+
  geom_point(size=3)+
#  scale_y_log10()+
  labs(x=expression(N[S]),y="RMSE",color="",tag = "A")+
  theme_bw()+
  scale_color_manual(values = c("#1b9e77","#7570b3"))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
#dev.off()
p1.rmse
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
tmp2
tmp2 %>% as.data.frame()
tmp2$n <- paste("Noff_s =",as.character(tmp2$n))
tmp2$type <- "Parents combined"
tmp2$stat <- ifelse(test = tmp2$stat == "chao", "Chao","Jackknife")
head(tmp2)
tmp2
table(tmp2$true_n)
#re-leveling
tmp2$n <- factor(tmp2$n,levels = c("Noff_s = 25","Noff_s = 50","Noff_s = 100","Noff_s = 200","Noff_s = 500"))
table(tmp2$n)
#tiff(filename = "Output/Bias.combined.figure.tiff",width = 4,height =5.5,units = "in",res = 150)
p2.bias <- ggplot(tmp2,aes(x=true_n,y=bias,color = stat))+
  facet_wrap(~n,ncol=1,strip.position = "right")+
  geom_hline(yintercept = 1,size=1,linetype=2,color="darkgrey")+
  geom_line(size=1)+
  geom_point(size=3)+
  labs(x=expression(N[S]),y=expression(Bias~(hat(N)[S]/N[S])),color="Method",tag="B")+
  scale_y_continuous(limits = c(0,1.3),breaks = seq(from = 0, to = 1, by=.25))+
  theme_bw()+
  scale_color_manual(values = c("#1b9e77","#7570b3"))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
#dev.off()
p2.bias
##########
### CV ###
##########

head(tmp)
tmp3 <- tmp %>% 
  select(n,Species,chao,jack1,type,true_n) %>%
  gather(key = "stat",value = "pred",chao:jack1) %>%
  group_by(true_n,n,type,stat) %>%
  summarize(count = n(),
            pred.mean = mean(pred),
            pred.sd = sd(pred),
            cv = (pred.sd/pred.mean)*100)
head(tmp3)
tmp3
tmp3 %>% as.data.frame()
tmp3$n <- paste("Noff_s =",as.character(tmp3$n))
tmp3$type <- "Parents combined"
tmp3$stat <- ifelse(test = tmp3$stat == "chao", "Chao","Jackknife")
head(tmp3)
tmp3
table(tmp3$true_n)
#re-leveling
tmp3$n <- factor(tmp3$n,levels = c("Noff_s = 25","Noff_s = 50","Noff_s = 100","Noff_s = 200","Noff_s = 500"))
table(tmp3$n)
#tiff(filename = "Output/CV.combined.figure.tiff",width = 4,height =5.5,units = "in",res = 150)
p3.cv <- ggplot(tmp3,aes(x=true_n,y=cv,color = stat))+
  facet_wrap(~n,ncol=1,strip.position = "right")+
  geom_line(size=1)+
  geom_point(size=3)+
  labs(x=expression(N[S]),y="Coefficient of variation",color="Method",tag = "C")+
  #  scale_y_continuous(limits = c(0,1.3),breaks = seq(from = 0, to = 1, by=.25))+
  theme_bw()+
  scale_color_manual(values = c("#1b9e77","#7570b3"))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
#dev.off()
p3.cv

#the finished product
tiff(filename = "Output/RMSE.Bias.CV.figure.tiff",width =11,height = 8,units = "in",res = 150)
grid.arrange(p1.rmse,p2.bias,p3.cv,ncol = 3)
dev.off()
#fin!