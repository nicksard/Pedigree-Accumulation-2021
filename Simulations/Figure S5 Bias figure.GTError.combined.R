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
tmp <- read.table(file = "Output/estimation.npar100_50_500.sex.ratio.parents.combined.WithError.information.txt",header = T,sep = "\t",stringsAsFactors = F)
tmp$n <- as.character(tmp$n)
tmp$type_a <- as.character(tmp$type_a)
tmp$type_b <- as.character(tmp$type_b)
table(tmp$n)
table(tmp$true_n)
head(tmp)

############
### Bias ###
############

head(tmp)
tmp2 <- tmp %>% 
  select(n,Species,chao,jack1,true_n,errors,type_a,type_b) %>%
  gather(key = "stat",value = "pred",chao:jack1) %>%
  group_by(true_n,n,stat,errors,type_a,type_b) %>%
  summarize(count = n(),
            pred.mean = mean(pred),
            true.mean = mean(true_n),
            bias = pred.mean/true.mean)
head(tmp2)
tmp2 %>% as.data.frame()
tmp2$n <- as.character(tmp2$n)
tmp2$type <- "Parents combined"
tmp2$stat <- ifelse(test = tmp2$stat == "chao", "Chao","Jackknife")
tmp2$type_a <- paste("a =",tmp2$type_a)
tmp2$type_b <- paste("b =",tmp2$type_b)
head(tmp2)
table(tmp2$true_n)
tmp2$g2 <- paste0(tmp2$errors,tmp2$stat)

#re-leveling
tmp2$n <- factor(tmp2$n,levels = c("50","500"))

tiff(filename = "Output/Bias.combined.WithError.figure.tiff",width = 5.5,height = 4,units = "in",res = 150)
ggplot(tmp2,aes(x=n,y=bias,color = stat,shape=errors,group=g2))+
  facet_grid(type_a~type_b,scales = "free_x")+
  geom_hline(yintercept = 1,size=1,linetype=2,color="darkgrey")+
  geom_line(size=.75)+
  geom_point(size=2)+
  labs(x=expression(N[off_s]),y=expression(Bias~(hat(N)[S]/N[S])),color="Method",shape="Errors")+
  theme_bw()+
  scale_color_manual(values = c("#252525","#969696"))+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=10,color = "black"),
        strip.text = element_text(size=9),
        legend.position = "none")
dev.off()
head(tmp1)
head(tmp2)

###############
### boxplot ###
###############
tmp3 <- tmp %>% 
  select(n,Species,chao,jack1,true_n,errors,type_a,type_b) %>%
  gather(key = "stat",value = "pred",chao:jack1) %>%
  group_by(true_n,n,stat,errors,type_a,type_b) %>%
  mutate(ratio = pred/true_n)
head(tmp3)

#fixing some things up
tmp3$stat <- ifelse(test = tmp3$stat == "chao", "Chao","Jackknife")
tmp3$type_a <- paste("a =",tmp3$type_a)
tmp3$type_b <- paste("b =",tmp3$type_b)
head(tmp3)
table(tmp3$true_n)
tmp3$g2 <- paste0(tmp3$errors,tmp3$stat)
tmp3$both <- paste(tmp3$type_a,tmp3$type_b,sep=", ")
head(tmp3)

tiff(filename = "Output/Boxplot.Bias.combined.WithError.figure.tiff",width = 5, height = 7,units = "in",res = 150)
ggplot(tmp3, aes(x=n,y=ratio,fill=errors))+
  facet_grid(both~stat)+
  geom_hline(yintercept = 1,size=1,linetype=2,color="darkgrey")+
  geom_boxplot()+
  labs(x=expression(N[off_s]),y=expression(hat(N)[S]/N[S]),color="Method",shape="Errors")+
  theme_bw()+
  scale_fill_manual(values = c("#a6cee3","#1f78b4"))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10,color = "black"),
        strip.text = element_text(size=10),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()
#fin!