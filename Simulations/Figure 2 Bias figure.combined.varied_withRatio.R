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
tmp <- read.table(file = "Output/estimation.25_500off.25_500_npar.sr1.ratio.combined.information.txt",header = T,sep = "\t",stringsAsFactors = F)
#tmp$n <- as.character(tmp$n)
table(tmp$n)
table(tmp$true_n)
tmp$off.par.ratio <- tmp$n/tmp$true_n
summary(tmp$n)
head(tmp)

############
### Bias ###
############

head(tmp)
tiff(filename = "Output/Bias.Noff_Ns_ratio.figure.tiff",width = 5.5,height =4,units = "in",res = 150)
tmp %>% 
  gather(key = "stat",value = "pred",chao,jack1) %>% 
  select(-chao.se:-boot.se) %>%
  mutate(bias = pred/true_n) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife")) %>%
  ggplot(aes(x=off.par.ratio,y=bias))+
  facet_wrap(~stat)+
  geom_point(size=1)+
  geom_smooth(se = F)+
  geom_vline(xintercept = 1,linetype=2,size=1,color="darkgrey")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x=expression(log[10](N[off_s]/N[S])),y=expression(log[10](hat(N)[S]/N[S])))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=11),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()

jpeg(filename = "Output/Bias.Noff_Ns_ratio.figure.v2.jpeg",width = 11,height = 8,units = "in",res = 150)
tmp %>% 
  gather(key = "stat",value = "pred",chao,jack1) %>% 
  select(-chao.se:-boot.se) %>%
  mutate(bias = pred/true_n) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife")) %>%
  ggplot(aes(x=off.par.ratio,y=bias))+
  facet_wrap(~stat)+
  geom_point()+
  geom_smooth(se = F)+
  geom_vline(xintercept = 1,linetype=2,size=1,color="darkgrey")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x=expression(N[off_s]/N[S]~"- (log10 scale)"),y=expression(hat(N)[S]/N[S]~"- (log10 scale)"))+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=16,color = "black"),
        strip.text = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18))
dev.off()

jpeg(filename = "Output/Bias.Noff_Ns_ratio.figure.v1.jpeg",width = 8,height = 11,units = "in",res = 150)
tmp %>% 
  gather(key = "stat",value = "pred",chao,jack1) %>% 
  select(-chao.se:-boot.se) %>%
  mutate(bias = pred/true_n) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife")) %>%
  ggplot(aes(x=off.par.ratio,y=bias))+
  facet_wrap(~stat,ncol=1)+
  geom_point()+
  geom_smooth(se = F)+
  geom_vline(xintercept = 1,linetype=2,size=1,color="darkgrey")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  labs(x=expression(N[off_s]/N[S]~"- (log10 scale)"),y=expression(hat(N)[S]/N[S]~"- (log10 scale)"))+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=16,color = "black"),
        strip.text = element_text(size=22),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18))
dev.off()
tmp %>% 
  gather(key = "stat",value = "pred",chao,jack1) %>% 
  select(-chao.se:-boot.se) %>%
  mutate(bias = pred/true_n) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife")) %>%
  filter(off.par.ratio >= 0.80 & off.par.ratio <=.9) %>%
  group_by(stat) %>%
  summarize(avg = mean(bias),
            sds = sd(bias),
            count = n(),
            top = max(bias),
            bot = min(bias),
            ses = sds/sqrt(count))
   becausehead()

#fin!