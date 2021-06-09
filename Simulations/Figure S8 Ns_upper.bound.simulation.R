#originally created on: August 13 2020, 2020
#by: Nick Sard

#ABOUT: This script was written to plot the upper bound of Chao and Jackknife estimators when 
#applied to a reconstructed pedigree

#loading in libraries
library(tidyverse)

#setting working directory and reading in data
setwd("C:/Users/sard/Google Drive/R/Data analysis/2020/Nspawners rarefaction project/Simulations/")

#load source scripts
# - none -

#making some simulated data
ntot <- 500
df <- data.frame(off = 1:ntot,
                 singletons = 2*(1:ntot))
df$doubletons <- 0
df$parents <- 2* df$off

#using singletons and doubletons to estimate Ns
df$chao <- df$parents + ((df$singletons*(df$singletons-1))/(2*(df$doubletons+1)))
df$jack <- round(df$parents + df$singletons + ((df$off-1)/df$off))
head(df, n =10)
tail(df)

df[df$off %in% c(10,25,30,40,50,65,70,71,72,75,100,250),]
tiff(filename = "Output/Ns_upper_bound.tiff",width = 11,height = 8,units = "in",res = 125)
df %>%
  select(off,chao,jack) %>%
  gather(key="stat",value="val",-off) %>%
  mutate(stat = ifelse(stat == "chao","Chao","Jackknife"))%>%
  ggplot(aes(x=off,y=val,color=stat))+
  geom_point()+
  theme_bw()+
  scale_color_brewer(type = "qual",palette = "Accent")+
  labs(y=expression(N[S]~" Upper bound"),x=expression(N[off_s]),color="")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12,color = "black"),
        strip.text = element_text(size=11),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
dev.off()

#fin!
