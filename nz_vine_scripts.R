install.packages("emmeans")

install.packages("lmerTest")
install.packages(Hmisc)
library(dplyr)
library(tidyverse)
library(readxl)
library(emmeans)
library(ggplot2)
library(tidyverse)
library(lmerTest)
library(nlme)
library(MASS)
library(hmisc)

jackie <- read.csv("C:/Users/ouz001/DATASCHOOL/stats/subset_vine_prop.csv")
str(jackie)

jackie$year_fac<- factor(jackie$year)
jackie$vine<- factor(jackie$vine)
jackie$Row_.<- factor(jackie$Row_.)
levels(jackie$Row_.)
jackie$vine_size_2005 <- factor(jackie$vine_size_2005, c("XS", "S", "M", "L", "XL"))
#wont run I have turned into factors and still only run?? Look at frans variables

with(jackie,table(year_fac,Cropload, Row_.))

# addapt Hiz$treatment <- relevel(Hiz$treatment, ref = "Tap ")
jackie$year_fac <- relevel(jackie$year_fac, ref = "2005")

str(jackie)
vine_lmer1 <- lmer(yld_kg_per_vine ~ Cropload * year_fac * Row_. +(1|vine), data=jackie)

anova(vine_lmer1)
anova_output_vine_lmer1 <- print(anova(vine_lmer1))

#Try different plots

yld_comp <- read.csv("C:/Users/ouz001/DATASCHOOL/stats/nz_vine/yld_yld_comp_nz_grapes.csv")
str(yld_comp)
yld_comp$year_fac<- factor(yld_comp$year)

yld_comp$vine_fac<- factor(yld_comp$vine)
yld_comp$vine_size.2005 <- factor(yld_comp$vine_size.2005, c("XS", "S", "M", "L", "XL"))

ggplot(yld_comp, aes(vine_size.2005, Total_number_bunches_per_vineHarvest, colour = year_fac))+
  geom_point()+
  facet_wrap(.~ Cropload)

#With the mean dipslayed as black
ggplot(yld_comp, aes(vine_size.2005, Total_number_bunches_per_vineHarvest, colour = year_fac))+
  geom_point(alpha=0.6, position = position_jitter(width=0.2))+
  stat_summary(fun.y = mean, geom = "point", colour = "black")+
  #stat_summary(fun.data = mean_sdl, fun.args = list(mult =1) , 
               #geom = "errorbar", width = 0.2)+
  facet_wrap(.~ Cropload)









#With the mean dipslayed as black + background changed
# yld comp vs vine size

Cropload <- labels_Cropload
levels(yld_comp$Cropload)[levels(yld_comp$Cropload)=="2c"] <- "2 cane pruned"
levels(yld_comp$Cropload)[levels(yld_comp$Cropload)=="4c"] <- "4 cane pruned"

#YES
ggplot(yld_comp, aes(vine_size.2005, Calculated_yield_per_vine_kg_Harvest, colour = year_fac))+
  geom_point(alpha=0.6, position = position_jitter(width=0.2))+
  geom_boxplot(alpha=0.6)+
  #stat_summary(fun.y = mean, geom = "point", colour = "black")+
  scale_colour_manual(values = c("red", "green", "blue"))+
  #stat_summary(fun.data = mean_sdl, fun.args = list(mult =1) , 
  #geom = "errorbar", width = 0.2)+
  facet_wrap(year_fac ~ Cropload) +
  theme_bw()+
  labs(title ="Calculated yield vs vine size",
       x = "vine size",
       y = "Calculated yield per vine kg",
       colour = "year")
str(yld_comp)

ggplot(yld_comp, aes(vine_size.2005, Total_yield_per_vine_kg_Harvest , colour = year_fac))+
  geom_point(alpha=0.6, position = position_jitter(width=0.2))+
  geom_boxplot(alpha=0.6)+
  scale_colour_manual(values = c("red", "green", "blue"))+
  facet_wrap(year_fac ~ Cropload) +
  theme_bw()+
  labs(title ="Total yield vs vine size",
       x = "vine size",
       y = "Total yield per vine in kg",
       colour = "Year")










# see if I can show interaction

ggplot(yld_comp, aes(year, Calculated_yield_per_vine_kg_Harvest, group = Cropload, colour = Cropload))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point()+
  #geom_point(colour = mycolour)+ #this is same colour for all my points
  #scale_colour_manual(values = vine_size_Palette)+ #this is colours for my geom_point of geom_line from the defined above
  scale_colour_manual(values = c("red", "blue"))+ # this is using default colours
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007))+
  labs(title ="Calculated yield vs year",
       x = "year",
       y = "Calculated yield per vine kg",
       colour = "Crop load")



#build a model to look at the relationship bewteen yield year and crop load

yld_yr_cropload_lm_add <- lm(Calculated_yield_per_vine_kg_Harvest ~ Cropload + year_fac, 
                             data = yld_comp)
anova(yld_yr_cropload_lm_add)
lm_add_anova_output <- print(anova(yld_yr_cropload_lm_add))
glimpse(lm_add_anova_output)

lm_add_anova_output$`Sum Sq` <- round(lm_add_anova_output$`Sum Sq`, 2)
lm_add_anova_output$`Mean Sq` <- round(lm_add_anova_output$`Mean Sq`, 2)
lm_add_anova_output$`F value` <- round(lm_add_anova_output$`F value`, 2)                             

#lm_add_anova_output$`Pr(>F)` <- round(lm_add_anova_output$`Pr(>F)`, 10) 

emmeans(yld_yr_cropload_lm_add, pairwise ~ year_fac|Cropload)

plot(yld_yr_cropload_lm_add, which = 1:2)

##Factorial 

yld_yr_cropload_lm_fac <- lm(Calculated_yield_per_vine_kg_Harvest ~ Cropload*year_fac, data = yld_comp)

anova(yld_yr_cropload_lm_fac)
lm_fact_anova_output <- print(anova(yld_yr_cropload_lm_fac))
glimpse(lm_fact_anova_output)

lm_fact_anova_output$`Sum Sq` <- round(lm_fact_anova_output$`Sum Sq`, 2)
lm_fact_anova_output$`Mean Sq` <- round(lm_fact_anova_output$`Mean Sq`, 2)
lm_fact_anova_output$`F value` <- round(lm_fact_anova_output$`F value`, 2)                             

#lm_fact_anova_output$`Pr(>F)` <- round(lm_fact_anova_output$`Pr(>F)`, 10) 

emmeans(yld_yr_cropload_lm, pairwise ~ year_fac|Cropload)



##Factorial with blocking
yld_yr_cropload_lmer <- lmer(Calculated_yield_per_vine_kg_Harvest ~ Cropload * year_fac 
                        +(1|vine), data=yld_comp)
anova(yld_yr_cropload)
emmeans(yld_yr_cropload, pairwise~Cropload|year_fac)
emmeans(yld_yr_cropload, pairwise~year_fac|Cropload)
emmeans(yld_yr_cropload, consec ~year_fac|Cropload)




ggplot(yld_comp, aes(year, Total_yield_per_vine_kg_Harvest, group = Cropload, colour = Cropload))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point()+
  #geom_point(colour = mycolour)+ #this is same colour for all my points
  #scale_colour_manual(values = vine_size_Palette)+ #this is colours for my geom_point of geom_line from the defined above
  scale_colour_manual(values = c("red", "blue"))+ # this is using default colours
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007))+
  labs(title ="Total yield vs year",
       x = "year",
       y = "Total yield per vine kg",
       colour = "Crop load")



ggplot(yld_comp, aes(vine_size.2005, Total_number_bunches_per_vineHarvest, colour = year_fac))+
  geom_point(alpha=0.6, position = position_jitter(width=0.1))+
  stat_summary(fun.y = mean, geom = "point", colour = "black")+
  #stat_summary(fun.data = mean_sdl, fun.args = list(mult =1) , 
  #geom = "errorbar", width = 0.2)+
  scale_colour_manual(values = c("red", "green", "blue"))+
  facet_wrap(.~ Cropload) +
  theme_bw()+
  labs(title = "Bunch number vs vine size",
       x = "vine size",
       y = "number of bunches",
       colour = "year")

ggplot(yld_comp, aes(vine_size.2005, bunch_weight_good_bunches_g_Harvest, colour = Cropload))+
  geom_boxplot(alpha = 0.5)+
  geom_point(alpha = 0.2)+
  scale_colour_manual(values = c("red", "blue"))+
  facet_wrap(Cropload ~ year_fac) +
  theme_bw()+
  labs(title ="Bunch weight yield vs vine size",
       x = "vine size",
       y = "Bunch weight (grams)",
       colour = "Crop load")

ggplot(yld_comp, aes(year, bunch_weight_good_bunches_g_Harvest, colour = Cropload))+
  geom_boxplot(alpha = 0.5)+
  #geom_point()+
  scale_colour_manual(values = c("black", "red", "green", "yellow", "blue"))+
  facet_wrap(. ~ vine_size.2005) +
  scale_x_continuous(breaks=c(2005, 2006, 2007))+
  theme_bw()+
  labs(title ="Bunch weight yield vs vine size",
       x = "Year",
       y = "Bunch weight (grams)",
       colour = "Crop load")

str(yld_comp)


ggplot(yld_comp, aes(vine_size.2005, Berry_weight_harvest, colour = year_fac))+
  geom_point(alpha=0.6, position = position_jitter(width=0.2))+
  stat_summary(fun.y = mean, geom = "point", colour = "black")+
  #stat_summary(fun.data = mean_sdl, fun.args = list(mult =1) , 
  #geom = "errorbar", width = 0.2)+
  scale_colour_manual(values = c("red", "green", "blue"))+
  facet_wrap(.~ Cropload) +
  theme_bw()+
  labs(title ="Berry weight",
       x = "vine size",
       y = "berry weight at harvest",
       colour = "year")


# yld comp vs year
str(yld_comp)

ggplot(yld_comp, aes(year, Calculated_yield_per_vine_kg_Harvest, colour = vine_size.2005))+
  geom_point(alpha=0.8, position = position_jitter(width=0.2))+
  stat_summary(fun.y = mean, geom = "point", colour = "black")+
  #stat_summary(fun.data = mean_sdl, fun.args = list(mult =1) , 
  #geom = "errorbar", width = 0.2)+
  geom_smooth(method =lm, se = FALSE)+
  facet_wrap(.~ Cropload) +
  theme_bw()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  scale_x_continuous(breaks=c(2005, 2006, 2007))+
  labs(title ="Calculated yield vs year",
       x = "year",
       y = "Calculated yield per vine kg",
       colour = "vine size")

ggplot(yld_comp, aes(year, Calculated_yield_per_vine_kg_Harvest, colour = vine_size.2005, group = year))+
  geom_boxplot()+
  facet_wrap(vine_size.2005 ~ Cropload) +
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007))+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  labs(title ="Calculated yield vs year",
       x = "year",
       y = "Calculated yield per vine kg",
       colour = "vine size")



#Try and get colours I want
#Creates one colour for all my points
mycolour <- "#4ABEFF"
vine_size_Palette <- c("#000000", "#CC0000", "#66CC00", "#FFFF00", "#0000FF")
#year_Palette 




