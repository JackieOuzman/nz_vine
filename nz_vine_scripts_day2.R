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

yld_comp <- read.csv("C:/Users/ouz001/DATASCHOOL/stats/nz_vine/yld_yld_comp_nz_grapes.csv")
yld_comp$year_fac<- factor(yld_comp$year)
yld_comp$vine_fac<- factor(yld_comp$vine)
yld_comp$year_fac <- relevel(yld_comp$year_fac, ref = "2005")
glimpse(yld_comp)

levels(yld_comp$Cropload)[levels(yld_comp$Cropload)=="2c"] <- "2 cane pruned"
levels(yld_comp$Cropload)[levels(yld_comp$Cropload)=="4c"] <- "4 cane pruned"

vine_size_Palette <- c("#000000", "#CC0000", "#66CC00", "#FFFF00", "#0000FF")

yld_comp$vine_size.2005 <- factor(yld_comp$vine_size.2005, c("XS", "S", "M", "L", "XL"))

ggplot(yld_comp, aes(year, Calculated_yield_per_vine_kg_Harvest, colour= year_fac))+
  geom_boxplot()+
  geom_point(alpha=0.1, size= 5)+
  scale_colour_manual(values = c("red", "green", "blue", "yellow"))+
  facet_wrap(. ~ Cropload) +
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007, 2008))+
  labs(title ="Calculated yield vs. year",
       x = "Year",
       y = "Calculated yield per vine in kg", 
       colour = "Year")

ggplot(yld_comp, aes(year, Total_yield_per_vine_kg_Harvest, colour= year_fac))+
  geom_boxplot()+
  geom_point(alpha=0.2, size=5)+
  scale_colour_manual(values = c("red", "green", "blue", "dark green"))+
  facet_wrap(. ~ Cropload) +
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007, 2008))+
  labs(title ="Total yield vs. year",
       x = "Year",
       y = "Total yield per vine in kg", 
       colour = "Year")

ggplot(yld_comp, aes(year, Calculated_yield_per_vine_kg_Harvest, group = Cropload, colour = Cropload))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point()+
  scale_colour_manual(values = c("red", "blue"))+ # this is using default colours
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007, 2008))+
  labs(title ="Calculated yield vs. year",
       x = "Year",
       y = "Calculated yield per vine in kg",
       colour = "Crop load")

ggplot(yld_comp, aes(year, Total_yield_per_vine_kg_Harvest, group = Cropload, colour = Cropload))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point()+
  #geom_point(colour = mycolour)+ #this is same colour for all my points
  #scale_colour_manual(values = vine_size_Palette)+ #this is colours for my geom_point of geom_line from the defined above
  scale_colour_manual(values = c("red", "blue"))+ # this is using default colours
  theme_bw()+
  scale_x_continuous(breaks=c(2005, 2006, 2007, 2008))+
  labs(title ="Total yield vs. year",
       x = "Year",
       y = "Total yield per vine in kg",
       colour = "Crop load")


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



yld_yr_cropload_lmer_block <- lmer(Calculated_yield_per_vine_kg_Harvest ~ Cropload * year_fac +(1|vine), data=yld_comp)

anova(yld_yr_cropload_lmer_block)
str(anova(yld_yr_cropload_lmer_block))

block_out <- anova(yld_yr_cropload_lmer_block)
block_out$`Pr(>F)`

ggplot(yld_comp, aes(vine_size.2005, Total_number_bunches_per_vineHarvest, colour = year_fac))+
  geom_boxplot(alpha = 0.5)+
  geom_point(alpha=0.6, position = position_jitter(width=0.1))+
  scale_colour_manual(values = c("red", "green", "blue", "yellow"))+
  facet_wrap(year_fac~ Cropload) +
  theme_bw()+
  labs(title = "Bunch number vs vine size",
       x = "vine size",
       y = "number of bunches",
       colour = "year")
