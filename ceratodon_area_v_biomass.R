
## R script for Area from image analyses accurately estimates dry-weight biomass of juvenile moss tissue
## Burtscher and List et al.
## Script by W. Burtscher and S. Carey

## R version 3.5.3


#### import data with averages for clonal replicates excluding dead replicates for stats and scatterplot ####
# these data can be found in Table S1.

growth_data_nodead <- read.csv("ceratodon_area_v_biomass_avg.csv")
growth_data_nodead$Week <- as.factor(growth_data_nodead$Week)

# parse data by week
growth_data_nodead_week1 <- subset(growth_data_nodead, Week=="1")
growth_data_nodead_week2 <- subset(growth_data_nodead, Week=="2")
growth_data_nodead_week3 <- subset(growth_data_nodead, Week=="3")


#### import unaveraged data excluding dead replicates for box plots ####
# these data can be found in Table S2.

raw_data <- read.csv("ceratodon_area_v_biomass_raw.csv")
raw_data$Week <- as.factor(raw_data$Week)

# parse data by week
raw_data_week1 <- subset(raw_data, Week=="1")
raw_data_week2 <- subset(raw_data, Week=="2")
raw_data_week3 <- subset(raw_data, Week=="3")
raw_data_week1$Individual = factor(raw_data_week1$Individual,c("Alsk_F","Alsk_M","Chile_F","Chile_M","Dur_F","Dur_M","Ecud_F","Ecud_M","Ren_F","Ren_M"))


#### correlation between area and biomass for each week ####

# week 1
area_biomass_corr_week1 <- cor(growth_data_nodead_week1$Area,growth_data_nodead_week1$Biomass)
area_biomass_corr_week1
# 0.3890323

# week 2
area_biomass_corr_week2 <- cor(growth_data_nodead_week2$Area,growth_data_nodead_week2$Biomass)
area_biomass_corr_week2
# 0.8580126

# week 3
area_biomass_corr_week3 <- cor(growth_data_nodead_week3$Area,growth_data_nodead_week3$Biomass)
area_biomass_corr_week3
# 0.8563432


#### ANOVA of biomass and inoculum size for each week  ####

# week 1
biomass_aov_nodead_week1 <- aov(Biomass ~ Inoculum_Size, data=growth_data_nodead_week1)
summary(biomass_aov_nodead_week1)

#Df Sum Sq Mean Sq F value   Pr(>F)    
#Inoculum_Size  1  2.195   2.195   28.51 4.48e-05 ***
#  Residuals     18  1.386   0.077                     

# week 2
biomass_aov_nodead_week2 <- aov(Biomass ~ Inoculum_Size, data=growth_data_nodead_week2)
summary(biomass_aov_nodead_week2)

#Df Sum Sq Mean Sq F value Pr(>F)
#Inoculum_Size  1   4.43   4.426   2.139  0.161
#Residuals     18  37.25   2.069  

# week 3
biomass_aov_nodead_week3 <- aov(Biomass ~ Inoculum_Size, data=growth_data_nodead_week3)
summary(biomass_aov_nodead_week3)

#Df Sum Sq Mean Sq F value Pr(>F)
#Inoculum_Size  1   8.24   8.240   1.127  0.302
#Residuals     18 131.56   7.309            


#### ANOVA of area and inoculum size for each week  ####

# week 1
area_aov_nodead_week1 <- aov(Area ~ Inoculum_Size, data=growth_data_nodead_week1)
summary(area_aov_nodead_week1)

#Df Sum Sq Mean Sq F value Pr(>F)
#Inoculum_Size  1    3.6    3.61   0.038  0.847
#Residuals     18 1690.3   93.90   

# week 2
area_aov_nodead_week2 <- aov(Area ~ Inoculum_Size, data=growth_data_nodead_week2)
summary(area_aov_nodead_week2)

#Df Sum Sq Mean Sq F value Pr(>F)
#Inoculum_Size  1    839     839   0.208  0.654
#Residuals     18  72463    4026    

# week 3
area_aov_nodead_week3 <- aov(Area ~ Inoculum_Size, data=growth_data_nodead_week3)
summary(area_aov_nodead_week3)

#Df Sum Sq Mean Sq F value Pr(>F)
#Inoculum_Size  1    962     962   0.088   0.77
#Residuals     18 196175   10899 


### plot figures and save in TIFF format ####

# scatterplot of area and biomass for each week of growth 
tiff("Figure1.tiff", units="in", width=8, height=3, res=300)
par(mar=c(4,4,1,1), oma=c(0,0,0,0), mgp=c(4.5,1,0), mfrow=c(1,3))
plot(growth_data_nodead_week1$Area, growth_data_nodead_week1$Biomass,
     font.main = 2, main="(a)",
     xlab = "", 
     ylab = "",
     xlim=c(0,50),
     ylim=c(0,2),
     cex.lab=1, cex.main=1, cex.axis=0.75, 
     pch=1, cex=1, col="black")
abline(lm(growth_data_nodead_week1$Biomass~growth_data_nodead_week1$Area))
title(ylab="Biomass (mg)",line=2, cex.lab=1.25)
plot(growth_data_nodead_week2$Area, growth_data_nodead_week2$Biomass,
     font.main = 2, main="(b)",
     xlab = "",
     ylab= "",
     xlim=c(0,250),
     ylim=c(0,8),
     cex.lab=1, cex.main=1, cex.axis=0.75, 
     pch=1, cex=1, col="black")
abline(lm(growth_data_nodead_week2$Biomass~growth_data_nodead_week2$Area))
title(xlab = expression(Area~(mm^{2})),line=3, cex.lab=1.25)
plot(growth_data_nodead_week3$Area, growth_data_nodead_week3$Biomass,
     font.main = 2, main="(c)",
     xlab = "",
     ylab= "",
     xlim=c(0,450),
     ylim=c(0,15),
     cex.lab=1, cex.main=1, cex.axis=0.75, 
     pch=1, cex=1, col="black")
abline(lm(growth_data_nodead_week3$Biomass~growth_data_nodead_week3$Area))
dev.off()

# boxplot of biomass for each isolate and inoculum size for each week of growth
tiff("Figure2.tiff", units="in", width=8, height=3, res=300)
par(mar=c(2,4,1,1), oma=c(2,0,0,0), mgp=c(4.5,1,0), mfrow=c(1,3))
boxplot(Biomass ~ FLabel, data=raw_data_week1, las=2,
        font.main = 2, main = "(a)",
        col=c('gray90','gray90', 'gray40', 'gray40'),
        ylab="",
        cex.lab=1, cex.main=1, cex.axis=0.75,
        ylim=c(0,20))
stripchart(Biomass ~ FLabel, vertical=TRUE, data=raw_data_week1,
           add=TRUE, pch=1, cex=1, col='black')
title(ylab="Biomass (mg)", line=2, cex.lab=1.25)
boxplot(Biomass ~ FLabel, data=raw_data_week2, las=2,
        font.main = 2, main = "(b)",
        col=c('gray90','gray90', 'gray40', 'gray40'),
        cex.lab=1, cex.main=1, cex.axis=0.75,
        ylim=c(0,20))
stripchart(Biomass ~ FLabel, vertical=TRUE, data=raw_data_week2,
           add=TRUE, pch=1, cex=1, col='black')
boxplot(Biomass ~ FLabel, data=raw_data_week3, las=2,
        font.main = 2, main = "(c)",
        col=c('gray90','gray90', 'gray40', 'gray40'),
        cex.lab=1, cex.main=1, cex.axis=0.75,
        ylim=c(0,20))
stripchart(Biomass ~ FLabel, vertical=TRUE, data=raw_data_week3,
           add=TRUE, pch=1, cex=1, col='black')
dev.off()

# boxplot of area for each isolate and inoculum size for each week of growth
tiff("Figure3.tiff", units="in", width=8, height=3, res=300)
par(mar=c(2,4,1,1), oma=c(2,0,0,0), mgp=c(4.5,1,0), mfrow=c(1,3))
boxplot(Area ~ FLabel, data=raw_data_week1, las=2, 
        font.main = 2, main = "(a)",
        col=c('gray90','gray90', 'gray40', 'gray40'),
        ylab="",
        cex.lab=1, cex.main=1, cex.axis=0.75,
        ylim=c(0,550))
stripchart(Area ~ FLabel, vertical=TRUE, data=raw_data_week1,
           add=TRUE, pch=1, cex=1, col='black')
title(ylab=expression(Area~(mm^{2})), line=2, cex.lab=1.25)
boxplot(Area ~ FLabel, data=raw_data_week2, las=2,
        font.main = 2, main = "(b)",
        col=c('gray90','gray90', 'gray40', 'gray40'),
        cex.lab=1, cex.main=1, cex.axis=0.75,
        ylim=c(0,550))
stripchart(Area ~ FLabel, vertical=TRUE, data=raw_data_week2,
           add=TRUE, pch=1, cex=1, col='black')
boxplot(Area ~ FLabel, data=raw_data_week3, las=2,
        font.main = 2, main = "(c)",
        col=c('gray90','gray90', 'gray40', 'gray40'),
        cex.lab=1, cex.main=1, cex.axis=0.75,
        ylim=c(0,550))
stripchart(Area ~ FLabel, vertical=TRUE, data=raw_data_week3,
           add=TRUE, pch=1, cex=1, col='black')
dev.off()
