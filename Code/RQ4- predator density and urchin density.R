#IMPORTING THE DATA FILE
mydata<- read.csv(file.choose())
View(mydata)

#Glms
glm4.1 <- glm(Total_urchin_density ~ Predator_density, data = mydata)
summary(glm4.1)
#0.867
plot(mydata$Total_urchin_density, mydata$Predator_density)
#checking residuals
resid(glm4.1)
plot(resid(glm4.1))
shapiro.test(resid(glm4.1))
#W = 0.9033, p-value = 0.1484
#normally distributed

glm4.2 <- glm(Average_urchin_size ~ Predator_density, data = mydata)
summary(glm4.2)
# 0.991
plot(mydata$Average_urchin_size, mydata$Predator_density)
#checking residuals
resid(glm4.2)
plot(resid(glm4.2))
shapiro.test(resid(glm4.2))

glm4.3 <- glm(Urchin_diversity ~ Predator_density, data = mydata)
summary(glm4.3)
# 0.689
plot(mydata$Urchin_diversity, mydata$Predator_density)
#checking residuals
resid(glm4.3)
plot(resid(glm4.3))
shapiro.test(resid(glm4.3))


glm4.4 <- glm(Mean_arbacia_density ~ Predator_density, data = mydata)
summary(glm4.4)
#0.3802
plot(mydata$Mean_arbacia_density, mydata$Predator_density)
#checking residuals
resid(glm4.4)
plot(resid(glm4.4))
shapiro.test(resid(glm4.4))

glm4.5 <- glm(Mean_Paracentrotus_density ~ Predator_density, data = mydata)
summary(glm4.5)
#0.765
plot(mydata$Mean_Paracentrotus_density, mydata$Predator_density)
#checking residuals
resid(glm4.5)
plot(resid(glm4.5))
shapiro.test(resid(glm4.5))

glm4.6 <- glm(Mean_sphaerechinus_density ~ Predator_density, data = mydata)
summary(glm4.6)
#0.765
plot(mydata$Mean_sphaerechinus_density, mydata$Predator_density)
#checking residuals
resid(glm4.6)
plot(resid(glm4.6))
shapiro.test(resid(glm4.6))

#Beta regression
bm4.7 <- betareg(Percent_algae_proportions ~ Predator_density, data = mydata)
summary(bm4.7)
#0.504
#checking residuals
resid(bm4.7)
plot(resid(bm4.7))
shapiro.test(resid(bm4.7))

glm4.8 <- glm(Total_urchin_density ~ Large_predators, data = mydata)
summary(glm4.8)
#0.8594
plot(mydata$Total_urchin_density, mydata$Large_predators)
#checking residuals
resid(glm4.8)
plot(resid(glm4.8))
shapiro.test(resid(glm4.8))

#IMPORTING THE NEW FISH DATA FILE
mydata<- read.csv(file.choose())
View(mydata)

glm4.9 <- glm(Total_urchin_density ~ D_vulgaris_total_density, data = mydata)
summary(glm4.9)
#0.911
plot(mydata$Total_urchin_density, mydata$D_vulgaris_total_density)
#checking residuals
resid(glm4.9)
plot(resid(glm4.9))
shapiro.test(resid(glm4.9))

glm4.1.1 <- glm(Total_urchin_density ~ D_annularis_total_density, data = mydata)
summary(glm4.1.1)
#0.71858
plot(mydata$Total_urchin_density, mydata$D_annularis_total_density)
#checking residuals
resid(glm4.1.1)
plot(resid(glm4.1.1))
shapiro.test(resid(glm4.1.1))

glm4.1.2 <- glm(Total_urchin_density ~ C_julis_total_density, data = mydata)
summary(glm4.1.2)
# 0.731
plot(mydata$Total_urchin_density, mydata$C_julis_total_density)
#checking residuals
resid(glm4.1.2)
plot(resid(glm4.1.2))
shapiro.test(resid(glm4.1.2))

glm4.1.3 <- glm(Total_urchin_density ~ S_aurata_total_density, data = mydata)
summary(glm4.1.3)
# 0.43369 
plot(mydata$Total_urchin_density, mydata$S_aurata_total_density)
#checking residuals
resid(glm4.1.3)
plot(resid(glm4.1.3))
shapiro.test(resid(glm4.1.3))

#checking AICc
library(MuMIn)
AICc(glm4.1, glm4.2, glm4.3, glm4.4, glm4.5, glm4.6, bm4.7, glm4.8, glm4.9, glm4.1.1, glm4.1.2, glm4.1.3)
#         df      AICc
#glm4.1    3  89.12958
#glm4.2    3  95.79842
#glm4.3    3 -33.81106
#glm4.4    3  25.71097
#glm4.5    3 -16.82626
#glm4.6    3  33.83659
#bm4.7     3   6.02179
#glm4.8    3  89.12524
#glm4.9    3  89.14874
#glm4.1.1  3  89.00344
#glm4.1.2  3  89.01827
#glm4.1.3  3  88.40621


#checking the liklihood ratio test-NOT FINISHED
TUD <- c(1.333333333, 6,
         2.933333333,
         2.666666667,
         0.933333333,
         0.4,
         5.866666667,
         1.333333333,
         4,
         11.06666667,
         1.066666667,
         5.6,
         4.533333333)

#plotting predatory fish density against urchin density
par(mar=c(5,6,4,1)+.1)
plot(TUD, mydata$Predator_density,
     ylab=expression(paste("Predator density (Fish per ",  m^{3}, ")")), xlab=expression(paste("Sea urchin density (Urchins per ",  m^{3}, ")")), pch = 16, col= "black", ylim = c(0,0.5), xlim = c(0,12))


#changing one of the glms for urchin size to include urchin size and density
glm4.2.1 <- glm(Total_urchin_density ~ Predator_density + Average_urchin_size, data = mydata)
summary(glm4.2.1)

