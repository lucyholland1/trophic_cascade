#RQ3
# testing the relationship between macroalgal cover and population density of urhcins

#IMPORTING THE DATA FILE
mydata<- read.csv(file.choose())
View(mydata)

#first test for normality using Shapio-wilks
shapiro.test(mydata$Total_urchin_density)
#W = 0.9013, p-value = 0.1394
#p is greater than 0.05, so the data is normally distributed
shapiro.test(mydata$Percent_algae)
#W = 0.93463, p-value = 0.3912
#p is greater than 0.05, so the data is normally distributed
shapiro.test(mydata$Competitor_density)
#W = 0.58468, p-value = 4.844e-05
#p is less than 0.05, so the data is not normally distributed
#will need to check residuals
shapiro.test(mydata$Mean_Arbacia_size)
#W = 0.91226, p-value = 0.3703
#normally distributed
shapiro.test(mydata$Mean_Paracentrotus_size)
#W = 0.94147, p-value = 0.5173
#normally distributed
shapiro.test(mydata$Mean_Sphaerechinus_size)
#W = 0.96589, p-value = 0.8159
#normally distributed 
shapiro.test(mydata$Average_urchin_size)
#W = 0.9732, p-value = 0.9293
#normally distributed

#Plot (use this code for most plots so they are uniform)
par(mar=c(5,5,2,2))
plot(mydata$Total_urchin_density, mydata$Percent_algae, xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.15, ylim=c(0,100), xlim=c(0,22))
#abline(bm1)
# add the Urchin density axis:
axis(side=1, at=seq(0, max(26, na.rm=T), 2), padj=-0.8)
# add the % macroalgae axis:
axis(side=2, at=seq(0, max(100, na.rm=T), 10), hadj=0.8, las=2)
# add in the labels for each axis:
mtext(side=1, expression(paste("Sea urchin density (Urchins per ",  m^{3}, ")")), line=2.8, cex=1.1, font=1)
mtext(side=2, "Macroalgae cover (%)", line=2.8, cex=1.1, font=1)

#///////////////////////////////////////////////////////////////////////////////////////////////////////////
#beta regression
install.packages("glmmTMB")
install.packages("brms")
install.packages("rstan")
library(betareg)
library(plyr)
library(lmtest)
library(glmmTMB)
library(boot)
library(emmeans)
library(brms)
library(ggplot2)
library(mvtnorm)
library(nlme)
library(rstan)

bm1 <- betareg(Percent_algae_proportions ~ Total_urchin_density, data = mydata)
summary(bm1)
#Coefficients (mean model with logit link):
#Estimate Std. Error z value Pr(>|z|)
#(Intercept)           0.24125    0.50551   0.477    0.633
#Total_urchin_density -0.09231    0.05821  -1.586    0.113

#Phi coefficients (precision model with identity link):
#  Estimate Std. Error z value Pr(>|z|)   
#(phi)   2.0288     0.6925    2.93  0.00339 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#Type of estimator: ML (maximum likelihood)
#Log-likelihood: 2.082 on 3 Df
#Pseudo R-squared: 0.1223
#Number of iterations: 20 (BFGS) + 2 (Fisher scoring) 

#suggests that 12% of variability in macroalgae percentage can be explained by urchin density
#Pr(>|z|)  value is very small which suggests this relationship is highly unlikely to have arisen by chance
#SIGNIFICANCE

#with competitor species (not transformed!!)
bm2 <- betareg(Percent_algae_proportions ~ Total_urchin_density + Competitor_density, data = mydata)
summary(bm2)
#Coefficients (mean model with logit link):
#Estimate Std. Error z value Pr(>|z|)  
#(Intercept)            0.57577    0.59381   0.970   0.3322  
#Total_urchin_density  -0.12327    0.06213  -1.984   0.0472 *
#  Competitor_density   -20.86945   23.35343  -0.894   0.3715  

#Phi coefficients (precision model with identity link):
#  Estimate Std. Error z value Pr(>|z|)   
#(phi)   2.1967     0.7624   2.881  0.00396 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#Type of estimator: ML (maximum likelihood)
#Log-likelihood: 2.462 on 4 Df
#Pseudo R-squared: 0.1529
#Number of iterations: 114 (BFGS) + 8 (Fisher scoring) 

#suggests this explains 15% of the variation in macroalgae percentage change?

#plot
par(mar=c(5,5,2,2))
plot(mydata$Total_urchin_density, mydata$Percent_algae, xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.15, ylim=c(0,100), xlim=c(0,25))
abline(bm2)
# add the Urchin density axis:
axis(side=1, at=seq(0, max(25), 2), padj=-0.8)
# add the % macroalgae axis:
axis(side=2, at=seq(0, max(100, na.rm=T), 20), hadj=0.8, las=2)
# add in the labels for each axis:
mtext(side=1, expression(paste("Sea urchin density (Urchins per ",  m^{3}, ")")), line=2.8, cex=1.1, font=1)
mtext(side=2, "Macroalgae cover (%)", line=2.8, cex=1.1, font=1)

#testing residuals
resid(bm1)
plot(resid(bm1))
shapiro.test(resid(bm1))
#W = 0.95577, p-value = 0.6876

resid(bm2)
plot(resid(bm2))
shapiro.test(resid(bm2))
#W = 0.9492, p-value = 0.5862

#testing model fit
install.packages("MuMIn")
library(MuMIn)
AICc(bm1, bm2)
# df     AICc
#bm1  3 4.503446
#bm2  4 8.075237
#suggets bm1 (without competitor species) is better
#liklihood ratio test
lrtest(bm1, bm2)
# #Df LogLik Df  Chisq Pr(>Chisq)
#1   3 2.0816                     
#2   4 2.4624  1 0.7615     0.3828
#this is a large P value, so we dont have evidence that the first model is better than the second model
#ie no significant difference between the models

#///////////////////////////////////////////////////////////////////////////////////////////////////
#glm (seeing if this model is a better fit than a beta regression)
glm1 <- glm(Percent_algae_proportions ~ Total_urchin_density, data = mydata)
summary(glm1)
#Call:
#glm(formula = Percent_algae_proportions ~ Total_urchin_density, 
#    data = mydata)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.3615  -0.2945   0.0682   0.1051   0.5344  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)           0.56097    0.13863   4.047  0.00193 **
#  Total_urchin_density -0.01936    0.01563  -1.239  0.24117   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.09085554)

#Null deviance: 1.13886  on 12  degrees of freedom
#Residual deviance: 0.99941  on 11  degrees of freedom
#AIC: 9.5404

#Number of Fisher Scoring iterations: 2

glm2 <- glm(Percent_algae_proportions ~ Total_urchin_density + Competitor_density, data = mydata)
summary(glm2)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.42564  -0.12184   0.00009   0.16278   0.51114  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)           0.66893    0.16161   4.139  0.00201 **
#  Total_urchin_density -0.02683    0.01645  -1.631  0.13390   
#Competitor_density   -7.78333    6.34458  -1.227  0.24801   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 0.08686781)

#Null deviance: 1.13886  on 12  degrees of freedom
#Residual deviance: 0.86868  on 10  degrees of freedom
#AIC: 9.7179

#Number of Fisher Scoring iterations: 2

#tetsing model fits
AICc(bm1, bm2, glm1, glm2)
#df      AICc
#bm1   3  4.503446
#bm2   4  8.075237
#glm1  3 12.207066
#glm2  4 14.717885

#showing that beta regression is a better fit compared to a glm (lower AICc)

#likelihood ratio test
lrtest(bm1,glm1)
#Model 1: Percent_algae_proportions ~ Total_urchin_density
#Model 2: Percent_algae_proportions ~ Total_urchin_density
#Df  LogLik Df  Chisq Pr(>Chisq)    
#1   3  2.0816                         
#2   3 -1.7702  0 7.7036  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#this is a small P value, so we have evidence that the first model is better than the second model
#ie significant difference between the models
lrtest(bm2,glm2)

#Model 1: Percent_algae_proportions ~ Total_urchin_density + Competitor_density
#Model 2: Percent_algae_proportions ~ Total_urchin_density + Competitor_density
#Df   LogLik Df  Chisq Pr(>Chisq)    
#1   4  2.46238                         
#2   4 -0.85894  0 6.6426  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#this is a small P value, so we have evidence that the first model is better than the second model
#ie significant difference between the models

#//////////////////////////////////////////////////////////////////////////////////////////////////
#testing if urchin size impacts algae cover
bm3 <- betareg(Percent_algae_proportions ~ Total_urchin_density + Average_urchin_size, data = mydata)
summary(bm3)
#Standardized weighted residuals 2:
#Min      1Q  Median      3Q     Max 
#-2.5939 -0.7675  0.2068  0.6937  1.6949 

#Coefficients (mean model with logit link):
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)          -0.31809    2.32947  -0.137    0.891
#Total_urchin_density -0.09011    0.05819  -1.549    0.121
#Average_urchin_size   0.01073    0.04417   0.243    0.808

#Phi coefficients (precision model with identity link):
#  Estimate Std. Error z value Pr(>|z|)   
#(phi)   2.0298     0.6923   2.932  0.00337 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#Type of estimator: ML (maximum likelihood)
#Log-likelihood: 2.114 on 4 Df
#Pseudo R-squared: 0.1305
#Number of iterations: 26 (BFGS) + 1 (Fisher scoring) 

#plot
par(mar=c(5,5,2,2))
plot(mydata$Average_urchin_size, mydata$Percent_algae, xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.15, ylim=c(0,100), xlim=c(28,65))
#abline(bm3)
# add the Urchin density axis:
axis(side=1, at=seq(0, max(64), 2), padj=-0.8)
# add the % macroalgae axis:
axis(side=2, at=seq(0, max(100, na.rm=T), 20), hadj=0.8, las=2)
# add in the labels for each axis:
mtext(side=1, "Average urchin size (mm)", line=2.8, cex=1.1, font=1)
mtext(side=2, "Macroalgae cover (%)", line=2.8, cex=1.1, font=1)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#Species models- to check if one partciular species is driving the barren state
bm4 <- betareg(Percent_algae_proportions ~ transformed_arbacia_density, data = mydata)
summary(bm4)
#p value for Arbacia density is 0.806 so not significant

bm5 <- betareg(Percent_algae_proportions ~ Transformed_paracentrotus_density, data = mydata)
summary(bm5)
#p value for Paracentrotus density is 0.102 so not significant

bm6 <- betareg(Percent_algae_proportions ~ Transformed_sphaerechinus_density, data = mydata)
summary(bm6)
#p value for Paracentrotus density is 0.931 so not significant

options(scipen=999)
#visualisation of species split with a scatter plot-NOT FINISHED
plot(mydata$Percent_algae_proportions, mydata$Transformed_paracentrotus_density,
     xlab=expression(paste("Sea urchin density (Urchins per ",  m^{3}, ")")), ylab="Percentage of Macroalgae (%) ", pch = 16, col= "red",xlim = c(0.001,100), log= "x",ylim = c(0,1),
     points(mydata$Percent_algae_proportions, mydata$Transformed_arbacia_density, pch = 17, col="green"),
     points(mydata$Percent_algae_proportions, mydata$Transformed_sphaerechinus_density, pch = 15 ,col="blue"))

legend("topright", inset=c(-0.3, 0),
       pch = c(19, 17, 15),
       title = "Species",
       c(expression(italic("A.lixula")),expression(italic("P.lividus")),expression(italic("S.granularis"))), 
       col = c("green","red","blue")) 


#par(mar = c(5, 4, 4, 8),                                  # Specify par parameters
#    xpd = TRUE)
# Add a legend to the plot
#legend("top",
#       legend=c("Macroalgae", "Bare Rock"),
#       col =c("white", "grey"), cex=0.8,
#       title="Habitat cover", text.font=4, bg='white')
