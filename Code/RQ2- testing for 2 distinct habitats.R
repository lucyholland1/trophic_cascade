#library packages required
install.packages("lme4")
install.packages("car")
install.packages("gridExtra")
install.packages("multcomp")
install.packages("ggplot2")
install.packages("grid")
install.packages("emmeans")
#install.packages("piecewiseSEM")
install.package("nloptr")
install.packages("ggpubr")
#options(repos=c(cran="http://cran.rstudio.com"))
install.packages("shiny")
library("nloptr")
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("grid")
library("gridExtra")
library("emmeans")
#library("piecewiseSEM")
library("ggpubr")
#RQ2
#testing if macroalgal habitats and urchin barrens are two distinct habiats around Silba?

#fistly a t-test for the original urchin habitat types (ie excluding mixed) to compare
#recall a t-test compares the means for two groups
#urchin density between barren and algae sites

#IMPORTING THE URCHIN FILE
mydata<- read.csv(file.choose())
View(mydata)

#first test for normality using Shapio-wilks
shapiro.test(mydata$Total_urchin_density)
#W = 0.88144, p-value = 0.07458
#p is greater than 0.05, so the data is normally distributed

#independent (un-paired) t test
#(do boxplot after)
# Data in two numeric vectors
#WITHOUT MIXED
barren_urchin_density <- c(6,2.933333333, 0.4, 4)
algae_urchin_density <- c(1.333333333, 2.666666667, 0.933333333, 5.866666667, 1.333333333) 
# Create a data frame
#RQ2data <- data.frame(barren_urchin_density,algae_urchin_density)
#print(RQ2data)
t.test(barren_urchin_density, algae_urchin_density, alternative = "two.sided", var.equal = FALSE)
#t = 0.61328, df = 6.0702, p-value = 0.5619
#95 percent confidence interval:
#-2.700679  4.514013
#sample estimates:
#  mean of x mean of y 
#3.333333  2.426667
#therefore the p value is more than the significance level alpha =0.05, so the barren urchin density 
#is not significantly different to algae urchin density

#visualisation
boxplot(algae_urchin_density,barren_urchin_density,
        names = c("Algae", "Barren"),
        col = c("white"),
        border = "black",
        ylab = "Urchin density (urchins/m^3)", xlab= "Habitat type",
        ylim = c(0,7),
        horizontal = FALSE,
        notch = FALSE) + theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color = "black"),
          axis.title =  element_text(size = 20),
          axis.text = element_text(size = 18),
          axis.line.y = element_line(color = "black"))

#/////////////////////////////////////////////////////////////////////////////////
#checking with the mixed habitats included
barren_urchin_density2 <- c(12,5.866666667,0.8,7.866666667,20.4,10.4,8.933333333)
algae_urchin_density2 <- c(2.666666667,5.333333333,1.733333333,11.33333333,2.666666667,2)

#first test for normality using Shapio-wilks
shapiro.test(barren_urchin_density2)
#W = 0.95615, p-value = 0.7851
shapiro.test(algae_urchin_density2)
#W = 0.74581, p-value = 0.01812
#p is greater than 0.05, so the data is normally distributed


t.test(barren_urchin_density2, algae_urchin_density2,alternative = "two.sided", var.equal = FALSE)
#t = 1.8979, df = 10.072, p-value = 0.08671
#again, the p-value is more than 0.05, so the urchin densities are not significantly different
#vsualise with a box plot:
#ggplot(barren_urchin_density,algae_urchin_density,aes(x = habitat, y = urchin_density)) + 
#  geom_boxplot()
par(mar=c(5,5,2,2))
boxplot(algae_urchin_density2,barren_urchin_density2,
        names = c("Algae", "Barren"),
        col = c("white"),
        border = "black",
        ylab = expression(paste("Sea urchin density (Urchins per ",  m^{3}, ")")), xlab= "Habitat type",
        ylim = c(0,25),
        horizontal = FALSE,
        notch = FALSE) + theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color = "black", cex=1.1, font=1),
          axis.title =  element_text(size = 20),
          axis.text = element_text(size = 18, face = "bold"),
          axis.line.y = element_line(color = "black"), cex=1.1, font=1)

#/////////////////////////////////////////////////////////////////////////////////
#now to compare algae cover between barren and urchin sites 
#first check for normality
shapiro.test(mydata$Percent_algae)
#W = 0.93463, p-value = 0.3912
#p is greater than 0.05, so the data is normally distributed



#visualising the data
# Box plots
#df <- data.frame (first_column  = c(2.327277704,0.103822627, 1.132903849, 1.955084525, 0.417748849),
#                  second_column = c(-6.618739234,-2.364836127,-1.277174201,-1.315907408,-2.271629594)
#)
#percent algae cover lists (with mixed classified into barren and algae):
algae_percentage_algae <- c(91.1111111,52.5932367,75.6374396 ,87.6,60.2944444,59.125516)
barren_percentage_algae <- c(0.1333333,8.589372,21.8031621,21.15,9.35,27.1171498, 36.6635266)
algae_percentage_rock <- c(7.938889, 46.995169,22.339372, 12.133333, 34.716667,38.722969)
barren_percentage_rock <- c(98.65,91.132367, 78.196838, 72.844444, 64.634365, 61.569807,85.871739)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggpubr")
install.packages("rlang")
#ggboxplot(df, x = "habitat", y = "percentage of macroalgae", 
#          color = "habitat", palette = c("#00AFBB", "#E7B800"),
#          ylab = "percentage of macroalgae", xlab = "habitat type")

boxplot(algae_percentage_algae,barren_percentage_algae,algae_percentage_rock,barren_percentage_rock,
        main = "Percentage Algae and Bare Rock Cover in Barren and Non-barren Habitats",
        names = c("Algae", "Barren", "Algae", "Barren"),
        col = c("white", "white", "grey", "grey"),
        border = "black",
        ylab = "Percentage cover (%)", xlab= "Habitat type",
        ylim = c(0,100),
        horizontal = FALSE,
        notch = FALSE
)
par(mar = c(5, 4, 4, 8),                                  # Specify par parameters
    xpd = TRUE)
# Add a legend to the plot
legend("top",
       legend=c("Macroalgae", "Bare Rock"),
       col =c("white", "grey"), cex=0.8,
       title="Habitat cover", text.font=4, bg='white')

legend("top", inset=.02, title="Habitat Cover Type",
       c("Macroalgae","Bare Rock"), fill=c("white", "grey"), horiz=TRUE, cex=0.8)

#scatter plot with bare rock vs macroalge for each point to show it is a continuous relationship
plot(mydata$Percent_bare_rock, mydata$Percent_algae,
     xlab="Percentage of Bare Rock (%) ", ylab="Percentage of Macroalgae (%) ", pch = ifelse(mydata$Percent_algae >= 50.0, 17, 19), col= ifelse(mydata$Percent_algae >= 50.0, "red","blue"), ylim= c(0,100), xlim = c(0,100))
legend("topright", 
       pch = c(19, 17),
       title = "Habitat",
       c("Barren", "Algae"), 
       col = c("blue", "red")) 
#///////////////////////////////////////////////////////////////////////////////////////////
if(!require(psych)){install.packages("psych")}
if(!require(betareg)){install.packages("betareg")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}

#IMPORTING THE URCHIN FILE
mydata<- read.csv(file.choose())
View(mydata)

model = betareg(Percent_algae_proportions ~ Habitat_type_split ,
                data = mydata)

library(emmeans)

joint_tests(model)
#model term         df1 df2 F.ratio p.value
#Habitat_type_split   1 Inf  40.489  <.0001

summary(model)
#Standardized weighted residuals 2:
#Min      1Q  Median      3Q     Max 
#-3.3843 -0.6929  0.0896  0.9255  1.3965 

#Coefficients (mean model with logit link):
#    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                0.8687     0.3281   2.648  0.00811 ** 
#    Habitat_type_splitBarren  -2.5553     0.5064  -5.046  4.5e-07 ***
    
#    Phi coefficients (precision model with identity link):
#    Estimate Std. Error z value Pr(>|z|)   
#(phi)    6.076      2.338   2.599  0.00936 **
#    ---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#Type of estimator: ML (maximum likelihood)
#Log-likelihood: 8.609 on 3 Df
#Pseudo R-squared: 0.5352
#Number of iterations: 20 (BFGS) + 2 (Fisher scoring) 

plot(fitted(model),
     residuals(model))




