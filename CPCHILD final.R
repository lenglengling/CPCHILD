setwd("~/Desktop/MAS Courses/STAT 581")  ## Set working directory

## Data cleaning and processing
install.packages('xlsx')
library(xlsx)
rawdata<-read.xlsx("CPCHILD Data.xlsx", sheetName = "Sheet1")
head(rawdata)

## rename variables
colnames(rawdata)[1:101]<-c('ID', 'Level', 'Q1.1', 'Q1.1B', 'Q1.2', 'Q1.2B','Q1.3', 'Q1.3B','Q1.4', 'Q1.4B','Q1.5', 'Q1.5B','Q1.6', 'Q1.6B','Q1.7', 'Q1.7B','Q1.8', 'Q1.8B','Q1.9', 'Q1.9B','Q2.10', 'Q2.10B','Q2.11', 'Q2.11B','Q2.12', 'Q2.12B','Q2.13', 'Q2.13B','Q2.14', 'Q2.14B','Q2.15', 'Q2.15B','Q2.16', 'Q2.16B','Q2.17', 'Q2.17B','Q3.18', 'Q3.18B','Q3.19', 'Q3.19B','Q3.20', 'Q3.20B','Q3.21', 'Q3.21B','Q3.22', 'Q3.22B','Q3.23', 'Q3.23B','Q3.24', 'Q3.24B','Q3.25', 'Q3.25B','Q3.26', 'Q3.26B','Q4.27', 'Q4.28', 'Q4.29', 'Q4.30','Q4.31','Q4.32', 'Q4.33', 'Q5.34', 'Q5.35', 'Q5.36', 'Q6.37','Q7.1', 'Q7.2','Q7.3', 'Q7.4','Q7.5', 'Q7.6','Q7.7', 'Q7.8','Q7.9', 'Q7.10','Q7.11', 'Q7.12','Q7.13', 'Q7.14','Q7.15', 'Q7.16','Q7.17', 'Q7.18','Q7.19','Q7.20','Q7.21', 'Q7.22','Q7.23', 'Q7.24','Q7.25', 'Q7.26','Q7.27', 'Q7.28','Q7.29','Q7.30','Q7.31', 'Q7.32','Q7.33', 'Q7.34','Q7.35', 'Q7.36')

cor(na.omit(rawdata[,c(3,4)]))

## Standardize data to 100-scale
for (i in c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54)) {
  rawdata[, i] <- rawdata[, i]/3*100
}

for (i in c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 55:61, 64)) {
  rawdata[, i] <- rawdata[, i]/6*100
}

for (i in c(37, 39, 41, 43, 45, 47, 49, 51, 53, 62, 63, 65)) {
  rawdata[, i] <- rawdata[, i]/5*100
}
rawdata
summary(rawdata)
write.xlsx(rawdata, "CPCHILD NewData.xlsx")  ## Save the standardized data in Excel sheet in the working directory
save(rawdata, file = "CPCHILD NewData.RData") ## Save the standardized data as R data in the working directory

## Data analysis
data = get(load('CPCHILD NewData.RData')) ## Reload the standardized R data
summary(data)
dim(data)
## Section 1 - 3 have subquestions
## Divide the quesions in section 1 - 3 into two groups: questions and subquestions
sub <- data[, c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54)]  ## subquestions
names(sub)
dim(sub) 
sum(is.na(sub))
sub$sub <- "Yes"
head(sub)
nonsub <- data[, c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53)]  ## questions
names(nonsub)
dim(nonsub)
sum(is.na(nonsub))
nonsub$sub <- "No"
head(nonsub)
ott <- merge(sub, nonsub)
ott$sub <- as.factor(ott$sub)



#### Missing data ####
## 30/76 rows contain missing values 
nonmissing <- na.omit(data)  ## listwise deletion of missing values
dim(nonmissing)
summary(data)
na <- colSums(is.na(data))  # Total counts of missing data for each variable
sum(na)
require(reshape)
md <- melt(na)
md$variable <- row.names(md)
md
summary(md)
nrow(md)
md$sub <- ifelse((substr(md$variable, nchar(md$variable), nchar(md$variable))=="B"), "Yes", "No")
mdnew <- md[-c(1,2,55:101),]
mdnew$sub <- as.factor(mdnew$sub)
summary(mdnew)
sum(mdnew$value)  # total counts of missing data in section 1-3
sum(mdnew$value[mdnew$sub == 'Yes']) # counts of missing data in sub-items section 1-3
sum(mdnew$value[mdnew$sub == 'No']) # counts of missing data in items section 1-3


## Plot the counts of missing data of questions in section 1 to 3
require(RColorBrewer)
g <- ggplot(mdnew, aes(variable, value))
g + geom_bar(stat="identity", width = 0.5, aes(fill=sub)) + 
    scale_y_continuous(breaks=seq(0, 10, 1)) +
    labs(title = 'Bar Chart',
         subtitle = "Number of missing data in each item and sub-item in section 1-3",
         x = "Item", 
         y = "Total Number of missing data") +
    theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
    scale_fill_brewer(palette = "Set1")

# Section 1: Personal Care 
section1 <- data[, c(3:20)]
# Section 2: Positioning, Transferring and Mobility
section2 <- data[, c(21:36)]
# Section 3: Comfort & Emotions
section3 <- data[, c(37:54)]
# Section 4: Communications & Social Interaction
section4 <- data[, c(55:61)]
# Section 5: Health
section5 <- data[, c(62:64)]
# Section 6: Overall Quality of Life
section6 <- data[, c(65)]
# Section 7: Importance of Items to Your Child's Quality of Life
section7 <- data[, c(66:101)]
# Overall: Section 1 to Section 6
overall <- data[, c(3:65)]  

#### Section 7: Relevance Test by Importance Ratings ####
psych::alpha(section7)  ## Section 7 is consistent, alpha = .98
install.packages('matrixStats')
install.packages('GMCM')
library(matrixStats)
mtx7 <- data.matrix(section7)  ## convert dataframe to matrix
mtx7
mean <- colMeans(mtx7, na.rm=TRUE)
sddev <- colSds(mtx7, na.rm=TRUE)
lower <- mean - sddev
upper <- mean + sddev
var = c(1:36)

df2 = data.frame(cbind(var, mean, lower, upper))
df2

my_grob = grobTree(textGrob("Importance Threshold", x=0.8,  y=0.32, hjust=0, gp=gpar(col="red", fontsize=10, fontface="italic")))
ggplot(df2, aes(x=var, y=mean)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  geom_point() + 
  geom_hline(aes(yintercept=2), color = "red") +
  annotation_custom(my_grob) +
  scale_x_continuous(breaks=seq(1, 36)) + 
  coord_cartesian(ylim = c(0, 6)) + 
  scale_y_continuous(breaks=seq(0, 5, 1), sec.axis = dup_axis(name = waiver(), labels = c("0" = "Least important", "1" = "Not very important", "2" = "Slightly important", "3" = "Fairly important", "4" = "Very important", "5" = "Most important"))) +
  labs(title="Mean importance scores with standard deviations", y="Importance score", x="Questionnaire item") + 
  theme_bw() + theme(plot.title=element_text(size=15, 
                                             lineheight=1),  # title
                     axis.title.x=element_text(vjust=0,  
                                               size=12),  # X axis title
                     axis.title.y=element_text(size=12),  # Y axis title
                     axis.text.x=element_text(size=10, 
                                              vjust=0),  # X axis text
                     axis.text.y=element_text(size=10))  # Y axis text

#### Cronbach's alpha to measure internal consistency of items in section 1-6 ####
install.packages('psych')
library(psych)
psych::alpha(overall) # in output, Q5.36 is reversed 
psych::alpha(overall, check.keys=TRUE)  ## Overall is consistent with alpha = .93
overallnew <- overall[, c(1:61, 63)]  # section 1 to section 6 except Q5.36
reverse <- data.frame(overallnew, -overall[,62]) # create a new data frame with negative score of Q5.36
mean.all <- rowMeans(reverse, na.rm=TRUE)  ## Compute the mean of all columns for each row
data.all <- data.frame(data$Level, mean.all)
data.all
data.all$data.Level<-factor(data.all$data.Level, levels=c("T","UL","LL","US","LS"))

# Boxplot
ggplot(data.all, aes(data.Level, mean.all)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(varwidth=T) +  # varwidth: adjusts the width of the boxes to be proportional to the number of observation it contains
  scale_x_discrete(breaks = c('T', 'UL', 'LL', 'US', 'LS'), 
                   labels = c('T\nn=4', 'UL\nn=15', 'LL\nn=39', 'US\nn=12', 'LS\nn=6')) +
  scale_y_continuous(breaks=seq(10, 100, 10)) + 
  coord_cartesian(ylim = c(45, 100)) +
  labs(title="Box Plot", 
       subtitle="Average Standardized Score of all items in Section 1-6 by Levels",
       x="Spina bifida level",
       y="Average Standardized Score (Section 1-6)") +
  theme_bw()

tapply(data.all$mean.all, data.all$data.Level, length)  ## counts of each level
tapply(data.all$mean.all, data.all$data.Level, mean)  ## mean of each level
tapply(data.all$mean.all, data.all$data.Level, sd)  ## SD of each level
tapply(data.all$mean.all, data.all$data.Level, median)  ## median of each level

plot(tapply(data.all$mean.all, data.all$data.Level, mean))  # plot mean of each level
abline(lm(all.num$mean.all~all.num$data.Level))  # add regression line
plot(tapply(data.all$mean.all, data.all$data.Level, median)) # plot median of each level

## histogram to assess normality of all data
ggplot(data.all,aes(data.all$mean.all))+
  geom_histogram(fill='slategray1', color='slategray3', binwidth = 4) +
  labs(title="Histogram", 
       subtitle="Average Standardized Score Grouped by Levels",
       x="Average Standardized Score") +
  theme_bw()

## histogram to assess normality by group 
ggplot(data.all,aes(data.all$mean.all))+
  geom_histogram(fill='slategray1', color='slategray3', binwidth = 6) +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  facet_grid(~data.Level) +
  labs(title="Histogram", 
       subtitle="Average Standardized Score Grouped by Levels",
       x="Average Standardized Score") +
  theme_bw()

## density plot to assess normality
g <- ggplot(data.all, aes(data.all$mean.all))
g + geom_density(aes(fill=factor(data.all$data.Level)), alpha=0.5) + 
  labs(title="Density plot", 
       subtitle="Average Standardized Score Grouped by Levels",
       x="Average Standardized Score",
       fill="Levels")

#### Correlation Analysis ####
require(tibble)
all.num <- as_data_frame(data.all)
all.num$data.Level <- as.numeric(all.num$data.Level)  # Convert spina bifida levels into integers by letting T=1, UL=2, LL=3, US=4, LS=5
all.num

library(magrittr)
library(dplyr)
gd <- all.num %>% 
  group_by(data.Level) %>% 
  summarise(mean.all = mean(mean.all))
gd

ggplot(all.num, aes(x=data.Level, y=mean.all)) + 
  geom_point(color = 'red', alpha = 0.3) +
  geom_point(data = gd) +
  geom_smooth(method="lm", se = F, size = 0.5) +
  labs(title="Scatter Plot with Regression Line", 
       subtitle="Average Standardized Score by Spina Bifida Levels",
       x="Spina Bifida Level",
       y='Overall Score (Average Standardized Score)') +
  theme_bw()

# Spearman rank correlation (non-parametric) assumptions: 1. ordinal; 2. monotonically related
cor.test(x = sort.all$mean.all, y = sort.all$data.Level, 
         data = sort.all,
         method = 'spearman',
         exact=FALSE)  ## rho = 0.3048443 p-value = 0.007416

# Kendall rank correlation (non-parametric)
cor.test(x = sort.all$mean.all, y = sort.all$data.Level, 
         data = sort.all,
         method = 'kendall')  ## tau = 0.2315348 p-value = 0.008468 

#### ANOVA is not preferred here as it does not take the ordering of spina bifida levels into account ####
## ANOVA as linear regression analysis
fit <- lm(data.all$mean.all ~ data.all$data.Level, data = data.all)
summary(fit)
fit$residuals
plot(fit$residuals)
anova(fit)
plot(fit)  ## Diagnostic plots for checking assumptions
## http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R5_Correlation-Regression/R5_Correlation-Regression7.html

## Alternative function for ANOVA
aov.out = aov(data.all$mean.all ~ data.all$data.Level, data = data.all)
summary(aov.out)
TukeyHSD(aov.out)
plot(aov.out)  ## Diagnostic plots for checking assumptions

## Test assumptions ##
# a. Homogeneity of variance
bartlett.test(data.all$mean.all ~ data.all$data.Level, data = data.all)  ## p-value=0.6805, fail to reject null hypothesis of equal variance
## 1-way ANOVA with unequal variances
oneway.test(data.all$mean.all ~ data.all$data.Level, data = data.all) 

# b. Normality test of residuals
proj(aov.out)
residuals <- proj(aov.out)[, 3]
plot(residuals)
shapiro.test(residuals)  ## p-value = 0.02536 reject null hypothesis of normality
## Kruskal-Wallace test is a non-parametric alternative for non-normal residuals
kruskal.test(data.all$mean.all ~ data.all$data.Level, data = data.all) ## p-value = 0.1229 do not reject null hypothesis of normality

# Assess bivariate normality
install.packages('MVN_5.0.tar.gz', repos = NULL, type="source")
install.packages('mvtnorm')
install.packages('pcaPP')
install.packages('quantreg')
install.packages('fpc')
install.packages('mvoutlier')
install.packages('MVN')  ## Requires R version 3.3.0 or higher
library(MVN)
result <- mvn(data = all.num, mvnTest = "mardia")
result  ## bivariate normality holds

## Pearson correlation is not prefered in here as the scores are arbitrarily assigned to spina bifida levels without knowing the actual magnitude of differences between levels ##
## Pearson correlation assumptions: 1. both variables are normally distributed; 2. linearity; 3. homoscedasticity (equal variance); 4. continuous data (or ordinal with equal intervals) 
# cor.test(x = sort.all$mean.all, y = sort.all$data.Level, data = sort.all, method = 'pearson')  ## cor = 0.2855023 p-value = 0.01242 