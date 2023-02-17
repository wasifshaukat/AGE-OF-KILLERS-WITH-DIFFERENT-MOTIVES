
######### CREATING UNIQUE DATA SET FOR COURSEWORK############

# unique sample was create and saved for later use

setwd("D:/MS Data Science/MATH 5741 STAT AND METHODS/Coursework")
#load(file = "killersandmotives.Rdata")
#createsample(201589942)
#save(mysample, file = "mysample.RData")
load(file = "mysample.Rdata")


############# CHECKING THE MOTIVES FOR THIS DATA SET ###########################

table(mysample$Motive)
# There are three different motives in this data set
# 1. Angel of Death
# 2. Convenience (Did not want children spouse)
# 3. Escape or avoid arrest
 ########## PREVIOUS RESEARCH #########
# Sigma square = variance = 74
# Meu = mean = 27

############################## DATA CLEANING ###################################

str(mysample)
head(mysample)
mysample[,]
rows = nrow(mysample)
rows
# There are total 71 observations in this Data Set stored in variables rows

# Three data quality issues were spotted, 
# 99999 values in AgeFirstKill column
# NA values in motive
# NA values in sentence 

mysample1 <- mysample[!(mysample$AgeFirstKill == 99999),]
str(mysample1)

#Entries that had 99999 in them
missing_agefirstkill <- sum(mysample$AgeFirstKill == 99999)
missing_agefirstkill

# so there are 9 enteries that needs to be removed because of missing value
perc_agefirstkill = missing_agefirstkill/rows * 100
perc_agefirstkill
# In terms of percentage there are 12.67606% values that are missing 


# Creating Calculated column for removing killers that killed before 1900

mysample1$firstkillyear <- mysample1$AgeFirstKill + mysample1$YearBorn

str(mysample1)

#No. of Entries that are to be removed because the kills were before 1900
kills_before_1900 <- sum(mysample1$firstkillyear< 1900)
kills_before_1900
perc_kills_before_1900 = kills_before_1900/rows *100
perc_kills_before_1900
# In terms of percentage there 1.408451% of values that killed before 1900


mysample2 <- mysample1[!(mysample1$firstkillyear < 1900),]
str(mysample2)

# No of Entries that have no values in column motive
missing_motive <- sum(is.na(mysample2$Motive) == TRUE)
missing_motive

mysample2
mysample5 <- mysample2
# No of Entries that have no values in column Sentence
missing_sentence <- sum(is.na(mysample2$Sentence))
missing_sentence
perc_missing_sentence = missing_sentence/rows * 100
perc_missing_sentence
# In terms of percentage there 2.816901% of values that are missing in Sentence column

mysample3 <- na.omit(mysample2)  #(TO BE DISCUSSED)
mysample3$CareerDuration <- mysample3$AgeLastKill - mysample3$AgeFirstKill
str(mysample3)

mysample4 <- mysample2[!is.na(mysample2$Motive),]
mysample4

mysample5$CareerDuration <- mysample2$AgeLastKill - mysample2$AgeFirstKill
str(mysample5)
mysample5
# The career duration appears to be normal with most of the killers having career less than 5 years
# The career of one individual is 31 years which is surprising.
#########################################
#CHECK ITS IMPACT ON 
max(mysample5$CareerDuration)

View(table(mysample5$Sex,mysample5$Race))

View(table(mysample5$CareerDuration,mysample5$Race))

View(table(mysample5$Sex,mysample5$CareerDuration))

View(table(mysample5$Motive,mysample5$AgeFirstKill))


mean(mysample5$AgeFirstKill)
  #### comparing all three motives####################

avg_ageoffirstkill_motiveangelofdeath <- mean (mysample5$AgeFirstKill[which(mysample5$Motive == "Angel of Death")])
var_ageoffirstkill_motiveangelofdeath <- var(mysample5$AgeFirstKill[which(mysample5$Motive == "Angel of Death")])
sd_ageoffirstkill_motiveangelofdeath <- sd(mysample5$AgeFirstKill[which(mysample5$Motive == "Angel of Death")])
avg_ageoffirstkill_motiveangelofdeath
var_ageoffirstkill_motiveangelofdeath
sd_ageoffirstkill_motiveangelofdeath

avg_ageoffirstkill_convenience <- mean (mysample5$AgeFirstKill[which(mysample5$Motive == "Convenience (didn't want children/spouse)")])
var_ageoffirstkill_convenience <- var (mysample5$AgeFirstKill[which(mysample5$Motive == "Convenience (didn't want children/spouse)")])
sd_ageoffirstkill_convenience <- sd(mysample5$AgeFirstKill[which(mysample5$Motive == "Convenience (didn't want children/spouse)")])
avg_ageoffirstkill_convenience
var_ageoffirstkill_convenience
sd_ageoffirstkill_convenience

avg_ageoffirstkill_escape <- mean (mysample5$AgeFirstKill[which(mysample5$Motive == "Escape or avoid arrest")])
var_ageoffirstkill_escape <- var(mysample5$AgeFirstKill[which(mysample5$Motive == "Escape or avoid arrest")])
sd_ageoffirstkill_escape <- sd(mysample5$AgeFirstKill[which(mysample5$Motive == "Escape or avoid arrest")])
avg_ageoffirstkill_escape
var_ageoffirstkill_escape
sd_ageoffirstkill_escape


motive_firstkill_var <- c(var_ageoffirstkill_motiveangelofdeath, var_ageoffirstkill_escape, var_ageoffirstkill_convenience)
motive_firstkill_mean <- c(avg_ageoffirstkill_motiveangelofdeath,avg_ageoffirstkill_escape, avg_ageoffirstkill_convenience)
motive_firstkill_sd <- c(sd_ageoffirstkill_motiveangelofdeath,sd_ageoffirstkill_escape, sd_ageoffirstkill_convenience)
motive <- c("angel of Death", "Escape", "Convenience")
df <- data.frame(motive, motive_firstkill_mean, motive_firstkill_var, motive_firstkill_sd)
df

Firstmurderaverage <- mean(mysample5$AgeFirstKill)
sd_firstmurder <- sd(mysample5$AgeFirstKill)
variance_firstmurder <- var(mysample5$AgeFirstKill)
Firstmurderaverage


# splitting the data according to motive

data_angeldeath <- mysample5[mysample5$Motive == "Angel of Death",]
data_escape <- mysample5[mysample5$Motive == "Escape or avoid arrest",]
data_convenience <- mysample5[mysample5$Motive == "Convenience (didn't want children/spouse)",]

# only dropping rows if all the columns are NA 
data_angeldeath <- data_angeldeath[rowSums(is.na(data_angeldeath)) != ncol(data_angeldeath), ]
data_convenience <- data_convenience[rowSums(is.na(data_convenience)) != ncol(data_convenience), ]
data_escape <- data_escape[rowSums(is.na(data_escape)) != ncol(data_escape), ]

#checking data frames
data_angeldeath
data_escape
data_convenience


max(data_angeldeath$AgeFirstKill)

# numerical summaries

table(data_angeldeath$Sex)
table(data_convenience$Sex)
table(data_escape$Sex)
count(data_angeldeath$Sex)

table(mysample5$Sex)
table(mysample5$Motive)
table(mysample5$Race)
table(mysample5$Race, mysample5$Sex)
hist(mysample5$CareerDuration, freq = TRUE, xlab = "Career Duration, Years", ylab = " No of Cases", labels = TRUE, main = "", cex = 1.5)
View(summary((mysample5$CareerDuration)))
hist(mysample5$AgeFirstKill, freq = TRUE, xlab = "Career Duration, Years", ylab = " No of Cases", labels = TRUE, main = "", cex = 1.5)

table(data_angeldeath$Race)
table(data_convenience$Race)
table(data_escape$Race)

boxplot(AgeFirstKill ~ Motive, data = mysample5, ylab = "Age At First Murder, years")
stripchart(mysample5$AgeFirstKill ~ mysample5$Motive, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = "blue")

boxplot(AgeFirstKill ~ Race, data = data_angeldeath)
boxplot(AgeFirstKill ~ Race, data = data_convenience)
boxplot(AgeFirstKill ~ Race, data = data_escape)
?boxplot

plot (data_angeldeath$AgeFirstKill,data_angeldeath$CareerDuration)
plot (mysample5$AgeFirstKill,mysample5$CareerDuration)

quantile(data_angeldeath$AgeFirstKill, type = 1)

## box plot for all motives with Race 
boxplot(AgeFirstKill ~ Race, data = data_angeldeath)
boxplot(AgeFirstKill ~ Race, data = data_escape)
boxplot(AgeFirstKill ~ Race, data = data_convenience)

boxplot(AgeFirstKill ~ Sex, data = data_angeldeath)
boxplot(AgeFirstKill ~ Race, data = data_escape)
boxplot(AgeFirstKill ~ Race, data = data_convenience)


############################################### MODELLING AND ESTIMATION ###################################
############################################################################################################

# # Plotting histograms of all the motives 

hist(mysample5$AgeFirstKill, freq = FALSE)

hist(data_angeldeath$AgeFirstKill, freq = FALSE, main = "", xlab = "Age at First Kill, years")
x <- seq(from = min(data_angeldeath$AgeFirstKill), to = max(data_angeldeath$AgeFirstKill), by = 0.1)
lines(x, dnorm(x, mean = avg_ageoffirstkill_motiveangelofdeath, sd = sd_ageoffirstkill_motiveangelofdeath), lwd = 2, col = "green")
lines(x,dpois(x, lambda = avg_ageoffirstkill_motiveangelofdeath), lwd = 2, col = "blue")
lines(x,dexp(x, rate = 1/avg_ageoffirstkill_motiveangelofdeath), lwd = 2, col = "red")
legend( x = "topright", legend=c("Exponential Distribution", "Poisson Distribution", "Normal Distribution"),col=c("red", "blue","green"), lty=1:2, cex=0.8)

hist(data_escape$AgeFirstKill, freq = FALSE)
x <- seq(from = min(data_escape$AgeFirstKill), to = max(data_escape$AgeFirstKill), by = 0.1)
lines(x, dnorm(x, mean = avg_ageoffirstkill_escape, sd = sd_ageoffirstkill_escape), lwd = 2, col = "dark green")


hist(data_convenience$AgeFirstKill, freq = FALSE)
x <- seq(from = min(data_convenience$AgeFirstKill), to = max(data_convenience$AgeFirstKill), by = 0.1)
lines(x, dnorm(x, mean = avg_ageoffirstkill_convenience, sd = sd_ageoffirstkill_convenience), lwd = 2, col = "red")

# not matching any frequencies. Lets generate CDFs for each motive

###### CDF Angel of Death ###########
####################################

par(mfrow = c(1,1))

Fn_angeldeath <- ecdf(data_angeldeath$AgeFirstKill)

plot(Fn_angeldeath, verticals = TRUE, pch = NA)
x <- 1:500  
G <- function(x){return(pnorm(x, mean = avg_ageoffirstkill_motiveangelofdeath, sd = sd_ageoffirstkill_motiveangelofdeath))}

lines(x, G(x), col = "red3") # less vertical separation between Fx and Gx suggest normal distribution

# Perform the Kolmogorov-Smirnov test

ks.test(x = data_angeldeath$AgeFirstKill , 
        y = "pnorm", 
        mean = avg_ageoffirstkill_motiveangelofdeath, sd = sd_ageoffirstkill_motiveangelofdeath)

# we fail to reject null hypothesis since P-value is 0.3 which is greater than 5% suggests normal distribution, consistent with previous finding

# Q-Q plots 


qqnorm(data_angeldeath$AgeFirstKill, main = "Angel of Death")

abline(a = avg_ageoffirstkill_motiveangelofdeath, b = sd_ageoffirstkill_motiveangelofdeath, col = "red")
# This does suggest to significant extent that distribution is normal

# SHAPIRO WILK TEST
shapiro.test(data_angeldeath$AgeFirstKill)

# P value is less than 5% suggesting that it is not normal distribution, rejects the null hypothesis

#Chi-squared goodness test
library(nortest)

pearson.test(data_angeldeath$AgeFirstKill)
# fails to reject null hypothese

###### CDF convenience ###########
##################################

Fn_conv <- ecdf(data_convenience$AgeFirstKill)

plot(Fn_conv, verticals = TRUE, pch = NA)

G <- function(x){return(pnorm(x, mean = avg_ageoffirstkill_convenience, sd = sd_ageoffirstkill_convenience))}
lines(x, G(x), col = "red3")

# Perform the Kolmogorov-Smirnov test

ks.test(x = data_convenience$AgeFirstKill , 
        y = "pnorm", 
        mean = avg_ageoffirstkill_convenience, sd = sd_ageoffirstkill_convenience)

# we fail to reject null hypothesis since P-value is 0.7 which is greater than 5% suggests normal distribution, consistent with previous finding

# Q-Q plots 

qqnorm(data_convenience$AgeFirstKill, main = "Convenience")

abline(a = avg_ageoffirstkill_convenience, b = sd_ageoffirstkill_convenience, col = 'red')
# QQ suggesting normality

# SHAPIRO WILK TEST
shapiro.test(data_convenience$AgeFirstKill)

# P value is greater than 5% suggesting that it is distribution, fail to reject the null hypothesis

#Chi-squared goodness test
library(nortest)

pearson.test(data_convenience$AgeFirstKill)

# fails to reject null hypotheses

###### CDF escape ###########
#############################


Fn_escp <- ecdf(data_escape$AgeFirstKill)

plot(Fn_escp, verticals = TRUE, pch = NA)

G <- function(x){return(pnorm(x, mean = avg_ageoffirstkill_escape, sd = sd_ageoffirstkill_escape))}
lines(x, G(x), col = "red3")

# suggests normality

# Perform the Kolmogorov-Smirnov test

ks.test(x = data_escape$AgeFirstKill , 
        y = "pnorm", 
        mean = avg_ageoffirstkill_escape, sd = sd_ageoffirstkill_escape)
# P value is greater than 5% suggesting that it is distribution, fail to reject the null hypothesis

# Q-Q plots 

qqnorm(data_escape$AgeFirstKill, main = "Escape or avoid arrest")

abline(a = avg_ageoffirstkill_escape, b = sd_ageoffirstkill_escape, col = "red")
# QQ suggesting normality

# SHAPIRO WILK TEST
shapiro.test(data_escape$AgeFirstKill)
#suggesting normality

#Chi-squared goodness test
library(nortest)

pearson.test(data_escape$AgeFirstKill)
#suggesting normality

df
c_results <- c("Test","Histogram", "CDF", "K-s test", "Q-Q plots", "Shapiro-Wilk", "Chi-squared")
c_results_angelofdeath <- c("Angel of Death","Not Normal","Normal","Normal","Not Normal","Not Normal","Normal")
c_results_conv <- c("Convenience", "Normal", "Normal","Normal", "Normal","Normal", "Normal" )
c_results_escp <- c("Escape or Avoid arrest","Not Normal", "Normal", "Normal", "Normal","Normal","Normal")
df.results <- data.frame(c_results,c_results_angelofdeath, c_results_conv,c_results_escp)
View(df.results)


##################################################INFERENECE########################################
####################################################################################################
####################################################################################################
length(data_angeldeath$AgeFirstKill)
length(data_convenience$AgeFirstKill)
length(data_escape$AgeFirstKill)

# since my data set is less than 30 i would use T test 
# it would be wrong to assume that sample variance is equal to population variance

############ t-tests############
#############################################
x1 <- data_angeldeath$AgeFirstKill
t.test(x1, alternative = "two.sided", mu = 27, conf.level = 0.95)

x2 <- data_convenience$AgeFirstKill
t.test(x2, alternative = "two.sided", mu = 27, conf.level = 0.95)

x3 <- data_escape$AgeFirstKill
t.test(x3, alternative = "two.sided", mu = 27, conf.level = 0.95)

# performing z test for population mean of 27 


##############################################################################

# Analysis labels for the left side:

analysis = c("Angel of Death (years)", 
             "Convenience 
(didn't want children/spouse) (years)", 
             "Escape or avoid arrest(years)")

# Results of each test (estimated mean, 
# upper CI limit, lower CI limit, p-value):

estimate  =  c(32.36364, 36.5 , 32.95 )             
upper     =  c(36.31404,44.24328, 35.84889)
lower     =  c(28.41323,28.75672, 30.05111)
pval      =  c(0.01018,0.02156, 0.00039 )


# Note that the order of the results in each vector
# must match the order of the labels in the 
# vector "analysis".


# Set the margin widths:

par(mar = c(6,6,1,6))

# Create an empty plot of a suitable 
# size (considering the width of your
# confidence intervals):

plot(x = 0,                                  # One point at (0,0).
     xlim = c(20, 50), ylim=c(0, 5),        # Axis limits.
     type = "n", xaxt = "n", yaxt="n",       # No points, no axes drawn.
     xlab = NULL, ylab= NULL, ann = FALSE,   # No axis labels or numbers.
     bty="n")                                # No box.

# Add a horizontal (side = 1) axis:

axis(side = 1, cex.axis = 1) 

# Add an axis label 4 lines below the axis:

mtext("Average age at first murder between killers with different motives, with 95% confidence interval", 
      side = 1, line = 4) 

# Add some grid lines, preferably lined up
# with the numbers on the horizontal axis:

for(i in c(25, 30, 35, 40,45)){
  
  lines(c(i, i), c(0, 5), lty = 2, col = "gray53")
  
}

lines(c(27, 27), c(0, 5), lty = 20, col = "red1")

# Add labels for each analysis on the left (side = 2) 
# at vertical heights of 1, 2, 3 and 4:

verticalpos = 1:3

mtext(text = analysis,  at = verticalpos, 
      side = 2, line = 5, outer = FALSE, las = 1, adj = 0)
mtext(text = "Motives", 
      side = 2 , at= 4:4, outer = FALSE, las = 1, adj = 0)

# Try changing the "line" option to move these closer to 
# or away from the plotted intervals.

# Plot the four point estimates (centres 
# of the CIs for each analysis): 

points(estimate, verticalpos, pch = 16)



# Plot the four interval estimates:

for(i in 1:4 ){
  
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
  
  lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
  
  lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
  
}

# Now we add numerical results on the right (side = 4), but we 
# need to put them into a nice form first. Note that
# paste() merges words and numbers, and formatC()
# allows us to control the number of decimal places.

est <- formatC(estimate, format='f', digits = 0)

P <- formatC(pval , format = 'f', digits = 4) 

pval <- paste("p =", P)    # Type pval to see what this does.

L <- formatC(lower, format = 'f', digits = 0)
U <- formatC(upper, format = 'f', digits = 0)

interval <- paste("(", L, ", ", U, "),", sep = "")   # Type interval to check.

# Putting it all together:

results <- paste(est, interval, pval)

# Add to the plot:

mtext(text = results, at = verticalpos, 
      side = 4, line = 4, outer = FALSE, las = 1, adj = 1)

# Like a Christmas present, an R 
# plot belongs in a box:

box("inner")


b <- c(log(data_angeldeath$AgeFirstKill))

hist(b, freq = FALSE)
