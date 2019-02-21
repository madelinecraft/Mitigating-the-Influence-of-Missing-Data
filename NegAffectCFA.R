getwd()
library("gdata")
setwd("/Users/madelinecraft/Desktop")
mydata <- read.xls("FAdata.xls")
mydata[mydata == -9] <- NA
mydata$B2DDAY <- mydata$M2ID <- NULL
head(mydata, 15)

#power analysis
fit1 <- fa(mydata, rotation = "varimax", cor = "poly", use = "pairwise" )
popmodel <- 'NegAff =~ ?*B2DC1 + ?*B2DC2 + ?*B2DC3 + ?*B2DC4 + ?*B2DC5 + ?*B2DC6 + ?*B2DC13 + ?*B2DC14 + ?*B2DC15 + ?*B2DC16 + ?*B2DC17 + ?*B2DC18 + ?*B2DC19 + ?*B2DC20
NegAff ~~ 1*NegAff
B2DC1 ~~ ?*B2DC1
B2DC2 ~~ ?*B2DC2
B2DC3 ~~ ?*B2DC3
B2DC4 ~~ ?*B2DC4
B2DC5 ~~ ?*B2DC5
B2DC6 ~~ ?*B2DC6
B2DC13 ~~ ?*B2DC13
B2DC14 ~~ ?*B2DC14
B2DC15 ~~ ?*B2DC15
B2DC16 ~~ ?*B2DC16
B2DC17 ~~ ?*B2DC17
B2DC18 ~~ ?*B2DC18
B2DC19 ~~ ?*B2DC19
B2DC20 ~~ ?*B2DC20'
myModel <- 'NegAff =~ B2DC1 + B2DC2 + B2DC3 + B2DC4 + B2DC5 + B2DC6 + B2DC13 + B2DC14 + B2DC15 + B2DC16 + B2DC17 + B2DC18 + B2DC19 + B2DC20'
results <- NULL
for (i in 1:1000)	{
	data <- simulateData(popmod, sample.nobs = 2022, auto.fixed.first = F)
	data <- as.data.frame(mydata)
	fit <- sem(model = myModel, data = mydata)
	results <- rbind(results, parameterEstimates(fit1))}
head(results)
mean(results$est[results$lhs == "X1"])
mean(results$pvalue < .05)
#POWER ANALYSIS NOT FINISHED

#obtaining polychoric sample and model implied covariance matrices
mydata[,c("B2DC1", "B2DC2", "B2DC3", "B2DC4", "B2DC5", "B2DC6", "B2DC13", "B2DC14", "B2DC15", "B2DC16", "B2DC17","B2DC18", "B2DC19", "B2DC20")] <- lapply(mydata[,c("B2DC1", "B2DC2", "B2DC3", "B2DC4", "B2DC5", "B2DC6", "B2DC13", "B2DC14", "B2DC15", "B2DC16", "B2DC17","B2DC18", "B2DC19", "B2DC20")], ordered)

library(lavaan)

fit2 <- cfa(myModel, data = mydata, ordered = c("B2DC1", "B2DC2", "B2DC3", "B2DC4", "B2DC5", "B2DC6", "B2DC13", "B2DC14", "B2DC15", "B2DC16", "B2DC17","B2DC18", "B2DC19", "B2DC20"))

inspect(fit2, "sampstat")$cov
inspect(fit2, "sigma")
resid(fit2)$cov

#r vanishing tetrads
install.packages("dagitty")
library('dagitty')
g <- dagitty("dag{{B2DC1 B2DC2 B2DC3 B2DC4 B2DC5 B2DC6 B2DC13 B2DC14 B2DC15 B2DC16 B2DC17 B2DC18 B2DC19 B2DC20} <- x}")
latents(g) <- c("x")

#number of implied tetrads
nrow(vanishingTetrads(g))

#how tetrads distribute across the typology
nrow(vanishingTetrads(g, "within"))
nrow(vanishingTetrads(g, "between"))0
nrow(vanishingTetrads(g, "epistemic"))

#Wishart's test
(localTests(g, mydata, "tetrads"))

#VANISHING TETRADS NOT FINISHED