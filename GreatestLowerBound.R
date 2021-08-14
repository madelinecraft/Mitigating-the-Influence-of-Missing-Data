getwd()
setwd("/Users/madelinecraft/Desktop/Pattern Mixture Model Research")
data = read.csv("big5_miss_indicator_withnames.csv", header = T)
head(data)
names(data)

#recode levels from character to numeric
install.packages('plyr')
library('plyr')
data$B1SE6A <- revalue(data$B1SE6A, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6B <- revalue(data$B1SE6B, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6C <- revalue(data$B1SE6C, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6D <- revalue(data$B1SE6D, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6E <- revalue(data$B1SE6E, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6F <- revalue(data$B1SE6F, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6G <- revalue(data$B1SE6G, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6H <- revalue(data$B1SE6H, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6I <- revalue(data$B1SE6I, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6J <- revalue(data$B1SE6J, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6K <- revalue(data$B1SE6K, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6L <- revalue(data$B1SE6L, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6M <- revalue(data$B1SE6M, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6N <- revalue(data$B1SE6N, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6O <- revalue(data$B1SE6O, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6P <- revalue(data$B1SE6P, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6Q <- revalue(data$B1SE6Q, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6R <- revalue(data$B1SE6R, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6S <- revalue(data$B1SE6S, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6T <- revalue(data$B1SE6T, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6U <- revalue(data$B1SE6U, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6V <- revalue(data$B1SE6V, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6W <- revalue(data$B1SE6W, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6X <- revalue(data$B1SE6X, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6Y <- revalue(data$B1SE6Y, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6Z <- revalue(data$B1SE6Z, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6AA <- revalue(data$B1SE6AA, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6BB <- revalue(data$B1SE6BB, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6CC <- revalue(data$B1SE6CC, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6DD <- revalue(data$B1SE6DD, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
data$B1SE6EE <- revalue(data$B1SE6EE, c("A LOT" = "1", "SOME" = "2", "A LITTLE" = "3", "NOT AT ALL" = "4"))
head(data)

#change items from factors to numeric
data[] <- lapply(data, function(x) {
		if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(data, class)

#histograms of items
par(mfrow=c(3, 3))
hist(data$B1SE6A)
hist(data$B1SE6B)
hist(data$B1SE6C)
hist(data$B1SE6D)
hist(data$B1SE6E)
hist(data$B1SE6F)
hist(data$B1SE6G)
hist(data$B1SE6H)
hist(data$B1SE6I)

#reverse coding items
#neuro: moody, worrying, nervous, calm(R)
#extraversion: outgoing, friendly, lively, active, talkative
#openness to experience: creative, imaginative, intelligent, curious, broad-minded, sophisticated, adventurous
#conscientiousness: organized, responsible, hardworking, careless(R), thorough
#agreeableness: helpful, warm, caring, softhearted, sympathetic
#agency: self-confident, forceful, assertive, outspoken, dominant
#**all items except (R) were reverse coded so that 4 reflects a high standing in each dimension
#1/A-outgoing, 2/B-helpful, 3/C-moody, 4/D-organized, 5/E-self-confident, 6/F-friendly, 7/G-warm, 8/H-worrying, 9/I-responsible, 10/J-forceful, 11/K-lively, 12/L-caring, 13/M-nervous, 14/N-creative, 15/O-assertive, 16/P-hardworking, 17/Q-imaginative, 18/R-softhearted, 19/S-CALM, 20/T-outspoken, 21/U-intelligent, 22/V-curious, 23/W-active, 24/X-CARELESS, 25/Y-broad-minded, 26/Z-sympathetic, 27/AA-talkative, 28/BB-sophisticated, 29/CC-adventurous, 30/DD-dominant, 31/EE-thorough
data$B1SE6A <- 5-data$B1SE6A
data$B1SE6B <- 5-data$B1SE6B
data$B1SE6C <- 5-data$B1SE6C
data$B1SE6D <- 5-data$B1SE6D
data$B1SE6E <- 5-data$B1SE6E
data$B1SE6F <- 5-data$B1SE6F
data$B1SE6G <- 5-data$B1SE6G
data$B1SE6H <- 5-data$B1SE6H
data$B1SE6I <- 5-data$B1SE6I
data$B1SE6J <- 5-data$B1SE6J
data$B1SE6K <- 5-data$B1SE6K
data$B1SE6L <- 5-data$B1SE6L
data$B1SE6M <- 5-data$B1SE6M
data$B1SE6N <- 5-data$B1SE6N
data$B1SE6O <- 5-data$B1SE6O
data$B1SE6P <- 5-data$B1SE6P
data$B1SE6Q <- 5-data$B1SE6Q
data$B1SE6R <- 5-data$B1SE6R
#data$B1SE6S <- 5-data$B1SE6S
data$B1SE6T <- 5-data$B1SE6T
data$B1SE6U <- 5-data$B1SE6U
data$B1SE6V <- 5-data$B1SE6V
data$B1SE6W <- 5-data$B1SE6W
#data$B1SE6X <- 5-data$B1SE6X
data$B1SE6Y <- 5-data$B1SE6Y
data$B1SE6Z <- 5-data$B1SE6Z
data$B1SE6AA <- 5-data$B1SE6AA
data$B1SE6BB <- 5-data$B1SE6BB
data$B1SE6CC <- 5-data$B1SE6CC
data$B1SE6DD <- 5-data$B1SE6DD
data$B1SE6EE <- 5-data$B1SE6EE

#neuroticism: C, H, M, S
#extraversion: A, F, K, W, AA
#openness: N, Q, U, V, Y, BB, CC
#conscientiousness: D, I, P, X, EE
#agreeableness: B, G, L, R, Z
#agency: E, J, O, T, DD

###NEUROTICISM###
#create subdata according to the dimensions of the scale
neuro <- data[c("B1SE6C", "B1SE6H", "B1SE6M", "B1SE6S")]

#calculate polychoric covariance matrix
library('psych')
corr_neuro_values <- polychoric(neuro)
corr_neuro_values
corr_neuro =matrix(c(1, .5, .47, .37, .5, 1, .7, .41, .47, .7, 1, .45, .37, .41, .45, 1), nrow=4, ncol=4)
isSymmetric(corr_neuro)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd_neuro <- apply(neuro, 2, sd, na.rm = T)
sd_neuro_matrix <- diag(sd_neuro)

#corr to cov matrix
cov_neuro <- sd_neuro_matrix%*%corr_neuro%*%sd_neuro_matrix

#estimate greatest lower bound
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(corr_neuro)
#wrong: GLB = 0.8293
#correct: 0.8245

install.packages('ltm')
library('ltm')
cronbach.alpha(neuro, na.rm = T)
#alpha = 0.741

###EXTRAVERSION###
#create subdata according to the dimensions of the scale
extra <- data[c("B1SE6A", "B1SE6F", "B1SE6K", "B1SE6W", "B1SE6AA")]

#calculate polychoric covariance matrix
corr_extra_values <- polychoric(extra)
corr_extra_values
corr_extra =matrix(c(1, .59, .6, .38, .6, .59, 1, .54, .34, .46, .6, .54, 1, .55, .48, .38, .34, .55, 1, .25, .6, .46, .48, .25, 1), nrow=5, ncol=5)
isSymmetric(corr_extra)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd_extra <- apply(extra, 2, sd, na.rm = T)
sd_extra_matrix <- diag(sd_extra)

#corr to cov matrix
cov_extra <- sd_extra_matrix%*%corr_extra%*%sd_extra_matrix

#estimate greatest lower bound
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(corr_extra)
#GLB = 0.8641

cronbach.alpha(extra, na.rm = T)
#alpha = 0.759

###OPENNESS###
#create subdata according to the dimensions of the scale
open <- data[c("B1SE6N", "B1SE6Q", "B1SE6U", "B1SE6V", "B1SE6Y", "B1SE6BB", "B1SE6CC")]

#calculate polychoric covariance matrix
corr_open_values <- polychoric(open)
corr_open_values
corr_open =matrix(c(1, .76, .34, .38, .26, .28, .36, .76, 1, .35, .46, .34, .33, .41, .34, .35, 1, .5, .37, .46, .35, .38, .46, .5, 1, .39, .35, .47, .26, .34, .37, .39, 1, .31, .36, .28, .33, .46, .35, .31, 1, .42, .36, .41, .35, .47, .36, .42, 1), nrow=7, ncol=7)
isSymmetric(corr_open)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd_open <- apply(open, 2, sd, na.rm = T)
sd_open_matrix <- diag(sd_open)

#corr to cov matrix
cov_open <- sd_open_matrix%*%corr_open%*%sd_open_matrix

#estimate greatest lower bound
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(corr_open)
#GLB = 0.8850

cronbach.alpha(open, na.rm = T)
#alpha = 0.773

###CONSCIENTIOUSNESS###
#create subdata according to the dimensions of the scale
consc <- data[c("B1SE6D", "B1SE6I", "B1SE6P", "B1SE6X", "B1SE6EE")]

#calculate polychoric covariance matrix
corr_consc_values <- polychoric(consc)
corr_consc_values
corr_consc =matrix(c(1, .49, .37, .31, .55, .49, 1, .58, .32, .53, .37, .58, 1, .24, .47, .31, .32, .24, 1, .29, .55, .53, .47, .29, 1), nrow=5, ncol=5)
isSymmetric(corr_consc)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd_consc <- apply(consc, 2, sd, na.rm = T)
sd_consc_matrix <- diag(sd_consc)

#corr to cov matrix
cov_consc <- sd_consc_matrix%*%corr_consc%*%sd_consc_matrix

#estimate greatest lower bound
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(corr_consc)
#old: GLB = 0.8339
#new: GLB = 0.8188

cronbach.alpha(consc, na.rm = T)
#alpha = 0.677

###AGREEABLENESS###
#create subdata according to the dimensions of the scale
agree <- data[c("B1SE6B", "B1SE6G", "B1SE6L", "B1SE6R", "B1SE6Z")]

#calculate polychoric covariance matrix
corr_agree_values <- polychoric(agree)
corr_agree_values
corr_agree =matrix(c(1, .59, .62, .39, .49, .59, 1, .71, .49, .59, .62, .71, 1, .59, .69, .39, .49, .59, 1, .64, .49, .59, .69, .64, 1), nrow=5, ncol=5)
isSymmetric(corr_agree)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd_agree <- apply(agree, 2, sd, na.rm = T)
sd_agree_matrix <- diag(sd_agree)

#corr to cov matrix
cov_agree <- sd_agree_matrix%*%corr_agree%*%sd_agree_matrix

#estimate greatest lower bound
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(corr_agree)
#GLB = 0.8967
?cronbach.alpha
cronbach.alpha(agree, na.rm = T)
#alpha = 0.802

###AGENCY###
#create subdata according to the dimensions of the scale
agency <- data[c("B1SE6E", "B1SE6J", "B1SE6O", "B1SE6T", "B1SE6DD")]

#calculate polychoric covariance matrix
corr_agency_values <- polychoric(agency)
corr_agency_values
corr_agency =matrix(c(1, .38, .54, .39, .38, .38, 1, .61, .55, .62, .54, .61, 1, .58, .6, .39, .55, .58, 1, .55, .38, .62, .6, .55, 1), nrow=5, ncol=5)
isSymmetric(corr_agency)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd_agency <- apply(agency, 2, sd, na.rm = T)
sd_agency_matrix <- diag(sd_agency)

#corr to cov matrix
cov_agency <- sd_agency_matrix%*%corr_agency%*%sd_agency_matrix

#estimate greatest lower bound
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(corr_agency)
#GLB = 0.8651

cronbach.alpha(agency, na.rm = T)
#alpha = 0.806

#export data
write.csv(data, file = "/Users/madelinecraft/Desktop/Pattern Mixture Model Research/big5_r", na = ".")
