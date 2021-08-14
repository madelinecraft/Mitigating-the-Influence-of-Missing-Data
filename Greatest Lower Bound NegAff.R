getwd()
setwd("/Users/madelinecraft/Desktop")
data = read.csv("negaff_withnames.csv", header = T)
head(data)
names(data)

#recode levels from character to numeric (here I coded them in reverse order so that a 5 represents a high standing in each item)
install.packages('plyr')
library('plyr')
data$B1SA24A<- revalue(data$B1SA24A, c("NONE OF THE TIME" = "1", "A LITTLE OF THE TIME" = "2", "SOME OF THE TIME" = "3", "MOST OF THE TIME" = "4", "ALL THE TIME" = "5"))
data$B1SA24B<- revalue(data$B1SA24B, c("NONE OF THE TIME" = "1", "A LITTLE OF THE TIME" = "2", "SOME OF THE TIME" = "3", "MOST OF THE TIME" = "4", "ALL THE TIME" = "5"))
data$B1SA24C<- revalue(data$B1SA24C, c("NONE OF THE TIME" = "1", "A LITTLE OF THE TIME" = "2", "SOME OF THE TIME" = "3", "MOST OF THE TIME" = "4", "ALL THE TIME" = "5"))
data$B1SA24D<- revalue(data$B1SA24D, c("NONE OF THE TIME" = "1", "A LITTLE OF THE TIME" = "2", "SOME OF THE TIME" = "3", "MOST OF THE TIME" = "4", "ALL THE TIME" = "5"))
data$B1SA24E<- revalue(data$B1SA24E, c("NONE OF THE TIME" = "1", "A LITTLE OF THE TIME" = "2", "SOME OF THE TIME" = "3", "MOST OF THE TIME" = "4", "ALL THE TIME" = "5"))
data$B1SA24F<- revalue(data$B1SA24F, c("NONE OF THE TIME" = "1", "A LITTLE OF THE TIME" = "2", "SOME OF THE TIME" = "3", "MOST OF THE TIME" = "4", "ALL THE TIME" = "5"))

#change items from factors to numeric
data[] <- lapply(data, function(x) {
		if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(data, class)

#histograms of items
par(mfrow=c(3, 2))
hist(data$B1SA24A)
hist(data$B1SA24B)
hist(data$B1SA24C)
hist(data$B1SA24D)
hist(data$B1SA24E)
hist(data$B1SA24F)

data2 <- data[c("B1SA24A", "B1SA24B", "B1SA24C", "B1SA24D", "B1SA24E", "B1SA24F")]
#calculate covariance matrix 
install.packages("lavaan")
library("lavaan")
corr_values <- lavCor(data2)
isSymmetric(corr_values)

#calculate a digaonal matrix of standard deviations (for converting from corr to cov)
sd <- apply(data2, 2, sd, na.rm = T)
sd_matrix <- diag(sd)

#corr to cov matrix
cov_values <- sd_matrix%*%corr_values%*%sd_matrix
cov =matrix(c(.46, .24, .25, .28, .31, .26, .24, .68, .42, .21, .32, .21, .25, .42, .69, .22, .35, .22, .28, .21, .22, .40, .30, .28, .31, .32, .35, .30, .78, .32, .26, .21, .22, .28, .32, .43), nrow=6, ncol=6)
isSymmetric(cov)

#estimate greatest lower bound 
install.packages('Rcsdp')
library('Rcsdp')
glb.algebraic(cov)
#GLB = 0.89

library('ltm')
cronbach.alpha(data2, na.rm = T)
#alpha = 0.84

#export data
write.csv(data, file = "/Users/madelinecraft/Desktop/negaff_r", na = ".")
?write.csv
