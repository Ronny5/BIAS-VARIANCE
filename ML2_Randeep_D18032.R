library(ggplot2)

f=function(x){
  #x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
df = as.data.frame(sapply(read.csv("Admission_Predict.csv"), as.numeric))

# Number of missing values
sum(is.na(df))
df=data.frame(apply(df,2,f))
str(df)
str(df$Chance.of.Admit)
str(df$GRE.Score)

# Random Sampling into test and train set
set.seed(0)
smp_size <- sample(1:nrow(df),350)
train = df[smp_size, ]
test = df[-smp_size, ]

# Train and Test set is of size 350 & 50 respectively
str(train)
str(test)

# Selecting a random sample from the train for modelling
randsmp <- function(train,ss=20){
rand = sample(1:nrow(train),ss)
s1_train = df[rand, ]
return (s1_train)
}
s1_train = randsmp(train)
str(s1_train)

# Creating an empty dataframe to store Train and test RSS for Degree 1 to 7
result = data.frame(matrix(NA, nrow=7, ncol=3))
names(result) = c("Polynomial_Degree","Train_rss","Test_rss")
result

#===========================
# PART - A                ##
#=============================================================================================
# Fitting Polynomial regression of order 7 on sample of different sizes
#=============================================================================================

# Creating an empty dataframe to store Train and test RSS for Degree 1 to 7
r1 = data.frame(matrix(NA, nrow=7, ncol=3))
names(r1) = c("Sample_Size","Train_rss","Test_rss")
r1

n=1
for (i in c(20,35,50,70,100,200,350)){
  set.seed(0)
  s1_train = randsmp(train,ss=i)
  m <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s1_train)
  m
  r1[n,1] = i
  r1[n,2] = sum(m$residuals^2)
  pred = predict(m, newdata=test)
  r1[n,3] = sum((pred-test$Chance.of.Admit)^2)
  n = n+1
}

pdf('D7_all1.pdf')
plot(r1[,1], r1[,3], xlab = 'Sample Size', ylab = 'Test RSS',main = 'Test RSS vs Sample Size for Order 7 Polynomial  ', pch=19, cex=0.5)
lines(r1[,1], r1[,3],  col='Blue', type='l')
dev.off() 

#===========================
# PART - B                ##
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

set.seed(0)
s1_train = randsmp(train)

ml1 <- lm(Chance.of.Admit ~ GRE.Score, s1_train)
ml1

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 1', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml1)[order(s1_train$GRE.Score)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
result[1,1] = 1
result[1,2] = sum(ml1$residuals^2)
pred1 = predict(ml1, newdata=test)
result[1,3] = sum((pred1-test$Chance.of.Admit)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

ml2 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s1_train)
ml2

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 2', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml2)[order(s1_train$GRE.Score)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
result[2,1] = 2
result[2,2] = sum(ml2$residuals^2)
pred2 = predict(ml2, newdata=test)
result[2,3] = sum((pred2-test$Chance.of.Admit)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

ml3 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3), s1_train)
ml3

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 3', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml3)[order(s1_train$GRE.Score)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
result[3,1] = 3
result[3,2] = sum(ml3$residuals^2)
pred3 = predict(ml3, newdata=test)
result[3,3] = sum((pred3-test$Chance.of.Admit)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

ml4 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4), s1_train)
ml4

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 4', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml4)[order(s1_train$GRE.Score)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
result[4,1] = 4
result[4,2] = sum(ml4$residuals^2)
pred4 = predict(ml4, newdata=test)
result[4,3] = sum((pred4-test$Chance.of.Admit)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

ml5 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5), s1_train)
ml5

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 5', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml5)[order(s1_train$GRE.Score)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
result[5,1] = 5
result[5,2] = sum(ml5$residuals^2)
pred5 = predict(ml5, newdata=test)
result[5,3] = sum((pred5-test$Chance.of.Admit)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

ml6 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5)+ I(GRE.Score^6), s1_train)
ml6

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 6', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml6)[order(s1_train$GRE.Score)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
result[6,1] = 6
result[6,2] = sum(ml6$residuals^2)
pred6 = predict(ml6, newdata=test)
result[6,3] = sum((pred6-test$Chance.of.Admit)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

ml7 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s1_train)
ml7

#PLOTTING THE MODEL OVER THE DATA
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 7', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml7)[order(s1_train$GRE.Score)], col='red', type='l')


#TRAIN AND TEST ACCURACY
result[7,1] = 7
result[7,2] = sum(ml7$residuals^2)
pred7 = predict(ml7, newdata=test)
result[7,3] = sum((pred7-test$Chance.of.Admit)^2)

# Showing the train and test RSS
View(result)

#=============================================================================================
# Linear Regression plot for model 1 to 7
#=============================================================================================

pdf('D1-7.pdf')
plot(s1_train$GRE.Score, s1_train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression of order 1 to 7', pch=19, cex=0.5)
lines(sort(s1_train$GRE.Score), fitted(ml1)[order(s1_train$GRE.Score)],  col='red', type='l')
lines(sort(s1_train$GRE.Score), fitted(ml2)[order(s1_train$GRE.Score)], col='green', type='l')
lines(sort(s1_train$GRE.Score), fitted(ml3)[order(s1_train$GRE.Score)], col=118, type='l')
lines(sort(s1_train$GRE.Score), fitted(ml4)[order(s1_train$GRE.Score)], col='black', type='l')
lines(sort(s1_train$GRE.Score), fitted(ml5)[order(s1_train$GRE.Score)], col=505, type='l')
lines(sort(s1_train$GRE.Score), fitted(ml6)[order(s1_train$GRE.Score)], col=454, type='l')
lines(sort(s1_train$GRE.Score), fitted(ml7)[order(s1_train$GRE.Score)], col='blue', type='l')
# Add a legend
legend(x =325,y =0.6,
       legend = c("Deg 1","Deg 2","Deg 3","Deg 4","Deg 5","Deg 6","Deg 7"),
       col = c('red','green',118,'black',505,454,'blue'),
       lty=1,
       cex = 0.9)
dev.off() 

#===========================
# PART - C                ##
#=============================================================================================
# Polynomial Regression of degree 1,2,7,8,9,10 for 4 data subsets of size 20.
#=============================================================================================

# Creating an empty dataframe to store Train and test RSS for Degree 1,2,7,8,9,10
r4 = data.frame(matrix(NA, nrow=7, ncol=4))
names(r4) = c("Polynomial_Degree","Subset","Train_rss","Test_rss")
r4

# Sample Subset 1
set.seed(100)
s1 = randsmp(train)

# Sample Subset 2
set.seed(200)
s2 = randsmp(train)

# Sample Subset 3
set.seed(300)
s3 = randsmp(train)

# Sample Subset 4
set.seed(400)
s4 = randsmp(train)


# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#====================================================

ml11 <- lm(Chance.of.Admit ~ GRE.Score, s1)
ml11
ml12 <- lm(Chance.of.Admit ~ GRE.Score, s2)
ml12
ml13 <- lm(Chance.of.Admit ~ GRE.Score, s3)
ml13
ml14 <- lm(Chance.of.Admit ~ GRE.Score, s4)
ml14

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression for sample 1', pch=19, cex=0.1, type='n')
lines(sort(s1$GRE.Score), fitted(ml11)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml12)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml13)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml14)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 1-S1","Ord 1-S2","Ord 1-S3","Ord 1-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)

#TRAIN AND TEST ACCURACY for Subset 1
r4[1,1:2] = c(1,1)
r4[1,3] = sum(ml1$residuals^2)
pred1 = predict(ml11, newdata=test)
r4[1,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r4[2,1:2] = c(1,2)
r4[2,3] = sum(ml1$residuals^2)
pred2 = predict(ml12, newdata=test)
r4[2,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r4[3,1:2] = c(1,3)
r4[3,3] = sum(ml1$residuals^2)
pred3 = predict(ml13, newdata=test)
r4[3,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r4[4,1:2] = c(1,4)
r4[4,3] = sum(ml1$residuals^2)
pred4 = predict(ml14, newdata=test)
r4[4,4] = sum((pred1-test$Chance.of.Admit)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#====================================================

ml21 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s1)
ml21
ml22 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s2)
ml22
ml23 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s3)
ml23
ml24 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s4)
ml24

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 2', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml21)[order(s1$GRE.Score)], col='red', type='l') 
lines(sort(s2$GRE.Score), fitted(ml22)[order(s2$GRE.Score)], col='blue', type='l') 
lines(sort(s3$GRE.Score), fitted(ml23)[order(s3$GRE.Score)], col='118', type='l') 
lines(sort(s4$GRE.Score), fitted(ml24)[order(s4$GRE.Score)], col='505', type='l') 
legend(x =325,y =0.6,
       legend = c("Ord 2-S1","Ord 2-S2","Ord 2-S3","Ord 2-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r4[5,1:2] = c(2,1)
r4[5,3] = sum(ml21$residuals^2)
pred1 = predict(ml21, newdata=test)
r4[5,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r4[6,1:2] = c(2,2)
r4[6,3] = sum(ml22$residuals^2)
pred2 = predict(ml22, newdata=test)
r4[6,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r4[7,1:2] = c(2,3)
r4[7,3] = sum(ml23$residuals^2)
pred3 = predict(ml23, newdata=test)
r4[7,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r4[8,1:2] = c(2,4)
r4[8,3] = sum(ml24$residuals^2)
pred4 = predict(ml24, newdata=test)
r4[8,4] = sum((pred4-test$Chance.of.Admit)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#====================================================

ml71 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s1)
ml71
ml72 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s2)
ml72
ml73 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s3)
ml73
ml74 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s4)
ml74

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 7', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml71)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml72)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml73)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml74)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 7-S1","Ord 7-S2","Ord 7-S3","Ord 7-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)

#TRAIN AND TEST ACCURACY for Subset 1
r4[9,1:2] = c(7,1)
r4[9,3] = sum(ml71$residuals^2)
pred1 = predict(ml71, newdata=test)
r4[9,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r4[10,1:2] = c(7,2)
r4[10,3] = sum(ml72$residuals^2)
pred2 = predict(ml72, newdata=test)
r4[10,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r4[11,1:2] = c(7,3)
r4[11,3] = sum(ml73$residuals^2)
pred3 = predict(ml73, newdata=test)
r4[11,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r4[12,1:2] = c(7,4)
r4[12,3] = sum(ml74$residuals^2)
pred4 = predict(ml74, newdata=test)
r4[12,4] = sum((pred4-test$Chance.of.Admit)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#====================================================

ml81 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s1)
ml81
ml82 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s2)
ml82
ml83 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s3)
ml83
ml84 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s4)
ml84

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 8', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml81)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml82)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml83)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml84)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 8-S1","Ord 8-S2","Ord 8-S3","Ord 8-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r4[13,1:2] = c(8,1)
r4[13,3] = sum(ml81$residuals^2)
pred1 = predict(ml81, newdata=test)
r4[13,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r4[14,1:2] = c(8,2)
r4[14,3] = sum(ml82$residuals^2)
pred2 = predict(ml82, newdata=test)
r4[14,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r4[15,1:2] = c(8,3)
r4[15,3] = sum(ml83$residuals^2)
pred3 = predict(ml83, newdata=test)
r4[15,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r4[16,1:2] = c(8,4)
r4[16,3] = sum(ml84$residuals^2)
pred4 = predict(ml84, newdata=test)
r4[16,4] = sum((pred4-test$Chance.of.Admit)^2)


# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#====================================================

ml91 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s1)
ml91
ml92 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s2)
ml92
ml93 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s3)
ml93
ml94 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s4)
ml94

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 9', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml91)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml92)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml93)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml94)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 9-S1","Ord 9-S2","Ord 9-S3","Ord 9-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r4[17,1:2] = c(9,1)
r4[17,3] = sum(ml91$residuals^2)
pred1 = predict(ml91, newdata=test)
r4[17,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r4[18,1:2] = c(9,2)
r4[18,3] = sum(ml92$residuals^2)
pred2 = predict(ml92, newdata=test)
r4[18,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r4[19,1:2] = c(9,3)
r4[19,3] = sum(ml93$residuals^2)
pred3 = predict(ml93, newdata=test)
r4[19,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r4[20,1:2] = c(9,4)
r4[20,3] = sum(ml94$residuals^2)
pred4 = predict(ml94, newdata=test)
r4[20,4] = sum((pred4-test$Chance.of.Admit)^2)


# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#====================================================

ml101 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s1)
ml101
ml102 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s2)
ml102
ml103 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s3)
ml103
ml104 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s4)
ml104

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 10', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml101)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml102)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml103)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml104)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 10-S1","Ord 10-S2","Ord 10-S3","Ord 10-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r4[21,1:2] = c(10,1)
r4[21,3] = sum(ml101$residuals^2)
pred1 = predict(ml101, newdata=test)
r4[21,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r4[22,1:2] = c(10,2)
r4[22,3] = sum(ml102$residuals^2)
pred2 = predict(ml102, newdata=test)
r4[22,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r4[23,1:2] = c(10,3)
r4[23,3] = sum(ml103$residuals^2)
pred3 = predict(ml103, newdata=test)
r4[23,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r4[24,1:2] = c(10,4)
r4[24,3] = sum(ml104$residuals^2)
pred4 = predict(ml104, newdata=test)
r4[24,4] = sum((pred4-test$Chance.of.Admit)^2)


#===========================
# PART - D                ##
#=============================================================================================
# Polynomial Regression of degree 1,2,7,8,9,10 for 4 data subsets of size 100.
#=============================================================================================

# Creating an empty dataframe to store Train and test RSS for Degree 1,2,7,8,9,10
r5 = data.frame(matrix(NA, nrow=7, ncol=4))
names(r5) = c("Polynomial_Degree","Subset","Train_rss","Test_rss")
r5

# Sample Subset 1
set.seed(100)
s1 = randsmp(train,ss = 100)

# Sample Subset 2
set.seed(200)
s2 = randsmp(train,ss = 100)

# Sample Subset 3
set.seed(300)
s3 = randsmp(train,ss = 100)

# Sample Subset 4
set.seed(400)
s4 = randsmp(train,ss = 100)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#====================================================

ml11 <- lm(Chance.of.Admit ~ GRE.Score, s1)
ml11
ml12 <- lm(Chance.of.Admit ~ GRE.Score, s2)
ml12
ml13 <- lm(Chance.of.Admit ~ GRE.Score, s3)
ml13
ml14 <- lm(Chance.of.Admit ~ GRE.Score, s4)
ml14

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 1', pch=19, cex=0.1, type='n')
lines(sort(s1$GRE.Score), fitted(ml11)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml12)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml13)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml14)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 1-S1","Ord 1-S2","Ord 1-S3","Ord 1-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)

#TRAIN AND TEST ACCURACY for Subset 1
r5[1,1:2] = c(1,1)
r5[1,3] = sum(ml1$residuals^2)
pred1 = predict(ml11, newdata=test)
r5[1,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r5[2,1:2] = c(1,2)
r5[2,3] = sum(ml1$residuals^2)
pred2 = predict(ml12, newdata=test)
r5[2,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r5[3,1:2] = c(1,3)
r5[3,3] = sum(ml1$residuals^2)
pred3 = predict(ml13, newdata=test)
r5[3,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r5[4,1:2] = c(1,4)
r5[4,3] = sum(ml1$residuals^2)
pred4 = predict(ml14, newdata=test)
r5[4,4] = sum((pred1-test$Chance.of.Admit)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#====================================================


ml21 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s1)
ml21
ml22 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s2)
ml22
ml23 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s3)
ml23
ml24 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2), s4)
ml24

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 2', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml21)[order(s1$GRE.Score)], col='red', type='l') 
lines(sort(s2$GRE.Score), fitted(ml22)[order(s2$GRE.Score)], col='blue', type='l') 
lines(sort(s3$GRE.Score), fitted(ml23)[order(s3$GRE.Score)], col='118', type='l') 
lines(sort(s4$GRE.Score), fitted(ml24)[order(s4$GRE.Score)], col='505', type='l') 
legend(x =325,y =0.6,
       legend = c("Ord 2-S1","Ord 2-S2","Ord 2-S3","Ord 2-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r5[5,1:2] = c(2,1)
r5[5,3] = sum(ml21$residuals^2)
pred1 = predict(ml21, newdata=test)
r5[5,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r5[6,1:2] = c(2,2)
r5[6,3] = sum(ml22$residuals^2)
pred2 = predict(ml22, newdata=test)
r5[6,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r5[7,1:2] = c(2,3)
r5[7,3] = sum(ml23$residuals^2)
pred3 = predict(ml23, newdata=test)
r5[7,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r5[8,1:2] = c(2,4)
r5[8,3] = sum(ml24$residuals^2)
pred4 = predict(ml24, newdata=test)
r5[8,4] = sum((pred4-test$Chance.of.Admit)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#====================================================

ml71 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s1)
ml71
ml72 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s2)
ml72
ml73 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s3)
ml73
ml74 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7), s4)
ml74

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 7', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml71)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml72)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml73)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml74)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 7-S1","Ord 7-S2","Ord 7-S3","Ord 7-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)

#TRAIN AND TEST ACCURACY for Subset 1
r5[9,1:2] = c(7,1)
r5[9,3] = sum(ml71$residuals^2)
pred1 = predict(ml71, newdata=test)
r5[9,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r5[10,1:2] = c(7,2)
r5[10,3] = sum(ml72$residuals^2)
pred2 = predict(ml72, newdata=test)
r5[10,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r5[11,1:2] = c(7,3)
r5[11,3] = sum(ml73$residuals^2)
pred3 = predict(ml73, newdata=test)
r5[11,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r5[12,1:2] = c(7,4)
r5[12,3] = sum(ml74$residuals^2)
pred4 = predict(ml74, newdata=test)
r5[12,4] = sum((pred4-test$Chance.of.Admit)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#====================================================

ml81 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s1)
ml81
ml82 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s2)
ml82
ml83 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s3)
ml83
ml84 <- lm(Chance.of.Admit ~ GRE.Score + I(GRE.Score^2) + I(GRE.Score^3) + I(GRE.Score^4) + I(GRE.Score^5) + I(GRE.Score^6) + I(GRE.Score^7) + I(GRE.Score^8), s4)
ml84

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 8', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml81)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml82)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml83)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml84)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 8-S1","Ord 8-S2","Ord 8-S3","Ord 8-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r5[13,1:2] = c(8,1)
r5[13,3] = sum(ml81$residuals^2)
pred1 = predict(ml81, newdata=test)
r5[13,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r5[14,1:2] = c(8,2)
r5[14,3] = sum(ml82$residuals^2)
pred2 = predict(ml82, newdata=test)
r5[14,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r5[15,1:2] = c(8,3)
r5[15,3] = sum(ml83$residuals^2)
pred3 = predict(ml83, newdata=test)
r5[15,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r5[16,1:2] = c(8,4)
r5[16,3] = sum(ml84$residuals^2)
pred4 = predict(ml84, newdata=test)
r5[16,4] = sum((pred4-test$Chance.of.Admit)^2)


# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#====================================================

ml91 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s1)
ml91
ml92 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s2)
ml92
ml93 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s3)
ml93
ml94 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9), s4)
ml94

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 9', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml91)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml92)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml93)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml94)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 9-S1","Ord 9-S2","Ord 9-S3","Ord 9-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r5[17,1:2] = c(9,1)
r5[17,3] = sum(ml91$residuals^2)
pred1 = predict(ml91, newdata=test)
r5[17,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r5[18,1:2] = c(9,2)
r5[18,3] = sum(ml92$residuals^2)
pred2 = predict(ml92, newdata=test)
r5[18,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r5[19,1:2] = c(9,3)
r5[19,3] = sum(ml93$residuals^2)
pred3 = predict(ml93, newdata=test)
r5[19,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r5[20,1:2] = c(9,4)
r5[20,3] = sum(ml94$residuals^2)
pred4 = predict(ml94, newdata=test)
r5[20,4] = sum((pred4-test$Chance.of.Admit)^2)


# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#====================================================

ml101 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s1)
ml101
ml102 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s2)
ml102
ml103 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s3)
ml103
ml104 <- lm(Chance.of.Admit~GRE.Score+I(GRE.Score^2)+I(GRE.Score^3)+I(GRE.Score^4)+I(GRE.Score^5)+I(GRE.Score^6)+I(GRE.Score^7)+I(GRE.Score^8)+I(GRE.Score^9)+I(GRE.Score^10), s4)
ml104

#PLOTTING THE MODEL OVER THE DATA
plot(train$GRE.Score, train$Chance.of.Admit, xlab = 'GRE.Score', ylab = 'Chance.of.Admit',main = 'Polynomial Regression order 10', pch=19, cex=0.5,type='n')
lines(sort(s1$GRE.Score), fitted(ml101)[order(s1$GRE.Score)], col='red', type='l')
lines(sort(s2$GRE.Score), fitted(ml102)[order(s2$GRE.Score)], col='blue', type='l')
lines(sort(s3$GRE.Score), fitted(ml103)[order(s3$GRE.Score)], col='118', type='l')
lines(sort(s4$GRE.Score), fitted(ml104)[order(s4$GRE.Score)], col='505', type='l')
legend(x =325,y =0.6,
       legend = c("Ord 10-S1","Ord 10-S2","Ord 10-S3","Ord 10-S4"),
       col = c('red','blue',118,505),
       lty=1,
       cex = 0.6)


#TRAIN AND TEST ACCURACY for Subset 1
r5[21,1:2] = c(10,1)
r5[21,3] = sum(ml101$residuals^2)
pred1 = predict(ml101, newdata=test)
r5[21,4] = sum((pred1-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 2
r5[22,1:2] = c(10,2)
r5[22,3] = sum(ml102$residuals^2)
pred2 = predict(ml102, newdata=test)
r5[22,4] = sum((pred2-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 3
r5[23,1:2] = c(10,3)
r5[23,3] = sum(ml103$residuals^2)
pred3 = predict(ml103, newdata=test)
r5[23,4] = sum((pred3-test$Chance.of.Admit)^2)
#TRAIN AND TEST ACCURACY for Subset 4
r5[24,1:2] = c(10,4)
r5[24,3] = sum(ml104$residuals^2)
pred4 = predict(ml104, newdata=test)
r5[24,4] = sum((pred4-test$Chance.of.Admit)^2)



#===========================
# PART - E                ##
#=============================================================================================
# Plotting Train & Test RSS against Complexity
#=============================================================================================

# Test RSS vs Complexity of the model for sample size 20
x = c(1,2,7,8,9,10)

plot(x = x,y = r4[r4$Subset == 1,4], xlab = 'Degree of Polynomial', ylab = 'Test RSS',main = 'Test RSS vs Sample Size for 7 Degree Polynomial', pch=19, cex=0.5, type='n')
lines(x = x,y = r4[r4$Subset == 1,4], col='red', type='l')
lines(x = x,y = r4[r4$Subset == 2,4], col='blue', type='l')
lines(x = x,y = r4[r4$Subset == 3,4], col='118', type='l')
lines(x = x,y = r4[r4$Subset == 4,4], col='505', type='l')

# Test RSS vs Complexity of the model for sample size 100

plot(x = x,y = r5[r5$Subset == 1,4], xlab = 'Degree of Polynomial', ylab = 'Test RSS',main = 'Test RSS vs Sample Size for 7 Degree Polynomial', pch=19, cex=0.5, type='n')
lines(x = x,y = r5[r5$Subset == 1,4], col='red', type='l')
lines(x = x,y = r5[r5$Subset == 2,4], col='blue', type='l')
lines(x = x,y = r5[r5$Subset == 3,4], col='118', type='l')
lines(x = x,y = r5[r5$Subset == 4,4], col='505', type='l')

# Test RMSE vs Complexity of the model for sample size 20

plot(x = x,y = sqrt((r4[r4$Subset == 1,4])/20), xlab = 'Degree of Polynomial', ylab = 'Test RMSE',main = 'Test RSS vs Sample Size for 7 Degree Polynomial', pch=19, cex=0.5, type='n')
lines(x = x,y = sqrt((r4[r4$Subset == 1,4])/20), col='red', type='l')
lines(x = x,y = sqrt((r4[r4$Subset == 2,4])/20), col='blue', type='l')
lines(x = x,y = sqrt((r4[r4$Subset == 3,4])/20), col='118', type='l')
lines(x = x,y = sqrt((r4[r4$Subset == 4,4])/20), col='505', type='l')

# Test RMSE vs Complexity of the model for sample size 100

plot(x = x,y = sqrt((r5[r5$Subset == 1,4])/100), xlab = 'Degree of Polynomial', ylab = 'Test RSS',main = 'Test RSS vs Sample Size for 7 Degree Polynomial', pch=19, cex=0.5, type='n')
lines(x = x,y = sqrt((r5[r5$Subset == 1,4])/100), col='red', type='l')
lines(x = x,y = sqrt((r5[r5$Subset == 2,4])/100), col='blue', type='l')
lines(x = x,y = sqrt((r5[r5$Subset == 3,4])/100), col='118', type='l')
lines(x = x,y = sqrt((r5[r5$Subset == 4,4])/100), col='505', type='l')


plot(x = range(0,10),y = range(0,0.9), xlab = 'Degree of Polynomial', ylab = 'Test RMSE',main = 'Test RSS vs Sample Size for 7 Degree Polynomial', pch=19, cex=0.5, type='n')
lines(x = x,y = sqrt((r4[r4$Subset == 1,4])/20), col='red', type='l')
lines(x = x,y = sqrt((r4[r4$Subset == 1,3])/20), col='blue', type='l')

