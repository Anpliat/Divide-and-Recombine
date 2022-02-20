###############################################################################################
#################################  Random replicate division  #################################
###############################################################################################

## Install 'datadr' package
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/datadr/datadr_0.8.6.tar.gz"
# install.packages(packageurl, repos=NULL, type="source", lib='C:/../R/win-library/4.0/')

#### Install 'trelliscope' package (from Github)
# devtools::install_github("delta-rho/trelliscope")

library(datadr)
data<- read.csv("C:/../new_project_data.csv", header=TRUE, stringsAsFactors=TRUE)
head(data)
str(data)

### contrasts will show you how it will work in the regression model
contrasts(data$SUBSCRIBED)

### separate the response from the predictors
y<-data$SUBSCRIBED
data_without_y<-data[,-19]

#### update: should a MapReduce job be run to obtain additional attributes for the result data prior to returning?

## Distributed Data Frames (ddr)
# initialize ddf from a data frame
# turn data into a ddf
data_ddr <- ddf(data, update = TRUE)
summary(data_ddr)
names(data_ddr)

#####################################################################
####################  Fitting a GLM to the data  ####################
#####################################################################

model<- glm(as.factor(y)~., data = data_without_y, family = binomial(link = "logit"))  ## family = binomial()
summary(model)
summary(model)$coefficients
names(model$coefficients)

## Without separating dependent variable form data (same results)
model32<- glm(as.factor(SUBSCRIBED)~., data = data, family = binomial(link = "logit"))  ## family = binomial()
summary(model32)
summary(model32)$coefficients

library(aod)
wald.test(b = coef(model), Sigma = vcov(model), Terms = 3:4)    # for job
wald.test(b = coef(model), Sigma = vcov(model), Terms = 5:7)    # for marital
wald.test(b = coef(model), Sigma = vcov(model), Terms = 8:12)   # for education
wald.test(b = coef(model), Sigma = vcov(model), Terms = 16:18)  # for seasons
wald.test(b = coef(model), Sigma = vcov(model), Terms = 19:22)  # for day_of_week
wald.test(b = coef(model), Sigma = vcov(model), Terms = 26:27)  # for poutcome

### GOF test (Goodness of fit)
pchisq(model$deviance, model$df.residual, lower.tail=FALSE)  
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
null.model<- glm(y~1, data = data, family = binomial(link = "logit"))
anova(null.model, model, test="Chisq")  # apor.Ho

library(pscl)
pR2(model)

library(sjPlot)
plot_model(model32, show.values = FALSE, value.offset = .3)

library(jtools)
library(ggstance)
plot_summs(model32)

#################################################################
#####################  10 splits (subsets)  #####################
#################################################################

nrow(data_ddr)/10
round(nrow(data_ddr)/10,0)

set.seed(127b)

rsubs <- divide(data_ddr, by = rrDiv(3893), update = TRUE)
rsubs

# plot distribution of the number of rows in each subset
library(ggplot2)
qplot(y = splitRowDistn(rsubs),
      xlab = "percentile", ylab = "number of rows in subset", 
      main='Distribution of the number of rows in each of the 10 subsets')


## Keys for random replicate divided data are simply labels indicating the bin
head(getKeys(rsubs))

nrow(rsubs[[1]][[2]])
nrow(rsubs[[2]][[2]])
nrow(rsubs[[3]][[2]])
nrow(rsubs[[4]][[2]])
nrow(rsubs[[5]][[2]])
nrow(rsubs[[6]][[2]])
nrow(rsubs[[7]][[2]])
nrow(rsubs[[8]][[2]])
nrow(rsubs[[9]][[2]])
nrow(rsubs[[10]][[2]])

########################################################################
#####################  Fitting a GLM with drGLM()  #####################
########################################################################

## Αpply a drGLM() transformation to rsubs and then call recombine() on the result
subs_glm <- addTransform(rsubs, function(x)
  drGLM(as.factor(SUBSCRIBED)~., data = x, family = binomial(link = "logit")))

tem1<-recombine(subs_glm, combMeanCoef)
tem1

########################################################################
#####################  Fitting a GLM with drBLB()  #####################
########################################################################

# Αdd bag of little bootstraps transformation

# Specifying the:
# statistic to be computed for each bootstrap sample
# metric: to compute on the statistics
# R: number of bootstrap replications
# We also need to tell it the total number of rows in the data set


##### => for 90% confidence Interval
subs_blb90 <- addTransform(rsubs, function(x) {
  drBLB(x,
        statistic = function(x, weights)
          coef(glm(as.factor(SUBSCRIBED)~., data = x, weights = weights, family = binomial(link = "logit"))),
        metric = function(x)
          quantile(x, c(0.05, 0.95)), R = 100, n = nrow(rsubs)
  )
})

# compute the mean of the resulting CI limits
coefs90 <- recombine(subs_blb90, combMean)
matrix(coefs90, ncol = 2, byrow = TRUE)


##### => for 95% confidence Interval
subs_blb <- addTransform(rsubs, function(x) {
  drBLB(x,
        statistic = function(x, weights)
          coef(glm(as.factor(SUBSCRIBED)~., data = x, weights = weights, family = binomial(link = "logit"))),
        metric = function(x)
          quantile(x, c(0.025, 0.975)), R = 100, n = nrow(rsubs)
  )
})

# compute the mean of the resulting CI limits
coefs <- recombine(subs_blb, combMean)
matrix(coefs, ncol = 2, byrow = TRUE)

# =======================================================================================

########## Comparing Coefficients

######### From simple GLM
coef_full<-summary(model32)$coefficients[,1]
coef_full

######### From drGLM()
# tem1<-recombine(subs_glm, combMeanCoef)
pin2<-matrix(tem1, ncol = 1, byrow = TRUE)
rownames(pin2)<-names(tem1)
pin2


matrix2<-cbind(coef_full, pin2)
colnames(matrix2)[2]<-"coef_drGLM(10)"
matrix2

# Calculate relative change
pin_change<-matrix(apply(matrix2, 1, function(x) abs((x[1] - x[2])/x[1])))
rownames(pin_change)<-names(tem1)
colnames(pin_change)<-"Relative Change"
pin_change

# =============================================

matrix2<-cbind(matrix2, round(abs(matrix2[,1]-matrix2[,2]),3))
colnames(matrix2)[3]<-"Error"
matrix2
matrix2[order(matrix2[,3], decreasing=TRUE),]

temp32<-cbind(matrix2, summary(model32)$coefficients[,2])
colnames(temp32)[4]<-"Std. Error (full GLM)"
temp32[order(temp32[,3], decreasing=TRUE),]

# =============================================

library(Metrics)
rmse(matrix2[,1], matrix2[,2])
sqrt(mean((matrix2[,1]-matrix2[,2])^2, na.rm = TRUE))

# ---------------------------------------------
########## Mean Absolute Error
## mae computes the average absolute difference between two numeric vectors

library(Metrics)
mae(matrix2[,1], matrix2[,2])
# =============================================


#########  Confidence Interval #########

###### => for 95% conf.interval
de1<-confint(model32)          ## CIs using profiled log-likelihood
## Calculate the length of a confidence interval
de1<-cbind(de1, round(de1[,2] - de1[,1],3))
colnames(de1)[3]<-"conf_width"
de1

### => quantile(x, c(0.025, 0.975))
de__blb<-matrix(coefs, ncol = 2, byrow = TRUE)
## Also, Calculate the length of a confidence interval
de__blb<-cbind(de__blb, round(de__blb[,2] - de__blb[,1],3))
colnames(de__blb)<-colnames(de1)
rownames(de__blb)<-rownames(de1)
de__blb


rmse(de1[,1], de__blb[,1])
rmse(de1[,2], de__blb[,2])
rmse(de1[,3], de__blb[,3])

mae(de1[,1], de__blb[,1])
mae(de1[,2], de__blb[,2])
mae(de1[,3], de__blb[,3])


change<-cbind(round(abs((de1[,1]-de__blb[,1])/de1[,1]),3),
              round(abs((de1[,2]-de__blb[,2])/de1[,2]),3),
              round(abs((de1[,3]-de__blb[,3])/de1[,3]),3)
)

rownames(change)<-rownames(de1)
colnames(change)<-colnames(de1)
change

change[change[,1]>1 | change [,2] >1,]
change[change[,1]>0.5 & change[,1]<=1 | change [,2] >0.5 & change[,2]<=1,]

### ======================================================================

###### => for 90% conf.interval

de90<-confint(model32, level=0.9)          ## CIs using profiled log-likelihood
## Calculate the length of a confidence interval
de90<-cbind(de90, round(de90[,2] - de90[,1],3))
colnames(de90)[3]<-"conf_width"
de90

### => quantile(x, c(0.05, 0.95))
de__blb90<-matrix(coefs90, ncol = 2, byrow = TRUE)
## Also, Calculate the length of a confidence interval
de__blb90<-cbind(de__blb90, round(de__blb90[,2] - de__blb90[,1],3))
colnames(de__blb90)<-colnames(de90)
rownames(de__blb90)<-rownames(de90)
de__blb90

rmse(de90[,1], de__blb90[,1])
rmse(de90[,2], de__blb90[,2])
rmse(de90[,3], de__blb90[,3])


mae(de90[,1], de__blb90[,1])
mae(de90[,2], de__blb90[,2])
mae(de90[,3], de__blb90[,3])


change90<-cbind(round(abs((de90[,1]-de__blb90[,1])/de90[,1]),3),
                round(abs((de90[,2]-de__blb90[,2])/de90[,2]),3),
                round(abs((de90[,3]-de__blb90[,3])/de90[,3]),3)
)

rownames(change90)<-rownames(de90)
colnames(change90)<-colnames(de90)
change90

change90[change90[,1]>1 | change90 [,2] >1,]
change90[change90[,1]>0.5 & change90[,1]<=1 | change90 [,2] >0.5 & change90[,2]<=1,]

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================

#################################################################
#####################  20 splits (subsets)  #####################
#################################################################

nrow(data_ddr)/20
round(nrow(data_ddr)/20,0)

set.seed(685)

rsubs20 <- divide(data_ddr, by = rrDiv(1946), update = TRUE)
rsubs20

# plot distribution of the number of rows in each subset
library(ggplot2)
qplot(y = splitRowDistn(rsubs20),
      xlab = "percentile", ylab = "number of rows in subset", 
      main='Distribution of the number of rows in each of the 20 subsets')


## Keys for random replicate divided data are simply labels indicating the bin:
head(getKeys(rsubs20))


summary(rsubs20[[1]][[2]])

nrow(rsubs20[[1]][[2]])
nrow(rsubs20[[2]][[2]])
nrow(rsubs20[[3]][[2]])
nrow(rsubs20[[4]][[2]])
nrow(rsubs20[[5]][[2]])
nrow(rsubs20[[6]][[2]])
nrow(rsubs20[[7]][[2]])
nrow(rsubs20[[8]][[2]])
nrow(rsubs20[[9]][[2]])
nrow(rsubs20[[10]][[2]])
nrow(rsubs20[[11]][[2]])
nrow(rsubs20[[12]][[2]])
nrow(rsubs20[[13]][[2]])
nrow(rsubs20[[14]][[2]])
nrow(rsubs20[[15]][[2]])
nrow(rsubs20[[16]][[2]])
nrow(rsubs20[[17]][[2]])
nrow(rsubs20[[18]][[2]])
nrow(rsubs20[[19]][[2]])
nrow(rsubs20[[20]][[2]])

########################################################################
#####################  Fitting a GLM with drGLM()  #####################
########################################################################

## Apply a drGLM() transformation to rsubs and then call recombine() on the result
subs_glm20 <- addTransform(rsubs20, function(x)
  drGLM(as.factor(SUBSCRIBED)~., data = x, family = binomial(link = "logit")))

tem20<-recombine(subs_glm20, combMeanCoef)
round(tem20,9)

########################################################################
#####################  Fitting a GLM with drBLB()  #####################
########################################################################

##### => for 95% confidence Interval

subs_blb20 <- addTransform(rsubs20, function(x) {
  drBLB(x,
        statistic = function(x, weights)
          coef(glm(as.factor(SUBSCRIBED)~., data = x, weights = weights, family = binomial(link = "logit"))),
        metric = function(x)
          quantile(x, c(0.025, 0.975)), R = 100, n = nrow(rsubs20)
  )
})

# compute the mean of the resulting CI limits
coefs20 <- recombine(subs_blb20, combMean)
matrix(coefs20, ncol = 2, byrow = TRUE)

# =======================================================================================

########## Comparing Coefficients

######### From simple GLM
coef_full<-summary(model32)$coefficients[,1]
coef_full

######### From drGLM()
# tem20<-recombine(subs_glm20, combMeanCoef)
pin22<-matrix(round(tem20,9), ncol = 1, byrow = TRUE)
rownames(pin22)<-names(tem20)
pin22

matrix22<-cbind(matrix2, pin22)
colnames(matrix22)[c(2,4)]<-c("coef_drGLM(10)","coef_drGLM(20)")
matrix22


# Calculate relative change
pin_change22<-matrix(apply(matrix22, 1, function(x) abs((x[1] - x[4])/x[1])))
rownames(pin_change22)<-names(tem20)
colnames(pin_change22)<-"Relative Change"
pin_change22

# =============================================
matrix22<-cbind(matrix22, round(abs(matrix22[,1]-matrix22[,4]),3))
colnames(matrix22)[c(3,5)]<-c("Error 10 from glm", "Error 20 from glm")
matrix22
matrix22[order(matrix22[,5], decreasing=TRUE),]

temp52<-cbind(matrix22, summary(model32)$coefficients[,2])
colnames(temp52)[6]<-"Std. Error (full GLM)"
temp52[order(temp52[,5], decreasing=TRUE),]

# =============================================

library(Metrics)
rmse(matrix22[,1], matrix22[,4])

########## Mean Absolute Error
library(Metrics)
mae(matrix22[,1], matrix22[,4])

# =============================================

#########  Confidence Interval #########

###### => for 95% conf.interval

### 95% confidence interval for glm with all the data
de1

### => quantile(x, c(0.025, 0.975))
de__blb20<-matrix(coefs20, ncol = 2, byrow = TRUE)
## Also, Calculate the length of a confidence interval
de__blb20<-cbind(de__blb20, round(de__blb20[,2] - de__blb20[,1],3))
colnames(de__blb20)<-colnames(de1)
rownames(de__blb20)<-rownames(de1)
de__blb20

rmse(de1[,1], de__blb20[,1])
rmse(de1[,2], de__blb20[,2])
rmse(de1[,3], de__blb20[,3])


mae(de1[,1], de__blb20[,1])
mae(de1[,2], de__blb20[,2])
mae(de1[,3], de__blb20[,3])

change20<-cbind(round(abs((de1[,1]-de__blb20[,1])/de1[,1]),3),
                round(abs((de1[,2]-de__blb20[,2])/de1[,2]),3),
                round(abs((de1[,3]-de__blb20[,3])/de1[,3]),3)
)

rownames(change20)<-rownames(de1)
colnames(change20)<-colnames(de1)
change20

change20[change20[,1]>1 | change20 [,2] >1,]
change20[change20[,1]>0.5 & change20[,1]<=1 | change20 [,2] >0.5 & change20[,2]<=1,]



#################################################################################################
#################################################################################################
#######################################  Custom Barplots  #######################################
#################################################################################################
#################################################################################################

# •	2 coefficients more than 100% relative change
# •	1 coefficient had more than 90% relative change
# •	4 coefficients from 20% to 40% relative change
# •	4 coefficients from 10% to 20% relative change
# •	21 coefficients less than 5% relative change

x1<-c(100,100,90,21,21,21,21,11,11,11,11,rep(1,21))
x1_cut<-cut(x1,breaks=c(0,5,10,20,40,80,91,101),
            labels=c("0-5%","5-10%","10-20%","20-40%",'40-80%','80-100%','>100%'),include.lowest = T)
table(x1_cut)

plot74<-barplot(table(x1_cut),main="Barplot (D&R with 10 splits)",ylab="Frequency",xlab="% Relative Change")
## Add text at top of bars
text(plot74, -0.1, as.numeric(table(x1_cut)),cex=1,pos=3, col = "red") 

# ===========================================================================================

# •	13 coefficients more than 100% relative change
# •	4 coefficients from 80% to 100% relative change
# •	8 coefficients from 20% to 40% relative change
# •	Only 7 coefficients less than 10% relative change, from which 5 were lower than 5% relative change

x2<-c(rep(1,5),7,7,rep(21,8),rep(91,4),rep(101,13))
x2_cut<-cut(x2,breaks=c(0,5,10,20,40,80,91,101),
            labels=c("0-5%","5-10%","10-20%","20-40%",'40-80%','80-100%','>100%'),include.lowest = T)
table(x2_cut)

plot74<-barplot(table(x2_cut),main="Barplot (D&R with 20 splits)",ylab="Frequency",xlab="% Relative Change")
## Add text at top of bars
text(plot74, -0.1, as.numeric(table(x2_cut)),cex=1,pos=3, col = "red") 


# ===========================================================================================

# 4 coefficients with relative change of at least one of its confidence limits more than 100%
# 5 coefficients with relative change of at least one of its confidence limits from 20% to 40%
# 11 coefficients with relative change of at least one of its confidence limits from 10% to 20%
# 12 coefficients with relative change for both of its confidence limits less than 6%

x31<-c(rep(1,12),rep(11,11),rep(21,5),rep(101,4))
x31_cut<-cut(x31,breaks=c(0,5,10,20,40,80,91,101),
             labels=c("0-6%","6-10%","10-20%","20-40%",'40-50%','50-100%','>100%'),include.lowest = T)
table(x31_cut)

plot79<-barplot(table(x31_cut),main="Barplot (D&R with 10 splits)",ylab="Frequency",xlab="% Relative change for (at least one) CI limit",col="azure3")
## Add text at top of bars
text(plot79, -0.1, as.numeric(table(x31_cut)),cex=1,pos=3, col = "red") 

# ===========================================================================================

# 14 coefficients with relative change of at least one of its confidence limits more than 100%
# 7 coefficients with relative change of at least one of its confidence limits from 50% to 100%
# 5 coefficients with relative change of at least one of its confidence limits from 40% to 50%
# 4 coefficients with relative change of at least one of its confidence limits from 20% to 40%
# 1 coefficients with relative change of at least one of its confidence limits from 10% to 20%
# Only 1 coefficient with relative change for both of its confidence limits less than 6%

x32<-c(1,11,rep(21,4),rep(41,5),rep(81,7),rep(101,14))
x32_cut<-cut(x32,breaks=c(0,5,10,20,40,80,91,101),
             labels=c("0-6%","6-10%","10-20%","20-40%",'40-50%','50-100%','>100%'),include.lowest = T)
table(x32_cut)


plot82<-barplot(table(x32_cut),main="Barplot (D&R with 20 splits)",ylab="Frequency",xlab="% Relative change for (at least one) CI limit",col="azure3")
## Add text at top of bars
text(plot82, -0.1, as.numeric(table(x32_cut)),cex=1,pos=3, col = "red") 

# ===========================================================================================
# ===========================================================================================

summary(data$nr.employed)
library(psych)
describe(data$nr.employed)