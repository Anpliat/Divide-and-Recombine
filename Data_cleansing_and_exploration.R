data<- read.csv("C:/../project_2.csv", header=TRUE,stringsAsFactors=TRUE)
head(data)
#edit(data)
dim(data)   
is.data.frame(data) 

any(is.na(data))
all(colSums(is.na(data)) != 0)
sum(sapply(data,is.na))
sum(sapply(data,is.null))
sum(sapply(data,is.nan))
sum(sapply(data,is.infinite))
any(duplicated.data.frame(data)==TRUE)  

class(data)
str(data) 
sapply(data,class) 
#------------------------------------------------------
#------------------------------------------------------


sapply(data[sapply(data,class)=="factor"],attributes)
factors<-data[sapply(data,class)=='factor']  
sinexeis<-data[sapply(data,class)=='numeric']  
diakrites<-data[sapply(data,class)=='integer'] 
summary(factors)
round(sapply(sinexeis,summary),1)
round(sapply(diakrites,summary),1)

#For numeric variables, we'll use the summary function. 
#For character/factor variables, we'll use table.
#------------------------------------------------------
posost11<-round(100* sum(data$default=="yes") / nrow(data),3)
print(paste("The proportion of yes values in default variable is = ",posost11," %",sep=""))
data$default<-NULL
#------------------------------------------------------

#poutcome: outcome of the previous marketing campaign
posost11<-round(100* sum(data$poutcome=="nonexistent") / nrow(data),0)
print(paste("The proportion of nonexistent values in poutcome variable is = ",posost11," %",sep=""))

#------------------------------------------------------
posost1<-round(100* sum(data$housing=="unknown") / nrow(data),0)
print(paste("The proportion of unknown values in housing and loan variables is = ",posost1," %",sep=""))
summary(data[,5:6])
dim(data)
data<-data[data$loan!="unknown",]

data$loan<-factor(data$loan)
data$housing<-factor(data$housing)
summary(data[,5:6])
dim(data)

posost2<-round(100* sum(data$marital=="unknown") / nrow(data),1)
print(paste("The proportion of unknown values in marital variable is = ",posost2," %",sep=""))
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
## Education
table(data$education)
primary<-c("basic.4y","basic.6y","basic.9y")
library(car)
data$education<-recode(data$education,"data$education[data$education%in%primary]='primary'")
levels(data$education)[5]<-"university"  # anti gia university.degree => university
table(data$education)
#------------------------------------------------------
#------------------------------------------------------
levels(data$education)
lev1<-c("illiterate","primary","high.school","professional.course","university","unknown")
data$education<-factor(data$education,levels=lev1)
#data$education<-factor(data$education,levels=lev1,ordered = T)
#----------------------------------------------------------

levels(data$day_of_week)
head(data$day_of_week)
lev2<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev2)
#data$day_of_week<-factor(data$day_of_week,levels=lev2,ordered = T)


factors<-data[sapply(data,class)=='factor']  
summary(factors) 
#------------------------------------------------------
#------------------------------------------------------

levels(data$job)
table(data$job)

White_collar<- c("admin.","entrepreneur","management","self-employed")
Blue_collar<-c("blue-collar","services","technician")
Other <- c("unemployed","housemaid","student","retired","unknown")      


job_old<-data$job
library(car)
data$job<-recode(data$job,"data$job[data$job%in%White_collar]='White_collar'")
data$job<-recode(data$job,"data$job[data$job%in%Blue_collar]='Blue_collar'")
data$job<-recode(data$job,"data$job[data$job%in%Other]='Other'")
table(data$job)

lev5<-c("White_collar","Blue_collar","Other")
data$job<-factor(data$job,levels=lev5)
table(data$job)

factors<-data[sapply(data,class)=='factor']  
summary(factors) 
#------------------------------------------------------
#------------------------------------------------------

## contrasts() function. This function will show us how the variables have been dummyfied by R
# and how to interpret them in a model.

sapply(factors,contrasts)
levels(data$SUBSCRIBED)
head(data$SUBSCRIBED)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

factors<-data[sapply(data,class)=='factor']  
summary(factors) 

####### Month 
levels(data$month)
spring<-c("mar","apr","may")
summer<-c("jun","jul","aug")
fall<-c("sep","oct","nov")
winter<-c("dec","jan","feb") 

seasons<-data$month
levels(seasons)
head(seasons)

library(car)
seasons<-recode(seasons,"seasons[seasons%in%spring]='spring'")
seasons<-recode(seasons,"seasons[seasons%in%summer]='summer'")
seasons<-recode(seasons,"seasons[seasons%in%fall]='fall'")
seasons<-recode(seasons,"seasons[seasons%in%winter]='winter'")

levels(seasons)
head(seasons)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

diakrites<-data[sapply(data,class)=='integer'] 
round(sapply(diakrites,summary),1)

### pdays variable

# summary of our ‘pdays’ column looks a little bit off .
# lets take a look at its value counts

length(unique(data$pdays))  #exei 16 diaforetikes times i metabliti auti
table(as.factor(data$pdays))
#999: means client was not previously contacted

#Proportion of missing values 
post<-100* sum(data$pdays==999) / nrow(data)
print(paste("The proportion of 999 value in pdays variable is = ",round(post,0)," %",sep=""))

# 97% of the values of pdays variable are 999, which renders this variable unusable. 
# In order to use it more properly we create a new variable depending on pdays and then
# we remove pdays from our dataset.


####### create a new variable#######
# After looking at the data, I could figure out to create some new variables.
# If you do not see it, look deeper into the data.
# check the distribution of the dependent variable with predictor variables.
# 999 means client was not previously contacted. Blepw oti to 97% twn clients were not previously contacted.
# We'll capture this trend using a binary coded variable.

p_contact<-data$pdays
table(p_contact)
p_contact <- ifelse(p_contact==999, 1, 0)  # an =999 => 1 ,alliws => 0
table(p_contact)
#-------       -----------          ------------         -----------      --------     

class(p_contact)
### etsi omws i p_contact exei class="numeric". TO metatrepw se "factor"
p_contact<-factor(p_contact,levels=c(0,1),labels=c("yes","no"))
class(p_contact)

table(p_contact)
#------------------------------------------------------
#------------------------------------------------------

####### transform variables #########

#Integers (Diakrites)
diakrites<-data[sapply(data,class)=='integer'] 
round(sapply(diakrites,summary),1)

length(unique(diakrites$age))      
length(unique(diakrites$duration))
length(unique(diakrites$campaign)) 
length(unique(diakrites$previous))
###length(unique(diakrites$pdays)) 

range(unique(diakrites$age))      
range(unique(diakrites$duration))  
range(unique(diakrites$campaign))  

sort(unique(diakrites$previous))
table(diakrites$previous)
post3<-100* sum(data$previous!=0) / nrow(data)
print(paste("The proportion of 999 value in pdays variable is = ",round(post3,0)," %",sep=""))
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
summary(data)
dim(data)
new_data<-cbind(data[,1:7],seasons,data[,9:11],p_contact,data[,13:20])
dim(new_data)
#colnames(new_data)[c(9,11,20)]<-c("day_of_week","campaign","SUBSCRIBED")
summary(new_data)
head(new_data)
#------------------------------------------------------
#------------------------------------------------------

data<-new_data
data$duration<-NULL
# write.csv(data,"C:/../new_project_data.csv",col.names = T, row.names=FALSE)

### Note: row.names=FALSE was used to avoid extra column when writing the csv file
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

#install.packages(c("corrgram","nnet", "class", "tree", "pgmm",
#                   "penalizedLDA", "klaR","dplyr","ggplot2", "e1071", "randomForest", "scoring"))

library(corrgram)
library(nnet)
library(class)
library(tree)
library(MASS)
library(pgmm)
library(penalizedLDA)
library(klaR)
library(dplyr)
library(ggplot2)
library(scoring)
library(e1071)    #first install class and then this one
library(randomForest)
library(mclust) # for adjustedRandIndex

str(data)

##############################################################################
##############################################################################
##############################################################################
##############################################################################

#########################################################################
#############################  For Numeric  #############################
#########################################################################
y<-data$SUBSCRIBED

win.graph()
par(mfrow=c(1,5))

boxplot(data$emp.var.rate ~ y,xlab="SUBSCRIBED",ylab="emp.var.rate",col="navy",medcol="red")
boxplot(data$cons.price.idx ~ y,xlab="SUBSCRIBED",ylab="cons.price.idx",col="navy",medcol="red") 
boxplot(data$cons.conf.idx ~ y,xlab="SUBSCRIBED",ylab="cons.conf.idx",col="navy",medcol="red")
boxplot(data$euribor3m ~ y,xlab="SUBSCRIBED",ylab="euribor3m",col="navy",medcol="red")
boxplot(data$nr.employed ~ y,xlab="SUBSCRIBED",ylab="nr.employed",col="navy",medcol="red")

title("Boxplot of SUBSCRIBED for numeric",outer=TRUE,cex.main =1.5,font=2,line=-2)   #βάζω τίτλο στο παράθυρο διαγραμμάτων
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
###################################
########## Correlations ###########
###################################
y_num<-y
levels(y_num)<-c(0,1)
levels(y_num)
head(y_num)
y_num<-as.numeric(y_num)
unique(y_num)


y_num<-ifelse(y_num==1, 0, 1)  # an =999 => 1 ,alliws => 0
unique(y_num)
#-----------------------------------------------------------------------
# correlation coefficients between output variable Y and the input variables
sinexeis<-data[sapply(data,class)=='numeric']  
head(sinexeis)

posotikes<-data[sapply(data,class)=='numeric' | sapply(data,class)=='integer'] 
head(posotikes)

corre0<-t(round(cor(y_num,posotikes),2))
colnames(corre0)<-"y"
corre0

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

sin1<-cbind(y_num,sinexeis)
colnames(sin1)[1]<-"SUBSCRIBED"

library(corrplot)
round(cor(sin1),2)
corrplot(round(cor(sin1),2),method="number",type="upper")
round(cor(posotikes),2)

win.graph()
library(corrplot)
corrplot(round(cor(posotikes),2),method="number",type="upper")

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#########################################################################
#############################  For Factors  #############################
#########################################################################

################################
########## BAR-PLOTS ###########
################################

factors<-data[sapply(data,class)=='factor']  
sinexeis<-data[sapply(data,class)=='numeric']  
diakrites<-data[sapply(data,class)=='integer'] 
summary(factors)
round(sapply(sinexeis,summary),1)
round(sapply(diakrites,summary),1)

############# Plots #############

y1<-factor(as.numeric(y),levels=c(2,1),labels=c("no","yes"))

xrwmata<-c("red","cyan")
barplot(round(prop.table(table(y))*100),main="Barplot for SUBSCRIBED",ylab="Percentage (%)",xlab="SUBSCRIBED",col=xrwmata)
legend('topright', fil=xrwmata, legend=levels(y), ncol=2, bty='n',cex=0.8)


win.graph()
par(mfrow=c(2,2))


lev41<-c("White_collar","Blue_collar","Other")
data$job<-factor(data$job,levels=lev41)
barplot(round(prop.table(table(y1,data$job))*100),main="Barplot for job",ylab="Percentage (%)",xlab="job",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)


# marital
table(data$marital)
lev41a<-c("divorced","single","married")
data$marital<-factor(data$marital,levels=lev41a)
barplot(round(prop.table(table(y1,data$marital))*100),main="Barplot for marital",ylab="Percentage (%)",xlab="marital",col=xrwmata)
legend('topleft', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

# housing 
barplot(round(prop.table(table(y1,data$housing))*100),main="Barplot for housing",ylab="Percentage (%)",xlab="housing",col=xrwmata)
legend('topleft', fil=xrwmata, legend=c("yes","no"),ncol=2, bty='n',cex=0.8)

# loan  
barplot(round(prop.table(table(y1,data$loan))*100),main="Barplot for loan",ylab="Percentage (%)",xlab="loan",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)


win.graph()
par(mfrow=c(3,2))

# contact 
barplot(round(prop.table(table(y1,data$contact))*100),main="Barplot for contact",ylab="Percentage (%)",xlab="contact",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), ncol=2, bty='n',cex=0.8)

# seasons 
barplot(round(prop.table(table(y1,data$seasons))*100),main="Barplot for seasons",ylab="Percentage (%)",xlab="seasons",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

# day_of_week
table(data$day_of_week)  
lev47<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev47)
barplot(round(prop.table(table(y1,data$day_of_week))*100),main="Barplot for day_of_week",ylab="Percentage (%)",xlab="day_of_week",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

# p_contact 
barplot(round(prop.table(table(y1,data$p_contact))*100),main="Barplot for p_contact",ylab="Percentage (%)",xlab="p_contact",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

# poutcome 
barplot(round(prop.table(table(y1,data$poutcome))*100),main="Barplot for poutcome",ylab="Percentage (%)",xlab="poutcome",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

# education
barplot(round(prop.table(table(y1,data$education))*100),main="Barplot for education",ylab="Percentage (%)",xlab="education",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

########################################
########## Contigency tables ###########
########################################

lev41<-c("White_collar","Blue_collar","Other")
data$job<-factor(data$job,levels=lev41)

lev41a<-c("divorced","single","married")
data$marital<-factor(data$marital,levels=lev41a)

lev47<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev47)


#install.packages("sjPlot")
library(sjPlot)

sjt.xtab(data$job,y,var.labels=c("JOB","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$marital,y,var.labels=c("MARITAL","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$education,y,var.labels=c("EDUCATION","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$housing,y,var.labels=c("HOUSING","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$loan,y,var.labels=c("LOAN","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$contact,y,var.labels=c("CONTACT","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$seasons,y,var.labels=c("seasons","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$day_of_week,y,var.labels=c("DAY_OF_WEEK","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$p_contact,y,var.labels=c("P_CONTACT","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$poutcome,y,var.labels=c("poutcome","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")


sjt.xtab(data$poutcome,data$p_contact,var.labels=c("poutcome","P_CONTACT"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$housing,data$loan,var.labels=c("housing","loan"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
