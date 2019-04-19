##################################### L&T Finance ###################################
#####--------------------------------##############---------------------------#######
# Loading the libraries
library(ROCR)
library(ggplot2)
#library(Information)
library(caTools)
library(eeptools)
library(lubridate)
library(tidyr)
library(stringr)
library(gdata)
library(gridExtra)
library(cowplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(dplyr)
library(randomForest)

#####-----------------------------------------------------------------------------
### Load main dataset  
main <- read.csv("~/Downloads/train.csv")
final_test <- read.csv("~/Downloads/test.csv")

#####-----------------------------------------------------------------------------
## Basic data quality checks ##
str(main)
# Check for duplicate values
sum(duplicated(main$UniqueID))
# 0 tells that there are no duplicate values
# Checking NA values in target variable
sum(is.na(main$loan_default))
# Checking % of deault in the first month EMI
(sum(main$loan_default)/nrow(main))*100
# Nearly ~21.7% are the default rate in the first month

# Missing values in all the variables 
sapply(main, function(x) sum(is.na(x)))
# There are some empty values 7661 in employment.type column. As the empty rows are less in number its safe to remove rows
main <- main[!(main$Employment.Type==""), ]

#####-----------------------------------------------------------------------------
table(main$MobileNo_Avl_Flag)
# MobileNo_Avl_Flag have all the rows as 1. Removing it from dataframe
main$MobileNo_Avl_Flag <- NULL
final_test$MobileNo_Avl_Flag <- NULL

#####-----------------------------------------------------------------------------
## Treating Date columns
#1.Date.of.Birth- Calculating Age from date of birth
main <- separate(main, "Date.of.Birth", into = c("Day", "Month", "Year"), "-")
main$Year <- as.numeric(main$Year)
main$Year <- if_else(main$Year == 0, main$Year+2000, main$Year+1900)
main <- unite(main,"Age",c("Year","Month","Day"),sep="-",remove = TRUE)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
main$Age <- as.Date(main$Age)
main$Age <- age(main$Age)
remove(age)

#2.Date.of.Birth-DisbursalDate
main <- separate(main, "DisbursalDate", into = c("Disbrsl_Day", "Disbrsl_Month", "Disbrsl_Year"), "-")
main$Disbrsl_Month <- as.numeric(main$Disbrsl_Month)
main$Disbrsl_Month <- month.name[main$Disbrsl_Month]
#Dropping Year as it is 2018 
main$Disbrsl_Year <- NULL
# Converting dates to month_start, month_mid and month_end
main$Disbrsl_Day <- as.numeric(main$Disbrsl_Day)
main$Disbrsl_Day <- if_else(main$Disbrsl_Day <= 5, "month_start", if_else(main$Disbrsl_Day <= 25, "month_mid", "month_end"))

#3.AVERAGE.ACCT.AGE
main <- separate(main, "AVERAGE.ACCT.AGE", into = c("a1", "a2"), " ")
main$a1 <- sub('yrs', '', main$a1)
main$a2 <- sub('mon', '', main$a2)
main$a1 <- as.numeric(main$a1)
main$a2 <- as.numeric(main$a2)
main$AVERAGE.ACCT.AGE <- main$a1 + main$a2
main$a1 <- NULL
main$a2 <- NULL

#4 CREDIT.HISTORY.LENGTH
main <- separate(main, "CREDIT.HISTORY.LENGTH", into = c("a1", "a2"), " ")
main$a1 <- sub('yrs', '', main$a1)
main$a2 <- sub('mon', '', main$a2)
main$a1 <- as.numeric(main$a1)
main$a2 <- as.numeric(main$a2)
main$CREDIT.HISTORY.LENGTH <- main$a1 + main$a2
main$a1 <- NULL
main$a2 <- NULL

#####-----------------------------------------------------------------------------
## Treating Date columns For test
#1.Date.of.Birth- Calculating Age from date of birth
final_test <- separate(final_test, "Date.of.Birth", into = c("Day", "Month", "Year"), "-")
final_test$Year <- as.numeric(final_test$Year)
final_test$Year <- if_else(final_test$Year == 0, final_test$Year+2000, final_test$Year+1900)
final_test <- unite(final_test,"Age",c("Year","Month","Day"),sep="-",remove = TRUE)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
final_test$Age <- as.Date(final_test$Age)
final_test$Age <- age(final_test$Age)
remove(age)

#2.Date.of.Birth-DisbursalDate
final_test <- separate(final_test, "DisbursalDate", into = c("Disbrsl_Day", "Disbrsl_Month", "Disbrsl_Year"), "-")
final_test$Disbrsl_Month <- as.numeric(final_test$Disbrsl_Month)
final_test$Disbrsl_Month <- month.name[final_test$Disbrsl_Month]
#Dropping Year as it is 2018 
final_test$Disbrsl_Year <- NULL
# Converting dates to month_start, month_mid and month_end
final_test$Disbrsl_Day <- as.numeric(final_test$Disbrsl_Day)
final_test$Disbrsl_Day <- if_else(final_test$Disbrsl_Day <= 5, "month_start", if_else(final_test$Disbrsl_Day <= 25, "month_mid", "month_end"))

#3.AVERAGE.ACCT.AGE
final_test <- separate(final_test, "AVERAGE.ACCT.AGE", into = c("a1", "a2"), " ")
final_test$a1 <- sub('yrs', '', final_test$a1)
final_test$a2 <- sub('mon', '', final_test$a2)
final_test$a1 <- as.numeric(final_test$a1)
final_test$a2 <- as.numeric(final_test$a2)
final_test$AVERAGE.ACCT.AGE <- final_test$a1 + final_test$a2
final_test$a1 <- NULL
final_test$a2 <- NULL

#4 CREDIT.HISTORY.LENGTH
final_test <- separate(final_test, "CREDIT.HISTORY.LENGTH", into = c("a1", "a2"), " ")
final_test$a1 <- sub('yrs', '', final_test$a1)
final_test$a2 <- sub('mon', '', final_test$a2)
final_test$a1 <- as.numeric(final_test$a1)
final_test$a2 <- as.numeric(final_test$a2)
final_test$CREDIT.HISTORY.LENGTH <- final_test$a1 + final_test$a2
final_test$a1 <- NULL
final_test$a2 <- NULL


#####-----------------------------------------------------------------------------
## Treating Categorical columns which are having a lot of labels
# we can use response rate to convert them to continous variable
# Here we can replace each Id with its Non-default rate
#1.branch_id
table(main$branch_id)
test <- aggregate(main$loan_default,by= list(main$branch_id), FUN = mean)
test$x = format(round((1- test$x) * 100,2), nsmall = 2)
colnames(test) <- c("branch_id","branch_id_RR")
main <- merge(main,test,by="branch_id",row.names=FALSE)
final_test <- merge(final_test,test,by="branch_id",row.names=FALSE,all.x=T)
remove(test)

#2.supplier_id
table(main$supplier_id)
test <- aggregate(main$loan_default,by= list(main$supplier_id), FUN = mean)
test$x = format(round((1- test$x) * 100,2), nsmall = 2)
colnames(test) <- c("supplier_id","supplier_id_RR")
main <- merge(main,test,by="supplier_id",row.names=FALSE)
final_test <- merge(final_test,test,by="supplier_id",row.names=FALSE,all.x=T)
remove(test)

#3.Current_pincode_ID
table(main$Current_pincode_ID)
test <- aggregate(main$loan_default,by= list(main$Current_pincode_ID), FUN = mean)
test$x = format(round((1- test$x) * 100,2), nsmall = 2)
colnames(test) <- c("Current_pincode_ID","Current_pincode_RR")
main <- merge(main,test,by="Current_pincode_ID",row.names=FALSE)
final_test <- merge(final_test,test,by="Current_pincode_ID",row.names=FALSE,all.x=T)
remove(test)

#4.Employee_code_ID
table(main$Employee_code_ID)
test <- aggregate(main$loan_default,by= list(main$Employee_code_ID), FUN = mean)
test$x = format(round((1- test$x) * 100,2), nsmall = 2)
colnames(test) <- c("Employee_code_ID","Employee_code_RR")
main <- merge(main,test,by="Employee_code_ID",row.names=FALSE)
final_test <- merge(final_test,test,by="Employee_code_ID",row.names=FALSE,all.x=T)
remove(test)

#5.State_ID
table(main$State_ID)
test <- aggregate(main$loan_default,by= list(main$State_ID), FUN = mean)
test$x = format(round((1- test$x) * 100,2), nsmall = 2)
colnames(test) <- c("State_ID","State_ID_RR")
main <- merge(main,test,by="State_ID",row.names=FALSE)
final_test <- merge(final_test,test,by="State_ID",row.names=FALSE,all.x=T)
remove(test)

#6.PERFORM_CNS.SCORE.DESCRIPTION
table(main$PERFORM_CNS.SCORE.DESCRIPTION)
test <- aggregate(main$loan_default,by= list(main$PERFORM_CNS.SCORE.DESCRIPTION), FUN = mean)
test$x = format(round((1- test$x) * 100,2), nsmall = 2)
colnames(test) <- c("PERFORM_CNS.SCORE.DESCRIPTION","PERFORM_CNS.SCORE.DESCRIPTION_RR")
main <- merge(main,test,by="PERFORM_CNS.SCORE.DESCRIPTION",row.names=FALSE)
final_test <- merge(final_test,test,by="PERFORM_CNS.SCORE.DESCRIPTION",row.names=FALSE,all.x=T)
remove(test)


# remove all 4 ID variables
main$branch_id <- NULL
main$supplier_id <- NULL
main$Current_pincode_ID <- NULL
main$Employee_code_ID <- NULL
main$State_ID <- NULL
main$PERFORM_CNS.SCORE.DESCRIPTION <- NULL


# remove all 4 ID variables (Test Dataset)
final_test$branch_id <- NULL
final_test$supplier_id <- NULL
final_test$Current_pincode_ID <- NULL
final_test$Employee_code_ID <- NULL
final_test$State_ID <- NULL
final_test$PERFORM_CNS.SCORE.DESCRIPTION <- NULL

#####-----------------------------------------------------------------------------
## EDA ##
## Dividing all the variables into categorical and continous dataframes
continuous <- c("disbursed_amount","asset_cost","ltv","Age","PERFORM_CNS.SCORE","PRI.NO.OF.ACCTS","PRI.ACTIVE.ACCTS",
               "PRI.OVERDUE.ACCTS","PRI.CURRENT.BALANCE","PRI.SANCTIONED.AMOUNT","PRI.DISBURSED.AMOUNT",
               "SEC.NO.OF.ACCTS","SEC.ACTIVE.ACCTS","SEC.OVERDUE.ACCTS","SEC.CURRENT.BALANCE","SEC.SANCTIONED.AMOUNT",
               "SEC.DISBURSED.AMOUNT","PRIMARY.INSTAL.AMT","SEC.INSTAL.AMT","NEW.ACCTS.IN.LAST.SIX.MONTHS",
               "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS","AVERAGE.ACCT.AGE","CREDIT.HISTORY.LENGTH","NO.OF_INQUIRIES","branch_id_RR",
               "supplier_id_RR","Current_pincode_RR","Employee_code_RR","State_ID_RR","PERFORM_CNS.SCORE.DESCRIPTION_RR")

categorical <- c("manufacturer_id","Employment.Type","Disbrsl_Day","Disbrsl_Month","Aadhar_flag","PAN_flag","VoterID_flag","Driving_flag","Passport_flag")

demo_continuous <- main[,continuous]
demo_categorical <- main[,categorical]
demo_continuous[] <- lapply(demo_continuous, as.numeric)
demo_categorical[] <- lapply(demo_categorical, as.factor)

# checking for any outliers in the continuous data using quantiles and boxplots
quantiles_df <- sapply(demo_continuous,function(x) quantile(x,seq(0,1,.1)))
### Using quantiles_df, we have identfied few rows having outliers 
# these IDs are :  603445, 629503, 582164, 545300, 428352, 549069, 476729, 518199, 433453, 426391, 579899
main<-main[!(main$UniqueID == "603445" | main$UniqueID == "629503" | main$UniqueID == "582164" | main$UniqueID == "545300" | 
               main$UniqueID == "428352" | main$UniqueID == "549069" | main$UniqueID == "476729" | main$UniqueID == "518199" | 
               main$UniqueID == "433453" | main$UniqueID == "426391" | main$UniqueID == "579899"),]
# Again creating dataframes 
demo_continuous <- main[,continuous]
demo_categorical <- main[,categorical]
loan_default <- as.factor(main$loan_default)
demo_continuous[] <- lapply(demo_continuous, as.numeric)
demo_categorical[] <- lapply(demo_categorical, as.factor)

#####-----------------------------------------------------------------------------
# Univariate analysis for the continuous variables
# Histogram and Boxplots for continuous variables 
box_theme_x<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                    axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_fun_continuous <- function(cont_col_name,var_name){
  plot_grid(ggplot(demo_continuous, aes(cont_col_name))+ geom_histogram(binwidth = 10) +  labs(x = var_name),
            ggplot(demo_continuous, aes(x="",y=cont_col_name))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, 
            align = "v",ncol = 1)
}

# plot_fun_continuous(demo_continuous$disbursed_amount,"disbursed_amount")
# plot_fun_continuous(demo_continuous$asset_cost,"asset_cost")
# plot_fun_continuous(demo_continuous$ltv,"ltv")
# plot_fun_continuous(demo_continuous$Age,"Age")
# plot_fun_continuous(demo_continuous$PERFORM_CNS.SCORE,"PERFORM_CNS.SCORE")
# plot_fun_continuous(demo_continuous$PRI.NO.OF.ACCTS,"PRI.NO.OF.ACCTS")
# plot_fun_continuous(demo_continuous$PRI.ACTIVE.ACCTS,"PRI.ACTIVE.ACCTS")
# plot_fun_continuous(demo_continuous$PRI.OVERDUE.ACCTS,"PRI.OVERDUE.ACCTS")
# plot_fun_continuous(demo_continuous$PRI.CURRENT.BALANCE,"PRI.CURRENT.BALANCE")
# plot_fun_continuous(demo_continuous$PRI.SANCTIONED.AMOUNT,"PRI.SANCTIONED.AMOUNT")
# plot_fun_continuous(demo_continuous$PRI.DISBURSED.AMOUNT,"PRI.DISBURSED.AMOUNT")
# plot_fun_continuous(demo_continuous$SEC.NO.OF.ACCTS,"SEC.NO.OF.ACCTS")
# plot_fun_continuous(demo_continuous$SEC.ACTIVE.ACCTS,"SEC.ACTIVE.ACCTS")
# plot_fun_continuous(demo_continuous$SEC.OVERDUE.ACCTS,"SEC.OVERDUE.ACCTS")
# plot_fun_continuous(demo_continuous$SEC.CURRENT.BALANCE,"SEC.CURRENT.BALANCE")
# plot_fun_continuous(demo_continuous$SEC.SANCTIONED.AMOUNT,"SEC.SANCTIONED.AMOUNT")
# plot_fun_continuous(demo_continuous$SEC.DISBURSED.AMOUNT,"SEC.DISBURSED.AMOUNT")
# plot_fun_continuous(demo_continuous$PRIMARY.INSTAL.AMT,"PRIMARY.INSTAL.AMT")
# plot_fun_continuous(demo_continuous$SEC.INSTAL.AMT,"SEC.INSTAL.AMT")
# plot_fun_continuous(demo_continuous$NEW.ACCTS.IN.LAST.SIX.MONTHS,"NEW.ACCTS.IN.LAST.SIX.MONTHS")
# plot_fun_continuous(demo_continuous$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS,"DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS")
# plot_fun_continuous(demo_continuous$AVERAGE.ACCT.AGE,"AVERAGE.ACCT.AGE")
# plot_fun_continuous(demo_continuous$CREDIT.HISTORY.LENGTH,"CREDIT.HISTORY.LENGTH")
# plot_fun_continuous(demo_continuous$NO.OF_INQUIRIES,"NO.OF_INQUIRIES")
# plot_fun_continuous(demo_continuous$branch_id_RR,"branch_id_RR")
# plot_fun_continuous(demo_continuous$supplier_id_RR,"supplier_id_RR")
# plot_fun_continuous(demo_continuous$Current_pincode_RR,"Current_pincode_RR")
# plot_fun_continuous(demo_continuous$Employee_code_RR,"Employee_code_RR")
# plot_fun_continuous(demo_continuous$State_ID_RR,"State_ID_RR")
# plot_fun_continuous(demo_continuous$PERFORM_CNS.SCORE.DESCRIPTION_RR,"PERFORM_CNS.SCORE.DESCRIPTION_RR")


# Analysing above plots, We need to do scaling for 10 columns
demo_continuous$PERFORM_CNS.SCORE <- scale(demo_continuous$PERFORM_CNS.SCORE)
demo_continuous$PRI.CURRENT.BALANCE <- scale(demo_continuous$PRI.CURRENT.BALANCE)
demo_continuous$PRI.SANCTIONED.AMOUNT <- scale(demo_continuous$PRI.SANCTIONED.AMOUNT)
demo_continuous$PRI.DISBURSED.AMOUNT <- scale(demo_continuous$PRI.DISBURSED.AMOUNT)
demo_continuous$SEC.NO.OF.ACCTS <- scale(demo_continuous$SEC.NO.OF.ACCTS)
demo_continuous$SEC.ACTIVE.ACCTS <- scale(demo_continuous$SEC.ACTIVE.ACCTS)
demo_continuous$SEC.OVERDUE.ACCTS <- scale(demo_continuous$SEC.OVERDUE.ACCTS)
demo_continuous$SEC.CURRENT.BALANCE <- scale(demo_continuous$SEC.CURRENT.BALANCE)
demo_continuous$SEC.SANCTIONED.AMOUNT <- scale(demo_continuous$SEC.SANCTIONED.AMOUNT)
demo_continuous$SEC.DISBURSED.AMOUNT <- scale(demo_continuous$SEC.DISBURSED.AMOUNT)

# Analysing above plots, We need to do scaling for 10 columns ini final_test
final_test$PERFORM_CNS.SCORE <- scale(final_test$PERFORM_CNS.SCORE)
final_test$PRI.CURRENT.BALANCE <- scale(final_test$PRI.CURRENT.BALANCE)
final_test$PRI.SANCTIONED.AMOUNT <- scale(final_test$PRI.SANCTIONED.AMOUNT)
final_test$PRI.DISBURSED.AMOUNT <- scale(final_test$PRI.DISBURSED.AMOUNT)
final_test$SEC.NO.OF.ACCTS <- scale(final_test$SEC.NO.OF.ACCTS)
final_test$SEC.ACTIVE.ACCTS <- scale(final_test$SEC.ACTIVE.ACCTS)
final_test$SEC.OVERDUE.ACCTS <- scale(final_test$SEC.OVERDUE.ACCTS)
final_test$SEC.CURRENT.BALANCE <- scale(final_test$SEC.CURRENT.BALANCE)
final_test$SEC.SANCTIONED.AMOUNT <- scale(final_test$SEC.SANCTIONED.AMOUNT)
final_test$SEC.DISBURSED.AMOUNT <- scale(final_test$SEC.DISBURSED.AMOUNT)

# Correlation
corrs = round(cor(demo_continuous[,-29], use = "pairwise.complete.obs"), 2)
#write.csv(corrs,"corr.csv")
## Higly correlated values:
# disbursed_amount and asset_cost
# SEC.SANCTIONED.AMOUNT and SEC.DISBURSED.AMOUNT 
# SEC.NO.OF.ACCTS and EC.ACTIVE.ACCTS
# SEC.CURRENT.BALANCE, SEC.SANCTIONED.AMOUNT and SEC.DISBURSED.AMOUNT

# Univariate analysis for the categorical variables
# for categorical variables
univariate_cat <- function(col_name,var_name) {
  ggplot(demo_categorical,aes(x = col_name)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1))
}

# univariate_cat(demo_categorical$manufacturer_id,"manufacturer_id")
# univariate_cat(demo_categorical$Employment.Type,"Employment.Type")
# univariate_cat(demo_categorical$Disbrsl_Day,"Disbrsl_Day")
# univariate_cat(demo_categorical$Disbrsl_Month,"Disbrsl_Month")
# univariate_cat(demo_categorical$Aadhar_flag,"Aadhar_flag")
# univariate_cat(demo_categorical$PAN_flag,"PAN_flag")
# univariate_cat(demo_categorical$VoterID_flag,"VoterID_flag")
# univariate_cat(demo_categorical$Driving_flag,"Driving_flag")
# univariate_cat(demo_categorical$Passport_flag,"Passport_flag")

# creating the dummy variables for all the categorical data
col_names <- c("manufacturer_id","Employment.Type","Disbrsl_Day","Disbrsl_Month")
categ_Df <- dplyr::select(demo_categorical, manufacturer_id, Employment.Type, Disbrsl_Day, Disbrsl_Month)
temp<- data.frame(sapply(categ_Df, function(x) factor(x)))
dummies<- data.frame(sapply(temp,function(x) data.frame(model.matrix(~x-1,data =temp))))
demo_categorical <- demo_categorical[ , !(names(demo_categorical) %in% col_names)]
# Final categorical data set
demo_categorical <- cbind(demo_categorical,dummies)
remove(categ_DF,temp,dummies)
# For final_test(Test Dataset)
categ_Df <- dplyr::select(final_test, manufacturer_id, Employment.Type, Disbrsl_Day)
temp<- data.frame(sapply(categ_Df, function(x) factor(x)))
dummies<- data.frame(sapply(temp,function(x) data.frame(model.matrix(~x-1,data =temp))))
final_test <- final_test[ , !(names(final_test) %in% col_names)]

#####-----------------------------------------------------------------------------
### FINAL DATASET ###
main_final <- cbind(demo_categorical, demo_continuous, loan_default)
final_test <- cbind(final_test,dummies)
##### Removing all unwanted variables-----------------------------------------------------------------------------
remove(categorical,continuous,col_names,loan_default,plot_fun_continuous,univariate_cat,box_theme_x,box_theme_y,categ_Df,corrs,dummies,demo_categorical,demo_continuous,temp)
final_test$Employment.Type.x <- NULL
#####-----------------------------------------------------------------------------
################# Creating the Normal Test and Train Dataset #########################
set.seed(100)
trainindices= sample(1:nrow(main_final), 0.7*nrow(main_final))
train = main_final[trainindices,]
test = main_final[-trainindices,]

##################################___________________#########################################
################################# Logistic Regression ########################################

# the very first model containing all the variables against target variable
model_1 <- glm(loan_default~.,data=train,family = "binomial")
summary(model_1)

# using STEPAIC to find to remove insignificant features
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
sort(vif(model_2))

#removing asset_cost based on high vif 
model_3 <- glm(formula = loan_default ~ Aadhar_flag + PAN_flag + Passport_flag + 
                 disbursed_amount + ltv + Age + PERFORM_CNS.SCORE + 
                 PRI.NO.OF.ACCTS + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
                 PRI.CURRENT.BALANCE + PRI.SANCTIONED.AMOUNT + SEC.NO.OF.ACCTS + 
                 PRIMARY.INSTAL.AMT + NEW.ACCTS.IN.LAST.SIX.MONTHS + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
                 CREDIT.HISTORY.LENGTH + NO.OF_INQUIRIES + branch_id_RR + 
                 supplier_id_RR + Current_pincode_RR + Employee_code_RR + 
                 PERFORM_CNS.SCORE.DESCRIPTION_RR, family = "binomial", data = train)
summary(model_3)
sort(vif(model_3))

# removing PRI.SANCTIONED.AMOUNT  based on highly insignificant
model_4 <- glm(formula = loan_default ~ Aadhar_flag + PAN_flag + Passport_flag + 
                 disbursed_amount + ltv + Age + PERFORM_CNS.SCORE + 
                 PRI.NO.OF.ACCTS + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
                 PRI.CURRENT.BALANCE + SEC.NO.OF.ACCTS + 
                 PRIMARY.INSTAL.AMT + NEW.ACCTS.IN.LAST.SIX.MONTHS + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
                 CREDIT.HISTORY.LENGTH + NO.OF_INQUIRIES + branch_id_RR + 
                 supplier_id_RR + Current_pincode_RR + Employee_code_RR + 
                 PERFORM_CNS.SCORE.DESCRIPTION_RR, family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))

# removing PRI.NO.OF.ACCTS  based on highly insignificant and high vif
model_5 <- glm(formula = loan_default ~ Aadhar_flag + PAN_flag + Passport_flag + 
                 disbursed_amount + ltv + Age + PERFORM_CNS.SCORE + 
                 PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
                 PRI.CURRENT.BALANCE + SEC.NO.OF.ACCTS + 
                 PRIMARY.INSTAL.AMT + NEW.ACCTS.IN.LAST.SIX.MONTHS + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
                 CREDIT.HISTORY.LENGTH + NO.OF_INQUIRIES + branch_id_RR + 
                 supplier_id_RR + Current_pincode_RR + Employee_code_RR + 
                 PERFORM_CNS.SCORE.DESCRIPTION_RR, family = "binomial", data = train)
summary(model_5)
sort(vif(model_5))

# removing CREDIT.HISTORY.LENGTH  based on highly insignificant and high vif
model_6 <- glm(formula = loan_default ~ Aadhar_flag + PAN_flag + Passport_flag + 
                 disbursed_amount + ltv + Age + PERFORM_CNS.SCORE + 
                 PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
                 PRI.CURRENT.BALANCE + SEC.NO.OF.ACCTS + 
                 PRIMARY.INSTAL.AMT + NEW.ACCTS.IN.LAST.SIX.MONTHS + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
                 NO.OF_INQUIRIES + branch_id_RR + 
                 supplier_id_RR + Current_pincode_RR + Employee_code_RR + 
                 PERFORM_CNS.SCORE.DESCRIPTION_RR, family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))

# removing Passport_flag  based on highly insignificant
model_7 <- glm(formula = loan_default ~ Aadhar_flag + PAN_flag + 
                 disbursed_amount + ltv + Age + PERFORM_CNS.SCORE + 
                 PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
                 PRI.CURRENT.BALANCE + SEC.NO.OF.ACCTS + 
                 PRIMARY.INSTAL.AMT + NEW.ACCTS.IN.LAST.SIX.MONTHS + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
                 NO.OF_INQUIRIES + branch_id_RR + 
                 supplier_id_RR + Current_pincode_RR + Employee_code_RR + 
                 PERFORM_CNS.SCORE.DESCRIPTION_RR, family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))

final_model_logistic <- model_7

# predicted probabilities of default for test data
test_pred = predict(final_model_logistic, type = "response",newdata = test[,-36])

# Let's see the summary 
summary(test_pred)
test$prob <- test_pred

test_actual_def <- factor(ifelse(test$loan_default==1,"Yes","No"))

#######################################################################
# finding the optimal cutoff value
perform_fn <- function(cutoff) 
{
  predicted_def <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_def, test_actual_def, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
cutoff
#0.217

# optimal cutoff obtained by tuning is 0.217

test_cutoff_def <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 65.8%
# Sensitivity = 63.9%
# Specificity = 66.4%

############################################################################################
################################# Model Evaluation##################################
########################## KS -statistic - Test Data ###############################

test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
test_actual_def <- ifelse(test_actual_def=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_def, test_actual_def)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.30
# KS- Stat value is 30%

# finding the area under the ROC curve
ROC <- performance(pred_object_test, measure = "auc")
area <- ROC@y.values[[1]]
area 
# 0.65

# plotting the ROC curve
tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) + geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression Model") +
  theme(axis.text.x=element_text(hjust=1))


####################################################################
# Lift & Gain Chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_def, test_pred, groups = 10)
default_decile

# Plotting Gain Chart
ggplot(default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_line(data=default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Logistic Model's Gain Chart")


# Plotting Lift Chart
ggplot(default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+ geom_line(data=default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Logistic Model's Lift Chart")

##########################################################################################
################# Predicting score for test dataset ###############################
#########################################################################################
final_test$Aadhar_flag <- as.factor(final_test$Aadhar_flag)
final_test$PAN_flag <- as.factor(final_test$PAN_flag)
final_test$branch_id_RR <- as.numeric(final_test$branch_id_RR)
final_test$supplier_id_RR <- as.numeric(final_test$supplier_id_RR)
final_test$Current_pincode_RR <- as.numeric(final_test$Current_pincode_RR)
final_test$Employee_code_RR <- as.numeric(final_test$Employee_code_RR)
final_test$PERFORM_CNS.SCORE.DESCRIPTION_RR <- as.numeric(final_test$PERFORM_CNS.SCORE.DESCRIPTION_RR)
final_test$loan_default  <- predict(final_model_logistic, type = "response", newdata = final_test)
df_to_upload <- final_test[,c("UniqueID","loan_default")]
df_to_upload$loan_default[is.na(df_to_upload$loan_default)] <- mean(final_test$loan_default,na.rm = T)
write.csv(df_to_upload,"upload.csv",row.names=FALSE)
## ROC area while uploading comes out to be 62%


####################################################################################
############################### Random Forrest ##################################### 
set.seed(100)
main_final_rf <- main_final
trainindices_rf= sample(1:nrow(main_final_rf), 0.7*nrow(main_final_rf))
train_rf = main_final_rf[trainindices_rf,]
test_rf = main_final_rf[-trainindices_rf,]

model_rf <- randomForest(loan_default ~.,data = train_rf,proximity = F,do.trace = T,
                         mtry = 2,ntree=100,importance = TRUE)

summary(model_rf)
varImpPlot(model_rf,type=2)

test_pred_rf <- predict(model_rf, test_rf, type = "prob")
test_actual_rf <- factor(ifelse(test_rf$loan_default==1,"yes","no"))

#finding the optimal cutoff value for probalility
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(test_pred_rf[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_actual_rf, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

# plotting cutoffs
plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
cutoff_rf
# 0.2673737

test_pred_optimal<- factor(ifelse(test_pred_rf[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(test_pred_optimal, test_actual_rf, positive = "yes")
conf_rf

# Accuracy : 78.5%
# Sensitivity : 8%       
# Specificity : 98%

####################### KS - statistic -Random Forest - Test Data######################## #######################

test_actual_rf<-ifelse(test_actual_rf == "yes", 1,0)
test_pred_optimal<-ifelse(test_pred_optimal == "yes", 1,0)

pred_object_test<- prediction(test_pred_optimal, test_actual_rf)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#KS-statistic is 15.4% 

# find area under the roc curve
roc <- performance(pred_object_test, measure = "auc")
area <- roc@y.values[[1]]
area 
#57.7%