library(dplyr)
library(caret)
library(lubridate)
library(caTools)
library(e1071)
library(data.table)
library(ROCR)
setwd("C:/Users/User/Desktop/Capstone Project/LoanStats3d.csv")
LC <- fread("Loan_Data.csv")

#Check the summary and structure
summary(LC)
str(LC)

#There are 4Id's missing from the observations 
#Therefore remove that observations from data
which(is.na(LC$member_id))
LC <- LC[-c(421096, 421097, 421098, 421099),]

#The data has 111 variables, for simplicity we will divide the data into 6 parts
#for further processing
part1 <- LC[,1:20]
part2 <- LC[,21:40]
part3 <- LC[,41:60]
part4 <- LC[,61:80]
part5 <- LC[,81:100]
part6 <- LC[,101:111]

#Analyzing "part1" features
summary(part1)

#Removing the variables which has no influence on building a model and has all uniques factors
#"id","member_id","pymnt_plan","url","desc","emp_title","grade","sub_grade"
part1[,c("id","member_id","pymnt_plan","url","desc","emp_title","grade","sub_grade")] <- NULL

#Mutate the variables in uselful form
part1$term <- as.numeric(sub(" months","", part1$term))
part1$emp_length <- sub("< 1 year",0, part1$emp_length)
part1$emp_length <- as.numeric(sub("\\+* year[s]*","",part1$emp_length))
part1$int_rate <- as.numeric(sub("%","",part1$int_rate))

#Convert Verified and source verified into one type Verified 
part1$verification_status <- as.character(part1$verification_status)
part1$verification_status <- ifelse(part1$verification_status == "Source Verified" |
                                      part1$verification_status == "Verified",
                                    "Verified",part1$verification_status )
part1$verification_status <- as.factor(part1$verification_status)

#Convert loan_status into good and bad indicators
part1$loan_status <- as.character(part1$loan_status)
part1$loan_status <- ifelse(part1$loan_status == "Charged Off" |
                              part1$loan_status == "Default" | 
                              part1$loan_status == "In Grace Period" |
                              part1$loan_status == "Late (16-30 days)" | 
                              part1$loan_status == "Late (31-120 days)",
                            0,1)
table(part1$loan_status)

#Check the structure
str(part1)



#Analysing part2
summary(part2)

#Removing the variables which has no influence on building a model and has all uniques factors
#"purpose","title","zip_code","addr_state"
part2[,c("purpose","title","zip_code","addr_state")] <- NULL

#Checking the percentage of NA's in each column
sapply(part2, function(x) sum(is.na(x))/length(x))*100

#"mths_since_last_delinq" & "mths_since_last_record" has more than 50% of NA's 
#remove those columns
part2[,c("mths_since_last_delinq", "mths_since_last_record")] <- NULL

#Mutate the variables in uselful form
part2$earliest_cr_line <- dmy(paste0("01-",part2$earliest_cr_line))
part2$revol_util <- as.numeric(sub("%","",part2$revol_util))

#Check the structure
str(part2)



#Analysing part3
summary(part3)

#Checking the percentage of NA's in each column
sapply(part3, function(x) sum(is.na(x))/length(x))*100

#"next_pymnt_d","mths_since_last_major_derog","annual_inc_joint","dti_joint",
#"verification_status_joint","open_acc_6m"
#remove those columns
part3[,c("next_pymnt_d","mths_since_last_major_derog","annual_inc_joint",
         "dti_joint","verification_status_joint","open_acc_6m")] <- NULL

#Mutate the variables in uselful form
part3$last_pymnt_d <- dmy(paste0("01-",part3$last_pymnt_d))
part3$last_credit_pull_d <- dmy(paste0("01-",part3$last_credit_pull_d))

#Remove columns which are dependent on above removed columns
part3[,c("policy_code","application_type")] <- NULL

#Check the structure
str(part3)



#Analysing part4
summary(part4)

#Checking the percentage of NA's in each column
sapply(part4, function(x) sum(is.na(x))/length(x))*100

#"open_il_6m","open_il_12m","open_il_24m","mths_since_rcnt_il","total_bal_il",
#"il_util","open_rv_12m","open_rv_24m","max_bal_bc","all_util","inq_fi","total_cu_tl",
#"inq_last_12m"
part4[,c("open_il_6m","open_il_12m","open_il_24m","mths_since_rcnt_il",
         "total_bal_il","il_util","open_rv_12m","open_rv_24m","max_bal_bc",
         "all_util","inq_fi","total_cu_tl","inq_last_12m")] <- NULL

#Replacing "bc_util", NA's value with mean
part4$bc_util[which(is.na(part4$bc_util))] <- mean(part4$bc_util,na.rm = T)

#Repalcing "bc_open_to_buy" , NA's value with median
part4$bc_open_to_buy[which(is.na(part4$bc_open_to_buy))] <- median(part4$bc_open_to_buy,na.rm = T)

#Check the structure
str(part4)



#Analysing part5
summary(part5)

#Checking the percentage of NA's in each column
sapply(part5, function(x) sum(is.na(x))/length(x))*100

#"mths_since_recent_bc_dlq","mths_since_recent_revol_delinq","num_tl_120dpd_2m"
#remove columns
part5[,c("mths_since_recent_bc_dlq","mths_since_recent_revol_delinq",
         "num_tl_120dpd_2m")] <- NULL

#Replacing NA's with apporpriate values
part5$mo_sin_old_il_acct[which(is.na(part5$mo_sin_old_il_acct))] <- 0
part5$mths_since_recent_bc[which(is.na(part5$mths_since_recent_bc))] <- 0
part5$mths_since_recent_inq[which(is.na(part5$mths_since_recent_inq))] <- mean(part5$mths_since_recent_inq, na.rm=T)

#Check the structure
str(part5)



#Analysing part6
summary(part6)

#Replacing NA's with appropriate values
part6$percent_bc_gt_75[which(is.na(part6$percent_bc_gt_75))] <- median(part6$percent_bc_gt_75, na.rm=T) 

#Check the structure
str(part6)



#Binding all the varaibles together
LC_new <- cbind(part1,part2,part3,part4,part5,part6)

#Changing the position of Target varaibles
LC_new <- LC_new[,c(1:11,13:73,12)]

#Removing all the columns which are in date format
LC_new[,c("issue_d","earliest_cr_line","last_pymnt_d","last_credit_pull_d")] <- NULL

#splitting the data into train and test
set.seed(123)
ind <- sample.split(LC_new$loan_status, SplitRatio = 0.8)
train <- subset(LC_new, ind==T)
test <-  subset(LC_new, ind==F)

#Building the model
model1 <- glm(loan_status ~ ., family = binomial(link = "logit"),
              data = train)
summary(model1)
predictCART1 <- predict(model1,newdata = test,type = "response")
prd <- ifelse(predictCART1<=0.5,0,1)
table(test$loan_status,prd)
pred <- prediction(prd, test$loan_status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

#Confusion Matrix
confusionMatrix(test$loan_status,prd)


#Building model
CART2 <- rpart::rpart(loan_status ~., data=train, method="class")
predictCART2 <- predict(CART2,newdata = test,type = "class")
table(test$loan_status,predictCART2)

#Confusion Matrix
confusionMatrix(test$loan_status,predictCART2)


