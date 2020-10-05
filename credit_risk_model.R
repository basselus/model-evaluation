
#***************************************************************************
#*********************** Load Necessary Packages ***************************
#***************************************************************************


packages = c("mice","tuneRF","funModeling","tidyverse","caret","dplyr", "psych")

package_check <- lapply(packages, FUN = function(x){
  
  if (!require(x, character.only=T)) {
    
    install.packages(x, dependencies = T)  
    library(x, character.only=T)
  }
  
})



#***************************************************************************
#*********************** Load Dataset **************************************
#***************************************************************************



credit=read.table("credit.txt", sep = ',', header = T)




#***************************************************************************
#*********************** Check Structure and clealiness ********************
#***************************************************************************

colnames(credit)
summary(credit)
status=df_status(credit, print_results = F) # this shows no missind data (na percentage is null)
status
describe(credit)
str(credit)
dim(credit)
glimpse(credit)
#***************************************************************************
#*********************** Profile numerical variables ***********************
#***************************************************************************

profiling_num(credit)
plot_num(credit)


#***************************************************************************
#*********************** Profile categorical variables *********************
#***************************************************************************

freq(credit)


#***************************************************************************
#*********************** Check correlated numeric features *****************
#***************************************************************************


pairs.panels(credit[,c(2,5,8,9,10,13,15)])

# Possible correlation between amount and month loan duration

#***************************************************************************
#*********************** Split data between train and test sets ************
#***************************************************************************

#training set will be 70% of the sample size
smp_size_2=floor(0.70*nrow(credit))

#set the seed to make the partition reproducible
train_rows_2=sample(seq_len(nrow(credit)), size = smp_size_2)

train<-credit[train_rows_2,]
test<-credit[-train_rows_2,]

#checking proportions across train and test
prop.table(table(train$default))
prop.table(table(test$default))
table(train$default)

#***************************************************************************
#*********************** Identify driving variables ************************
#*********************** Logistic regression *******************************
#***************************************************************************


#training a model : #removing split feature and dependent variable

creditLogReg <- glm(train$default ~ ., data = train[,c(-17,-18)], family = "binomial" ) 
summary(creditLogReg)

# use the confint function to obtain confidence intervals for the coefficient estimates

confint(creditLogReg)


# using standard errors

confint.default(creditLogReg)
