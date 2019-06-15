# clearing out memory and initializing time to calculate script running time
rm(list = ls())
gc()
start_time = Sys.time()

# importing the csv file

URL <- tempfile()
download.file("https://raw.githubusercontent.com/sonam-bhadauria/HarvardX-CYO/master/HR-Employee-Attrition.csv", URL)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#library(data.table)
HR_data <- read.csv(URL,sep = ',',header = TRUE,stringsAsFactors = TRUE,check.names = TRUE)
HR_data <- data.table(HR_data)
# Aim is to predict if an employee is going to resign or not
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#library(dplyr)
glimpse(HR_data)
summary(HR_data)

# Attrition No: 1233 and Yes: 237 shows unbalanced data. We will have to consider this when building a model
# Employee Count is 1 for all observations, we will remove this column
# Over 18 is Y for all employees, we will remove this column
# Standard Hours is 80 for all observation, we will remove this column

# Check for NA values
apply(is.na(HR_data), 2, sum)
# No NA's found (since sum = 0 for all columns)

# Since employee number can be used for feature generation about data of joining of employee. However in the absence of 
# any metadata we will remove it also
HR_data$EmployeeNumber <- NULL
HR_data$EmployeeCount <- NULL
HR_data$Over18 <- NULL
HR_data$StandardHours <- NULL

#C Check for Duplicated Records
sum(is.na(duplicated(HR_data)))
# No duplicates found

# Running summary again to check if we missed something
summary(HR_data)
# observations: Education, Environment Satisfaction, Job Involvement, Job Level, Job Satisfaction, Performance Rating,
# Relationship satisfaction, Stock Option Level, Work Life Balance are integers however can be considered as factors
# Modifying the above columns to factors
HR_data$Education <- as.factor(HR_data$Education)
HR_data$EnvironmentSatisfaction <- as.factor(HR_data$EnvironmentSatisfaction)
HR_data$JobInvolvement <- as.factor(HR_data$JobInvolvement)
HR_data$JobLevel <- as.factor(HR_data$JobLevel)
HR_data$JobSatisfaction <- as.factor(HR_data$JobSatisfaction)
HR_data$PerformanceRating <- as.factor(HR_data$PerformanceRating)
HR_data$RelationshipSatisfaction <- as.factor(HR_data$RelationshipSatisfaction)
HR_data$StockOptionLevel <- as.factor(HR_data$StockOptionLevel)
HR_data$WorkLifeBalance <- as.factor(HR_data$WorkLifeBalance)

#Visualizing the attrition
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#library(ggplot2)
HR_data %>%
  group_by(Attrition) %>%
  tally() %>%
  ggplot(aes(x = Attrition, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Attrition", y="Count of Attriation")+
  ggtitle("Attrition")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))
# 19% of the employees show attrition


# Visualizing age distribution in a histogram
ggplot(data=HR_data, aes(HR_data$Age)) + 
  geom_histogram(breaks=seq(20, 50, by=2), 
                 col="red", 
                 aes(fill=..count..))+
  labs(x="Age", y="Count")+
  scale_fill_gradient("Count", low="green", high="red")
# majority of employees between 28-36, with most popular in 34-36


# Attrition based on business travel
HR_data %>%
  ggplot(aes(x = BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = 2) +
  labs(y = "Percentage", fill= "business Travel") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")
# People who travel frequently tend to have more attrition

# attrition by gender
HR_data %>%
  ggplot(aes(x = Gender, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "Gender") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")
# no discernable observation

# Attrition by marital status
HR_data %>%
  ggplot(aes(x = MaritalStatus, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "MaritalStatus") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")
# people who are single tend to have highest attrition and people who are married tend to have the least chances

# Attrition by Monthly salary
HR_data %>%
  ggplot(mapping = aes(x = MonthlyIncome)) + 
  geom_histogram(aes(fill = Attrition), bins=20)+
  labs(x="Monthlt Income", y="Number Attriation")+
  ggtitle("Attrition in regards to Monthly Income")
# maximum attrition is with the people under 5000 

# Attrition based on Work Life Balance
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
#library(gridExtra)
g1<-HR_data %>%
  ggplot(aes(x = WorkLifeBalance, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "WorkLifeBalance") +
  facet_grid(~Attrition) +
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

g2<- HR_data %>%
  group_by(WorkLifeBalance, Attrition) %>%
  tally() %>%
  ggplot(aes(x = WorkLifeBalance, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))+
  labs(x="  Work Life Balance", y="Number Attriation")+
  ggtitle("Attrition in regards to  Work Life Balance")
grid.arrange(g1,g2)


# First stage Using Random Forest 

# Splitting into train test
rfData <- HR_data
set.seed(123)
indexes = sample(1:nrow(rfData), size=0.8*nrow(rfData))
RFRaw.train.Data <- rfData[indexes,]
RFRaw.test.Data <- rfData[-indexes,]

# Building the model
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
#library(randomForest)
Raw.rf.model <- randomForest(Attrition~.,RFRaw.train.Data, importance=TRUE,ntree=1000)
varImpPlot(Raw.rf.model)
# OverTime, Monthly Income , Age, Total Working Years and Job Role are the top 5  contributors
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#library(caret)
Raw.rf.prd <- predict(Raw.rf.model, newdata = RFRaw.test.Data)
confusionMatrix(RFRaw.test.Data$Attrition, Raw.rf.prd)
# Accuracy : 0.8605
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
#library(pROC)
plot.roc(as.numeric(RFRaw.test.Data$Attrition), as.numeric(Raw.rf.prd),lwd=2, type="b",print.auc=TRUE,col ="blue")
# we see the AUC is 0.582


# do some Feature Engineering
# 1) Make age group 18-24 > Young, 25-54 > Middle  and > 54 as Senior
HR_data$AgeGroup <- as.factor(
  ifelse(HR_data$Age<=24,"Young", ifelse(
    HR_data$Age<=54,"Middle","Senior"
  ))
)
table(HR_data$AgeGroup)
# so majority of employees come in "Middle"

# 2) Create a column "Total Satisfaction" which encompasses:
# Env Satisfacton, Job Involvement, Job Satisfaction, Relationship and WorkLife Balance

HR_data$TotlaSatisfaction <- 
  as.numeric(HR_data$EnvironmentSatisfaction)+
  as.numeric(HR_data$JobInvolvement)+
  as.numeric(HR_data$JobSatisfaction)+
  as.numeric(HR_data$RelationshipSatisfaction)+
  as.numeric(HR_data$WorkLifeBalance)

summary(HR_data$TotlaSatisfaction)

# 3) Study Years of total education
HR_data$YearsEducation <-  ifelse(HR_data$Education==1,10,ifelse(HR_data$Education==2,12,
                                                                 ifelse(HR_data$Education==3,16,ifelse(HR_data$Education==4,18,22))))  

table(HR_data$YearsEducation)
# the majority of employee are 16 years education (Bachelor)

# 4) Categorise monthly income into low-high based on average income
HR_data$IncomeLevel <- as.factor(
  ifelse(HR_data$MonthlyIncome<ave(HR_data$MonthlyIncome),"Low","High")
)
table(HR_data$IncomeLevel)


# LEts make a correlation matrix
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
#library(corrplot)
corrplot(cor(sapply(HR_data,as.integer)),method = "pie")
# we can see some variables are highly correlated
# Joblevel and monthly income, Education and YearsEducation


# Lets make a New Random Forest
rfData <- HR_data
set.seed(123)
indexes = sample(1:nrow(rfData), size=0.8*nrow(rfData))
RFtrain.Data <- rfData[indexes,]
RFtest.Data <- rfData[-indexes,]

rf.model <- randomForest(Attrition~.,RFtrain.Data, importance=TRUE,ntree=500)
varImpPlot(rf.model)

# Here OverTime, TotalSatisfaction,Monthly Income, Age and Job Role are top 5
rf.prd <- predict(rf.model, newdata = RFtest.Data)
confusionMatrix(RFtest.Data$Attrition, rf.prd) #Accuracy = 0.8605
plot.roc (as.numeric(RFtest.Data$Attrition), as.numeric(rf.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
# We observe our  AUC has improved to 0.598 over the last RF with Raw Data

# Trying other Algorithms

# Trying Support Vector Machine
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
#library(e1071)
svmData <- HR_data
set.seed(123)
indexes = sample(1:nrow(svmData), size=0.8*nrow(svmData))
SVMtrain.Data <- svmData[indexes,]
SVMtest.Data <- svmData[-indexes,]
tuned <- tune(svm,factor(Attrition)~.,data = SVMtrain.Data)
svm.model <- svm(SVMtrain.Data$Attrition~., data=SVMtrain.Data
                 ,type="C-classification", gamma=tuned$best.model$gamma
                 ,cost=tuned$best.model$cost
                 ,kernel="radial")
svm.prd <- predict(svm.model,newdata=SVMtest.Data)
confusionMatrix(svm.prd,SVMtest.Data$Attrition) 

svm.plot <-plot.roc (as.numeric(SVMtest.Data$Attrition), as.numeric(svm.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
# We observce our AUC(0.561) and Accuracy (0.8537) compared to RF is bad
# There are a lot of false positives


# Trying Extreme Gradient Boost
# USing best tunned hyperprameters(as shown in project report - 
# not included in code here as it takes more than 1 hour to execute)
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
#library(xgboost)
set.seed(123)
xgbData <- HR_data
indexes <- sample(1:nrow(xgbData), size=0.8*nrow(xgbData))
XGBtrain.Data <- xgbData[indexes,]
XGBtest.Data <- xgbData[-indexes,]

formula = Attrition~.
fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )

xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 15,
                       eta = .05,
                       gamma = 0.1,
                       colsample_bytree = .8,
                       min_child_weight = 3,
                       subsample = 0.75
)
XGB.model <- train(formula, data = XGBtrain.Data,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)
importance <- varImp(XGB.model)
varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                            Importance = round(importance[[1]]$Overall,2))
# Create a rank variable based on importance of variables
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity',colour="white", fill = "lightgreen") +
  geom_text(aes(x = Variables, y = 1, label = Rank),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_bw()

# We can observe that Monthly Income, OverTime, Total Satisfaction, Total Working Years and Daily Rate are top 5

XGB.prd <- predict(XGB.model,XGBtest.Data)
confusionMatrix(XGB.prd, XGBtest.Data$Attrition)
XGB.plot <- plot.roc (as.numeric(XGBtest.Data$Attrition), as.numeric(XGB.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
# We can see that accuracy is 0.8673 and AUC us 0.635

# Lets solve the unbalance problem in the Dataset
# Using SMOTE method
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
#library(DMwR)
Classcount = table(HR_data$Attrition)
# Over Sampling
over = ( (0.6 * max(Classcount)) - min(Classcount) ) / min(Classcount) #2.121
# Under Sampling
under = (0.4 * max(Classcount)) / (min(Classcount) * over) # 0.98

over = round(over, 1) * 100 #210
under = round(under, 1) * 100 #100
#Generate the balanced data set
BalancedData = SMOTE(Attrition~., HR_data, perc.over = over, k = 5, perc.under = under)
# lets check the output of the Balancing
BalancedData %>%
  group_by(Attrition) %>%
  tally() %>%
  ggplot(aes(x = Attrition, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Attrition", y="Count of Attriation")+
  ggtitle("Attrition")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))


# Now running XGBoost again with the balanced data

# USing best tunned hyperprameters(as shown in project report - 
# not included in code here as it takes more than 1 hour to execute)
set.seed(123)
xgbData <- BalancedData
indexes = sample(1:nrow(xgbData), size=0.8*nrow(xgbData))
BLtrain.Data <- xgbData[indexes,]
BLtest.Data <- xgbData[-indexes,]

formula = Attrition~.
fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )
NewxgbGrid <- expand.grid(nrounds = 800,
                          max_depth = 2,
                          eta = .05,
                          gamma = 0,
                          colsample_bytree = 0.8,
                          min_child_weight = 2,
                          subsample = 0.5
)

# from tunegrid 5
NewxgbGrid <- expand.grid(nrounds = 3000,
                          max_depth = 5,
                          eta = .05,
                          gamma = 0.1,
                          colsample_bytree = 0.6,
                          min_child_weight = 2,
                          subsample = 0.5
)


NewXGB.model = train(formula, data = BLtrain.Data,
                     method = "xgbTree"
                     ,trControl = fitControl
                     , verbose=0
                     , maximize=FALSE
                     ,tuneGrid = NewxgbGrid
                     ,na.action = na.pass
)

importance <- varImp(NewXGB.model)
varImportance <- data.frame(Variables = row.names(importance[[1]]),
                            Importance = round(importance[[1]]$Overall,2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance)) +
  geom_bar(stat='identity',colour="white", fill = "lightgreen") +
  geom_text(aes(x = Variables, y = 1, label = Rank),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() +
  theme_bw()

NewXGB.prd <- predict(NewXGB.model,BLtest.Data)
confusionMatrix(NewXGB.prd, BLtest.Data$Attrition) # 0.8903

#In my other macbook the same code gave an accuracy of .8974
#optimizer may find a different local minimum, so the accuracy could differ in different computers

NXGB.plot <- plot.roc (as.numeric(BLtest.Data$Attrition), as.numeric(NewXGB.prd),lwd=2, type="b", print.auc=TRUE, col ="green")


# plotting all the metrics together
par(mfrow=c(2,3))
plot.roc (as.numeric(XGBtest.Data$Attrition), as.numeric(XGB.prd),main="XGBoost",lwd=2, type="b", print.auc=TRUE, col ="blue")
plot.roc (as.numeric(BLtest.Data$Attrition), as.numeric(NewXGB.prd),main="New XGBoost",lwd=2, type="b", print.auc=TRUE, col ="green")
plot.roc (as.numeric(SVMtest.Data$Attrition), as.numeric(svm.prd),main="SVM",lwd=2, type="b", print.auc=TRUE, col ="red")
plot.roc (as.numeric(RFRaw.test.Data$Attrition), as.numeric(Raw.rf.prd), main="Random Forest",lwd=2, type="b", print.auc=TRUE, col ="seagreen4")
plot.roc (as.numeric(RFtest.Data$Attrition), as.numeric(rf.prd), main="Raw Data Random Forest",lwd=2, type="b", print.auc=TRUE, col ="seagreen")

end_time  = Sys.time()
print(end_time - start_time) #1.31 mins