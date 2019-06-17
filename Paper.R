# Load Data-set 
url <- "https://github.com/jdb443/ph125.9x_Capstone_Project/blob/master/grownhuman.csv?raw=true"
salary<- read.csv(url)


# Load Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")

library(corrplot)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(data.table)
library(ROCR)
library(rpart)
library(randomForest)


#Data

# General dataset properties
str(salary)
salary[salary == "?"] <- NA
sapply(salary,function(x) sum(is.na(x)))


# Dependent Variables
# The categorical outcome to be predicted is income. It has two categories greater than $50,000 and less than or equal to 50000, with 24.080% of the entries representing individuals earning more than $50,000 a year and 75.919% representing individuals earning less than $50000 a year. Based on over 32000 data points.
salary %>% group_by(income) %>% summarize(Count=n()) %>% knitr::kable()



# Independent Variables Predictors
names(salary)


# Unique factors
unique<- t(data.table(age= n_distinct(salary$age), Workclass=n_distinct(salary$workclass), fnlwgt=n_distinct(salary$fnlwgt), education=n_distinct(salary$education), educationnum=n_distinct(salary$education.num), 
                      maritalstatus=n_distinct(salary$marital.status), occupation=n_distinct(salary$occupation), 
                      relationship=n_distinct(salary$relationship), race=n_distinct(salary$race), sex=n_distinct(salary$sex), 
                      capitalgain=n_distinct(salary$capital.gain), capitalloss=n_distinct(salary$capital.loss),
                      hoursperweek=n_distinct(salary$hours.per.week), nativecountry=n_distinct(salary$native.country)))

colnames(unique)<-"Unique Levels"

# Table to display unique factors
data.matrix(unique) %>% knitr::kable()


# Frequency of Predictors

p1<-salary%>% na.omit %>% ggplot(aes(age)) +  geom_histogram(bins=30, fill="azure", color="dodgerblue")
p2<-salary%>% na.omit %>% ggplot(aes(capital.gain)) +  geom_histogram(bins=30, fill="azure", color="dodgerblue") + ylab("")
p3<-salary%>% na.omit %>% ggplot(aes(capital.loss)) +  geom_histogram(bins=30, fill="azure", color="dodgerblue")
p4<-salary%>% na.omit %>% ggplot(aes(hours.per.week)) +  geom_histogram(bins=30, fill="azure", color="dodgerblue") + ylab("")
p5<-salary%>% na.omit %>% ggplot(aes(education.num)) +  geom_histogram(bins=30, fill="azure", color="dodgerblue")
grid.arrange(p1,p2,p3,p4,p5, layout_matrix=rbind(c(1,2), c(3,4), c(5)))

# Locating the presence of zeroes/null data points
sum(salary$capital.gain==0)/length(salary$capital.gain)
sum(salary$capital.gain==0)
sum(salary$capital.loss==0)/length(salary$capital.loss)
sum(salary$capital.loss==0)


# Correlation of Predictors
round(cor(salary[c(1,5,13)]),3) %>% knitr::kable()
x <- round(cor(salary[c(1,5,13)]),3) 
corrplot(x, type="upper", order="hclust")


# Correlation between Predictors and Income value
p1 <- ggplot(aes(x=income, y=age), data = salary) + geom_boxplot()  +  
  ggtitle('Age vs. Income')
p2 <- ggplot(aes(x=income, y=education.num), data = salary) + geom_boxplot() + 
  ggtitle('Education vs. Income')
p3 <- ggplot(aes(x=income, y=hours.per.week), data = salary) + geom_boxplot() + 
  ggtitle('Hours vs. Income ')
grid.arrange(p1, p2, p3, ncol=3)
p4 <- ggplot(aes(x=sex, y=income), data = salary) + geom_boxplot() + ggtitle('Gender vs. Income')



# Exploration of the predictors 
p1<- salary %>% na.omit %>% group_by(sex, income) %>% summarise(n=n()) %>% ggplot(aes(x=sex, y=n, fill=income)) + geom_bar(stat = 'identity',  position= position_stack(vjust=0.5))  +  theme(panel.background = element_rect(fill = "wheat"), panel.border = element_rect(fill = NA, color = "darkgreen"))+ ggtitle("Sex vs Income")

p2 <- salary %>% na.omit %>% group_by(workclass, income) %>% summarise(n=n()) %>%   ggplot(aes(x=workclass, y=n, fill=income))  + geom_bar(stat = 'identity',  position= position_stack(vjust=0.5))  + theme(panel.background = element_rect(fill = "wheat"), panel.border = element_rect(fill = NA, color = "darkgreen")) +   theme(axis.text.x = element_text(angle = 60, hjust = 1)) + coord_flip()+ ggtitle("Workclass vs Income")  

p3<- salary %>% na.omit %>% group_by(occupation, income) %>% summarise(n=n()) %>% ggplot(aes(x=occupation, y=n, fill=income))  + geom_bar(stat = 'identity',  position= position_stack(vjust=0.5))  + theme(panel.background = element_rect(fill = "wheat"), panel.border = element_rect(fill = NA, color = "darkgreen")) +  coord_flip()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +ggtitle("Occupation vs Income") 

p4<- salary %>% na.omit %>% group_by(race, income) %>% summarise(n=n()) %>%   ggplot(aes(x=race, y=n, fill=income))  + geom_bar(stat = 'identity',  position= position_stack(vjust=0.5)) + theme(panel.background = element_rect(fill = "wheat"), panel.border = element_rect(fill = NA, color = "darkgreen")) +  coord_flip()+  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Race vs Income") 

p5<-salary %>% na.omit %>% group_by(marital.status, income) %>% summarise(n=n()) %>%   ggplot(aes(x=marital.status, y=n, fill=income))  + geom_bar(stat = 'identity',  position= position_stack(vjust=0.5)) + theme(panel.background = element_rect(fill = "wheat"), panel.border = element_rect(fill = NA, color = "darkgreen")) +  coord_flip() +   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Marital Status vs Income")

p6<-salary %>% na.omit %>% group_by(native.country, income) %>% summarise(n=n()) %>%ggplot(aes(x=native.country, y=n, fill=income))  + geom_bar(stat = 'identity',  position= position_stack(vjust=0.5))+ theme(panel.background = element_rect(fill = "wheat"), panel.border = element_rect(fill = NA, color = "darkgreen")) +   theme(axis.text.x = element_text(angle = 90, hjust = 1))  + ggtitle("Country vs Income") 

grid.arrange(p1,p2,p3,p4,p5,p6, layout_matrix=rbind(c(1,2), c(3), c(4,5), c(6)))




# Removing predictor columns
salary<-select(salary, -c(fnlwgt, education, relationship, capital.gain, capital.loss, native.country))
salary<-salary%>% na.omit()

# Creating factors from the income variables
salary$income = as.factor(ifelse(salary$income==salary$income[1],0,1))


# Partition of data
set.seed(1)
test_index <- createDataPartition(y = salary$income, times = 1, p = 0.10, list = FALSE)
train <- salary %>% slice(-test_index)
test <- salary %>% slice(test_index)



# Fit Model


# GLM: Logistic Regression
fitglm <-glm(income ~.,family=binomial(link='logit'),data=train)
phat<- predict(fitglm, test, type = "response")
yhat<- ifelse(phat>0.5, 1,0) %>% factor()
confusionMatrix(yhat, test$income)$overall["Accuracy"]


# KNN: k-Nearest Neighbor 
fitknn <- train(income ~ ., method = "knn", tuneGrid = data.frame(k = seq(1, 10, 2)), data = train)
ggplot(fitknn, highlight = TRUE)
fitknn$bestTune

fitknn <-knn3(income ~.,data=train, k=9)
yhatknn<-predict(fitknn, test, type="class")
confusionMatrix(yhatknn, test$income)$overall["Accuracy"]

# Recursive Partitioning and Regression Trees: RPART
fitrpart<-train(income~., method="rpart", data=train)
yhat<-predict(fitrpart, test, type="raw")
confusionMatrix(yhat, test$income)$overall["Accuracy"]

# Random Forest
fitrf<- randomForest(income~.,data= train)
yhat<-predict(fitrf, test, type="class")
confusionMatrix(yhat, test$income)$overall["Accuracy"]

