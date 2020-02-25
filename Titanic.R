# Import the data set in R, create a variable called titanic & store the data in it.

titanic <- read.csv("/home/sunbeam/R_loveeshbhat_imarticus/programs/day18practice/Titanic.csv")
head(titanic)

# Print the structure of dataset.

str(titanic)

# Print last 6 rows of dataset.

tail(titanic,6)

# Create a barplot using ggplot with class on x-axis, frequency on y -axis & sex as the fill, write your 
# observation.

library(ggplot2)
ggplot(titanic, aes(x = Pclass, fill = Sex)) + geom_bar()

## OBSERVATION :- As compared to first two Pclass’s,the third Pclass has more number of male and female 
##                count.

# Use ggplot() to estimate your chances of servival from the distibution of sexes within the classes of 
# the ship; Hint: use fact grids.

ggplot(titanic, aes(x = Pclass, fill = Sex)) + geom_bar(position = "dodge") + facet_grid(~Survived)

# Use ggplot() to estimate your chances of survival based on your age from the distribution of sexes 
# within the class of the ship; Hint: Use the above plot & overlay age using jitter plot .

posn.j <- position_jitter(0.5, 0)
ggplot(titanic,aes(x=factor(Pclass),y=Age,col=factor(Sex)))+
  geom_jitter(size=3,alpha=0.5,position=posn.j)+
  facet_grid(". ~ Survived")

# Import the training dataset using the following link and print the structure to console 
# “http://50.amazonaws.com/assets.datacamp.com/course/kaggle/train.csv”

train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)
str(train)

# Import the testing dataset using the following link & print the structure to console 
# “http://50.amazonaws.com/assets.datacamp.com/course/kaggle/train.csv”

test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)
str(test)

# Print the survival rates in absolute numbers for train.

table(train$Survived)

# Print th survival rates in proportion for train.

prop.table(table(train$Survived))

# Print a two way comparision table for sex and survived

table(train$Sex, train$Survived)

# Print a two way comparision for sex and survived, show the proportions row wise i.e for Male & Female

prop.table(table(train$Sex, train$Survived),margin =1)

# Create a column called child using the condition age < 18

library(dplyr)
train$Child <- NA
train$Child[train$Age < 18] <- "1"
train$Child[train$Age >= 18] <- "0"
head(train)

# Print a two way comparison table to show the proportion of this child variable with respect to survival

prop.table(table(train$Child,train$Survived))

# Using rpart build a decision tree on the training dataset Dependent Vaar : Survived 
# Independent var : Pclass,sex,age,parch,fare,embarked

library(lattice)
library(caret)
train$Survived=as.factor(train$Survived)
tree <- train(Survived  ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, 
              method = "rpart", na.action = na.exclude)
plot(tree$finalModel,uniform = T,main="decison tree")
text(tree$finalModel,use.n = T,all=T,cex=.8)

# Plot your decision tee using fancyrpart plots

library(rattle)
fancyRpartPlot(tree$finalModel)

# Make predictions on the test set

prediction <- predict(tree, test,type = "prob")
head(prediction)