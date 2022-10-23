#ALY6015 Group Project
# Class: ALY6015-21788
#Team Member: Kejiang Yao, Xiaozhu Zhao, Yu Qiu
# Date: 02/19/2022


library(dplyr)
library(tidyr)
library(psych)
library(readr)
library(ggplot2)
library(glmnet)
library(Metrics)

CollegeScore <- read_csv("Group/CollegeScorecard.csv")
View(CollegeScore)

options(scipen = 999)

#Select variables needed
college <- CollegeScore %>%
  select(name, state, predominantdegree, highestdegreegranted, controlofinstitution, mediandebt, agebegin, femalepct, year, undergraduateenrollment, whiteundergraduatepct, blackundergraduatepct, hispanicundergraduatepct, asianpacificislander, nativeamalundergraduatepct, parttimepct, completionrate)

str(college)
summary(is.na(college))

college$mediandebt <- as.numeric(college$mediandebt)
college$femalepct <- as.numeric(college$femalepct)
college$agebegin <- as.numeric(college$agebegin)

#Remove the row with categorical missing values
college2 <- college[!is.na(college$controlofinstitution),]

#Impute missing data with mean
college2$mediandebt[is.na(college2$mediandebt)] <- mean(college2$mediandebt, na.rm = T)
college2$agebegin[is.na(college2$agebegin)] <- mean(college2$agebegin, na.rm = T)
college2$femalepct[is.na(college2$femalepct)] <- mean(college2$femalepct, na.rm = T)
college2$undergraduateenrollment[is.na(college2$undergraduateenrollment)] <- median(college2$undergraduateenrollment, na.rm = T)
college2$whiteundergraduatepct[is.na(college2$whiteundergraduatepct)] <- mean(college2$whiteundergraduatepct, na.rm = T)
college2$blackundergraduatepct[is.na(college2$blackundergraduatepct)] <- mean(college2$blackundergraduatepct, na.rm = T)
college2$hispanicundergraduatepct[is.na(college2$hispanicundergraduatepct)] <- mean(college2$hispanicundergraduatepct, na.rm = T)
college2$asianpacificislander[is.na(college2$asianpacificislander)] <- mean(college2$asianpacificislander, na.rm = T)
college2$nativeamalundergraduatepct[is.na(college2$nativeamalundergraduatepct)] <- mean(college2$nativeamalundergraduatepct, na.rm = T)
college2$parttimepct[is.na(college2$parttimepct)] <- mean(college2$parttimepct, na.rm = T)
college2$completionrate[is.na(college2$completionrate)] <- mean(college2$completionrate, na.rm = T)

sum(is.na(college2))

#Descriptive Statistics 
psych::describe(college2, omit = T) %>%
  select(n, mean, sd, median, min, max)

#Add new variable Region
NE <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
MW <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
S <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL","KY","MS","TN","AR","LA","OK","TX")
W <- c("AZ","CO","ID","NM","MT","UT","NV","WY","CA","OR","WA", "HI")

region <- list(NorthEast =NE , MidWest = MW, South = S, West = W)

college2$region <- sapply(college2$state, function(state) names(region)[grep(state, region)])

college2 <- unnest(college2, region, keep_empty = TRUE)

str(college2)

sum(is.na(college2))

college2 <- na.omit(college2)

#Add new variable Ownership
college2$ownership <- ifelse(college2$controlofinstitution == "Public", "Public", "Private")
table(college2$ownership)

#SubGroup Descriptive Statistics by Region

Wdata <- subset(college2, region == "West")
MWdata <- subset(college2, region == "MidWest")
Sdata <- subset(college2, region == "South")
NEdata <- subset(college2, region == "NorthEast")

Wdes <- psych::describe(Wdata, omit = T) %>%
  select(n, mean, sd)

MWdes <- psych::describe(MWdata, omit = T) %>%
  select(n, mean, sd)

Sdes <- psych::describe(Sdata, omit = T) %>%
  select(n, mean, sd)

NEdes <- psych::describe(NEdata, omit = T) %>%
  select(n, mean, sd)

subdes <- cbind(round(Wdes,2), round(MWdes,2), round(Sdes,2), round(NEdes,2))
subdes

#SubGroup Descriptive Statistics by Control

private <- subset(college2, ownership == "Private")
public <- subset(college2, ownership == "Public")

pubdes <- psych::describe(public, omit = T) %>%
  select(n, mean, sd)
prides <-psych::describe(private, omit = T) %>%
  select(n, mean, sd)

subdes2 <- cbind(round(pubdes,2), round(prides,2))
subdes2

##########
#EDA
##########

#categorical 3,4,5,9,18,19

#predominantdegree
counts <- table(college2$predominantdegree)
counts <- sort(counts, decreasing = TRUE)
counts
par(mfrow = c(1,1), mar=c(4,4,2,1))
barplot(counts, main="Predominant Degree",
        ylab="Number of school", ylim = c(0,60000), cex.axis = .5, cex.names =.8,
        names.arg=c("Certificate", "Bachelor", "Associate","Graduate","Non"))
#highestdegreegranted
counts <- table(college2$highestdegreegranted)
counts <- sort(counts, decreasing = TRUE)
counts
barplot(counts, main="Highest Degree Granted",
        ylab="Number of school", ylim = c(0,50000), cex.axis = .5, cex.names = .8,
        names.arg=c("Certificate", "Graduate", "Associate","Bachelor","Not class"))
#controlofinstitution
counts <- table(college2$controlofinstitution)
counts <- sort(counts, decreasing = TRUE)
counts
barplot(counts, main="Control of Institution",
        ylab="Number of school", ylim = c(0,60000), cex.axis = .5, cex.names = .8)
#region
counts <- table(college2$region)
counts <- sort(counts, decreasing = TRUE)
counts
barplot(counts, main="Region",
        ylab="Number of school", ylim = c(0,50000), cex.axis = .5, cex.names = .8)

#ownership
counts <- table(college2$ownership)
counts <- sort(counts, decreasing = TRUE)
counts
barplot(counts, main="Ownership",
        ylab="Number of school", ylim = c(0,100000), cex.axis = .5, cex.names = .8)

#6,7,8,10,11-17
#median debt
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$mediandebt, horizontal = T, ylim = c(0, 45000))
hist(college2$mediandebt, las = 1, xlim = c(0, 45000), main = "", xlab = "Median Debt", breaks = 50, ylab = "")
#age begin
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$agebegin, horizontal = T, ylim = c(0, 100))
hist(college2$agebegin, las = 1, xlim = c(0, 100), main = "", xlab = "Age Begin", breaks = 50, ylab = "")
#female percentage
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$femalepct, horizontal = T, ylim = c(0, 1))
hist(college2$femalepct, las = 1, xlim = c(0, 1), main = "", xlab = "Female percentage", breaks = 50, ylab = "")
#undergraduate enrollment
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$undergraduateenrollment, horizontal = T, ylim = c(0, 10000))
hist(college2$undergraduateenrollment, las = 1, xlim = c(0, 250000), main = "", xlab = "Undergraduate enrollment", breaks = 50, ylab = "")

#white undergraduate pct
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$whiteundergraduatepct, horizontal = T, ylim = c(0, 1))
hist(college2$whiteundergraduatepct, las = 1, xlim = c(0, 1), main = "", xlab = "White undergraduate pct", breaks = 50, ylab = "")

#black undergraduate pct
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$blackundergraduatepct, horizontal = T, ylim = c(0, 1))
hist(college2$blackundergraduatepct, las = 1, xlim = c(0, 1), main = "", xlab = "Black undergraduate pct", breaks = 50, ylab = "")

#hispanic undergraduate pct
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$hispanicundergraduatepct, horizontal = T, ylim = c(0, 1))
hist(college2$hispanicundergraduatepct, las = 1, xlim = c(0, 1), main = "", xlab = "Hispanic undergraduate pct", breaks = 50, ylab = "")

#asian pacific islander
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$asianpacificislander, horizontal = T, ylim = c(0, 1))
hist(college2$asianpacificislander, las = 1, xlim = c(0, 1), main = "", xlab = "Asian pacific islander", breaks = 50, ylab = "")

#native amal undergraduate pct
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$nativeamalundergraduatepct, horizontal = T, ylim = c(0, 1))
hist(college2$nativeamalundergraduatepct, las = 1, xlim = c(0, 1), main = "", xlab = "Native amal undergraduate pct", breaks = 50, ylab = "")

#Part time pct
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$parttimepct, horizontal = T, ylim = c(0, 1))
hist(college2$parttimepct, las = 1, xlim = c(0, 1), main = "", xlab = "Part time pct", breaks = 50, ylab = "")

#completion rate
par(mfrow = c(2,1), cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
boxplot(college2$completionrate, horizontal = T, ylim = c(0, 1))
hist(college2$completionrate, las = 1, xlim = c(0, 1), main = "", xlab = "Completion rate", breaks = 50, ylab = "")


attach(college2)

qplot(x=undergraduateenrollment, y=completionrate, color=ownership, shape=ownership, geom="point" ) + scale_shape(solid = FALSE)


#boxplot
qplot(x=ownership, y=completionrate, fill=ownership, geom = "boxplot") +guides(fill=FALSE) + ylab("Completion rate")
# create a dataset
Region <- c(rep("Midwest" , 2) , rep("Northeast" , 2) , rep("South" , 2) , rep("West" , 2) )
Ownership <- rep(c("Public" , "Private") , 4)
value <- c(43.65,57.03,47.51,61.23,38.03,56.02,35.86,60.46)
data <- data.frame(Region,Ownership,value)
ggplot(data, aes(fill=Ownership, y=value, x=Region)) + 
  geom_bar(position="dodge", stat="identity") + ylab("Completion rate") + theme_bw()

#boxplot ownership vs ethnicity group
a <- qplot(x=ownership, y=whiteundergraduatepct, fill=ownership, geom = "boxplot") +guides(fill=FALSE) + ylab("White student percentage")+ theme_bw()

b <- qplot(x=ownership, y=blackundergraduatepct, fill=ownership, geom = "boxplot") +guides(fill=FALSE)+ylim(c(0,0.4))+ ylab("Black student percentage")+ theme_bw()

c <- qplot(x=ownership, y=hispanicundergraduatepct, fill=ownership, geom = "boxplot") +guides(fill=FALSE) +ylim(c(0,0.3))+ ylab("Hispanic student percentage")+ theme_bw()

d <- qplot(x=ownership, y=asianpacificislander, fill=ownership, geom = "boxplot") +guides(fill=FALSE)+ylim(c(0,0.1))+ ylab("Asian student percentage")+ theme_bw()

e <- qplot(x=ownership, y=nativeamalundergraduatepct, fill=ownership, geom = "boxplot") +guides(fill=FALSE)+ylim(c(0,0.05))+ ylab("Native American student percentage")+ theme_bw()
library(gridExtra)
grid.arrange(a,b,c,d,e, nrow = 2)



average_median_debt <- c(7568,5987,6057,6109,6137,6157,6236,6316,6445,6581,6694,6907,8323,8764,9187,9747,10319,10808)
years <- c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2010,2011,2012,2013,2014,2015)
data <- data.frame(average_median_debt,years)

ggplot(data=data, aes(x=years, y=average_median_debt, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point() + theme_bw() + xlab("Year") +ylab("Average Median Debt")

average_completion_rate <- c(52.99,52.73,51.89,51.92,52.41,53.84,53.55,53.27,53.49,53.48,53.06,52.36,52.98,53.29,52.99,53.13,53.07,53.18)
data <- data.frame(average_completion_rate,years)

ggplot(data=data, aes(x=years, y=average_completion_rate, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point() + theme_bw() + xlab("Year") +ylab("Average Completion Rate")


################
#ANOVATest#
#################
#The average median Debt is dependent on the ownership of the institutions

interaction <- aov(completionrate ~ ownership * region, data = college2)
summary(interaction)

#interaction plot
par(mar = c(5,4,3,4), cex =0.9)
interaction.plot(x.factor = college2$region, trace.factor = college2$ownership, response = college2$completionrate, fun = mean, ylab = "Completion Rate", xlab = "Regions", col = c("red", "blue"), lty =1, lwd =2, trace.label = "University Ownership", leg.bty = "o", las =1)

#Tukey Test
tukey <- TukeyHSD(interaction)

par(cex = 0.9, mai = c(1,3,1,1))
plot(tukey, las =1)




####################
#Linear Regression
####################

#Split Data

set.seed(3)
trainindex <- sample(x = nrow(college2), size = nrow(college2)*0.7)

train <- college2[trainindex, ]%>%
  select(predominantdegree, highestdegreegranted, mediandebt, agebegin, femalepct, undergraduateenrollment, whiteundergraduatepct, blackundergraduatepct, hispanicundergraduatepct, asianpacificislander, nativeamalundergraduatepct, parttimepct, completionrate, ownership, region)

test <- college2[-trainindex, ]%>%
  select(predominantdegree, highestdegreegranted, mediandebt, agebegin, femalepct, undergraduateenrollment, whiteundergraduatepct, blackundergraduatepct, hispanicundergraduatepct, asianpacificislander, nativeamalundergraduatepct, parttimepct, completionrate, ownership, region)

trainX <- Matrix::sparse.model.matrix(completionrate ~., train) [, -1]
testX <- Matrix::sparse.model.matrix(completionrate ~., test)[, -1]

dim(trainX)
dim(testX)

trainY <- train$completionrate
testY <- test$completionrate

#Apply Lasso Regression
lassoreg <- glmnet(x = trainX, y = trainY, alpha = 1)

plot(lassoreg, xvar = "lambda", las =1)


lassoreg$lambda

#Cross Validation : Find the best values of lambda
set.seed(4)
lasso <- cv.glmnet(trainX, trainY, nfolds = 10)
plot(lasso, las =1)
lasso
#lambda min
log(lasso$lambda.min)

#lambda 1 sd. error of lambda min
log(lasso$lambda.1se)


#Plot again the lasso model with the 2 lambda lines
plot(lassoreg, xvar = "lambda", las =1)
abline(v = log(lasso$lambda.1se), col = "red", lty = "dashed")
abline(v = log(lasso$lambda.min), col = "red", lty = "dashed")

#Create the model and find the coefficients
model1se <- glmnet(trainX, trainY, alpha = 1, lambda = lasso$lambda.1se)
coef(model1se)

#Calculate RMSE and Check Over-fitting 
predTrain <- predict(model1se, newx = trainX)
trainrmse <- rmse(trainY, predTrain)

predTest <- predict(model1se, newx = testX)
testrmse <- rmse(testY, predTest)

sqrt(mean((testY - predTest)^2))

trainrmse
testrmse



#Calculate R-Squared Value
y <- test$completionrate
y2 <- train$completionrate
sst <- sum((y - mean(y))^2)
sse <- sum((predTest - y)^2)
rsq <- 1 - sse/sst
rsq

sst2 <- sum((y2 - mean(y2))^2)
sse2 <- sum((predTrain - y2)^2)
rsq2 <- 1 - sse2/sst2
rsq2

#Adjusted R-square
1- ((1-rsq2)*(85710-1)/(85710 - 17 -1))



#Stepwise 

model2 <- lm(completionrate ~ as.factor(predominantdegree) + as.factor(highestdegreegranted) + mediandebt + agebegin + femalepct + undergraduateenrollment + whiteundergraduatepct + blackundergraduatepct + hispanicundergraduatepct + asianpacificislander + nativeamalundergraduatepct + parttimepct + as.factor(ownership) + as.factor(region), data = train)

summary(model2)

#Stepwise Selection

models <- lm(completionrate ~ 1, data = train)

stepwise <- step(models, direction = "both", scope = formula(model2))
summary(stepwise)


#Calculate RMSE 
predTrain2 <- predict(stepwise, newdata = train)
train2rmse <- rmse(train$completionrate, predTrain2)

predTest2 <- predict(stepwise, newdata = test)
test2rmse <- rmse(test$completionrate, predTest2)
train2rmse
test2rmse
trainrmse
testrmse
sqrt(mean((testY - predTest2)^2))

library(car)
vif(stepwise)


#Adjusted R-squared
1-((1-0.3907)*(85710-1)/(85710 - 21 -1))


####################
#Logistic Regression
###################

#train test split
library(caret) 
set.seed(3456) 
trainIndex <- createDataPartition(college2$ownership, p = 0.7, list = FALSE, times = 1) 
train <- college2[ trainIndex,c(6:17,19)] 
test <- college2[-trainIndex,c(6:17,19)] 
#using pure numeric variable
model <- glm(as.factor(ownership) ~ .,family=binomial(link='logit'), data=train)
summary(model)

#using numeric and categorical variable
train <- college2[trainIndex,]
test <- college2[-trainIndex,]
model1 <- glm(as.factor(ownership) ~ as.factor(predominantdegree)+as.factor(highestdegreegranted)+as.factor(region)+mediandebt+agebegin+femalepct+
                year+undergraduateenrollment+whiteundergraduatepct+blackundergraduatepct+hispanicundergraduatepct+asianpacificislander+
                nativeamalundergraduatepct+parttimepct+completionrate,family=binomial(link='logit'), data=train)
summary(model1)
result <- summary.glm(model1)$coefficients
write.csv(result, "result.csv")

#display regression coefficent (log-odds)
coef(model1)
#display regression coefficents (odds)
odds <- exp(coef(model1))
write.csv(odds, "odds.csv")


#train set prediction
library(caret)
library(stats)
probabilities.train <- predict(model1, newdata = train, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.train>=0.5,"Public","Private"))
#model accuracy
confusionMatrix(predicted.classes.min, as.factor(train$ownership), positive = "Private")
#test set prediction
probabilities.test <- predict(model1, newdata = test, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.test>=0.5,"Public","Private"))
#model accuracy
confusionMatrix(predicted.classes.min, as.factor(test$ownership), positive = "Private")
#plot ROC curve
library(pROC)
test_roc <- roc(test$ownership ~ probabilities.test, plot = TRUE, print.auc = TRUE)
#Calculate and interpret the AUC. 
test_roc$auc

install.packages("jtools")
library(jtools)
a <- summ(model1)
a$coeftable
write.csv(a$coeftable,"result1.csv")

a <- confusionMatrix(predicted.classes.min, as.factor(test$ownership), positive = "Private")
a$table
plot(a$table)

library(ggplot2)
library(dplyr)

table <- data.frame(a$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference)))

fit <- lm(data = college2, completionrate ~ whiteundergraduatepct+blackundergraduatepct+hispanicundergraduatepct+asianpacificislander+nativeamalundergraduatepct)
summary(fit)

fit <- lm(data = college2, completionrate ~ whiteundergraduatepct)
summary(fit)





##########################
#K-mean Classification
##########################
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

mydata1 <- test %>%
  select(mediandebt , agebegin , femalepct , undergraduateenrollment , whiteundergraduatepct , blackundergraduatepct, hispanicundergraduatepct, asianpacificislander , nativeamalundergraduatepct , parttimepct, completionrate)

str(mydata1)

mydata <- scale(mydata1) # standardize variables
fit <- kmeans(mydata, centers =6, nstart = 25) # 6 cluster solution

fviz_cluster(fit, data = mydata)


# exploratory

fviz_nbclust(mydata, kmeans, method = "silhouette")

aggregate(mydata,by=list(fit$cluster),FUN=mean) # get cluster means 
mydata1 <- data.frame(mydata1, as.factor(fit$cluster)) # append cluster assignment
fit
View(mydata1)


#LASSO
set.seed(123)
trainindex2 <- sample(x = nrow(mydata1), size = nrow(mydata1)*0.7)

train2 <- mydata1[trainindex2, ]
test2 <- mydata1[-trainindex2, ]

trainX2 <- Matrix::sparse.model.matrix(completionrate ~., train2) [, -1]
testX2 <- Matrix::sparse.model.matrix(completionrate ~., test2)[, -1]

dim(trainX2)
dim(testX2)

trainY2 <- train2$completionrate
testY2 <- test2$completionrate

#Cross Validation : Find the best values of lambda
set.seed(124)
lasso2 <- cv.glmnet(trainX2, trainY2, nfolds = 10)
plot(lasso2, las =1)

#lambda min
log(lasso2$lambda.min)

#lambda 1 sd. error of lambda min
log(lasso2$lambda.1se)


lassoreg2 <- glmnet(x = trainX2, y = trainY2, alpha = 1)

plot(lassoreg2, xvar = "lambda", las =1)
abline(v = log(lasso2$lambda.1se), col = "red", lty = "dashed")
abline(v = log(lasso2$lambda.min), col = "red", lty = "dashed")

#Coefficient
model1se2 <- glmnet(trainX2, trainY2, alpha = 1, lambda = lasso2$lambda.1se)
coef(model1se2)

#Prediction and RMSE
predTrain22 <- predict(model1se2, newx = trainX2)
trainrmse22 <- rmse(trainY2, predTrain22)

predTest22 <- predict(model1se2, newx = testX2)
testrmse22 <- rmse(testY2, predTest22)

trainrmse22
testrmse22

#R-squared
y3 <- test2$completionrate
y23 <- train2$completionrate
sst3 <- sum((y3 - mean(y3))^2)
sse3 <- sum((predTest22 - y3)^2)
rsq3 <- 1 - sse3/sst3
rsq3

sst23 <- sum((y23 - mean(y23))^2)
sse23 <- sum((predTrain22 - y23)^2)
rsq23 <- 1 - sse23/sst23
rsq23

#Adjusted R-squared

1-((1-rsq23)*(11020-1)/(11020 - 10 -1))
