---
  title: "exam 1"
author: "safinaz alI"
date: "10/13/2022"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  ```{r}
load("~/Desktop/Household_Pulse_data_w48 (1).RData")

#question 1 
#BASED on the data it seems that people with some college are the most that have had no telework in th past work. However the higher degree the more likely they are to work remotely. So im going to compare the people taht do not work remotely at all in some college to an adv deg person that works 1-2 days.

restrict5 <-(Household_Pulse_data$TWDAYS == "had 1-2 telework days in past week") & (Household_Pulse_data$EEDUC == "adv deg") & (Household_Pulse_data$RECVDVACC == "yes got vaxx")
data_new5 <- subset(Household_Pulse_data,restrict5) 
t.test(restrict5)
sd(restrict5)
summary(data_new5$TWDAYS) #SUMMARY:3%
prop.table(summary(data_new5$TWDAYS), margin = NULL)

restrict7 <-(Household_Pulse_data$TWDAYS == "had no telework days in past week") & (Household_Pulse_data$EEDUC == "some coll") & (Household_Pulse_data$RECVDVACC =="no did not get vaxx")
data_new7 <- subset(Household_Pulse_data,restrict7) 
t.test(restrict7)
sd(restrict7)
summary(data_new7$TWDAYS) #SUMMARY:3%
prop.table(summary(data_new6$TWDAYS), margin = NULL)

#if you run my restrict you can see that people that had no telework and had some college had less then 0.05 making it significant since it it has 95% confidence interval.When i added vaccine status i realize being vaccinated or not did not play a factor into you working remote or not in which my original hypothesis was taht you were more likely to not be vaccinated if yo uwork mostly online. 


#question 2 

restrict3 <-(Household_Pulse_data$HADCOVIDRV == "yes tested + or doc told had Covid") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$LONGCOVID == " had symptoms 3mo or more Long Covid") 
data_new3 <- subset(Household_Pulse_data,restrict3)
#I found the standard error to get the outcome difference  using a t-test to get the confidence interval to prove the difference is actually there. I used the standard deviation and sample size from the restriction to run it
t.test(restrict3)
sd(restrict3)
#with this summary i got the difference from the subset 
summary(data_new3$HADCOVIDRV) #SUMMARY:%37
#This prop table was able to find the marginal probabilities after i was able to add the cross table for the data to see the probabilities of vaccine status irrespective of the outcome their employment
prop.table(summary(data_new6$HADCOVIDRV), margin = NULL)

# I repeated the subset to see different correlation between the data 

restrict1 <-(Household_Pulse_data$HADCOVIDRV == "yes tested + or doc told had Covid ") & (Household_Pulse_data$RECVDVACC == "no did not get vaxx") | (Household_Pulse_data$LONGCOVID == "no")  
data_new1 <- subset(Household_Pulse_data,restrict1) 
t.test(restrict1)
sd(restrict1)
summary(data_new1$HADCOVIDRV) #SUMMARY:30%
prop.table(summary(data_new6$HADCOVIDRV), margin = NULL)

#based on the restrict you can see that less people that got tet for covid that did not get the vaccine vs people that did get the vaccine.so my orginal hypothesis of people getting vaccine and being less likely to get covid is wrong since there is a 6% higher chance for them to get vs the people taht did not get the vaccine.  based on 



#question 3
load("/Users/safinazali/Desktop/Household_Pulse_data_w48 (1).RData")
attach(Household_Pulse_data)

restrict2 <-(Household_Pulse_data$TENURE == "housing owned free and clear") & (Household_Pulse_data$ANYWORK == "no employment in last 7 days") & (Household_Pulse_data$ANXIOUS == "several days anxiety over past 2 wks") | (Household_Pulse_data$ANXIOUS == "more than half the days anxiety over past 2 wks") 
data_new2 <- subset(Household_Pulse_data,restrict2) 
t.test(restrict2)
sd(restrict2)
summary(data_new2$BOOSTERRV) #SUMMARY:14%
prop.table(summary(data_new2$BOOSTERRV), margin = NULL)

restrict6 <-(Household_Pulse_data$TENURE== " housing rented ") & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days") & (Household_Pulse_data$ANXIOUS == "several days anxiety over past 2 wks") | (Household_Pulse_data$ANXIOUS == "more than half the days anxiety over past 2 wks") 
data_new6 <- subset(Household_Pulse_data,restrict6) 
t.test(restrict6)
sd(restrict6)
#summary(data_new2$BOOSTERRV) #SUMMARY:9%
prop.table(summary(data_new2$BOOSTERRV), margin = NULL)

#I looked into housing rented and housing owned and free because i believe that if you owned your house and didnt have no bills and you dont work you will have less anxiety however base don the restrict i ran you can see that 13% of people are in that situation while people that are renting their homes and working are less stressed i guess you can say with 9%. its a small difference but it is suprsing taht people with prepaid homes are more stressed then people with jobs and a housing payment 


#CREATED dummy variables so that i can run a regression to see the relationship between teh dataset 
home <- ifelse(TENURE == "housing owned free and clear" | TENURE == "housing rented",1,0)
work <- ifelse(ANYWORK == "yes employment in last 7 days",1,0)
anx<- ifelse(ANXIOUS== "more than half the days anxiety over past 2 wks" | ANXIOUS== "several days anxiety over past 2 wks",1,0)

model_temp1 <-lm(home ~ work+ anx)
summary(model_temp1)

#this is a xtabs of the data that changed it to 1,0 so that it can be easier to classify for the knn 
xtabs(~home + work + anx)

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

#here i ran multiple numerical vales so that i can conclude it into my knn classifier because the more data i run the more accurate it will be so i saw that using race as a factor. the kids variable since its the onnly numeric data given plus the marital status to get enough combination to generate for the amount of observation in the data base 
race <- as.factor(RHISPANIC)
kids <- norm_varb(Num_kids_homeschool+Num_kids_Priv_School+Num_kids_Pub_School)
marry <- (MS=='married') * 1 + (MS=='widowed ')*2+(MS=='divorced ')*3+(MS=='separated')*4+(MS=='never')*5     

norm_kids <- norm_varb(kids)
norm_marry <- norm_varb(marry)

data_use_prelim <- cbind(home,work,anx,kids,marry)
data_use_prelim <- data.frame(data_use_prelim)

good_obs_data_use <- complete.cases(data_use_prelim,race)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(race,good_obs_data_use)
gender0 <- complete.cases(data_use_prelim, race)
summary(data_use_prelim)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

#based on the knn classifier you can see that i had an 77 to 81% accuracy with the predicted values 
summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1,15, by= 2)) {
  pred_race <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_race == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}


cl_data_n <- as.numeric(cl_data)
#i ran a regression on the predicted values so that it can run it on the orginal database i had with the regression just looking at home work and anxious variable. 
model_ols1 <- lm(cl_data_n ~ train_data$home + train_data$work + train_data$anx)
summary(model_ols1)
y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])

cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$home + train_data$work + train_data$anx)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])                    


```

## Including Plots

You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
