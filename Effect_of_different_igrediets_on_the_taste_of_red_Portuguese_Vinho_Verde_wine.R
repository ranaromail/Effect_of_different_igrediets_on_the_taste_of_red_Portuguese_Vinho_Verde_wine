install.packages("tidyverse")
install.packages("leaps")
install.packages("ISLR") 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyquant)
library(MASS)
library(leaps)
library(ISLR) 
library(boot)
library(stats)
library(GGally)




attach(winequality_red)

ggplot(data=winequality_red)+
  geom_bar(mapping = aes(x= factor(quality)))+
  xlab("Quality (3/Poor to 8/High)")+
  ylab("Number of samples")+
  ggtitle("Vinho Verde Red Wine")



ggplot(data=winequality_red,mapping = aes(x=density,y= alcohol))+
  geom_point()+
  geom_line()+
  xlab("Density")+
  ylab("Fixed Acidity")+
  ggtitle("Alcohol Vs Density")



ggplot(data=winequality_red,mapping = aes(x=density,y= fixed_acidity))+
  geom_point()+
  geom_line()+
  xlab("Density")+
  ylab("Fixed Acidity")+
  ggtitle("Fixed Acidity Vs Density")







ggplot(data=winequality_red)+
  geom_boxplot(mapping = aes(x=factor(quality), y=alcohol))+
  xlab("Quality")+
  ylab("Alcohol")+
  ggtitle("Alcohol Vs Quality")





quality8=filter(winequality_red, quality >= 7)
quality8=data.frame(quality8)
View(quality8)

ggplot(data=quality8)+
  geom_histogram(mapping= aes(x=sulphates, fill=factor(quality)),position = 'dodge')+
  xlab("Sulphates")+
  ylab("Number of Samples")+
  labs(fill="Grade of Wine")+
  ggtitle("Sulphates effect")


ggplot(data=quality8)+
  geom_bar(mapping = aes(x=volatile_acidity,fill=factor(quality)),position=position_dodge2(preserve = "single"),width = 0.75)+
  xlab("")+
  ylab("Number of Samples")+
  labs(fill="Grade of Wine")+
  ggtitle("Alcohol effect")


ggplot(data=winequality_red,mapping = aes(x=density,y= alcohol))+
  geom_point()+
  geom_line()+
  xlab("Density")+
  ylab("Alcohol")+
  ggtitle("Alcohol Vs Density")


ggplot(data=winequality_red,mapping = aes(x=density,y= fixed_acidity))+
  geom_point()+
  geom_line()+
  xlab("Density")+
  ylab("Fixed Acidity")+
  ggtitle("Fixed Acidity Vs Density")


####LET'S LOOK COERRELEATION

cor(winequality_red)




attach(winequality_red)
View(winequality_red)


m1=lm(data=winequality_red,quality~alcohol+sulphates)
summary(m1)


mo1=lm(data = winequality_red, quality~alcohol+volatile_acidity+pH)
summary(mo1)

mo2=lm(data = winequality_red, quality~alcohol+volatile_acidity+pH+sulphates)
summary(mo2)

mo21=lm(data = winequality_red, quality~log(alcohol)+volatile_acidity+log(pH)+sulphates)
summary(mo21)


#####best result
mo22=lm(data = winequality_red, quality~log(alcohol)+volatile_acidity+log(pH)+log(sulphates))
summary(mo22)

mo23=lm(data = winequality_red, quality~log(alcohol)+poly(volatile_acidity,2,raw='TRUE')+log(pH)+log(sulphates))
summary(mo23)


mo3=lm(data = winequality_red, quality~poly(alcohol,2,raw = 'TRUE')+volatile_acidity+pH+sulphates)
summary(mo3)


mo4=lm(data = winequality_red, quality~poly(alcohol,2,raw = 'TRUE')+poly(volatile_acidity,2)+pH+sulphates)
summary(mo4)

mo5=lm(data = winequality_red, quality~poly(alcohol,3,raw = 'TRUE')+poly(volatile_acidity,2)+pH+sulphates)
summary(mo5)



#adding other variables too

model1= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+pH+sulphates+alcohol)
model1
summary(model1)

model2= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+pH+sulphates)
summary(model2)


model3= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+pH)
summary(model3)


model4= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density)
summary(model4)


model5= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide+total_sulfur_dioxide)
summary(model5)


model6= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides+free_sulfur_dioxide)
summary(model6)


model7= lm(data=winequality_red, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+chlorides)
summary(model7)



model8= lm(data=winequality_red, quality~volatile_acidity+alcohol+sulphates+total_sulfur_dioxide)
summary(model8)




# REgression Method 'Best'

fit1= regsubsets(quality~.,data=winequality_red,nvmax = 11)
summary(fit1)
summary(fit1)$rsq
summary(fit1)$adjr2
summary(forward)$cp
summary(fit1)$bic
summary(fit1)$rss
coef(fit1,6)
coef(forward,6)
coef(backward,6)

x = seq(1, 11, by=1)
y = summary(fit1)$rsq
rsq1 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=rsq1)+geom_point()+geom_line()+xlab("Model Number")+ylab("R2")+
  ggtitle("Model selection with BEST method")
# 8th model has the highest adjr2


x = seq(1, 11, by=1)
y = summary(fit1)$cp
cp1 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=cp1)+geom_point()+geom_line()+xlab("Model Number")+ylab("CP")+ ggtitle("Model selection with BEST method")
# 7th model has lowest cp


x = seq(1, 11, by=1)
y = summary(fit1)$bic
adjr2bic1 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2bic1)+geom_point()+geom_line()+xlab("Model Number")+ylab("BIC")+ggtitle("Model selection with BEST method")
# 6th model has the lowest bic

x = seq(1, 11, by=1)
y = summary(fit1)$rss
adjr2rss1 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2rss1)+geom_point()+geom_line()+xlab("Model Number")+ylab("RSS")
# till 6 it has  a good change in rss



coef(fit1,6)
coef(fit1,7)
coef(fit1,8)





###poly 2
fit2 = regsubsets(quality~ poly(fixed_acidity, 2, raw = 'TRUE')+poly(volatile_acidity, 2, raw = 'TRUE')+
                    poly(citric_acid,2, raw = 'TRUE')+poly(chlorides, 2, raw = 'TRUE')+
                    poly(free_sulfur_dioxide,2, raw = 'TRUE')+poly(total_sulfur_dioxide,2, raw = 'TRUE')+poly(pH,2, raw = 'TRUE')+poly(sulphates,2, raw = 'TRUE')+poly(alcohol,2, raw = 'TRUE'),data=winequality_red, nvmax = 18)

summary(fit2)
summary(fit2)$adjr2
summary(fit3)$adjr2
summary(fit2)$adjr2
summary(fit2)$cp

x = seq(1, 18, by=1)
y = summary(fit2)$adjr2
adjr2Res2 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res2)+geom_point()+geom_line()+xlab("Model Number")+ylab("Adjusted R2 for 'poly 2'")

coef(fit2,6)

x = seq(1, 18, by=1)
y = summary(fit2)$cp
adjr2Res2 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res2)+geom_point()+geom_line()+xlab("Model Number")+ylab("CP for 'poly 2'")

x = seq(1, 18, by=1)
y = summary(fit2)$bic
adjr2Res2 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res2)+geom_point()+geom_line()+xlab("Model Number")+ylab("BIC for 'poly 2'")

coef(fit2,1)
coef(fit2,2)
coef(fit2,3)
coef(fit2,4)
coef(fit2,5)
coef(fit2,6)
coef(fit2,7)
coef(fit2,8)
coef(fit2,9)





####poly 3
fit3 = regsubsets(quality~ poly(fixed_acidity, 3, raw = 'TRUE')+poly(volatile_acidity, 3, raw = 'TRUE')+
                     poly(citric_acid, 3, raw = 'TRUE')+poly(chlorides, 3, raw = 'TRUE')+
                     poly(free_sulfur_dioxide, 3, raw = 'TRUE')+poly(total_sulfur_dioxide,3, raw = 'TRUE')+poly(pH,3, raw = 'TRUE')+poly(sulphates,3, raw = 'TRUE')+poly(alcohol,3, raw = 'TRUE'),data=winequality_red, nvmax = 27)

summary(fit3)rsq
summary(fit3)$adjr2
summary(fit3)$cp
summary(fit3)$bic

x = seq(1, 27, by=1)
y = summary(fit3)$adjr2
adjr2Res3 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res3)+geom_point()+geom_line()+xlab("Model Number")+ylab("Adjusted R2 for 'poly 3'")

x = seq(1, 27, by=1)
y = summary(fit3)$cp
adjr2Res3 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res2)+geom_point()+geom_line()+xlab("Model Number")+ylab("CP for 'poly 3'")

x = seq(1, 27, by=1)
y = summary(fit3)$bic
adjr2Res3 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res3)+geom_point()+geom_line()+xlab("Model Number")+ylab("BIC R2 for 'poly 3'")


coef(fit3,1)
coef(fit3,2)
coef(fit3,3)

coef(fit3,4)







####poly4
fit4 = regsubsets(quality~ poly(fixed_acidity, 4, raw = 'TRUE')+poly(volatile_acidity, 4, raw = 'TRUE')+
                    poly(citric_acid, 4, raw = 'TRUE')+poly(chlorides, 4, raw = 'TRUE')+
                    poly(free_sulfur_dioxide, 4, raw = 'TRUE')+poly(total_sulfur_dioxide,4, raw = 'TRUE')+poly(pH,4, raw = 'TRUE')+poly(sulphates,4, raw = 'TRUE')+poly(alcohol,4, raw = 'TRUE'),winequality_red,nvmax = 36)

summary(fit4)$adjr
x = seq(1, 36, by=1)
y = summary(fit4)$adjr2
adjr2Res4 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res4)+geom_point()+geom_line()+xlab("Model Number")+ylab("Adjusted R2 for 'poly 4'")


x = seq(1, 36, by=1)
y = summary(fit4)$cp
adjr2Res4 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res4)+geom_point()+geom_line()+xlab("Model Number")+ylab("Adjusted R2 for 'poly 4'")

x = seq(1,36, by=1)
y = summary(fit4)$bic
adjr2Res4 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res4)+geom_point()+geom_line()+xlab("Model Number")+ylab("BIC for 'poly 4'")

coef(fit4,1)
coef(fit4,2)
coef(fit4,3)
coef(fit4,4)

fit5 = regsubsets(quality~ volatile_acidity+chlorides+poly(sulphates,5, raw = 'TRUE')+
                    log(alcohol),winequality_red,nvmax = 16)

summary(fit5)$adjr
x = seq(1, 16, by=1)
y = summary(fit5)$adjr2
adv5 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adv5)+geom_point()+geom_line()+xlab("Model Number")+ylab("Adjusted R2 for 'poly 5'")
coef(fit5,4)

##############################forward approach for regression 



forward = regsubsets(quality~., method="forward", data=winequality_red,nvmax = 11)
summary(forward)
summary(forward)$adjr2
summary(forward)$rsq
summary(forward)$cp
summary(forward)$bic
summary(forward)$rss



x = seq(1, 11, by=1)
y = summary(forward)$rsq
forwardrsq = data.frame(x, y)
ggplot(aes(x=x, y=y), data=forwardrsq)+geom_point()+geom_line()+
  xlab("Model Number")+
  ylab("R2")+
  ggtitle("Model selection with 'Forward' method")


x = seq(1, 11, by=1)
y = summary(forward)$cp
forwardcp = data.frame(x, y)
ggplot(aes(x=x, y=y), data=forwardcp)+geom_point()+geom_line()+
  xlab("Model Number")+
  ylab("CP")+
  ggtitle("Model selection with 'Forward' method")



x = seq(1, 11, by=1)
y = summary(forward)$bic
forwardbic = data.frame(x, y)
ggplot(aes(x=x, y=y), data=forwardbic)+geom_point()+geom_line()+
  xlab("Model Number")+
  ylab("BIC")+
  ggtitle("Model selection with 'Forward' method")


x = seq(1, 11, by=1)
y = summary(forward)$rss
forwardrss = data.frame(x, y)
ggplot(aes(x=x, y=y), data=forwardrss)+geom_point()+geom_line()+xlab("Model Number")+ylab("RSS for 'Forward'")


coef(forward,1)
coef(forward,2)
coef(forward,3)
coef(forward,4)



#################backward approach for regression 



backward = regsubsets(quality~., method="backward", data=winequality_red,nvmax = 11)
summary(backward)


x = seq(1, 11, by=1)
y = summary(backward)$rsq
backrsq1 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=backrsq1)+geom_point()+geom_line()+
  xlab("Model Number")+
  ylab("R2")+
  ggtitle("Model selection with 'Backward' method")

x = seq(1, 11, by=1)
y = summary(backward)$cp
backcp1 = data.frame(x, y)
ggplot(aes(x=x, y=y), data=backcp1)+geom_point()+geom_line()+
  xlab("Model Number")+
  ylab("Cp")+
  ggtitle("Model selection with 'Backward' method")


x = seq(1, 11, by=1)
y = summary(backward)$bic
backbic1= data.frame(x, y)
ggplot(aes(x=x, y=y), data=backbic1)+geom_point()+geom_line()+
  xlab("Model Number")+
  ylab("BIC")+
  ggtitle("Model selection with 'Backward' method")


x = seq(1, 11, by=1)
y = summary(backward)$rss
backwardrss = data.frame(x, y)
ggplot(aes(x=x, y=y), data=backwardrss)+geom_point()+geom_line()+xlab("Model Number")+ylab("RSS for 'Backward'")


summary(backward)$adjr2
summary(backward)$rsq
summary(backward)$cp
summary(backward)$bic
summary(backward)$rss



#

backwardp2 = regsubsets(quality~.+poly(density,5,raw= 'True')-density, method="backward", data=winequality_red,nvmax = 11)
summary(backward)

x = seq(1, 11, by=1)
y = summary(backward)$adjr2
adjr2Res = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("Model Number")+ylab("Adjr2 for 'Backward poly'")


x = seq(1, 11, by=1)
y = summary(backward)$cp
adjr2Res = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("Model Number")+ylab("Cp for 'Backward poly'")


x = seq(1, 11, by=1)
y = summary(backward)$bic
adjr2Res = data.frame(x, y)
ggplot(aes(x=x, y=y), data=adjr2Res)+geom_point()+geom_line()+xlab("Model Number")+ylab("BIC for 'Backward poly'")

summary(backwardp2)$adjr2




##############    cross validation    ############################################################


mod = rep(0, 10)
mod[1] = "quality~ volatile_acidity + chlorides+ sulphates + alcohol"
mod[2] = "quality~ volatile_acidity + chlorides+ sulphates + alcohol+pH"
mod[3]=    "quality~ volatile_acidity + chlorides+ poly(sulphates,5, raw = 'TRUE')+log(alcohol)"
mod[4] = "quality~ volatile_acidity + chlorides+ poly(sulphates,2) + log(alcohol)+poly(pH,2)"
mod[5] = "quality~ volatile_acidity + chlorides+ poly(sulphates,3) + log(alcohol)+poly(pH,3)"
mod[6] = "quality~ volatile_acidity + chlorides+ poly(sulphates,4) + log(alcohol)+poly(pH,4)"
mod[7] = "quality~ volatile_acidity + chlorides+ poly(sulphates,4) + log(alcohol)+poly(pH,4)+ total_sulfur_dioxide"
mod[8] = "quality~ volatile_acidity + chlorides+ poly(sulphates,4) + log(alcohol)+poly(pH,4)+ poly(total_sulfur_dioxide,2)"
mod[9] = "quality~ volatile_acidity + chlorides+ poly(sulphates,4) + log(alcohol)+poly(pH,4)+ poly(total_sulfur_dioxide,3)"
mod[10] = "quality~ volatile_acidity + chlorides+ poly(sulphates,4) + log(alcohol)+poly(pH,4)+ poly(total_sulfur_dioxide,4)"




cv.error=rep(0,10)
for(i in 1:10){
  model=glm(eval(parse(text=paste(mod[j]))))
  cv.error[i] = cv.glm(winequality_red, model, K=nrow(winequality_red))$delta[1]
}
View(cv.error)
cv.error

numRows = nrow(winequality_red)
id = seq(1, numRows, by =1)

wineShuffle = slice(winequality_red, sample(1:n()))
wineShuffle = mutate(wineShuffle, id)

k=5

errors = matrix( nrow = 10, ncol = 5)
errors[1,2] = 0
View(errors)

for(j in 1:10){
  for(i in 1:5){
    errors[j,i] = 0
  }
}
totalError = 0
for(j in 1:10){ # the 10 different models
  for(i in 1:k){ #the k folds for each model
    test = filter(wineShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
    train = anti_join(wineShuffle, test, by="id")
    model = lm(eval(parse(text=paste(mod[j])), train))
    errors[j,i] = mean((test$quality - predict.lm(model, test))^2)
  }
}


avgRegEr = rep(0,10)
avgRegEr
for(j in 1:10){
  for(i in 1:5){
    avgRegEr[j] = avgRegEr[j]+errors[j, i]
  }
}
avgRegEr/k
cv.error
#now we confirmed that our calculated CV errors are very similar to those offered by glm
#(difference likely due to random points chosen) we can move on
#we did this so we could find Standard Error (SE) and create error bars
se = rep(0,10)
for (i in 1:10){
  se[i] = sqrt(var(errors[i,])/k)
}
se
#now making data frame for ease of plotting
x = seq(1,10, by = 1)
faithBest = data.frame(x,avgRegEr/k , se)
faithBest
ggplot(data = faithBest, aes(x = x, y=avgRegEr.k))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = avgRegEr.k-se, ymax = avgRegEr.k +se))+
  xlab("Model Number")+
  ylab("Error")+
  ggtitle("Cross Validation Standard Error Plot")



########################################Classification############################################

attach(winequality_red)

ggplot(data=winequality_red)+
  geom_point(mapping = aes(x=alcohol,y=,color=factor(quality)))

ggpairs(winequality_red, aes(colour=factor(quality)))

table(winequality_red$quality)


winelevel0= lda(quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+
                  chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+pH+
                  sulphates+alcohol,data=winequality_red)
winePred0 = predict(winelevel0, winequality_red)
table(winePred0$class, winequality_red$quality)
#965


winelevel = lda(quality~fixed_acidity+volatile_acidity, data=winequality_red)
winePred = predict(winelevel, winequality_red)
table(winePred$class, winequality_red$quality)


winelevel = lda(quality~alcohol+sulphates+volatile_acidity,data=winequality_red)
winePred = predict(winelevel, winequality_red)
table(winePred$class, winequality_red$quality)
#918/1599

winelevela = lda(quality~alcohol+sulphates+volatile_acidity+fixed_acidity,data=winequality_red)
winePreda = predict(winelevela, winequality_red)
table(winePreda$class, winequality_red$quality)
#917/1599

winelevelb = lda(quality~alcohol+poly(sulphates,2,raw = 'TRUE')+volatile_acidity, data=winequality_red)
winePredb = predict(winelevelb, winequality_red)
table(winePredb$class, winequality_red$quality)
#926/1599


winelevelc = lda(quality~alcohol+poly(sulphates,2,raw = 'TRUE')+volatile_acidity+fixed_acidity, data=winequality_red)
winePredc = predict(winelevelc, winequality_red)
table(winePredc$class, winequality_red$quality)
#928/1599

wineleveld = lda(quality~alcohol+poly(sulphates,5,raw = 'TRUE')+volatile_acidity+fixed_acidity+pH,data=winequality_red)
winePredd = predict(wineleveld, winequality_red)
table(winePredd$class, winequality_red$quality)
#937/1599


winelevele = lda(quality~poly(alcohol,3,raw = 'TRUE')+poly(sulphates,5,raw = 'TRUE')+volatile_acidity+fixed_acidity)
winePrede = predict(winelevele, winequality_red)
table(winePrede$class, winequality_red$quality)
#926/1599


winelevelf = lda(quality~poly(alcohol,3,raw = 'TRUE')+sulphates+volatile_acidity+fixed_acidity)
winePredf = predict(winelevelf, winequality_red)
table(winePredf$class, winequality_red$quality)
#896

# tarining and testing for classification models
id = seq(1, 150, by=1)
winemix = slice(winequality_red, sample(1:n()))
winerando = mutate(winemix, id)


IDwine = mutate(winequality_red, id=row_number())
View(IDwine)

trainDataSet = sample_frac(IDwine, .5)
View(trainDataSet)

testDatawine = anti_join(IDwine, trainDataSet, by ="id")
View(testDatawine)



wineModel = lda(quality~alcohol+poly(sulphates,5, raw='TRUE')+volatile_acidity+fixed_acidity+pH,data=trainDataSet)


wineTrainSetPredictions= predict(wineModel, trainDataSet)
table(wineTrainSetPredictions$class, trainDataSet$quality)





################################################################################################################
#                      Cross Validation for classificaton              

attach(winequality_red)
id = seq(1, 1599, by=1)
wineMix = slice(winequality_red, sample(1:n()))
wineRando = mutate(wineMix, id)

k=5
numRows=nrow(winequality_red)
errors8=rep(0,k)
totalError=0
for (i in 1:k) {
  test=filter(wineRando,id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  train = anti_join(wineRando, test, by="id")
  medo=lda(data=train, quality~fixed_acidity+volatile_acidity+citric_acid+residual_sugar+
             chlorides+free_sulfur_dioxide+total_sulfur_dioxide+density+pH+
             sulphates+alcohol)
  medoguess=predict(medo,test)
  errors8[i]=1-mean(medoguess$class == test$quality)
  totalError= errors8[i]+totalError

}

errors1
errors2
errors3
errors4
errors5
errors6
errors7
errors8

avgE= rep(0,8)
for (i in 1:k) {
  avgE[1]=errors1[i]+avgE[1]
  avgE[2]=errors2[i]+avgE[2]
  avgE[3]=errors3[i]+avgE[3]
  avgE[4]=errors4[i]+avgE[4]
  avgE[5]=errors5[i]+avgE[5]
  avgE[6]=errors6[i]+avgE[6]
  avgE[7]=errors7[i]+avgE[7]
  avgE[8]=errors8[i]+avgE[8]
}

se=rep(0,8)

for (i in 1:k) {
  avgE[i]=avgE[i]/k
  
}

avgE

se[1]=sqrt(var(errors1)/k)
se[2]=sqrt(var(errors2)/k)
se[3]=sqrt(var(errors3)/k)
se[4]=sqrt(var(errors4)/k)
se[5]=sqrt(var(errors5)/k)
se[6]=sqrt(var(errors6)/k)
se[7]=sqrt(var(errors7)/k)
se[8]=sqrt(var(errors8)/k)

cvwine=data.frame(avgE,se)
View(cvwine)

mn=seq(1,8,by=1)
cvwine=data.frame(avgE,se,mn)


ggplot(cvwine,aes(x=mn,y=avgE))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=avgE-se,ymax=avgE+se))+
  
  ylab(" Error")+
  xlab("Model Number")+
  ggtitle("Cross Validation for Classification models")

