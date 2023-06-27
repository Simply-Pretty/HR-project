library(dplyr)
library(car)
library(ggplot2)
library(randomForest)
library(cvTools)

setwd("C://Users/Sukriti/Desktop")

#reading hr_train

hr_train=read.csv("hr_train.csv",stringsAsFactors=FALSE)

#reading hr_test

hr_test=read.csv("hr_test.csv",stringsAsFactors=FALSE)

#To see which col are not common in the 2dataset

setdiff(names(hr_train),names(hr_test))

#creating a col left for test data and assigning "NA" to it
hr_test$left="NA"

#creating col "data" where all the rows of test and train are named "test" and "train" respectively

hr_train$data="train"
hr_test$data="test"

# The 2 datasets are binded so that the cleaning can be done in one shot

hr_all=rbind(hr_train,hr_test)

glimpse(hr_all)

# From the glimpse left,sales and salary are character variables

## Data preparation

#Salary
table(hr_all$salary)

hr_all=hr_all%>%
  mutate(medium_sal=as.numeric(salary=="medium"),
         low_sal=as.numeric(salary=="low")
  )%>%
  select(-salary)


#sales
table(hr_all$sales)
hr_all=hr_all%>%
  mutate(sales_acc=as.numeric(sales=="accounting"),
         sales_hr=as.numeric(sales=="hr"),
         sales_IT=as.numeric(sales=="IT"),
         sales_marketing=as.numeric(sales=="marketing"),
         sales_p.mng=as.numeric(sales=="product_mng"),
         sales_Rand=as.numeric(sales=="RandD"),
         sales_sales=as.numeric(sales=="sales"),
         sales_support=as.numeric(sales=="support"),
         sales_technical=as.numeric(sales=="technical")
  )%>%
  select(-sales)


#look for missing values

sum(apply(hr_all, 2,function(x)sum(is.na(x))))


## Splitting the binded data into train and test as it was


hr_train = hr_all %>% filter(data=='train') %>% select(-data)

#Converting the TV "left to factors since this is a classification problem

hr_train=hr_train%>%
  mutate(left=as.factor(left))

hr_test = hr_all %>% filter(data=='test') %>% select(-data,-left)



#splitting hr_train to train and test

set.seed(2)

s=sample(1:nrow(hr_train),0.8*nrow(hr_train))

hr_model=hr_train[s,] ## model building train - 80%

hr_validation=hr_train[-s,] ## model validation test - 20%


#Hyperparameter Tuning

param=list(mtry=c(1,5,9,10),
           
           ntree=c(50,200,300,400),
           
           maxnodes=c(10,20,40,50),
           
           nodesize=c(1,5,10,20))





#expand.grid(param)

subset_paras=function(full_list_para,n=2){
  
  all_comb=expand.grid(full_list_para)
  
  set.seed(2)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
  
}





my_params=subset_paras(param,nrow(expand.grid(param)))



my_auc=function(y,yhat){
  
  roccurve=pROC::roc(y,yhat)
  
  score=pROC::auc(roccurve)
  
  return(score)
  
}

#print.data.frame(my_params)



my_params[, "AUC"] = numeric(nrow(my_params))


myauc=0

#nrow(my_params)

for(i in 1:nrow(my_params)){
  
  print(paste('starting iteration :',i))
  
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~.,
             
             data =hr_model,
             
             tuning =params,
             
             folds = cvFolds(nrow(hr_model), K=5, type ="random"),
             
             cost =my_auc, seed =2,
             
             predictArgs = list(type="prob")
             
  )
  
  score.this=k$cv[,2]
  
  my_params[i, "AUC"] = score.this
  
  if(score.this>myauc){
    
    print(params)
    
    myauc=score.this
    
    print(myauc)
    
    best_params=my_params[i,]
    
    print.data.frame(best_params)
    
  }
  
  print('DONE')
  
  
  
}


#Random forest model build based on the train data of hr_train

library(randomForest)

#hr_model$left=as.factor(hr_model$left)
rf.hr_model=randomForest(left~.,data=hr_model,mtry=best_params$mtry,ntree=best_params$ntree,maxnodes=best_params$maxnodes,nodesize=best_params$nodesize)


#Prediction of the TV based on the model of the train data of hr_train and assigning to the hr_model
hr_model$rf.pred=predict(rf.hr_model,newdata=hr_model,type="prob")[,2]


#AUC for train data under hr_train 

library(pROC)
hr_model_auc=auc(roc(hr_model$left~hr_model$rf.pred))
hr_model_auc
plot(roc(hr_model$left~hr_model$rf.pred))

#Prediction of test data based on the train data under hr_train

hr_validation$pred=predict(rf.hr_model,newdata=hr_validation,type="prob")[,2]


#AUC for test data of train

hr_validation_auc=auc(roc(hr_validation$left~hr_validation$pred))
hr_validation_auc
plot(roc(hr_validation$left~hr_validation$pred))


#prediction based on the model of the train data of hr_train

m.rf_pred=predict(rf.hr_model,newdata=hr_train,type="prob")[,2]
auc(roc(hr_train$left~m.rf_pred))


# Model and prediction for whole train data

model_train.rf=randomForest(left~.,data=hr_train,mtry=best_params$mtry,ntree=best_params$ntree,maxnodes=best_params$maxnodes,nodesize=best_params$nodesize)
hr_train$rf.pred=predict(model_train.rf,newdata=hr_train,type="prob")[,2]

#AUC for train data
train_auc=auc(roc(hr_train$left~hr_train$rf.pred))
train_auc
plot(roc(hr_train$left~hr_train$rf.pred))

#Finding the score of the test data based on the model of the train data

#score2=predict(rf.hr_model,newdata=hr_test,type="prob")[,1]
score1=predict(model_train.rf,newdata=hr_test,type="prob")[,2]



importance(rf.hr_model)
varImpPlot(rf.hr_model)

#The employees are leaving majorly because satisfaction level

table(hr_model$satisfaction_level)

boxplot(hr_model$satisfaction_level~hr_model$left)
boxplot(hr_model$average_montly_hours~hr_model$left)

#Interpretation:
#So, from the box-plot the satisfaction level is low for the employees who are leaving.
# Employees are also leaving because of more working hours.
#So, employees are leaving because they are not satisfied.

boxplot(hr_train$satisfaction_level~hr_train$left)

left=as.data.frame(score1)
names(left)=c("left")
write.csv(left,"RINI_BISWAS_P4_part2.csv",row.names=F)







