# meta analysis for Using secondary cases to characterize the severity of an emerging or re-emerging infection #
# by Can Wang and Tim K. Tsang

### meta analysis for COVID symptoms

library(metafor)
library(Hmisc)

## data process

covid <- read.csv('C:/Users/can/Desktop/data_summary.csv')[c(1:12,15:18),c(3,4,8,9,19,20,14,12,21,11,13,17,24)]

for (i in 5:14) {
  covid[covid[,i]=="",i] <- NA
}

symptom_table <- covid[,c(3:13)]


## RE model
fever.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,1]),1]), ni = as.numeric(symptom_table[!is.na(symptom_table[,1]),11]), method = 'ML',
                                      measure="PLO",model="UM.RS")
cough.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,2]),2]), ni = as.numeric(symptom_table[!is.na(symptom_table[,2]),11]), method = 'ML',
                         measure="PLO",model="UM.RS")
sore.throat.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,3]),3]), ni = as.numeric(symptom_table[!is.na(symptom_table[,3]),11]), method = 'ML',
                               measure="PLO",model="UM.RS")
headache.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,4]),4]), ni = as.numeric(symptom_table[!is.na(symptom_table[,4]),11]), method = 'ML',
                            measure="PLO",model="UM.RS")
diarrhea.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,5]),5]), ni = as.numeric(symptom_table[!is.na(symptom_table[,5]),11]), method = 'ML',
                            measure="PLO",model="UM.RS")
fatigue.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,6]),6]), ni = as.numeric(symptom_table[!is.na(symptom_table[,6]),11]), method = 'ML',
                           measure="PLO",model="UM.RS")
myalgia.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,7]),7]), ni = as.numeric(symptom_table[!is.na(symptom_table[,7]),11]), method = 'ML',
                           measure="PLO",model="UM.RS")
chills.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,8]),8]), ni = as.numeric(symptom_table[!is.na(symptom_table[,8]),11]), method = 'ML',
                          measure="PLO",model="UM.RS") 
dyspnea.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,9]),9]), ni = as.numeric(symptom_table[!is.na(symptom_table[,9]),11]), method = 'ML',
                           measure="PLO",model="UM.RS")
rhinorrhea.re_all <- rma.glmm(xi = as.numeric(symptom_table[!is.na(symptom_table[,10]),10]), ni = as.numeric(symptom_table[!is.na(symptom_table[,10]),11]), method = 'ML',
                              measure="PLO",model="UM.RS")

reml <- list(fever.re_all,cough.re_all,sore.throat.re_all,headache.re_all,diarrhea.re_all,fatigue.re_all,myalgia.re_all,chills.re_all,dyspnea.re_all,diarrhea.re_all)

## obtain I2 and p-value for heterogeneity
I2 <- estimate <- se <- lb <- ub <- n.study <- rep(NA,10)

for (i in 1:10){
  estimate[i] = round(reml[[i]]$b,2)
  lb[i] <- round(reml[[i]]$ci.lb,2)
  ub[i] <- round(reml[[i]]$ci.ub,2)
  I2[i]= round(reml[[i]]$I2,2)
  se[i] <- round(reml[[i]]$se,2)
  n.study[i] <- length(symptom_table[!is.na(symptom_table[,i]),i])/2
}

table_all <- data.frame(estimate=estimate,lb=lb,ub=ub,se=se,I2=paste0(I2,'%'),n.study=n.study,n.items=n.study*2)
table_all$symptoms <- c('fever','cough','sore throat','headache','diarrhea','fatigue','myalgia','chills','dyspnea','rhinorrhea')


## RE model for secondary cases only

symptom_table_sec <- symptom_table[c(2,4,6,8,10,12,14,16),c(1:9,11)]


fever.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,1]),1]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,1]),10]), method = 'ML',
                         measure="PLO",model="UM.RS")
cough.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,2]),2]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,2]),10]), method = 'ML',
                         measure="PLO",model="UM.RS")
sore.throat.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,3]),3]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,3]),10]), method = 'ML',
                               measure="PLO",model="UM.RS")
headache.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,4]),4]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,4]),10]), method = 'ML',
                            measure="PLO",model="UM.RS")
diarrhea.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,5]),5]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,5]),10]), method = 'ML',
                            measure="PLO",model="UM.RS")
fatigue.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,6]),6]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,6]),10]), method = 'ML',
                           measure="PLO",model="UM.RS")
myalgia.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,7]),7]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,7]),10]), method = 'ML',
                           measure="PLO",model="UM.RS")
chills.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,8]),8]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,8]),10]), method = 'ML',
                          measure="PLO",model="UM.RS") 
dyspnea.re_sec <- rma.glmm(xi = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,9]),9]), ni = as.numeric(symptom_table_sec[!is.na(symptom_table_sec[,9]),10]), method = 'ML',
                           measure="PLO",model="UM.RS")

## obtain I2 and p-value for heterogeneity

reml_sec <- list(fever.re_sec,cough.re_sec,sore.throat.re_sec,headache.re_sec,diarrhea.re_sec,fatigue.re_sec,myalgia.re_sec,chills.re_sec,dyspnea.re_sec)
I2_sec <- estimate_sec <- se_sec <- lb_sec <- ub_sec <- n.study_sec <- rep(NA,9)

for (i in 1:9){
  estimate_sec[i] = round(reml_sec[[i]]$b,2)
  lb_sec[i] <- round(reml_sec[[i]]$ci.lb,2)
  ub_sec[i] <- round(reml_sec[[i]]$ci.ub,2)
  I2_sec[i]= round(reml_sec[[i]]$I2,2)
  se_sec[i] <- round(reml_sec[[i]]$se,2)
  n.study_sec[i] <- length(symptom_table_sec[!is.na(symptom_table_sec[,i]),i])
}

table_sec <- data.frame(estimate=estimate_sec,lb_sec,ub_sec,se=se_sec,I2_sec=paste0(I2=I2_sec,'%'),n.study=n.study_sec,n.items=n.study_sec*1)
table_sec$symptoms <- c('fever','cough','sore throat','headache','diarrhea','fatigue','myalgia','chills','dyspnea')


## for index cases only

symptom_table_index <- symptom_table[c(1,3,5,7,9,11,13,15),c(1:9,11)]


fever.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,1]),1]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,1]),10]), method = 'ML',
                         measure="PLO",model="UM.RS")
cough.re_index<- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,2]),2]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,2]),10]), method = 'ML',
                         measure="PLO",model="UM.RS")
sore.throat.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,3]),3]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,3]),10]), method = 'ML',
                               measure="PLO",model="UM.RS")
headache.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,4]),4]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,4]),10]), method = 'ML',
                            measure="PLO",model="UM.RS")
diarrhea.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,5]),5]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,5]),10]), method = 'ML',
                            measure="PLO",model="UM.RS")
fatigue.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,6]),6]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,6]),10]), method = 'ML',
                           measure="PLO",model="UM.RS")
myalgia.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,7]),7]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,7]),10]), method = 'ML',
                           measure="PLO",model="UM.RS")
chills.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,8]),8]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,8]),10]), method = 'ML',
                          measure="PLO",model="UM.RS") 
dyspnea.re_index <- rma.glmm(xi = as.numeric(symptom_table_index[!is.na(symptom_table_index[,9]),9]), ni = as.numeric(symptom_table_index[!is.na(symptom_table_index[,9]),10]), method = 'ML',
                           measure="PLO",model="UM.RS")

## obtain I2 and p-value for heterogeneity

reml_index <- list(fever.re_index,cough.re_index,sore.throat.re_index,headache.re_index,diarrhea.re_index,fatigue.re_index,myalgia.re_index,chills.re_index,dyspnea.re_index)
I2_index <- estimate_index <- se_index <- lb_index <- ub_index <- n.study_index <- rep(NA,9)

for (i in 1:9){
  estimate_index[i] = round(reml_index[[i]]$b,2)
  lb_index[i] <- round(reml_index[[i]]$ci.lb,2)
  ub_index[i] <- round(reml_index[[i]]$ci.ub,2)
  I2_index[i]= round(reml_index[[i]]$I2,2)
  se_index[i] <- round(reml_index[[i]]$se,2)
  n.study_index[i] <- length(symptom_table_index[!is.na(symptom_table_index[,i]),i])
}

table_index <- data.frame(estimate=estimate_index,lb_index,ub_index,se=se_index,I2_index=paste0(I2=I2_index,'%'),n.study=n.study_index,n.items=n.study_index*1)
table_index$symptoms <- c('fever','cough','sore throat','headache','diarrhea','fatigue','myalgia','chills','dyspnea')

## output

write.csv(table_all,'C:\\Users\\can\\Desktop\\table_all.csv')
write.csv(table_sec,'C:\\Users\\can\\Desktop\\table_sec.csv')
write.csv(table_index,'C:\\Users\\can\\Desktop\\table_index.csv')


###########
#### meta-analysis for studies in China

covid_China <- covid[c(3:14),c(1:10,13)]

## both index and secondary cases
symptom_table_China <- covid_China[,c(3:11)]

fever.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,1]),1]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,1]),9]), method = 'ML',
                         measure="PLO",model="UM.RS")
cough.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,2]),2]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,2]),9]), method = 'ML',
                         measure="PLO",model="UM.RS")
sore.throat.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,3]),3]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,3]),9]), method = 'ML',
                               measure="PLO",model="UM.RS")
headache.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,4]),4]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,4]),9]), method = 'ML',
                            measure="PLO",model="UM.RS")
diarrhea.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,5]),5]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,5]),9]), method = 'ML',
                            measure="PLO",model="UM.RS")
fatigue.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,6]),6]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,6]),9]), method = 'ML',
                           measure="PLO",model="UM.RS")
myalgia.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,7]),7]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,7]),9]), method = 'ML',
                           measure="PLO",model="UM.RS")
chills.re_all_China <- rma.glmm(xi = as.numeric(symptom_table_China[!is.na(symptom_table_China[,8]),8]), ni = as.numeric(symptom_table_China[!is.na(symptom_table_China[,8]),9]), method = 'ML',
                          measure="PLO",model="UM.RS") 

reml_China <- list(fever.re_all_China,cough.re_all_China,sore.throat.re_all_China,headache.re_all_China,diarrhea.re_all_China,fatigue.re_all_China,myalgia.re_all_China,chills.re_all_China)

## obtain I2 and p-value for heterogeneity
I2_China <- estimate_China <- se_China <- lb_China <- ub_China <- n.study_China <- rep(NA,8)

for (i in 1:8){
  estimate_China[i] = round(reml_China[[i]]$b,2)
  lb_China[i] <- round(reml_China[[i]]$ci.lb,2)
  ub_China[i] <- round(reml_China[[i]]$ci.ub,2)
  I2_China[i]= round(reml_China[[i]]$I2,2)
  se_China[i] <- round(reml_China[[i]]$se,2)
  n.study_China[i] <- length(symptom_table_China[!is.na(symptom_table_China[i]),i])/2
}

table_all_China <- data.frame(estimate=estimate_China,lb=lb_China,ub=ub_China,se=se_China,I2=paste0(I2_China,'%'),n.study=n.study_China,n.items=n.study_China*2)
table_all_China$symptoms <- c('fever','cough','sore throat','headache','diarrhea','fatigue','myalgia','chills')

## index cases

symptom_China_index <- symptom_table_China[c(1,3,5,7,9,11),c(1:3,5:7,9)]


fever.re_index_China <- rma.glmm(xi = as.numeric(symptom_China_index[!is.na(symptom_China_index[,1]),1]), ni = as.numeric(symptom_China_index[!is.na(symptom_China_index[,1]),7]), method = 'ML',
                           measure="PLO",model="UM.RS")
cough.re_index_China <- rma.glmm(xi = as.numeric(symptom_China_index[!is.na(symptom_China_index[,2]),2]), ni = as.numeric(symptom_China_index[!is.na(symptom_China_index[,2]),7]), method = 'ML',
                          measure="PLO",model="UM.RS")
sore.throat.re_index_China <- rma.glmm(xi = as.numeric(symptom_China_index[!is.na(symptom_China_index[,3]),3]), ni = as.numeric(symptom_China_index[!is.na(symptom_China_index[,3]),7]), method = 'ML',
                                 measure="PLO",model="UM.RS")
diarrhea.re_index_China <- rma.glmm(xi = as.numeric(symptom_China_index[!is.na(symptom_China_index[,4]),4]), ni = as.numeric(symptom_China_index[!is.na(symptom_China_index[,4]),7]), method = 'ML',
                              measure="PLO",model="UM.RS")
fatigue.re_index_China <- rma.glmm(xi = as.numeric(symptom_China_index[!is.na(symptom_China_index[,5]),5]), ni = as.numeric(symptom_China_index[!is.na(symptom_China_index[,5]),7]), method = 'ML',
                             measure="PLO",model="UM.RS")
myalgia.re_index_China <- rma.glmm(xi = as.numeric(symptom_China_index[!is.na(symptom_China_index[,6]),6]), ni = as.numeric(symptom_China_index[!is.na(symptom_China_index[,6]),7]), method = 'ML',
                             measure="PLO",model="UM.RS")


## obtain I2 and p-value for heterogeneity

reml_index_China <- list(fever.re_index_China,cough.re_index_China,sore.throat.re_index_China,diarrhea.re_index_China,fatigue.re_index_China,myalgia.re_index_China)
I2_index_China <- estimate_index_China <- se_index_China <- lb_index_China <- ub_index_China <- n.study_index_China <- rep(NA,6)

for (i in 1:6){
  estimate_index_China[i] = round(reml_index_China[[i]]$b,2)
  lb_index_China[i] <- round(reml_index_China[[i]]$ci.lb,2)
  ub_index_China[i] <- round(reml_index_China[[i]]$ci.ub,2)
  I2_index_China[i]= round(reml_index_China[[i]]$I2,2)
  se_index_China[i] <- round(reml_index_China[[i]]$se,2)
  n.study_index_China[i] <- length(symptom_China_index[!is.na(symptom_China_index[,i]),i])
}

table_index_China <- data.frame(estimate=estimate_index_China,lb_index_China,ub_index_China,se=se_index_China,I2_index_China=paste0(I2_index_China,'%'),n.study=n.study_index_China,n.items=n.study_index_China*1)
table_index_China$symptoms <- c('fever','cough','sore throat','diarrhea','fatigue','myalgia')

## secondary cases

symptom_China_sec <- symptom_table_China[c(2,4,6,8,10,12),c(1:3,5:7,9)]


fever.re_sec_China <- rma.glmm(xi = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,1]),1]), ni = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,1]),7]), method = 'ML',
                                 measure="PLO",model="UM.RS")
cough.re_sec_China <- rma.glmm(xi = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,2]),2]), ni = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,2]),7]), method = 'ML',
                                 measure="PLO",model="UM.RS")
sore.throat.re_sec_China <- rma.glmm(xi = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,3]),3]), ni = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,3]),7]), method = 'ML',
                                       measure="PLO",model="UM.RS")
diarrhea.re_sec_China <- rma.glmm(xi = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,4]),4]), ni = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,4]),7]), method = 'ML',
                                    measure="PLO",model="UM.RS")
fatigue.re_sec_China <- rma.glmm(xi = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,5]),5]), ni = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,5]),7]), method = 'ML',
                                   measure="PLO",model="UM.RS")
myalgia.re_sec_China <- rma.glmm(xi = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,6]),6]), ni = as.numeric(symptom_China_sec[!is.na(symptom_China_sec[,6]),7]), method = 'ML',
                                   measure="PLO",model="UM.RS")


## obtain I2 and p-value for heterogeneity

reml_sec_China <- list(fever.re_sec_China,cough.re_sec_China,sore.throat.re_sec_China,diarrhea.re_sec_China,fatigue.re_sec_China,myalgia.re_sec_China)
I2_sec_China <- estimate_sec_China <- se_sec_China <- lb_sec_China <- ub_sec_China <- n.study_sec_China <- rep(NA,6)

for (i in 1:6){
  estimate_sec_China[i] = round(reml_sec_China[[i]]$b,2)
  lb_sec_China[i] <- round(reml_sec_China[[i]]$ci.lb,2)
  ub_sec_China[i] <- round(reml_sec_China[[i]]$ci.ub,2)
  I2_sec_China[i]= round(reml_sec_China[[i]]$I2,2)
  se_sec_China[i] <- round(reml_sec_China[[i]]$se,2)
  n.study_sec_China[i] <- length(symptom_China_sec[!is.na(symptom_China_sec[,i]),i])
}

table_sec_China <- data.frame(estimate=estimate_sec_China,lb_sec_China,ub_sec_China,se=se_sec_China,I2_sec_China=paste0(I2_sec_China,'%'),n.study=n.study_sec_China,n.items=n.study_sec_China*1)
table_sec_China$symptoms <- c('fever','cough','sore throat','diarrhea','fatigue','myalgia')

#### output

write.csv(table_all_China,'C:\\Users\\can\\Desktop\\table_China_all.csv')
write.csv(table_sec_China,'C:\\Users\\can\\Desktop\\table_China_sec.csv')
write.csv(table_index_China,'C:\\Users\\can\\Desktop\\table_China_index.csv')



