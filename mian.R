rm(list = ls())
setwd("D:/R Project/cathay_competition")
library(xgboost)
library(dplyr)
library(caret)
library(Matrix)
library(caTools)
library(ggplot2)
library(mice)

sub<-read.csv("submit_test1006-1.csv",fileEncoding = "big-5")
train<-read.csv("train.csv",fileEncoding = "big-5") 
test <- read.csv("test.csv",fileEncoding = "big-5")
test <- mutate(test,Y1="")
all <- rbind( train,test)


#--------factor處理----------



all$AGE <- factor(all$AGE,levels = c("低", "中","中高","高"),ordered = TRUE)
all$EDUCATION_CD <- factor(all$EDUCATION_CD,levels = c("1", "2","3","4"), ordered = TRUE) #學歷有高低之分
all$MARRIAGE_CD <- factor(all$MARRIAGE_CD)
all$APC_1ST_AGE <- factor(all$APC_1ST_AGE,levels = c("低", "中","中高","高"), ordered = TRUE)
all$INSD_1ST_AGE <- factor(all$INSD_1ST_AGE,levels = c("低", "中","中高","高"), ordered = TRUE)
all$RFM_R <- factor(all$RFM_R,levels = c("低", "中","中高","高"), ordered = TRUE)
all$REBUY_TIMES_CNT <- factor(all$REBUY_TIMES_CNT,levels = c("低", "中","中高","高"), ordered = TRUE)
all$LEVEL <- factor(all$LEVEL,levels = c(1,2,3,4,5), ordered = TRUE) #學歷有高低之分
all$RFM_M_LEVEL <- factor(all$RFM_M_LEVEL,levels = c(3,5,7,8,9,10), ordered = TRUE) #學歷有高低之分
all$LIFE_CNT <- factor(all$LIFE_CNT,levels = c("低", "中","高"), ordered = TRUE) #學歷有高低之分
all$Y1 <- as.integer(all$Y1) -1



#--------新增欄位----------


all$OUTPATIENT_SURGERY_AMT_CUT = as.factor(cut(all$OUTPATIENT_SURGERY_AMT,
                                                 breaks = c(-Inf,0,0.1,Inf),   #Inf 為無限值
                                                 labels = c( "1" ,"2", "3")))


all$PAY_LIMIT_MED_MISC_AMT_CUT = as.factor(cut(all$PAY_LIMIT_MED_MISC_AMT,
                                                 breaks = c(-Inf,0,0.5,Inf),   #Inf 為無限值
                                                 labels = c( "1" ,"2", "3")))



all$AG_CNT_CUT = as.factor(cut(all$AG_CNT,
                                 breaks = c(-Inf,0,1,Inf),   #Inf 為無限值
                                 labels = c( "1" ,"2", "3")))

all$AG_NOW_CNT_CUT = as.factor(cut(all$AG_NOW_CNT,
                                     breaks = c(-Inf,0,1,Inf),   #Inf 為無限值
                                     labels = c( "1" ,"2", "3")))

all$CLC_CUR_NUM_CUT = as.factor(cut(all$CLC_CUR_NUM,
                                      breaks = c(-Inf,0,1,Inf),   #Inf 為無限值
                                      labels = c( "1" ,"2", "3")))



all$ANNUITY_AMT_CUT= as.factor(cut(all$ANNUITY_AMT,
                                   breaks = c(-Inf,0.5,Inf),   #Inf 為無限值
                                   labels = c( "1" ,"2")))


all$L1YR_A_ISSUE_CNT_CUT <- as.factor(cut(all$L1YR_A_ISSUE_CNT,
                                          breaks = c(-Inf,0,5,Inf),   #Inf 為無限值
                                          labels = c( "1" ,"2" ,"3")))

all$CHANNEL_A_POL_CNT_CUT <- as.factor(cut(all$CHANNEL_A_POL_CNT,
                                           breaks = c(-Inf,0,37,Inf),   #Inf 為無限值
                                           labels = c( "1" ,"2" ,"3")))

all$INSD_CNT_CUT <- as.factor(cut(all$INSD_CNT,
                                  breaks = c(-Inf,0,Inf),   #Inf 為無限值
                                  labels = c( "1" ,"2")))

all$BMI_CUT= as.factor(cut(all$BMI ,
                           breaks = c(-Inf,0.1,0.3,0.5,Inf),   #Inf 為無限值
                           labels = c( "1" ,"2","3","4")))

all$TERMINATION_RATE_CUT = as.factor(cut(all$TERMINATION_RATE ,
                                         breaks = c(-Inf,0,30,80,Inf),   #Inf 為無限值
                                         labels = c( "1","2","3","4")))

all$LONG_TERM_CARE_AMT_CUT = as.factor(cut(all$LONG_TERM_CARE_AMT,
                                           breaks = c(-Inf,0,0.1,0.3,Inf), #Inf 為無限值
                                           labels = c( "1" ,"2","3","4")))

all$LIFE_INSD_CNT_CUT = as.factor(cut(all$LIFE_INSD_CNT,
                                      breaks = c(-Inf,0.1,0.2,Inf),   #Inf 為無限值
                                      labels = c( "1" ,"2","3")))


all$APC_1ST_AGE_NUM <-as.integer(all$APC_1ST_AGE)
all$AGE_NUM <-as.integer(all$AGE)
all$EDUCATION_CD_NUM <-as.integer(all$EDUCATION_CD)
all$INSD_1ST_AGE_NUM <-as.integer(all$INSD_1ST_AGE)
all$RFM_R_NUM <-as.integer(all$RFM_R)
all$REBUY_TIMES_CNT_NUM <-as.integer(all$REBUY_TIMES_CNT)
all$LEVEL_NUM <- as.integer(all$LEVEL)
all$RFM_M_LEVEL_NUM <- as.integer(all$RFM_M_LEVEL)
all$LIFE_CNT_NUM <- as.integer(all$LIFE_CNT)



all$System_NA_AMT = as.factor(is.na(all$DIEBENEFIT_AMT))

all$System_NA_X_IND = as.factor(is.na(all$X_A_IND))

all$System_NA_APC_1ST = as.factor(is.na(all$APC_1ST_AGE))

all$System_NA_ISSUE_INSD = as.factor(is.na(all$IF_ISSUE_INSD_A_IND))

all$System_NA_ADD_INSD = as.factor(is.na(all$IF_ADD_INSD_F_IND))

all$System_NA_FINANCETOOLS = as.factor(is.na(all$FINANCETOOLS_A ))

all$System_NA_IND = as.factor(is.na(all$A_IND ))

all$X_IND_SUM = as.factor(apply(all[,74:81],1,function(x){sum(x == "Y")}))

all$F_SUM = as.factor(apply(all[,125:131],1,function(x){sum(x == "Y")}))

all$CITY_CD = as.factor(all$CHARGE_CITY_CD==all$CONTACT_CITY_CD)

all$DT_AB = as.factor(all$LAST_A_CCONTACT_DT=="Y" & all$LAST_B_CONTACT_DT=="Y")

all$DT_A_B = as.factor(all$LAST_A_CCONTACT_DT != all$LAST_B_CONTACT_DT)

all$IDT_AB = as.factor(all$LAST_A_ISSUE_DT=="Y" & all$LAST_B_ISSUE_DT=="Y")

all$IDT_A_B = as.factor(all$LAST_A_ISSUE_DT != all$LAST_B_ISSUE_DT)

all$REAL_IND_SY = as.factor(all$IF_S_REAL_IND=="Y" & all$IF_Y_REAL_IND=="Y")








#--------NA值處理----------

x <- c()
for(i in 1:length(all[])){
  if( is.factor(all[,i])  ){
    x <- c(x,i)                      #x記錄下是factor的欄位向量
    all[,i] <- as.integer(all[,i])  #將factor欄位轉換為integer
  }
}

all[is.na(all)] <- (-999)        #將NA轉換成-999

for(q in x){              
  all[,q] <- as.factor(all[,q])   #將integer欄位轉換為factor，才能轉為dummy varience
}


#-----------轉dummy與拆開train與test-----------



all1 = cbind(all[-x],model.matrix(~ . - 1, all[x]))
save(all1,file = "final1.RData")


#----------train/ test------



train1 <- all1[c(1:100000),]
test1 <- all1[c(100001:250000),]



#----------XGboost模型訓練：參數以list傳入---------------
grep("CUS_ID",colnames(train1))
grep("Y1", colnames(train1))
DM = xgb.DMatrix(data=data.matrix(train1[-c(1,39)]), label=train1$Y1)


t0 = Sys.time()
df = data.frame()
for(i in 1:1) for(g in c(0.3)) for(d in c(4))
  for(e in seq(0.04, 0.12, 0.02)) {
    set.seed(i*123)
    cv = xgb.cv(data=DM, nfold=15, nrounds=360, prediction=F, 
                verbose=0, early_stopping_rounds = 10,
                param=list("objective" = "binary:logistic",
                           "eval_metric" = "auc",
                           "max_depth" = d,
                           "eta" = e,
                           "gamma"= g ) )
    r = cv$best_iteration
    auc = cv$evaluation_log$test_auc_mean[r]
    s = sprintf("d=%d, e=%.3f, g=%.3f: auc=%f, rnd=%d",d,e,g,auc,r)
    print(s)
    df = rbind(df, data.frame(d,e,g,auc,r)) }
Sys.time() - t0 

df = aggregate(cbind(auc, r) ~ d + e + g, df, mean)
df$d = as.factor(df$d)
df$g = as.factor(df$g)
ggplot(data=df,aes(x=e, y=auc)) + theme_light() +
  geom_line(aes(colour=g),size=1.2,alpha=0.5) +
  geom_point(aes(colour=g),size=1.5) + 
  facet_wrap(~d, nrow=1)

#--------------train xgboost model------------------
xgb1 = xgboost(DM, nrounds = 172, objective = "binary:logistic",
               eval_metric = "auc", verbose=1,
               params=list("eta"=0.06, "max_depth"=4, "gamma"=0.3) )



pred = predict(xgb1, data.matrix( test1[,c(-1,-39)] ),missing = -999)
#m_preds = data.frame( xgb1 = pred )
answer = data.frame(
  CUS_ID = test1[,1], 
  Ypred = pred
)
hist(pred)

write.csv(answer,file = "submit_test1005_2.csv",row.names = F)


sum(sapply(all[x], class) != 'factor')
sapply(all[x], function(x) length(levels(x)) ) %>% {.[.>12]}



