## library  ----

library(data.table)
library(scorecard)
library(survival)
library(survminer)
library(rms)
library(Hmisc)


# load data ----


# read&subset data
datapath="D:\\Users\\GAOYUAN910\\Desktop\\ALL_CODE\\Py\\#002.Scorecard\\"

modelpath="D:\\Users\\GAOYUAN910\\Desktop\\DOCU\\MODEL_DOCU\\#19.1.14 Survival\\"

support2 = read.csv(paste(datapath, "support2.csv", sep='') , encoding='UTF-8')


# x_fact<-sapply(Prosper,is.logical)
# myda<-Prosper[,x_fact,drop=FALSE]
# str(myda)


# sub ----


# loaddata and tag ----
data = support2



