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

Prosper = read.csv(paste(datapath, "ProsperLoanData.csv", sep='') , encoding='UTF-8')


# x_fact<-sapply(Prosper,is.logical)
# myda<-Prosper[,x_fact,drop=FALSE]
# str(myda)


# sub ----

loan_var = c(
  "IncomeVerifiable",
  "IsBorrowerHomeowner",
  "CurrentlyInGroup",
  
  "IncomeRange",
  "TotalTrades",
  "EmploymentStatusDuration",
  "EmploymentStatus",
  
  "ClosedDate",
  "LoanOriginationDate",
  "FirstRecordedCreditLine",
  "InquiriesLast6Months",
  "TotalInquiries",
  "CurrentDelinquencies",
  "AmountDelinquent",
  
  "BankcardUtilization",
  "AvailableBankcardCredit",
  "LoanCurrentDaysDelinquent",
  "DelinquenciesLast7Years",
  "CreditGrade"
  
)

summary(Prosper$CreditGrade)

# loaddata and tag ----
data = Prosper[loan_var]

# eda
summary(data$CreditGrade)
# summary(data$CurrentDelinquencies)
summary(data$LoanCurrentDaysDelinquent)
table(data$LoanCurrentDaysDelinquent)




# feature ----

# time
data$LoanOriginationDate =  substr(data$LoanOriginationDate,1,10)
data$OriginationDate = strptime(data$LoanOriginationDate, "%Y/%m/%d")

data$ClosedDate=  substr(data$ClosedDate,1,10)
data$CloseDate= strptime(data$ClosedDate, "%Y/%m/%d")


data$LoanDays= as.integer(difftime(  data$CloseDate , data$OriginationDate ,  
                                    units =  "days"))  
summary(data$LoanDays)




# flag
data$Flag_MisCredit = ifelse( (data$CreditGrade)== "" , "MisGrade", "Graded")
table(data$Flag_MisCredit)

data$Flag_death = ifelse(data$LoanCurrentDaysDelinquent>= 1300, 0, 1)
table(data$Flag_death)

# data=data[data$CurrentDelinquencies>0,]



# sample
set.seed(17)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


# head(train)

# plot( Hmisc::naclus( train  ))
# vc =  Hmisc::varclus (~ . , sim='hoeffding' , data=train)
# plot (vc)



# survive ----


dd  = datadist(train)
options(datadist ='dd')
#options(prType ='latex')

units(train$LoanDays) =  'Day'
units(train$LoanCurrentDaysDelinquent ) ='Day'

surv_object <- Surv( time=train$LoanDays, #time=train$CloseDate ,  type='interval',
                      event=train$Flag_death  )


survplot( npsurv(surv_object ~ train$Flag_MisCredit ), conf ='none',  
           fun=qnorm , logt = TRUE ) 



##Create a Cox PH model for the training data.
 
cox.mod=cph(surv_object~  IsBorrowerHomeowner  + BankcardUtilization   
            ,data= train  , x=T,y=T,surv=TRUE ,  time.inc=5*12 ) #dist='lognormal',

cox.mod

print(cox.mod, coefs = FALSE )
anova(cox.mod)
plot(summary(cox.mod), log=TRUE , main ='')


##Checking Proportional Hazards
z  = predict (cox.mod, type ='terms')


# s t o r e r a w x , y s o c a n g e t r e s i d u a l s
f.short =  cph(surv_object ~ z, x=TRUE , y= TRUE )

phtest =  cox.zph ( f.short , transform ='identity')
phtest


plot(phtest , var='IsBorrowerHomeowner')
plot(phtest , var='BankcardUtilization')


##Testing Interactions

##Test all interactions with dose
z.var = z[,"IsBorrowerHomeowner"]   
z.other =  z[,-1]  
f.ia =  cph(surv_object~ z.var * z.other )
print( anova ( f.ia ), size ='tsz')



##Plot relationship between each predictor and log lambda
ggplot(rms::Predict(cox.mod), sepdiscrete ='vertical',nlevels =4, vnames ='names')
          



##Validating the Model
set.seed (1)  
v  = validate(cox.mod,   B =300)  #  method="crossvalidation" ,
# latex(v, file ='')
v


# #cal  validate 1-year survival probability estimates
# cal = calibrate(cox.mod , B=300 , u= 5*12 , maxdim = 1)
# cal =  calibrate(cox.mod, cmethod ='KM', u=1, m=60 , B=120 , pr= FALSE )
# plot(cal)


## Presenting the Model
plot ( summary (cox.mod), log=TRUE , main ='') # BankcardUtilization=c(0.1 ,1.5)



##  

expected.surv  = Mean (cox.mod)
quantile.surv  = Quantile (cox.mod)


expected.surv  = function(lp = NULL , parms = 0.802352037606488 )
{
  names ( parms ) =  NULL
  exp(lp + exp (2 * parms )/2)
}

quantile.surv  = function(q = 0.5 , lp = NULL , parms = 0.802352037606488 )
{
  names ( parms ) =  NULL
  f  = function (lp , q, parms ) lp + exp( parms ) * qnorm (q)
  names (q) =  format (q)
  drop (exp( outer (lp , q, FUN = f, parms = parms )))
}


median.surv  = function (x) quantile.surv(lp=x)


nom  =
nomogram ( cox.mod ,
           BankcardUtilization = c(0.1 , 0.5 , 1 ,1.5)  ,
           fun= list ('Median Survival Time '= median.surv ,
                      'Mean Survival Time ' = expected.surv ),
           fun.at =  c(.1 ,.25 ,.5 ,1 ,2 ,5 ,10 ,20 ,40)
)

plot(nom , xfrac =.45 , cex.var =0.8, cex.axis =.75 , lmgp = 0.25)



## predict ----
## Approximating the Full Model

Z  = predict(cox.mod) # X * b e t a h a t
a  = ols(Z ~  BankcardUtilization , sigma =1)

fastbw(a, aics =10000)  





# ##  Draw nomogram, with predictions stated 4 ways ----
# 
# surv =  Survival(cox.mod)
# surv3  = function(x) surv(1*2, lp=x)
# surv5  = function(x) surv(5*2, lp=x)
# quan =  Quantile(cox.mod)
# med =  function(x) quan(lp=x)/12
# 
# 
# nom  = nomogram(cox.mod. ,  
#                 fun= list(surv3 , surv5 , med)
#                 )
# 
# 
# nom  = nomogram(cox.mod,    BankcardUtilization = c(0.1 , 0.5 , 1 ,1.5) ,
#                 fun= list(surv3 , surv5 , med),
#                 funlabel =c('3 -year Survival ','5 -year Survival ',
#                             'Median Survival Time ( years )'),
#                 fun.at = list(ss , ss , c(.5 ,1:6) ))
# 
# plot (nom , xfrac =.65 , lmgp =.35)






# # other----
# 
# ##Validate the model with the test data
# test_surv=with(test,  Surv(time =  test$CurrentDelinquencies,  event = test$censored ) )
# validated=val.surv(cox.mod, newdata= test , S=test_surv)
#   
# 
# v  = validate (cox.mod, B =300)
# cal  = calibrate (cox.mod  , B=300 ) # , u=5*12, maxdim =4)
# # summary(validated)
# 
# ##Now what I need is to take the external validation and compute the  c-index. 
# 
# 
# ##Create your survival estimates
# estimates=survest(cox.mod,newdata=test,times=5*365)$surv
# 
# 
# ##Determine concordance
# rcorr = rcorr.cens(x=estimates,S= test_surv )
# rcorr


