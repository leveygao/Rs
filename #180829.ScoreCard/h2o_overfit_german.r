
#using dataset germancredit as sample
data("GermanCredit")
Sub_German=GermanCredit[  ,c("amount","present_residence","duration","age")]    #ease for demo
target=ifelse(GermanCredit$credit_risk=="good",0,1)
data=cbind(Sub_German,target)
  
  
library(h2o)
library(data.table)


localH2O = h2o.init()

dth2o = as.h2o(data)  


# h2o.glm  
fit = h2o.glm(y="target", training_frame=dth2o,  seed=17,
              family="binomial", nfolds=2, alpha=1, lambda_search=TRUE) # summary(fit)

# # variable importance
# varimp = data.table(h2o.varimp(fit))[names!=""][!is.na(coefficients) & coefficients > 0 & sign == "POS"]
# var_sel3 = c(varimp$names, "creditability")



model_fit_h2o= fit@model
class(model_fit_h2o)


model_fit_coe_table= model_fit_h2o$coefficients_table
class(model_fit_coe_table)

#
dt_h2o_pred= predict(fit, type='response', dth2o)

dt_h2o_pred_df=as.data.frame(dt_h2o_pred) 
class(dt_h2o_pred_df)
dt_h2o_num=dt_h2o_pred_df$p1
class(dt_h2o_num)


#
perf_eva(data$target, dt_h2o_num, type = c("ks","lift","roc","pr"))





# glm ------
model = glm(target ~ ., family = binomial(link='logit'), data = data)
class(model)

# Select a formula-based model by AIC
m_step = step(model, direction="both", trace=FALSE)
model_fin = eval(m_step$call)
class(model_fin)

#predicted proability
dt_pred = predict(model_fin, type='response', data)

#
perf_eva(data$target, dt_pred, type = c("ks","lift","roc","pr"))
head(dt_pred)

