library(h2o)
library(data.table)


localH2O = h2o.init()

dth2o = as.h2o(card_fin)  


# h2o.glm  
fit = h2o.glm(y="target", training_frame=dth2o,  seed=17,
              family="binomial", nfolds=5, alpha=1, lambda_search=TRUE) # summary(fit)

# variable importance
varimp = data.table(h2o.varimp(fit))[names!=""][!is.na(coefficients) & coefficients > 0 & sign == "POS"]
var_sel3 = c(varimp$names, "target")
var_sel3




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
