## library  ----

library(data.table)
require(zoo)
library(glue)




# load data ----


# read&subset data
datapath="D:\\Users\\GAOYUAN910\\Desktop\\DOCU\\MODEL_DOCU\\DATA\\Loanbill_20181102\\"

modelpath="D:\\Users\\GAOYUAN910\\Desktop\\DOCU\\MODEL_DOCU\\#19.1.14 Survival\\"

Loanbill = read.csv(paste(datapath, "Loanbill_20181102.csv", sep='') , encoding='UTF-8')
Repay= read.csv(paste(datapath, "Repayschedule_20181102.csv", sep='') , encoding='UTF-8')

# sub ----

loan_var = c(
  "memberId",
  "orderNo",
  "loanDate",
  "loanAmount",
  "totalNum",
  "UPDATE_DAY"
  )

pay_var = c(
  "memberId",
  "orderNo",
  "orderType",
  "num",
  "repayStatus",
  "isOverdue",
  
  "repayDate",
  "payDate",
  "overdueNum",
  "crtTime"
)



Loanbill = Loanbill[loan_var]
Repay = Repay[pay_var]


Loanbill$loanDate = as.Date( Loanbill$loanDate, "%Y-%m-%d")
Repay$repayDate  = as.Date( Repay$repayDate  , "%Y-%m-%d") 
Repay$paydate  = substr(Repay$payDate,1,9)
table(Repay$paydate)
Repay$pay_date = ifelse((glue::trim(Repay$paydate))=="",
                        as.Date( Repay$paydate  , "%d%b%Y"), NA)


 
table(Repay$pay_date)

data = merge(Loanbill , Repay, by= "orderNo")
data$repaymonth= as.yearmon(data$repayDate, "%y-%m")
data$loanmth= as.yearmon(data$loanDate, "%y-%m")


subset = subset(data, ( loanmth >= "2018-08"  & repaymonth < "2018-10" & repayStatus!=3))
subset= data.table::setorder(subset ,  memberId.x, orderNo , num)

# head(data)


table(data$repaymonth)
table(data$loanmth)

summary(data$repaymonth)

 



summary(ProsperLoanData$LoanOriginationDate)


















