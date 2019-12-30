##Multi-Linear-Logistic-Regression-Inferential-Analysis

##Working Environment : RStudio 1.2.5001
##The dataset contains 50 observations of 5 variables : Spending on R&D, Administration, Marketing, the State (New York, California, Florida) in which the Startup is based and the Profit generated by each Startup.
##The objective is to attempt a prediction of a any startup's profitability based on its spending and the State.


##Exloratory Data Anaylysis
>> str(startup_data)
'data.frame':	50 obs. of  5 variables:
 $ R.D.Spend      : num  165349 162598 153442 144372 142107 ...
 $ Administration : num  136898 151378 101146 118672 91392 ...
 $ Marketing.Spend: num  471784 443899 407935 383200 366168 ...
 $ State          : Factor w/ 3 levels "California","Florida",..: 3 1 2 3 2 3 1 2 3 1 ...
 $ Profit         : num  192262 191792 191050 182902 166188 ...
> 
 