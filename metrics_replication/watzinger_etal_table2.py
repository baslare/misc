# -*- coding: utf-8 -*-
"""
Created on Thu Feb 24 18:27:55 2022

@author: Efe
"""

import pandas as pd
import os
import statsmodels.formula.api as sm 
import re


import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri



os.getcwd()


dt = pd.read_stata("watzinger_etal/Kit_WNFS/data/mainAnalysis_AEJ.dta")
dt["pre_cd_cit"] = dt["pre_cd_cit_1956"]
dt = dt[~((dt["citing_y"]<1949 )| (dt["citing_y"]> (dt["publn_y"]+16)))]
dt = dt[dt["controlGroup"] < 3]


dt0 = dt.filter(items=["cites_in_y","pat_publn_id", "nclass0" ,"citing_y"])
dt1 = dt.filter(regex=("pre_cd_cit"))
dt2 = dt.filter(items=["cd","publn_y","appln_y","subcat","transistor"])

dta = pd.concat([dt0,dt1,dt2],axis=1)


del dt0,dt1,dt2

dta["cites_in_y"] = dta["cites_in_y"].apply(lambda x: 100*int(x))
dta.reset_index(inplace=True,drop=True )

index_cols = list(dta.columns)
index_cols.remove("citing_y")
index_cols.remove("cites_in_y")


dt_wide = dta.pivot(index=index_cols,columns="citing_y",values="cites_in_y").reset_index()


rename_dict = {x:"cites_in_y"+str(x) for x in dt_wide.columns[17:39]}
dt_wide.rename(rename_dict,axis=1,inplace=True)
dt_wide.reset_index(inplace=True,drop=True)

### CEM part - call R as the R package is awesome

match_it = importr("MatchIt")
pandas2ri.activate()
robjects.globalenv["dt_wide_r1"] = pandas2ri.py2rpy_pandasdataframe(dt_wide)
robjects.r("cem_result <- matchit(cd~ publn_y+nclass0+pre_cd_cit,data=dt_wide_r1,method='cem',cutpoints=list(publn_y=0,nclass0=0,pre_cd_cit=0))")
robjects.r("match_weights <- cem_result$weights")
match_weight = robjects.globalenv["match_weights"]
dt_wide["cem_matched"] = match_weight
dt_wide_cem = dt_wide[dt_wide["cem_matched"] != 0]


no_treated_1 = len(dt_wide_cem[dt_wide_cem["cd"] == 1]["pre_cd_cit"])

#### melt it back

dt_wide_cem.reset_index(inplace=True,drop=True)
dt_long = pd.wide_to_long(dt_wide_cem,"cites_in_y",i=["pat_publn_id","cd","publn_y","nclass0","pre_cd_cit"],j="year").reset_index()

#### next treatments

dt_long["treatment"] = [int(x) for x in (dt_long["year"] > 1955) ]

dt_long["tre_x_cd"] = dt_long["treatment"]*dt_long["cd"]

number_treated = sum(dt_long["cd"][dt_long["cd"] == 1])

#### diff-in-diff
#the dependent variable has some missing values,
#the clustered error function doesnt handle it well
#so, we need to slice the df first

dt_long_sub = dt_long[dt_long["cites_in_y"].isna()]
model = sm.wls(formula="cites_in_y ~ cd + treatment + tre_x_cd",data=dt_long_sub,weights=dt_long_sub["cem_matched"])
results_ = model.fit(method="pinv", cov_type="cluster",cov_kwds={"groups":dt_long_sub["nclass0"]})
results_2 = model.fit(method="pinv", cov_type="HC3")
print(results_.summary())
print(results_2.summary())

y_summ = dt_long["cites_in_y"].describe()




