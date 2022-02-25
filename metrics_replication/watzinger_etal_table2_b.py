# -*- coding: utf-8 -*-
"""
Created on Fri Feb 25 17:05:05 2022

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
dt = dt[dt["controlGroup"] < 3]
dtb = dt[(dt["publn_y"] < 1949) & (dt["publn_y"] > 1939)]
dtb = dtb[~((dtb["citing_y"] <1949 ) | (dtb["citing_y"] > (dtb["publn_y"]+16))) ]  
dtb.reset_index(inplace=True,drop=True)


dtb["c_0_total"] = 0
dtb["c_4_total"] = 0
dtb["c_0_ys"] = 0
dtb["c_4_ys"] = 0
dtb["c_4_HN"] = 0 
dtb["c_0_HN"] = 0
dtb["c_0_conc"] = 0
dtb["c_4_conc"] = 0
dtb["c_0_ys_conc"] = 0
dtb["c_4_ys_conc"] = 0
dtb["c_0_ys_HN"] = 0
dtb["c_4_ys_HN"] = 0

regex_list = ["ccom_[0-9]+$","y_ys_[0-9]+$","y_ccom_[0-9]+_HYN$","y_ccom_[0-9]+_HN$", "_8_conc$","_8_ys_conc$"]

col_list_tuple = [("c_0_total","c_4_total"),
                  ("c_0_ys","c_4_ys"),
                  ("c_0_ys_HN","c_4_ys_HN"),
                  ("c_0_HN","c_4_HN"),
                  ("c_0_conc","c_4_conc"),
                  ("c_0_ys_conc","c_4_ys_conc")]


                  

for reg,tup in zip(regex_list,col_list_tuple):
    cols = [bool(re.search(reg, str(x))) for x in dtb.columns]
    goal_cols = dtb.columns[cols]
    lower_15 = [int(re.search("[0-9]+", str(x)).group(0)) <= 15  for x in goal_cols]
    upper_15 = [int(re.search("[0-9]+", str(x)).group(0)) > 15  for x in goal_cols]

    dtb[tup[0]] = dtb[goal_cols[lower_15]].sum(axis=1)
    dtb[tup[1]] = dtb[goal_cols[upper_15]].sum(axis=1)
 

dtb["c_0_LN"] = dtb["c_0_total"]-dtb["c_0_HN"]
dtb["c_4_LN"] = dtb["c_4_total"]-dtb["c_4_HN"]

dtb["c_0_ys_LN"] = dtb["c_0_ys"]-dtb["c_0_ys_HN"]
dtb["c_4_ys_LN"] = dtb["c_4_ys"]-dtb["c_4_ys_HN"]

dtb["c_0_conc_LN"] = dtb["c_0_total"]-dtb["c_0_conc"]
dtb["c_4_conc_LN"] = dtb["c_4_total"]-dtb["c_4_conc"]

dtb["c_0_ys_conc_LN"] = dtb["c_0_ys"]-dtb["c_0_ys_conc"]
dtb["c_4_ys_conc_LN"] = dtb["c_4_ys"]-dtb["c_4_ys_conc"]

dtb["cites_yso_not_comm"] = dtb["cites_not_comm"]-dtb["cites_ys_not_comm"]
dtb["cites_yso_comm"] = dtb["cites_comm"]-dtb["cites_ys_comm"]

dt_k = dtb[["cites_in_y_w_Kog_xi_av","total_cites_in_y_US","self_cites","self_cites_comm","self_cites_not_comm","cites_in_y","cites_comm","cites_ys_comm","cites_yso_comm","cites_ysc_comm","cites_not_comm","cites_ys_not_comm","cites_yso_not_comm","cites_ysc_not_comm","cites_in_y_Young_Small","c_0_HN","c_0_LN","c_4_HN","c_4_LN","c_0_total","c_4_total","c_0_ys","c_4_ys","c_0_conc_LN","c_0_conc","c_4_conc_LN","c_4_conc","c_0_ys_HN","c_0_ys_LN","c_4_ys_HN","c_4_ys_LN","c_0_ys_conc","c_0_ys_conc_LN","c_4_ys_conc","c_4_ys_conc_LN"]]
k_cols = dt_k.columns


dt_k = dt_k.apply(  lambda x: x.apply(lambda y: 100*int(y)))

dt0 = dtb.filter(items=["pat_publn_id","nclass0","citing_y"])
dt1 = dtb.filter(regex=("pre_cd_cit"))
dt2 = dtb.filter(items=["cd","publn_y","appln_y","subcat","transistor"])

dtb = pd.concat([dt_k,dt0,dt1,dt2],axis=1)
del dt_k, dt0, dt1, dt2

dtb.reset_index(inplace=True,drop=True)

index_cols = list(dtb.columns)
for x in k_cols:
    index_cols.remove(x)
index_cols.remove("citing_y")


dtb_wide = dtb.pivot(index=index_cols,columns="citing_y",values=k_cols).reset_index()

dtb_newcols = [str(x[0]) + str(x[1]) for x in dtb_wide.columns]
dtb_wide.columns = dtb_newcols
#dtb_wide.reset_index(inplace=True,drop=True)

#ysumm results make no sense!


### CEM part - call R as the R package is awesome

match_it = importr("MatchIt")
robjects.r("rm(list=ls())")
pandas2ri.activate()
robjects.globalenv["dtb_wide"] = pandas2ri.py2rpy_pandasdataframe(dtb_wide)
robjects.r("result_b <- matchit(cd ~ publn_y+ nclass0 + pre_cd_cit,data=dtb_wide,method='cem',cutpoints=list(publn_y=0,nclass0=0,pre_cd_cit=0))")
robjects.r("match_weights <- result_b$weights")
match_weight = robjects.globalenv["match_weights"]
dtb_wide["cem_matched"] = match_weight
dtb_wide_cem = dtb_wide[dtb_wide["cem_matched"] != 0]


no_treated_2 = len(dtb_wide_cem[dtb_wide_cem["cd"] == 1]["pre_cd_cit"])

#### melt it back

dtb_long = pd.wide_to_long(dtb_wide_cem, k_cols,i=["pat_publn_id","cd","publn_y","nclass0","pre_cd_cit"],j="year").reset_index()

#### treatments
number_treated = len(dtb_long["cd"][dtb_long["cd"] == 1]) # Bell
dtb_long["treatment"] = [int(x) for x in (dtb_long["year"] > 1955) ] # Post
dtb_long["tre_x_cd"] = dtb_long["treatment"]*dtb_long["cd"] # Post x Bell
dtb_long["c_0_old_conc"] =dtb_long["c_0_conc"] - dtb_long["c_0_ys_conc"]
dtb_long["c_0_old_LN"] =dtb_long["c_0_conc_LN"] - dtb_long["c_0_ys_conc_LN"]

dtb_long["conc_ln"]= dtb_long["c_0_conc_LN"] + dtb_long["c_4_conc_LN"]
dtb_long["conc"]= dtb_long["c_0_conc"] + dtb_long["c_4_conc"]

#### diff-in-diff
#the dependent variable has some missing values,
#the clustered error function doesnt handle it well
#so, we need to slice the df first
dtb_long_sub = dtb_long[~(dtb_long["cites_in_y"].isna())]
dtb_long_sub.reset_index(inplace=True,drop=True)


#store the models and statistics in arrays
dep_vars = ["cites_in_y", "c_4_total",  "c_4_ys", "c_0_total", "cites_in_y_w_Kog_xi_av", "c_0_ys" , "c_0_conc",  "c_0_conc_LN", "c_0_ys_conc", "c_0_ys_conc_LN", "c_0_old_conc", "c_0_old_LN"]
models_list = [sm.wls(formula=f"{y}~ cd + treatment + tre_x_cd",data=dtb_long_sub,weights=dtb_long_sub["cem_matched"]) for y in dep_vars]
cluster_list = [x.fit(method="pinv", cov_type="cluster",cov_kwds={"groups":dtb_long_sub["nclass0"]}) for x in models_list]


