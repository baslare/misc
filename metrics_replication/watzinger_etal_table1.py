# -*- coding: utf-8 -*-
"""
Created on Thu Feb 24 14:32:46 2022

@author: Efe
"""

import os
import pandas as pd
import pickle


os.getcwd()
dt = pd.read_stata("watzinger_etal/Kit_WNFS//data/mainAnalysis_AEJ.dta")
cm = pd.read_stata("watzinger_etal/Kit_WNFS//data/communication_sic_nclass0.dta")


dt = dt[~((dt["m"] == 0) & (dt["m1"] == 0))]


dt = dt[~(dt["publn_y"]<1940)]

dt["xi"].iloc[(dt["Kogan_xi"]>0) & (dt["xi"] == 0)] = None

chars = ["total_cites_in_y", "self_cites", "cites_in_y", "cites_in_y_New", "cites_in_y_Young", "cites_in_y_Old", "cites_in_y_size_10", "cites_in_y_Large"]
group_dict = {x: "sum" for x in chars}
group_dict_2 = {"pre_cd_cit":"mean","xi":"mean","Kogan_xi":"mean"}


                    
group_dict.update(group_dict_2)
group_by_vars = ["cd", "pat_publn_id","appln_y","publn_y","belllabs","western","teletype","att", "bellsystem"]

dt_grouped = dt.groupby(group_by_vars,as_index=False).agg(group_dict)
dt_grouped.drop_duplicates(subset=["pat_publn_id"],inplace=True)
dt_grouped["patentProtected"] = 17-(1956-dt_grouped["publn_y"]) # 1940 vs 1956


chars2 = ["appln_y","publn_y","patentProtected","total_cites_in_y","cites_in_y","self_cites","pre_cd_cit"]    
group_dict_summary = {x: "mean" for x in chars2}


stats1 = dt_grouped[dt_grouped["cd"] == 1].agg(group_dict_summary) #6685 (5)
stats1b = dt_grouped[dt_grouped["cd"] == 0].agg(group_dict_summary)  #172464 (1)


###

dt = dt[(dt["publn_y"]<1949)]

dt_grouped2 = dt.groupby(group_by_vars,as_index=False).agg(group_dict)
dt_grouped2.drop_duplicates(subset=["pat_publn_id"],inplace=True)
dt_grouped2["patentProtected"] = 17-(1956-dt_grouped2["publn_y"]) # 1940 vs 1956

stats2 = dt_grouped2[dt_grouped2["cd"] == 1].agg(group_dict_summary) #3371 (6)
stats2b = dt_grouped2[dt_grouped2["cd"] == 0].agg(group_dict_summary)  #75739 (2)


###

dt = dt.merge(cm,how="left",on="nclass0")
dt["impact"] = dt["impact_y"] > 15
group_by_vars.append("impact")

dt_grouped3 = dt.groupby(group_by_vars,as_index=False).agg(group_dict)
dt_grouped3.drop_duplicates(subset=["pat_publn_id"],inplace=True)
dt_grouped3["patentProtected"] = 17-(1956-dt_grouped3["publn_y"]) # 1940 vs 1956

stats3 = dt_grouped3[(dt_grouped3["cd"] == 1) & (dt_grouped3["impact"] == 0)].agg(group_dict_summary) #2650 (8)
stats4 = dt_grouped3[(dt_grouped3["cd"] == 1) & (dt_grouped3["impact"] == 1)].agg(group_dict_summary)  #1121 (7)
stats3b = dt_grouped3[(dt_grouped3["cd"] == 0) & (dt_grouped3["impact"] == 0)].agg(group_dict_summary) #70619 (4)
stats4b = dt_grouped3[(dt_grouped3["cd"] == 0) & (dt_grouped3["impact"] == 1)].agg(group_dict_summary)  #5120 (3) 

stats_df = pd.DataFrame({1:stats1b,
                         2:stats2b,
                         3:stats4b,
                         4:stats3b,
                         5:stats1,
                         6:stats2,
                         7:stats4,
                         8:stats3})

with open("table_1.pickle","wb") as p:
    pickle.dump(stats_df,p,protocol=pickle.HIGHEST_PROTOCOL)



#%%
