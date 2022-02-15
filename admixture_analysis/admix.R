require(admixtools)
require(tidyverse)

inds <- read.delim("admix/reich/v44.3_1240K_public.ind")
inds$a <- inds[,1]%>% str_trim("both") %>% str_squish()

tx <- sapply(inds$a, function(x) (x %>% str_split(" ")))
inds$i_1 <- sapply(tx, function(x) (x %>% str_split(" "))[1])
inds$i_2 <- sapply(tx, function(x) (x %>% str_split(" "))[2])
inds$i_3 <- sapply(tx, function(x) (x %>% str_split(" "))[3])
inds_unique <- inds %>% group_by(i_3) %>% summarise(count=n())
inds_left <- inds %>% group_by(i_1) %>% summarise(count=n())
inds_s_ <- inds_left %>% filter(str_detect(i_1,"^(S_)"))

inds$suffix_s <- inds$i_1 %in% inds_s_$i_1
inds_unique$i_3 <- inds_unique$i_3 %>% as.character()
s_pops <- unlist(inds$i_3[inds$suffix_s] %>% unique())
s_pops <- data.frame(s_pops)
s_pops_ <- left_join(s_pops, inds_unique,by=c("s_pops"="i_3"))

#./plink --file efe22 --make-bed --out efe_new --noweb
#right= c("Russia_DevilsCave_N.SG","Israel_C","Iran_C_TepeHissar","Czech_EBA","Armenia_EBA","Morocco_Iberomaurusian",'Switzerland_Bichon.SG','Turkey_N','Georgia_Kotias.SG','Russia_HG_Karelia', 'Russia_Yana_UP.SG', 'Iran_GanjDareh_N', 'Russia_Kolyma_M.SG')
right= c("Mbuti.DG","Russia_DevilsCave_N.SG","Israel_C","Iran_C_TepeHissar","Han.SDG","Armenia_EBA","Morocco_Iberomaurusian",'Switzerland_Bichon.SG','Turkey_N','Georgia_Kotias.SG','Russia_HG_Karelia', 'Russia_Yana_UP.SG', 'Iran_GanjDareh_N', 'Russia_Kolyma_M.SG')
left = c("Bulgarian.DG","Adygei.SDG","Adygei.DG","Turkmen.SG","Armenian.DG","Greek_1.DG","Georgian.DG","Tatar_Volga.SG","Turkish.DG","Iranian.DG")

left_old = c("Bulgarian.DG","Adygei.DG","Tatar_Volga.SG")
right_old= c("Mbuti.DG","Russia_DevilsCave_N.SG","Israel_C","Armenia_EBA","Iran_C_TepeHissar","Czech_EBA","Han.SDG","Morocco_Iberomaurusian",'Switzerland_Bichon.SG','Turkey_N','Georgia_Kotias.SG','Russia_HG_Karelia', 'Russia_Yana_UP.SG', 'Iran_GanjDareh_N', 'Russia_Kolyma_M.SG')


right_old <-  c("Mbuti.DG","Russia_DevilsCave_N.SG","Israel_C","Iran_C_TepeHissar","Czech_EBA","Han.SDG","Armenia_EBA","Morocco_Iberomaurusian",'Switzerland_Bichon.SG','Turkey_N','Georgia_Kotias.SG','Russia_HG_Karelia', 'Russia_Yana_UP.SG', 'Iran_GanjDareh_N', 'Russia_Kolyma_M.SG')

#caucasusR <- c("Mbuti.DG", "Russia_Kostenki14.SG",  "Russia_Ust_Ishim.DG",  "Russia_MA1_HG.SG", "Han.SDG", "Papuan.DG", "Onge.DG","	
#Italy_North_Villabruna_HG", Vestonice16, ElMiron, Ethiopia_4500BP.SG, Karitiana.DG, Natufian, Iran_Ganj_Dareh_Neolithic)


admixtools::eigenstrat_to_plink("admix/reich/v44.3_1240K_public",outpref = "admix/plink/master_plink_2",pops=c(left_old,right_old))




shell("cd admix/plink & plink --bfile master_plink_2 --bmerge efe_gns.bed efe_gns.bim efe_gns.fam --make-bed --out merged_data --noweb",ignore.stderr = TRUE)
shell("cd admix/plink & plink --bfile merged_data --missing",ignore.stderr = TRUE)




qpab <- qpadm(data = "admix/plink/merged_data",left = left_old,right = right_old, target = "efe22_FAM")

extract_f2("admix/plink/merged_data",outdir = "admix/efe_1",overwrite = TRUE)

qpa <- qpadm(data = "admix/efe_1/",left = left_old,right = right_old, target = "efe22_FAM")
f2_a <- f2("admix/plink/merged_data",pop1 = "efe22",c(left_old,right_old,"efe22_FAM","gns_FAM"))

onlypops <- qpadm(data = "admix/plink/master_plink_2",left = left_old[c(3,1,11)],right = right_old, target = "Turkish.DG")
f2_holder <- f2_from_geno("admix/plink/merged_data")


qpaa <- qpadm(data = f2_holder,left = left_old,right = right_old, target = "efe22_FAM")

f2stats <- f2(f2_holder,pop1 = "India_RoopkundB_oNearEast",pop2 = c(left_old,right_old))



qpe <- qpadm(data = "admix/efe",left = left_old[c(3,1,9)],right = right_old, target = "efe22_FAM")


trk <- qpadm("admix/onlypops",left = left_old[c(3,10,9)],right = right_old,target = "Turkish.DG")

qpa <- qpadm(data = "admix/f2_2",left = left_old[c(3,1,8)],right = right_old, target = "efe22_FAM")
qpa$weights


f2_holder <- f2_from_precomp(dir = "admix/",pops = c(left_old,right_old,"efe22_FAM","gns_FAM"),afprod = T)

fst_ <- admixtools::fst(data = f2_holder,pop1 = 'efe22_FAM',pop2 =c(left_old,right_old,"efe22_FAM"))
f2_ <- f2_from_geno(pref = "admix/plink/merged_data")

f4_ <- f4blockdat_from_geno(pref = "admix/plink/merged_data",left = left_old,right = right_old)

f3_ <- admixtools::f3(data = f2_,pop1 = "efe22_FAM",pop2 = left,pop3=left)

qpa2 <- qpadm(data = "f2_no4",left = left[-c(2,5)],right = right, target = "efe22_FAM")
qpa2$weights


qpa3 <- qpadm(data = "f2_no4",left = left[-c(4,1)],right = right, target = "efe22_FAM")
qpa3$weights

qpa4 <- qpadm(data = "f2_no4",left = left[-c(4,5)],right = right, target = "efe22_FAM")
qpa4$weights

qpa$weights[,3:5] <- qpa$weights[,3:5] %>% round(digits = 3)
qpa$weights[,1] <- "me"

qpa3$weights[,3:5] <- qpa3$weights[,3:5] %>% round(digits = 3)
qpa3$weights[,1] <- "me"

stargazer::stargazer(qpa3$weights,type = 'text',summary = FALSE,digits = 1,digits.extra = 1)

f2_ <- admixtools::f2('admix/f2_no4',pop1 = 'efe22_FAM',c(left,right))
f2_$pop1 <- "me"
f2_[,3:4] <- f2_[,3:4] %>% round(digits = 5)
stargazer::stargazer(f2_[order(f2_$est),],type = 'text',summary = FALSE,digits = 1,digits.extra = 1)

gg2 <- find_graphs(f2_holder,outpop = "Switzerland_Bichon.SG")

stargazer::stargazer(qpa$weights %>% mutate_at(.vars = c("weight","se","z"),.funs = round,3) %>% mutate(target="my_geno"),type = 'text',summary = FALSE,digits = 1,digits.extra = 1)
stargazer::stargazer(qpa$rankdrop %>% round(3),type="text",summary = FALSE,digits = 1,digits.extra = 1)

stargazer::stargazer(trk$weights %>% mutate_at(.vars = c("weight","se","z"),.funs = round,3),type = 'text',summary = FALSE,digits = 1,digits.extra = 1)
stargazer::stargazer(trk$rankdrop %>% round(3),type="text",summary = FALSE,digits = 1,digits.extra = 1)
