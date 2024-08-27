#Relatedness ADHD


library(data.table)
library(readxl)
library(dplyr)
library(janitor)
###Prepare Data:



Raw_Manifest_File <- Final_Manifest_EO_GSA_082223_1_ 
Raw_Manifest_File$iid<- paste(Raw_Manifest_File$`Sentrix Barcode`, Raw_Manifest_File$`Sentrix Position`, sep = "_")


# load relatedness data file
relatedness_stats = fread("king/king_output_autosomes.kin0")
relatedness_stats = janitor::clean_names(relatedness_stats)
relatedness_stats = relatedness_stats[,.(iid1,iid2,kinship)]


# load family structure data file (csv file with child, father, mother columns)


V2_Official_Olfson_2024_Commands <- read_excel("V2_Official_Olfson_2024_Commands.xlst", 
                                               sheet = "Complete_Trios_QC")
View(V2_Official_Olfson_2024_Commands)
trios = V2_Official_Olfson_2024_Commands
  
#trios = janitor::clean_names(trios)
trios <- data.table(trios)
trios[,fid := seq_along(iid)]

trios_long = rbindlist(list(trios[,.(iid,FID)],trios[,.(father,FID)],trios[,.(mother,FID)]),use.names = F)
setnames(trios_long,c("person","fam_id"))

child_list = unique(trios$iid)
child_label = rep("child",length(child_list))

parent_list = unique(c(trios$mother,trios$father))
parent_label = rep("parent",length(parent_list))

all_list = unique(c(trios$mother,trios$father,trios$iid))


#look only at relationships where both indv1 and indv2 are located in trios file:
relatedness_stats = relatedness_stats[is.element(iid1,all_list)]
relatedness_stats = relatedness_stats[is.element(iid2,all_list)]


# add the family identities
relatedness_stats = merge(relatedness_stats,trios_long,by.x = "iid1", by.y = "person", all.x = TRUE,allow.cartesian=TRUE)
setnames(relatedness_stats,"fam_id","fam_id_1")

relatedness_stats = merge(relatedness_stats,trios_long,by.x = "iid2", by.y = "person", all.x = TRUE,allow.cartesian=TRUE)
setnames(relatedness_stats,"fam_id","fam_id_2")

# add the type identities
relatedness_stats[,type_1 := factor(iid1, levels = c(child_list,parent_list), labels = c(child_label,parent_label))]
relatedness_stats[,type_2 := factor(iid2, levels = c(child_list,parent_list), labels = c(child_label,parent_label))]

df1 <- merge(relatedness_stats, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid1', by.y = 'iid', all.x = TRUE)
names(df1)[names(df1) == 'Sample ID'] <- 'sample_id_1'
df1  <- merge(df1, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid2', by.y = 'iid', all.x = TRUE)
names(df1)[names(df1) == 'Sample ID'] <- 'sample_id_2'


#pull all relationships from relatedness_stats with relatedness_phi>.18 in the following cases for indv1 and indv2:

# potential relationships between families, indv1 and indv2 not from same family from trio file and phi>0.18
inter_fam_rel= relatedness_stats[((fam_id_1 != fam_id_2) & pi_hat > 0.4)]
df1 <- merge(inter_fam_rel, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid1', by.y = 'iid', all.x = TRUE)
names(df1)[names(df1) == 'Sample ID'] <- 'sample_id_1'
df1  <- merge(df1, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid2', by.y = 'iid', all.x = TRUE)
names(df1)[names(df1) == 'Sample ID'] <- 'sample_id_2'
unique_ids_df1 <- data.frame(unique(c(df1$fam_id_1, df1$fam_id_2)))
colnames(unique_ids_df1)[1] <- "FID"


# mother-father pairs that look like siblings (phi >0.18)
cosang_rel= relatedness_stats[((fam_id_1 == fam_id_2) & (type_1 == type_2) & (iid1 != iid2) & pi_hat  > 0.4)]
cosang_rel <- cosang_rel[cosang_rel$type_1 == 'parent' & cosang_rel$type_2 == 'parent', ]
df2 <- merge(cosang_rel, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid1', by.y = 'iid', all.x = TRUE)
names(df2)[names(df2) == 'Sample ID'] <- 'sample_id_1'
df2 <- merge(df2, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid2', by.y = 'iid', all.x = TRUE)
names(df2)[names(df2) == 'Sample ID'] <- 'sample_id_2'

#	Merging 


# reported parent-child relationships that may not be real (<0.18) [proof against relationship]
non_pat_rel= relatedness_stats[((fam_id_1 == fam_id_2) & (type_1 != type_2) & pi_hat  < 0.4)]
df3 <- merge(non_pat_rel, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid1', by.y = 'iid', all.x = TRUE)
names(df3)[names(df3) == 'Sample ID'] <- 'sample_id_1'
df3  <- merge(df3, Raw_Manifest_File[, c('iid', 'Sample ID')], by.x = 'iid2', by.y = 'iid', all.x = TRUE)
names(df3)[names(df3) == 'Sample ID'] <- 'sample_id_2'
df3 <- unique(df3)
unique_ids_df3 <- data.frame(unique(c(df3$fam_id_1, df3$fam_id_2)))
colnames(unique_ids_df3 )[1] <- "FID"


#merge unique family ids 
complete_relatednes <- rbind(df1,df2,df3)
part1 <- data.frame(unique(complete_relatednes$fam_id_1))
colnames(part1)[1] <- "FID"

part2 <- data.frame(unique(complete_relatednes$fam_id_2))
colnames(part2 )[1] <- "FID"
failed_fids <- rbind(part1,part2)
failed_fids <-unique(failed_fids)


###OR MAYBE JUST RBIND THEM AND REMOVE NON UNIQUE ROWS?

write.csv(cosang_rel,"./cosang_rel.csv")
write.csv(non_pat_rel,"./non_pat_rel.csv")
write.csv(inter_fam_rel,"./inter_fam_rel.csv")

#### reported parent-child relationships which did not show with >0.18 in this data [no proof for relationship]

