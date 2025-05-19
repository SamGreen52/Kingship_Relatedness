# Kingship_Relatedness

Pathway that has these files: /gpfs/gibbs/project/olfson/srg52/Project_3_Emily/Target/Quality Control/Quality Control of Related and Sex/Family_Factors/Relatedness_Check/king

Files needed: 
-plink2 
-plink binary files (bed, bim and fam). In this case, "output_ocd". 
-Relatedness_Script
-relatedness_KING.R  (This assumes you have a pedigree file that has columns: Proband_id, Mom_id, and Father_ID. 

1. ./plink2 --bfile output_ocd --chr 1-22 --make-bed --out FINAL_OCD_autosomes
2. sbatch Relatedness_Script
3. Run script relatedness_KING.R  in rstudio. 
