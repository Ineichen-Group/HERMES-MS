###call libraries
library(metafor)
library(meta)
library(readxl)
library(weightr)
library(stringr)
library(ggplot2)
library(ggfortify)

###read file
MS <- read.csv("HERMES_INCLUDED.csv",sep=',')

### Clean and condense dataframe
selected_columns <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                      "Animal.model", "Species", "Strain", "Sex", "Age",
                      "Tested.drug.s.", "Comparator", "Outcome", "Total.number.of.animals", "Treatment.regimen",
                      "EAE.score.drug.1", "n.animals.drug.1", "Variance.measure.drug.1", "EAE.score.control", "n.animals.control", "Variability.measure.control", "Data.type...27",
                      "MRI.drug.1", "n.MRI.1", "Variability.measure...30", "MRI.control.1", "n.animals.1", "Variance.measure...33", "Data.type...34", "MRI.Outcome...35",
                      "Comment", "keywords", "address",
                      "Tested.drug.2", "Outcome.drug.2", "Treatment.regimen.drug.2...66", "EAE.score.drug.2...67", "n.animals.drug.2...68", "Variance.measure.drug.2...69", "EAE.score.control.2...70", "n.animals.control.2...71", "Variability.measure.control.2...72", "Data.type.2",
                      "Tested.drug.3", "Treatment.regimen.drug.2...76", "EAE.score.drug.2...77", "n.animals.drug.2...78", "Variance.measure.drug.2...79", "EAE.score.control.2...80", "n.animals.control.2...81", "Variability.measure.control.2...82", "Data.type.3",
                      "MRI.drug.2", "n.animals.MRI.2", "Variability.measure...87", "MRI.control.2", "n.animals.2", "Variance.measure...90", "Data.type...91", "MRI.Outcome...92",
                      "MRI.drug.3", "n.animals.MRI.3", "Variability.measure...96", "MRI.control.3", "n.animals.MRI.3", "Variance.measure...99", "Data.type...100", "MRI.Outcome...101", 
                      "MRI.drug.4", "n.animals.MRI.4", "Variability.measure...104", "MRI.control.4", "n.animals.4", "Variance.measure...107", "Data.type...108", "MRI.Outcome...109")

MS_master <- MS[selected_columns]# Subset the dataframe to keep only the selected columns

names(MS_master) <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                    "Animal.model", "Species", "Strain", "Sex", "Age",
                    "Drug1", "Comparator.drug1", "Outcome.drug1", "N.total.drug1", "Regimen.drug1",
                    "EAEscore.drug1", "N.animals.EAE.drug1", "Var.EAE.drug1", "EAEscore.ctrl1", "N.animals.EAE.ctrl1", "Var.EAE.ctrl1", "Datatype.EAE.drug1",
                    "MRI.drug1", "N.animals.MRI.drug1", "Var.MRI.drug1", "MRI.ctrl1", "N.animals.MRI.ctrl1", "Var.MRI.ctrl1", "Datatype.MRI.drug1", "MRI.outcome.drug1",
                    "Comment", "Keywords", "Address",
                    "EAE.drug2", "Outcome.drug2", "Regimen.drug2", "EAEscore.drug2", "N.animals.EAE.drug2", "Var.EAE.drug2", "EAEscore.ctrl2", "N.animals.EAE.ctrl2", "Var.EAE.ctrl2", "Datatype.EAE.drug2",
                    "EAE.drug3", "Regimen.drug3", "EAEscore.drug3", "N.animals.EAE.drug3", "Var.EAE.drug3", "EAEscore.ctrl3", "N.animals.EAE.ctrl3", "Var.EAE.ctrl3", "Datatype.EAE.drug3",
                    "MRI.drug2", "N.animals.MRI.drug2", "Var.MRI.drug2", "MRI.ctrl2", "N.animals.MRI.ctrl2", "Var.MRI.ctrl2", "Datatype.MRI.drug2", "MRI.outcome.drug2",
                    "MRI.drug3", "N.animals.MRI.drug3", "Var.MRI.drug3", "MRI.ctrl3", "N.animals.MRI.ctrl3", "Var.MRI.ctrl3", "Datatype.MRI.drug3", "MRI.outcome.drug3", 
                    "MRI.drug4", "N.animals.MRI.drug4", "Var.MRI.drug4", "MRI.ctrl4", "N.animals.MRI.ctrl4", "Var.MRI.ctrl4", "Datatype.MRI.drug4", "MRI.outcome.drug4")
MS_master$Provenance <- "D1.EAE"#add variable to distinguish different data provenance

### Complement dataframe with EAE.drugs 2
selected_columns_EAEdrug2 <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                               "Animal.model", "Species", "Strain", "Sex", "Age",
                               "Comment", "Keywords", "Address",
                               "EAE.drug2", "Outcome.drug2", "Regimen.drug2", "EAEscore.drug2", "N.animals.EAE.drug2", "Var.EAE.drug2", "EAEscore.ctrl2", "N.animals.EAE.ctrl2", "Var.EAE.ctrl2", "Datatype.EAE.drug2")

MS_EAEdrug2 <- MS_master[selected_columns_EAEdrug2]
MS_EAEdrug2 <- MS_EAEdrug2[!is.na(MS_EAEdrug2$EAE.drug2), ]#remove NAs
names(MS_EAEdrug2) <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                        "Animal.model", "Species", "Strain", "Sex", "Age",
                        "Comment", "Keywords", "Address",
                        "Drug1", "Outcome.drug1", "Regimen.drug1", "EAEscore.drug1", "N.animals.EAE.drug1", "Var.EAE.drug1", "EAEscore.ctrl1", "N.animals.EAE.ctrl1", "Var.EAE.ctrl1", "Datatype.EAE.drug1")
MS_EAEdrug2$Provenance <- "D2.EAE"

missing_cols <- setdiff(colnames(MS_master), colnames(MS_EAEdrug2))

# Add missing columns with NA values to MS_EAEdrug2
MS_EAEdrug2 <- cbind(MS_EAEdrug2, setNames(data.frame(matrix(NA, nrow = nrow(MS_EAEdrug2), ncol = length(missing_cols))), missing_cols))

# Reorder columns in MS_EAEdrug2 to match the order in MS_master
MS_EAEdrug2 <- MS_EAEdrug2[, colnames(MS_master)]
same_column_order <- identical(colnames(MS_EAEdrug2), colnames(MS_master))#Check if similar column order
same_column_order

# Stack MS_EAEdrug2 on top of MS_master
MS_master <- rbind(MS_EAEdrug2, MS_master)

### Complement dataframe with EAE.drugs3
selected_columns_EAEdrug3 <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                               "Animal.model", "Species", "Strain", "Sex", "Age",
                               "Comment", "Keywords", "Address",
                               "EAE.drug3", "Regimen.drug3", "EAEscore.drug3", "N.animals.EAE.drug3", "Var.EAE.drug3", "EAEscore.ctrl3", "N.animals.EAE.ctrl3", "Var.EAE.ctrl3", "Datatype.EAE.drug3")
MS_EAEdrug3 <- MS_master[selected_columns_EAEdrug3]
MS_EAEdrug3 <- MS_EAEdrug3[!is.na(MS_EAEdrug3$EAE.drug3), ]#remove NAs

names(MS_EAEdrug3) <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                        "Animal.model", "Species", "Strain", "Sex", "Age",
                        "Comment", "Keywords", "Address",
                        "Drug1", "Regimen.drug1", "EAEscore.drug1", "N.animals.EAE.drug1", "Var.EAE.drug1", "EAEscore.ctrl1", "N.animals.EAE.ctrl1", "Var.EAE.ctrl1", "Datatype.EAE.drug1")
MS_EAEdrug3$Provenance <- "D3.EAE"

missing_cols <- setdiff(colnames(MS_master), colnames(MS_EAEdrug3))

# Add missing columns with NA values to MS_EAEdrug3
MS_EAEdrug3 <- cbind(MS_EAEdrug3, setNames(data.frame(matrix(NA, nrow = nrow(MS_EAEdrug3), ncol = length(missing_cols))), missing_cols))

# Reorder columns in MS_EAEdrug3 to match the order in MS_master
MS_EAEdrug3 <- MS_EAEdrug3[, colnames(MS_master)]
same_column_order <- identical(colnames(MS_EAEdrug3), colnames(MS_master))#Check if similar column order
same_column_order

# Stack MS_EAEdrug3 on top of MS_master
MS_master <- rbind(MS_EAEdrug3, MS_master)

#Put provenance column at first position
Provenance <- MS_master[, ncol(MS_master)]
MS_master <- MS_master[, -ncol(MS_master)]
MS_master <- cbind(Provenance, MS_master)

## Replace UTF-8 characters
MS_master$Author <- gsub("<c3><a1>", "a", MS_master$Author)
MS_master$Author <- gsub("<c3><a7>", "c", MS_master$Author)
MS_master$Author <- gsub("<c3><a9>", "e", MS_master$Author)
MS_master$Author <- gsub("<c3><b6>", "o", MS_master$Author)
MS_master$Author <- gsub("<c3><b4>", "o", MS_master$Author)
MS_master$Author <- gsub("<c4><87>", "c", MS_master$Author)
MS_master$Author <- gsub("<c4><8d>", "c", MS_master$Author)
MS_master$Author <- gsub("<c3><a4>", "a", MS_master$Author)
MS_master$Author <- gsub("<c3><bc>", "u", MS_master$Author)
MS_master$Author <- gsub("<c3><a1>", "a", MS_master$Author)
MS_master$Author <- gsub("<c4><8d>", "c", MS_master$Author)
MS_master$Author <- gsub("<c3><a8>", "e", MS_master$Author)
MS_master$Author <- gsub("<c3><ae>", "i", MS_master$Author)
MS_master$Author <- gsub("<c3><a2>", "a", MS_master$Author)
MS_master$Author <- gsub("<c3><a6>", "ae", MS_master$Author)
MS_master$Author <- gsub("<c3><b1>", "n", MS_master$Author)
MS_master$Author <- gsub("<c4><93>", "e", MS_master$Author)
MS_master$Author <- gsub("<c3><b3>", "o", MS_master$Author)
MS_master$Author <- gsub("<c3><ad>", "i", MS_master$Author)
MS_master$Author <- gsub("<c3><b8>", "o", MS_master$Author)
MS_master$Author <- gsub("<c5><82>", "l", MS_master$Author)
MS_master$Author <- gsub("<c3><81>", "A", MS_master$Author)
MS_master$Author <- gsub("<c3><9c>", "U", MS_master$Author)
MS_master$Author <- gsub("<c4><85>", "a", MS_master$Author)
MS_master$Author <- gsub("<c3><ba>", "u", MS_master$Author)
MS_master$Author <- gsub("<c3><98>", "o", MS_master$Author)
MS_master$Author <- gsub("<e2><80><99>", "", MS_master$Author)#would be a right single quotation mark
MS_master$Author <- gsub("<c5><84>", "n", MS_master$Author)

MS_master$Journal <- gsub("Proceedings of the National Academy of Sciences of the United States of America", "PNAS", MS_master$Journal)
MS_master$Journal <- gsub("Proc Natl Acad Sci U S A", "PNAS", MS_master$Journal)
MS_master$Journal <- gsub("Frontiers in Behavioral Neuroscience", "Front Beh Neurosci", MS_master$Journal)
MS_master$Journal <- gsub("Neuroscience and Behavioral Physiology", "Neurosci Beh Phy", MS_master$Journal)
MS_master$Journal <- gsub("Brazilian Journal of Pharmaceutical Sciences", "Braz J Pharm", MS_master$Journal)
MS_master$Journal <- gsub("Archives of Biochemistry and Biophysics", "Arch Biochem", MS_master$Journal)
MS_master$Journal <- gsub("Scandinavian Journal of Immunology", "Scand Immunol", MS_master$Journal)
MS_master$Journal <- gsub("Activitas Nervosa Superior Rediviva", "Act Nerv Sup Red", MS_master$Journal)
MS_master$Journal <- gsub("CNS and Neurological Disorders - Drug Targets", "CNS Neurol Dis", MS_master$Journal)
MS_master$Journal <- gsub("Biochemical and Biophysical Research Communications", "Biochem Biophys Res", MS_master$Journal)
MS_master$Journal <- gsub("European Review for Medical and Pharmacological Sciences", "Eur Rev Med Pharm", MS_master$Journal)
MS_master$Journal <- gsub("Clinical and Experimental Neuroimmunology", "Clin Exp Neuroimmun", MS_master$Journal)
MS_master$Journal <- gsub("Journal of Neuropathology and Experimental Neurology", "J Neuropath Exp Neurol", MS_master$Journal)
MS_master$Journal <- gsub("Arch Immunol Ther Exp (Warsz)", "Arch Immunol Ther", MS_master$Journal)
MS_master$Journal <- gsub("Brazilian Journal of Medical and Biological Research", "Braz J Biol", MS_master$Journal)
MS_master$Journal <- gsub("International Journal of Molecular Sciences", "Int J Mol Sci", MS_master$Journal)
MS_master$Journal <- gsub("International Journal of Pharmaceutical Sciences Review and Research", "Int J Pharm", MS_master$Journal)
MS_master$Journal <- gsub("Journal of Neuroscience Research", "J Neurosci Res", MS_master$Journal)
MS_master$Journal <- gsub("Iranian Journal of Basic Medical Sciences", "Ir J Med Biol", MS_master$Journal)
MS_master$Journal <- gsub("Indian Journal of Pharmaceutical Sciences", "Ind J Pharm", MS_master$Journal)
MS_master$Journal <- gsub("International Journal of Biological Sciences", "Int J Biol Sci", MS_master$Journal)
MS_master$Journal <- gsub("Neurologia (Engl Ed)", "Neurologia", MS_master$Journal)


MS_master$first_author <- str_extract(MS_master$Author, regex("^(.+?),", ignore_case = T))
MS_master$first_author <- substring(MS_master$first_author, 1, nchar(MS_master$first_author)-1)
MS_master$Study.year.journal <- paste(MS_master$first_author, MS_master$Year, MS_master$Journal, sep = ", ")

#####################
### Add MRI data ####
#####################

### Complement dataframe with MRI.drugs 2
selected_columns_MRIdrug2 <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                               "Animal.model", "Species", "Strain", "Sex", "Age",
                               "Comment", "Keywords", "Address",
                               "Drug1", "MRI.drug2", "N.animals.MRI.drug2", "Var.MRI.drug2", "MRI.ctrl2", "N.animals.MRI.ctrl2", "Var.MRI.ctrl2", "Datatype.MRI.drug2", "MRI.outcome.drug2")

MS_MRIdrug2 <- MS_master[selected_columns_MRIdrug2]
MS_MRIdrug2 <- MS_MRIdrug2[!is.na(MS_MRIdrug2$MRI.drug2), ]#remove NAs
names(MS_MRIdrug2) <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                        "Animal.model", "Species", "Strain", "Sex", "Age",
                        "Comment", "Keywords", "Address",
                        "Drug1", "MRI.drug1", "N.animals.MRI.drug1", "Var.MRI.drug1", "MRI.ctrl1", "N.animals.MRI.ctrl1", "Var.MRI.ctrl1", "Datatype.MRI.drug1", "MRI.outcome.drug1")

MS_MRIdrug2$Provenance <- "MRI2"

missing_cols <- setdiff(colnames(MS_master), colnames(MS_MRIdrug2))

# Add missing columns with NA values to MS_MRIdrug2
MS_MRIdrug2 <- cbind(MS_MRIdrug2, setNames(data.frame(matrix(NA, nrow = nrow(MS_MRIdrug2), ncol = length(missing_cols))), missing_cols))

# Reorder columns in MS_MRIdrug2 to match the order in MS_master
MS_MRIdrug2 <- MS_MRIdrug2[, colnames(MS_master)]
same_column_order <- identical(colnames(MS_MRIdrug2), colnames(MS_master))#Check if similar column order
same_column_order

# Stack MS_MRIdrug2 on top of MS_master
MS_master <- rbind(MS_MRIdrug2, MS_master)




### Complement dataframe with MRI.drugs 3
selected_columns_MRIdrug3 <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                               "Animal.model", "Species", "Strain", "Sex", "Age",
                               "Comment", "Keywords", "Address",
                               "Drug1", "MRI.drug3", "N.animals.MRI.drug3", "Var.MRI.drug3", "MRI.ctrl3", "N.animals.MRI.ctrl3", "Var.MRI.ctrl3", "Datatype.MRI.drug3", "MRI.outcome.drug3")

MS_MRIdrug3 <- MS_master[selected_columns_MRIdrug3]
MS_MRIdrug3 <- MS_MRIdrug3[!is.na(MS_MRIdrug3$MRI.drug3), ]#remove NAs
names(MS_MRIdrug3) <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                        "Animal.model", "Species", "Strain", "Sex", "Age",
                        "Comment", "Keywords", "Address",
                        "Drug1", "MRI.drug1", "N.animals.MRI.drug1", "Var.MRI.drug1", "MRI.ctrl1", "N.animals.MRI.ctrl1", "Var.MRI.ctrl1", "Datatype.MRI.drug1", "MRI.outcome.drug1")

MS_MRIdrug3$Provenance <- "MRI3"

missing_cols <- setdiff(colnames(MS_master), colnames(MS_MRIdrug3))

# Add missing columns with NA values to MS_MRIdrug3
MS_MRIdrug3 <- cbind(MS_MRIdrug3, setNames(data.frame(matrix(NA, nrow = nrow(MS_MRIdrug3), ncol = length(missing_cols))), missing_cols))

# Reorder columns in MS_MRIdrug3 to match the order in MS_master
MS_MRIdrug3 <- MS_MRIdrug3[, colnames(MS_master)]
same_column_order <- identical(colnames(MS_MRIdrug3), colnames(MS_master))#Check if similar column order
same_column_order

# Stack MS_MRIdrug4 on top of MS_master
MS_master <- rbind(MS_MRIdrug3, MS_master)




### Complement dataframe with MRI.drugs 4
selected_columns_MRIdrug4 <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                               "Animal.model", "Species", "Strain", "Sex", "Age",
                               "Comment", "Keywords", "Address",
                               "Drug1", "MRI.drug4", "N.animals.MRI.drug4", "Var.MRI.drug4", "MRI.ctrl4", "N.animals.MRI.ctrl4", "Var.MRI.ctrl4", "Datatype.MRI.drug4", "MRI.outcome.drug4")

MS_MRIdrug4 <- MS_master[selected_columns_MRIdrug4]
MS_MRIdrug4 <- MS_MRIdrug4[!is.na(MS_MRIdrug4$MRI.drug4), ]#remove NAs
names(MS_MRIdrug4) <- c("Author", "Year", "Journal", "Title", "Abstract", "DOI",
                        "Animal.model", "Species", "Strain", "Sex", "Age",
                        "Comment", "Keywords", "Address",
                        "Drug1", "MRI.drug1", "N.animals.MRI.drug1", "Var.MRI.drug1", "MRI.ctrl1", "N.animals.MRI.ctrl1", "Var.MRI.ctrl1", "Datatype.MRI.drug1", "MRI.outcome.drug1")

MS_MRIdrug4$Provenance <- "MRI4"

missing_cols <- setdiff(colnames(MS_master), colnames(MS_MRIdrug4))

# Add missing columns with NA values to MS_MRIdrug4
MS_MRIdrug4 <- cbind(MS_MRIdrug4, setNames(data.frame(matrix(NA, nrow = nrow(MS_MRIdrug4), ncol = length(missing_cols))), missing_cols))

# Reorder columns in MS_MRIdrug4 to match the order in MS_master
MS_MRIdrug4 <- MS_MRIdrug4[, colnames(MS_master)]
same_column_order <- identical(colnames(MS_MRIdrug4), colnames(MS_master))#Check if similar column order
same_column_order

# Stack MS_MRIdrug4 on top of MS_master
MS_master <- rbind(MS_MRIdrug4, MS_master)###FINAL MS_Master file with all EAE and MRI data as "Drug1"
MS_master$Species <- ifelse(is.na(MS_master$Species), "mouse", MS_master$Species)#replace one instance of NA species with mouse

###########################
##### Meta-analysis ######
##########################

## Remove observations without EAE score variance measure
MS_master_ma <- MS_master[MS_master$Var.EAE.drug1 != "NR", ]
MS_master_ma <- MS_master_ma[MS_master_ma$Var.EAE.ctrl1 != "NR", ]
MS_master_ma <- MS_master_ma[MS_master_ma$N.animals.EAE.drug1 != "NR", ]
MS_master_ma <- MS_master_ma[MS_master_ma$N.animals.EAE.ctrl1 != "NR", ]
MS_master_ma <- MS_master_ma[MS_master_ma$N.animals.EAE.ctrl1 != "NULL", ]
MS_master_ma <- MS_master_ma[MS_master_ma$Datatype.EAE.drug1 != "NR", ]
MS_master_ma <- MS_master_ma[!is.na(MS_master_ma$Datatype.EAE.drug1), ]
MS_master_ma$Datatype.EAE.drug1[MS_master_ma$Datatype.EAE.drug1 == "mean" & !grepl(", SEM", MS_master_ma$Datatype.EAE.drug1)] <- "mean, SEM"

#Remove all drugs with <3 EAE scores
sort(table(MS_master_ma$Drug1), decreasing = T)

MS_master_ma <- MS_master_ma[MS_master_ma$Drug1 %in% names(table(MS_master_ma$Drug1))[table(MS_master_ma$Drug1) >= 3], ]

sort(table(MS_master_ma$Drug1), decreasing = T)

# Convert SEM to SD
sort(table(MS_master_ma$Datatype.EAE.drug1), decreasing = T)
MS_master_ma$EAEscore.drug1
MS_master_ma$EAEscore.drug1 <- as.numeric(MS_master_ma$EAEscore.drug1)
MS_master_ma$N.animals.EAE.drug1 <- as.numeric(MS_master_ma$N.animals.EAE.drug1)
MS_master_ma$Var.EAE.drug1 <- as.numeric(MS_master_ma$Var.EAE.drug1)
MS_master_ma$EAEscore.ctrl1 <- as.numeric(MS_master_ma$EAEscore.ctrl1)
MS_master_ma$N.animals.EAE.ctrl1 <- as.numeric(MS_master_ma$N.animals.EAE.ctrl1)
MS_master_ma$Var.EAE.ctrl1 <- as.numeric(MS_master_ma$Var.EAE.ctrl1)

condition <- MS_master_ma$Datatype.EAE.drug1 == "mean, SEM"  # Define the condition for applying the formulas

# Apply the formulas to the selected rows
MS_master_ma$Var.EAE.drug1 <- ifelse(condition, MS_master_ma$Var.EAE.drug1 / sqrt(MS_master_ma$N.animals.EAE.drug1), MS_master_ma$Var.EAE.drug1)
MS_master_ma$Var.EAE.ctrl1 <- ifelse(condition, MS_master_ma$Var.EAE.ctrl1 / sqrt(MS_master_ma$N.animals.EAE.ctrl1), MS_master_ma$Var.EAE.ctrl1)
MS_master_ma$Var.EAE.drug1 <- ifelse(MS_master_ma$Var.EAE.drug1 == 0, MS_master_ma$Var.EAE.drug1 + 0.001, MS_master_ma$Var.EAE.drug1)
MS_master_ma$Var.EAE.ctrl1 <- ifelse(MS_master_ma$Var.EAE.ctrl1 == 0, MS_master_ma$Var.EAE.ctrl1 + 0.001, MS_master_ma$Var.EAE.ctrl1)

MS_master_ma$D_cohen <- (MS_master_ma$EAEscore.drug1 - MS_master_ma$EAEscore.ctrl1)/(((MS_master_ma$Var.EAE.drug1^2)+(MS_master_ma$Var.EAE.ctrl1^2))/2)

## Split dataframe
# Split the dataframe into separate dataframes based on the "Drug1" column
drug_dfs <- split(MS_master_ma, MS_master_ma$Drug1)

Fingolimod_ma <- drug_dfs$Fingolimod
Interferon_ma <- drug_dfs$'Interferon Beta 1'
D3_ma <- drug_dfs$'Vitamin D3'
GA_ma <- drug_dfs$'Glatiramer acetate'
DMF_ma <- drug_dfs$'Dimethyl fumarate'
Minocycline_ma <- drug_dfs$Minocycline
Alemtuzumab_ma <- drug_dfs$Alemtuzumab
Atorvastatin_ma <- drug_dfs$Atorvastatin
Rituximab_ma <- drug_dfs$Rituximab
Estriol_ma <- drug_dfs$Estriol
Natalizumab_ma <- drug_dfs$Natalizumab
EGCG_ma <- drug_dfs$'Epigallocatechin gallate'
Siponimod_ma <- drug_dfs$Siponimod
Teriflunomide_ma <- drug_dfs$Teriflunomide
Abatacept_ma <- drug_dfs$Abatacept



#####################
### Fingolimod    ###
#####################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Fingolimod_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Fingolimod_ma <- merge(ordered_df, Fingolimod_ma, by = "Study.year.journal")
Fingolimod_ma.ordered <- Fingolimod_ma[order(Fingolimod_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Fingolimod_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Fingolimod_forest.png", width = 3200, height = 4000, res = 300)
forest(pes.forest.ordered,
       xlim = c(-300,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Fingolimod_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#####################
## Interferon Beta ##
#####################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Interferon_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Interferon_ma <- merge(ordered_df, Interferon_ma, by = "Study.year.journal")
Interferon_ma.ordered <- Interferon_ma[order(Interferon_ma$effect_sizes), ]
Interferon_ma.ordered <- subset(Interferon_ma.ordered, Interferon_ma.ordered$effect_sizes > -50)
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Interferon_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Interferon_forest_noRamos.png", width = 3200, height = 2800, res = 300)
forest(pes.forest.ordered,
       xlim = c(-50,5), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Interferon_TF_noRamos.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#####################
### Vitamin D3    ###
#####################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = D3_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
D3_ma <- merge(ordered_df, D3_ma, by = "Study.year.journal")
D3_ma <- D3_ma[!duplicated(D3_ma$effect_sizes), ]#deduplicate
D3_ma.ordered <- D3_ma[order(D3_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = D3_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("D3_forest.png", width = 3200, height = 2900, res = 300)
forest(pes.forest.ordered,
       xlim = c(-105,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("D3_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#############################
### Glatiramer acetate    ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = GA_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
GA_ma <- merge(ordered_df, GA_ma, by = "Study.year.journal")
GA_ma.ordered <- GA_ma[order(GA_ma$effect_sizes), ]
GA_ma.ordered <- subset(GA_ma.ordered, GA_ma.ordered$effect_sizes > -100)
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = GA_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("GA_forest_noRamos.png", width = 3200, height = 2200, res = 300)
forest(pes.forest.ordered,
       xlim = c(-105,5), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("GA_TF_noRamos.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#############################
### Dimethyl Fumarate     ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = DMF_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
DMF_ma <- merge(ordered_df, DMF_ma, by = "Study.year.journal")
DMF_ma.ordered <- DMF_ma[order(DMF_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = DMF_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("DMF_forest.png", width = 3200, height = 2000, res = 300)
forest(pes.forest.ordered,
       xlim = c(-105,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("DMF_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#############################
###     Minocycline       ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Minocycline_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Minocycline_ma <- merge(ordered_df, Minocycline_ma, by = "Study.year.journal")
Minocycline_ma.ordered <- Minocycline_ma[order(Minocycline_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Minocycline_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Minocycline_forest.png", width = 3200, height = 1700, res = 300)
forest(pes.forest.ordered,
       xlim = c(-30,20), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Minocycline_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#############################
###     Alemtuzumab       ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Alemtuzumab_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Alemtuzumab_ma <- merge(ordered_df, Alemtuzumab_ma, by = "Study.year.journal")
Alemtuzumab_ma.ordered <- Alemtuzumab_ma[order(Alemtuzumab_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Alemtuzumab_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Alemtuzumab_forest.png", width = 3200, height = 1300, res = 300)
forest(pes.forest.ordered,
       xlim = c(-60,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Alemtuzumab_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)


#############################
###     Atorvastatin      ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Atorvastatin_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Atorvastatin_ma <- merge(ordered_df, Atorvastatin_ma, by = "Study.year.journal")
Atorvastatin_ma.ordered <- Atorvastatin_ma[order(Atorvastatin_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Atorvastatin_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Atorvastatin_forest.png", width = 3200, height = 1200, res = 300)
forest(pes.forest.ordered,
       xlim = c(-40,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Atorvastatin_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)



#############################
###     Rituximab         ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Rituximab_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Rituximab_ma <- merge(ordered_df, Rituximab_ma, by = "Study.year.journal")
Rituximab_ma.ordered <- Rituximab_ma[order(Rituximab_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Rituximab_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Rituximab_forest.png", width = 3200, height = 1200, res = 300)
forest(pes.forest.ordered,
       xlim = c(-45,25), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Rituximab_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)



#############################
###       Estriol         ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Estriol_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Estriol_ma <- merge(ordered_df, Estriol_ma, by = "Study.year.journal")
Estriol_ma.ordered <- Estriol_ma[order(Estriol_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Estriol_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Estriol_forest.png", width = 3200, height = 1200, res = 300)
forest(pes.forest.ordered,
       xlim = c(-160,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Estriol_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)


#############################
###     Natalizumab      ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Natalizumab_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Natalizumab_ma <- merge(ordered_df, Natalizumab_ma, by = "Study.year.journal")
Natalizumab_ma.ordered <- Natalizumab_ma[order(Natalizumab_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Natalizumab_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Natalizumab_forest.png", width = 3200, height = 1200, res = 300)
forest(pes.forest.ordered,
       xlim = c(-60,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Natalizumab_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#############################
###     EGCG              ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = EGCG_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
EGCG_ma <- merge(ordered_df, EGCG_ma, by = "Study.year.journal")
EGCG_ma.ordered <- EGCG_ma[order(EGCG_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = EGCG_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("EGCG_forest.png", width = 3200, height = 1000, res = 300)
forest(pes.forest.ordered,
       xlim = c(-60,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("EGCG_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#############################
###     Siponimod         ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Siponimod_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Siponimod_ma <- merge(ordered_df, Siponimod_ma, by = "Study.year.journal")
Siponimod_ma.ordered <- Siponimod_ma[order(Siponimod_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Siponimod_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Siponimod_forest.png", width = 3200, height = 1000, res = 300)
forest(pes.forest.ordered,
       xlim = c(-100,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Siponimod_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)


#############################
###     Teriflunomide     ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Teriflunomide_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Teriflunomide_ma <- merge(ordered_df, Teriflunomide_ma, by = "Study.year.journal")
Teriflunomide_ma.ordered <- Teriflunomide_ma[order(Teriflunomide_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Teriflunomide_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Teriflunomide_forest.png", width = 3200, height = 1000, res = 300)
forest(pes.forest.ordered,
       xlim = c(-100,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Teriflunomide_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)


#############################
###     Abatacept         ###
#############################
pes.forest <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Abatacept_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Abatacept_ma <- merge(ordered_df, Abatacept_ma, by = "Study.year.journal")
Abatacept_ma.ordered <- Abatacept_ma[order(Abatacept_ma$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                               Study.year.journal, data = Abatacept_ma.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Abatacept_forest.png", width = 3200, height = 900, res = 300)
forest(pes.forest.ordered,
       xlim = c(-100,0), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Abatacept_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)


#Summary forest plot
Forest <- data.frame(study=c('Teriflunomide', 'Alemtuzumab', 'Glatiramer acetate', 'Fingolimod', 'Natalizumab',
                             'Siponimod', 'Dimethyl fumarate', 'Interferon beta', 'Rituximab',
                             'Pooled approved', 
                             'Estriol', 'Epigallocatechin gallate', 'Vitamin D3', 'Abatacept', 'Atorvastatin', 'Minocycline',
                             'Pooled failed'),
                          index=1:17,
                          result=c(-26.1, -19.5, -18.3, -15.4, -14.7, -14.5, -12.6, -9.9, -6.6,
                                   -13.9,
                                   -31.4, -15.8, -14.1, -13.5, -11.8, -9.2,
                                   -13.6),
                          error_lower=c(-37.3, -25.7, -21.9, -17.4, -20.0, -20.4, -15.7, -11.8, -12.6,
                                        -15.0,
                                        -45.3, -22.2, -16.0, -21.3, -15.4, -12.0,
                                        -15.0),
                          error_upper=c(-14.8, -13.4, -14.8, -13.4, -9.4, -8.5, -9.6, -8.0, -0.5,
                                        -12.8,
                                        -17.5, -9.3, -12.1, -5.8, -8.1, -6.4,
                                        -12.3))
##Pooled measures: approved -13.8866 [-14.9687; -12.8045], k = 176, n = 3871; failed -13.6397 [-15.0270; -12.2524], k = 78, n = 1727

#create forest plot
# Reorder index and study variables
Forest$index <- factor(Forest$index, levels = rev(Forest$index))
Forest$study <- factor(Forest$study, levels = Forest$study[order(Forest$index)])

# Convert index to numeric
Forest$index <- as.numeric(as.character(Forest$index))

# Create fill color variable
Forest$color <- ifelse(Forest$index <= 9, "blue", "red")

Forest$color <- ifelse(Forest$index <= 9, "blue",
                       ifelse(Forest$index == 10 | Forest$index == 17, "black", "red"))

# Create forest plot
png("Umbrella_forestribi.png", width = 2000, height = 1750, res = 300)
ggplot(data = Forest, aes(y = index, x = result)) +
  geom_errorbarh(aes(xmin = error_lower, xmax = error_upper, color = color), 
                 height = 0.1, lwd = 0.5) +
  geom_point(aes(color = color, fill = color), pch = 18, size = 3.5) +
  scale_y_reverse(breaks = Forest$index, labels = Forest$study) +
  scale_color_manual(values = c("black", "#6896ca", "#ff7b74")) +
  scale_fill_manual(values = c("black", "#6896ca", "#ff7b74")) +
  labs(title = "", y = "", x = "Hedges' g standardized mean difference") +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha = 1) +
  guides(color = "none", fill = "none") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(face = c("plain", "plain", "plain", "plain",
                                            "plain", "plain", "plain", "plain",
                                            "plain", "bold", "plain", "plain",
                                            "plain", "plain", "plain", "plain",
                                            "bold"), size = c(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12)))
dev.off()
#Warning message:
#Vectorized input to `element_text()` is not officially supported.
#ℹ Results may be unexpected or may change in future versions of ggplot2. Comment: That is fine!


### Umbrella meta-analysis
Approved_ma <- rbind(Teriflunomide_ma, Alemtuzumab_ma, GA_ma, Fingolimod_ma, Natalizumab_ma, Siponimod_ma, DMF_ma, Interferon_ma, Rituximab_ma)
Failed_ma <- rbind(Estriol_ma, EGCG_ma, D3_ma, Abatacept_ma, Atorvastatin_ma, Minocycline_ma)

pes.forest.approved <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Approved_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

pes.forest.failed <- metacont(N.animals.EAE.drug1, EAEscore.drug1, Var.EAE.drug1, N.animals.EAE.ctrl1, EAEscore.ctrl1, Var.EAE.ctrl1,
                       Study.year.journal, data = Failed_ma, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")



############################
#### Meta-analysis MRI ####
############################
## Remove observations without MRI score variance measure
MS_master_ma_mri <- MS_master[!is.na(MS_master$MRI.drug1), ]
MS_master_ma_mri <- MS_master_ma_mri[MS_master_ma_mri$MRI.drug1 != "NR", ]
MS_master_ma_mri <- MS_master_ma_mri[MS_master_ma_mri$Datatype.MRI.drug1 != "NR", ]
MS_master_ma_mri <- MS_master_ma_mri[MS_master_ma_mri$Var.MRI.drug1 != "NR", ]
MS_master_ma_mri$Datatype.MRI.drug1[MS_master_ma_mri$Datatype.MRI.drug1 == "mean" & !grepl(", SEM", MS_master_ma_mri$Datatype.MRI.drug1)] <- "mean, SEM"

sort(table(MS_master_ma_mri$Drug1), decreasing = T)

MS_master_ma_mri <- MS_master_ma_mri[MS_master_ma_mri$Drug1 %in% names(table(MS_master_ma_mri$Drug1))[table(MS_master_ma_mri$Drug1) >= 3], ]

sort(table(MS_master_ma_mri$Drug1), decreasing = T)

# Convert SEM to SD
sort(table(MS_master_ma_mri$Datatype.MRI.drug1), decreasing = T)
MS_master_ma_mri$MRI.drug1
MS_master_ma_mri$MRI.drug1 <- as.numeric(MS_master_ma_mri$MRI.drug1)
MS_master_ma_mri$N.animals.MRI.drug1 <- as.numeric(MS_master_ma_mri$N.animals.MRI.drug1)
MS_master_ma_mri$Var.MRI.drug1 <- as.numeric(MS_master_ma_mri$Var.MRI.drug1)
MS_master_ma_mri$MRI.ctrl1 <- as.numeric(MS_master_ma_mri$MRI.ctrl1)
MS_master_ma_mri$N.animals.MRI.ctrl1 <- as.numeric(MS_master_ma_mri$N.animals.MRI.ctrl1)
MS_master_ma_mri$Var.MRI.ctrl1 <- as.numeric(MS_master_ma_mri$Var.MRI.ctrl1)

condition <- MS_master_ma_mri$Datatype.MRI.drug1 == "mean, SEM"  # Define the condition for applying the formulas

# Apply the formulas to the selected rows
MS_master_ma_mri$Var.MRI.drug1 <- ifelse(condition, MS_master_ma_mri$Var.MRI.drug1 / sqrt(MS_master_ma_mri$N.animals.MRI.drug1), MS_master_ma_mri$Var.MRI.drug1)
MS_master_ma_mri$Var.MRI.ctrl1 <- ifelse(condition, MS_master_ma_mri$Var.MRI.ctrl1 / sqrt(MS_master_ma_mri$N.animals.MRI.ctrl1), MS_master_ma_mri$Var.MRI.ctrl1)
MS_master_ma_mri$Var.MRI.drug1 <- ifelse(MS_master_ma_mri$Var.MRI.drug1 == 0, MS_master_ma_mri$Var.MRI.drug1 + 0.001, MS_master_ma_mri$Var.MRI.drug1)
MS_master_ma_mri$Var.MRI.ctrl1 <- ifelse(MS_master_ma_mri$Var.MRI.ctrl1 == 0, MS_master_ma_mri$Var.MRI.ctrl1 + 0.001, MS_master_ma_mri$Var.MRI.ctrl1)



## Split dataframe
# Split the dataframe into separate dataframes based on the "Drug1" column
drug_dfs_mri <- split(MS_master_ma_mri, MS_master_ma_mri$Drug1)

Fingolimod_ma_mri <- drug_dfs_mri$Fingolimod
#D3_ma_mri <- drug_dfs_mri$'Vitamin D3' #only 1 study
#GA_ma_mri <- drug_dfs_mri$'Glatiramer acetate' #only 1 study
#Rituximab_ma_mri <- drug_dfs_mri$Rituximab #only 1 study
Natalizumab_ma_mri <- drug_dfs_mri$Natalizumab

#####################
### Fingolimod    ###
#####################
pes.forest <- metacont(N.animals.MRI.drug1, MRI.drug1, Var.MRI.drug1, N.animals.MRI.ctrl1, MRI.ctrl1, Var.MRI.ctrl1,
                       Study.year.journal, data = Fingolimod_ma_mri, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Fingolimod_ma_mri <- merge(ordered_df, Fingolimod_ma_mri, by = "Study.year.journal")
Fingolimod_ma_mri <- Fingolimod_ma_mri[!duplicated(Fingolimod_ma_mri$Title), ]#deduplicate
Fingolimod_ma_mri.ordered <- Fingolimod_ma_mri[order(Fingolimod_ma_mri$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.MRI.drug1, MRI.drug1, Var.MRI.drug1, N.animals.MRI.ctrl1, MRI.ctrl1, Var.MRI.ctrl1,
                               Study.year.journal, data = Fingolimod_ma_mri.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Fingolimod_forest_MRI.png", width = 3000, height = 900, res = 300)
forest(pes.forest.ordered,
       xlim = c(-40,5), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Fingolimod_TF.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)

#####################
###  Natalizumab  ###
#####################
pes.forest <- metacont(N.animals.MRI.drug1, MRI.drug1, Var.MRI.drug1, N.animals.MRI.ctrl1, MRI.ctrl1, Var.MRI.ctrl1,
                       Study.year.journal, data = Natalizumab_ma_mri, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# add SMD to order dataframe for forest plot
Study.year.journal <- pes.forest$studlab
effect_sizes <- pes.forest$TE
ordering_df <- data.frame(Study.year.journal, effect_sizes)# Create a dataframe for ordering
ordered_df <- ordering_df[order(ordering_df$effect_sizes), ]# Order the dataframe based on effect size
Natalizumab_ma_mri <- merge(ordered_df, Natalizumab_ma_mri, by = "Study.year.journal")
Natalizumab_ma_mri <- Natalizumab_ma_mri[!duplicated(Natalizumab_ma_mri$Title), ]#deduplicate
Natalizumab_ma_mri.ordered <- Natalizumab_ma_mri[order(Natalizumab_ma_mri$effect_sizes), ]
pes.forest.ordered <- metacont(N.animals.MRI.drug1, MRI.drug1, Var.MRI.drug1, N.animals.MRI.ctrl1, MRI.ctrl1, Var.MRI.ctrl1,
                               Study.year.journal, data = Natalizumab_ma_mri.ordered, sm = "SMD", method.ci = "NAsm", method.tau = "DL", title = "")

# Forestplot
png("Natalizumab_forest_MRI.png", width = 3000, height = 900, res = 300)
forest(pes.forest.ordered,
       xlim = c(-40,5), pscale = 1,
       rightcols = c("effect", "ci", "w.random"),
       rightlabs = c("Hedges' g", "95%-CI", "Weights"),
       leftcols = c("studlab"),
       leftlabs = c("Study", "Ribi"),
       clab = "Prevalence", 
       xlab = "Hedges g standardized mean difference",
       fs.xlab = 12,
       fs.study = 12,
       fs.study.lables = 12,
       fs.heading = 12,
       squaresize = 0.5, col.square = "navy", col.square.lines = "navy",
       col.diamond = "maroon", col.diamond.lines = "maroon",
       comb.fixed = FALSE,
       lty.fixed = 0,
       lty.random = 2,
       type.study = "square",
       type.random = "diamond",
       ff.fixed = "bold.italic",
       ff.random = "bold.italic",
       hetlab = "Heterogeneity",
       fs.hetstat = 10,
       smlab = "",
       printQ = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       print.tau2 = TRUE,
       col.by = "black",
       digits = 3,
       order = "SMD")
dev.off()

# publication bias
#Funnel
pes.logit.inf.funnel <- rma(pes.forest.ordered$TE, sei=pes.forest.ordered$seTE, method = "DL", weighted = TRUE, data = pes.forest.ordered)#DerSimonian-Laird
metafor::funnel(pes.logit.inf.funnel)
predict(pes.logit.inf.funnel)
pes.logit.inf.funnel
#Trim-and-fill (only possible for analysis without moderators): infers studies which are lacking to reach symmetry
pes.logit.inf.tf <- trimfill(pes.logit.inf.funnel)
predict(pes.logit.inf.tf, transf = transf.ilogit, digits = 5)
pes.logit.inf.tf#estimated number of missing studies = 16 on left side
metafor::funnel(pes.logit.inf.tf)

png("Natalizumab_TF_MRI.png", width = 2000, height = 2000, res = 300)
metafor::funnel(pes.logit.inf.tf)
dev.off()

#Egger's regression test
regtest(pes.logit.inf.funnel)#default test for funnel plot analysis, i.e., funnel plot asymmetry test. p < 0.05 --> funnel plot significantly asymmetric
#Rank correlation
ranktest(pes.logit.inf.funnel)

# Influence analysis/sensitivity analysis (leave-one-out)
metainf(pes.forest.ordered)


##################################
##### Logistic regression  #######
##################################

#Load libraries
library(dplyr)
library(maps)

##Create country list
all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")
all_countries <- str_c(all_countries, "|United States", "|United Kingdom") #add United States and United Kingdom

#Country check function
check_two_or_more_countries <- function(x) {
  unique_countries <- unique(unlist(str_extract_all(x, paste(all_countries, collapse = "|"))))
  if (length(unique_countries) >= 2) {
    return("Yes")
  } else {
    return("No")
  }
}

# Summarize data for each drug and calculate additional variables
Abatacept_logreg <- MS_master %>%
  filter(Drug1 == "Abatacept") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Alemtuzumab_logreg <- MS_master %>%
  filter(Drug1 == "Alemtuzumab") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Atacicept_logreg <- MS_master %>%
  filter(Drug1 == "Atacicept") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Atorvastatin_logreg <- MS_master %>%
  filter(Drug1 == "Atorvastatin") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Cladribine_logreg <- MS_master %>%
  filter(Drug1 == "Cladribine") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

DMF_logreg <- MS_master %>%
  filter(Drug1 == "Dimethyl fumarate") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

EGCG_logreg <- MS_master %>%
  filter(Drug1 == "Epigallocatechin gallate") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Estriol_logreg <- MS_master %>%
  filter(Drug1 == "Estriol") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Fampridine_logreg <- MS_master %>%
  filter(Drug1 == "Fampridine") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Fingolimod_logreg <- MS_master %>%
  filter(Drug1 == "Fingolimod") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

GA_logreg <- MS_master %>%
  filter(Drug1 == "Glatiramer acetate") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Inosine_logreg <- MS_master %>%
  filter(Drug1 == "Inosine") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

IFN_logreg <- MS_master %>%
  filter(Drug1 == "Interferon Beta 1") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Minocycline_logreg <- MS_master %>%
  filter(Drug1 == "Minocycline") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

MMF_logreg <- MS_master %>%
  filter(Drug1 == "Monomethyl fumarate") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Natalizumab_logreg <- MS_master %>%
  filter(Drug1 == "Natalizumab") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Opicinumab_logreg <- MS_master %>%
  filter(Drug1 == "Opicinumab") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Ozanimod_logreg <- MS_master %>%
  filter(Drug1 == "Ozanimod") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Pegifn_logreg <- MS_master %>%
  filter(Drug1 == "Peginterferon beta-1a") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Ponesimod_logreg <- MS_master %>%
  filter(Drug1 == "Ponesimod") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Raltegravir_logreg <- MS_master %>%
  filter(Drug1 == "Raltegravir") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Rituximab_logreg <- MS_master %>%
  filter(Drug1 == "Rituximab") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Siponimod_logreg <- MS_master %>%
  filter(Drug1 == "Siponimod") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Teriflunomide_logreg <- MS_master %>%
  filter(Drug1 == "Teriflunomide") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Toralizumab_logreg <- MS_master %>%
  filter(Drug1 == "Toralizumab") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

Ustekinumab_logreg <- MS_master %>%
  filter(Drug1 == "Ustekinumab") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )

D3_logreg <- MS_master %>%
  filter(Drug1 == "Vitamin D3") %>%
  mutate(Country = sapply(str_extract_all(Address, paste(all_countries, collapse = "|")), toString)) %>%
  summarise(
    Total_animals = sum(N.total.drug1, na.rm = TRUE),
    Mean_animals = mean(N.total.drug1, na.rm = TRUE),
    Extrapol_animals = n() * Mean_animals,
    Two_or_More_Models = ifelse(length(unique(Animal.model)) >= 2, "Yes", "No"),
    Unique_Animal_Models = paste(unique(Animal.model), collapse = ", "),
    Two_Sexes = ifelse(any(Sex == "both") | (any(Sex == "female") & any(Sex == "male")), "Yes", "No"),
    Regimen = ifelse(any(grepl("2", Regimen.drug1)), "Therapeutic", "Non-Therapeutic"),
    Two_or_More_Species = ifelse(length(unique(Species)) >= 2, "Yes", "No"),
    Unique_Species = paste(unique(Species), collapse = ", "),
    Two_or_More_Strains = ifelse(length(unique(Strain)) >= 2, "Yes", "No"),
    Unique_Strains = paste(unique(Strain), collapse = ", "),
    Outcomes = ifelse(
      any(!is.na(EAEscore.drug1) & EAEscore.drug1 != "NR") &
        any(!is.na(MRI.outcome.drug1) & MRI.outcome.drug1 != "NR"),
      "Yes", "No"
    ),
    Two_or_More_Countries = check_two_or_more_countries(Country),
    Number_of_Studies = n(),
    Positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 == "1"),
    Non_positive = sum(!is.na(Outcome.drug1) & Outcome.drug1 %in% c("0", "2", "3")),
    Outcome_NA = sum(is.na(Outcome.drug1))
  )


###########################
Abatacept_logreg$Drug <- "Abatacept"
Abatacept_logreg$Status <- "Failed"
Alemtuzumab_logreg$Drug <- "Alemtuzumab"
Alemtuzumab_logreg$Status <- "Approved"
Atacicept_logreg$Drug <- "Atacicept"
Atacicept_logreg$Status <- "Failed"
Atorvastatin_logreg$Drug <- "Atorvastatin"
Atorvastatin_logreg$Status <- "Failed"
Cladribine_logreg$Drug <- "Cladribine"
Cladribine_logreg$Status <- "Approved"
DMF_logreg$Drug <- "Dimethyl fumarate"
DMF_logreg$Status <- "Approved"
EGCG_logreg$Drug <- "EGCG"
EGCG_logreg$Status <- "Failed"
Estriol_logreg$Drug <- "Estriol"
Estriol_logreg$Status <- "Failed"
Fampridine_logreg$Drug <- "Fampridine"
Fampridine_logreg$Status <- "Approved"
Fingolimod_logreg$Drug <- "Fingolimod"
Fingolimod_logreg$Status <- "Approved"
GA_logreg$Drug <- "Glatiramer acetate"
GA_logreg$Status <- "Approved"
Inosine_logreg$Drug  <- "Inosine"
Inosine_logreg$Status <- "Failed"
IFN_logreg$Drug <- "Interferon Beta"
IFN_logreg$Status <- "Approved"
Minocycline_logreg$Drug <- "Minocycline"
Minocycline_logreg$Status <- "Failed"
MMF_logreg$Drug <- "Monomethyl fumarate"
MMF_logreg$Status <- "Approved"
Natalizumab_logreg$Drug <- "Natalizumab"
Natalizumab_logreg$Status <- "Approved"
Opicinumab_logreg$Drug <- "Opicinumab"
Opicinumab_logreg$Status <- "Failed"
Ozanimod_logreg$Drug <- "Ozanimod"
Ozanimod_logreg$Status <- "Approved"
Pegifn_logreg$Drug <- "Pefinterferon"
Pegifn_logreg$Status <- "Approved"
Ponesimod_logreg$Drug <- "Ponesimod"
Ponesimod_logreg$Status <- "Approved"
Raltegravir_logreg$Drug <- "Raltegravir"
Raltegravir_logreg$Status <- "Failed"
Rituximab_logreg$Drug <- "Rituximab"
Rituximab_logreg$Status <- "Approved"
Siponimod_logreg$Drug <- "Siponimod"
Siponimod_logreg$Status <- "Approved"
Teriflunomide_logreg$Drug <- "Teriflunomide"
Teriflunomide_logreg$Status <- "Approved"
Toralizumab_logreg$Drug <- "Toralizumab"
Toralizumab_logreg$Status <- "Failed"
Ustekinumab_logreg$Drug <- "Ustekinumab"
Ustekinumab_logreg$Status <- "Failed"
D3_logreg$Drug <- "Vitamin D3"
D3_logreg$Status <- "Failed"

Master_logreg <- bind_rows(Abatacept_logreg, Alemtuzumab_logreg, Atacicept_logreg, Atorvastatin_logreg, Cladribine_logreg,
  DMF_logreg, EGCG_logreg, Estriol_logreg, Fampridine_logreg, Fingolimod_logreg, GA_logreg, Inosine_logreg, IFN_logreg,
  Minocycline_logreg, MMF_logreg, Natalizumab_logreg, Opicinumab_logreg,  Ozanimod_logreg,  Pegifn_logreg, Ponesimod_logreg,
  Raltegravir_logreg, Rituximab_logreg, Siponimod_logreg, Teriflunomide_logreg, Toralizumab_logreg, Ustekinumab_logreg,
  D3_logreg)

Master_logreg <- Master_logreg[, c(ncol(Master_logreg), 1:(ncol(Master_logreg)-1))]
Master_logreg <- Master_logreg[, c(ncol(Master_logreg), 1:(ncol(Master_logreg)-1))]
Master_logreg[Master_logreg == "NaN"] <- NA
Master_logreg$Total_animals <- replace(Master_logreg$Total_animals, Master_logreg$Total_animals == 0, NA)
Master_logreg$Prop_positive <- Master_logreg$Positive/Master_logreg$Number_of_Studies
Master_logreg$Status_bin <- ifelse(Master_logreg$Status == "Approved", 1, 0)

##print table
write.table(Master_logreg, file = "Logreg_table.txt", sep = ";", quote = FALSE, row.names = F)


##Run logistic regression model
summary_logreg <- summary(fit <- glm(Status_bin ~ Total_animals + Extrapol_animals + Two_or_More_Models + Two_Sexes + Regimen +
               Two_or_More_Species + Two_or_More_Strains + Outcomes + Two_or_More_Countries + 
               Number_of_Studies + Prop_positive,
             data = Master_logreg, 
             family = binomial))
summary_logreg$coefficients

Master_logreg$y_pred <- predict(model, Master_logreg, type="response")
Master_logreg$rounded_y_pred <- round(Master_logreg$y_pred, 10)



Master_logreg_subset1 <- subset(Master_logreg, Master_logreg$y_pred < 0.99 &
                                 Master_logreg$y_pred > 0.01)

summary(fit <- glm(Status_bin ~ Total_animals + Extrapol_animals + Two_or_More_Models +
                     Two_or_More_Species + Outcomes + 
                     Number_of_Studies + Prop_positive,
                   data = Master_logreg_subset, 
                   family = binomial))

Master_logreg_subset2 <- subset(Master_logreg, Master_logreg$y_pred < 0.99)

summary(fit <- glm(Status_bin ~ Total_animals + Extrapol_animals + Two_or_More_Models + Two_Sexes + Regimen +
                     Two_or_More_Species + Two_or_More_Strains + Outcomes + Two_or_More_Countries + 
                     Number_of_Studies + Prop_positive,
                   data = Master_logreg_subset2, 
                   family = binomial))

Master_logreg_subset3 <- subset(Master_logreg, Master_logreg$y_pred > 0.01)

summary(fit <- glm(Status_bin ~ Total_animals + Extrapol_animals + Two_or_More_Models + Two_Sexes +
                     Two_or_More_Species + Two_or_More_Strains + Outcomes + 
                     Number_of_Studies + Prop_positive,
                   data = Master_logreg_subset3, 
                   family = binomial))


nrow(MS_master[MS_master$Drug1 == "Fingolimod",])


nrow(MS_master[MS_master$Drug1 %in% c("Fingolimod", "Alemtuzumab", "Cladribine", "Dimethyl fumarate",
                                      "Fampridine", "Glatiramer acetate", "Interferon Beta 1",
                                      "Monomethyl fumarate", "Natalizumab", "Ozanimod",
                                      "Peginterferon beta-1a", "Ponesimod", "Rituximab",
                                      "Siponimod", "Teriflunomide"),])
nrow(MS_master[MS_master$Drug1 %in% c("Abatacept", "Atacicept", "Atorvastatin", "Epigallocatechin gallate",
                                      "Estriol", "Inosine", "Minocycline",
                                      "Opicinumab", "Toralizumab", "Ustekinumab",
                                      "Vitamin D3"),])

table(MS_master$Drug1)


## Test dataframe
test_df <- data.frame(
  Animal_ID = c(1, 2, 3, 4, 5),
  Drug1 = c("Fingolimod", "Fingolimod", "Other Drug", "Fingolimod", "Other Drug"),
  Age = c(30, 40, 25, 50, 35),
  Sex = c("NR", "female", "both", "male", "female"),
  Animal.model = c("Model1", "Model2", "Model3", "Model1", "Model1"),
  EAEscore.drug1 = c(3, 4, NA, 5, NA),
  MRI.outcome.drug1 = c(NA, "NR", NA, NA, NA),
  N.total.drug1 = c(24, 11, 5, 16, NA),
  Regimen.drug1 = c("1", "1,2", "1", "2", "1"),
  Species = c("Mouse", "Rat", "Mouse", "Rat", "Rat"),
  Strain = c("StrainA", "StrainB", "StrainA", "StrainC", "StrainB"),
  Address = c("123 Main St, New York, France", "456 Elm St, France", "789 Maple Ave, Paris, France", "321 Oak St, Berlin, France", "987 Pine St, Sydney, France")
)



test <- MS_master %>%
  filter(is.naDrug1 == "Abatacept" & )
test <- MS_master[!is.na(MS_master$tte), ]

test <- merge(MS_master_ma, MS_master_ma_mri, by = "Title", all.x = T)


mean(Teriflunomide_ma$N.total.drug1, na.rm = TRUE)*nrow(Teriflunomide_ma)
sum(Teriflunomide_ma$N.total.drug1, na.rm = TRUE)
mean(Alemtuzumab_ma$N.total.drug1, na.rm = TRUE)*nrow(Alemtuzumab_ma)
sum(Alemtuzumab_ma$N.total.drug1, na.rm = TRUE)
mean(GA_ma$N.total.drug1, na.rm = TRUE)*nrow(GA_ma)
sum(GA_ma$N.total.drug1, na.rm = TRUE)
mean(Fingolimod_ma$N.total.drug1, na.rm = TRUE)*nrow(Fingolimod_ma)
sum(Fingolimod_ma$N.total.drug1, na.rm = TRUE)
mean(Natalizumab_ma$N.total.drug1, na.rm = TRUE)*nrow(Natalizumab_ma)
sum(Natalizumab_ma$N.total.drug1, na.rm = TRUE)




###############################
####   survival analysis   ####
###############################
library(survival)
library(survminer)
library(grid)
library(gridExtra)

MS_survival <- read_excel("C:/Users/benja/Dropbox/Research/Experiments/Multiple-sclerosis-translation/MS_translation/Drug_CV.xlsx", sheet=3)
MS_survival$First_animal_overall_final <- as.numeric(MS_survival$First_animal_overall_final)
MS_survival$First_human <- as.numeric(MS_survival$First_human)
MS_survival$failure_year <- as.numeric(MS_survival$failure_year)
MS_survival$FDA_approval <- as.numeric(MS_survival$FDA_approval)
MS_survival$event <- 1

#### Animal -> first-in-human
MS_survival_firsthuman <- subset(MS_survival, MS_survival$Animal_to_human == 1)
MS_survival_firsthuman$tte_animal.human <- MS_survival_firsthuman$First_human - MS_survival_firsthuman$First_animal_overall_final
MS_survival_firsthuman <- subset(MS_survival_firsthuman, tte_animal.human > 0)

model_fit_clin <- survfit(Surv(tte_animal.human, event) ~ 1, data = MS_survival_firsthuman, type = "kaplan-meier")
summary(model_fit_clin)$table

# Create the survival plot without the risk table
surv_plot <- ggsurvplot(model_fit_clin,
                        pval = F, conf.int = TRUE,
                        risk.table = FALSE,
                        linetype = "strata",
                        surv.median.line = "hv",
                        ggtheme = theme_minimal(),
                        palette = "#4cb080",
                        legend = "none",
                        font.tickslab = c(14, "plain", "black"))

surv_plot$plot <- surv_plot$plot + 
  labs(title = "A. Survival time until first in human trial",
       x = "",
       y = "\n Survival probability \n") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) +  # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 30))

# Create the risk table plot with adjusted font
risk_table_plot <- ggsurvplot(model_fit_clin,
                              risk.table = TRUE,
                              ggtheme = theme_minimal(),
                              font.tickslab = c(16, "plain", "black"),
                              risk.table.fontsize = 6)  # Adjust font size for the risk table

risk_table_plot$table <- risk_table_plot$table + 
  labs(y = "\n Strata \n \n", x = "\n Time (years)") + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) + # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 30))

# Combine and display
Clin_survival <- grid.arrange(surv_plot$plot, risk_table_plot$table, ncol = 1, heights = c(2, 1))


#### Animal -> failure
MS_survival_failure <- subset(MS_survival, MS_survival$Animal_to_failure == 1 |
                                MS_survival$Animal_to_failure == 0 )
MS_survival_failure$tte_failure <- ifelse(MS_survival_failure$Animal_to_failure == 1,
                                          MS_survival_failure$failure_year - MS_survival_failure$First_animal_overall_final,
                                          MS_survival_approval$FDA_approval - MS_survival_approval$First_animal_overall_final)
MS_survival_failure <- subset(MS_survival_failure, tte_failure > 0)
MS_survival_failure$event <- ifelse(MS_survival_failure$Animal_to_failure == 1, 1, 0)


model_fit_failure <- survfit(Surv(tte_failure, event) ~ 1, data = MS_survival_failure, type = "kaplan-meier")
summary(model_fit_failure)$table

# Create the survival plot without the risk table
surv_plot <- ggsurvplot(model_fit_failure,
                        pval = F, conf.int = TRUE,
                        risk.table = FALSE,
                        linetype = "strata",
                        surv.median.line = "hv",
                        ggtheme = theme_minimal(),
                        palette = "#4cb080",
                        legend = "none",
                        font.tickslab = c(14, "plain", "black"))

surv_plot$plot <- surv_plot$plot + 
  labs(title = "C. Survival time until failure",
       x = "",
       y = "\n Survival probability \n") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) +  # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 30))

# Create the risk table plot with adjusted font
risk_table_plot <- ggsurvplot(model_fit_failure,
                              risk.table = TRUE,
                              ggtheme = theme_minimal(),
                              font.tickslab = c(16, "plain", "black"),
                              risk.table.fontsize = 6)  # Adjust font size for the risk table

risk_table_plot$table <- risk_table_plot$table + 
  labs(y = "\n Strata \n \n", x = "\n Time (years)") + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) + # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 30))

# Combine and display
Failure_survival <- grid.arrange(surv_plot$plot, risk_table_plot$table, ncol = 1, heights = c(2, 1))

#### Animal -> approval
MS_survival_approval <- subset(MS_survival, MS_survival$Animal_to_approval == 1|
                                            MS_survival$Animal_to_approval == 0)
MS_survival_approval$tte_approval <- ifelse(MS_survival_approval$Animal_to_approval == 1,
                                            MS_survival_approval$FDA_approval - MS_survival_approval$First_animal_overall_final,
                                            MS_survival_approval$failure_year - MS_survival_approval$First_animal_overall_final)
MS_survival_approval <- subset(MS_survival_approval, tte_approval > 0)
MS_survival_approval$event <- ifelse(MS_survival_approval$Animal_to_approval == 1, 1, 0)

model_fit_approval <- survfit(Surv(tte_approval, event) ~ 1, data = MS_survival_approval, type = "kaplan-meier")
summary(model_fit_approval)$table

# Create the survival plot without the risk table
surv_plot <- ggsurvplot(model_fit_approval,
                        pval = F, conf.int = TRUE,
                        risk.table = FALSE,
                        linetype = "strata",
                        surv.median.line = "hv",
                        ggtheme = theme_minimal(),
                        palette = "#4cb080",
                        legend = "none",
                        font.tickslab = c(14, "plain", "black"))

surv_plot$plot <- surv_plot$plot + 
  labs(title = "B. Survival time until FDA approval",
       x = "",
       y = "\n Survival probability \n") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold", hjust = 0)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) +  # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 30))

# Create the risk table plot with adjusted font
risk_table_plot <- ggsurvplot(model_fit_approval,
                              risk.table = TRUE,
                              ggtheme = theme_minimal(),
                              font.tickslab = c(16, "plain", "black"),
                              risk.table.fontsize = 6)  # Adjust font size for the risk table

risk_table_plot$table <- risk_table_plot$table + 
  labs(y = "\n Strata \n \n", x = "\n Time (years)") + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) + # Replace max_time with the maximum value of your data
  coord_cartesian(xlim = c(0, 30))

# Combine and display
Approval_survival <- grid.arrange(surv_plot$plot, risk_table_plot$table, ncol = 1, heights = c(2, 1))


#Combine graphs
grid_arrangement_survival <- grid.arrange(
  Clin_survival,
  Approval_survival,
  Failure_survival,
  ncol = 1
)

#Print figure
png("Survival_plot_censored.png", width=2000, height=4000, res=300)
grid.arrange(
  Clin_survival,
  Approval_survival,
  Failure_survival,
  ncol = 1
)
dev.off()


#Trial ranges
#Overall
#Median first animal trial (range). 
median(MS_survival$First_animal_overall_final, na.rm = T)
range(MS_survival$First_animal_overall_final, na.rm = T)

#Median first in MS trial (range).
median(MS_survival$First_human, na.rm = T)
range(MS_survival$First_human, na.rm = T)

#Approved
#Median first animal trial (range). 
median(MS_survival$First_animal_overall_final[MS_survival$Label == "Marketed"], na.rm = T)
range(MS_survival$First_animal_overall_final[MS_survival$Label == "Marketed"], na.rm = T)

#Median first in MS trial (range).
median(MS_survival$First_human[MS_survival$Label == "Marketed"], na.rm = T)
range(MS_survival$First_human[MS_survival$Label == "Marketed"], na.rm = T)

#Failed
#Median first animal trial (range). 
median(MS_survival$First_animal_overall_final[MS_survival$Label == "Failed"], na.rm = T)
range(MS_survival$First_animal_overall_final[MS_survival$Label == "Failed"], na.rm = T)

#Median first in MS trial (range).
median(MS_survival$First_human[MS_survival$Label == "Failed"], na.rm = T)
range(MS_survival$First_human[MS_survival$Label == "Failed"], na.rm = T)



############################
### Interrater agreement ###
############################
#Species
rater1 <- rep(1,60)
rater2 <- c(rep(1, 56),2,2,2,2)
sum(rater1 == rater2) / length(rater1)

#Sex
rater1 <- rep(1,60)
rater2 <- c(rep(1,53),2,2,2,2,2,2,2)
sum(rater1 == rater2) / length(rater1)

#Age
rater1 <- rep(1, 60)
rater2 <- c(rep(1,53),2,2,2,2,2,2,2)
sum(rater1 == rater2) / length(rater1)


#Tested drugs
rater1 <- rep(1, 60)
rater2 <- c(rep(1,54),2,2,2,2,2,2)
sum(rater1 == rater2) / length(rater1)

#Outcomes
rater1 <- rep(1, 60)
rater2 <- c(rep(1,50),2,2,2,2,2,2,2,2,2,2)
sum(rater1 == rater2) / length(rater1)





###older version of umbrella forest plot, should work
ggplot(data = Forest, aes(y = index, x = result)) +
  geom_errorbarh(aes(xmin = error_lower, xmax = error_upper, color = color), 
                 height = 0.1, lwd = 0.5) +
  geom_point(aes(color = color, fill = color), pch = 18, size = 3.5) +
  scale_y_reverse(breaks = Forest$index, labels = Forest$study) +
  scale_color_manual(values = c("black", "#799FCB", "#ff6961")) +
  scale_fill_manual(values = c("black", "#799FCB", "#ff6961")) +
  labs(title = "", y = "", x = "Hedges' g") +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha = 1) +
  guides(color = "none", fill = "none") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12))
