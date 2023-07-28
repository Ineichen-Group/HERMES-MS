#Install the required package
install.packages('googlesheets4')
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("crayon")
install.packages("crayon")
install.packages("readxl")
install.packages("openxlsx") 
install.packages("forcats")


#Load the required library 
library(googlesheets4)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(crayon)
library(readxl)
library(openxlsx) 
library(forcats)
library(readxl)
library(ggbeeswarm)


#Read google sheets data into R
HERMES_Extraction<- read_excel("MS_translation_extraction.xlsx", sheet=2, na="")

#2 the second sheet is imported
#unsere NAs zu NRs machen(?) und mit NAs importieret(gemacht)
HERMES_Extraction <- HERMES_Extraction %>%
  mutate_all(~replace(., . == "NA", "NR"))

# Read google sheets data (Drug CV) into R
Drug_CV_Data <- read_excel("MS_translation_extraction.xlsx", 
                           sheet = "Drug_CV", na="")
# Replace "NA" with "NR" and convert to NAs
Drug_CV_Data <- Drug_CV_Data %>%
  mutate_all(~replace(., . == "NA", "NR")) %>%
  mutate_all(as.character) %>%
  mutate_all(~replace(., . == "NR", NA))

#druglist importieren
druglist_total <- read_excel("Druglist_Multiple-sclerosis_Mastersheet.xlsx", sheet= "Included_SR", n_max=43)
druglist <- data.frame(druglist_total$Name, druglist_total$Reason...15)



########## ***PART 1: Data cleanup*** (most of it prepared by Pia)

#has to be done before INCLUDED dataframe is built
HERMES_Extraction["296", "Strain"]<-"C57BL/6" #Z297- nur C57BL/6 wurde EAE induziert;
HERMES_Extraction["281", "Strain"]<- "SWXJ" #clearly steated in graph of clinical scores and in methods part Z282
HERMES_Extraction["291", "Strain"]<- "C57BL/6" #Z292 as stated in comment
HERMES_Extraction["298", "Strain"]<- "Lewis" # Z299 as stated in comment
HERMES_Extraction["298", "Species"]<- "rat" # Z299 as stated in comment
HERMES_Extraction["313", "Strain"]<- "SJL/J" # Z314 only RREAE used- this model was only induced in SJL/J mice
HERMES_Extraction["324", "Strain"]<- "2D2xTh" # Z325 s. onenote- werden noch gekreuzt
HERMES_Extraction["331", "Strain"]<- "C57BL/6"# Z332 as stated in comment
HERMES_Extraction["351", "Strain"]<- "C57BL/6"# Z352 as stated in comment 
HERMES_Extraction["351", "Species"]<- "mouse" # Z352 as stated in comment 
#Z396 no clinical score extracted- keep strain combination-> splitten ...
#Z397 no clinical score extracted- keep combination? mouse, xenopus
#Z416 no clinical score extracted- keep combination? mouse, rat
HERMES_Extraction["414", "Species"]<- "rat" # Z415 as stated in comment
HERMES_Extraction["414", "Strain"]<- "Sprague-Dawley" # s. oben
HERMES_Extraction["442", "Strain"]<- "SJL/J" #Z443 as stated in comment
HERMES_Extraction["450", "Strain"]<- "C57BL/6"# Z451 as stated in comment
HERMES_Extraction["461", "Strain"]<- "B10.PL" #Z462 as stated in comment
HERMES_Extraction["465", "Strain"]<- "Lewis"# Z466 as stated in comment 
HERMES_Extraction["465", "Species"]<- "rat" # Z466 as stated in comment
HERMES_Extraction["470", "Strain"]<- "Dark Agouti"# Z471 as stated in comment 
#Z589 no clinical score extracted- keep strain combination?

#generates dataframe with only included papers
HERMES_INCLUDED<-data.frame(HERMES_Extraction[0,])
for (i in 2:740) {
  if (HERMES_Extraction[i, "Included"] == 1) {
    HERMES_INCLUDED <- rbind(HERMES_INCLUDED, HERMES_Extraction[i, ])
  }
}
#print(HERMES_INCLUDED)

#generates dataframe with only excluded papers
HERMES_EXCLUDED<-data.frame(HERMES_Extraction[0,])
for (i in 2:740) {
  if (HERMES_Extraction[i, "Included"] == 0) {
    HERMES_EXCLUDED <- rbind(HERMES_EXCLUDED, HERMES_Extraction[i, ])
  }
}

#count how many times something occurs -useNA stellt auch NAs dar

#BEREINIGEN VON SCHREIBFEHLERN

#table(HERMES_EXCLUDED$`Reason for exclusion`, useNA="ifany")
HERMES_EXCLUDED$`Reason for exclusion`<-gsub("dulicate","duplicate", HERMES_EXCLUDED$`Reason for exclusion`)
HERMES_EXCLUDED$`Reason for exclusion`<-gsub("Other","other", HERMES_EXCLUDED$`Reason for exclusion`)
table(HERMES_EXCLUDED$`Reason for exclusion`, useNA="ifany")

table(HERMES_INCLUDED$`Extracted by`, useNA="ifany") #no problems

#wenn mehrmals ausgeführt wird --> TMEV-IDD-IDD --> daher dieser code:
#table(HERMES_INCLUDED$`Animal model`, useNA="ifany") 
HERMES_INCLUDED$`Animal model`[HERMES_INCLUDED$`Animal model`=="TMEV"]<-"TMEV-IDD"
#HERMES_INCLUDED$`Animal model`[HERMES_INCLUDED$`Animal model`=="TMEV-IDD-IDD"]<-"TMEV-IDD"
HERMES_INCLUDED$`Animal model`[HERMES_INCLUDED$`Animal model`=="AT-EAE"]<-"passive EAE"
HERMES_INCLUDED$`Animal model`[HERMES_INCLUDED$`Animal model`=="passive EAE"]<-"EAE" 
table(HERMES_INCLUDED$`Animal model`, useNA="ifany") 

#table(HERMES_INCLUDED$Species, useNA="ifany") 
HERMES_INCLUDED$Species<-gsub("mosue", "mouse", HERMES_INCLUDED$Species)
HERMES_INCLUDED$Species<-gsub("rat, mouse", "mouse, rat", HERMES_INCLUDED$Species)
table(HERMES_INCLUDED$Species, useNA="ifany")

#table(HERMES_INCLUDED$Strain, useNA="ifany")
# Wistar (Albino rats) mit Wistar poolen-> ja
# albino line H substrain of C57BL/6-> gepoolt
# SJL, "SJL/J", "SJL/L", "SJL (H-2s)", "SJL / ola", "Swiss Jim Lambert" gepoolt
# B10.PL, B10.PL/Sn, B10.PL(73NS)/Sn, B10.PL-H2uH2-T18a/(73NS)/SnJ (B10.PL) gepoolt
# TCRMOG (2D2), IgHMOG (TH) Namen entsprechend angepasst
corrections <- data.frame(
  incorrect = c("C57Bl/6", "C57BL/6J", "C57BL6", "C57/BL6", "C57BL/6/J", "C57Bl76", "C57BL76",
                "C57BL6/J", "C57BL/5", "C57B16", "C57BL/6N", "B6", "albino (line H)", "B10.PL/Sn", "B10.PL(73NS)/Sn",
                "B10.PL-H2uH2-T18a/(73NS)/SnJ (B10.PL)", "(PLxSJL )F1", "PL-SJLF1/J", "SJL x BALB/c",
                "SJLxBALB/c", "SJL/J", "SJL/L", "SJL (H-2s)", "SJL / ola", "Swiss Jim Lambert",
                "BN", "Sprague- Dawley", "Sprague–Dawley", "SJL, C57BL/6", "2D2xTh", "Th", 
                "TCRMOG (2D2), IgHMOG (TH)", "C57BL/6, 2D2", "C57BL/6.Cg-Tg (IgHMOGy1-YFP) 16Jrs/J (IgHMOGy1-YFP)", "Biozzi ABH (H2dq1)", "DA/OlaHsd"),
  correct = c("C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6",
              "C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6", "C57BL/6", "B10.PL", "B10.PL",
              "B10.PL", "(PLxSJL)F1", "(PLxSJL)F1", "(SJLxBALB/c)F1", "(SJLxBALB/c)F1", "SJL",
              "SJL", "SJL", "SJL", "SJL", "Brown Norway", "Sprague-Dawley", "Sprague-Dawley",
              "C57BL/6, SJL", "TCRMOGxIgHMOG", "IgHMOG", "TCRMOG, IghMOG", "C57BL/6, TCRMOG",  "C57BL/B6.Cg-IgHMOGy1-YFP", "Biozzi ABH" , "Dark Agouti")
)

# Apply the corrections to the 'Strain' column in the dataframe
replace_terms <- function(text, replacements) {
  for (i in 1:nrow(replacements)) {
    text <- gsub(replacements[i, "incorrect"], replacements[i, "correct"], text, fixed = TRUE)
  }
  return(text)
}
HERMES_INCLUDED$Strain <- replace_terms(HERMES_INCLUDED$Strain, corrections)

HERMES_INCLUDED$Strain[HERMES_INCLUDED$Strain=="SJL x BALB/c"]<-"(SJLxBALB/c)F1"
HERMES_INCLUDED$Strain[HERMES_INCLUDED$Strain=="SJLxBALB/c"]<-"(SJLxBALB/c)F1"
#GFP zusamme nehmen???

table(HERMES_INCLUDED$Strain, useNA="ifany") 
#Wenn man das correction/replace terms mehrmals hintereinander ausführt, werden die SJLxBALB/c F1 doppelt ersetzt- kann aber einfach zurückgesetzt werden in dem man das Included Dataframe einfach neu lädt und alles nur einmal ausführt


#table(HERMES_INCLUDED$Sex, useNA="ifany")
HERMES_INCLUDED$Sex<-gsub("both sexes", "both", HERMES_INCLUDED$Sex)
table(HERMES_INCLUDED$Sex, useNA="ifany")

#table(HERMES_INCLUDED$`Tested drug(s)`, useNA="ifany") #hat noch Komibs- so lassen?
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="Interferon Beta1"]<-"Interferon Beta 1"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="Minocyclin"]<-"Minocycline"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="17beta-estradiol"]<-"Estradiol"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="FIngolimmod"]<-"Fingolimod"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="Fingolimmod"]<-"Fingolimod"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="1,25-dihydroxyvitamin D3"]<-"Calcitriol"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="Calcitriol"]<-"Vitamin D3"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="Estradiol"]<-"Estriol"
#
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="anti-CD20 monoclonal antibody"]<-"anti-CD20" 
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="anti-CD20 antibody 18B12"]<-"anti-CD20"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="anti-CD20"]<-"Rituximab"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="CTLA-4-Fc"]<-"CTLA4-Ig"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="CTLA4-Ig"]<-"Abatacept"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="Anti-mouse CD52 monoclonal antibody"]<-"anti-CD52"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="murine anti-CD52 antibody"]<-"anti-CD52"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="anti-CD52"]<-"Alemtuzumab"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="CTLA4-Ig"]<-"Abatacept"
HERMES_INCLUDED$`Tested drug(s)`[HERMES_INCLUDED$`Tested drug(s)`=="anti-VLA-4 antibody"]<-"Natalizumab"
table(HERMES_INCLUDED$`Tested drug(s)`, useNA="ifany")
#Z113: Fingolimod und GA wurde verwendet, keine Daten extrahiert


#table(HERMES_INCLUDED$`Tested drug 2`, useNA="ifany")
HERMES_INCLUDED$`Tested drug 2`[HERMES_INCLUDED$`Tested drug 2`=="estriol"]<-"Estriol"
HERMES_INCLUDED$`Tested drug 2`[HERMES_INCLUDED$`Tested drug 2`=="Calcitriol"]<-"Vitamin D3"
table(HERMES_INCLUDED$`Tested drug 2`, useNA="ifany")
table(HERMES_INCLUDED$`Tested drug 3`, useNA="ifany")

#table(HERMES_INCLUDED$`Data type...27`, useNA="ifany")
HERMES_INCLUDED$`Data type...27`[HERMES_INCLUDED$`Data type...27`=="mean, EM"]<-"mean,SEM"
HERMES_INCLUDED$`Data type...27`[HERMES_INCLUDED$`Data type...27`=="mean, SE"] <- "mean, SEM"
HERMES_INCLUDED$`Data type...27`[HERMES_INCLUDED$`Data type...27`=="mean,SEM"] <- "mean, SEM"
HERMES_INCLUDED$`Data type...27`[HERMES_INCLUDED$`Data type...27`=="mean, NA"] <- "mean"
table(HERMES_INCLUDED$`Data type...27`, useNA="ifany")
HERMES_INCLUDED$`Data type...34`[HERMES_INCLUDED$`Data type...34`=="mean, SE"] <- "mean, SEM"
table(HERMES_INCLUDED$`Data type...34`, useNA="ifany")
#table(HERMES_INCLUDED$`Data type...73`, useNA="ifany")
HERMES_INCLUDED$`Data type 2`[HERMES_INCLUDED$`Data type 2`=="mean, NA"] <- "mean"
table(HERMES_INCLUDED$`Data type 2`, useNA="ifany")
table(HERMES_INCLUDED$`Data type 3`, useNA="ifany")
table(HERMES_INCLUDED$`Data type...91`, useNA="ifany")
table(HERMES_INCLUDED$`Data type...100`, useNA="ifany")

HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="apparent diffusion coefficient values parallel to the long axis"]<-"ADC"
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="gadolinium enhancement measure"]<-"gadolinium"
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="Gadolinium Score"]<-"gadolinium"
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="Lesion volume total"]<-"T2"
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="T2 lesion load"]<-"T2"
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="T1 lesion in mm^3"]<-"T1"
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="MRI Volume"]<-"brain volume" #richtig so???
HERMES_INCLUDED$`MRI Outcome...35`[HERMES_INCLUDED$`MRI Outcome...35`=="ventricle size"]<-"brain volume" #richtig so???
table(HERMES_INCLUDED$`MRI Outcome...35`, useNA="ifany")

HERMES_INCLUDED$`MRI Outcome...92`[HERMES_INCLUDED$`MRI Outcome...92`=="apparent diffusion coefficient values perpendicular to the optic nerve long axis"]<-"ADC"
HERMES_INCLUDED$`MRI Outcome...92`[HERMES_INCLUDED$`MRI Outcome...92`=="Gd-DOTA Medulla"]<-"gadolinium"
HERMES_INCLUDED$`MRI Outcome...92`[HERMES_INCLUDED$`MRI Outcome...92`=="normalized T2 signal intensity Corpus callosum"]<-"T2"
HERMES_INCLUDED$`MRI Outcome...92`[HERMES_INCLUDED$`MRI Outcome...92`=="T2 cerebral edema"]<-"T2"
HERMES_INCLUDED$`MRI Outcome...92`[HERMES_INCLUDED$`MRI Outcome...92`=="T2-WSI"]<-"T2"
HERMES_INCLUDED$`MRI Outcome...92`[HERMES_INCLUDED$`MRI Outcome...92`=="MRI load"]<-""#zu was poolen???
table(HERMES_INCLUDED$`MRI Outcome...92`, useNA="ifany")

HERMES_INCLUDED$`MRI Outcome...101`[HERMES_INCLUDED$`MRI Outcome...101`=="gadolinium enhancement measure"]<-"gadolinium"
HERMES_INCLUDED$`MRI Outcome...101`[HERMES_INCLUDED$`MRI Outcome...101`=="Gd-DOTA Inf. Colliculus"]<-"gadolinium"
HERMES_INCLUDED$`MRI Outcome...101`[HERMES_INCLUDED$`MRI Outcome...101`=="PIxel Intensity"]<-"" #wohin soll man das poolen???
HERMES_INCLUDED$`MRI Outcome...101`[HERMES_INCLUDED$`MRI Outcome...101`=="normalized T2 signal intensity Hippocampus"]<-"T2"
table(HERMES_INCLUDED$`MRI Outcome...101`, useNA="ifany")

HERMES_INCLUDED$`MRI Outcome...109`[HERMES_INCLUDED$`MRI Outcome...109`=="normalized T2 signal intensity Cortex"]<-"T2"
HERMES_INCLUDED$`MRI Outcome...109`[HERMES_INCLUDED$`MRI Outcome...109`=="Gd-Dota Striatum"]<-"gadolinium"
table(HERMES_INCLUDED$`MRI Outcome...109`, useNA="ifany")



###addition of drug status column (marketed vs failed)
HERMES_drugs <- data.frame(status = character(474))  # Create a new data frame with 'status' column

for (y in 1:474) {
  found_match <- FALSE
  
  for (j in 1:43) {
    if (!is.na(HERMES_INCLUDED[y, "Tested drug(s)"]) && HERMES_INCLUDED[y, "Tested drug(s)"] == druglist[j, "druglist_total.Name"]) {
      found_match <- TRUE
      
      if (druglist[j, "druglist_total.Reason...15"] == "Marketed") {
        HERMES_drugs$status[y] <- "Marketed"
      } else {
        HERMES_drugs$status[y] <- "Failed"
      }
      
      break  # Exit the inner loop once a match is found
    }
  }
  
  if (!found_match) {
    HERMES_drugs$status[y] <- "NA"
  }
}

HERMES_INCLUDED$status <- HERMES_drugs$status  # Add 'status' column from HERMES_drugs to HERMES_INCLUDED

###drug status of second drug tested in publication:
HERMES_drugs2 <- data.frame(status2 = character(474))  # Create a new data frame with 'status2' column

for (y in 1:474) {
  found_match <- FALSE
  
  for (j in 1:43) {
    if (!is.na(HERMES_INCLUDED[y, "Tested drug 2"]) && HERMES_INCLUDED[y, "Tested drug 2"] == druglist[j, "druglist_total.Name"]) {
      found_match <- TRUE
      
      if (druglist[j, "druglist_total.Reason...15"] == "Marketed") {
        HERMES_drugs2$status2[y] <- "Marketed"
      } else {
        HERMES_drugs2$status2[y] <- "Failed"
      }
      
      break  # Exit the inner loop once a match is found
    }
  }
  
  if (!found_match) {
    HERMES_drugs2$status2[y] <- "NA"
  }
}

HERMES_INCLUDED$status2 <- HERMES_drugs2$status2  # Add 'status2' column from HERMES_drugs2 to HERMES_INCLUDED
###


### use if needed: Make separate dataframes for each drug
# # Ensure that "Tested drug(s)" is a factor
# HERMES_INCLUDED$`Tested drug(s)` <- as.factor(HERMES_INCLUDED$`Tested drug(s)`)
# 
# # Split the data into a list of data frames
# split_data <- split(HERMES_INCLUDED, HERMES_INCLUDED$`Tested drug(s)`)
# 
# # Loop over the list and assign each data frame to a new variable
# for (i in seq_along(split_data)) {
#   # Create a valid variable name from the drug name
#   var_name <- make.names(levels(HERMES_INCLUDED$`Tested drug(s)`)[i])
#   
#   # Assign the data frame to a new variable in the global environment
#   assign(var_name, split_data[[i]])
# }
###


########## ***PART 2: Graphs***

#colours
display.brewer.all()
Blues9<- brewer.pal(9,"Blues")
BuPu5<- brewer.pal(5, "BuPu")
hist(HERMES_INCLUDED$Year)
Set2<-brewer.pal(5, "Set2")
spectral <- brewer.pal(10, "Spectral")

#
#numbers from before data extraction are stored at another place
nrow(HERMES_Extraction) #number of papers after abstract screening and deletion of all papers labeled as conference abstract
nrow(HERMES_INCLUDED) #number of included papers in data extraction
nrow(HERMES_EXCLUDED) #number of excluded papers in data extraction
table(HERMES_EXCLUDED$`Reason for exclusion`)


### Figure 1 FLOW CHART
library(DiagrammeR)

grViz("
  digraph {
    
    # graph attributes
    graph [layout = dot, rankdir = TB]
      
    # node attributes
    node [shape = box, style = filled, fillcolor = lightsteelblue1, fontcolor = midnightblue, fontname = Calibri, color = darkslategray, penwidth = 2.0]
      
    # edge attributes
    edge [color = darkslategray, penwidth = 2.0]
      
    # nodes
    A [label = <<FONT POINT-SIZE='18'>Identification of successfully and non-successfully translated MS drugs on clinical trial level</FONT><BR/><BR/><FONT POINT-SIZE='10'>Marked-approved disease-modifying treatments for MS are considered successful drug candidates.<BR/>Non-successfull drug candidates were identified via MS clinical trial search on Adis Insigth and also via 3 published reviews on the topic.<BR/>The selected non-successfull drug candidates failed in clinical trials due to safety or efficacy issues.</FONT>>]
    B [label = <<FONT POINT-SIZE='18'>Identification of preclinical in vivo studies testing these drugs in MS animal models</FONT><BR/><BR/><FONT POINT-SIZE='10'>Search performed in MEDLINE via PubMed and in EMBASE using a predefined search string</FONT>>]
    C [label = <<FONT POINT-SIZE='18'>Study selection: identification of all studies which test a drug of interest in an MS animal model</FONT><BR/><BR/><FONT POINT-SIZE='10'>Screening of title and abstract of the studies with the help of the ASReview tool.<BR/>1113 studies were identified as relevant based on our criteria.<BR/>This number was further reduced by removal of conference abstracts.</FONT>>]
    D [label = <<FONT POINT-SIZE='18'>Data extraction</FONT><BR/><BR/><FONT POINT-SIZE='10'>Data such as EAE scores, animal model, tested drug, etc. were ectracted from 739 selected studies.<BR/>Of these 739 studies, 264 were excluded for reasons such as non-clinically relevant treatment approach, study present in duplicate,<BR/>or the tested drug is not within the group of drugs of interest to this study.</FONT>>]
    E [label = <<FONT POINT-SIZE='18'>Data analysis</FONT><BR/><BR/><FONT POINT-SIZE='10'>Systematic comparison of the 475 preclinical in vivo studies of successful and non-successful drug candidates.<BR/>Statistical modelling to identify disriminatory experimental features in these studies.</FONT>>]
    
    # edges
    A -> B
    B -> C
    C -> D
    D -> E

  }
")

### FIGURE 2 - Species
library(dplyr)

species_freq <- HERMES_INCLUDED %>%
  group_by(Species) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by species frequency
species_freq <- species_freq %>%
  arrange(desc(Frequency))

#create plot showing percentage of overall species, and number of studies using the species
library(ggplot2)
ggplot(species_freq, aes(x = reorder(Species, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Species", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 

library(tidyr)
library(dplyr)
library(stringr)

# Split the Species column into individual species
HERMES_INCLUDED <- HERMES_INCLUDED %>%
  mutate(Species = str_split(Species, ", ")) %>%
  unnest(Species)

# Standardize species names
HERMES_INCLUDED <- HERMES_INCLUDED %>%
  mutate(Species = case_when(
    Species == "monkeys" ~ "monkey",
    TRUE ~ Species
  ))

# Count the frequency and calculate percentage for each species
species_freq <- HERMES_INCLUDED %>%
  group_by(Species) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by species frequency
species_freq <- species_freq %>%
  arrange(desc(Frequency))

# Plot the data
library(ggplot2)
ggplot(species_freq, aes(x = reorder(Species, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Species", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Pool strains < 5: 
# Filter strains that occur only 1-5 times
single_occurrence <- species_freq %>%
  filter(Frequency >= 1 & Frequency <= 5)

# Calculate the percentage for the pooled strains
pooled_frequency <- sum(single_occurrence$Frequency)
pooled_percentage <- (pooled_frequency / sum(species_freq$Frequency)) * 100

# Create a new category for pooled strains
pooled_species <- data.frame(Species = "Other",
                             Frequency = pooled_frequency,
                             Percentage = pooled_percentage)

# Remove the single-occurrence strains from the original data
species_freq_pooled <- species_freq %>%
  filter(Frequency > 5)

# Combine the single-occurrence strains with the modified data
species_freq_pooled <- bind_rows(species_freq_pooled, pooled_species)

# Create the bar plot with percentage labels with pooling
ggplot(species_freq_pooled, aes(x = reorder(Species, -Frequency), y = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5) + 
  labs(x = "Species", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### FIGURE 2 - sex

Sex_freq <- HERMES_INCLUDED %>%
  group_by(Sex) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by frequency
Sex_freq <- Sex_freq %>%
  arrange(desc(Frequency))

# Create the plot showing the percentage of overall sexes and the number of studies
library(ggplot2)
ggplot(Sex_freq, aes(x = reorder(Sex, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Sex", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### FIGURE 2 - Tested drug(s) 
library(ggplot2)
library(tidyr)

# Ensure all columns are of the same type
HERMES_INCLUDED$`Tested drug(s)` <- as.character(HERMES_INCLUDED$Tested.drug.s.)
HERMES_INCLUDED$`Tested drug 2` <- as.character(HERMES_INCLUDED$Tested.drug.2)
HERMES_INCLUDED$`Tested drug 3` <- as.character(HERMES_INCLUDED$Tested.drug.3)

# Gather all tested drugs into one column in an intermediate data frame
HERMES_drugs <- HERMES_INCLUDED %>%
  gather(key = "drug_key", value = "Tested drug(s)", `Tested drug(s)`, `Tested drug 2`, `Tested drug 3`, na.rm = TRUE)

# Count the frequency and calculate percentage for each drug
drug_freq <- HERMES_drugs %>%
  group_by(`Tested drug(s)`) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by frequency
drug_freq <- drug_freq %>%
  arrange(desc(Frequency))

# Create the plot showing the percentage of tested drugs and the number of studies for each drug
ggplot(drug_freq, aes(x = reorder(`Tested drug(s)`, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Tested Drugs", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Pool drugs < 5:
# Filter drugs that occur only 1-5 times
single_occurrence <- drug_freq %>%
  filter(Frequency >= 1 & Frequency <= 5)

# Calculate the percentage for the pooled strains
pooled_frequency <- sum(single_occurrence$Frequency)
pooled_percentage <- (pooled_frequency / sum(drug_freq$Frequency)) * 100

# Remove the single-occurrence strains from the original data
drug_freq_pooled <- drug_freq %>%
  filter(Frequency > 5)

# Create a new category for pooled strains using tibble()
pooled_drugs <- tibble(`Tested drug(s)` = "Other",
                       Frequency = pooled_frequency,
                       Percentage = pooled_percentage)

# Combine the single-occurrence strains with the modified data
drug_freq_pooled <- bind_rows(drug_freq_pooled, pooled_drugs)

# Create the bar plot with percentage labels with pooling
ggplot(drug_freq_pooled, aes(x = reorder(`Tested drug(s)`, -Frequency), y = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5) + 
  labs(x = "Tested Drugs", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### FIGURE 2 - Animal model
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

# Split the 'Animal model' column into individual models
HERMES_INCLUDED <- HERMES_INCLUDED %>%
  mutate(`Animal model` = str_split(`Animal model`, ", ")) %>%
  unnest(`Animal model`)

# Count the frequency and calculate percentage for each animal model
animal_model_freq <- HERMES_INCLUDED %>%
  group_by(`Animal model`) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by frequency
animal_model_freq <- animal_model_freq %>%
  arrange(desc(Frequency))

# Create the plot showing the percentage of animal models and the number of studies for each model
ggplot(animal_model_freq, aes(x = reorder(`Animal model`, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Animal Model", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Pool animal models < 5 
# Filter strains that occur only 1-5 times
single_occurrence <- animal_model_freq %>%
  filter(Frequency >= 1 & Frequency <= 5)

# Calculate the percentage for the pooled animal models
pooled_frequency <- sum(single_occurrence$Frequency)
pooled_percentage <- (pooled_frequency / sum(animal_model_freq$Frequency)) * 100

# Create a new category for pooled animal models
pooled_animal_model <- data.frame(`Animal model` = "Other",
                             Frequency = pooled_frequency,
                             Percentage = pooled_percentage)

# Remove the single-occurrence animal models from the original data
animal_model_freq_pooled <- animal_model_freq %>%
  filter(Frequency > 5)

# Combine the single-occurrence animal models with the modified data
animal_model_freq_pooled <- bind_rows(animal_model_freq_pooled, pooled_animal_model)

# # Create the bar plot with percentage labels with pooling (introduces "NA" instead of "Other" therefore commented out)
# ggplot(animal_model_freq_pooled, aes(x = reorder(`Animal model`, -Frequency), y = Percentage)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = Frequency), vjust = -0.5) + 
#   labs(x = "Animal model", y = "Percentage") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(tidyr)
library(dplyr)
library(stringr)

# Split the 'Animal model' column into individual models
HERMES_INCLUDED <- HERMES_INCLUDED %>%
  mutate(`Animal model` = str_split(`Animal model`, ", ")) %>%
  unnest(`Animal model`)

# Count the frequency and calculate percentage for each animal model
animal_model_freq <- HERMES_INCLUDED %>%
  group_by(`Animal model`) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by frequency
animal_model_freq <- animal_model_freq %>%
  arrange(desc(Frequency))

# Filter animal models that occur only 1-5 times
single_occurrence <- animal_model_freq %>%
  filter(Frequency >= 1 & Frequency <= 5)

# Calculate the percentage for the pooled animal models
pooled_frequency <- sum(single_occurrence$Frequency)
pooled_percentage <- (pooled_frequency / sum(animal_model_freq$Frequency)) * 100

# Create a new category for pooled animal models
pooled_animal_model <- tibble(`Animal model` = "Other",
                              Frequency = pooled_frequency,
                              Percentage = pooled_percentage)

# Remove the single-occurrence animal models from the original data
animal_model_freq_pooled <- animal_model_freq %>%
  filter(Frequency > 5)

# Combine the single-occurrence animal models with the modified data
animal_model_freq_pooled <- bind_rows(animal_model_freq_pooled, pooled_animal_model)

# Create the bar plot with percentage labels with pooling
ggplot(animal_model_freq_pooled, aes(x = reorder(`Animal model`, -Frequency), y = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5) + 
  labs(x = "Animal model", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### FIGURE 2 - Strain
strain_freq <- 
  HERMES_INCLUDED %>%
  group_by(Strain) %>%
  summarise(Frequency = n()) %>%
  mutate(Strain=str_split(Strain, ',')) %>% 
    unnest(Strain) %>% 
    mutate(Strain=trimws(Strain)) %>% 
  ungroup() %>% 
    group_by(Strain) %>%
    summarise(Frequency = sum(Frequency)) %>% 
    # mutate(Strain=ifelse(Frequency<=5,"Other",Strain)) %>%   
    # ungroup() %>% 
    # group_by(Strain) %>%
    # summarise(Frequency = sum(Frequency)) %>% 
    mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Sort the data by strain frequency
strain_freq <- strain_freq %>%
  arrange(desc(Frequency))

# Create the bar plot with percentage labels without pooling
ggplot(strain_freq, aes(x = reorder(Strain, -Frequency), y = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5) + 
  labs(x = "Strain", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# pool strains < 5
# Filter strains that occur only 1-5 times
single_occurrence <- strain_freq %>%
  filter(Frequency >= 1 & Frequency <= 5)

# Calculate the percentage for the pooled strains
pooled_frequency <- sum(single_occurrence$Frequency)
pooled_percentage <- (pooled_frequency / sum(strain_freq$Frequency)) * 100

# Create a new category for pooled strains
pooled_strains <- data.frame(Strain = "Other",
                             Frequency = pooled_frequency,
                             Percentage = pooled_percentage)

# Remove the single-occurrence strains from the original data
strain_freq_pooled <- strain_freq %>%
  filter(Frequency > 5)

# Combine the single-occurrence strains with the modified data
strain_freq_pooled <- bind_rows(strain_freq_pooled, pooled_strains)

# Create the bar plot with percentage labels with pooling
ggplot(strain_freq_pooled, aes(x = reorder(Strain, -Frequency), y = Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5) + 
  labs(x = "Strain", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



####

### FIGURE 2 - PLOT MERGE
common_theme <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.margin = margin(20, 40, 20, 40),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13.5),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) # Increase margin sizes to provide more room for labels

# First plot
p1 <- ggplot(species_freq_pooled, aes(x = reorder(Species, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "#b6b6b6") +
  # geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(x = "Species", y = "Number of\nstudies")+
  ylim(0, 450) +
  common_theme

# Second plot
p2 <- ggplot(Sex_freq, aes(x = reorder(Sex, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "#b6b6b6") +
  labs(x = "Sex", y = "Number of\nstudies") +
  ylim(0, 450) +
  common_theme

drug_freq_pooled<-drug_freq_pooled %>% 
  rename(drugs=1)

# Third plot
p3 <- 
drug_status_df %>% 
  pivot_longer(everything(),names_to="status",values_to="drugs") %>% 
  data.frame() %>% 
  merge(.,data.frame(drug_freq_pooled),by="drugs") %>% 
  mutate(status=ifelse(status=="Approved drugs","Approved drugs","Failed drugs")) %>% 
  mutate(drugs=case_when(
    drugs=="Glatiramer acetate"~"GA",
    drugs=="Dimethyl fumarate"~"DMF",
    drugs=="Peginterferon beta-1a"~"PEG-IFN beta-1a",
    drugs=="Interferon Beta 1"~"IFN beta-1",
    drugs=="Monomethyl fumarate"~"MMF",
    drugs=="Epigallocatechin gallate"~"EGCG",
    TRUE~drugs
  )) %>% 
ggplot(aes(x = reorder(drugs, -Frequency), y = Frequency,fill=status)) +
  geom_bar(stat = "identity") +
  labs(x = "Tested Drugs", y ="Number of\nstudies") +
  # ylim(0, 100) +
  common_theme+
  scale_fill_manual(values=c("Approved drugs"="#6896ca","Failed drugs"="#ff7b74"))+
  theme(legend.title=element_blank())

# Fourth plot
p4 <- ggplot(animal_model_freq_pooled, aes(x = reorder(`Animal model`, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "#b6b6b6") +
  labs(x = "Animal Model", y = "Number of\nstudies") +
  # ylim(0, 100) +
  common_theme

# Fifth plot
p5 <- ggplot(strain_freq_pooled, aes(x = reorder(Strain, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "#b6b6b6") + # Add the fill argument here
  labs(x = "Strain", y = "Number of\nstudies") +
  # ylim(0, 100) +
  common_theme

row1<-grid.arrange(p1+ggtitle("A: Species"), p2+ggtitle("B: Sex"),
                    ncol=2)
row3<-grid.arrange(p4+ggtitle("C: Animal model"), p5+ggtitle("D: Strain"), ncol=2)

grid.arrange(row1, p3+ggtitle("E: Tested Drugs"),row3,ncol=1,heights=c(1,1.5,1)) #adjust x axis label margins 


#needs to be done - fix sizes of the panels
#remove/dot after A.species DONE
#shorten the names whenever possibe /pull strains together
#fix the comma with the strain DONE
#margins on axis labels
#A B C in bold font 


#### TABLE 1
# Subset the HERMES_INCLUDED data frame into approved and dropped-out drugs based on status
# and remove duplicates using distinct()
approved_drugs <- HERMES_INCLUDED %>%
  filter(status == "Marketed") %>%
  select(`Tested drug(s)`) %>%
  distinct() %>%
  rename("Approved drugs" = `Tested drug(s)`)

approved_drugs2 <- HERMES_INCLUDED %>%
  filter(status2 == "Marketed") %>%
  select(`Tested drug 2`) %>%
  distinct() %>%
  rename("Approved drugs" = `Tested drug 2`)

dropped_out_drugs <- HERMES_INCLUDED %>%
  filter(status == "Failed") %>%
  select(`Tested drug(s)`) %>%
  distinct() %>%
  rename("Dropped-out drugs" = `Tested drug(s)`)

dropped_out_drugs2 <- HERMES_INCLUDED %>%
  filter(status2 == "Failed") %>%
  select(`Tested drug 2`) %>%
  distinct() %>%
  rename("Dropped-out drugs" = `Tested drug 2`)

# Concatenate the two sets of approved and dropped-out drugs
approved_drugs_all <- bind_rows(approved_drugs, approved_drugs2) %>% distinct()
dropped_out_drugs_all <- bind_rows(dropped_out_drugs, dropped_out_drugs2) %>% distinct()

# Function to ensure both data frames have the same number of rows for binding
lengthen <- function(df, n) {
  n_add <- n - nrow(df)
  df[nrow(df) + seq_len(n_add),] <- NA
  df
}

# Make both data frames have the same number of rows
n <- max(nrow(approved_drugs_all), nrow(dropped_out_drugs_all))
approved_drugs_all <- lengthen(approved_drugs_all, n)
dropped_out_drugs_all <- lengthen(dropped_out_drugs_all, n)

# Combine the two data frames
drug_status_df <- cbind(approved_drugs_all, dropped_out_drugs_all)

# View the new data frame
print(drug_status_df)

###


### FIGURE 4 - EFFECT size calculation 
# Add a new column "Orig_Row_Num" which stores the original row numbers
HERMES_INCLUDED$Orig_Row_Num <- 1:nrow(HERMES_INCLUDED)

# Now select specific columns from HERMES_INCLUDED and create HERMES_EFFECT_SIZE
# Including the new "Orig_Row_Num" column
HERMES_EFFECT_SIZE <- HERMES_INCLUDED[, c("Orig_Row_Num", "EAE score drug 1", "Variance measure drug 1", "EAE score control", "Variability measure control", "Data type...27", "n animals drug 1", "n animals control", "Tested drug(s)")]

# Select specific columns from HERMES_INCLUDED and create HERMES_EFFECT_SIZE
#HERMES_EFFECT_SIZE <- HERMES_INCLUDED[, c("EAE score drug 1", "Variance measure drug 1", "EAE score control", "Variability measure control", "Data type...27", "n animals drug 1", "n animals control", "Tested drug(s)")]

### add tested drug 2

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Select the second set of columns from HERMES_INCLUDED, rename them to match HERMES_EFFECT_SIZE, and add them to it
HERMES_EFFECT_SIZE2 <- HERMES_INCLUDED %>%
  select(Orig_Row_Num, `EAE score drug 2...67`, `Variance measure drug 2...69`, 
         `EAE score control 2...70`, `Variability measure control 2...72`, 
         `Data type 2`, `n animals drug 2...68`, `n animals control 2...71`, 
         `Tested drug 2`) %>%
  rename(`Orig_Row_Num` = `Orig_Row_Num`,
         `EAE score drug 1` = `EAE score drug 2...67`, 
         `Variance measure drug 1` = `Variance measure drug 2...69`, 
         `EAE score control` = `EAE score control 2...70`, 
         `Variability measure control` = `Variability measure control 2...72`, 
         `Data type...27` = `Data type 2`, 
         `n animals drug 1` = `n animals drug 2...68`, 
         `n animals control` = `n animals control 2...71`, 
         `Tested drug(s)` = `Tested drug 2`)

# Combine the original and new dataframes
HERMES_EFFECT_SIZE <- bind_rows(HERMES_EFFECT_SIZE, HERMES_EFFECT_SIZE2)


# Create new columns
HERMES_EFFECT_SIZE$SD_EAE_score_drug_1 <- NA
HERMES_EFFECT_SIZE$SD_EAE_score_control <- NA

# Filter out rows with "NR" or "NULL" in "n animals drug 1" or "n animals control"
HERMES_EFFECT_SIZE <- HERMES_EFFECT_SIZE[!(HERMES_EFFECT_SIZE$`n animals drug 1` %in% c("NR", "NULL")) & !(HERMES_EFFECT_SIZE$`n animals control` %in% c("NR", "NULL")), ]

# Filter out rows with "NR" in "Data type", "NR" in "Variability measure control", or "NR" in "Variance measure drug 1"
HERMES_EFFECT_SIZE <- HERMES_EFFECT_SIZE[!(HERMES_EFFECT_SIZE$`Data type` %in% c("NR", "mean")) & !(HERMES_EFFECT_SIZE$`Variability measure control` %in% "NR") & !(HERMES_EFFECT_SIZE$`Variance measure drug 1` %in% "NR"), ]

# Filter out rows with "NA" in "Data type"
HERMES_EFFECT_SIZE <- HERMES_EFFECT_SIZE[!is.na(HERMES_EFFECT_SIZE$`Data type`), ]

# Filter out rows with "NA" in "Variability measure control" or "Variance measure drug 1"
HERMES_EFFECT_SIZE <- HERMES_EFFECT_SIZE[!(is.na(HERMES_EFFECT_SIZE$`Variability measure control`) | is.na(HERMES_EFFECT_SIZE$`Variance measure drug 1`)), ]

# Convert columns to numeric
HERMES_EFFECT_SIZE$`Variance measure drug 1` <- as.numeric(HERMES_EFFECT_SIZE$`Variance measure drug 1`)
HERMES_EFFECT_SIZE$`Variability measure control` <- as.numeric(HERMES_EFFECT_SIZE$`Variability measure control`)
HERMES_EFFECT_SIZE$`EAE score drug 1` <- as.numeric(HERMES_EFFECT_SIZE$`EAE score drug 1`)
HERMES_EFFECT_SIZE$`EAE score control` <- as.numeric(HERMES_EFFECT_SIZE$`EAE score control`)
HERMES_EFFECT_SIZE$`n animals drug 1` <- as.numeric(HERMES_EFFECT_SIZE$`n animals drug 1`)
HERMES_EFFECT_SIZE$`n animals control` <- as.numeric(HERMES_EFFECT_SIZE$`n animals control`)

# Fill the new columns based on conditions
for (i in 1:nrow(HERMES_EFFECT_SIZE)) {
  if (HERMES_EFFECT_SIZE[i, "Data type"] == "mean, SEM") {
    HERMES_EFFECT_SIZE[i, "SD_EAE_score_drug_1"] <- HERMES_EFFECT_SIZE[i, "Variance measure drug 1"] * sqrt(HERMES_EFFECT_SIZE[i, "n animals drug 1"])
    HERMES_EFFECT_SIZE[i, "SD_EAE_score_control"] <- HERMES_EFFECT_SIZE[i, "Variability measure control"] * sqrt(HERMES_EFFECT_SIZE[i, "n animals control"])
  } else if (HERMES_EFFECT_SIZE[i, "Data type"] == "mean, SD") {
    HERMES_EFFECT_SIZE[i, "SD_EAE_score_drug_1"] <- HERMES_EFFECT_SIZE[i, "Variance measure drug 1"]
    HERMES_EFFECT_SIZE[i, "SD_EAE_score_control"] <- HERMES_EFFECT_SIZE[i, "Variability measure control"]
  }
}

# Convert the newly filled columns to numeric
HERMES_EFFECT_SIZE$SD_EAE_score_drug_1 <- as.numeric(HERMES_EFFECT_SIZE$SD_EAE_score_drug_1)
HERMES_EFFECT_SIZE$SD_EAE_score_control <- as.numeric(HERMES_EFFECT_SIZE$SD_EAE_score_control)

# Calculate Cohen's d as the effect size
HERMES_EFFECT_SIZE$Effect_size <- (HERMES_EFFECT_SIZE$`EAE score control` - HERMES_EFFECT_SIZE$`EAE score drug 1`) / sqrt((HERMES_EFFECT_SIZE$SD_EAE_score_control^2 + HERMES_EFFECT_SIZE$SD_EAE_score_drug_1^2) / 2)

# Print the updated data frame
print(HERMES_EFFECT_SIZE)

# Filter out rows with "NA" in "Effect_size"
HERMES_EFFECT_SIZE <- HERMES_EFFECT_SIZE[!is.na(HERMES_EFFECT_SIZE$`Effect_size`), ]

### remove the effect size values that are "inf" (it should be possible to do this in a better way?)
# HERMES_EFFECT_SIZE <- HERMES_EFFECT_SIZE[-176, ]

#plot effect size vs tested drugs:
library(ggplot2)

# Plot the effect size by tested drugs
ggplot(HERMES_EFFECT_SIZE, aes(x = `Tested drug(s)`, y = `Effect_size`)) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Tested Drugs", y = "Effect Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

######

## plot effect size of approved drugs on the left side of the plot and the dropped-out drugs on the right side.

# Plot effect size vs tested drugs
ggplot(HERMES_EFFECT_SIZE, aes(x = `Tested drug(s)`, y = Effect_size)) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Tested Drugs", y = "Effect Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Access the Approved drugs and Dropped-out drugs from drug_status_df
approved_drugs <- drug_status_df$`Approved drugs`
dropped_out_drugs <- drug_status_df$`Dropped-out drugs`

# Reorder the levels of the Tested drug(s) factor based on category
HERMES_EFFECT_SIZE$`Tested drug(s)` <- factor(HERMES_EFFECT_SIZE$`Tested drug(s)`,
                                              levels = c(rev(approved_drugs), dropped_out_drugs))

# Adjust the plot to separate approved and dropped-out drugs
HERMES_EFFECT_SIZE_empty<-HERMES_EFFECT_SIZE
HERMES_EFFECT_SIZE_empty[]<-NA
HERMES_EFFECT_SIZE_empty<-head(HERMES_EFFECT_SIZE_empty,2)
HERMES_EFFECT_SIZE_empty$`Tested drug(s)`<-c("dropped_median","approved_median")

#move the summary next to the group + IQ range - then on top of each other, check the drug name, sort them by median ef per drug, diamond for summary 
rbind(HERMES_EFFECT_SIZE,
HERMES_EFFECT_SIZE_empty %>% 
  mutate(Effect_size=
  case_when(
    `Tested drug(s)`=="dropped_median"~median(HERMES_EFFECT_SIZE$Effect_size[HERMES_EFFECT_SIZE$Drug_Status=="Dropped Out"]),
    `Tested drug(s)`=="approved_median"~median(HERMES_EFFECT_SIZE$Effect_size[HERMES_EFFECT_SIZE$Drug_Status=="Approved"])
  ))) %>% 
ggplot(aes(x = `Tested drug(s)`, y = Effect_size)) +
  geom_point(aes(color=Drug_Status),size=3)+
  # geom_point(aes(y=mean_ES_status))+
  labs(x = "Tested Drugs", y = "Effect Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.minor = element_blank())+
    scale_color_manual(values=c("Approved"="#6896ca", "Dropped Out"="#ff7b74"))


#### PLOT MEDIAN EFFECT SIZE 
# Categorize the drugs into approved and dropped out
HERMES_EFFECT_SIZE$Drug_Status <- ifelse(HERMES_EFFECT_SIZE$`Tested drug(s)` %in% approved_drugs, 
                                         "Approved", "Dropped Out")

# Calculate the median effect size, minimum and maximum for each category
category_medians <- HERMES_EFFECT_SIZE %>%
  group_by(Drug_Status) %>%
  summarise(Median_Effect_Size = median(Effect_size, na.rm = TRUE),
            Min_Effect_Size = min(Effect_size, na.rm = TRUE),
            Max_Effect_Size = max(Effect_size, na.rm = TRUE),
            .groups = "drop")

# Print the calculated values
print(category_medians)

# Plot the effect size by tested drugs
ggplot(HERMES_EFFECT_SIZE, aes(x = Drug_Status, y = Effect_size)) +
  # Individual data points
  geom_point(aes(color = Drug_Status), size = 3) +
  scale_color_manual(values = c("Approved" = "steelblue", "Dropped Out" = "red")) +
  # Medians
  geom_point(data = category_medians, aes(x = Drug_Status, y = Median_Effect_Size), color = "black", size = 5) +
  labs(x = "Drug Status", y = "Effect Size", color = "Drug Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### statistical analysis of effect size:

# Calculate the median effect size for each individual drug
individual_drug_medians <- HERMES_EFFECT_SIZE %>%
  group_by(`Tested drug(s)`, Drug_Status) %>%
  summarise(Median_Effect_Size_Drug = median(Effect_size, na.rm = TRUE),
            .groups = "drop")

# Print the calculated values for each drug
print(individual_drug_medians)

#wilcoxon test:
# Separate the medians into two groups
approved_medians <- individual_drug_medians %>% 
  filter(Drug_Status == "Approved") %>%
  pull(Median_Effect_Size_Drug)

dropped_out_medians <- individual_drug_medians %>% 
  filter(Drug_Status == "Dropped Out") %>%
  pull(Median_Effect_Size_Drug)

# Perform the Wilcoxon test
wilcox_test_result <- wilcox.test(approved_medians, dropped_out_medians)

# Print the result
print(wilcox_test_result)


#####
### FIGURE 4 - MRI effect size
### EFFECT size calculation (MRI)
# Add a new column "Orig_Row_Num" which stores the original row numbers
#HERMES_INCLUDED$Orig_Row_Num <- 1:nrow(HERMES_INCLUDED)

# Now select specific columns from HERMES_INCLUDED and create HERMES_EFFECT_SIZE_MRI
# Including the new "Orig_Row_Num" column
HERMES_EFFECT_SIZE_MRI <- HERMES_INCLUDED[, c("Orig_Row_Num", "MRI drug 1", "Variability measure...30", "MRI control 1", "Variance measure...33", "Data type...34", "n MRI 1", "n animals 1", "Tested drug(s)")]


##### trial to add tested drug 2

# Load necessary libraries
library(dplyr)
library(ggplot2)


# Select the second set of columns from HERMES_INCLUDED, rename them to match HERMES_EFFECT_SIZE_MRI, and add them to it
HERMES_EFFECT_SIZE_MRI_2 <- HERMES_INCLUDED %>%
  select(Orig_Row_Num, `MRI drug 2`, `Variability measure...87`, 
         `MRI control 2`, `Variance measure...90`, 
         `Data type...91`, `n animals MRI 2`, `n animals 2`, `Tested drug(s)`) %>%
  rename(`Orig_Row_Num` = `Orig_Row_Num`,
         `MRI drug 1` = `MRI drug 2`, 
         `Variability measure...30` = `Variability measure...87`, 
         `MRI control 1` = `MRI control 2`, 
         `Variance measure...33` = `Variance measure...90`, 
         `Data type...34` = `Data type...91`, 
         `n MRI 1` = `n animals MRI 2`, 
         `n animals 1` = `n animals 2`,
         `Tested drug(s)` = `Tested drug(s)`)

# Convert columns to numeric
HERMES_EFFECT_SIZE_MRI$`Variability measure...30` <- as.numeric(HERMES_EFFECT_SIZE_MRI$`Variability measure...30`)
HERMES_EFFECT_SIZE_MRI$`Variance measure...33` <- as.numeric(HERMES_EFFECT_SIZE_MRI$`Variance measure...33`)
HERMES_EFFECT_SIZE_MRI$`MRI drug 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI$`MRI drug 1`)
HERMES_EFFECT_SIZE_MRI$`MRI control 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI$`MRI control 1`)
HERMES_EFFECT_SIZE_MRI$`n MRI 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI$`n MRI 1`)
HERMES_EFFECT_SIZE_MRI$`n animals 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI$`n animals 1`)
# Convert columns to numeric
HERMES_EFFECT_SIZE_MRI_2$`MRI drug 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`MRI drug 1`)
HERMES_EFFECT_SIZE_MRI_2$`Variability measure...30` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`Variability measure...30`)
HERMES_EFFECT_SIZE_MRI_2$`Variance measure...33` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`Variance measure...33`)
HERMES_EFFECT_SIZE_MRI_2$`MRI control 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`MRI control 1`)
#HERMES_EFFECT_SIZE_MRI_2$`n MRI 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`n MRI 1`)
#HERMES_EFFECT_SIZE_MRI_2$`n animals 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`n animals 1`)


###  
# replace NULLs with NA in 'n MRI 1' and 'n animals 1'
HERMES_EFFECT_SIZE_MRI_2$`n MRI 1` <- lapply(HERMES_EFFECT_SIZE_MRI_2$`n MRI 1`, function(x) if(is.null(x)) NA else x)
HERMES_EFFECT_SIZE_MRI_2$`n animals 1` <- lapply(HERMES_EFFECT_SIZE_MRI_2$`n animals 1`, function(x) if(is.null(x)) NA else x)

# Now that all NULLs are replaced with NAs, convert the list columns to numeric
HERMES_EFFECT_SIZE_MRI_2$`n MRI 1` <- unlist(HERMES_EFFECT_SIZE_MRI_2$`n MRI 1`)
HERMES_EFFECT_SIZE_MRI_2$`n animals 1` <- unlist(HERMES_EFFECT_SIZE_MRI_2$`n animals 1`)

# Try to convert to numeric again
HERMES_EFFECT_SIZE_MRI_2$`n MRI 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`n MRI 1`)
HERMES_EFFECT_SIZE_MRI_2$`n animals 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_2$`n animals 1`)

# Filter out rows with "NR" or "NULL" in "n MRI 1" or "n animals 1"
HERMES_EFFECT_SIZE_MRI <- HERMES_EFFECT_SIZE_MRI[!(HERMES_EFFECT_SIZE_MRI$`n MRI 1` %in% c("NR", "NULL")) & !(HERMES_EFFECT_SIZE_MRI$`n animals 1` %in% c("NR", "NULL")), ]

# Filter out rows with "NR" in "Data type", "NR" in "Variability measure control", or "NR" in "Variance measure drug 1"
HERMES_EFFECT_SIZE_MRI <- HERMES_EFFECT_SIZE_MRI[!(HERMES_EFFECT_SIZE_MRI$`Data type...34` %in% c("NR", "mean")) & !(HERMES_EFFECT_SIZE_MRI$`Variance measure...33` %in% "NR") & !(HERMES_EFFECT_SIZE_MRI$`Variability measure...30` %in% "NR"), ]

# Filter out rows with "NA" in "Data type"
HERMES_EFFECT_SIZE_MRI <- HERMES_EFFECT_SIZE_MRI[!is.na(HERMES_EFFECT_SIZE_MRI$`Data type...34`), ]

# Filter out rows with "NA" in "Variability measure control" or "Variance measure drug 1"
HERMES_EFFECT_SIZE_MRI <- HERMES_EFFECT_SIZE_MRI[!(is.na(HERMES_EFFECT_SIZE_MRI$`Variance measure...33`) | is.na(HERMES_EFFECT_SIZE_MRI$`Variability measure...30`)), ]

# Filter out rows with "NR" or "NULL" in "n MRI 1" or "n animals 1"
HERMES_EFFECT_SIZE_MRI_2 <- HERMES_EFFECT_SIZE_MRI_2[!(HERMES_EFFECT_SIZE_MRI_2$`n MRI 1` %in% c("NR", "NULL")) & !(HERMES_EFFECT_SIZE_MRI_2$`n animals 1` %in% c("NR", "NULL")), ]

# Filter out rows with "NR" in "Data type", "NR" in "Variability measure control", or "NR" in "Variance measure drug 1"
HERMES_EFFECT_SIZE_MRI_2 <- HERMES_EFFECT_SIZE_MRI_2[!(HERMES_EFFECT_SIZE_MRI_2$`Data type...34` %in% c("NR", "mean")) & !(HERMES_EFFECT_SIZE_MRI_2$`Variance measure...33` %in% "NR") & !(HERMES_EFFECT_SIZE_MRI_2$`Variability measure...30` %in% "NR"), ]

# Filter out rows with "NA" in "Data type"
HERMES_EFFECT_SIZE_MRI_2 <- HERMES_EFFECT_SIZE_MRI_2[!is.na(HERMES_EFFECT_SIZE_MRI_2$`Data type...34`), ]

# Filter out rows with "NA" in "Variability measure control" or "Variance measure drug 1"
HERMES_EFFECT_SIZE_MRI_2 <- HERMES_EFFECT_SIZE_MRI_2[!(is.na(HERMES_EFFECT_SIZE_MRI_2$`Variance measure...33`) | is.na(HERMES_EFFECT_SIZE_MRI_2$`Variability measure...30`)), ]
###

### trial to add tested drug 3
# Select the third set of columns from HERMES_INCLUDED, rename them to match HERMES_EFFECT_SIZE_MRI, and add them to it
HERMES_EFFECT_SIZE_MRI_3 <- HERMES_INCLUDED %>%
  select(Orig_Row_Num, `MRI drug 3`, `Variability measure...96`, 
         `MRI control 3`, `Variance measure...99`, 
         `Data type...100`, `n animals MRI 3`, `n animals 3`, `Tested drug(s)`) %>%
  rename(`Orig_Row_Num` = `Orig_Row_Num`,
         `MRI drug 1` = `MRI drug 3`, 
         `Variability measure...30` = `Variability measure...96`, 
         `MRI control 1` = `MRI control 3`, 
         `Variance measure...33` = `Variance measure...99`, 
         `Data type...34` = `Data type...100`, 
         `n MRI 1` = `n animals MRI 3`, 
         `n animals 1` = `n animals 3`,
         `Tested drug(s)` = `Tested drug(s)`)

# Convert columns to numeric
HERMES_EFFECT_SIZE_MRI_3$`Variability measure...30` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`Variability measure...30`)
HERMES_EFFECT_SIZE_MRI_3$`Variance measure...33` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`Variance measure...33`)
HERMES_EFFECT_SIZE_MRI_3$`MRI drug 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`MRI drug 1`)
HERMES_EFFECT_SIZE_MRI_3$`MRI control 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`MRI control 1`)
HERMES_EFFECT_SIZE_MRI_3$`n MRI 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`n MRI 1`)
HERMES_EFFECT_SIZE_MRI_3$`n animals 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`n animals 1`)

# Filter and clean your data (as you did previously for the other data sets)
# replace NULLs with NA in 'n MRI 1' and 'n animals 1'
HERMES_EFFECT_SIZE_MRI_3$`n MRI 1` <- lapply(HERMES_EFFECT_SIZE_MRI_3$`n MRI 1`, function(x) if(is.null(x)) NA else x)
HERMES_EFFECT_SIZE_MRI_3$`n animals 1` <- lapply(HERMES_EFFECT_SIZE_MRI_3$`n animals 1`, function(x) if(is.null(x)) NA else x)

# Now that all NULLs are replaced with NAs, convert the list columns to numeric
HERMES_EFFECT_SIZE_MRI_3$`n MRI 1` <- unlist(HERMES_EFFECT_SIZE_MRI_3$`n MRI 1`)
HERMES_EFFECT_SIZE_MRI_3$`n animals 1` <- unlist(HERMES_EFFECT_SIZE_MRI_3$`n animals 1`)

# Try to convert to numeric again
HERMES_EFFECT_SIZE_MRI_3$`n MRI 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`n MRI 1`)
HERMES_EFFECT_SIZE_MRI_3$`n animals 1` <- as.numeric(HERMES_EFFECT_SIZE_MRI_3$`n animals 1`)

# Filter out rows with "NR" or "NULL" in "n MRI 1" or "n animals 1"
HERMES_EFFECT_SIZE_MRI_3 <- HERMES_EFFECT_SIZE_MRI_3[!(HERMES_EFFECT_SIZE_MRI_3$`n MRI 1` %in% c("NR", "NULL")) & !(HERMES_EFFECT_SIZE_MRI_3$`n animals 1` %in% c("NR", "NULL")), ]

# Filter out rows with "NR" in "Data type", "NR" in "Variability measure control", or "NR" in "Variance measure drug 1"
HERMES_EFFECT_SIZE_MRI_3 <- HERMES_EFFECT_SIZE_MRI_3[!(HERMES_EFFECT_SIZE_MRI_3$`Data type...34` %in% c("NR", "mean")) & !(HERMES_EFFECT_SIZE_MRI_3$`Variance measure...33` %in% "NR") & !(HERMES_EFFECT_SIZE_MRI_3$`Variability measure...30` %in% "NR"), ]

# Filter out rows with "NA" in "Data type"
HERMES_EFFECT_SIZE_MRI_3 <- HERMES_EFFECT_SIZE_MRI_3[!is.na(HERMES_EFFECT_SIZE_MRI_3$`Data type...34`), ]

# Filter out rows with "NA" in "Variability measure control" or "Variance measure drug 1"
HERMES_EFFECT_SIZE_MRI_3 <- HERMES_EFFECT_SIZE_MRI_3[!(is.na(HERMES_EFFECT_SIZE_MRI_3$`Variance measure...33`) | is.na(HERMES_EFFECT_SIZE_MRI_3$`Variability measure...30`)), ]


# Combine the original and new dataframes
HERMES_EFFECT_SIZE_MRI <- bind_rows(HERMES_EFFECT_SIZE_MRI, HERMES_EFFECT_SIZE_MRI_2, HERMES_EFFECT_SIZE_MRI_3)

####

# Create new columns
HERMES_EFFECT_SIZE_MRI$SD_MRI_score_drug <- NA
HERMES_EFFECT_SIZE_MRI$SD_MRI_score_control <- NA

# Fill the new columns based on conditions
for (i in 1:nrow(HERMES_EFFECT_SIZE_MRI)) {
  if (HERMES_EFFECT_SIZE_MRI[i, "Data type"] == "mean, SEM") {
    HERMES_EFFECT_SIZE_MRI[i, "SD_MRI_score_drug"] <- HERMES_EFFECT_SIZE_MRI[i, "Variability measure"] * sqrt(HERMES_EFFECT_SIZE_MRI[i, "n MRI 1"])
    HERMES_EFFECT_SIZE_MRI[i, "SD_MRI_score_control"] <- HERMES_EFFECT_SIZE_MRI[i, "Variance measure"] * sqrt(HERMES_EFFECT_SIZE_MRI[i, "n animals 1"])
  } else if (HERMES_EFFECT_SIZE_MRI[i, "Data type"] == "mean, SD") {
    HERMES_EFFECT_SIZE_MRI[i, "SD_MRI_score_drug"] <- HERMES_EFFECT_SIZE_MRI[i, "Variability measure"]
    HERMES_EFFECT_SIZE_MRI[i, "SD_MRI_score_control"] <- HERMES_EFFECT_SIZE_MRI[i, "Variance measure"]
  }
}

# Convert the newly filled columns to numeric
HERMES_EFFECT_SIZE_MRI$SD_MRI_score_drug <- as.numeric(HERMES_EFFECT_SIZE_MRI$SD_MRI_score_drug)
HERMES_EFFECT_SIZE_MRI$SD_MRI_score_control <- as.numeric(HERMES_EFFECT_SIZE_MRI$SD_MRI_score_control)

# Calculate Cohen's d as the effect size
HERMES_EFFECT_SIZE_MRI$Effect_size_MRI <- (HERMES_EFFECT_SIZE_MRI$`SD_MRI_score_control` - HERMES_EFFECT_SIZE_MRI$`SD_MRI_score_drug`) / sqrt((HERMES_EFFECT_SIZE_MRI$SD_MRI_score_control^2 + HERMES_EFFECT_SIZE_MRI$SD_MRI_score_drug^2) / 2)

# Print the updated data frame
print(HERMES_EFFECT_SIZE_MRI)

# Filter out rows with "NA" in "Effect_size"
HERMES_EFFECT_SIZE_MRI <- HERMES_EFFECT_SIZE_MRI[!is.na(HERMES_EFFECT_SIZE_MRI$`Effect_size_MRI`), ]

#plot effect size vs tested drugs:
library(ggplot2)

# Plot the effect size by tested drugs
ggplot(HERMES_EFFECT_SIZE_MRI, aes(x = `Tested drug(s)`, y = `Effect_size_MRI`)) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Tested Drugs", y = "MRI Effect Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

######


## plot effect size of approved drugs on the left side of the plot and the dropped-out drugs on the right side.

# Plot effect size vs tested drugs
ggplot(HERMES_EFFECT_SIZE_MRI, aes(x = `Tested drug(s)`, y = Effect_size_MRI)) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Tested Drugs", y = "MRI Effect Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Access the Approved drugs and Dropped-out drugs from drug_status_df
approved_drugs <- drug_status_df$`Approved drugs`
dropped_out_drugs <- drug_status_df$`Dropped-out drugs`

# Reorder the levels of the Tested drug(s) factor based on category
HERMES_EFFECT_SIZE_MRI$`Tested drug(s)` <- factor(HERMES_EFFECT_SIZE_MRI$`Tested drug(s)`,
                                                  levels = c(rev(approved_drugs), dropped_out_drugs))

# Adjust the plot to separate approved and dropped-out drugs
ggplot(HERMES_EFFECT_SIZE_MRI, aes(x = `Tested drug(s)`, y = Effect_size_MRI)) +
  geom_point(color = "steelblue", size = 3) +
  geom_point(data = subset(HERMES_EFFECT_SIZE_MRI, `Tested drug(s)` %in% dropped_out_drugs),
             color = "red", size = 3) +
  labs(x = "Tested Drugs", y = "MRI Effect Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### PLOT MEDIAN EFFECT SIZE 
# Categorize the drugs into approved and dropped out
HERMES_EFFECT_SIZE_MRI$Drug_Status <- ifelse(HERMES_EFFECT_SIZE_MRI$`Tested drug(s)` %in% approved_drugs, 
                                             "Approved", "Dropped Out")

# Calculate the median effect size, minimum and maximum for each category
category_medians <- HERMES_EFFECT_SIZE_MRI %>%
  group_by(Drug_Status) %>%
  summarise(Median_Effect_Size_MRI = median(Effect_size_MRI, na.rm = TRUE),
            Min_Effect_Size_MRI = min(Effect_size_MRI, na.rm = TRUE),
            Max_Effect_Size_MRI = max(Effect_size_MRI, na.rm = TRUE),
            .groups = "drop")

# Print the calculated values
print(category_medians)

# Plot the effect size by tested drugs
ggplot(HERMES_EFFECT_SIZE_MRI, aes(x = Drug_Status, y = Effect_size_MRI)) +
  # Individual data points
  geom_point(aes(color = Drug_Status), size = 3) +
  scale_color_manual(values = c("Approved" = "steelblue", "Dropped Out" = "red")) +
  # Medians
  geom_point(data = category_medians, aes(x = Drug_Status, y = Median_Effect_Size_MRI), color = "black", size = 5) +
  labs(x = "Drug Status", y = "MRI Effect Size", color = "Drug Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### statistical analysis of effect size:

# Calculate the median effect size for each individual drug
individual_drug_medians <- HERMES_EFFECT_SIZE_MRI %>%
  group_by(`Tested drug(s)`, Drug_Status) %>%
  summarise(Median_Effect_Size_Drug_MRI = median(Effect_size_MRI, na.rm = TRUE),
            .groups = "drop")

# Print the calculated values for each drug
print(individual_drug_medians)

#wilcoxon test:
# Separate the medians into two groups
approved_medians <- individual_drug_medians %>% 
  filter(Drug_Status == "Approved") %>%
  pull(Median_Effect_Size_Drug_MRI)

dropped_out_medians <- individual_drug_medians %>% 
  filter(Drug_Status == "Dropped Out") %>%
  pull(Median_Effect_Size_Drug_MRI)

# Perform the Wilcoxon test
wilcox_test_result <- wilcox.test(approved_medians, dropped_out_medians)

# Print the result
print(wilcox_test_result)

###


### FIGURE 5 - DRUG CV 

# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Convert the dataframe from wide to long format
HERMES_INCLUDED_long <- HERMES_INCLUDED %>%
  pivot_longer(cols = starts_with("Tested drug"),
               names_to = "Drug",
               values_to = "Drug name")

# Remove rows where the drug name is NA
HERMES_INCLUDED_long <- HERMES_INCLUDED_long %>% 
  filter(!is.na(`Drug name`))

# Create the plot
# ggplot(HERMES_INCLUDED_long, aes(x = Year, y = `Drug name`, color = `Drug name`)) +
#   geom_point(color = "red") +
#   theme_minimal() +
#   labs(title = "Publication timeline for each drug",
#        x = "Year",
#        y = "Drug name") +
#   scale_color_discrete(guide = FALSE)  # Turn off the color legend

#Add blue dots for first in human trial
# Filter the Drug_CV_Data dataframe
Drug_CV_Data_filtered <- Drug_CV_Data %>%
  filter(!is.na(`First in human trial`))

# Filter for drugs in human trials
Drug_CV_Data_filtered_approved <- filter(Drug_CV_Data_filtered, `Name` %in% drug_status_df$`Approved drugs`) #Filter for drugs that are approved
Drug_CV_Data_filtered_notapproved <- filter(Drug_CV_Data_filtered, `Name` %in% drug_status_df$`Dropped-out drugs`) #Filter for drugs that are NOT approved

# Filter for drugs NOT in human trials
HERMES_INCLUDED_long_approved <- filter(HERMES_INCLUDED_long, `Drug name` %in% drug_status_df$`Approved drugs`) # Filter for drugs that are approved
HERMES_INCLUDED_long_notapproved <- filter(HERMES_INCLUDED_long, `Drug name` %in% drug_status_df$`Dropped-out drugs`) # Filter for drugs that are NOT approved

## change characters in year to numeric

HERMES_INCLUDED_long_approved$Year = as.numeric(as.character(HERMES_INCLUDED_long_approved$Year)) 
Drug_CV_Data_filtered_approved$`First in human trial` =as.numeric(as.character(Drug_CV_Data_filtered_approved$`First in human trial`))

HERMES_INCLUDED_long_notapproved$Year = as.numeric(as.character(HERMES_INCLUDED_long_notapproved$Year)) 
Drug_CV_Data_filtered_notapproved$`First in human trial` =as.numeric(as.character(Drug_CV_Data_filtered_notapproved$`First in human trial`))

## generate size matrix

#HERMES_INCLUDED_long_approved$Count <- 

# Create the plot for approved drugs

rbind(data.frame(
  HERMES_INCLUDED_long_approved$Year,
  HERMES_INCLUDED_long_approved$`Drug name`
) %>% data.frame() %>% 
  rename(`Drug name`=2,Year=1) %>% 
  mutate(item="animal"),

data.frame(
  Drug_CV_Data_filtered_approved$Name,
  Drug_CV_Data_filtered_approved$`First in human trial`
) %>% 
  rename(`Drug name`=1,Year=2) %>% 
  mutate(item="human")) %>% 
ggplot() +
  geom_beeswarm(aes(x=Year,y=`Drug name`,fill=item),cex=0.6,alpha=0.9,size=4.5,shape=21,color="gray40")+
  theme_minimal() +
  labs(title = "Publication timeline for each drug (approved)",
       x = "Year",
       y = "Drug name") +
  # xlim(1977,2023) +
  scale_fill_manual(values=c("animal"="#e4eae1","human"="#4c9734"))+
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=15)
        )









# Create the plot for not approved drugs
ggplot() +
  geom_point(data = HERMES_INCLUDED_long_notapproved, aes(x = `Year`, y = `Drug name`), color = "red") +
  geom_point(data = Drug_CV_Data_filtered_notapproved, aes(x = `First in human trial`, y = Name), color = "blue") +
  theme_minimal() +
  labs(title = "Publication timeline for each drug (NOT approved)",
       x = "Year",
       y = "Drug name") +
  xlim(1977,2023) +
  scale_color_discrete(guide = FALSE)  # Turn off the color legend

######



### some additional analysis, not for figures:

#calculating Total number of animals

HERMES_INCLUDED$Outcome <- lapply(HERMES_INCLUDED$Outcome, as.numeric)
#HERMES_INCLUDED$`Total number of animals`<-as.numeric(unlist(HERMES_INCLUDED$`Total number of animals`))
HERMES_INCLUDED$`Total number of animals` <- lapply(HERMES_INCLUDED$`Total number of animals`, as.numeric)
tot_reportedanimals<-sum(as.numeric(HERMES_INCLUDED$`Total number of animals`), na.rm=TRUE)
tot_na<-sum(is.na(HERMES_INCLUDED$`Total number of animals`))
animalperstudy<- (tot_reportedanimals/(length(HERMES_INCLUDED$`Total number of animals`)-tot_na))
#print(animalperstudy)
extrapolation_animal<- animalperstudy * (length(HERMES_INCLUDED$`Total number of animals`))
print(extrapolation_animal)


### export HERMES_INCLUDED as CSV
HERMES_INCLUDED <- apply(HERMES_INCLUDED,2,as.character)
write.csv(HERMES_INCLUDED, "/Users/ingridberg/Desktop/UZH Masterstudium/masterarbeit/Data Extraction/R skripte//HERMES_INCLUDED_CSV.csv", row.names=TRUE)
