######################################################################
##                                                                  ##                                  
##                      Prostate embolization study                 ##
##                to compare Wide screen vs. small screen           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud
# date : 02/17/2020

###################### set-up section ################################

# Set the projet path to the root level -
root.dir = rprojroot::find_rstudio_root_file()

# # load readxl package to read easyly Excel file with an install conditon
# if(!require(readxl)){
#   install.packages("readxl")
#   library(readxl)
# }

# load lubridate package to determine patient age from birthdate with an install conditon
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

# load doMC package for parallel computing
if(!require(doMC)){
  install.packages("doMC")
  library(doMC)
}
# load foreach package for parallel computing on for loop
if(!require(foreach)){
  install.packages("foreach")
  library(foreach)
}
# load doparallel package to determine patient age from birthdate with an install conditon
# if(!require(doParallel)){
#   install.packages("doParallel")
#   library(doParallel)
# }
# load tictoc package to measure running time of R code
if(!require(tictoc)){
  install.packages("tictoc")
  library(tictoc)
}
# load excel package to write results in Excel file
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}
# load dplyr for data handling
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
# load prettyR package for better statistical analysis
if(!require(prettyR)){
  install.packages("prettyR")
  library(prettyR)
}
# # load sulmarytoolspackage for better table output
# if(!require(summarytools)){
#   install.packages("summarytools")
#   library(summarytools)
# }


############################### data import section ##################################
# read the database with data frame existing test
# my_all_data <- read_excel("data/CV-IR_Tenon_Radiologie_detailed_data_export_tronque.xlsx")

## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# unfortunetely Excel importation yield to parse errors in date data for instance.
# SO TO AVIOD THIS PROBLEM THE USER HAVE TO IMPORT DATA IN CSV FORMAT 
# BY CONVERTING ORIGINAL DATA FROM .XLSX TO .CSV IN EXCEL SOFTWARE
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

tic("to import detailled data in Rstudio")
#my_all_data <- read.csv2("data/CV-IR_Tenon_Radiologie_detailed_data_export_tronque.csv", sep = ";")
if(exists("my_all_data")){
  print("raw data importation have already done")
}else{
  my_all_data <- read.csv2("data/CV-IR_Tenon_Radiologie_detailed_data_export.csv", sep = ";")
}
toc()

################################## data tayloring section ##################################
# Collums of interest selection
selection = c("Study.date..YYYY.MM.DD.", "Accession.number",
              "Patient.ID", "Patient.birthdate..YYYY.MM.DD.",
              "Patient.weight..kg.", "Patient.size..cm.",
              "BMI", "Standard.study.description", "Performing.physician.last.name",
              "Peak.Skin.Dose..mGy.",
              "Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.",
              "Total.Acquisition.DAP..mGy.cm..","Total.Fluoro.DAP..mGy.cm..",
              "Total.Air.Kerma..mGy.",
              "Total.Acquisition.Air.Kerma..mGy.", "Total.Fluoro.Air.Kerma..mGy.",
              "Total.Time.of.Fluoroscopy..s.", "Number.of.Acquisition.Series",
              "Irradiation.Event.Type","Proprietary.Type", "Dose.Preference",
              "Justification.Code","Raised.alerts.",
              "Positioner.Primary.Angle..deg.", "Positioner.Secondary.Angle..deg.",
              "Field.of.View..cm.")

# data filter to keep only interested collums for this study
Study_data <- my_all_data[,selection]


####### CBCT +/- and large display LD+/- labelization  ###########################

#  add new levels for Irradiation.Event.Type as CBCT+/-
levels(Study_data$Irradiation.Event.Type) <- c(levels(Study_data$Irradiation.Event.Type), "CBCT+","CBCT-")
# rename factors for none CBCT acqusition
Study_data$Irradiation.Event.Type[Study_data$Irradiation.Event.Type == 'FLUOROSCOPY'] <- 'CBCT-'
Study_data$Irradiation.Event.Type[Study_data$Irradiation.Event.Type == 'STATIONARY_ACQUISITION'] <- 'CBCT-'
Study_data$Irradiation.Event.Type[Study_data$Irradiation.Event.Type == 'STEPPING_ACQUISITION'] <- 'CBCT-'
# rename factor for CBCT acqusition
Study_data$Irradiation.Event.Type[Study_data$Irradiation.Event.Type == 'ROTATIONAL_ACQUISITION'] <- 'CBCT+'

#  create new levels as LD+/-
print("Large display have been installed since 2019-06-27")
LD <- Study_data$Study.date..YYYY.MM.DD.
LD <- gsub("[: -]", "" , LD, perl=TRUE) # delete ":" , " ", and "-" from the date value
LD <- as.numeric(LD) # convert LD in numeric format to apply cut function
LD <- cut(LD,c(20180101,20190627, Inf),c("LD-","LD+")) # cut to segemnt date between LD+ and LD- 
LD <- factor(as.character(LD),levels=c("LD-","LD+")) # convert factor in character
Study_data$LD <- LD # add LD factor to the data frame



######################## age patient computation #################################

#  instance null vector with appropriate dimension to bind with Study_data
Patient.Age <- rep(0, nrow(Study_data))

# Loop with parallelization to calculate patient age in years 
# and add this information to Study_data dataframe
# also have a condition to test global environement objet for debugging
tic("for loop with parallelization")
if(exists("Study_data_age")){
  print("patient age computation have already done")
}else{
  cores <- detectCores()
  registerDoMC(cores - 1)
  Patient.Age <- foreach(i = 1:nrow(Study_data)) %dopar% {
    naiss = ymd_hms(Study_data[i,4])
    evt = as.POSIXct(Study_data[i,1])
    age = as.period(interval(naiss, evt))@year
    Patient.Age <- age
  }
}
toc()

Patient.Age <- as.character(Patient.Age)
Study_data_age <-cbind(Study_data,Patient.Age)

############### clinical indication selection and data control #################

Study_data_prostate <- subset(Study_data_age, Standard.study.description == "EMBOLISATION PROSTATIQUE")

# patient and exam number control
patient_number <- length(unique(Study_data_prostate[,"Patient.ID"]))
exam_number <- length(unique(Study_data_prostate[,"Accession.number"]))
# patient ID control with Mathias list to verify if selectde patients are identical than matthias' list
patient_list_matthias <- read.csv2("data/Liste_patient_matthias.csv", sep = ";")
patient_list_matthias_DW <- as.data.frame(patient_list_matthias[,2])
colnames(patient_list_matthias_DW) <- 'Patient.ID.Matthias'
patient_list_matthias <- as.array(patient_list_matthias[,2])
patient_list <- as.array(unique(Study_data_prostate[,"Patient.ID"]))

patient_number_verified <- length(intersect(patient_list, patient_list_matthias))

patient_verified <- intersect(patient_list, patient_list_matthias)
patient_comparison <- as.data.frame(patient_verified)

# loop to harmonize nrow dimension for each column between patient_comparison and patient_list_matthias
loop_end <- abs(nrow(patient_list_matthias) - nrow(patient_comparison))
loop_lenght <- c(1:loop_end)
for (i in loop_lenght) {
  patient_comparison <- rbind(patient_comparison, c(0))
}
# add patient_list_matthias values to patient_comparison
patient_comparison$patient_list_matthias <- patient_list_matthias

# loop to harmonize nrow dimension for each column between patient_comparison and patient_list
loop_end <- abs(nrow(patient_list) - nrow(patient_comparison))
loop_lenght <- c(1:loop_end)
for (i in loop_lenght) {
  patient_comparison <- rbind(patient_comparison, c(0))
}
# add patient_list values to patient_comparison
patient_comparison$patient_list <- patient_list
# write excel file to verify by other user the collected data
write.xlsx(patient_comparison, 'data/comparaison_liste_patient.xlsx', sheetName = "Comparaison_ID_patient", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

# patient_list_cat %>%
#   distinct(Patient.ID.Script, Patient.ID.Matthias, .keep_all = TRUE)
# duplicated(patient_list_cat[,1:2])

##########################################################################
##########################################################################
##########################################################################
################ global statistical analysis #############################
##########################################################################
##########################################################################
##########################################################################

############### main statistical analysis #################
# global stat for unique value by exam
Study_data_prostate_stat <- cbind(Study_data_prostate[,2:3],Study_data_prostate[,5:7], Study_data_prostate[,9:27], Study_data_prostate[,28]) # to select row of interest for statistical computing
colnames(Study_data_prostate_stat) <- c("Accession.Number","Patient.ID", "Patient.weight", "Patient.size", "BMI", "Performing.physician.last.name",
                                        "Peak.Skin.Dose", "Image.and.Fluoroscopy.DAP", "Total.Acquisition.DAP", "Total.Fluoro.DAP",
                                        "Total.Air.Kerma", "Total.Acquisition.Air.Kerma", "Total.Fluoro.Air.Kerma",
                                        "Total.Time.of.Fluoroscopy", "Number.of.Acquisition.Series","Irradiation.Event.Type",
                                        "Proprietary.Type","Dose.Preference","Justification.Code", "Raised.alerts",
                                        "Positioner.Primary.Angle", "Positioner.Secondary.Angle", "Field.of.View","LD", "Patient.Age") # to rename correctly patient age column
write.csv(Study_data_prostate_stat, "output/Study_data_prostate_stat.csv")
write.xlsx(Study_data_prostate_stat, "output/Study_data_prostate_stat.xlsx")

#Study_data_prostate_stat_unique <- cbind(Study_data_prostate_stat[,1:14], Study_data_prostate_stat[,23:24]) # to select column with unique value per exam
Study_data_prostate_stat_unique <- Study_data_prostate_stat %>% select(Accession.Number, Patient.ID, Patient.weight, Patient.size, BMI,
                                                                       Performing.physician.last.name, Peak.Skin.Dose,
                                                                       Image.and.Fluoroscopy.DAP, Total.Acquisition.DAP,
                                                                       Total.Fluoro.DAP, Total.Air.Kerma, Total.Acquisition.Air.Kerma,
                                                                       Total.Fluoro.Air.Kerma, Total.Time.of.Fluoroscopy, Number.of.Acquisition.Series,
                                                                       LD, Patient.Age) # to select column of interest and keeping the column's name



Study_data_prostate_stat_unique <- Study_data_prostate_stat_unique[!duplicated(Study_data_prostate_stat_unique$Accession.Number), ] # to keep only one row per exam to perform global stats
print("TNN12 is the codename in DW for non overexposure")
print("TNN13 is the codename in DW for overexposure")
temp_DAP_fluoro <- as.data.frame(as.numeric(levels(Study_data_prostate_stat_unique[,10]))[Study_data_prostate_stat_unique[,10]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_fluoro) <- c("Total.Fluoro.DAP") # add colnames for labellization
temp_DAP_graph <- as.data.frame(as.numeric(levels(Study_data_prostate_stat_unique[,9]))[Study_data_prostate_stat_unique[,9]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_graph) <- c("Total.Acquisition.DAP") # add colnames for labellization
Study_data_prostate_stat_unique <- cbind(Study_data_prostate_stat_unique[,1:8], temp_DAP_fluoro, temp_DAP_graph, Study_data_prostate_stat_unique[,11:17]) # bind alll columns for summary
write.csv(Study_data_prostate_stat_unique, "output/Study_data_prostate_stat_unique.csv")
write.xlsx(Study_data_prostate_stat_unique, "output/Study_data_prostate_stat_unique.xlsx")

Global_stat_unique_value <- summary(Study_data_prostate_stat_unique)
write.csv(Global_stat_unique_value, "output/stats/Global_stat_unique_value.csv")

## global stat for non unique value by exam : FOV and incidence angles

Study_data_prostate_stat_multiple <- Study_data_prostate_stat[ ,21:23] # to select FOV and incidence angles columns
FOV_ok_list <- which(Study_data_prostate_stat_multiple$Field.of.View > 0) # to keep only FOV > 0 as FOV value = 0 it's an DoseWatch export error
Study_data_prostate_stat_multiple <- Study_data_prostate_stat_multiple[FOV_ok_list,] # to supress FOV bad value
Global_stat_multiple_value <- summary(Study_data_prostate_stat_multiple)
write.csv(Global_stat_multiple_value, "output/stats/Global_stat_FOV&incidence_angle.csv")


################ CBCT acquisition analysis #############################
# Summary of acquisition type repartition between all examinations
Acquisition_table <- table(Study_data_prostate$Patient.ID, Study_data_prostate$Irradiation.Event.Type)
CBCT_position <- which(Study_data_prostate$Irradiation.Event.Type == "CBCT+")

# selection of sequences with CBCT
Study_data_prostate_CBCT <- subset(Study_data_prostate, Irradiation.Event.Type == "CBCT+")
# count patient number with CBCT /!\ some patient could have multiple exams
Study_data_prostate_CBCT <- data.frame(Study_data_prostate_CBCT$Patient.ID, Study_data_prostate_CBCT$Accession.number, Study_data_prostate_CBCT$Irradiation.Event.Type, Study_data_prostate_CBCT$Proprietary.Type)
Patient_number_CBCT <- length(unique(Study_data_prostate_CBCT[,"Study_data_prostate_CBCT.Patient.ID"]))
Patient_number_wo_CBCT <- length(unique(Study_data_prostate[,"Patient.ID"])) - Patient_number_CBCT
# select only Patient ID and Irradiation event to table CBCT number per examination
Exam_ID_list_CBCT <- table(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number, droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Irradiation.Event.Type))
# statistical analysis on CBCT acquisition

# CBCT_stat <- describe(Exam_ID_list_CBCT, num.desc=c("mean","median","sd","min","max","valid.n"))
# write.csv(CBCT_stat, "output/stats/CBCT_patient_stat.csv")

#CBCT_mean <- mean(Exam_ID_list_CBCT)
### graphic to illustrate CBCT acquisition repartition
# lbls <- Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number # create label = accession number
Study_data_prostate_CBCT_acquisition_num <- as.numeric(Exam_ID_list_CBCT,2) # convert factor in numeric value for pourcent
pct <- round(Study_data_prostate_CBCT_acquisition_num/sum(Study_data_prostate_CBCT_acquisition_num)*100,1) # generate pourcent of CBCT acquisition
lbls <- paste(pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Exam_ID_list_CBCT, labels = lbls, col=rainbow(length(lbls)), main = "CBCT acquisiton number for each patient")
dev.print(device = png, file = "output/CBCT_acquisition_number.png", width = 600, height = 400)
lbls <- Study_data_prostate_CBCT_acquisition_num
pie(Exam_ID_list_CBCT, labels = lbls, col=rainbow(length(lbls)), main = "CBCT acquisiton number for each patient")
dev.print(device = png, file = "output/CBCT_acquisition_number_absolute.png", width = 600, height = 400)


### CBCT protocol analysis and graph
CBCT_protocol_used <- table(droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Proprietary.Type))
pct <- round(as.numeric(CBCT_protocol_used)/sum(as.numeric(CBCT_protocol_used))*100,1) # generate pourcent of CBCT protocols
lbls <- names(CBCT_protocol_used) # create label = protocol name
lbls <- paste(lbls, pct, sep = ", \n ") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(CBCT_protocol_used, labels = lbls, cex = 0.8, main = "CBCT protocols used")
dev.print(device = png, file = "output/CBCT_protocols.png", width = 600, height = 400)
lbls <- as.numeric(CBCT_protocol_used)
pie(CBCT_protocol_used, labels = lbls, cex = 0.8, main = "CBCT protocols used")
dev.print(device = png, file = "output/CBCT_protocols_absolute.png", width = 600, height = 400)


################ FOV size analysis #############################

# Summary of FOV distribution between all examinations
FOV_table <- table(droplevels(Study_data_prostate$Patient.ID), Study_data_prostate$Field.of.View..cm.)
FOV_table <- FOV_table[,2:5] # supress FOV = 0 cm column as it's an error of DoseWatch export
FOV_table <- round(prop.table(FOV_table, margin = 1)*100) # table in purcent for each examn


FOV_tab_total <- round(prop.table(addmargins(FOV_table,1),1)*100,1)  # FOV frequencies in purcent by patient plus total frequency
# FOV_tab_total <- cbind(addmargins(prop.table(addmargins(FOV_table,1),1),2), c(margin.table(FOV_table,1),sum(FOV_table)))
# colnames(FOV_tab_total)<-c(colnames(FOV_table),"TOTAL","EFFECTIF") 


# ### graphic to illustrate FOV distribution in all examinations
#
lbls <- colnames(FOV_tab_total) # create label = FOV size
lbls <- paste(lbls, "cm") # add cm
pct <- round(tail(FOV_tab_total,1)/sum(tail(FOV_tab_total,1))*100) # generate pourcent of total FOV size
lbls <- paste(lbls, pct, sep = ", \n") # add percents to labels
lbls <- paste(lbls," %",sep="") # ad % to labels 
pie(tail(FOV_tab_total,1), labels = lbls, cex =0.9, main = "FOV distribution in all examinations")
dev.print(device = png, file = "output/FOV_distribution.png", width = 600, height = 400)
lbls <- tail(FOV_tab_total,1)
pie(tail(FOV_tab_total,1), labels = lbls, cex =0.9, main = "FOV distribution in all examinations")
dev.print(device = png, file = "output/FOV_distribution_asolute.png", width = 600, height = 400)
##########################################################################
##########################################################################
##########################################################################
################ CBCT+/- and LD +/- analysis #############################
##########################################################################
##########################################################################
##########################################################################

################ FOV distribution CBCT+/- #############################

FOV_CBCT_table <- table(droplevels(Study_data_prostate$Irradiation.Event.Type), Study_data_prostate$Field.of.View..cm.)
FOV_CBCT_table <- FOV_CBCT_table[,2:5] # supress FOV = 0 cm column as it's an error of DoseWatch export
FOV_CBCT_table_pourcent <- round(prop.table(FOV_CBCT_table, margin = 1)*100) # table in purcent for each examn


FOV_tab_CBCT_total <- round(prop.table(addmargins(FOV_CBCT_table,1),1)*100,1)  # FOV frequencies in purcent by patient plus total frequency
# FOV_tab_total <- cbind(addmargins(prop.table(addmargins(FOV_table,1),1),2), c(margin.table(FOV_table,1),sum(FOV_table)))
# colnames(FOV_tab_total)<-c(colnames(FOV_table),"TOTAL","EFFECTIF") 


# ### graphic to illustrate FOV distribution in all examinations
#
lbls <- colnames(FOV_tab_CBCT_total) # create label = FOV size
lbls <- paste(lbls, "cm") # add cm
pct_CBCT_total <- round(tail(FOV_tab_CBCT_total,1)/sum(tail(FOV_tab_CBCT_total,1))*100) # generate pourcent of total FOV size
pct_CBCT_plus <- round(head(FOV_tab_CBCT_total,1)/sum(head(FOV_tab_CBCT_total,1))*100) # generate pourcent of total FOV size
pct_CBCT_minus <- round(FOV_tab_CBCT_total[2,]/sum(FOV_tab_CBCT_total[2,])*100) # generate pourcent of total FOV size
lbls_total <- paste(lbls, pct_CBCT_total, sep = ", \n") # add percents to labels
lbls_total <- paste(lbls_total," %",sep="") # ad % to labels 
pie(tail(FOV_tab_CBCT_total,1), labels = lbls_total, cex =0.9, main = "FOV distribution with or without CBCT acquisitions")
dev.print(device = png, file = "output/FOV_CBCT_distribution.png", width = 600, height = 400)
lbls_plus <- paste(lbls, pct_CBCT_plus, sep = ", \n") # add percents to labels
lbls_plus <- paste(lbls_plus," %",sep="") # ad % to labels 
pie(head(FOV_tab_CBCT_total,1), labels = lbls_plus, cex =0.9, main = "FOV distribution with CBCT acquisitions")
dev.print(device = png, file = "output/FOV_CBCTplus_distribution.png", width = 600, height = 400)
lbls_minus <- paste(lbls, pct_CBCT_minus, sep = ", \n") # add percents to labels
lbls_minus <- paste(lbls_minus," %",sep="") # ad % to labels 
pie(FOV_tab_CBCT_total[2,], labels = lbls_minus, cex =0.9, main = "FOV distribution without CBCT acquisitions")
dev.print(device = png, file = "output/FOV_CBCTminus_distribution.png", width = 600, height = 400)


################ FOV distribution LD+/- #############################

FOV_LD_table <- table(droplevels(Study_data_prostate$LD), Study_data_prostate$Field.of.View..cm.)
FOV_LD_table <- FOV_LD_table[,2:5] # supress FOV = 0 cm column as it's an error of DoseWatch export
FOV_LD_table_pourcent <- round(prop.table(FOV_LD_table, margin = 1)*100) # table in purcent for each examn

FOV_tab_LD_total <- round(prop.table(addmargins(FOV_LD_table,1),1)*100,1)  # FOV frequencies in purcent by patient plus total frequency

# ### graphic to illustrate FOV distribution in all examinations
#
lbls <- colnames(FOV_tab_LD_total) # create label = FOV size
lbls <- paste(lbls, "cm") # add cm
pct_LD_total <- round(tail(FOV_tab_LD_total,1)/sum(tail(FOV_tab_LD_total,1))*100) # generate pourcent of total FOV size
pct_LD_plus <- round(FOV_tab_LD_total[2,]/sum(FOV_tab_LD_total[2,])*100) # generate pourcent of total FOV size
pct_LD_minus <- round(head(FOV_tab_LD_total,1)/sum(head(FOV_tab_LD_total,1))*100) # generate pourcent of total FOV size
lbls_total <- paste(lbls, pct_LD_total, sep = ", \n") # add percents to labels
lbls_total <- paste(lbls_total," %",sep="") # ad % to labels 
pie(tail(FOV_tab_LD_total,1), labels = lbls_total, cex =0.9, main = "FOV distribution with all displays configurations")
dev.print(device = png, file = "output/FOV_all_LD_distribution.png", width = 600, height = 400)
lbls_plus <- paste(lbls, pct_LD_plus, sep = ", \n") # add percents to labels
lbls_plus <- paste(lbls_plus," %",sep="") # ad % to labels 
pie(FOV_tab_LD_total[2,], labels = lbls_plus, cex =0.9, main = "FOV distribution with LD configuration")
dev.print(device = png, file = "output/FOV_LDplus_distribution.png", width = 600, height = 400)
lbls_minus <- paste(lbls, pct_LD_minus, sep = ", \n") # add percents to labels
lbls_minus <- paste(lbls_minus," %",sep="") # ad % to labels
pie(head(FOV_tab_LD_total,1), labels = lbls_minus, cex =0.9, main = "FOV distribution without LD configuration")
dev.print(device = png, file = "output/FOV_LDminus_distribution.png", width = 600, height = 400)


##########################################################################
##########################################################################
##########################################################################
################  statistical analysis by group ##########################
##########################################################################
##########################################################################
##########################################################################

################ CBCT+/- analysis #############################

# global stat part
global_data_prostate <- Study_data_prostate %>% select(Accession.number, Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
                                                       Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., Total.Acquisition.DAP..mGy.cm..,
                                                       Total.Fluoro.DAP..mGy.cm.., Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
                                                       Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                       Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                       Field.of.View..cm., LD, Patient.Age) # to select column of interest and keeping the column's name

Data_prostate_CBCT_plus <- subset(global_data_prostate, global_data_prostate$Irradiation.Event.Type == "CBCT+") # select only rows with CBCT
global_data_prostate_CBCT_plus <- Data_prostate_CBCT_plus[!duplicated(Data_prostate_CBCT_plus$Accession.number), ] # to keep only one row per exam to perform global stats
temp_DAP_fluoro_plus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_plus[,9]))[global_data_prostate_CBCT_plus[,9]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_fluoro_plus) <- c("Total.Fluoro.DAP") # add colnames for labellization
temp_DAP_graph_plus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_plus[,8]))[global_data_prostate_CBCT_plus[,8]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_graph_plus) <- c("Total.Acquisition.DAP") # add colnames for labellization
temp_Patient_age_plus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_plus[,20]))[global_data_prostate_CBCT_plus[,20]]) # convert factor with level to a dataframe with numeric value
colnames(temp_Patient_age_plus) <- c("Patient.age") # add colnames for labellization
global_data_prostate_CBCT_plus_unique <- global_data_prostate_CBCT_plus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
                                                                                   Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                                   Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
                                                                                   Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                                                   Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
global_data_prostate_CBCT_plus_unique <- cbind(global_data_prostate_CBCT_plus_unique, temp_DAP_fluoro_plus, temp_DAP_graph_plus, temp_Patient_age_plus) # bind all columns for summary
Stat_CBCT_plus_unique <- summary(global_data_prostate_CBCT_plus_unique)
write.csv(Stat_CBCT_plus_unique, "output/stats/Stat_CBCT_plus_unique.csv")

Data_prostate_CBCT_minus <- subset(global_data_prostate, global_data_prostate$Irradiation.Event.Type == "CBCT-") # select only rows without CBCT
global_data_prostate_CBCT_minus <- Data_prostate_CBCT_minus[!duplicated(Data_prostate_CBCT_minus$Accession.number), ] # to keep only one row per exam to perform global stats
temp_DAP_fluoro_minus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_minus[,9]))[global_data_prostate_CBCT_minus[,9]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_fluoro_minus) <- c("Total.Fluoro.DAP") # add colnames for labellization
temp_DAP_graph_minus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_minus[,8]))[global_data_prostate_CBCT_minus[,8]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_graph_minus) <- c("Total.Acquisition.DAP") # add colnames for labellization
temp_Patient_age_minus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_minus[,20]))[global_data_prostate_CBCT_minus[,20]]) # convert factor with level to a dataframe with numeric value
colnames(temp_Patient_age_minus) <- c("Patient.age") # add colnames for labellization
global_data_prostate_CBCT_minus_unique <- global_data_prostate_CBCT_minus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Peak.Skin.Dose..mGy., Performing.physician.last.name,
                                                                                     Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
                                                                                     Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                                                     Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
global_data_prostate_CBCT_minus_unique <- cbind(global_data_prostate_CBCT_minus_unique, temp_DAP_fluoro_minus, temp_DAP_graph_minus, temp_Patient_age_minus) # bind all columns for summary
Stat_CBCT_minus_unique <- summary(global_data_prostate_CBCT_minus_unique)
write.csv(Stat_CBCT_minus_unique, "output/stats/Stat_CBCT_minus_unique.csv")

# Anova_CBCT_minus_unique <- anova(global_data_prostate_CBCT_minus_unique)
# stat for non unique value (e.g FOV, incidence angle) part

FOV_angle_data_prostate <- Study_data_prostate %>% select(Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                          Field.of.View..cm., LD) # to select column of interest and keeping the column's name

FOV_angle_prostate_CBCT_plus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$Irradiation.Event.Ty == "CBCT+") # select only rows with CBCT
Stat_CBCT_plus_FOV_angle <- summary(FOV_angle_prostate_CBCT_plus)
write.csv(Stat_CBCT_plus_FOV_angle, "output/stats/Stat_CBCT_plus_FOV_angle.csv")

FOV_angle_prostate_CBCT_minus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$Irradiation.Event.Ty == "CBCT-") # select only rows without CBCT
Stat_CBCT_minus_FOV_angle <- summary(FOV_angle_prostate_CBCT_minus)
write.csv(Stat_CBCT_minus_FOV_angle, "output/stats/Stat_CBCT_minus_FOV_angle.csv")

# Anova_CBCT_minus_FOV_angle <- anova(FOV_angle_prostate_CBCT_minus)
# t.test(IP_dose_colon$Colon[IP_dose_colon$Logiciel == "CT-Expo"], IP_dose_colon$Colon[IP_dose_colon$Logiciel == "Duke Organ Dose"], paired=FALSE)
# wilcox.test(x, y, paired = TRUE)

### Boite à moustache dose absorbée suivant logiciel
# ggplot(file_ip_4logiciels) + geom_boxplot(aes(x=Logiciel, y=Colon, fill=Logiciel)) + theme(plot.title = element_text(hjust = 0.5))+labs(title="Répartition de la dose absorbée au Côlon (en mGy)\n en fonction des logiciels de calculs de dose", x= "", y ="Dose absorbee (mGy)", size=10)
# dev.print(device = png, file = "output/ggplot/bam/IP_total/BAM_IP_total_Colon.png", width = 600, height = 400)

################ LD+/- analysis #############################

# global stat part
global_data_prostate <- Study_data_prostate %>% select(Accession.number, Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
                                                       Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., Total.Acquisition.DAP..mGy.cm..,
                                                       Total.Fluoro.DAP..mGy.cm.., Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
                                                       Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                       Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                       Field.of.View..cm., LD, Patient.Age) # to select column of interest and keeping the column's name

Data_prostate_LD_plus <- subset(global_data_prostate, global_data_prostate$LD == "LD+") # select only rows with Large Display (LD)
global_data_prostate_LD_plus <- Data_prostate_LD_plus[!duplicated(Data_prostate_LD_plus$Accession.number), ] # to keep only one row per exam to perform global stats
temp_DAP_fluoro_plus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_plus[,9]))[global_data_prostate_LD_plus[,9]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_fluoro_plus) <- c("Total.Fluoro.DAP") # add colnames for labellization
temp_DAP_graph_plus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_plus[,8]))[global_data_prostate_LD_plus[,8]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_graph_plus) <- c("Total.Acquisition.DAP") # add colnames for labellization
temp_Patient_age_plus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_plus[,19]))[global_data_prostate_LD_plus[,19]]) # convert factor with level to a dataframe with numeric value
colnames(temp_Patient_age_plus) <- c("Patient.age") # add colnames for labellization
global_data_prostate_LD_plus_unique <- global_data_prostate_LD_plus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
                                                                               Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                               Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
                                                                               Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                                               Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
global_data_prostate_LD_plus_unique <- cbind(global_data_prostate_LD_plus_unique, temp_DAP_fluoro_plus, temp_DAP_graph_plus, temp_Patient_age_plus) # bind all columns for summary
Stat_LD_plus_unique <- summary(global_data_prostate_LD_plus_unique)
write.csv(Stat_LD_plus_unique, "output/stats/Stat_LD_plus_unique.csv")


Data_prostate_LD_minus <- subset(global_data_prostate, global_data_prostate$LD == "LD-") # select only rows without Large Display (LD)
global_data_prostate_LD_minus <- Data_prostate_LD_minus[!duplicated(Data_prostate_LD_minus$Accession.number), ] # to keep only one row per exam to perform global stats
temp_DAP_fluoro_minus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_minus[,9]))[global_data_prostate_LD_minus[,9]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_fluoro_minus) <- c("Total.Fluoro.DAP") # add colnames for labellization
temp_DAP_graph_minus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_minus[,8]))[global_data_prostate_LD_minus[,8]]) # convert factor with level to a dataframe with numeric value
colnames(temp_DAP_graph_minus) <- c("Total.Acquisition.DAP") # add colnames for labellization
temp_Patient_age_minus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_minus[,19]))[global_data_prostate_LD_minus[,19]]) # convert factor with level to a dataframe with numeric value
colnames(temp_Patient_age_minus) <- c("Patient.age") # add colnames for labellization
global_data_prostate_LD_minus_unique <- global_data_prostate_LD_minus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
                                                                                 Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                                 Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
                                                                                 Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                                                 Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
global_data_prostate_LD_minus_unique <- cbind(global_data_prostate_LD_minus_unique, temp_DAP_fluoro_minus, temp_DAP_graph_minus, temp_Patient_age_minus) # bind all columns for summary
Stat_LD_minus_unique <- summary(global_data_prostate_LD_minus_unique)
write.csv(Stat_LD_minus_unique, "output/stats/Stat_LD_minus_unique.csv")

# Anova_LD_minus_unique <- anova(global_data_prostate_LD_minus_unique)

# stat for non unique value (e.g FOV, incidence angle) part

FOV_angle_data_prostate <- Study_data_prostate %>% select(Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                          Field.of.View..cm., LD) # to select column of interest and keeping the column's name

FOV_angle_prostate_LD_plus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$LD == "LD+") # select only rows with Large Display (LD)
Stat_LD_plus_FOV_angle <- summary(FOV_angle_prostate_LD_plus)
write.csv(Stat_LD_plus_FOV_angle, "output/stats/Stat_LD_plus_FOV_angle.csv")

FOV_angle_prostate_LD_minus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$LD == "LD-") # select only rows without Large Display (LD
Stat_LD_minus_FOV_angle <- summary(FOV_angle_prostate_LD_minus)
write.csv(Stat_LD_minus_FOV_angle, "output/stats/Stat_LD_minus_FOV_angle.csv")



################ TODO -LIST ########################

# A faire :
# ANNOVA CBCT +/- ; LD +/- HOW ???????
# t.student CBCT +/- ; LD +/- HOW ???????
# wilcoxon CBCT +/- ; LD +/- HOW ???????
# PCA CBCT +/- ~ dose pic ; LD +/- ~ dose pic
# TSNE CBCT +/- ~ dose pic ; LD +/- ~ dose pic
