######################################################################
##                                                                  ##                                  
##                      Prostate embolization study                 ##
##                to compare Wide screen vs. small screen           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud, MPE, M.Sc. Medical imaging department - Tenon University Hospital
# date of creation : 02/07/2020

# date of the last update : 21/07/2021
# last review : second version to compare the use or non-use of VesselAssist software
# project lead by Dr. Matthias Barral, MD, PhD. Medical imaging department - Tenon University Hospital

###################### set-up section ################################

# Set the project path to the root level -
root.dir = rprojroot::find_rstudio_root_file()

# load lubridate package to determine patient age from birthday with an install condition
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

# load doMC package for parallel computing
if(!require(doMC)){
  install.packages("doMC")
  library(doMC)
}

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

# load tidyverse for data science such as data handling and visualization
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

# load factoshiny for statistical computation aid
if(!require(Factoshiny)){
  install.packages("Factoshiny")
  library(Factoshiny)
}# load factoshiny for statistical computation aid
if(!require(FactoMineR)){
  install.packages("FactoMineR")
  library(FactoMineR)
}


###############################################################################################################
###############################################################################################################
######################### First part : data pre-treatment #####################################################
###############################################################################################################
###############################################################################################################


############################### data import section ##################################


## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# unfortunately Excel importation yield to parse errors in date data for instance.
# SO TO AVOID THIS PROBLEM THE USER HAVE TO IMPORT DATA IN CSV FORMAT 
# BY CONVERTING ORIGINAL DATA FROM .XLSX TO .CSV IN EXCEL SOFTWARE
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

# the study is based from patient database extracted between 02/01/2018 to 4/06/2021

tic("to import detailled data in Rstudio")
if(exists("DoseWatch_export")){
  print("raw data importation have already done")
}else{
 
  # operations to correctly read DoseWatch export file *NEW* format only for DoseWatch v3.2.3 and above.
  # if you have DoseWatch v3.1 or under comment the three following lines and uncomment the last previous command line.
  all_content = readLines("data_v2/Interventional_Tenon_Radiologie_detailed_data_export.csv") # to read the whole file
  skip_content = all_content[-c(1,2,3)] # to suppress the first 3 rows with bad value yield to bad header with read.csv2 function
  DoseWatch_export <- read.csv2(textConnection(skip_content), sep = ";")
  # my_all_data <- read.csv2("data_v2/Interventional_Tenon_Radiologie_detailed_data_export.csv", sep = ";")
}
toc()

################################## data tailoring section ##################################
# data filter to keep only interested columns for this study
DoseWatch_Selected_data <- DoseWatch_export %>% select(Patient.last.name, Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                       Patient.birthdate..YYYY.MM.DD.,
                                                       Patient.weight..kg., Patient.size..cm.,
                                                       BMI, Standard.study.description,
                                                       Peak.Skin.Dose..mGy.,
                                                       Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                       Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                       Total.Air.Kerma..mGy.,
                                                       Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                       Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                       Irradiation.Event.Type,Proprietary.Type, Dose.Preference,
                                                       Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                       Field.of.View..cm.) # to select column of interest and keeping the column's name

# convert Series.time, Patient Birthdate and Study date  columns in right time format
DoseWatch_Selected_data <- DoseWatch_Selected_data %>%
  mutate(Series.Time = hm(Series.Time), Patient.birthdate..YYYY.MM.DD. = ymd_hms(Patient.birthdate..YYYY.MM.DD.),
         Study.date..YYYY.MM.DD. = as.POSIXct(Study.date..YYYY.MM.DD.))

# sort each line by Accession number and then by acquisition hour
DoseWatch_Selected_data <- arrange(DoseWatch_Selected_data, Accession.number, Series.Time)

######################## age patient computation #################################
#  instance null vector with appropriate dimension to bind with Study_data
Patient.Age <- rep(0, nrow(DoseWatch_Selected_data))

# Loop with parallelization to calculate patient age in years 
# and add this information to Study_data dataframe
# also have a condition to test global environment object for debugging
tic("for loop with parallelization")
if(exists("Study_data_selected_exam_without_duplicates")){
  print("patient age computation have already done")
}else{
  cores <- detectCores()
  registerDoMC(cores - 1)
  Patient.Age <- foreach(i = 1:nrow(DoseWatch_Selected_data)) %dopar% {
    #naiss = ymd_hms(DoseWatch_Selected_data[i,4]) # deprecated line as mutate function can convert easily "time" column
    #evt = as.POSIXct(DoseWatch_Selected_data[i,1]) # deprecated line as mutate function can convert easily "time" column
    # by suppressing those 2 previous lines and use mutate function instead => Computing acceleration by a factor 6 !!!!!!
    age = as.period(interval(DoseWatch_Selected_data[i,6], DoseWatch_Selected_data[i,2]))@year
    Patient.Age <- age
  }
}
toc()

Patient.Age <- as.character(Patient.Age)
Study_data_age <-cbind(DoseWatch_Selected_data,Patient.Age)

Study_data_selected_age <- Study_data_age %>% select(Patient.last.name, Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                     Patient.Age,
                                                     Patient.birthdate..YYYY.MM.DD.,
                                                     Patient.weight..kg., Patient.size..cm.,
                                                     BMI, Standard.study.description,
                                                     Peak.Skin.Dose..mGy.,
                                                     Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                     Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                     Total.Air.Kerma..mGy.,
                                                     Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                     Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                     Irradiation.Event.Type,Proprietary.Type, Dose.Preference,
                                                     Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
                                                     Field.of.View..cm.)

############### column format conversion #################
# convert patient years in numeric
Study_data_selected_age$Patient.Age <- as.numeric(Study_data_selected_age$Patient.Age)
Study_data_selected_age$Total.Acquisition.DAP..mGy.cm.. <- as.numeric(Study_data_selected_age$Total.Acquisition.DAP..mGy.cm..)
Study_data_selected_age$Total.Fluoro.DAP..mGy.cm.. <- as.numeric(Study_data_selected_age$Total.Fluoro.DAP..mGy.cm..)

############### retrieve study patient lines #################

# add a filter to choose dedicated exam for this study
Study_data_selected_exam <- Study_data_selected_age %>% filter(Accession.number == 30034982996 |
                                                                 Accession.number == 30035241463 | Accession.number == 30034983096 |
                                                                 Accession.number == 30035053141 | Accession.number == 30035373571 |
                                                                 Accession.number == 30035346090 | Accession.number == 30035382287 |
                                                                 Accession.number == 30036207685 | Accession.number == 30037175809 | 
                                                                 Accession.number == 30038566792 | Accession.number == 30038486266 |
                                                                 Accession.number == 30038496467 | Accession.number == 30039008427 |
                                                                 Accession.number == 30039269549 | Accession.number == 30039660385 |
                                                                 Accession.number == 30039269114 | Accession.number == 30039481072 |
                                                                 Accession.number == 30039480794 | Accession.number == 30040738831 |
                                                                 Accession.number == 30040929614 | Accession.number == 30040484921 |
                                                                 Accession.number == 30040890761 | Accession.number == 30040959456 |
                                                                 Accession.number == 30041643891 | Accession.number == 30046466580 |  
                                                                 Accession.number == 30041842120 | Accession.number == 30041059698 |
                                                                 Accession.number == 30041618675 | Accession.number == 30040352898 |
                                                                 Accession.number == 30041770605 | Accession.number == 30041110524 |
                                                                 Accession.number == 30040485398 | Accession.number == 30042776814 |
                                                                 Accession.number == 30041624704 | Accession.number == 30040962763 |
                                                                 Accession.number == 30042720701 | Accession.number == 30043071308 |
                                                                 Accession.number == 30043870534 | Accession.number == 30043113877 |
                                                                 Accession.number == 30043219641 | Accession.number == 30044170978 |
                                                                 Accession.number == 30043818613 | Accession.number == 30043411855 |
                                                                 Accession.number == 30043902552 | Accession.number == 30044452243 |
                                                                 Accession.number == 30045035470 | Accession.number == 30044195855 |
                                                                 Accession.number == 30044600813 | Accession.number == 30045986706 |
                                                                 Accession.number == 30045792078 | Accession.number == 30045807821 |
                                                                 Accession.number == 30046467063 | Accession.number == 30046484391 |
                                                                 Accession.number == 30032928352 | Accession.number == 30033153121 |
                                                                 Accession.number == 30031838672 | Accession.number == 30031800470 |
                                                                 Accession.number == 30032107360 | Accession.number == 30031800592 |
                                                                 Accession.number == 30029780414 | Accession.number == 30031185851 |
                                                                 Accession.number == 30030160986 | Accession.number == 30031001792 |
                                                                 Accession.number == 30030533112 | Accession.number == 30028117919 |
                                                                 Accession.number == 30028125485 | Accession.number == 30028694354 |
                                                                 Accession.number == 30028276798 | Accession.number == 30027925109 |
                                                                 Accession.number == 30026441418 | Accession.number == 30026114824 |
                                                                 Accession.number == 30025841249 | Accession.number == 30026163655 |
                                                                 Accession.number == 30026298414 | Accession.number == 30023767089 |
                                                                 Accession.number == 30023346300 | Accession.number == 30023813528 |
                                                                 Accession.number == 30023239878 | Accession.number == 30022654755 |
                                                                 Accession.number == 30022626205 | Accession.number == 30021239922 |
                                                                 Accession.number == 30021173788 | Accession.number == 30019846218 |
                                                                 Accession.number == 30017681129 | Accession.number == 30017086347 |
                                                                 Accession.number == 30016180136 | Accession.number == 30016128315 |
                                                                 Accession.number == 30015411613 | Accession.number == 30013529532 |
                                                                 Accession.number == 30011611853 | Accession.number == 30011104386) 

# sort each line by Study date and then by acquisition hour
Study_data_selected_exam <- arrange(Study_data_selected_exam, Study.date..YYYY.MM.DD., Series.Time)


####### VesselAssist software using (VA) +/- labelization  ###########################


#  create new levels as +/- VA
print("VesselAssist have been installed since 2020-11-01")
print("All exams generated after 2020-10-31 are considered as performed with VesselAssist software")
VA <- Study_data_selected_exam$Study.date..YYYY.MM.DD.
VA <- gsub("[: -]", "" , VA, perl=TRUE) # delete ":" , " ", and "-" from the date value
VA <- as.numeric(VA) # convert VA in numeric format to apply cut function
VA <- cut(VA,c(20180102,20201031, Inf),c("VA-","VA+")) # cut to segment date between VA+ and VA- 
VA <- factor(as.character(VA),levels=c("VA-","VA+")) # convert factor in character
Study_data_selected_exam$VA <- VA # add VA factor to the data frame

# Remove duplicates in order to have only one row by exam
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# if you have several exam without accession number associated the following.
# command line will only keep the first exam without accession number 
#  SO YOU COULD LOST DATA INFORMATION
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
Study_data_selected_exam_without_duplicates <- Study_data_selected_exam[!duplicated(Study_data_selected_exam$Accession.number), ] # to keep only one row for each exam time.
# to select only row that not depends on sequence parameters
Study_data_selected_exam_without_duplicates <- Study_data_selected_exam_without_duplicates %>% select(Patient.last.name, Study.date..YYYY.MM.DD., Patient.ID, Accession.number,
                                                                                                                                  Patient.Age,
                                                                                                                                  Patient.weight..kg., Patient.size..cm.,
                                                                                                                                  BMI,
                                                                                                                                  Peak.Skin.Dose..mGy.,
                                                                                                                                  Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                                                                                  Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                                                                                                  Total.Air.Kerma..mGy.,
                                                                                                                                  Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                                                                                                  Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
                                                                                                                                  Dose.Preference, VA)

################## Global environment cleaning ###########################

# Remove dataframe which don't still have any interest
if(exists("VA")) {
  print("Global environment will be clean")
  rm (DoseWatch_Selected_data, Study_data_selected_age, Study_data_age, all_content, skip_content, VA, Patient.Age)
}else{
  print("Global environment already clean")
}


##########################################################################
##########################################################################
##########################################################################
################ global statistical analysis #############################
##########################################################################
##########################################################################
##########################################################################

# selected only interested columns for Factoshiny package (cos2 > 0.7)

Study_data_selected_exam_without_duplicates_factoshiny <- Study_data_selected_exam_without_duplicates %>% select(Accession.number,
                                                                                                      Patient.weight..kg.,
                                                                                                      BMI,
                                                                                                      Peak.Skin.Dose..mGy.,
                                                                                                      Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                                                      Total.Fluoro.DAP..mGy.cm..,
                                                                                                      Total.Air.Kerma..mGy.,
                                                                                                      Total.Fluoro.Air.Kerma..mGy.,
                                                                                                      VA)


# Converted the values in the first column (e.g patient names or accession number if you have duplicated patient) into row names in the dataframe for Factoshiny computation
Study_data_selected_exam_without_duplicates_factoshiny <- data.frame(Study_data_selected_exam_without_duplicates_factoshiny[,-1], row.names=Study_data_selected_exam_without_duplicates_factoshiny[,1])

globalstat <- summary(Study_data_selected_exam_without_duplicates)

# write excel file to communicate with other people
print("Create output Excel file for data table and main statistics")
write.xlsx(Study_data_selected_exam_without_duplicates, "output/v2/Study_data_treated.xlsx", sheetName = "Study_data_treated",
           col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
write.xlsx(globalstat, "output/v2/globalstat.xlsx", sheetName = "globalstat",
           col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# execute manually the following line to launch correctly Factoshiny as the ~ character in path file/dataframe is not correctly recognize by Factoshiny/FactoMiner package
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
#Factoshiny(Study_data_selected_exam_without_duplicates_factoshiny)

Factoshiny(Study_data_selected_exam_without_duplicates_factoshiny)

# Factoshiny output command lines post manual treatment
# nb <- missMDA::estim_ncpPCA(Study_data_selected_exam_without_duplicates_factoshiny,quali.sup=c(8))$ncp
# dfcompleted <- missMDA::imputePCA(Study_data_selected_exam_without_duplicates_factoshiny,ncp=nb,quali.sup=c(8))$completeObs
# res.PCA<-PCA(dfcompleted,quali.sup=c(8),graph=FALSE)
# plot.PCA(res.PCA,choix='var',habillage = 'contrib',title="Graphe des variables de l'ACP")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select='cos2  0.7',habillage='Peak.Skin.Dose..mGy.',title="Graphe des individus de l'ACP",cex=1.05,cex.main=1.05,cex.axis=1.05,label =c('quali'))


# ############### main statistical analysis #################
# # global stat for unique value by exam
# Study_data_prostate_stat <- cbind(Study_data_prostate[,2:3],Study_data_prostate[,5:7], Study_data_prostate[,9:27], Study_data_prostate[,28]) # to select row of interest for statistical computing
# colnames(Study_data_prostate_stat) <- c("Accession.Number","Patient.ID", "Patient.weight", "Patient.size", "BMI", "Performing.physician.last.name",
#                                         "Peak.Skin.Dose", "Image.and.Fluoroscopy.DAP", "Total.Acquisition.DAP", "Total.Fluoro.DAP",
#                                         "Total.Air.Kerma", "Total.Acquisition.Air.Kerma", "Total.Fluoro.Air.Kerma",
#                                         "Total.Time.of.Fluoroscopy", "Number.of.Acquisition.Series","Irradiation.Event.Type",
#                                         "Proprietary.Type","Dose.Preference","Justification.Code", "Raised.alerts",
#                                         "Positioner.Primary.Angle", "Positioner.Secondary.Angle", "Field.of.View","LD", "Patient.Age") # to rename correctly patient age column
# write.csv(Study_data_prostate_stat, "output/Study_data_prostate_stat.csv")
# write.xlsx(Study_data_prostate_stat, "output/Study_data_prostate_stat.xlsx")
# 
# #Study_data_prostate_stat_unique <- cbind(Study_data_prostate_stat[,1:14], Study_data_prostate_stat[,23:24]) # to select column with unique value per exam
# Study_data_prostate_stat_unique <- Study_data_prostate_stat %>% select(Accession.Number, Patient.ID, Patient.weight, Patient.size, BMI,
#                                                                        Performing.physician.last.name, Peak.Skin.Dose,
#                                                                        Image.and.Fluoroscopy.DAP, Total.Acquisition.DAP,
#                                                                        Total.Fluoro.DAP, Total.Air.Kerma, Total.Acquisition.Air.Kerma,
#                                                                        Total.Fluoro.Air.Kerma, Total.Time.of.Fluoroscopy, Number.of.Acquisition.Series,
#                                                                        LD, Patient.Age) # to select column of interest and keeping the column's name
# 
# 
# 
# Study_data_prostate_stat_unique <- Study_data_prostate_stat_unique[!duplicated(Study_data_prostate_stat_unique$Accession.Number), ] # to keep only one row per exam to perform global stats
# print("TNN12 is the codename in DW for non overexposure")
# print("TNN13 is the codename in DW for overexposure")
# temp_DAP_fluoro <- as.data.frame(as.numeric(levels(Study_data_prostate_stat_unique[,10]))[Study_data_prostate_stat_unique[,10]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_fluoro) <- c("Total.Fluoro.DAP") # add colnames for labellization
# temp_DAP_graph <- as.data.frame(as.numeric(levels(Study_data_prostate_stat_unique[,9]))[Study_data_prostate_stat_unique[,9]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_graph) <- c("Total.Acquisition.DAP") # add colnames for labellization
# Study_data_prostate_stat_unique <- cbind(Study_data_prostate_stat_unique[,1:8], temp_DAP_fluoro, temp_DAP_graph, Study_data_prostate_stat_unique[,11:17]) # bind all columns for summary
# write.csv(Study_data_prostate_stat_unique, "output/Study_data_prostate_stat_unique.csv")
# write.xlsx(Study_data_prostate_stat_unique, "output/Study_data_prostate_stat_unique.xlsx")
# 
# Global_stat_unique_value <- summary(Study_data_prostate_stat_unique)
# write.csv(Global_stat_unique_value, "output/stats/Global_stat_unique_value.csv")
# 
# ## global stat for non unique value by exam : FOV and incidence angles
# 
# Study_data_prostate_stat_multiple <- Study_data_prostate_stat[ ,21:23] # to select FOV and incidence angles columns
# FOV_ok_list <- which(Study_data_prostate_stat_multiple$Field.of.View > 0) # to keep only FOV > 0 as FOV value = 0 it's an DoseWatch export error
# Study_data_prostate_stat_multiple <- Study_data_prostate_stat_multiple[FOV_ok_list,] # to suppress FOV bad value
# Global_stat_multiple_value <- summary(Study_data_prostate_stat_multiple)
# write.csv(Global_stat_multiple_value, "output/stats/Global_stat_FOV&incidence_angle.csv")
# 
# 
# ################ CBCT acquisition analysis #############################
# # Summary of acquisition type distribution between all examinations
# Acquisition_table <- table(Study_data_prostate$Patient.ID, Study_data_prostate$Irradiation.Event.Type)
# CBCT_position <- which(Study_data_prostate$Irradiation.Event.Type == "CBCT+")
# 
# # selection of sequences with CBCT
# Study_data_prostate_CBCT <- subset(Study_data_prostate, Irradiation.Event.Type == "CBCT+")
# # count patient number with CBCT /!\ some patient could have multiple exams
# Study_data_prostate_CBCT <- data.frame(Study_data_prostate_CBCT$Patient.ID, Study_data_prostate_CBCT$Accession.number, Study_data_prostate_CBCT$Irradiation.Event.Type, Study_data_prostate_CBCT$Proprietary.Type)
# Patient_number_CBCT <- length(unique(Study_data_prostate_CBCT[,"Study_data_prostate_CBCT.Patient.ID"]))
# Patient_number_wo_CBCT <- length(unique(Study_data_prostate[,"Patient.ID"])) - Patient_number_CBCT
# # select only Patient ID and Irradiation event to table CBCT number per examination
# Exam_ID_list_CBCT <- table(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number, droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Irradiation.Event.Type))
# # statistical analysis on CBCT acquisition
# 
# # CBCT_stat <- describe(Exam_ID_list_CBCT, num.desc=c("mean","median","sd","min","max","valid.n"))
# # write.csv(CBCT_stat, "output/stats/CBCT_patient_stat.csv")
# 
# #CBCT_mean <- mean(Exam_ID_list_CBCT)
# ### graphic to illustrate CBCT acquisition distribution
# # lbls <- Study_data_prostate_CBCT$Study_data_prostate_CBCT.Accession.number # create label = accession number
# Study_data_prostate_CBCT_acquisition_num <- as.numeric(Exam_ID_list_CBCT,2) # convert factor in numeric value for percent
# pct <- round(Study_data_prostate_CBCT_acquisition_num/sum(Study_data_prostate_CBCT_acquisition_num)*100,1) # generate percent of CBCT acquisition
# lbls <- paste(pct) # add percents to labels
# lbls <- paste(lbls,"%",sep="") # ad % to labels 
# pie(Exam_ID_list_CBCT, labels = lbls, col=rainbow(length(lbls)), main = "CBCT acquisiton number for each patient")
# dev.print(device = png, file = "output/CBCT_acquisition_number.png", width = 600, height = 400)
# lbls <- Study_data_prostate_CBCT_acquisition_num
# pie(Exam_ID_list_CBCT, labels = lbls, col=rainbow(length(lbls)), main = "CBCT acquisiton number for each patient")
# dev.print(device = png, file = "output/CBCT_acquisition_number_absolute.png", width = 600, height = 400)
# 
# 
# ### CBCT protocol analysis and graph
# CBCT_protocol_used <- table(droplevels(Study_data_prostate_CBCT$Study_data_prostate_CBCT.Proprietary.Type))
# pct <- round(as.numeric(CBCT_protocol_used)/sum(as.numeric(CBCT_protocol_used))*100,1) # generate pourcent of CBCT protocols
# lbls <- names(CBCT_protocol_used) # create label = protocol name
# lbls <- paste(lbls, pct, sep = ", \n ") # add percents to labels
# lbls <- paste(lbls,"%",sep="") # ad % to labels 
# pie(CBCT_protocol_used, labels = lbls, cex = 0.8, main = "CBCT protocols used")
# dev.print(device = png, file = "output/CBCT_protocols.png", width = 600, height = 400)
# lbls <- as.numeric(CBCT_protocol_used)
# pie(CBCT_protocol_used, labels = lbls, cex = 0.8, main = "CBCT protocols used")
# dev.print(device = png, file = "output/CBCT_protocols_absolute.png", width = 600, height = 400)
# 
# 
# ################ FOV size analysis #############################
# 
# # Summary of FOV distribution between all examinations
# FOV_table <- table(droplevels(Study_data_prostate$Patient.ID), Study_data_prostate$Field.of.View..cm.)
# FOV_table <- FOV_table[,2:5] # suppress FOV = 0 cm column as it's an error of DoseWatch export
# FOV_table <- round(prop.table(FOV_table, margin = 1)*100) # table in percent for each exam
# 
# 
# FOV_tab_total <- round(prop.table(addmargins(FOV_table,1),1)*100,1)  # FOV frequencies in percent by patient plus total frequency
# # FOV_tab_total <- cbind(addmargins(prop.table(addmargins(FOV_table,1),1),2), c(margin.table(FOV_table,1),sum(FOV_table)))
# # colnames(FOV_tab_total)<-c(colnames(FOV_table),"TOTAL","EFFECTIF") 
# 
# 
# # ### graphic to illustrate FOV distribution in all examinations
# #
# lbls <- colnames(FOV_tab_total) # create label = FOV size
# lbls <- paste(lbls, "cm") # add cm
# pct <- round(tail(FOV_tab_total,1)/sum(tail(FOV_tab_total,1))*100) # generate percent of total FOV size
# lbls <- paste(lbls, pct, sep = ", \n") # add percents to labels
# lbls <- paste(lbls," %",sep="") # ad % to labels 
# pie(tail(FOV_tab_total,1), labels = lbls, cex =0.9, main = "FOV distribution in all examinations")
# dev.print(device = png, file = "output/FOV_distribution.png", width = 600, height = 400)
# lbls <- tail(FOV_tab_total,1)
# pie(tail(FOV_tab_total,1), labels = lbls, cex =0.9, main = "FOV distribution in all examinations")
# dev.print(device = png, file = "output/FOV_distribution_asolute.png", width = 600, height = 400)
# ##########################################################################
# ##########################################################################
# ##########################################################################
# ################ CBCT+/- and LD +/- analysis #############################
# ##########################################################################
# ##########################################################################
# ##########################################################################
# 
# ################ FOV distribution CBCT+/- #############################
# 
# FOV_CBCT_table <- table(droplevels(Study_data_prostate$Irradiation.Event.Type), Study_data_prostate$Field.of.View..cm.)
# FOV_CBCT_table <- FOV_CBCT_table[,2:5] # suppress FOV = 0 cm column as it's an error of DoseWatch export
# FOV_CBCT_table_pourcent <- round(prop.table(FOV_CBCT_table, margin = 1)*100) # table in percent for each exam
# 
# 
# FOV_tab_CBCT_total <- round(prop.table(addmargins(FOV_CBCT_table,1),1)*100,1)  # FOV frequencies in percent by patient plus total frequency
# # FOV_tab_total <- cbind(addmargins(prop.table(addmargins(FOV_table,1),1),2), c(margin.table(FOV_table,1),sum(FOV_table)))
# # colnames(FOV_tab_total)<-c(colnames(FOV_table),"TOTAL","EFFECTIF") 
# 
# 
# # ### graphic to illustrate FOV distribution in all examinations
# #
# lbls <- colnames(FOV_tab_CBCT_total) # create label = FOV size
# lbls <- paste(lbls, "cm") # add cm
# pct_CBCT_total <- round(tail(FOV_tab_CBCT_total,1)/sum(tail(FOV_tab_CBCT_total,1))*100) # generate percent of total FOV size
# pct_CBCT_plus <- round(head(FOV_tab_CBCT_total,1)/sum(head(FOV_tab_CBCT_total,1))*100) # generate percent of total FOV size
# pct_CBCT_minus <- round(FOV_tab_CBCT_total[2,]/sum(FOV_tab_CBCT_total[2,])*100) # generate percent of total FOV size
# lbls_total <- paste(lbls, pct_CBCT_total, sep = ", \n") # add percents to labels
# lbls_total <- paste(lbls_total," %",sep="") # ad % to labels 
# pie(tail(FOV_tab_CBCT_total,1), labels = lbls_total, cex =0.9, main = "FOV distribution with or without CBCT acquisitions")
# dev.print(device = png, file = "output/FOV_CBCT_distribution.png", width = 600, height = 400)
# lbls_plus <- paste(lbls, pct_CBCT_plus, sep = ", \n") # add percents to labels
# lbls_plus <- paste(lbls_plus," %",sep="") # ad % to labels 
# pie(head(FOV_tab_CBCT_total,1), labels = lbls_plus, cex =0.9, main = "FOV distribution with CBCT acquisitions")
# dev.print(device = png, file = "output/FOV_CBCTplus_distribution.png", width = 600, height = 400)
# lbls_minus <- paste(lbls, pct_CBCT_minus, sep = ", \n") # add percents to labels
# lbls_minus <- paste(lbls_minus," %",sep="") # ad % to labels 
# pie(FOV_tab_CBCT_total[2,], labels = lbls_minus, cex =0.9, main = "FOV distribution without CBCT acquisitions")
# dev.print(device = png, file = "output/FOV_CBCTminus_distribution.png", width = 600, height = 400)
# 
# 
# ################ FOV distribution LD+/- #############################
# 
# FOV_LD_table <- table(droplevels(Study_data_prostate$LD), Study_data_prostate$Field.of.View..cm.)
# FOV_LD_table <- FOV_LD_table[,2:5] # suppress FOV = 0 cm column as it's an error of DoseWatch export
# FOV_LD_table_pourcent <- round(prop.table(FOV_LD_table, margin = 1)*100) # table in percent for each exam
# 
# FOV_tab_LD_total <- round(prop.table(addmargins(FOV_LD_table,1),1)*100,1)  # FOV frequencies in percent by patient plus total frequency
# 
# # ### graphic to illustrate FOV distribution in all examinations
# #
# lbls <- colnames(FOV_tab_LD_total) # create label = FOV size
# lbls <- paste(lbls, "cm") # add cm
# pct_LD_total <- round(tail(FOV_tab_LD_total,1)/sum(tail(FOV_tab_LD_total,1))*100) # generate percent of total FOV size
# pct_LD_plus <- round(FOV_tab_LD_total[2,]/sum(FOV_tab_LD_total[2,])*100) # generate percent of total FOV size
# pct_LD_minus <- round(head(FOV_tab_LD_total,1)/sum(head(FOV_tab_LD_total,1))*100) # generate percent of total FOV size
# lbls_total <- paste(lbls, pct_LD_total, sep = ", \n") # add percents to labels
# lbls_total <- paste(lbls_total," %",sep="") # ad % to labels 
# pie(tail(FOV_tab_LD_total,1), labels = lbls_total, cex =0.9, main = "FOV distribution with all displays configurations")
# dev.print(device = png, file = "output/FOV_all_LD_distribution.png", width = 600, height = 400)
# lbls_plus <- paste(lbls, pct_LD_plus, sep = ", \n") # add percents to labels
# lbls_plus <- paste(lbls_plus," %",sep="") # ad % to labels 
# pie(FOV_tab_LD_total[2,], labels = lbls_plus, cex =0.9, main = "FOV distribution with LD configuration")
# dev.print(device = png, file = "output/FOV_LDplus_distribution.png", width = 600, height = 400)
# lbls_minus <- paste(lbls, pct_LD_minus, sep = ", \n") # add percents to labels
# lbls_minus <- paste(lbls_minus," %",sep="") # ad % to labels
# pie(head(FOV_tab_LD_total,1), labels = lbls_minus, cex =0.9, main = "FOV distribution without LD configuration")
# dev.print(device = png, file = "output/FOV_LDminus_distribution.png", width = 600, height = 400)
# 
# 
# ##########################################################################
# ##########################################################################
# ##########################################################################
# ################  statistical analysis by group ##########################
# ##########################################################################
# ##########################################################################
# ##########################################################################
# 
# ################ CBCT+/- analysis #############################
# 
# # global stat part
# global_data_prostate <- Study_data_prostate %>% select(Accession.number, Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
#                                                        Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., Total.Acquisition.DAP..mGy.cm..,
#                                                        Total.Fluoro.DAP..mGy.cm.., Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
#                                                        Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
#                                                        Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
#                                                        Field.of.View..cm., LD, Patient.Age) # to select column of interest and keeping the column's name
# 
# Data_prostate_CBCT_plus <- subset(global_data_prostate, global_data_prostate$Irradiation.Event.Type == "CBCT+") # select only rows with CBCT
# global_data_prostate_CBCT_plus <- Data_prostate_CBCT_plus[!duplicated(Data_prostate_CBCT_plus$Accession.number), ] # to keep only one row per exam to perform global stats
# temp_DAP_fluoro_plus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_plus[,9]))[global_data_prostate_CBCT_plus[,9]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_fluoro_plus) <- c("Total.Fluoro.DAP") # add colnames for labellization
# temp_DAP_graph_plus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_plus[,8]))[global_data_prostate_CBCT_plus[,8]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_graph_plus) <- c("Total.Acquisition.DAP") # add colnames for labellization
# temp_Patient_age_plus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_plus[,20]))[global_data_prostate_CBCT_plus[,20]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_Patient_age_plus) <- c("Patient.age") # add colnames for labellization
# global_data_prostate_CBCT_plus_unique <- global_data_prostate_CBCT_plus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
#                                                                                    Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
#                                                                                    Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
#                                                                                    Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
#                                                                                    Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
# global_data_prostate_CBCT_plus_unique <- cbind(global_data_prostate_CBCT_plus_unique, temp_DAP_fluoro_plus, temp_DAP_graph_plus, temp_Patient_age_plus) # bind all columns for summary
# Stat_CBCT_plus_unique <- summary(global_data_prostate_CBCT_plus_unique)
# write.csv(Stat_CBCT_plus_unique, "output/stats/Stat_CBCT_plus_unique.csv")
# 
# Data_prostate_CBCT_minus <- subset(global_data_prostate, global_data_prostate$Irradiation.Event.Type == "CBCT-") # select only rows without CBCT
# global_data_prostate_CBCT_minus <- Data_prostate_CBCT_minus[!duplicated(Data_prostate_CBCT_minus$Accession.number), ] # to keep only one row per exam to perform global stats
# temp_DAP_fluoro_minus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_minus[,9]))[global_data_prostate_CBCT_minus[,9]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_fluoro_minus) <- c("Total.Fluoro.DAP") # add colnames for labellization
# temp_DAP_graph_minus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_minus[,8]))[global_data_prostate_CBCT_minus[,8]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_graph_minus) <- c("Total.Acquisition.DAP") # add colnames for labellization
# temp_Patient_age_minus <- as.data.frame(as.numeric(levels(global_data_prostate_CBCT_minus[,20]))[global_data_prostate_CBCT_minus[,20]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_Patient_age_minus) <- c("Patient.age") # add colnames for labellization
# global_data_prostate_CBCT_minus_unique <- global_data_prostate_CBCT_minus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Peak.Skin.Dose..mGy., Performing.physician.last.name,
#                                                                                      Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
#                                                                                      Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
#                                                                                      Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
# global_data_prostate_CBCT_minus_unique <- cbind(global_data_prostate_CBCT_minus_unique, temp_DAP_fluoro_minus, temp_DAP_graph_minus, temp_Patient_age_minus) # bind all columns for summary
# Stat_CBCT_minus_unique <- summary(global_data_prostate_CBCT_minus_unique)
# write.csv(Stat_CBCT_minus_unique, "output/stats/Stat_CBCT_minus_unique.csv")
# 
# # Anova_CBCT_minus_unique <- anova(global_data_prostate_CBCT_minus_unique)
# # stat for non unique value (e.g FOV, incidence angle) part
# 
# FOV_angle_data_prostate <- Study_data_prostate %>% select(Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
#                                                           Field.of.View..cm., LD) # to select column of interest and keeping the column's name
# 
# FOV_angle_prostate_CBCT_plus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$Irradiation.Event.Ty == "CBCT+") # select only rows with CBCT
# Stat_CBCT_plus_FOV_angle <- summary(FOV_angle_prostate_CBCT_plus)
# write.csv(Stat_CBCT_plus_FOV_angle, "output/stats/Stat_CBCT_plus_FOV_angle.csv")
# 
# FOV_angle_prostate_CBCT_minus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$Irradiation.Event.Ty == "CBCT-") # select only rows without CBCT
# Stat_CBCT_minus_FOV_angle <- summary(FOV_angle_prostate_CBCT_minus)
# write.csv(Stat_CBCT_minus_FOV_angle, "output/stats/Stat_CBCT_minus_FOV_angle.csv")
# 
# # Anova_CBCT_minus_FOV_angle <- anova(FOV_angle_prostate_CBCT_minus)
# # t.test(IP_dose_colon$Colon[IP_dose_colon$Logiciel == "CT-Expo"], IP_dose_colon$Colon[IP_dose_colon$Logiciel == "Duke Organ Dose"], paired=FALSE)
# # wilcox.test(x, y, paired = TRUE)
# 
# ### Boite à moustache dose absorbée suivant logiciel
# # ggplot(file_ip_4logiciels) + geom_boxplot(aes(x=Logiciel, y=Colon, fill=Logiciel)) + theme(plot.title = element_text(hjust = 0.5))+labs(title="Répartition de la dose absorbée au Côlon (en mGy)\n en fonction des logiciels de calculs de dose", x= "", y ="Dose absorbee (mGy)", size=10)
# # dev.print(device = png, file = "output/ggplot/bam/IP_total/BAM_IP_total_Colon.png", width = 600, height = 400)
# 
# ################ LD+/- analysis #############################
# 
# # global stat part
# global_data_prostate <- Study_data_prostate %>% select(Accession.number, Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
#                                                        Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2., Total.Acquisition.DAP..mGy.cm..,
#                                                        Total.Fluoro.DAP..mGy.cm.., Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
#                                                        Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
#                                                        Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
#                                                        Field.of.View..cm., LD, Patient.Age) # to select column of interest and keeping the column's name
# 
# Data_prostate_LD_plus <- subset(global_data_prostate, global_data_prostate$LD == "LD+") # select only rows with Large Display (LD)
# global_data_prostate_LD_plus <- Data_prostate_LD_plus[!duplicated(Data_prostate_LD_plus$Accession.number), ] # to keep only one row per exam to perform global stats
# temp_DAP_fluoro_plus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_plus[,9]))[global_data_prostate_LD_plus[,9]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_fluoro_plus) <- c("Total.Fluoro.DAP") # add colnames for labellization
# temp_DAP_graph_plus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_plus[,8]))[global_data_prostate_LD_plus[,8]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_graph_plus) <- c("Total.Acquisition.DAP") # add colnames for labellization
# temp_Patient_age_plus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_plus[,19]))[global_data_prostate_LD_plus[,19]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_Patient_age_plus) <- c("Patient.age") # add colnames for labellization
# global_data_prostate_LD_plus_unique <- global_data_prostate_LD_plus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
#                                                                                Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
#                                                                                Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
#                                                                                Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
#                                                                                Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
# global_data_prostate_LD_plus_unique <- cbind(global_data_prostate_LD_plus_unique, temp_DAP_fluoro_plus, temp_DAP_graph_plus, temp_Patient_age_plus) # bind all columns for summary
# Stat_LD_plus_unique <- summary(global_data_prostate_LD_plus_unique)
# write.csv(Stat_LD_plus_unique, "output/stats/Stat_LD_plus_unique.csv")
# 
# 
# Data_prostate_LD_minus <- subset(global_data_prostate, global_data_prostate$LD == "LD-") # select only rows without Large Display (LD)
# global_data_prostate_LD_minus <- Data_prostate_LD_minus[!duplicated(Data_prostate_LD_minus$Accession.number), ] # to keep only one row per exam to perform global stats
# temp_DAP_fluoro_minus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_minus[,9]))[global_data_prostate_LD_minus[,9]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_fluoro_minus) <- c("Total.Fluoro.DAP") # add colnames for labellization
# temp_DAP_graph_minus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_minus[,8]))[global_data_prostate_LD_minus[,8]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_DAP_graph_minus) <- c("Total.Acquisition.DAP") # add colnames for labellization
# temp_Patient_age_minus <- as.data.frame(as.numeric(levels(global_data_prostate_LD_minus[,19]))[global_data_prostate_LD_minus[,19]]) # convert factor with level to a dataframe with numeric value
# colnames(temp_Patient_age_minus) <- c("Patient.age") # add colnames for labellization
# global_data_prostate_LD_minus_unique <- global_data_prostate_LD_minus %>% select(Patient.weight..kg., Patient.size..cm., BMI, Performing.physician.last.name,
#                                                                                  Peak.Skin.Dose..mGy., Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
#                                                                                  Total.Air.Kerma..mGy., Total.Acquisition.Air.Kerma..mGy.,
#                                                                                  Total.Fluoro.Air.Kerma..mGy., Total.Time.of.Fluoroscopy..s., Number.of.Acquisition.Series,
#                                                                                  Irradiation.Event.Type, Field.of.View..cm., LD) # to keep only right columns
# global_data_prostate_LD_minus_unique <- cbind(global_data_prostate_LD_minus_unique, temp_DAP_fluoro_minus, temp_DAP_graph_minus, temp_Patient_age_minus) # bind all columns for summary
# Stat_LD_minus_unique <- summary(global_data_prostate_LD_minus_unique)
# write.csv(Stat_LD_minus_unique, "output/stats/Stat_LD_minus_unique.csv")
# 
# # Anova_LD_minus_unique <- anova(global_data_prostate_LD_minus_unique)
# 
# # stat for non unique value (e.g FOV, incidence angle) part
# 
# FOV_angle_data_prostate <- Study_data_prostate %>% select(Irradiation.Event.Type, Positioner.Primary.Angle..deg., Positioner.Secondary.Angle..deg.,
#                                                           Field.of.View..cm., LD) # to select column of interest and keeping the column's name
# 
# FOV_angle_prostate_LD_plus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$LD == "LD+") # select only rows with Large Display (LD)
# Stat_LD_plus_FOV_angle <- summary(FOV_angle_prostate_LD_plus)
# write.csv(Stat_LD_plus_FOV_angle, "output/stats/Stat_LD_plus_FOV_angle.csv")
# 
# FOV_angle_prostate_LD_minus <- subset(FOV_angle_data_prostate, FOV_angle_data_prostate$LD == "LD-") # select only rows without Large Display (LD
# Stat_LD_minus_FOV_angle <- summary(FOV_angle_prostate_LD_minus)
# write.csv(Stat_LD_minus_FOV_angle, "output/stats/Stat_LD_minus_FOV_angle.csv")
# 
# 
# 
# ################ TODO -LIST ########################
# 
# # A faire :
# # ANNOVA CBCT +/- ; LD +/- HOW ???????
# # t.student CBCT +/- ; LD +/- HOW ???????
# # wilcoxon CBCT +/- ; LD +/- HOW ???????
# # PCA CBCT +/- ~ dose pic ; LD +/- ~ dose pic
# # TSNE CBCT +/- ~ dose pic ; LD +/- ~ dose pic
