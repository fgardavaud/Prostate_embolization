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

# the study is based from patient database extracted between 02/01/2018 to 1/10/2021

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
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# confidential part. Use your own patient labeling to this part by replacing 0 numbers.
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\


Study_data_selected_exam <- Study_data_selected_age %>% filter(Accession.number == 0000000000 |
                                                                 Accession.number == 000000000 ) 

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

print("Launch Factoshiny graphical interface")
Factoshiny(Study_data_selected_exam_without_duplicates_factoshiny)

# Factoshiny output command lines post manual treatment
# nb <- missMDA::estim_ncpPCA(Study_data_selected_exam_without_duplicates_factoshiny,quali.sup=c(8))$ncp
# dfcompleted <- missMDA::imputePCA(Study_data_selected_exam_without_duplicates_factoshiny,ncp=nb,quali.sup=c(8))$completeObs
# res.PCA<-PCA(dfcompleted,quali.sup=c(8),graph=FALSE)
# plot.PCA(res.PCA,choix='var',habillage = 'contrib',title="Graphe des variables de l'ACP")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select='cos2  0.7',habillage='Peak.Skin.Dose..mGy.',title="Graphe des individus de l'ACP",cex=1.05,cex.main=1.05,cex.axis=1.05,label =c('quali'))


