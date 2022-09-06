######################################################################
##                                                                  ##                                  
##                      Prostate embolization study                 ##
##                to compare Wide screen vs. small screen           ##
##                                                                  ##                                 
######################################################################

# created by François Gardavaud, MPE, M.Sc. Medical imaging department - Tenon University Hospital
# date of creation : 02/07/2020

# date of the last update : 7/03/2022
# last review : include more patient in the study and generate stats between no and yes groups.
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

# load grateful to list package used
if(!require(grateful)){
  remotes::install_github("Pakillo/grateful")
  library(grateful)
}

# load ggpubr to provide some easy-to-use functions for creating and customizing ‘ggplot2’- based publication ready plots
if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

# load rstatix to plot Boxplots with p-values
if(!require(rstatix)){
  install.packages("rstatix")
  library(rstatix)
}

# load ggeasy to taylor more easyly graphics
if(!require(ggeasy)){
  install.packages("ggeasy")
  library(ggeasy)
}

# load patchwork to allow to addition graphs in a same graph
if(!require(patchwork)){
  install.packages("patchwork")
  library(patchwork)
}

# load DataEditR package to edit easily dataframe and save them in .csv
if(!require(DataEditR)){
  install.packages("DataEditR")
  library(DataEditR)
}
# Save output to R object & csv file
#mtcars_new <- data_edit(mtcars, save_as = "mtcars_new.csv")

# load ggannotate package to eadd label and arrow easy in plot
if(!require(ggannotate)){
  install.packages("ggannotate")
  library(ggannotate)
}
# To launch shyny interface on the last graph (you could also specify the graph in the bracket)
# ggannotate()

# load skimr package to extra information than summary function
if(!require(skimr)){
  install.packages("skimr")
  library(skimr)
}

# load visdat package to visualize type of data in a dataframe
if(!require(visdat)){
  install.packages("visdat")
  library(visdat)
}

# load DataExplorer package to visualize kind of data (string, numeric, factor, ...) in a dataframe
if(!require(DataExplorer)){
  install.packages("DataExplorer")
  library(DataExplorer)
}
#to launch it : create_report(dataframe_name) or create_report(dataframe_name, y = "variable_of_interest")

# load Arsenal package to compute stat analysis with pretty table (in Rmarkdown) 
if(!require(arsenal)){
  install.packages("arsenal")
  library(arsenal)
}



# # load factoshiny for statistical computation aid
# if(!require(Factoshiny)){
#   install.packages("Factoshiny")
#   library(Factoshiny)
# }# load factoshiny for statistical computation aid
# if(!require(FactoMineR)){
#   install.packages("FactoMineR")
#   library(FactoMineR)
# }


# # load Rcommander GUI for basic stat analysis
# if(!require(Rcmdr)){
#   install.packages("Rcmdr")
#   library(Rcmdr)
# }

# # load readxl package to read easily Excel file with an install condition
# if(!require(readxl)){
#   install.packages("readxl")
#   library(readxl)
# }


###############################################################################################################
######################### First part : data pre-treatment #####################################################
###############################################################################################################



############################### data import section ##################################


## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# unfortunately Excel importation yield to parse errors in date data for instance.
# SO TO AVOID THIS PROBLEM THE USER HAVE TO IMPORT DATA IN CSV FORMAT 
# BY CONVERTING ORIGINAL DATA FROM .XLSX TO .CSV IN EXCEL SOFTWARE
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

# the study is based from patient database extracted between 02/01/2018 to 17/02/2022

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

# convert Accession.number column in right format
DoseWatch_Selected_data <- DoseWatch_Selected_data %>%
  mutate(Accession.number = as.character(Accession.number))

# sort each line by Accession number and then by acquisition hour
DoseWatch_Selected_data <- arrange(DoseWatch_Selected_data, Accession.number, Series.Time)

# on corrige les valeurs abérantes d'IMC
DoseWatch_Selected_data <- DoseWatch_Selected_data %>% mutate(BMI = replace(BMI, which(BMI<0), NA))

# on ajoute une classe de BMI aux données patients
BMI=cut(DoseWatch_Selected_data$BMI,c(0,25,Inf),c("< 25","> 25"))
# on peut aussi spécifier si on inclut les bornes inf
# bmi <- cut(patient$BMI,c(0,18.5,25,30,Inf),c("low","medium","high","obese"), include.lowest = TRUE)

# on converti les facteurs en caractères.
BMI=factor(as.character(BMI),levels=c("< 25","> 25"))
# on rajoute à l'object patient une nouvelle colonne "bmi"
DoseWatch_Selected_data$bmi.class=BMI



############### retrieve study patient lines #################

# add a filter to choose dedicated exam for this study
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# confidential part. Use your own patient labeling to this part by replacing 0 numbers.
# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

DoseWatch_Selected_data <- DoseWatch_Selected_data %>% filter(Accession.number == 00000000000 |
                                                                Accession.number == 00000000000 | Accession.number == 00000000000
                                                              )

######################## age patient computation #################################


tic("for loop with parallelization")
if(exists("Study_data_selected_exam_without_duplicates")){
  print("patient age computation have already done")
}else{
  #  instance null vector with appropriate dimension to bind with Study_data
  Patient.Age <- rep(0, nrow(DoseWatch_Selected_data))
  # Loop with parallelization to calculate patient age in years 
  # and add this information to Study_data dataframe
  # also have a condition to test global environment object for debugging
  cores <- detectCores()
  registerDoMC(cores - 1)
  Patient.Age <- foreach(i = 1:nrow(DoseWatch_Selected_data)) %dopar% {
    #naiss = ymd_hms(DoseWatch_Selected_data[i,4]) # deprecated line as mutate function can convert easily "time" column
    #evt = as.POSIXct(DoseWatch_Selected_data[i,1]) # deprecated line as mutate function can convert easily "time" column
    # by suppressing those 2 previous lines and use mutate function instead => Computing acceleration by a factor 6 !!!!!!
    age = as.period(interval(DoseWatch_Selected_data[i,6], DoseWatch_Selected_data[i,2]))@year
    Patient.Age <- age
  }
  Patient.Age <- as.character(Patient.Age)
}
toc()
Study_data_age <-cbind(DoseWatch_Selected_data,Patient.Age)


########## age classification ############

# on convertit la variable Patient.Age en numérique
Study_data_age$Patient.Age <- as.numeric(Study_data_age$Patient.Age)

# on ajoute une classe d'âge aux données patients
Age <- cut(Study_data_age$Patient.Age,c(0,70,Inf),c("< 70","> 70"))

# on converti les facteurs en caractères.
Age <- factor(as.character(Age),levels=c("< 70","> 70"))
# on rajoute à l'object patient une nouvelle colonne "bmi"
Study_data_age$Age.class=Age

Study_data_selected_age <- Study_data_age %>% select(Patient.last.name, Study.date..YYYY.MM.DD., Series.Time, Patient.ID, Accession.number,
                                                     Patient.Age, Age.class,
                                                     Patient.birthdate..YYYY.MM.DD.,
                                                     Patient.weight..kg., Patient.size..cm.,
                                                     BMI, bmi.class, Standard.study.description,
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

Study_data_selected_age$Total.Acquisition.DAP..mGy.cm.. <- as.numeric(Study_data_selected_age$Total.Acquisition.DAP..mGy.cm..)
Study_data_selected_age$Total.Fluoro.DAP..mGy.cm.. <- as.numeric(Study_data_selected_age$Total.Fluoro.DAP..mGy.cm..)

# sort each line by Study date and then by acquisition hour
Study_data_selected_exam <- arrange(Study_data_selected_age, Study.date..YYYY.MM.DD., Series.Time)

############### Compute Exam duration #################

#create a data frame with only the first and last row for each Accession number
Study_data_selected_exam_duration <- Study_data_selected_exam %>% 
  group_by(Accession.number) %>%
  slice(c(1, n())) %>% # select only the first and last row for each group value.
  mutate (Series.Time.Minute = (Series.Time@hour)*60 + Series.Time@minute) %>% # convert Series.Time in minutes
  ungroup()


#add two columns with time difference between first and second row for each exam in hour and in minute.
Study_data_selected_exam_duration <- Study_data_selected_exam_duration %>% 
  group_by(Accession.number) %>%
  dplyr::mutate(
    Exam.duration.Hour = (dplyr::last(Series.Time) - dplyr::first(Series.Time)),
    Exam.duration.Minute = (dplyr::last(Series.Time.Minute) - dplyr::first(Series.Time.Minute))
  )

# suppress unnecessary column in order to merge data in one dataframe
Study_data_selected_exam_duration_filtered <- Study_data_selected_exam_duration %>% select(Accession.number, Exam.duration.Hour, Exam.duration.Minute)
# Remove duplicates based on Accession number
Study_data_selected_exam_duration_filtered <- Study_data_selected_exam_duration_filtered[!duplicated(Study_data_selected_exam_duration_filtered$Accession.number), ] # to keep only one row for each exam time.

# merge data between big dataframe and dataframe with only exam duration
Study_data_selected_exam_with_duration <- merge(Study_data_selected_exam , Study_data_selected_exam_duration_filtered, by.x = "Accession.number", by.y = "Accession.number")
# sort each line by Accession number and then by acquisition hour
Study_data_selected_exam <- arrange(Study_data_selected_exam_with_duration, Accession.number, Series.Time)
# remove Study_data_selected_exam_with_duration dataframe
rm(Study_data_selected_exam_with_duration)

####### VesselAssist software using (VA) +/- labelization  ###########################


#  create new levels as +/- VA
print("VesselAssist have been installed since 2020-11-01")
print("All exams generated after 2020-10-31 are considered as performed with VesselAssist software")
VA <- Study_data_selected_exam$Study.date..YYYY.MM.DD.
VA <- gsub("[: -]", "" , VA, perl=TRUE) # delete ":" , " ", and "-" from the date value
VA <- as.numeric(VA) # convert VA in numeric format to apply cut function
VA <- cut(VA,c(20180102,20201031, Inf),c("no","yes")) # cut to segment date between yes and no 
VA <- factor(as.character(VA),levels=c("no","yes")) # convert factor in character
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
                                                                                                      Patient.Age, Age.class,
                                                                                                      Patient.weight..kg., Patient.size..cm.,
                                                                                                      BMI, bmi.class,
                                                                                                      Peak.Skin.Dose..mGy.,
                                                                                                      Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                                                      Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                                                                      Total.Air.Kerma..mGy.,
                                                                                                      Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                                                                      Total.Time.of.Fluoroscopy..s., Exam.duration.Minute, Number.of.Acquisition.Series,
                                                                                                      Dose.Preference, VA)

################## Global environment cleaning ###########################

# Remove dataframe which don't still have any interest
if(exists("VA")) {
  print("Global environment will be clean")
  if(exists("all_content")){
    rm (DoseWatch_Selected_data, Study_data_selected_age, Study_data_age, all_content, skip_content, VA)
  }else{
    rm (DoseWatch_Selected_data, Study_data_selected_age, Study_data_age, VA)
  }}else{
    print("Global environment already clean")
  }

###############################################################################################################
######################### Second part : Statistical computation ###############################################
###############################################################################################################


############# Find avariable of interest with PCA method

# selected only interested columns based on PCA (cos2 > 0.6) computed thanks to Factoshiny package 
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
# execute manually the following line to launch correctly Factoshiny
# as the ~ character in path file/dataframe is not correctly recognize by Factoshiny/FactoMiner package
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
## /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\


#print("Launch Factoshiny graphical interface")
#Factoshiny(Study_data_selected_exam_without_duplicates_factoshiny)

# Factoshiny output command lines after manual treatment
#nb <- missMDA::estim_ncpPCA(Study_data_selected_exam_without_duplicates_factoshiny,quali.sup=c(8))$ncp
#dfcompleted <- missMDA::imputePCA(Study_data_selected_exam_without_duplicates_factoshiny,ncp=nb,quali.sup=c(8))$completeObs
#res.PCA<-PCA(dfcompleted,quali.sup=c(8),graph=FALSE)
#plot.PCA(res.PCA,choix='var',habillage = 'contrib',title="Graphe des variables de l'ACP")
#plot.PCA(res.PCA,invisible=c('ind.sup'),select='cos2  0.7',habillage='Peak.Skin.Dose..mGy.',title="Graphe des individus de l'ACP",cex=1.05,cex.main=1.05,cex.axis=1.05,label =c('quali'))






################ global statistical analysis #############################

Study_data_selected_exam_without_duplicates_boxplot <- Study_data_selected_exam_without_duplicates %>% select(Accession.number,
                                                                                                              Age.class,
                                                                                                              bmi.class,
                                                                                                              Peak.Skin.Dose..mGy.,
                                                                                                              Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2.,
                                                                                                              Total.Acquisition.DAP..mGy.cm..,Total.Fluoro.DAP..mGy.cm..,
                                                                                                              Total.Air.Kerma..mGy.,
                                                                                                              Total.Acquisition.Air.Kerma..mGy., Total.Fluoro.Air.Kerma..mGy.,
                                                                                                              Total.Time.of.Fluoroscopy..s., Exam.duration.Minute, Number.of.Acquisition.Series,
                                                                                                              VA)


# Converted the values in the first column (e.g patient names or accession number if you have duplicated patient) into row names in the dataframe for Factoshiny computation
Study_data_selected_exam_without_duplicates_boxplot <- data.frame(Study_data_selected_exam_without_duplicates_boxplot[,-1], row.names=Study_data_selected_exam_without_duplicates_boxplot[,1])




globalstat <- summary(Study_data_selected_exam_without_duplicates)
globalstat_skim <- skim(Study_data_selected_exam_without_duplicates)

# write excel file to communicate with other people
print("Create output Excel file for data table and main statistics")
write.xlsx(Study_data_selected_exam_without_duplicates, "output/v2/Study_data_treated.xlsx", sheetName = "Study_data_treated",
           col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
write.xlsx(globalstat, "output/v2/globalstat.xlsx", sheetName = "globalstat",
           col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
write.xlsx(globalstat_skim, "output/v2/globalstat_skim.xlsx", sheetName = "globalstat_skim",
           col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)



# ################ group statistical analysis #############################
# 
# # compute classic statistical analysis for no group only
# globalstat_VA_no <- summary(Study_data_selected_exam_without_duplicates_boxplot %>% filter(VA == "no"))
# globalstat_VA_no_skim <- skim(Study_data_selected_exam_without_duplicates_boxplot %>% filter(VA == "no"))
# 
# # compute classic statistical analysis for yes group only
# globalstat_VA_yes <- summary(Study_data_selected_exam_without_duplicates_boxplot %>% filter(VA == "yes"))
# globalstat_VA_yes_skim <- skim(Study_data_selected_exam_without_duplicates_boxplot %>% filter(VA == "yes"))
# 
# 
# # write excel file to communicate with other people
# print("Create output Excel file for data table and main statistics for no and yes group")
# write.xlsx(globalstat_VA_no, "output/v2/globalstat_VA_no.xlsx", sheetName = "globalstat_VA_no",
#            col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
# write.xlsx(globalstat_VA_no_skim, "output/v2/globalstat_VA_no_skim.xlsx", sheetName = "globalstat_VA_no_skim",
#            col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
# write.xlsx(globalstat_VA_yes, "output/v2/globalstat_VA_yes.xlsx", sheetName = "globalstat_VA_yes",
#            col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
# write.xlsx(globalstat_VA_yes_skim, "output/v2/globalstat_VA_yes_skim.xlsx", sheetName = "globalstat_VA_yes_skim",
#            col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


# set-up configuration for statistics tables with arsenal package
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  )
)

# define label for statistics tables with arsenal package
my_labels <- list(
  VA = "Vessel Assist",
  Peak.Skin.Dose..mGy. = "PSD (mGy)",
  Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2. = "Total DAP (mGy.cm^2)",
  Total.Acquisition.DAP..mGy.cm.. = "Acquisition DAP (mGy.cm^2)",
  Total.Fluoro.DAP..mGy.cm.. = "Fluoro DAP (mGy.cm^2)",
  Total.Air.Kerma..mGy. = "Total Air Kerma (mGy)",
  Total.Acquisition.Air.Kerma..mGy.  = "Acquisition Air Kerma (mGy)",
  Total.Fluoro.Air.Kerma..mGy. = "Fluoro Air Kerma (mGy)",
  Total.Time.of.Fluoroscopy..s. = "FT (s)",
  Number.of.Acquisition.Series = "Nacq"
)

# Summarize without a group (by) variable
tab.noby <- tableby(~ Age.class + bmi.class + VA + Peak.Skin.Dose..mGy. + Image.and.Fluoroscopy.Dose.Area.Product..mGy.cm2. + Total.Acquisition.DAP..mGy.cm.. +
                      Total.Fluoro.DAP..mGy.cm.. + Total.Air.Kerma..mGy. + Total.Acquisition.Air.Kerma..mGy. + Total.Fluoro.Air.Kerma..mGy. +
                      Total.Time.of.Fluoroscopy..s. + Exam.duration.Minute + Number.of.Acquisition.Series, data=Study_data_selected_exam_without_duplicates_boxplot, control = my_controls)
cohort_stat <- summary(tab.noby, text = TRUE, pfootnote=TRUE, labelTranslations = my_labels, title = "Radiation exposure data — total cohort", digits=1, digits.p=2, digits.pct=1)

## write to a Word document
write2word(cohort_stat, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/Total_cohort_stat.docx", title="Radiation exposure data — total cohort")
write2pdf(cohort_stat, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/Total_cohort_stat.pdf", title="Radiation exposure data — total cohort")


# table_VA <- tableby(VA ~ ., data = Study_data_selected_exam_without_duplicates_boxplot) 
# write2word(table_VA, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/VA_Univariate_analysis.docx", title="Radiation exposure data—results of VA univariate analysis")
# 
# table_BMI <- tableby(bmi.class ~ ., data = Study_data_selected_exam_without_duplicates_boxplot) 
# write2word(table_BMI, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/BMI_Univariate_analysis.docx", title="Radiation exposure data—results of BMI univariate analysis")
# 
# table_Age <- tableby(Age.class ~ ., data = Study_data_selected_exam_without_duplicates_boxplot) 
# write2word(table_Age, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/Age_Univariate_analysis.docx", title="Radiation exposure data—results of Age univariate analysis")


table_all <- tableby(list(Age.class, bmi.class, VA) ~ ., data = Study_data_selected_exam_without_duplicates_boxplot, control = my_controls) 
univariate_analysis <- summary(table_all, text = TRUE, pfootnote=TRUE, labelTranslations = my_labels, title = "Radiation exposure data—results of univariate analysis", digits=0, digits.p=2, digits.pct=1, term.name = TRUE)
write2word(univariate_analysis, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/Univariate_analysis.docx", title="Radiation exposure data—results of univariate analysis")
write2pdf(univariate_analysis, "~/Documents/R/Gitlab_APHP/prostate_embolization_APHP/output/v2/Univariate_analysis.pdf", title="Radiation exposure data—results of univariate analysis")


# ----------------- graphical analysis section --------------------


# heatmap correlation with DataExplorer package
# /!\/!\/!\/!\/!\/!\ becareful with label group for BMI and Age if they contain special character /!\/!\/!\/!\/!\/!\/!\
# 1. Open jpeg file
jpeg("output/v2/heatmap_correlation.jpg", width = 937, height = 738)
# 2. Create the plot
plot_correlation(na.omit(Study_data_selected_exam_without_duplicates_boxplot))
# 3. Close the file
dev.off()





## ####################### Miscellaneous #####################################
# Create word document to list package citation
cite_packages(out.format = "docx")




## ####################### TO-DO List #####################################

# faire 1 box plot avec test stat krusky-Wallis de comparaison des groupes pour chaque indicateur de dose => prendre en modèle l'étude Duke Organ Dose ...
# se servir de gganotate pour l'annotation des graphes
# ... assembler les différents graphes avec patchwork package si besoin.







