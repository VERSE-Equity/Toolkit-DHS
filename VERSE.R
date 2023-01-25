######################################################
# Vaccine Economics Research for Sustainability and Equity (VERSE)
# VERSE Equity Toolkit
######################################################
rm(list=ls()) # clear


# IMPORTANT
# Search for "ACTION NEEDED" to find where the model needs your input



# ACTION NEEDED: Set Your Working Directory (where the model outputs, i.e. figures and tables, will be saved)
setwd("YOUR DIRECTORY HERE")


##### VERSE Equity Tool Inputs #####

# ACTION NEEDED: Choose the country and DHS year based on the country list: https://dhsprogram.com/Countries/
COUNTRY <- "Madagascar"
YEAR <- 2021


# ACTION NEEDED: Leave default vaccines or add/remove vaccines based on the list below:
# List of vaccines: BCG, DTP1, DTP2, DTP3, POLIO1, POLIO2, POLIO3, MCV1, MCV2, PolioBD, HEPBBD, HEPB1, HEPB2, HEPB3, PENTA1, PENTA2, PENTA3, PCV1, PCV2, PCV3, ROTA1, ROTA2, ROTA3, HIB1, HIB2, HIB3, OPV1, OPV2, OPV3, IPV1, IPV2, IPV3, FULL, ZERO, COMPLETE
VACCINES <- c("BCG","DTP1","DTP2","DTP3","POLIO1","POLIO2","POLIO3","MCV1","ZERO","FULL","COMPLETE")


# ACTION NEEDED: Change whether you want maps to be generated ("YES" --> more processing time)
MAP = "YES"

# Other model inputs (keep default unless more advanced programming is required)
FACTORS  <-c("region","rural","education","wealth","sex","insurance")
SCHEDULE <-c("DEFAULT")
DATA     <- "DHS"
GEO      <- "District"


# TROUBLESHOOTING: If error due to file not being able to unzip
# Type in the following:
# get_available_datasets(clear_cache=TRUE)


##### R Packages Installation #####
# Before running the program and install & load the packages, please ensure your R and R-Studio are up-to-date
if(!require(usethis)) install.packages("usethis", repos = "http://cran.us.r-project.org")
if(!require(rmapshaper)) install.packages("rmapshaper", repos = "http://cran.us.r-project.org")
if(!require(PHEindicatormethods)) install.packages("PHEindicatormethods", repos = "http://cran.us.r-project.org")
if(!require(radiant)) install.packages("radiant", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(rdhs)) install.packages("rdhs", repos = "http://cran.us.r-project.org")
#if(!require(rdhs)) devtools::install_github("ropensci/rdhs")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(margins)) install.packages("margins", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(rineq)) devtools::install_github("brechtdv/rineq")
if(!require(labelled)) install.packages("labelled", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggforce)) install.packages("ggforce", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(prevR)) install.packages("prevR", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(matchmaker)) install.packages("matchmaker", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")



##### Connection to DHS database (require Internet connection) #####


# ACTION NEEDED: Enter "1" in Console when prompted ("1" = YES)

set_rdhs_config(email = "ewatts13@jhu.edu",
                project = "Vaccine Economics Research for Sustainability and Equity (VERSE)",
                config_path = "~/.rdhs.json",
                global = TRUE)
1

# Enter password: verseteam


##### VERSE Function #####

VERSE <- function(DATA,COUNTRY,YEAR,VACCINES,SCHEDULE,FACTORS,GEO,MAP) { 
  
  #Mute unnecessary warnings
  options(dplyr.summarise.inform = FALSE)
  oldw <- getOption("warn")
  options(warn = -1)
  
  # Connect with DHS and import the data 
  if (DATA[1]=="DHS"){
    
    DATA1<- DATA[1]
    
    # Capture country IDs  
    ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))  
    invisible(capture.output(str(ids)))
    
    # Collect VERSE COUNTRY input to use to search for surveys
    newid<- subset(ids, ids$CountryName==COUNTRY)
    country_id<- c(newid$DHS_CountryCode)
    
    # Clear current cache to prevent downlad errors
    get_available_datasets(clear_cache=TRUE)
    
    # Find all the surveys that fit our search criteria
    survs <- dhs_surveys(countryIds = country_id,
                         surveyType = "DHS",
                         surveyYearStart = YEAR)
    
    #Save Survey ID
    mapid<- toString(survs[1,2])
    
    # Download corresponding geographic mapping data
    if (MAP=="YES"){
      mapping <- download_boundaries(surveyId = mapid, method = "sf", quiet_download = TRUE)
    }
    
    # Store relevant dataset names
    datasets <- dhs_datasets(surveyIds = survs$SurveyId, 
                             fileFormat = "flat")
    
    invisible(capture.output(str(datasets)))
    
    # Select the Children's Recode version of dataset
    datasets <- filter(datasets, FileType == "Children's Recode")
    
    # Download the DHS file names
    downloads <- get_datasets(datasets$FileName)
    
    # Download the correct DHS Children's Recode Version
    DATA <- readRDS(downloads[[1]])
    
  } else{
    # If Data is not DHS and you are uploading your own data use that instead
    DATA1<- DATA[1]
    DATA <- DATA
    
    # Find geographic shape files for specified country 
    ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))  
    invisible(capture.output(str(ids)))
    
    # Store Country IDs  
    newid<- subset(ids, ids$CountryName==COUNTRY)
    country_id<- c(newid$DHS_CountryCode)
    
    # Find all the surveys that fit our search criteria
    survs <- dhs_surveys(countryIds = country_id,
                         surveyType = "DHS",
                         surveyYearStart = YEAR)
    # Save Survey ID
    mapid<- toString(survs[2])
    
    # Download corresponding geographic mapping data
    if (MAP=="YES"){
      mapping <- download_boundaries(surveyId = mapid, method = "sf", quiet_download = TRUE)
    }
  }
  
  # Create Storage Vectors & Dataframes
  output <- data.frame(matrix(NA, nrow = length(FACTORS)+3, ncol = 1))
  output_list <- list()
  efficiency_list <- list()
  map_list <- list()
  
  # Fix Mapping Data for countries with Errors in geographic indicator formatting
  if (COUNTRY=="Bangladesh"){
    mapping$sdr_subnational_boundaries$REGCODE <- as.numeric(c(1,2,3,4,5,6,7,8))
  }
  
  #Fix b19 age data for countries where it is missing
  if ((is.na(summary(DATA$b19)[4])=="TRUE")&(is.na(summary(DATA$hw1)[4])=="TRUE")) {
    DATA <- DATA %>% mutate("b19" = DATA$v008 - DATA$b3)
  }
  
  # Create shell vectors for all national-level outputs
  CI_Results<- c()
  CI_Results_95ciLB<- c()
  CI_Results_95ciUB<- c()
  CI_Results<- c()
  HII_Results<- c()
  HII_Results_95ciLB<- c()
  HII_Results_95ciUB<- c()
  CI_E_Results<- c()
  CI_E_Results_95ciLB<- c()
  CI_E_Results_95ciUB<- c()
  AEG_Composite_Results<- c()
  AEG_Composite_Results_95ciLB<- c()
  AEG_Composite_Results_95ciUB<- c()
  AEG_Wealth_Results<- c()
  AEG_Wealth_Results_95ciLB<- c()
  AEG_Wealth_Results_95ciUB<- c()
  AEG_Sex_Results<-c()
  AEG_Rural_Results<-c()
  AEG_Insurance_Results<-c()
  REG_Sex_Results<-c()
  REG_Rural_Results<-c()
  REG_Insurance_Results<-c()
  SII_Education_Results<-c()
  SII_Wealth_Results<-c()
  SII_Region_Results<-c()
  RII_Education_Results<-c()
  RII_Wealth_Results<-c()
  RII_Region_Results<-c()
  CI_Wealth_Results<- c()
  CI_Wealth_Results_95ciLB<- c()
  CI_Wealth_Results_95ciUB<- c()
  CI_E_Wealth_Results<- c()
  CI_E_Wealth_Results_95ciLB<- c()
  CI_E_Wealth_Results_95ciUB<- c()
  Coverage_Results<- c()
  Coverage_Results_95ciLB<- c()
  Coverage_Results_95ciUB<- c()
  
  # Reduce the data size (if you are using the DHS, only)
  if (DATA1=="DHS"){
    # Keep selected variables only
    default_keep <- c("caseid", "v001", "v002", "v003", "v005", "v012", "v013", "v020", "v024", 
                      "v025", "v101", "v106", "v107", "v133", "v137", "v136", "v151", "v152", 
                      "v155", "v190", "v191", "v202", "v203", "v481", "v501", "v701", "v702", "v131", "v130",
                      "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h9a", "h0", "h10", "hw1",
                      "h50", "h61", "h62", "h63", "h51", "h52", "h53", "h54", "h55", "h56", "h57", "h58",
                      "h59", "h60", "h64", "h65", "h66", "hep0", "hep1", "hep2", "hep3", "s515", "b5", "b4","b8","b9", "b19", "sstate", "sdistri", "sslumc", "sslumo", 
                      "sd005","s052", "s190s", "s190u", "s191u", "s190us", "s190r", "s191r", "s190rs", "sv005", "s515")
    
    # Replace dataset iin memory with smaller dataset
    dhs_data <- DATA %>% select(any_of(default_keep))
  } else {
    dhs_data <- DATA
  }
  
  # Notes for automatic upload of Country-Specific Schedules
  # Load Vaccine Schedule Data (Need to make sure setwd() is specified before running VERSE)
  # schedule <- read_csv("WHO.vaxsched.csv")
  
  #Reduce schedule dataset to only selected country & vaccines specified
  # schedule <- schedule %>% select(any_of(c(VACCINES,"Country")))
  # schedule <- schedule[schedule$Country %in% c(COUNTRY),]
  
  #Fix Pakistan's Obscure weighting system
  if ((COUNTRY=="Pakistan")&(YEAR==2016)){
    dhs_data <- dhs_data %>% mutate("v005" = dhs_data$v005+dhs_data$sv005)
  }
  
  # Create Vaccine Schedule from WHO General EPI Guidance
  if (SCHEDULE[1]=="DEFAULT"){
    schedule<-as.data.frame(matrix(as.numeric(c(0,      2,     3,     4,      2,       3,       4,      2,     3,     4,      2,     3,    4,     12,    24,      0,        2,    3,     4,     2,     3,     4,      2,      3,      4,     0,        2,     3,      4,      5,   108,   6,    7,    6,     24,         24,        24,      18,    12,    12,      2,        3,       4,       12,     0,      24)),nrow=1))
    schedule_names<-                          c("BCG","DTP1","DTP2","DTP3","POLIO1","POLIO2","POLIO3","OPV1","OPV2","OPV3","IPV1","IPV2","IPV3","MCV1","MCV2","PolioBD","HIB1","HIB2","HIB3","PCV1","PCV2","PCV3","ROTA1","ROTA2","ROTA3","HEPBBD","HEPB1","HEPB2","HEPB3","HEPB4","HPV","JE1","JE2","TCV","CHOLERA1","CHOLERA2","CHOLERA3","MENA","MENC","HEPA","PENTA1","PENTA2","PENTA3", "ZERO", "FULL", "COMPLETE")
    colnames(schedule) <- schedule_names
  } else {
    # Create Schedule from input Vector (must align with VACCINES() input vector and be age in months)
    schedule<-as.data.frame(matrix(as.numeric(SCHEDULE),nrow=1))
    schedule_names<-VACCINES                          
    colnames(schedule) <- schedule_names
  }
  
  # Create Age in Months Variable for Children who are Alive only
  # Note in some datasets b19 holds complete child age info in others it is hw1
  # Identfies if approriate variable is b19 or hw1 and renames the correct variable to hw1 
  if (((length(dhs_data$b19))!= 0)&(is.na(summary(dhs_data$b19)[4])!="TRUE")){
    dhs_data <- dhs_data %>% mutate("hw1" = dhs_data$b19)
    dhs_data$hw1<- replace(dhs_data$hw1,dhs_data$b5 ==0,NA)
  }
  
  #Defining fair inequity based upon Vaccine Schedule by creating vaccine-specific underage cutoffs
  for (i in VACCINES){
    dhs_data$q<- ifelse(dhs_data$hw1 < schedule[,i][1], 1, 
                        ifelse(dhs_data$hw1>=schedule[,i][1], 0, 0))
    dhs_data$q<- replace(dhs_data$q,is.na(dhs_data$q),0)
    names(dhs_data)[names(dhs_data) == "q"] <- paste("underage_",i, sep="")
  }
  
  #Create binary IPV=1 vs. OPV=0 indicator for Polio since h4, h6, and h8 can be either OPV or IPV 
  if ((is.null(dhs_data$h60)[1]=="TRUE")|(table(is.na(dhs_data$h60))[1]==length(dhs_data$h60))){
    dhs_data <- dhs_data %>% mutate("IPVind" = 0)
  }else {
    dhs_data <- dhs_data %>% mutate("IPVind" = ifelse(dhs_data$h60 ==0 | dhs_data$h60 >=8, 0,
                                                      ifelse(dhs_data$h60 ==1 | dhs_data$h60 ==2 | dhs_data$h60 ==3, 1,
                                                             "NA")))}
  
  # Fix Missing Values in determinants:
  if ((max(dhs_data$v106)[1]>=6)&(COUNTRY!="Yemen")){
    dhs_data <- dhs_data %>% mutate("v106" = ifelse(dhs_data$v106>=6,NA,dhs_data$v106))
  }
  
  if (is.na(summary(dhs_data$v481)[4])=="TRUE"){
    dhs_data <- dhs_data %>% mutate("v481" = 0)
  }
  
  if (summary(dhs_data$v481)[6] >=8){
    dhs_data <- dhs_data %>% mutate("v481" = ifelse(dhs_data$v481>=8,0,dhs_data$v481))
  }
  
  if ((COUNTRY=="Uganda")&(YEAR<2005)){
    dhs_data$v190<-dhs_data$s052
  }
  
  #Create Vaccine-Specific Outcomes
  
  # BCG (1 = received, 0 = did not recive, NA = no data)
  if ("BCG" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("BCG" = ifelse(dhs_data$h2 ==0 | dhs_data$h2 >=8, 0,
                                                   ifelse(dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3, 1,
                                                          "NA")))}
  # DTP1 (1 = received, 0 = did not recive, NA = no data)
  if ("DTP1" %in% VACCINES){
    if (is.na(summary(dhs_data$h3)[4])=="TRUE"){
      dhs_data$h3 <- dhs_data$h51
    }
    dhs_data <- dhs_data %>% mutate("DTP1" = ifelse(dhs_data$h3 ==0 | dhs_data$h3 >=8, 0,
                                                    ifelse(dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3, 1,
                                                           "NA")))}
  
  # POLIO1 (1 = received, 0 = did not recive, NA = no data)
  if ("POLIO1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("POLIO1" = ifelse(((dhs_data$h4 ==0 | dhs_data$h4 >=8) & (dhs_data$IPVind ==0))|((dhs_data$h4 ==0 | dhs_data$h4 >=8) & (dhs_data$IPVind ==1)), 0,
                                                      ifelse((dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & ((dhs_data$IPVind ==0)| (dhs_data$IPVind ==1)), 1,
                                                             "NA")))}
  
  # OPV1 (1 = received, 0 = did not recive, NA = no data)
  if ("OPV1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("OPV1" = ifelse((dhs_data$h4 ==0 | dhs_data$h4 >=8) & (dhs_data$IPVind ==0), 0,
                                                    ifelse((dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$IPVind ==0), 1,
                                                           "NA")))}
  # DTP2 (1 = received, 0 = did not recive, NA = no data)
  if ("DTP2" %in% VACCINES){
    if (is.na(summary(dhs_data$h5)[4])=="TRUE"){
      dhs_data$h3 <- dhs_data$h52
    }
    dhs_data <- dhs_data %>% mutate("DTP2" = ifelse(dhs_data$h5 ==0 | dhs_data$h5 >=8, 0,
                                                    ifelse(dhs_data$h5 ==1 | dhs_data$h5 ==2 | dhs_data$h5 ==3, 1,
                                                           "NA")))}
  
  # POLIO2 (1 = received, 0 = did not recive, NA = no data)
  if ("POLIO2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("POLIO2" = ifelse(((dhs_data$h6 ==0 | dhs_data$h6 >=8) & (dhs_data$IPVind ==0))|((dhs_data$h6 ==0 | dhs_data$h6 >=8) & (dhs_data$IPVind ==1)), 0,
                                                      ifelse((dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & ((dhs_data$IPVind ==0)| (dhs_data$IPVind ==1)), 1,
                                                             "NA")))}
  
  # OPV2 (1 = received, 0 = did not recive, NA = no data)
  if ("OPV2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("OPV2" = ifelse((dhs_data$h6 ==0 | dhs_data$h6 >=8) & (dhs_data$IPVind ==0), 0,
                                                    ifelse((dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & (dhs_data$IPVind ==0), 1,
                                                           "NA")))}
  # DTP3 (1 = received, 0 = did not recive, NA = no data)
  if ("DTP3" %in% VACCINES){
    if (is.na(summary(dhs_data$h7)[4])=="TRUE"){
      dhs_data$h3 <- dhs_data$h53
    }
    dhs_data <- dhs_data %>% mutate("DTP3" = ifelse(dhs_data$h7 ==0 | dhs_data$h7 >=8, 0,
                                                    ifelse(dhs_data$h7 ==1 | dhs_data$h7 ==2 | dhs_data$h7 ==3, 1,
                                                           "NA")))}
  
  # POLIO3 (1 = received, 0 = did not recive, NA = no data)
  if ("POLIO3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("POLIO3" = ifelse(((dhs_data$h8 ==0 | dhs_data$h8 >=8) & (dhs_data$IPVind ==0))|((dhs_data$h8 ==0 | dhs_data$h8 >=8) & (dhs_data$IPVind ==1)), 0,
                                                      ifelse((dhs_data$h8 ==1 | dhs_data$h8 ==2 | dhs_data$h8 ==3) & ((dhs_data$IPVind ==0)| (dhs_data$IPVind ==1)), 1,
                                                             "NA")))}
  
  # OPV3 (1 = received, 0 = did not recive, NA = no data)
  if ("OPV3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("OPV3" = ifelse((dhs_data$h8 ==0 | dhs_data$h8 >=8) & (dhs_data$IPVind ==0), 0,
                                                    ifelse((dhs_data$h8 ==1 | dhs_data$h8 ==2 | dhs_data$h8 ==3) & (dhs_data$IPVind ==0), 1,
                                                           "NA")))}
  # MCV1 (1 = received, 0 = did not recive, NA = no data)
  if ("MCV1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("MCV1" = ifelse(dhs_data$h9 ==0 | dhs_data$h9 >=8, 0,
                                                    ifelse(dhs_data$h9 ==1 | dhs_data$h9 ==2 | dhs_data$h9 ==3, 1,
                                                           "NA")))}
  # MCV2 (1 = received, 0 = did not recive, NA = no data)
  if ("MCV2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("MCV2" = ifelse(dhs_data$h9a ==0 | dhs_data$h9a >=8, 0,
                                                    ifelse(dhs_data$h9a ==1 | dhs_data$h9a ==2 | dhs_data$h9a ==3, 1,
                                                           "NA")))}
  # Polio Birth Dose (1 = received, 0 = did not recive, NA = no data)
  if ("PolioBD" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PolioBD" = ifelse(dhs_data$h0 ==0 | dhs_data$h0 >=8, 0,
                                                       ifelse(dhs_data$h0 ==1 | dhs_data$h0 ==2 | dhs_data$h0 ==3, 1,
                                                              "NA")))}
  # Hep B Birth Dose (1 = received, 0 = did not recive, NA = no data)
  if ("HEPBBD" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HEPBBD" = ifelse(dhs_data$h50 ==0 | dhs_data$h50 >=8, 0,
                                                      ifelse(dhs_data$h50 ==1 | dhs_data$h50 ==2 | dhs_data$h50 ==3, 1,
                                                             "NA")))}
  # Hep B1 (1 = received, 0 = did not recive, NA = no data)
  if ("HEPB1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HEPB1" = ifelse(dhs_data$h61 ==0 | dhs_data$h61 >=8, 0,
                                                     ifelse(dhs_data$h61 ==1 | dhs_data$h61 ==2 | dhs_data$h61 ==3, 1,
                                                            "NA")))}
  # Hep B2 (1 = received, 0 = did not recive, NA = no data)
  if ("HEPB2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HEPB2" = ifelse(dhs_data$h62 ==0 | dhs_data$h62 >=8, 0,
                                                     ifelse(dhs_data$h62 ==1 | dhs_data$h62 ==2 | dhs_data$h62 ==3, 1,
                                                            "NA")))}
  # Hep B3 (1 = received, 0 = did not recive, NA = no data)
  if ("HEPB3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HEPB3" = ifelse(dhs_data$h63 ==0 | dhs_data$h63 >=8, 0,
                                                     ifelse(dhs_data$h63 ==1 | dhs_data$h63 ==2 | dhs_data$h63 ==3, 1,
                                                            "NA")))}
  # Pentavalent 1 (1 = received, 0 = did not recive, NA = no data)
  if ("PENTA1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PENTA1" = ifelse(dhs_data$h51 ==0 | dhs_data$h51 >=8, 0,
                                                      ifelse(dhs_data$h51 ==1 | dhs_data$h51 ==2 | dhs_data$h51 ==3, 1,
                                                             "NA")))}
  # Pentavalent 2 (1 = received, 0 = did not recive, NA = no data)
  if ("PENTA2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PENTA2" = ifelse(dhs_data$h52 ==0 | dhs_data$h52 >=8, 0,
                                                      ifelse(dhs_data$h52 ==1 | dhs_data$h52 ==2 | dhs_data$h52 ==3, 1,
                                                             "NA")))}
  # Pentavalent3 (1 = received, 0 = did not recive, NA = no data)
  if ("PENTA3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PENTA3" = ifelse(dhs_data$h53 ==0 | dhs_data$h53 >=8, 0,
                                                      ifelse(dhs_data$h53 ==1 | dhs_data$h53 ==2 | dhs_data$h53 ==3, 1,
                                                             "NA")))}
  # Pneumococcal 1 (1 = received, 0 = did not recive, NA = no data)
  if ("PCV1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PCV1" = ifelse(dhs_data$h54 ==0 | dhs_data$h54 >=8, 0,
                                                    ifelse(dhs_data$h54 ==1 | dhs_data$h54 ==2 | dhs_data$h54 ==3, 1,
                                                           "NA")))}
  # Pneumococcal 2 (1 = received, 0 = did not recive, NA = no data)
  if ("PCV2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PCV2" = ifelse(dhs_data$h55 ==0 | dhs_data$h55 >=8, 0,
                                                    ifelse(dhs_data$h55 ==1 | dhs_data$h55 ==2 | dhs_data$h55 ==3, 1,
                                                           "NA")))}
  # Pneumococcal 3 (1 = received, 0 = did not recive, NA = no data)
  if ("PCV3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("PCV3" = ifelse(dhs_data$h56 ==0 | dhs_data$h56 >=8, 0,
                                                    ifelse(dhs_data$h56 ==1 | dhs_data$h56 ==2 | dhs_data$h56 ==3, 1,
                                                           "NA")))}
  # Rotavirus 1 (1 = received, 0 = did not recive, NA = no data)
  if ("ROTA1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("ROTA1" = ifelse(dhs_data$h57 ==0 | dhs_data$h57 >=8, 0,
                                                     ifelse(dhs_data$h57 ==1 | dhs_data$h57 ==2 | dhs_data$h57 ==3, 1,
                                                            "NA")))}
  # Rotavirus 2 (1 = received, 0 = did not recive, NA = no data)
  if ("ROTA2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("ROTA2" = ifelse(dhs_data$h58 ==0 | dhs_data$h58 >=8, 0,
                                                     ifelse(dhs_data$h58 ==1 | dhs_data$h58 ==2 | dhs_data$h58 ==3, 1,
                                                            "NA")))}
  # Rotavirus 3 (1 = received, 0 = did not recive, NA = no data)
  if ("ROTA3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("ROTA3" = ifelse(dhs_data$h59 ==0 | dhs_data$h59 >=8, 0,
                                                     ifelse(dhs_data$h59 ==1 | dhs_data$h59 ==2 | dhs_data$h59 ==3, 1,
                                                            "NA")))}
  # HIB 1 (1 = received, 0 = did not recive, NA = no data)
  if ("HIB1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HIB1" = ifelse(dhs_data$h64 ==0 | dhs_data$h64 >=8, 0,
                                                    ifelse(dhs_data$h64 ==1 | dhs_data$h64 ==2 | dhs_data$h64 ==3, 1,
                                                           "NA")))}
  # HIB 2 (1 = received, 0 = did not recive, NA = no data)
  if ("HIB2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HIB2" = ifelse(dhs_data$h65 ==0 | dhs_data$h65 >=8, 0,
                                                    ifelse(dhs_data$h65 ==1 | dhs_data$h65 ==2 | dhs_data$h65 ==3, 1,
                                                           "NA")))}
  # HIB 3 (1 = received, 0 = did not recive, NA = no data)
  if ("HIB3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("HIB3" = ifelse(dhs_data$h66 ==0 | dhs_data$h66 >=8, 0,
                                                    ifelse(dhs_data$h66 ==1 | dhs_data$h66 ==2 | dhs_data$h66 ==3, 1,
                                                           "NA")))}     
  # Polio - IPV 1 (1 = received, 0 = did not recive, NA = no data)
  if ("IPV1" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("IPV1" = ifelse((dhs_data$h4 ==0 | dhs_data$h4 >=8) & (dhs_data$IPVind ==1), 0,
                                                    ifelse(dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3, 1,
                                                           "NA")))}
  # Polio - IPV 2 (1 = received, 0 = did not recive, NA = no data)
  if ("IPV2" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("IPV2" = ifelse((dhs_data$h6 ==0 | dhs_data$h6 >=8) & (dhs_data$IPVind ==1), 0,
                                                    ifelse(dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3, 1,
                                                           "NA")))}
  # Polio - IPV 3 (1 = received, 0 = did not recive, NA = no data)
  if ("IPV3" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("IPV3" = ifelse((dhs_data$h8 ==0 | dhs_data$h8 >=8) & (dhs_data$IPVind ==1), 0,
                                                    ifelse(dhs_data$h8 ==1 | dhs_data$h8 ==2 | dhs_data$h8 ==3, 1,
                                                           "NA")))}
  
  # Indicator for Fully Immunized for Age (1 = Fully Immuniized, 0 = Not Fully Immunized, NA = no data)
  # At a minimum Fully Immunized utilizes data on BCG, MCV1, Polio1-3, DTP1-3 (and Penta1-3, depending on location)
  # Remove MCV1 & MCV2 from FULL for countries where an ongoing campaign is concurrent with DHS data collection. 
  
  if ("FULL" %in% VACCINES){
    if ((COUNTRY=="Afghanistan")
        |(COUNTRY=="Angola")
        |(COUNTRY=="Benin")
        |(COUNTRY=="Cambodia")
        |(COUNTRY=="Chad")
        |(COUNTRY=="Cote d’Ivoire")
        |(COUNTRY=="Eritrea")
        |(COUNTRY=="Ethiopia")
        |(COUNTRY=="Ghana")
        |(COUNTRY=="Guinea")
        |(COUNTRY=="Indonesia")
        |(COUNTRY=="Lesotho")
        |(COUNTRY=="Liberia")
        |(COUNTRY=="Madagascar")
        |(COUNTRY=="Malawi")
        |(COUNTRY=="Mali")
        |(COUNTRY=="Niger")
        |(COUNTRY=="Nigeria")
        |(COUNTRY=="Pakistan")
        |(COUNTRY=="Rwanda")
        |(COUNTRY=="Senegal")
        |(COUNTRY=="Sierra Leone")
        |(COUNTRY=="Tanzania")
        |(COUNTRY=="Togo")
        |(COUNTRY=="Timor-Leste")){
      if(("BCG" %in% VACCINES) & (("DTP1" %in% VACCINES)|("PENTA1" %in% VACCINES)) & (("DTP2" %in% VACCINES)|("PENTA2" %in% VACCINES)) & (("DTP3" %in% VACCINES)|("PENTA3" %in% VACCINES)) & (("OPV1" %in% VACCINES)|("IPV1" %in% VACCINES)|("PENTA1" %in% VACCINES)|("POLIO1" %in% VACCINES)) & (("OPV2" %in% VACCINES)|("IPV2" %in% VACCINES)|("PENTA2" %in% VACCINES)|("POLIO2" %in% VACCINES)) & (("OPV3" %in% VACCINES)|("IPV3" %in% VACCINES)|("PENTA3" %in% VACCINES)|("POLIO3" %in% VACCINES))){
        
        # Create VACCINES input, minus FULL, ZERO, and COMPLETE
        VAX_TOTAL = VACCINES[VACCINES != "FULL"]
        VAX_TOTAL = VAX_TOTAL[VAX_TOTAL !="ZERO"]
        VAX_TOTAL = VAX_TOTAL[VAX_TOTAL !="COMPLETE"]
        VAX_TOTAL = VAX_TOTAL[VAX_TOTAL !="MCV1"]
        VAX_TOTAL = VAX_TOTAL[VAX_TOTAL !="MCV2"]
        for (i in VAX_TOTAL){
          FI <- paste("FI_",i, sep="")
          dhs_data[,FI] = ifelse(((dhs_data[,i]==0)&(dhs_data$hw1 < schedule[,i][1]))|((dhs_data[,i]==1)&(dhs_data$hw1 >= schedule[,i][1])),1,0)
        }
        
        # Create small dataset with only the vaccine-specific immunized for age inicators produced above
        newdata<- dhs_data %>% 
          select(starts_with("FI_"))
        
        # Sum the binary indicator for appropriately immunized for age for each across observations
        dhs_data <- dhs_data %>% mutate("FULL_SUM" = rowSums(newdata))
        
        # Create fully immunized variable as the sum being equal to length of VACCINES input, minus FULL, ZERO, and COMPLETE (e.g. received all)
        dhs_data <- dhs_data %>% mutate("FULL" = ifelse((dhs_data[,"FULL_SUM"]==length(VAX_TOTAL)),1,0))
        
        # If only a few select vaccines are specified in VACCINES, pull data on other routine vaccines (BCG, DTP1-3, Polio1-3) to create FULL variable
      } else {
        dhs_data <- dhs_data %>% mutate("FULL" = ifelse((((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$hw1 < schedule[,"DTP1"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$hw1 >= schedule[,"DTP1"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$h5 ==1 | dhs_data$h5 ==2 | dhs_data$h5 ==3) & (dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & (dhs_data$hw1 >= schedule[,"DTP2"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$h5 ==1 | dhs_data$h5 ==2 | dhs_data$h5 ==3) & (dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & (dhs_data$h7 ==1 | dhs_data$h7 ==2 | dhs_data$h7 ==3) & (dhs_data$h8 ==1 | dhs_data$h8 ==2 | dhs_data$h8 ==3) & (dhs_data$hw1 >= schedule[,"DTP3"][1]))
                                                         
        ), 1, 0))
      }
    } else {
      if(("BCG" %in% VACCINES) & (("DTP1" %in% VACCINES)|("PENTA1" %in% VACCINES)) & (("DTP2" %in% VACCINES)|("PENTA2" %in% VACCINES)) & (("DTP3" %in% VACCINES)|("PENTA3" %in% VACCINES)) & (("OPV1" %in% VACCINES)|("IPV1" %in% VACCINES)|("PENTA1" %in% VACCINES)|("POLIO1" %in% VACCINES)) & (("OPV2" %in% VACCINES)|("IPV2" %in% VACCINES)|("PENTA2" %in% VACCINES)|("POLIO2" %in% VACCINES)) & (("OPV3" %in% VACCINES)|("IPV3" %in% VACCINES)|("PENTA3" %in% VACCINES)|("POLIO3" %in% VACCINES)) & ("MCV1" %in% VACCINES)){
        
        # Create VACCINES input, minus FULL, ZERO, and COMPLETE
        VAX_TOTAL = VACCINES[VACCINES != "FULL"]
        VAX_TOTAL = VAX_TOTAL[VAX_TOTAL !="ZERO"]
        VAX_TOTAL = VAX_TOTAL[VAX_TOTAL !="COMPLETE"]
        for (i in VAX_TOTAL){
          FI <- paste("FI_",i, sep="")
          dhs_data[,FI] = ifelse(((dhs_data[,i]==0)&(dhs_data$hw1 < schedule[,i][1]))|((dhs_data[,i]==1)&(dhs_data$hw1 >= schedule[,i][1])),1,0)
        }
        
        # Create small dataset with only the vaccine-specific immunized for age inicators produced above
        newdata<- dhs_data %>% 
          select(starts_with("FI_"))
        
        # Sum the binary indicator for appropriately immunized for age for each across observations
        dhs_data <- dhs_data %>% mutate("FULL_SUM" = rowSums(newdata))
        
        # Create fully immunized variable as the sum being equal to length of VACCINES input, minus FULL, ZERO, and COMPLETE (e.g. received all)
        dhs_data <- dhs_data %>% mutate("FULL" = ifelse((dhs_data[,"FULL_SUM"]==length(VAX_TOTAL)),1,0))
        
        # If only a few select vaccines are specified in VACCINES, pull data on other routine vaccines (BCG, DTP1-3, Polio1-3, MCV1) to create FULL variable
      } else {
        dhs_data <- dhs_data %>% mutate("FULL" = ifelse((((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$hw1 < schedule[,"DTP1"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$hw1 >= schedule[,"DTP1"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$h5 ==1 | dhs_data$h5 ==2 | dhs_data$h5 ==3) & (dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & (dhs_data$hw1 >= schedule[,"DTP2"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$h5 ==1 | dhs_data$h5 ==2 | dhs_data$h5 ==3) & (dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & (dhs_data$h7 ==1 | dhs_data$h7 ==2 | dhs_data$h7 ==3) & (dhs_data$h8 ==1 | dhs_data$h8 ==2 | dhs_data$h8 ==3) & (dhs_data$hw1 >= schedule[,"DTP3"][1]))|
                                                           ((dhs_data$h2 ==1 | dhs_data$h2 ==2 | dhs_data$h2 ==3) & (dhs_data$h3 ==1 | dhs_data$h3 ==2 | dhs_data$h3 ==3) & (dhs_data$h4 ==1 | dhs_data$h4 ==2 | dhs_data$h4 ==3) & (dhs_data$h5 ==1 | dhs_data$h5 ==2 | dhs_data$h5 ==3) & (dhs_data$h6 ==1 | dhs_data$h6 ==2 | dhs_data$h6 ==3) & (dhs_data$h7 ==1 | dhs_data$h7 ==2 | dhs_data$h7 ==3) & (dhs_data$h8 ==1 | dhs_data$h8 ==2 | dhs_data$h8 ==3) & (dhs_data$h9 ==1 | dhs_data$h9 ==2 | dhs_data$h9 ==3) & (dhs_data$hw1 >= schedule[,"MCV1"][1]))
        ), 1, 0))
      }
    }
  }
  
  #Create indicator for completed the routine immunization schedule by 24 months of age 
  if ("COMPLETE" %in% VACCINES){
    dhs_data <- dhs_data %>% mutate("COMPLETE" = ifelse(dhs_data$FULL==1 & dhs_data$hw1>=schedule[,"COMPLETE"][1], 1, 0))
    dhs_data$COMPLETE<- replace(dhs_data$COMPLETE,dhs_data$hw1<schedule[,"COMPLETE"][1],NA)
  }
  
  # Create Zero-Dose Indicator (never having received a single dose of BCG, DTP, PENTA, OPV/IPV, or MCV1 by 12 months)    
  if ("ZERO" %in% VACCINES){
    
    if(("BCG" %in% VACCINES) & (("DTP1" %in% VACCINES)|("PENTA1" %in% VACCINES)) & (("DTP2" %in% VACCINES)|("PENTA2" %in% VACCINES)) & (("DTP3" %in% VACCINES)|("PENTA3" %in% VACCINES)) & (("OPV1" %in% VACCINES)|("IPV1" %in% VACCINES)|("PENTA1" %in% VACCINES)) & (("OPV2" %in% VACCINES)|("IPV2" %in% VACCINES)|("PENTA2" %in% VACCINES)) & (("OPV3" %in% VACCINES)|("IPV3" %in% VACCINES)|("PENTA3" %in% VACCINES)) & ("MCV1" %in% VACCINES)){
      for (i in VAX_TOTAL){
        FI <- paste("FI_",i, sep="")
        dhs_data[,FI] = ifelse(((dhs_data[,i]==0)&(dhs_data$hw1 < schedule[,i][1]))|((dhs_data[,i]==1)&(dhs_data$hw1 >= schedule[,i][1])),1,0)
      }
      
      newdata<- dhs_data %>% 
        select(starts_with("FI_"))
      
      #Create indicator for no routine immunizations by 12 months of age 
      dhs_data <- dhs_data %>% mutate("FULL_SUM" = rowSums(newdata))
      dhs_data <- dhs_data %>% mutate("ZERO" = ifelse((dhs_data$FULL_SUM ==0) & (dhs_data$hw1 >= schedule[,"ZERO"][1]), 1,
                                                      ifelse(dhs_data$FULL_SUM =="NA", "NA", 0)))
      
    } else {
      
      #Create indicator for no routine immunizations by 12 months of age (never having received a single dose of BCG, DTP, PENTA, OPV/IPV, or MCV1)
      dhs_data <- dhs_data %>% mutate("ZERO" = ifelse(((dhs_data$h2 ==0 | dhs_data$h2 >=8) & (dhs_data$h3 ==0 | dhs_data$h3 >=8) & (dhs_data$h4 ==0 | dhs_data$h4 >=8) & (dhs_data$h5 ==0 | dhs_data$h5 >=8) & (dhs_data$h6 ==0 | dhs_data$h6 >=8) & (dhs_data$h7 ==0 | dhs_data$h7 >=8) & (dhs_data$h8 ==0 | dhs_data$h8 >=8) & (dhs_data$h9 ==0 | dhs_data$h9 >=8) & (dhs_data$hw1 >= schedule[,"ZERO"][1])),
                                                      1, 0))
      
    }
  }
  
  
  
  #Align Sub-National Outputs with GIS Map Boundaries where v101 is different
  FLAG<-0
  
  if(COUNTRY[1]=="Nigeria"){
    dhs_data$v101 <- dhs_data$sstate/10
    dhs_data$v024 <- dhs_data$sstate/10
  }
  
  if((COUNTRY[1]=="Uganda")&(YEAR>2012)){
    dhs_data$GEO<-dhs_data$v101
    dhs_data$v101 <- dhs_data$v101+1
    dhs_data$v024 <- dhs_data$v024+1
  }
  
  if((COUNTRY[1]=="Uganda")&(YEAR==2011)){
    dhs_data$GEO<-dhs_data$v101
  }
  
  if(COUNTRY[1]=="Senegal"){
    dhs_data$v101 <- dhs_data$v024
  }
  
  if(COUNTRY[1]=="Jordan"){
    dhs_data$v101 <- dhs_data$v024
    dhs_data <- dhs_data %>% mutate("v101" = ifelse(dhs_data$v101<20, dhs_data$v101-10,
                                                    ifelse(dhs_data$v101>=20 & dhs_data$v101<30, dhs_data$v101-16,
                                                           ifelse(dhs_data$v101>=30, dhs_data$v101-22,
                                                                  dhs_data$v101))))
  }
  
  if(COUNTRY[1]=="Indonesia"){
    dhs_data$v101 <- dhs_data$v024
    dhs_data <- dhs_data %>% mutate("v101" = ifelse(dhs_data$v101<20, dhs_data$v101-10,
                                                    ifelse(dhs_data$v101>=20 & dhs_data$v101<30, dhs_data$v101-11,
                                                           ifelse(dhs_data$v101>=30 & dhs_data$v101<50, dhs_data$v101-20,
                                                                  ifelse(dhs_data$v101>=50 & dhs_data$v101<60, dhs_data$v101-34,
                                                                         ifelse(dhs_data$v101>=60 & dhs_data$v101<70, dhs_data$v101-41,
                                                                                ifelse(dhs_data$v101>=70 & dhs_data$v101<80, dhs_data$v101-46,
                                                                                       ifelse(dhs_data$v101>=80 & dhs_data$v101<90, dhs_data$v101-50,
                                                                                              ifelse(dhs_data$v101>=90 & dhs_data$v101<93, dhs_data$v101-58,
                                                                                                     ifelse(dhs_data$v101>=93, dhs_data$v101-60,
                                                                                                            dhs_data$v101))))))))))
  }
  
  if(COUNTRY[1]=="Timor-Leste"){
    dhs_data$v101 <- dhs_data$v024
  }
  
  if(COUNTRY[1]=="Yemen"){
    dhs_data$GEO<-dhs_data$v101
    dhs_data$v101 <- dhs_data$v101-10
  }
  
  if((COUNTRY[1]=="India") & (YEAR<=2007)){
    dhs_data$GEO<-dhs_data$v101
    dhs_data <- dhs_data %>% mutate("v101" = ifelse(dhs_data$v101>=5 & dhs_data$v101<=24, dhs_data$v101-1,
                                                    ifelse(dhs_data$v101>=25 & dhs_data$v101<=30, dhs_data$v101-3,
                                                           ifelse(dhs_data$v101>30, dhs_data$v101-5,dhs_data$v101))))
  }
  
  if((COUNTRY[1]=="Madagascar")&(YEAR>2018)){
    dhs_data$GEO<-dhs_data$v101
    dhs_data <- dhs_data %>% mutate("v101" = ifelse(dhs_data$v101>=1 & dhs_data$v101<=20, dhs_data$v101-9,
                                                    ifelse(dhs_data$v101>=20 & dhs_data$v101<=30, dhs_data$v101-15,
                                                           ifelse(dhs_data$v101>=30 & dhs_data$v101<=40, dhs_data$v101-20,
                                                                  ifelse(dhs_data$v101>=40 & dhs_data$v101<=50, dhs_data$v101-27,
                                                                         ifelse(dhs_data$v101>=50 & dhs_data$v101<=60, dhs_data$v101-33,
                                                                                ifelse(dhs_data$v101>60, dhs_data$v101-39,dhs_data$v101)))))))
    
  }
  
  if(min(dhs_data$v101)==0){
    FLAG <- 1
    dhs_data$GEO<-dhs_data$v101
    dhs_data$v101 <- dhs_data$v101+1
    dhs_data$v024 <- dhs_data$v024+1
  }
  
  
  # Fix insurance & education variable to avoid reference level errors
  dhs_data <- dhs_data %>% mutate("v481" = v481+1)
  dhs_data$v481[is.na(dhs_data$v481)] <- 1
  dhs_data <- dhs_data %>% mutate("v106" = v106+1)
  
  if (COUNTRY[1]!="Yemen"){
    dhs_data<- dhs_data[complete.cases(dhs_data$v106),]
  }
  
  if ((COUNTRY=="Senegal")&(YEAR==2019)){
    dhs_data<- dhs_data[complete.cases(dhs_data$v106),]
  }
  
  if ((COUNTRY=="Burkina Faso")&(YEAR==2010)){
    dhs_data<- dhs_data[complete.cases(dhs_data$v106),]
  }
  
  if ((COUNTRY=="Comoros")&(YEAR==2012)){
    dhs_data<- dhs_data[complete.cases(dhs_data$v106),]
  }
  
  # Create Reference Values Vector
  FACT<-c()
  
  for (l in 1:length(FACTORS)){
    ifelse(FACTORS[l]=="region", FACT[l]<-"v101",
           ifelse(FACTORS[l]=="rural", FACT[l]<-"v025",
                  ifelse(FACTORS[l]=="education", FACT[l]<-"v106",
                         ifelse(FACTORS[l]=="wealth", FACT[l]<-"v190",
                                ifelse(FACTORS[l]=="sex", FACT[l]<-"b4",
                                       ifelse(FACTORS[l]=="insurance", FACT[l]<-"v481","NA"))))))
  }
  
  # Create Reference Levels Based on Fully Immunized for Age Outcome
  REF<- c()
  
  for(l in FACT){
    REF_DATA<-dhs_data
    REF_DATA[,"FULL"] <- as.numeric(REF_DATA[,"FULL"])
    REF_DATA<-subset(dhs_data, dhs_data[,"FULL"]==1|dhs_data[,"FULL"]==0)
    REF_DATA[,"FULL"] <- as.numeric(REF_DATA[,"FULL"])
    invisible(capture.output(REF_DATA<- REF_DATA %>% 
                               group_by(REF_DATA[,l]) %>% 
                               summarise(coverage=weighted.mean(FULL,v005))))
    REF_DATA<-na.omit(REF_DATA)
    REF_DATA<- filter(REF_DATA, coverage==max(REF_DATA$coverage))
    value<-c(as.numeric(REF_DATA[,1][1]))
    REF<-c(REF,value)
  }
  
  n = length(REF)
  
  for (j in 1:n){
    nameref <- paste("Ref",j, sep="")
    dhs_data[,nameref] = REF[j]
  }
  
  # Correct geographiclabels if stored in sstate instead of v101
  if(COUNTRY[1]=="Nigeria"){
    GEO_CI<- c(val_labels(dhs_data$sstate))
  } else{
    if((COUNTRY[1]=="Uganda")&(YEAR>2012)){
      GEO_CI<- c(val_labels(dhs_data$GEO))
    } else {
      if(COUNTRY[1]=="Jordan"){
        GEO_CI<- c(val_labels(dhs_data$v024))
      }else {
        if(COUNTRY[1]=="Indonesia"){
          GEO_CI<- c(val_labels(dhs_data$v024))
        }else {
          if(COUNTRY[1]=="Timor-Leste"){
            GEO_CI<- c(val_labels(dhs_data$v024))
          } else {
            if(COUNTRY[1]=="Lesotho"){
              GEO_CI<- c(val_labels(dhs_data$v024))
            } else {
              if(COUNTRY[1]=="Namibia"){
                GEO_CI<- c(val_labels(dhs_data$v024))
                if (length(GEO_CI)==14){
                  GEO_CI<- head(GEO_CI, -1)
                }
              } else {
                if(COUNTRY[1]=="Zimbabwe"){
                  GEO_CI<- c(val_labels(dhs_data$v024))
                } else {
                  if(COUNTRY[1]=="Egypt"){
                    GEO_CI<- c(val_labels(dhs_data$GEO))
                  } else {
                    if(COUNTRY[1]=="Yemen"){
                      GEO_CI<- c(val_labels(dhs_data$GEO))
                    } else {
                      if((COUNTRY[1]=="India") & (YEAR<=2007)){
                        GEO_CI<- c(val_labels(dhs_data$GEO))
                      } else {
                        if((COUNTRY[1]=="Madagascar") & (YEAR<=2021)){
                          GEO_CI<- c(val_labels(dhs_data$v024))
                        } else {
                          if(FLAG[1]==1){
                            GEO_CI<- c(val_labels(dhs_data$GEO))
                          } else {
                            GEO_CI<- c(val_labels(dhs_data$v101))
                          }}}}}}}}}}}}}
  
  #Fix Uganda 2011
  if((COUNTRY[1]=="Uganda")&(YEAR==2011)){
    GEO_CI <- GEO_CI[1:10]
  }
  
  #Store Geographic Sub-unit Names
  GEO_CI<- names(GEO_CI)
  
  #Create Sub-National Composite Index Output Data Frames
  CI_Results_GEO_Output <- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_Results_GEO_Output_95ciLB <- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_Results_GEO_Output_95ciUB <- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  HII_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  HII_Results_GEO_Output_95ciLB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  HII_Results_GEO_Output_95ciUB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_E_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_E_Results_GEO_Output_95ciLB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_E_Results_GEO_Output_95ciUB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Composite_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Composite_Results_GEO_Output_95ciLB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Composite_Results_GEO_Output_95ciUB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  
  #Create Sub-National Traditional Index Output Data Frames
  AEG_Sex_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Rural_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Insurance_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  REG_Sex_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  REG_Rural_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  REG_Insurance_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  SII_Education_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  SII_Wealth_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  RII_Education_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  RII_Wealth_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_Wealth_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_Wealth_Results_GEO_Output_95ciLB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_Wealth_Results_GEO_Output_95ciUB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_E_Wealth_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_E_Wealth_Results_GEO_Output_95ciLB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  CI_E_Wealth_Results_GEO_Output_95ciUB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Wealth_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Wealth_Results_GEO_95ciLB_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  AEG_Wealth_Results_GEO_95ciUB_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  Coverage_Results_GEO_Output<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  Coverage_Results_GEO_Output_95ciLB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  Coverage_Results_GEO_Output_95ciUB<- data.frame(matrix(NA, nrow = length(GEO_CI), ncol = 1))
  
  for (i in VACCINES){
    
    # convert vaccines to to numeric vector
    dhs_data[,i] <- as.numeric(dhs_data[,i])
    
    #Create Vaccine-Specific Variables
    underage_name <- paste("underage_",i, sep="")
    dhs_data$underage_i = dhs_data[,underage_name]
    outcome_name <- paste("",i, sep="")
    dhs_data$outcome =  dhs_data[, outcome_name]
    dhs_data$new_location <- factor(dhs_data$v101)
    
    # Create Design Matrix
    design <- svydesign(data = dhs_data,
                        ids = ~caseid,
                        weights = ~v005)
    
    # Subset Data to be only data where outcome data is available
    data_i <- subset(dhs_data, dhs_data[,i]==1|dhs_data[,i]==0)
    
    #Set Up Logistic Regression
    if ((COUNTRY=="Maldives")&(YEAR==2016)){
      if (i=="BCG"|i=="FULL"|i=="COMPLETE") {
        logit_i <- svyglm(outcome ~ 
                            relevel(factor(new_location), ref = Ref1[1]) + 
                            relevel(factor(v106), ref = Ref3[1]) + 
                            relevel(factor(v190), ref = Ref4[1]) + 
                            relevel(factor(b4), ref = Ref5[1]), 
                          design = design, family = binomial(link="logit"), data = data_i)
      } else{
        logit_i <- svyglm(outcome ~ factor(underage_i) + 
                            relevel(factor(new_location), ref = Ref1[1]) + 
                            relevel(factor(v106), ref = Ref3[1]) + 
                            relevel(factor(v190), ref = Ref4[1]) + 
                            relevel(factor(b4), ref = Ref5[1]), 
                          design = design, family = binomial(link="logit"), data = data_i)
      }
    } else {
      if ((COUNTRY=="Yemen")&(YEAR==2013)){
        if (i=="BCG"|i=="FULL"|i=="COMPLETE") {
          logit_i <- svyglm(outcome ~ 
                              relevel(factor(v101), ref = Ref1[1]) + 
                              relevel(factor(v025), ref = Ref2[1]) + 
                              relevel(factor(v190), ref = Ref4[1]) + 
                              relevel(factor(b4), ref = Ref5[1]) + 
                              relevel(factor(v481), ref = Ref6[1]), 
                            design = design, family = binomial(link="logit"), data = data_i)
        } else{
          logit_i <- svyglm(outcome ~ factor(underage_i) + 
                              relevel(factor(v101), ref = Ref1[1]) + 
                              relevel(factor(v025), ref = Ref2[1]) + 
                              relevel(factor(v190), ref = Ref4[1]) + 
                              relevel(factor(b4), ref = Ref5[1]) + 
                              relevel(factor(v481), ref = Ref6[1]), 
                            design = design, family = binomial(link="logit"), data = data_i)
        }
      } else {
        if (min(data_i$v481)==max(data_i$v481)) {
          if (i=="BCG"|i=="FULL"|i=="COMPLETE") {
            logit_i <- svyglm(outcome ~ 
                                relevel(factor(v101), ref = Ref1[1]) + 
                                relevel(factor(v025), ref = Ref2[1]) + 
                                relevel(factor(v106), ref = Ref3[1]) + 
                                relevel(factor(v190), ref = Ref4[1]) + 
                                relevel(factor(b4), ref = Ref5[1]), 
                              design = design, family = binomial(link="logit"), data = data_i)
          } else{
            logit_i <- svyglm(outcome ~ factor(underage_i) + 
                                relevel(factor(v101), ref = Ref1[1]) + 
                                relevel(factor(v025), ref = Ref2[1]) + 
                                relevel(factor(v106), ref = Ref3[1]) + 
                                relevel(factor(v190), ref = Ref4[1]) + 
                                relevel(factor(b4), ref = Ref5[1]), 
                              design = design, family = binomial(link="logit"), data = data_i)
          }
          
        } else {
          if (i=="BCG"|i=="FULL"|i=="COMPLETE") {
            logit_i <- svyglm(outcome ~ 
                                relevel(factor(v101), ref = Ref1[1]) + 
                                relevel(factor(v025), ref = Ref2[1]) + 
                                relevel(factor(v106), ref = Ref3[1]) + 
                                relevel(factor(v190), ref = Ref4[1]) + 
                                relevel(factor(b4), ref = Ref5[1]) + 
                                relevel(factor(v481), ref = Ref6[1]), 
                              design = design, family = binomial(link="logit"), data = data_i)
          } else{
            logit_i <- svyglm(outcome ~ factor(underage_i) + 
                                relevel(factor(v101), ref = Ref1[1]) + 
                                relevel(factor(v025), ref = Ref2[1]) + 
                                relevel(factor(v106), ref = Ref3[1]) + 
                                relevel(factor(v190), ref = Ref4[1]) + 
                                relevel(factor(b4), ref = Ref5[1]) + 
                                relevel(factor(v481), ref = Ref6[1]), 
                              design = design, family = binomial(link="logit"), data = data_i)
          }
        }
      }
    }
    
    summary(logit_i)
    
    
    # Calculate Predicted Probabilities & Store in Pred_Probs Object
    
    if ((COUNTRY=="Maldives") & (YEAR==2016)){
      pred_probs <- data.frame(cbind(logit_i$fitted.values, as.numeric(names(logit_i$fitted.values))))
      names(pred_probs)[2] <- 'MERGE_ID'
      names(pred_probs)[1] <- 'hci_du.response'
    } else {
      pred_probs <- data.frame(hci=predict(logit_i, data = dhs_data, type="response",na.action = na.exclude))
    }
    
    # Computing Direct Unfairness Metric
    # Compute predicted probability holding child age (fair equity) at reference category (DTP1_underage==0)
    
    data_i_unfair <- data.frame(data_i)
    data_i_unfair$underage_i <- replace(data_i_unfair[,underage_name],data_i_unfair[,underage_name]==1,0)
    
    # Create New Predictions on Direct Unfairness Dataset
    pred_probs_2 <- data.frame(hci_du = predict(logit_i, data_i_unfair, type="response"))
    
    # Computing Predicted Fairness Metric    
    # Compute predicted probability holding all unfair variables at reference levels (fairness equity) at reference category levels
    
    data_i_fair <- data.frame(data_i)
    
    # Create Reference Level Variable Dataset for all of the Fair Predictors
    # Set Unfair Predictors to Reference Levels
    
    data_i_fair$v101 <- replace(data_i_fair$v101,data_i_fair$v101!=0,0)
    data_i_fair$v025 <- replace(data_i_fair$v025,data_i_fair$v025!=0,0)
    data_i_fair$v106 <- replace(data_i_fair$v106,data_i_fair$v106!=0,0)
    data_i_fair$v190 <- replace(data_i_fair$v190,data_i_fair$v190!=0,0)
    data_i_fair$b4 <- replace(data_i_fair$b4,data_i_fair$b4!=0,0)
    data_i_fair$v481 <- replace(data_i_fair$v481,data_i_fair$v481!=0,0)
    
    
    data_i_fair$v101 <- replace(data_i_fair$v101,data_i_fair$v101!=REF[1],REF[1])
    data_i_fair$v025 <- replace(data_i_fair$v025,data_i_fair$v025!=REF[2],REF[2])
    data_i_fair$v106 <- replace(data_i_fair$v106,data_i_fair$v106!=REF[3],REF[3])
    data_i_fair$v190 <- replace(data_i_fair$v190,data_i_fair$v190!=REF[4],REF[4])
    data_i_fair$b4 <- replace(data_i_fair$b4,data_i_fair$b4!=REF[5],REF[5])
    data_i_fair$v481 <- replace(data_i_fair$v481,data_i_fair$v481!=REF[6],REF[6])
    
    # Set Constant (Unfair) Predictors as Factors
    data_i_fair$v101 <- as.numeric(data_i_fair$v101)
    data_i_fair$v025 <- as.numeric(data_i_fair$v025)
    data_i_fair$v106 <- as.numeric(data_i_fair$v106)
    data_i_fair$v190 <- as.numeric(data_i_fair$v190)
    data_i_fair$b4 <- as.numeric(data_i_fair$b4)
    data_i_fair$v481 <- as.numeric(data_i_fair$v481)
    
    # Create New Predictions on Predicted Fairness Dataset
    data_i_fair$predict = summary(logit_i)$coefficients[1] + summary(logit_i)$coefficients[2]*data_i_fair$underage_i
    
    logit2prob <- function(logit){
      odds <- exp(logit)
      prob <- odds / (1 + odds)
      return(prob)
    }
    
    prob <- logit2prob(data_i_fair$predict)
    
    pred_probs_3 <- data.frame(hci_fair = prob)
    
    # Calculating the Direct Concentration Index
    direct_ci <- ci(y = data_i[,i], x = pred_probs_2$hci_du.response, wt=data_i$v005, type = "CI")
    CI_1 <- round(concentration_index(direct_ci), digits = 3)
    CI_1_95ciLB<- round(CI_1 - 1.96*sqrt(direct_ci$variance), digits = 3)
    CI_1_95ciUB<- round(CI_1 + 1.96*sqrt(direct_ci$variance), digits = 3)
    
    # Calculating the Horizontal Ineqity Index (HII) 
    CIFair <- ci(y = pred_probs_3$hci_fair, x = pred_probs_2$hci_du.response, wt=data_i$v005, type = "CI")
    CI_Fair <- concentration_index(CIFair)
    HII <- round(CI_1 - CI_Fair, digits = 3)
    HII_95ciLB<- round(HII - (1.96*sqrt((direct_ci$variance + CIFair$variance)/length(data_i[,i]))), digits = 3)
    HII_95ciUB<- round(HII + (1.96*sqrt((direct_ci$variance + CIFair$variance)/length(data_i[,i]))), digits = 3)
    
    # Calculating the Erreygers Corrected Composite Concentration Index 
    CIE <- ci(y = data_i[,i], x = pred_probs_2$hci_du.response, wt=data_i$v005, type = "CIc")
    CI_E <- round(concentration_index(CIE), digits = 3)
    CI_E_95ciLB<- round(CI_E - 1.96*sqrt(CIE$variance), digits = 3)
    CI_E_95ciUB<- round(CI_E + 1.96*sqrt(CIE$variance), digits = 3)
    
    # Calculating the Composite Absolute Equity Gap
    rank<-c(pred_probs_2$hci_du.response)
    quantiles<- quantile(rank, probs = seq(0, 1, 0.20))
    comp_data<- data.frame(cbind(data_i[,i],data_i[,"v005"],rank))
    names(comp_data)[names(comp_data) == "V1"] <- paste("",i, sep="")
    names(comp_data)[names(comp_data) == "V2"] <- paste("v005","", sep="")
    AEG_Composite <- round(weighted.mean(comp_data[,i][rank>=quantiles[5]],comp_data[,"v005"][rank>=quantiles[5]]) - weighted.mean(comp_data[,i][rank<quantiles[2]],comp_data[,"v005"][rank<quantiles[2]]), digits=3)
    AEG_Composite_95ciLB <- round(AEG_Composite - (1.96*round(sqrt(((weighted.sd(comp_data[,i][rank>=quantiles[5]],comp_data[,"v005"][rank>=quantiles[5]]))^2)/length(comp_data[,i][rank>=quantiles[5]]) + (((weighted.sd(comp_data[,i][rank<quantiles[2]],comp_data[,"v005"][rank<quantiles[2]]))^2)/length(comp_data[,i][rank<quantiles[2]]))), digits=3)), digits = 3)
    AEG_Composite_95ciUB <- round(AEG_Composite + (1.96*round(sqrt(((weighted.sd(comp_data[,i][rank>=quantiles[5]],comp_data[,"v005"][rank>=quantiles[5]]))^2)/length(comp_data[,i][rank>=quantiles[5]]) + (((weighted.sd(comp_data[,i][rank<quantiles[2]],comp_data[,"v005"][rank<quantiles[2]]))^2)/length(comp_data[,i][rank<quantiles[2]]))), digits=3)), digits = 3)
    
    assign(paste("CI_1_",i, sep=""),CI_1)
    assign(paste("HII_",i, sep=""), HII)
    assign(paste("CI_E_",i, sep=""),CI_E)
    assign(paste("AEG_Composite_",i, sep=""),AEG_Composite)
    assign(paste("CI_1_95ciLB_",i, sep=""), CI_1_95ciLB)
    assign(paste("CI_1_95ciUB_",i, sep=""), CI_1_95ciUB)
    assign(paste("HII_95ciLB_",i, sep=""), HII_95ciLB)
    assign(paste("HII_95ciUB_",i, sep=""), HII_95ciUB)
    assign(paste("CI_E_95ciLB_",i, sep=""), CI_E_95ciLB)
    assign(paste("CI_E_95ciUB_",i, sep=""), CI_E_95ciUB)
    assign(paste("AEG_Composite_95ciLB_",i, sep=""), AEG_Composite_95ciLB)
    assign(paste("AEG_Composite_95ciUB_",i, sep=""), AEG_Composite_95ciUB)
    
    #Subset Coverage to be only among correct for age-group for Vaccine (older than schedule age)
    data_ic <- subset(data_i, data_i[,"underage_i"]==0)
    coverage<- c(weighted.mean(data_ic$outcome,data_ic$v005))
    coverage_95ciLB<- round(coverage - (1.96*sqrt((coverage*(1-coverage))/length(data_ic$outcome))), digits = 3)
    coverage_95ciUB<- round(coverage + (1.96*sqrt((coverage*(1-coverage))/length(data_ic$outcome))), digits = 3)
    
    #Name other CI Results
    CI_Results<- c(CI_Results, assign(paste("CI_1_",i, sep=""),CI_1))
    CI_Results_95ciLB<- c(CI_Results_95ciLB,assign(paste("CI_1_95ciLB_",i, sep=""), CI_1_95ciLB))
    CI_Results_95ciUB<- c(CI_Results_95ciUB,assign(paste("CI_1_95ciUB_",i, sep=""), CI_1_95ciUB))
    HII_Results<- c(HII_Results, assign(paste("HII_",i, sep=""), HII))
    HII_Results_95ciLB<- c(HII_Results_95ciLB, assign(paste("HII_95ciLB_",i, sep=""), HII_95ciLB))
    HII_Results_95ciUB<- c(HII_Results_95ciUB,assign(paste("HII_95ciUB_",i, sep=""), HII_95ciUB))
    CI_E_Results<- c(CI_E_Results, assign(paste("CI_E_",i, sep=""),CI_E))
    CI_E_Results_95ciLB<- c(CI_E_Results_95ciLB,assign(paste("CI_E_95ciLB_",i, sep=""), CI_E_95ciLB))
    CI_E_Results_95ciUB<- c(CI_E_Results_95ciUB,assign(paste("CI_E_95ciUB_",i, sep=""), CI_E_95ciUB))
    AEG_Composite_Results <- c(AEG_Composite_Results, assign(paste("AEG_Composite_",i, sep=""), AEG_Composite))
    AEG_Composite_Results_95ciLB<- c(AEG_Composite_Results_95ciLB,assign(paste("AEG_Composite_95ciLB_",i, sep=""), AEG_Composite_95ciLB))
    AEG_Composite_Results_95ciUB<- c(AEG_Composite_Results_95ciUB,assign(paste("AEG_Composite_95ciUB_",i, sep=""), AEG_Composite_95ciUB))
    Coverage_Results<- c(Coverage_Results, assign(paste("Coverage_",i, sep=""),coverage))
    Coverage_Results_95ciLB<- c(Coverage_Results_95ciLB,assign(paste("Coverage_Results_95ciLB_",i, sep=""), coverage_95ciLB))
    Coverage_Results_95ciUB<- c(Coverage_Results_95ciUB,assign(paste("Coverage_Results_95ciUB_",i, sep=""), coverage_95ciUB))
    
    #Compute Absolte Equity Gap (AEG) & Ratio Equity Gap (REG) for Binary Factors
    AEG_Sex <- round(weighted.mean(data_i[,i][data_i[,FACT[5]]==REF[5]],data_i[,"v005"][data_i[,FACT[5]]==REF[5]]) - weighted.mean(data_i[,i][data_i[,FACT[5]]!=REF[5]],data_i[,"v005"][data_i[,FACT[5]]!=REF[5]]), digits=3)
    AEG_Rural <- round(weighted.mean(data_i[,i][data_i[,FACT[2]]==REF[2]],data_i[,"v005"][data_i[,FACT[2]]==REF[2]]) - weighted.mean(data_i[,i][data_i[,FACT[2]]!=REF[2]],data_i[,"v005"][data_i[,FACT[2]]!=REF[2]]), digits=3)
    AEG_Insurance <- round(weighted.mean(data_i[,i][data_i[,FACT[6]]==REF[6]],data_i[,"v005"][data_i[,FACT[6]]==REF[6]]) - weighted.mean(data_i[,i][data_i[,FACT[6]]!=REF[6]],data_i[,"v005"][data_i[,FACT[6]]!=REF[6]]), digits=3)
    REG_Sex <- round(weighted.mean(data_i[,i][data_i[,FACT[5]]==REF[5]],data_i[,"v005"][data_i[,FACT[5]]==REF[5]])/(weighted.mean(data_i[,i][data_i[,FACT[5]]!=REF[5]],data_i[,"v005"][data_i[,FACT[5]]!=REF[5]])), digits=3)
    REG_Rural <- round(weighted.mean(data_i[,i][data_i[,FACT[2]]==REF[2]],data_i[,"v005"][data_i[,FACT[2]]==REF[2]])/(weighted.mean(data_i[,i][data_i[,FACT[2]]!=REF[2]],data_i[,"v005"][data_i[,FACT[2]]!=REF[2]])), digits=3)
    REG_Insurance <- round(weighted.mean(data_i[,i][data_i[,FACT[6]]==REF[6]],data_i[,"v005"][data_i[,FACT[6]]==REF[6]])/(weighted.mean(data_i[,i][data_i[,FACT[6]]!=REF[6]],data_i[,"v005"][data_i[,FACT[6]]!=REF[6]])), digits=3)
    
    #Compute Slope Index of Ineqality (SII) & Relative Index of Inequality (RII) for Categorical Factors
    
    #Region SII & RII
    SII<- data.frame("SII", data_i[,FACT[1]], data_i[,i],data_i[,"v005"])
    names(SII)[names(SII) == "X.SII."] <- paste("SII","", sep="")
    names(SII)[names(SII) == "data_i...FACT.1.."] <- paste(FACT[1],"", sep="")
    names(SII)[names(SII) == "data_i...i."] <- paste("outcome","", sep="")
    names(SII)[names(SII) == "data_i....v005.."] <- paste("","v005", sep="")
    
    #Create Region SII Dataset
    SII<- SII %>%
      group_by(v101) %>%
      summarise(outcome_mean=weighted.mean(outcome,v005),
                outcome_se=(weighted.sd(outcome,v005)/(sqrt(length(outcome)))),
                population=length(outcome))
    
    if (length(unique(data_i$v101))<5){
      SII_output <- "NA"
    } else{
      #Compute Region SII & RII
      SII_output <- phe_sii(
        data = SII,
        quantile = v101,
        population = population,
        value = outcome_mean,
        rii = TRUE,
        se = outcome_se
        
      )
    }
    SII_region<- ifelse(SII_output[1]=="NA","NA",round(SII_output[1], digits = 3))
    RII_region<- ifelse(SII_output[1]=="NA","NA",round(SII_output[2], digits = 3)) 
    
    #Education SII & RII
    SII<- data.frame("SII", data_i[,FACT[3]], data_i[,i],data_i[,"v005"])
    names(SII)[names(SII) == "X.SII."] <- paste("SII","", sep="")
    names(SII)[names(SII) == "data_i...FACT.3.."] <- paste(FACT[3],"", sep="")
    names(SII)[names(SII) == "data_i...i."] <- paste("outcome","", sep="")
    names(SII)[names(SII) == "data_i....v005.."] <- paste("","v005", sep="")
    
    #Create Education Dataset
    SII<- SII %>%
      group_by(v106) %>%
      summarise(outcome_mean=weighted.mean(outcome,v005),
                outcome_se=(weighted.sd(outcome,v005)/(sqrt(length(outcome)))),
                population=length(outcome))
    
    #Correct Education SII Dataset Values
    SII1<-SII
    SII1$v106<-SII1$v106*2
    SII2<-SII
    SII2$v106<-(SII2$v106*2)+1
    SII<- rbind.data.frame(SII1,SII2)
    
    if (length(unique(data_i$v106))<5){
      SII_output <- "NA"
    } else{
      
      #Compute Education SII
      SII_output <- phe_sii(
        data = SII,
        quantile = v106,
        population = population,
        value = outcome_mean,
        rii = TRUE,
        se = outcome_se
      )
    }
    SII_education<- ifelse(SII_output[1]=="NA","NA",round(SII_output[1], digits = 3))
    RII_education<- ifelse(SII_output[1]=="NA","NA",round(SII_output[2], digits = 3)) 
    
    #Wealth Quintile SII & RII
    SII<- data.frame("SII", data_i[,FACT[4]], data_i[,i],data_i[,"v005"])
    names(SII)[names(SII) == "X.SII."] <- paste("SII","", sep="")
    names(SII)[names(SII) == "data_i...FACT.4.."] <- paste(FACT[4],"", sep="")
    names(SII)[names(SII) == "data_i...i."] <- paste("outcome","", sep="")
    names(SII)[names(SII) == "data_i....v005.."] <- paste("","v005", sep="")
    
    #Create Wealth Quintile SII & RII Dataset
    SII<- SII %>%
      group_by(v190) %>%
      summarise(outcome_mean=weighted.mean(outcome,v005),
                outcome_se=(weighted.sd(outcome,v005)/(sqrt(length(outcome)))),
                population=length(outcome))
    
    #Compute Wealth Quintile SII & RII
    SII_output <- phe_sii(
      data = SII,
      quantile = v190,
      population = population,
      value = outcome_mean,
      rii = TRUE,
      se = outcome_se
    )
    SII_wealth<- round(SII_output[1], digits = 3)
    RII_wealth<- round(SII_output[2], digits = 3) 
    
    #Create Errygers Corrected concentration index for socio-economic status
    ci_errygers <- ci(x=data_i[,i],y=data_i[,FACT[4]],wt=data_i$v005,type = "CIc")
    CI_E_Wealth <- round(concentration_index(ci_errygers), digits = 3)
    CI_E_Wealth_95ciLB<- round(CI_E_Wealth - 1.96*sqrt(abs(ci_errygers$variance)), digits = 3)
    CI_E_Wealth_95ciUB<- round(CI_E_Wealth + 1.96*sqrt(abs(ci_errygers$variance)), digits = 3)
    
    
    #Create Wagstaff concentration index for socio-economic status
    ci_wagstaff <- ci(x=data_i[,i],y=data_i[,FACT[4]],wt=data_i$v005,type = "CI")
    CI_W_Wealth <- round(concentration_index(ci_wagstaff), digits = 3)
    CI_W_Wealth_95ciLB<- round(CI_W_Wealth - 1.96*sqrt(abs(ci_wagstaff$variance)), digits = 3)
    CI_W_Wealth_95ciUB<- round(CI_W_Wealth + 1.96*sqrt(abs(ci_wagstaff$variance)), digits = 3)
    
    # Calculating the Wealth-Based Absolute Equity Gap
    comp_data<- data.frame(cbind(data_i[,"outcome"],data_i[,"v005"],data_i[,"v190"]))
    names(comp_data)[names(comp_data) == "X1"] <- paste("",i, sep="")
    names(comp_data)[names(comp_data) == "X2"] <- paste("v005","", sep="")
    names(comp_data)[names(comp_data) == "X3"] <- paste("v190","", sep="")
    AEG_Wealth <- round(weighted.mean(comp_data[,i][comp_data$v190>=5],comp_data[,"v005"][comp_data$v190>=5]) - weighted.mean(comp_data[,i][comp_data$v190<2],comp_data[,"v005"][comp_data$v190<2]), digits=3)
    AEG_Wealth_95ciLB <- round(AEG_Wealth - (1.96*round(sqrt(((weighted.sd(comp_data[,i][comp_data$v190>=5],comp_data[,"v005"][comp_data$v190>=5]))^2)/length(comp_data[,i][comp_data$v190>=5]) + (((weighted.sd(comp_data[,i][comp_data$v190<2],comp_data[,"v005"][comp_data$v190<2]))^2)/length(comp_data[,i][comp_data$v190<2]))), digits=3)), digits = 3)
    AEG_Wealth_95ciUB <- round(AEG_Wealth + (1.96*round(sqrt(((weighted.sd(comp_data[,i][comp_data$v190>=5],comp_data[,"v005"][comp_data$v190>=5]))^2)/length(comp_data[,i][comp_data$v190>=5]) + (((weighted.sd(comp_data[,i][comp_data$v190<2],comp_data[,"v005"][comp_data$v190<2]))^2)/length(comp_data[,i][comp_data$v190<2]))), digits=3)), digits = 3)
    
    #Store other Indices
    AEG_Sex_Results<-c(AEG_Sex_Results,assign(paste("AEG_Sex_",i, sep=""),AEG_Sex))
    AEG_Rural_Results<-c(AEG_Rural_Results,assign(paste("AEG_Rural_",i, sep=""),AEG_Rural))
    AEG_Insurance_Results<-c(AEG_Insurance_Results,assign(paste("AEG_Insurance_",i, sep=""),AEG_Insurance))
    REG_Sex_Results<-c(REG_Sex_Results,assign(paste("REG_Sex_",i, sep=""),REG_Sex))
    REG_Rural_Results<-c(REG_Rural_Results,assign(paste("REG_Rural_",i, sep=""),REG_Rural))
    REG_Insurance_Results<-c(REG_Insurance_Results,assign(paste("REG_Insurance_",i, sep=""),REG_Insurance))
    SII_Education_Results<-c(SII_Education_Results,assign(paste("SII_Education_",i, sep=""),as.numeric(SII_education)))
    SII_Wealth_Results<-c(SII_Wealth_Results,assign(paste("SII_Wealth_",i, sep=""),as.numeric(SII_wealth)))
    SII_Region_Results<-c(SII_Region_Results,assign(paste("SII_Region_",i, sep=""),as.numeric(SII_region)))
    RII_Education_Results<-c(RII_Education_Results,assign(paste("RII_Education_",i, sep=""),as.numeric(RII_education)))
    RII_Wealth_Results<-c(RII_Wealth_Results,assign(paste("RII_Wealth_",i, sep=""),as.numeric(RII_wealth)))
    RII_Region_Results<-c(RII_Region_Results,assign(paste("RII_Region_",i, sep=""),as.numeric(RII_region)))
    CI_E_Wealth_Results<- c(CI_E_Wealth_Results,assign(paste("CI_E_Wealth_",i, sep=""),CI_E_Wealth))
    CI_E_Wealth_Results_95ciLB<- c(CI_E_Wealth_Results_95ciLB,assign(paste("CI_E_Wealth_95ciLB_",i, sep=""),CI_E_Wealth_95ciLB))
    CI_E_Wealth_Results_95ciUB<- c(CI_E_Wealth_Results_95ciUB,assign(paste("CI_E_Wealth_95ciUB_",i, sep=""),CI_E_Wealth_95ciUB))
    CI_Wealth_Results<- c(CI_Wealth_Results,assign(paste("CI_Wealth_",i, sep=""),CI_W_Wealth))
    CI_Wealth_Results_95ciLB<- c(CI_Wealth_Results_95ciLB,assign(paste("CI_Wealth_95ciLB_",i, sep=""),CI_W_Wealth_95ciLB))
    CI_Wealth_Results_95ciUB<- c(CI_Wealth_Results_95ciUB,assign(paste("CI_Wealth_95ciUB_",i, sep=""),CI_W_Wealth_95ciUB))
    AEG_Wealth_Results <- c(AEG_Wealth_Results, assign(paste("AEG_Wealth_",i, sep=""), AEG_Wealth))
    AEG_Wealth_Results_95ciLB<- c(AEG_Wealth_Results_95ciLB,assign(paste("AEG_Wealth_95ciLB_",i, sep=""), AEG_Wealth_95ciLB))
    AEG_Wealth_Results_95ciUB<- c(AEG_Wealth_Results_95ciUB,assign(paste("AEG_Wealth_95ciUB_",i, sep=""), AEG_Wealth_95ciUB))
    
    #Decomposition
    design <- svydesign(data = dhs_data,
                        ids = ~caseid,
                        weights = ~v005)
    
    if ((COUNTRY=="Yemen")&(YEAR==2013)){
      logit_de <- svyglm(outcome ~ underage_i+v101+v025+v190+b4+v481, 
                         design = design, family = binomial(link="logit"), data = data_i)
    } else {
      if (min(data_i$v481)==max(data_i$v481)){
        logit_de <- svyglm(outcome ~ underage_i+v101+v025+v106+v190+b4, 
                           design = design, family = binomial(link="logit"), data = data_i)
      } else {
        logit_de <- svyglm(outcome ~ underage_i+v101+v025+v106+v190+b4+v481, 
                           design = design, family = binomial(link="logit"), data = data_i)
      }
    }
    
    ###Loop Decomposition###
    new_data <- new_data <- data_i %>% select("underage_i",all_of(FACT))
    decomposition <- data.frame()
    decomposition_x <-data.frame() 
    
    for (x in colnames(new_data)) {
      decomposition_x <- tibble(
        name_t = c(x),
        cindex_t = c(concentration_index(ci(y = new_data[[x]], x = pred_probs_2$hci_du.response, type = "CI"))),
        elastic_t = c(mean(coef(logit_de)[x]*logit_de$fitted.values*(1-logit_de$fitted.values)*new_data[[x]]/logit_de$fitted.values)),
        contribution_t = abs(as.numeric(elastic_t)*as.numeric(cindex_t)),
        percent_t = 100*abs(as.numeric(contribution_t)/CI_1),
      )
      decomposition <- rbind.data.frame(decomposition,  decomposition_x)
    }
    
    resid_c_t <- CI_1-sum(decomposition$contribution_t, na.rm=TRUE)
    
    residual <- tibble(name_t = "Residual",
                       cindex_t = "" ,
                       elastic_t = "",
                       contribution_t = resid_c_t,
                       percent_t = 100*abs(resid_c_t/CI_1))
    
    decomposition <- rbind.data.frame(decomposition,  residual)
    
    decomposition$contribution_t[is.na(decomposition$contribution_t)] <- 0
    decomposition$percent_t[is.na(decomposition$percent_t)] <- 0
    
    if (sum(decomposition$percent_t)>100){
      decomposition$percent_t<-decomposition$percent_t/(sum(decomposition$percent_t))*100
    }
    
    #Create Decomposition Pie Graph Parameters
    decomposition_pie <- decomposition %>% 
      mutate(end = 2 * pi * cumsum(percent_t)/sum(percent_t),
             start = lag(end, default = 0),
             middle = 0.5 * (start + end),
             hjust = ifelse(middle > pi, 1, 0),
             vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
    
    #Create Graphic Titles
    gtitle<- (ifelse(i=="ZERO","Decomposition of Zero-Dose Inequity",
                     ifelse(i=="FULL","Decomposition of Fully Immunized for Age Equity", 
                            ifelse(i=="COMPLETE","Decomposition of Equity in Completing Vaccination Schedule",
                                   paste(paste("Decomposition of",i, sep=" "),"Coverage Equity", sep=" ")))))
    
    #Store Decomposition Pie Graph
    pie_i<- ggplot(decomposition_pie, aes(ymax=end, ymin=start, xmax=4.25, xmin=3, fill = factor(name_t))) +
      geom_rect() +
      ggtitle(gtitle) +
      theme(plot.title = element_text(size=28,face="bold",hjust = 0),
            legend.title=element_blank(),
            legend.text=element_text(size=22), # Increase text font size
            legend.spacing.y = unit(0.5, "cm"), # Row spacing in Legend
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      guides(fill = guide_legend(byrow = TRUE)) + # Required to add spacing in Legend (with legend.spacing.y)
      coord_polar(theta="y") +
      xlim(c(2, 4.25)) + #Turns the Pie into a Donut Chart
      scale_fill_manual(values=c("#E0F3F8","#4575b4","#74add1","#d73027", "#f46d43", "#fdae61","#fee090","grey80"),
                        breaks=c("underage_i","v101", "v025", "v106", "v190", "b4", "v481","Residual"),
                        labels=c(paste(paste("Underage:",round(decomposition_pie$percent_t[1],1), sep=" "),"%", sep=""),
                                 paste(paste(paste(GEO,":",sep=""),round(decomposition_pie$percent_t[2],1), sep=" "),"%", sep=""),
                                 paste(paste("Urban/Rural:",round(decomposition_pie$percent_t[3],1), sep=" "),"%", sep=""),
                                 paste(paste("Maternal Education Level:",round(decomposition_pie$percent_t[4],1), sep=" "),"%", sep=""),
                                 paste(paste("Wealth Quintile:",round(decomposition_pie$percent_t[5],1), sep=" "),"%", sep=""),
                                 paste(paste("Sex of Child:",round(decomposition_pie$percent_t[6],1), sep=" "),"%", sep=""),
                                 paste(paste("Covered by Health Insurance:",round(decomposition_pie$percent_t[7],2), sep=" "),"%", sep=""),
                                 paste(paste("Unexplained Variation:",round(decomposition_pie$percent_t[8],1), sep=" "),"%", sep="")))
    
    #Save Pie Chart to Output
    output_list[[paste("pie_",i, sep="")]] <- assign(paste("pie_",i, sep=""),pie_i)
    
    total <- tibble(name_t = "Total",
                    cindex_t = "" ,
                    elastic_t = "",
                    contribution_t = sum(decomposition$contribution_t, na.rm=TRUE),
                    percent_t = sum(decomposition$percent_t,na.rm=TRUE))
    
    decomposition <- rbind.data.frame(decomposition,total)
    decomposition$ciLB<- c(100*((decomposition$percent_t/100) - (1.96*sqrt(((decomposition$percent_t/100)*(1-(decomposition$percent_t/100)))/length(data_i$outcome)))))
    decomposition$ciUB<- c(100*((decomposition$percent_t/100) + (1.96*sqrt(((decomposition$percent_t/100)*(1-(decomposition$percent_t/100)))/length(data_i$outcome)))))
    
    output[,1] <- decomposition$name_t
    output <- cbind.data.frame(output,round(decomposition$percent_t,digits=2), round(decomposition$ciLB,digits=2),round(decomposition$ciUB,digits=2))
    names(output)[names(output) == "round(decomposition$percent_t, digits = 2)"] <- paste("percent_",i, sep="")
    names(output)[1] <- "factors"
    names(output)[3] <- paste("Percent95ciLB_",i, sep="")
    names(output)[4] <- paste("Percent95ciUB_",i, sep="")
    
    #Create Subnational-Level Storage Vectors
    CI_Results_GEO<- c()
    CI_Results_GEO_95ciLB<- c()
    CI_Results_GEO_95ciUB<- c()
    HII_Results_GEO<- c()
    HII_Results_GEO_95ciLB<- c()
    HII_Results_GEO_95ciUB<- c()
    CI_E_Results_GEO <- c()
    CI_E_Results_GEO_95ciLB<- c()
    CI_E_Results_GEO_95ciUB<- c()
    AEG_Composite_Results_GEO <- c()
    AEG_Composite_Results_GEO_95ciLB<- c()
    AEG_Composite_Results_GEO_95ciUB<- c()
    AEG_Sex_Results_GEO<-c()
    AEG_Rural_Results_GEO<-c()
    AEG_Insurance_Results_GEO<-c()
    REG_Sex_Results_GEO<-c()
    REG_Rural_Results_GEO<-c()
    REG_Insurance_Results_GEO<-c()
    SII_Education_Results_GEO<-c()
    SII_Wealth_Results_GEO<-c()
    RII_Education_Results_GEO<-c()
    RII_Wealth_Results_GEO<-c()
    CI_Wealth_Results_GEO<- c()
    CI_Wealth_Results_GEO_95ciLB<- c()
    CI_Wealth_Results_GEO_95ciUB<- c()
    CI_E_Wealth_Results_GEO<- c()
    CI_E_Wealth_Results_GEO_95ciLB<- c()
    CI_E_Wealth_Results_GEO_95ciUB<- c()
    AEG_Wealth_Results_GEO <- c()
    AEG_Wealth_Results_GEO_95ciLB<- c()
    AEG_Wealth_Results_GEO_95ciUB<- c()
    Coverage_Results_GEO<- c()
    Coverage_Results_GEO_95ciLB<- c()
    Coverage_Results_GEO_95ciUB<- c()
    
    #Begin Subnational Analysis
    for (k in 1:length(GEO_CI)){
      
      # Subset Data to be only data from GEO region outcome data is available
      data_k <- subset(data_i, (data_i$v101==k))
      
      # Use only that GEO region's data for equity calculation
      pred_probs_k <- subset(pred_probs, (data_i$v101==k))
      pred_probs_2_k <- subset(pred_probs_2, (data_i$v101==k))
      pred_probs_3_k <- subset(pred_probs_3, (data_i$v101==k))
      
      # Calculating the Direct Concentration Index
      direct_ci <- ci(y = data_k[,i], x = pred_probs_2_k$hci_du.response, wt=data_k$v005, type = "CI")
      CI_1 <- concentration_index(direct_ci)
      CI_1_95ciLB<- CI_1 - 1.96*sqrt(direct_ci$variance)
      CI_1_95ciUB<- CI_1 + 1.96*sqrt(direct_ci$variance)
      
      # Calculating the Horizontal Ineqity Index (HII) 
      CIFair <- ci(y = pred_probs_3_k$hci_fair, x = pred_probs_2_k$hci_du.response, wt=data_k$v005, type = "CI")
      CI_Fair <- concentration_index(CIFair)
      HII <- CI_1 - CI_Fair
      HII_95ciLB<- HII - (1.96*sqrt((direct_ci$variance + CIFair$variance)/length(data_i[,i])))
      HII_95ciUB<- HII + (1.96*sqrt((direct_ci$variance + CIFair$variance)/length(data_i[,i])))
      
      # Calculating the Erreygers Corrected Concentration Index 
      CIE <- ci(y = data_k[,i], x = pred_probs_2_k$hci_du.response, wt=data_k$v005, type = "CIc")
      CI_E <- concentration_index(CIE)
      CI_E_95ciLB<- CI_E - 1.96*sqrt(CIE$variance)
      CI_E_95ciUB<- CI_E + 1.96*sqrt(CIE$variance)
      
      # Calculating the Composite Absolute Equity Gap
      rank<-c(pred_probs_2_k$hci_du.response)
      quantiles<- quantile(rank, probs = seq(0, 1, 0.20))
      comp_data_k<- data.frame(cbind(data_k[,i],data_k[,"v005"],rank))
      names(comp_data_k)[names(comp_data_k) == "V1"] <- paste("",i, sep="")
      names(comp_data_k)[names(comp_data_k) == "V2"] <- paste("v005","", sep="")
      AEG_Composite <- round(weighted.mean(comp_data_k[,i][rank>=quantiles[5]],comp_data_k[,"v005"][rank>=quantiles[5]]) - weighted.mean(comp_data_k[,i][rank<quantiles[2]],comp_data_k[,"v005"][rank<quantiles[2]]), digits=3)
      AEG_Composite_95ciLB <- AEG_Composite - (1.96*round(sqrt(((weighted.sd(comp_data[,i][rank>=quantiles[5]],comp_data[,"v005"][rank>=quantiles[5]]))^2)/length(comp_data[,i][rank>=quantiles[5]]) + (((weighted.sd(comp_data[,i][rank<quantiles[2]],comp_data[,"v005"][rank<quantiles[2]]))^2)/length(comp_data[,i][rank<quantiles[2]]))), digits=3))
      AEG_Composite_95ciUB <- AEG_Composite + (1.96*round(sqrt(((weighted.sd(comp_data[,i][rank>=quantiles[5]],comp_data[,"v005"][rank>=quantiles[5]]))^2)/length(comp_data[,i][rank>=quantiles[5]]) + (((weighted.sd(comp_data[,i][rank<quantiles[2]],comp_data[,"v005"][rank<quantiles[2]]))^2)/length(comp_data[,i][rank<quantiles[2]]))), digits=3))
      
      # Calculating State-Level Coverage
      data_kc <- subset(data_k, data_k[,"underage_i"]==0)
      coverage_GEO<- c(weighted.mean(data_kc$outcome,data_kc$v005))
      coverage_GEO_95ciLB<- round(coverage_GEO - (1.96*sqrt((coverage_GEO*(1-coverage_GEO))/length(data_kc$outcome))), digits = 3)
      coverage_GEO_95ciUB<- round(coverage_GEO + (1.96*sqrt((coverage_GEO*(1-coverage_GEO))/length(data_kc$outcome))), digits = 3)
      
      #Storing Results
      CI_Results_GEO<- c(CI_Results_GEO, round(CI_1, digits=3))
      CI_Results_GEO_95ciLB<- c(CI_Results_GEO_95ciLB, round(CI_1_95ciLB, digits=3))
      CI_Results_GEO_95ciUB<- c(CI_Results_GEO_95ciUB, round(CI_1_95ciUB, digits=3))
      HII_Results_GEO<- c(HII_Results_GEO, round(HII, digits=3))
      HII_Results_GEO_95ciLB<- c(HII_Results_GEO_95ciLB, round(HII_95ciLB, digits=3))
      HII_Results_GEO_95ciUB<- c(HII_Results_GEO_95ciUB, round(HII_95ciUB, digits=3))
      CI_E_Results_GEO<- c(CI_E_Results_GEO, round(CI_E, digits=3))
      CI_E_Results_GEO_95ciLB<- c(CI_E_Results_GEO_95ciLB, round(CI_E_95ciLB, digits=3))
      CI_E_Results_GEO_95ciUB<- c(CI_E_Results_GEO_95ciUB, round(CI_E_95ciUB, digits=3))
      AEG_Composite_Results_GEO<- c(AEG_Composite_Results_GEO, round(AEG_Composite, digits=3))
      AEG_Composite_Results_GEO_95ciLB<- c(AEG_Composite_Results_GEO_95ciLB, round(AEG_Composite_95ciLB, digits=3))
      AEG_Composite_Results_GEO_95ciUB<- c(AEG_Composite_Results_GEO_95ciUB, round(AEG_Composite_95ciUB, digits=3))
      Coverage_Results_GEO<- c(Coverage_Results_GEO, round(coverage_GEO, digits=3))
      Coverage_Results_GEO_95ciLB<- c(Coverage_Results_GEO_95ciLB, round(coverage_GEO_95ciLB, digits=3))
      Coverage_Results_GEO_95ciUB<- c(Coverage_Results_GEO_95ciUB, round(coverage_GEO_95ciUB, digits=3))
      
      #Compute State-Level Absolte Equity Gap (AEG) & Ratio Equity Gap (REG) for Binary Factors
      AEG_Sex_GEO <- round(weighted.mean(data_k[,i][data_k[,FACT[5]]==REF[5]],data_k[,"v005"][data_k[,FACT[5]]==REF[5]]) - weighted.mean(data_k[,i][data_k[,FACT[5]]!=REF[5]],data_k[,"v005"][data_k[,FACT[5]]!=REF[5]]), digits=3)
      AEG_Rural_GEO <- round(weighted.mean(data_k[,i][data_k[,FACT[2]]==REF[2]],data_k[,"v005"][data_k[,FACT[2]]==REF[2]]) - weighted.mean(data_k[,i][data_k[,FACT[2]]!=REF[2]],data_k[,"v005"][data_k[,FACT[2]]!=REF[2]]), digits=3)
      AEG_Insurance_GEO <- round(weighted.mean(data_k[,i][data_k[,FACT[6]]==REF[6]],data_k[,"v005"][data_k[,FACT[6]]==REF[6]]) - weighted.mean(data_k[,i][data_k[,FACT[6]]!=REF[6]],data_k[,"v005"][data_k[,FACT[6]]!=REF[6]]), digits=3)
      REG_Sex_GEO <- round(weighted.mean(data_k[,i][data_k[,FACT[5]]==REF[5]],data_k[,"v005"][data_k[,FACT[5]]==REF[5]])/(weighted.mean(data_k[,i][data_k[,FACT[5]]!=REF[5]],data_k[,"v005"][data_k[,FACT[5]]!=REF[5]])), digits=3)
      REG_Rural_GEO <- round(weighted.mean(data_k[,i][data_k[,FACT[2]]==REF[2]],data_k[,"v005"][data_k[,FACT[2]]==REF[2]])/(weighted.mean(data_k[,i][data_k[,FACT[2]]!=REF[2]],data_k[,"v005"][data_k[,FACT[2]]!=REF[2]])), digits=3)
      REG_Insurance_GEO <- round(weighted.mean(data_k[,i][data_k[,FACT[6]]==REF[6]],data_k[,"v005"][data_k[,FACT[6]]==REF[6]])/(weighted.mean(data_k[,i][data_k[,FACT[6]]!=REF[6]],data_k[,"v005"][data_k[,FACT[6]]!=REF[6]])), digits=3)
      
      if (length(data_k$v101)==0){
        
        SII_education_GEO<- "NA"
        RII_education_GEO<- "NA"
        SII_wealth_GEO<- "NA"
        RII_wealth_GEO<- "NA"
        CI_W_Wealth_GEO <- "NA"
        CI_W_Wealth_95ciLB_GEO<- "NA"
        CI_W_Wealth_95ciUB_GEO<- "NA"
        CI_E_Wealth_GEO <- "NA"
        CI_E_Wealth_95ciLB_GEO<- "NA"
        CI_E_Wealth_95ciUB_GEO<- "NA"
        
      }else{
        #Compute State-Level Slope Index of Ineqality (SII) & Relative Index of Inequality (RII) for Categorical Factors
        #Education SII & RII
        SII<- data.frame("SII", data_k[,FACT[3]], data_k[,i],data_k[,"v005"])
        names(SII)[names(SII) == "X.SII."] <- paste("SII","", sep="")
        names(SII)[names(SII) == "data_k...FACT.3.."] <- paste(FACT[3],"", sep="")
        names(SII)[names(SII) == "data_k...i."] <- paste("outcome","", sep="")
        names(SII)[names(SII) == "data_k....v005.."] <- paste("","v005", sep="")
        
        SII<- SII %>%
          group_by(v106) %>%
          summarise(outcome_mean=weighted.mean(outcome,v005),
                    outcome_se=(weighted.sd(outcome,v005)/(sqrt(length(outcome)))),
                    population=length(outcome))
        
        SII1<-SII
        SII1$v106<-SII1$v106*2
        SII2<-SII
        SII2$v106<-(SII2$v106*2)+1
        SII<- rbind.data.frame(SII1,SII2)
        
        if (CI_1 == "NaN"){
          SII_output <- "NA"
        } else {
          if (length(unique(data_k$v106))<5){
            SII_output <- "NA"
          } else {
            SII_output <- phe_sii(
              data = SII,
              quantile = v106,
              population = population,
              value = outcome_mean,
              rii = TRUE,
              se = outcome_se
            )
          }
        }
        
        SII_education_GEO<- ifelse(SII_output[1]=="NA","NA",round(SII_output[1], digits = 3))
        RII_education_GEO<- ifelse(SII_output[1]=="NA","NA",round(SII_output[2], digits = 3)) 
        
        #SII_education_GEO<- round(SII_output[1], digits = 3)
        #RII_education_GEO<- round(SII_output[2], digits = 3)
        
        #Wealth Quintile SII & RII
        SII<- data.frame("SII", data_k[,FACT[4]], data_k[,i],data_k[,"v005"])
        names(SII)[names(SII) == "X.SII."] <- paste("SII","", sep="")
        names(SII)[names(SII) == "data_k...FACT.4.."] <- paste(FACT[4],"", sep="")
        names(SII)[names(SII) == "data_k...i."] <- paste("outcome","", sep="")
        names(SII)[names(SII) == "data_k....v005.."] <- paste("","v005", sep="")
        
        SII<- SII %>%
          group_by(v190) %>%
          summarise(outcome_mean=weighted.mean(outcome,v005),
                    outcome_se=(weighted.sd(outcome,v005)/(sqrt(length(outcome)))),
                    population=length(outcome))
        
        if ((length(SII$v190)==5) &(CI_1 != "NaN")){
          SII_output <- phe_sii(
            data = SII,
            quantile = v190,
            population = population,
            value = outcome_mean,
            rii = TRUE,
            se = outcome_se
          )
        } else {
          SII_output <- "NA"
        }
        
        SII_wealth_GEO<- ifelse(SII_output[1]=="NA","NA",round(SII_output[1], digits = 3))
        RII_wealth_GEO<- ifelse(SII_output[1]=="NA","NA",round(SII_output[2], digits = 3)) 
        
        #Create Errygers Corrected concentration index for socio-economic status
        ci_errygers_GEO <- ci(x=data_k[,i],y=data_k[,FACT[4]],wt=data_k$v005,type = "CIc")
        CI_E_Wealth_GEO <- round(concentration_index(ci_errygers_GEO), digits = 3)
        CI_E_Wealth_95ciLB_GEO<- round(CI_E_Wealth_GEO - 1.96*sqrt(abs(ci_errygers_GEO$variance)), digits = 3)
        CI_E_Wealth_95ciUB_GEO<- round(CI_E_Wealth_GEO + 1.96*sqrt(abs(ci_errygers_GEO$variance)), digits = 3)
        
        #Create Wagstaff Corrected concentration index for socio-economic status
        ci_wagstaff_GEO <- ci(x=data_k[,i],y=data_k[,FACT[4]],wt=data_k$v005,type = "CI")
        CI_W_Wealth_GEO <- round(concentration_index(ci_wagstaff_GEO), digits = 3)
        CI_W_Wealth_95ciLB_GEO<- round(CI_W_Wealth_GEO - 1.96*sqrt(abs(ci_wagstaff_GEO$variance)), digits = 3)
        CI_W_Wealth_95ciUB_GEO<- round(CI_W_Wealth_GEO + 1.96*sqrt(abs(ci_wagstaff_GEO$variance)), digits = 3)
        
        # Calculating the Wealth-Based Absolute Equity Gap
        comp_data_k<- data.frame(cbind(data_k[,"outcome"],data_k[,"v005"],data_k[,"v190"]))
        names(comp_data_k)[names(comp_data_k) == "X1"] <- paste("",i, sep="")
        names(comp_data_k)[names(comp_data_k) == "X2"] <- paste("v005","", sep="")
        names(comp_data_k)[names(comp_data_k) == "X3"] <- paste("v190","", sep="")
        HIGH<- max(comp_data_k$v190)
        LOW<- min(comp_data_k$v190)
        AEG_Wealth_GEO <- round(weighted.mean(comp_data_k[,i][comp_data_k$v190>=HIGH],comp_data_k[,"v005"][comp_data_k$v190>=HIGH]) - weighted.mean(comp_data_k[,i][comp_data_k$v190<=LOW],comp_data_k[,"v005"][comp_data_k$v190<=LOW]), digits=3)
        AEG_Wealth_GEO_95ciLB <- round(AEG_Wealth_GEO - (1.96*round(sqrt(((weighted.sd(comp_data_k[,i][comp_data_k$v190>=HIGH],comp_data_k[,"v005"][comp_data_k$v190>=HIGH]))^2)/length(comp_data_k[,i][comp_data_k$v190>=HIGH]) + (((weighted.sd(comp_data_k[,i][comp_data_k$v190<=LOW],comp_data_k[,"v005"][comp_data_k$v190<=LOW]))^2)/length(comp_data_k[,i][comp_data_k$v190<=LOW]))), digits=3)), digits = 3)
        AEG_Wealth_GEO_95ciUB <- round(AEG_Wealth_GEO + (1.96*round(sqrt(((weighted.sd(comp_data_k[,i][comp_data_k$v190>=HIGH],comp_data_k[,"v005"][comp_data_k$v190>=HIGH]))^2)/length(comp_data_k[,i][comp_data_k$v190>=HIGH]) + (((weighted.sd(comp_data_k[,i][comp_data_k$v190<=LOW],comp_data_k[,"v005"][comp_data_k$v190<=LOW]))^2)/length(comp_data_k[,i][comp_data_k$v190<=LOW]))), digits=3)), digits = 3)
        
      }
      
      #Store other Indices
      AEG_Sex_Results_GEO<- c(AEG_Sex_Results_GEO,assign(paste("AEG_Sex_",i, sep=""),AEG_Sex_GEO))
      AEG_Rural_Results_GEO<- c(AEG_Rural_Results_GEO,assign(paste("AEG_Rural_",i, sep=""),AEG_Rural_GEO))
      AEG_Insurance_Results_GEO<- c(AEG_Insurance_Results_GEO,assign(paste("AEG_Insurance_",i, sep=""),AEG_Insurance_GEO))
      REG_Sex_Results_GEO<- c(REG_Sex_Results_GEO,assign(paste("REG_Sex_",i, sep=""),REG_Sex_GEO))
      REG_Rural_Results_GEO<- c(REG_Rural_Results_GEO,assign(paste("REG_Rural_",i, sep=""),REG_Rural_GEO))
      REG_Insurance_Results_GEO<- c(REG_Insurance_Results_GEO,assign(paste("REG_Insurance_",i, sep=""),REG_Insurance_GEO))
      SII_Education_Results_GEO<- c(SII_Education_Results_GEO,assign(paste("SII_Education_",i, sep=""),as.numeric(SII_education_GEO)))
      SII_Wealth_Results_GEO<- ifelse(SII_wealth_GEO=="NA",c(SII_Wealth_Results_GEO,assign(paste("SII_Wealth_",i, sep=""),SII_wealth_GEO)),c(SII_Wealth_Results_GEO,assign(paste("SII_Wealth_",i, sep=""),as.numeric(SII_wealth_GEO))))
      RII_Education_Results_GEO<- c(RII_Education_Results_GEO,assign(paste("RII_Education_",i, sep=""),as.numeric(RII_education_GEO)))
      RII_Wealth_Results_GEO<- ifelse(RII_wealth_GEO=="NA", c(RII_Wealth_Results_GEO,assign(paste("RII_Wealth_",i, sep=""),RII_wealth_GEO)), c(RII_Wealth_Results_GEO,assign(paste("RII_Wealth_",i, sep=""),as.numeric(RII_wealth_GEO))))
      CI_Wealth_Results_GEO<- c(CI_Wealth_Results_GEO,assign(paste("CI_Wealth_",i, sep=""),CI_W_Wealth_GEO))
      CI_Wealth_Results_GEO_95ciLB<- c(CI_Wealth_Results_GEO_95ciLB,assign(paste("CI_Wealth_95ciLB_",i, sep=""),CI_W_Wealth_95ciLB_GEO))
      CI_Wealth_Results_GEO_95ciUB<- c(CI_Wealth_Results_GEO_95ciUB,assign(paste("CI_Wealth_95ciUB_",i, sep=""),CI_W_Wealth_95ciUB_GEO))
      CI_E_Wealth_Results_GEO<- c(CI_E_Wealth_Results_GEO,assign(paste("CI_E_Wealth_",i, sep=""),CI_E_Wealth_GEO))
      CI_E_Wealth_Results_GEO_95ciLB<- c(CI_E_Wealth_Results_GEO_95ciLB,assign(paste("CI_E_Wealth_95ciLB_",i, sep=""),CI_E_Wealth_95ciLB_GEO))
      CI_E_Wealth_Results_GEO_95ciUB<- c(CI_E_Wealth_Results_GEO_95ciUB,assign(paste("CI_E_Wealth_95ciUB_",i, sep=""),CI_E_Wealth_95ciUB_GEO))
      AEG_Wealth_Results_GEO<- c(AEG_Wealth_Results_GEO,assign(paste("AEG_Wealth_GEO_",i, sep=""), AEG_Wealth_GEO))
      AEG_Wealth_Results_GEO_95ciLB<- c(AEG_Wealth_Results_GEO_95ciLB,assign(paste("AEG_Wealth_GEO_95ciLB_",i, sep=""), AEG_Wealth_GEO_95ciLB))
      AEG_Wealth_Results_GEO_95ciUB<- c(AEG_Wealth_Results_GEO_95ciUB,assign(paste("AEG_Wealth_GEO_95ciUB_",i, sep=""), AEG_Wealth_GEO_95ciUB))
      
    }
    
    
    #Create Equity-Efficiency Plane
    invisible(capture.output(efficiency_data <- data_ic %>% 
                               group_by(data_ic$v024) %>% 
                               summarise(coverage=weighted.mean(outcome,v005),
                                         underage = weighted.mean(underage_i,v005),
                                         urban_rural = weighted.mean(v025,v005)*100/2,
                                         maternal_edu = weighted.mean(v106,v005)*100/3,
                                         wealth = weighted.mean(v190,v005)*100/5,
                                         sex = weighted.mean(b4,v005)*100/2,
                                         insurance = weighted.mean(v481,v005))))
    
    if (COUNTRY=="Nigeria"){
      GEO_UNIT <- c(val_labels(dhs_data$sstate))
      GEO_NAMES <- names(GEO_UNIT)
      GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
    } else {
      if ((COUNTRY=="Uganda")&(YEAR>2012)){
        GEO_UNIT <- c(val_labels(dhs_data$GEO))
        GEO_NAMES <- names(GEO_UNIT)
        GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
      } else  {
        if (COUNTRY=="Jordan"){
          GEO_UNIT <- c(val_labels(dhs_data$v024))
          GEO_NAMES <- names(GEO_UNIT)
          GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
        } else {
          if(COUNTRY[1]=="Indonesia"){
            GEO_UNIT <- c(val_labels(dhs_data$v024))
            GEO_NAMES <- names(GEO_UNIT)
            GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
          } else {
            if ((COUNTRY=="Philippines") & (is.na(efficiency_data[18,][1])=="TRUE")){
              GEO_UNIT <- c(val_labels(dhs_data$v024))
              GEO_UNIT <- GEO_UNIT[-18]
              GEO_NAMES <- names(GEO_UNIT)
              GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
              TMP_GEO <- CI_Results_GEO
              CI_Results_GEO <- CI_Results_GEO[-18]
            } else {
              if(COUNTRY[1]=="Timor-Leste"){
                GEO_UNIT <- c(val_labels(dhs_data$v024))
                GEO_NAMES <- names(GEO_UNIT)
                GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
              } else {
                if(COUNTRY[1]=="Lesotho"){
                  GEO_UNIT <- c(val_labels(dhs_data$v024))
                  GEO_NAMES <- names(GEO_UNIT)
                  GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                } else {
                  if(COUNTRY[1]=="Namibia"){
                    GEO_UNIT <- c(val_labels(dhs_data$v024))
                    GEO_UNIT <- head(GEO_UNIT, -1)
                    GEO_NAMES <- names(GEO_UNIT)
                    GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                  } else {
                    if(COUNTRY[1]=="Zimbabwe"){
                      GEO_UNIT <- c(val_labels(dhs_data$v024))
                      GEO_NAMES <- names(GEO_UNIT)
                      GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                    } else {
                      if (COUNTRY=="Egypt"){
                        GEO_UNIT <- c(val_labels(dhs_data$GEO))
                        GEO_NAMES <- names(GEO_UNIT)
                        GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                      } else  {
                        if (COUNTRY=="Yemen"){
                          GEO_UNIT <- c(val_labels(dhs_data$GEO))
                          GEO_NAMES <- names(GEO_UNIT)
                          GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                        } else  {
                          if ((COUNTRY=="India") & (YEAR<=2007)){
                            GEO_UNIT <- c(val_labels(dhs_data$GEO))
                            GEO_NAMES <- names(GEO_UNIT)
                            GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                          } else {
                            if ((COUNTRY=="Uganda")&(YEAR==2011)){
                              GEO_UNIT <- c(val_labels(dhs_data$GEO))
                              GEO_UNIT<-GEO_UNIT[1:10]
                              GEO_NAMES <- names(GEO_UNIT)
                              GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                            } else  {
                              if ((COUNTRY=="Madagascar")& (YEAR<=2021)){
                                GEO_UNIT <- c(val_labels(dhs_data$v024))
                                GEO_NAMES <- names(GEO_UNIT)
                                GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                              } else  { 
                                if (FLAG[1]==1){
                                  GEO_UNIT <- c(val_labels(dhs_data$GEO))
                                  GEO_NAMES <- names(GEO_UNIT)
                                  GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                                } else {
                                  GEO_UNIT <- c(val_labels(dhs_data$v101))
                                  GEO_NAMES <- names(GEO_UNIT)
                                  GEO_LABEL<- paste(GEO_UNIT, GEO_NAMES, sep=" = ")
                                }}}}}}}}}}}}}}}
    
    if (COUNTRY=="Egypt"){
      efficiency <- cbind.data.frame(CI_Results_GEO[-c(2,5)], GEO_UNIT[-c(2,5)], GEO_NAMES[-c(2,5)], GEO_LABEL[-c(2,5)], efficiency_data)
    } else {
      efficiency <- cbind.data.frame(CI_Results_GEO, GEO_UNIT, GEO_NAMES, GEO_LABEL, efficiency_data)
    }
    
    efficiency$equity_axis <- abs(ifelse(efficiency$CI_Results_GEO>=0, 1 - efficiency$CI_Results_GEO, (efficiency$CI_Results_GEO*-1) - 1))
    
    etitle<- (ifelse(i=="ZERO","Zero-Dose Equity-Prevalence Plane",
                     ifelse(i=="FULL","Fully Immunized for Age Equity-Coverage Plane", 
                            ifelse(i=="COMPLETE","Completed Vaccination Schedule Equity-Coverage Plane",
                                   paste(i,"Vaccination Equity-Coverage Plane", sep=" ")))))
    
    efficiency_list[[paste("",i, sep="")]] <- assign(paste("",i, sep=""),efficiency)
    
    color_vector<-rep("#000000", length(GEO_UNIT))
    
    if (i=="ZERO"){
      efficiency_i<- ggplot(efficiency_list[[i]],aes(x= CI_Results_GEO,y=coverage*100, colour = GEO_NAMES, label=GEO_UNIT)) +
        geom_point(alpha = .4,shape=20, color="blue3") +
        scale_y_continuous(name="Prevalence") +
        scale_x_continuous(name = "Inequality: Composite Index") +
        theme_classic(base_size = 16) +
        geom_label_repel(label=GEO_UNIT, size=6, max.overlaps = Inf, fill = "white") +
        ggtitle(etitle) +
        theme(axis.title.y = element_text(angle = 0)) +
        guides(colour = guide_legend(ncol = 3)) +
        theme(legend.position="bottom") +
        scale_colour_manual(values=color_vector, labels=GEO_LABEL) +
        guides(color = guide_legend(override.aes = list(size = 0)))
      
    } else{
      efficiency_i<- ggplot(efficiency_list[[i]],aes(x= equity_axis,y=coverage*100, colour = GEO_NAMES, label=GEO_UNIT)) +
        geom_point(alpha = .4,shape=20, color="blue3") +
        scale_y_continuous(name="% Coverage") +
        scale_x_continuous(name = "Equity: (1 - Composite Index)") +
        theme_classic(base_size = 16) +
        geom_label_repel(label=GEO_UNIT, size=6, max.overlaps = Inf, fill = "white") +
        ggtitle(etitle)+
        theme(axis.title.y = element_text(angle = 0)) + 
        guides(colour = guide_legend(ncol = 3)) +
        theme(legend.position="bottom") +
        labs(colour = GEO) +
        scale_colour_manual(values=color_vector, labels=GEO_LABEL) +
        guides(color = guide_legend(override.aes = list(size = 0)))
      
    }
    
    
    
    output_list[[paste("efficiency_",i, sep="")]] <- assign(paste("efficiency_",i, sep=""),efficiency_i)
    
    if ((COUNTRY=="Philippines") & (is.na(efficiency_data[18,][1])=="TRUE")){
      TMP_GEO_Coverage <- Coverage_Results_GEO
      Coverage_Results_GEO <- Coverage_Results_GEO[-18]
    }
    
    if (MAP=="YES"){
      #Create Mapping Data
      map <- cbind.data.frame(Coverage_Results_GEO, CI_Results_GEO, GEO_UNIT)
      names(map)[names(map) == "GEO_UNIT"] <- "REGCODE"
      map_data = left_join(mapping$sdr_subnational_boundaries, map)
      maptitle_equity<- (ifelse(i=="ZERO","Zero-Dose Equity Heat Map",
                                ifelse(i=="FULL","Fully Immunized Equity Heat Map", 
                                       ifelse(i=="COMPLETE","Completed Vaccination Schedule Equity Heat Map",
                                              paste(i,"Vaccination Equity Heat Map", sep=" ")))))
      
      maptitle_coverage<- (ifelse(i=="ZERO","Zero-Dose Prevalence Map",
                                  ifelse(i=="FULL","Fully Immunized Coverage Map", 
                                         ifelse(i=="COMPLETE","Completed Vaccination Schedule Coverage Map",
                                                paste(i,"Vaccination Coverage Map", sep=" ")))))
      
      map_list[[paste("",i, sep="")]] <- assign(paste("",i, sep=""),map_data)
      
      #Create Composite Equity Heat Map
      map_i<- (ggplot(map_list[[i]]) + 
                 geom_sf(aes(fill=CI_Results_GEO)) + 
                 ggtitle(maptitle_equity) + 
                 scale_fill_distiller(palette = "RdYlGn", name = "Concentration\nIndex") +
                 theme(panel.background = element_rect(fill = "white"),
                       axis.ticks = element_blank(),
                       axis.text = element_blank(),
                       panel.grid = element_line(color = "white")))
      
      #Store Equity Heat Maps
      output_list[[paste("equity_map_",i, sep="")]] <- assign(paste("equity_map_",i, sep=""),map_i)
      
      if(i=="ZERO"){
        #Create Coverage Map
        map_c<- (ggplot(map_list[[i]]) + 
                   geom_sf(aes(fill=Coverage_Results_GEO)) + 
                   ggtitle(maptitle_coverage) + 
                   scale_fill_distiller(palette = "RdYlGn", name = "Prevalence (%)") +
                   theme(panel.background = element_rect(fill = "white"),
                         axis.ticks = element_blank(),
                         axis.text = element_blank(),
                         panel.grid = element_line(color = "white")))
      }else{
        map_c<- (ggplot(map_list[[i]]) + 
                   geom_sf(aes(fill=Coverage_Results_GEO)) + 
                   ggtitle(maptitle_coverage) + 
                   scale_fill_distiller(palette = "RdYlGn", direction = 1, name = "Coverage (%)") +
                   theme(panel.background = element_rect(fill = "white"),
                         axis.ticks = element_blank(),
                         axis.text = element_blank(),
                         panel.grid = element_line(color = "white")))
      }
      
      #Store Coverage Maps
      output_list[[paste("coverage_map_",i, sep="")]] <- assign(paste("coverage_map_",i, sep=""),map_c)
      
    }
    
    if ((COUNTRY=="Philippines") & (is.na(efficiency_data[18,][1])=="TRUE")){
      CI_Results_GEO <- TMP_GEO
      Coverage_Results_GEO <- TMP_GEO_Coverage
    }
    
    CI_Results_GEO_Output<- cbind.data.frame(CI_Results_GEO_Output, CI_Results_GEO)
    CI_Results_GEO_Output_95ciLB<- cbind.data.frame(CI_Results_GEO_Output_95ciLB, CI_Results_GEO_95ciLB)
    CI_Results_GEO_Output_95ciUB<- cbind.data.frame(CI_Results_GEO_Output_95ciUB, CI_Results_GEO_95ciUB)
    HII_Results_GEO_Output<- cbind.data.frame(HII_Results_GEO_Output, HII_Results_GEO)
    HII_Results_GEO_Output_95ciLB<- cbind.data.frame(HII_Results_GEO_Output_95ciLB, HII_Results_GEO_95ciLB)
    HII_Results_GEO_Output_95ciUB<- cbind.data.frame(HII_Results_GEO_Output_95ciUB, HII_Results_GEO_95ciUB)
    CI_E_Results_GEO_Output<- cbind.data.frame(CI_E_Results_GEO_Output, CI_E_Results_GEO)
    CI_E_Results_GEO_Output_95ciLB<- cbind.data.frame(CI_E_Results_GEO_Output_95ciLB, CI_E_Results_GEO_95ciLB)
    CI_E_Results_GEO_Output_95ciUB<- cbind.data.frame(CI_E_Results_GEO_Output_95ciUB, CI_E_Results_GEO_95ciUB)
    AEG_Composite_Results_GEO_Output<- cbind.data.frame(AEG_Composite_Results_GEO_Output, AEG_Composite_Results_GEO)
    AEG_Composite_Results_GEO_Output_95ciLB<- cbind.data.frame(AEG_Composite_Results_GEO_Output_95ciLB, AEG_Composite_Results_GEO_95ciLB)
    AEG_Composite_Results_GEO_Output_95ciUB<- cbind.data.frame(AEG_Composite_Results_GEO_Output_95ciUB, AEG_Composite_Results_GEO_95ciUB)
    Coverage_Results_GEO_Output<- cbind.data.frame(Coverage_Results_GEO_Output, Coverage_Results_GEO)
    Coverage_Results_GEO_Output_95ciLB <- cbind.data.frame(Coverage_Results_GEO_Output_95ciLB, Coverage_Results_GEO_95ciLB)
    Coverage_Results_GEO_Output_95ciUB <- cbind.data.frame(Coverage_Results_GEO_Output_95ciUB, Coverage_Results_GEO_95ciUB)
    AEG_Sex_Results_GEO_Output<- cbind.data.frame(AEG_Sex_Results_GEO_Output, AEG_Sex_Results_GEO)
    AEG_Rural_Results_GEO_Output<- cbind.data.frame(AEG_Rural_Results_GEO_Output, AEG_Rural_Results_GEO)
    AEG_Insurance_Results_GEO_Output<- cbind.data.frame(AEG_Insurance_Results_GEO_Output, AEG_Insurance_Results_GEO)
    REG_Sex_Results_GEO_Output<- cbind.data.frame(REG_Sex_Results_GEO_Output, REG_Sex_Results_GEO)
    REG_Rural_Results_GEO_Output<- cbind.data.frame(REG_Rural_Results_GEO_Output, REG_Rural_Results_GEO)
    REG_Insurance_Results_GEO_Output<- cbind.data.frame(REG_Insurance_Results_GEO_Output, REG_Insurance_Results_GEO)
    SII_Education_Results_GEO_Output<- cbind.data.frame(SII_Education_Results_GEO_Output, SII_Education_Results_GEO)
    SII_Wealth_Results_GEO_Output<- cbind.data.frame(SII_Wealth_Results_GEO_Output, SII_Wealth_Results_GEO)
    RII_Education_Results_GEO_Output<- cbind.data.frame(RII_Education_Results_GEO_Output, RII_Education_Results_GEO)
    RII_Wealth_Results_GEO_Output<- cbind.data.frame(RII_Wealth_Results_GEO_Output, RII_Wealth_Results_GEO)
    CI_Wealth_Results_GEO_Output<- cbind.data.frame(CI_Wealth_Results_GEO_Output, CI_Wealth_Results_GEO)
    CI_Wealth_Results_GEO_Output_95ciLB<- cbind.data.frame(CI_Wealth_Results_GEO_Output_95ciLB, CI_Wealth_Results_GEO_95ciLB)
    CI_Wealth_Results_GEO_Output_95ciUB<- cbind.data.frame(CI_Wealth_Results_GEO_Output_95ciUB, CI_Wealth_Results_GEO_95ciUB)
    CI_E_Wealth_Results_GEO_Output<- cbind.data.frame(CI_E_Wealth_Results_GEO_Output, CI_E_Wealth_Results_GEO)
    CI_E_Wealth_Results_GEO_Output_95ciLB<- cbind.data.frame(CI_E_Wealth_Results_GEO_Output_95ciLB, CI_E_Wealth_Results_GEO_95ciLB)
    CI_E_Wealth_Results_GEO_Output_95ciUB<- cbind.data.frame(CI_E_Wealth_Results_GEO_Output_95ciUB, CI_E_Wealth_Results_GEO_95ciUB)
    AEG_Wealth_Results_GEO_Output<- cbind.data.frame(AEG_Wealth_Results_GEO_Output, AEG_Wealth_Results_GEO)
    AEG_Wealth_Results_GEO_95ciLB_Output<- cbind.data.frame(AEG_Wealth_Results_GEO_95ciLB_Output, AEG_Wealth_Results_GEO_95ciLB)
    AEG_Wealth_Results_GEO_95ciUB_Output<- cbind.data.frame(AEG_Wealth_Results_GEO_95ciUB_Output, AEG_Wealth_Results_GEO_95ciUB)
    
    names(CI_Results_GEO_Output)[names(CI_Results_GEO_Output) == "CI_Results_GEO"] <- paste("CI_GEO_",i, sep="")
    names(CI_Results_GEO_Output_95ciLB)[names(CI_Results_GEO_Output_95ciLB) == "CI_Results_GEO_95ciLB"] <- paste("CI_GEO_95ciLB_",i, sep="")
    names(CI_Results_GEO_Output_95ciUB)[names(CI_Results_GEO_Output_95ciUB) == "CI_Results_GEO_95ciUB"] <- paste("CI_GEO_95ciUB_",i, sep="")
    names(HII_Results_GEO_Output)[names(HII_Results_GEO_Output) == "HII_Results_GEO"] <- paste("HII_GEO_",i, sep="")
    names(HII_Results_GEO_Output_95ciLB)[names(HII_Results_GEO_Output_95ciLB) == "HII_Results_GEO_95ciLB"] <- paste("HII_GEO_95ciLB_",i, sep="")
    names(HII_Results_GEO_Output_95ciUB)[names(HII_Results_GEO_Output_95ciUB) == "HII_Results_GEO_95ciUB"] <- paste("HII_GEO_95ciUB_",i, sep="")
    names(CI_E_Results_GEO_Output)[names(CI_E_Results_GEO_Output) == "CI_E_Results_GEO"] <- paste("CI_E_GEO_",i, sep="")
    names(CI_E_Results_GEO_Output_95ciLB)[names(CI_E_Results_GEO_Output_95ciLB) == "CI_E_Results_GEO_95ciLB"] <- paste("CI_E_GEO_95ciLB_",i, sep="")
    names(CI_E_Results_GEO_Output_95ciUB)[names(CI_E_Results_GEO_Output_95ciUB) == "CI_E_Results_GEO_95ciUB"] <- paste("CI_E_GEO_95ciUB_",i, sep="")
    names(AEG_Composite_Results_GEO_Output)[names(AEG_Composite_Results_GEO_Output) == "AEG_Composite_Results_GEO"] <- paste("AEG_Composite_GEO_",i, sep="")
    names(AEG_Composite_Results_GEO_Output_95ciLB)[names(AEG_Composite_Results_GEO_Output_95ciLB) == "AEG_Composite_Results_GEO_95ciLB"] <- paste("AEG_Composite_GEO_95ciLB_",i, sep="")
    names(AEG_Composite_Results_GEO_Output_95ciUB)[names(AEG_Composite_Results_GEO_Output_95ciUB) == "AEG_Composite_Results_GEO_95ciUB"] <- paste("AEG_Composite_GEO_95ciUB_",i, sep="")
    names(AEG_Wealth_Results_GEO_Output)[names(AEG_Wealth_Results_GEO_Output) == "AEG_Wealth_Results_GEO"] <- paste("AEG_Wealth_Results_GEO_",i, sep="")
    names(AEG_Wealth_Results_GEO_95ciLB_Output)[names(AEG_Wealth_Results_GEO_95ciLB_Output) == "AEG_Wealth_Results_GEO_95ciLB"] <- paste("AEG_Wealth_Results_GEO_95ciLB_",i, sep="")
    names(AEG_Wealth_Results_GEO_95ciUB_Output)[names(AEG_Wealth_Results_GEO_95ciUB_Output) == "AEG_Wealth_Results_GEO_95ciUB"] <- paste("AEG_Wealth_Results_GEO_95ciUB_",i, sep="")
    names(Coverage_Results_GEO_Output)[names(Coverage_Results_GEO_Output) == "Coverage_Results_GEO"] <- paste("Coverage_GEO_",i, sep="")
    names(Coverage_Results_GEO_Output_95ciLB)[names(Coverage_Results_GEO_Output_95ciLB) == "Coverage_Results_GEO_95ciLB"] <- paste("Coverage_Results_GEO_95ciLB_",i, sep="")
    names(Coverage_Results_GEO_Output_95ciUB)[names(Coverage_Results_GEO_Output_95ciUB) == "Coverage_Results_GEO_95ciUB"] <- paste("Coverage_Results_GEO_95ciUB_",i, sep="")
    
    names(AEG_Sex_Results_GEO_Output)[names(AEG_Sex_Results_GEO_Output) == "AEG_Sex_Results_GEO"] <- paste("AEG_Sex_Results_GEO_",i, sep="")
    names(AEG_Rural_Results_GEO_Output)[names(AEG_Rural_Results_GEO_Output) == "AEG_Rural_Results_GEO"] <- paste("AEG_Rural_Results_GEO_",i, sep="")
    names(AEG_Insurance_Results_GEO_Output)[names(AEG_Insurance_Results_GEO_Output) == "AEG_Insurance_Results_GEO"] <- paste("AEG_Insurance_Results_GEO_",i, sep="")
    names(REG_Sex_Results_GEO_Output)[names(REG_Sex_Results_GEO_Output) == "REG_Sex_Results_GEO"] <- paste("REG_Sex_Results_GEO_",i, sep="")
    names(REG_Rural_Results_GEO_Output)[names(REG_Rural_Results_GEO_Output) == "REG_Rural_Results_GEO"] <- paste("REG_Rural_Results_GEO_",i, sep="")
    names(REG_Insurance_Results_GEO_Output)[names(REG_Insurance_Results_GEO_Output) == "REG_Insurance_Results_GEO"] <- paste("REG_Insurance_Results_GEO_",i, sep="")
    names(SII_Education_Results_GEO_Output)[names(SII_Education_Results_GEO_Output) == "SII_Education_Results_GEO"] <- paste("SII_Education_Results_GEO_",i, sep="")
    names(SII_Wealth_Results_GEO_Output)[names(SII_Wealth_Results_GEO_Output) == "SII_Wealth_Results_GEO"] <- paste("SII_Wealth_Results_GEO_",i, sep="")
    names(RII_Education_Results_GEO_Output)[names(RII_Education_Results_GEO_Output) == "RII_Education_Results_GEO"] <- paste("RII_Education_Results_GEO_",i, sep="")
    names(RII_Wealth_Results_GEO_Output)[names(RII_Wealth_Results_GEO_Output) == "RII_Wealth_Results_GEO"] <- paste("RII_Wealth_Results_GEO_",i, sep="")
    names(CI_Wealth_Results_GEO_Output)[names(CI_Wealth_Results_GEO_Output) == "CI_Wealth_Results_GEO"] <- paste("CI_Wealth_Results_GEO_",i, sep="")
    names(CI_Wealth_Results_GEO_Output_95ciLB)[names(CI_Wealth_Results_GEO_Output_95ciLB) == "CI_Wealth_Results_GEO_95ciLB"] <- paste("CI_Wealth_Results_GEO_95ciLB_",i, sep="")
    names(CI_Wealth_Results_GEO_Output_95ciUB)[names(CI_Wealth_Results_GEO_Output_95ciUB) == "CI_Wealth_Results_GEO_95ciUB"] <- paste("CI_Wealth_Results_GEO_95ciUB_",i, sep="")
    names(CI_E_Wealth_Results_GEO_Output)[names(CI_E_Wealth_Results_GEO_Output) == "CI_E_Wealth_Results_GEO"] <- paste("CI_E_Wealth_Results_GEO_",i, sep="")
    names(CI_E_Wealth_Results_GEO_Output_95ciLB)[names(CI_E_Wealth_Results_GEO_Output_95ciLB) == "CI_E_Wealth_Results_GEO_95ciLB"] <- paste("CI_E_Wealth_Results_GEO_95ciLB_",i, sep="")
    names(CI_E_Wealth_Results_GEO_Output_95ciUB)[names(CI_E_Wealth_Results_GEO_Output_95ciUB) == "CI_E_Wealth_Results_GEO_95ciUB"] <- paste("CI_E_Wealth_Results_GEO_95ciUB_",i, sep="")
    
    print(paste(i,"analysis completed", sep=" "))
  }
  
  
  CI_Results_GEO_Output[,1]<- GEO_CI
  CI_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  CI_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  HII_Results_GEO_Output[,1]<- GEO_CI
  HII_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  HII_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  CI_E_Results_GEO_Output[,1]<- GEO_CI
  CI_E_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  CI_E_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  AEG_Composite_Results_GEO_Output[,1]<- GEO_CI
  AEG_Composite_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  AEG_Composite_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  Coverage_Results_GEO_Output[,1]<- GEO_CI
  Coverage_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  Coverage_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  AEG_Sex_Results_GEO_Output[,1]<- GEO_CI
  AEG_Rural_Results_GEO_Output[,1]<- GEO_CI
  AEG_Insurance_Results_GEO_Output[,1]<- GEO_CI
  REG_Sex_Results_GEO_Output[,1]<- GEO_CI
  REG_Rural_Results_GEO_Output[,1]<- GEO_CI
  REG_Insurance_Results_GEO_Output[,1]<- GEO_CI
  SII_Education_Results_GEO_Output[,1]<- GEO_CI
  SII_Wealth_Results_GEO_Output[,1]<- GEO_CI
  RII_Education_Results_GEO_Output[,1]<- GEO_CI
  RII_Wealth_Results_GEO_Output[,1]<- GEO_CI
  CI_Wealth_Results_GEO_Output[,1]<- GEO_CI
  CI_Wealth_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  CI_Wealth_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  CI_E_Wealth_Results_GEO_Output[,1]<- GEO_CI
  CI_E_Wealth_Results_GEO_Output_95ciLB[,1]<- GEO_CI
  CI_E_Wealth_Results_GEO_Output_95ciUB[,1]<- GEO_CI
  AEG_Wealth_Results_GEO_Output[,1]<- GEO_CI
  AEG_Wealth_Results_GEO_95ciLB_Output[,1]<- GEO_CI
  AEG_Wealth_Results_GEO_95ciUB_Output[,1]<- GEO_CI
  
  
  names(CI_Results_GEO_Output)[1] <-GEO
  names(CI_Results_GEO_Output_95ciLB)[1] <-GEO
  names(CI_Results_GEO_Output_95ciUB)[1] <-GEO
  names(HII_Results_GEO_Output)[1]<- GEO
  names(HII_Results_GEO_Output_95ciLB)[1]<- GEO
  names(HII_Results_GEO_Output_95ciUB)[1]<- GEO
  names(CI_E_Results_GEO_Output)[1]<- GEO
  names(CI_E_Results_GEO_Output_95ciLB)[1]<- GEO
  names(CI_E_Results_GEO_Output_95ciUB)[1]<- GEO
  names(AEG_Composite_Results_GEO_Output)[1]<- GEO
  names(AEG_Composite_Results_GEO_Output_95ciLB)[1]<- GEO
  names(AEG_Composite_Results_GEO_Output_95ciUB)[1]<- GEO
  names(Coverage_Results_GEO_Output)[1]<- GEO
  names(Coverage_Results_GEO_Output_95ciLB)[1]<- GEO
  names(Coverage_Results_GEO_Output_95ciUB)[1]<- GEO
  names(AEG_Sex_Results_GEO_Output)[1] <-GEO
  names(AEG_Rural_Results_GEO_Output)[1] <-GEO
  names(AEG_Insurance_Results_GEO_Output)[1] <-GEO
  names(REG_Sex_Results_GEO_Output)[1] <-GEO
  names(REG_Rural_Results_GEO_Output)[1] <-GEO
  names(REG_Insurance_Results_GEO_Output)[1] <-GEO
  names(SII_Education_Results_GEO_Output)[1] <-GEO
  names(SII_Wealth_Results_GEO_Output)[1] <-GEO
  names(RII_Education_Results_GEO_Output)[1] <-GEO
  names(RII_Wealth_Results_GEO_Output)[1] <-GEO
  names(CI_Wealth_Results_GEO_Output)[1] <-GEO
  names(CI_Wealth_Results_GEO_Output_95ciLB)[1] <-GEO
  names(CI_Wealth_Results_GEO_Output_95ciUB)[1] <-GEO
  names(CI_E_Wealth_Results_GEO_Output)[1] <-GEO
  names(CI_E_Wealth_Results_GEO_Output_95ciLB)[1] <-GEO
  names(CI_E_Wealth_Results_GEO_Output_95ciUB)[1] <-GEO
  names(AEG_Wealth_Results_GEO_Output)[1]<- GEO
  names(AEG_Wealth_Results_GEO_95ciLB_Output)[1]<- GEO
  names(AEG_Wealth_Results_GEO_95ciUB_Output)[1]<- GEO
  
  #Reset Warnings
  options(warn = oldw)
  
  #Create Output of Function
  output[,1] <- c("Underage",GEO,"Rural","Maternal Education","Wealth Index","Sex of Child","Health Insurance", "Residual","Total")
  
  output_list <- c(output_list, output)
  
  equity_national <- data.frame(VACCINES, Coverage_Results, Coverage_Results, Coverage_Results_95ciLB, Coverage_Results_95ciUB, CI_Results, CI_Results_95ciLB, CI_Results_95ciUB, HII_Results, HII_Results_95ciLB, HII_Results_95ciUB, CI_E_Results, CI_E_Results_95ciLB, CI_E_Results_95ciUB, AEG_Composite_Results, AEG_Composite_Results_95ciLB, AEG_Composite_Results_95ciUB, AEG_Sex_Results, AEG_Rural_Results, AEG_Insurance_Results, REG_Sex_Results, REG_Rural_Results, REG_Insurance_Results, SII_Education_Results, SII_Wealth_Results, SII_Region_Results, RII_Education_Results, RII_Wealth_Results, RII_Region_Results, CI_Wealth_Results, CI_Wealth_Results_95ciLB, CI_Wealth_Results_95ciUB, CI_E_Wealth_Results, CI_E_Wealth_Results_95ciLB, CI_E_Wealth_Results_95ciUB, AEG_Wealth_Results, AEG_Wealth_Results_95ciLB, AEG_Wealth_Results_95ciUB)
  
  equity_state<- data.frame(Coverage_Results_GEO_Output, Coverage_Results_GEO_Output_95ciLB, Coverage_Results_GEO_Output_95ciUB, CI_Results_GEO_Output, CI_Results_GEO_Output_95ciLB, CI_Results_GEO_Output_95ciUB, HII_Results_GEO_Output, HII_Results_GEO_Output_95ciLB, HII_Results_GEO_Output_95ciUB, CI_E_Results_GEO_Output, CI_E_Results_GEO_Output_95ciLB, CI_E_Results_GEO_Output_95ciUB, AEG_Composite_Results_GEO_Output, AEG_Composite_Results_GEO_Output_95ciLB, AEG_Composite_Results_GEO_Output_95ciUB, AEG_Wealth_Results_GEO_Output, AEG_Wealth_Results_GEO_95ciLB_Output, AEG_Wealth_Results_GEO_95ciUB_Output)
  
  reference<-data.frame(FACTORS,REF)
  
  output_list <- c(output_list, equity_national, Coverage_Results_GEO_Output, Coverage_Results_GEO_Output_95ciLB, Coverage_Results_GEO_Output_95ciUB, CI_Results_GEO_Output, CI_Results_GEO_Output_95ciLB, CI_Results_GEO_Output_95ciUB, HII_Results_GEO_Output, HII_Results_GEO_Output_95ciLB, HII_Results_GEO_Output_95ciUB, CI_E_Results_GEO_Output, CI_E_Results_GEO_Output_95ciLB, CI_E_Results_GEO_Output_95ciUB, AEG_Composite_Results_GEO_Output, AEG_Composite_Results_GEO_Output_95ciLB, AEG_Composite_Results_GEO_Output_95ciUB, AEG_Sex_Results_GEO_Output, AEG_Rural_Results_GEO_Output, AEG_Insurance_Results_GEO_Output, REG_Sex_Results_GEO_Output, REG_Rural_Results_GEO_Output, REG_Insurance_Results_GEO_Output, SII_Education_Results_GEO_Output, SII_Wealth_Results_GEO_Output, RII_Education_Results_GEO_Output, RII_Wealth_Results_GEO_Output, CI_Wealth_Results_GEO_Output, CI_Wealth_Results_GEO_Output_95ciLB, CI_Wealth_Results_GEO_Output_95ciUB, CI_E_Wealth_Results_GEO_Output, CI_E_Wealth_Results_GEO_Output_95ciLB, CI_E_Wealth_Results_GEO_Output_95ciUB, AEG_Wealth_Results_GEO_Output, AEG_Wealth_Results_GEO_95ciLB_Output, AEG_Wealth_Results_GEO_95ciUB_Output, reference)
  
  return(output_list)
}


##### Run Function #####
results <- VERSE(DATA,COUNTRY,YEAR,VACCINES,SCHEDULE,FACTORS,GEO,MAP)

# See Guidance Documentation for how to call specific results within the results list


##### JPEG and CSV Exportation #####

# Exporting the decomposition pie chart into the working directory
for (i in VACCINES) {
  X <- paste("pie_",i, sep="")
  jpeg(filename = paste0(COUNTRY, "_", YEAR, "_decomposition_pie_", i, ".jpg"), width = 1000, height = 650, quality = 100)
  print(results[X])
  dev.off()
}

# Exporting the equity heat map into the working directory
for (i in VACCINES) {
  X <- paste("equity_map_",i, sep="")
  jpeg(filename = paste0(COUNTRY, "_", YEAR, "_map_equity_", i, ".jpg"), width = 500, height = 420, quality = 100)
  print(results[X])
  dev.off()
}

# Exporting the vaccine coverage and health outcome prevalence map into the working directory
for (i in VACCINES) {
  X <- paste("coverage_map_",i, sep="")
  jpeg(filename = paste0(COUNTRY, "_", YEAR, "_map_coverage_", i, ".jpg"), width = 500, height = 420, quality = 100)
  print(results[X])
  dev.off()
}

# Exporting the equity-coverage plane into the working directory
for (i in VACCINES) {
  X <- paste("efficiency_",i, sep="")
  jpeg(filename = paste0(COUNTRY, "_", YEAR, "_EffEquPlane_", i, ".jpg"), width = 1000, height = 1000, quality = 100)
  print(results[X])
  dev.off()
}

# Exporting the equity-coverage plane zoomed in (but cannot use the legend) into the working directory - CURRENTLY INACTIVE
#for (i in VACCINES) {
#  X <- paste("efficiency_",i, sep="")
#  jpeg(filename = paste0(COUNTRY, "_", YEAR, "_EffEquPlane_", i, "_DONOTUSELEGEND.jpg"), width = 300, height = 300, quality = 100)
#  print(results[X])
#  dev.off()
#}


# Generate CSV files with indicator values at the National level
national_data <- as.data.frame(results$VACCINES)
national_data$Coverage_Results <- results$Coverage_Results
national_data$CI_Wealth_Results <- results$CI_Wealth_Results
national_data$CI_Wealth_Results_95ciLB <- results$CI_Wealth_Results_95ciLB
national_data$CI_Wealth_Results_95ciUB <- results$CI_Wealth_Results_95ciUB
national_data$CI_E_Wealth_Results <- results$CI_E_Wealth_Results
national_data$CI_E_Wealth_Results_95ciLB <- results$CI_E_Wealth_Results_95ciLB
national_data$CI_E_Wealth_Results_95ciUB <- results$CI_E_Wealth_Results_95ciUB
national_data$CI_Results <- results$CI_Results
national_data$CI_Results_95ciLB <- results$CI_Results_95ciLB
national_data$CI_Results_95ciUB <- results$CI_Results_95ciUB
national_data$CI_E_Results <- results$CI_E_Results
national_data$CI_E_Results_95ciLB <- results$CI_E_Results_95ciLB
national_data$CI_E_Results_95ciUB <- results$CI_E_Results_95ciUB
national_data$AEG_Composite_Results <- results$AEG_Composite_Results
national_data$AEG_Composite_Results_95ciLB <- results$AEG_Composite_Results_95ciLB
national_data$AEG_Composite_Results_95ciUB <- results$AEG_Composite_Results_95ciUB

write.csv(national_data, file = paste0(COUNTRY, "_", YEAR, "_NATIONAL_DATA.csv"))


# Generate CSV files with indicators values at the Subnational level
# Preparing coverage data to be exported
coverage_data <- as.data.frame(results$District)
for (i in VACCINES) {
  X <- paste("Coverage_GEO_",i, sep="")
  coverage_data[i] <- results[X]
}

# Preparing CI data to be exported
CI_data <- as.data.frame(results$District)
for (i in VACCINES) {
  X <- paste("CI_GEO_",i, sep="")
  CI_data[i] <- results[X]
}

# Preparing CI_E data to be exported
CI_E_data <- as.data.frame(results$District)
for (i in VACCINES) {
  X <- paste("CI_E_GEO_",i, sep="")
  CI_E_data[i] <- results[X]
}

# Preparing CI Wealth data to be exported
CI_Wealth_data <- as.data.frame(results$District)
for (i in VACCINES) {
  X <- paste("CI_Wealth_Results_GEO_",i, sep="")
  CI_Wealth_data[i] <- results[X]
}

# Preparing CI_E Wealth data to be exported
CI_E_Wealth_data <- as.data.frame(results$District)
for (i in VACCINES) {
  X <- paste("CI_E_Wealth_Results_GEO_",i, sep="")
  CI_E_Wealth_data[i] <- results[X]
}



write.csv(coverage_data, file = paste0(COUNTRY, "_", YEAR, "_SUBNAT_DATA_coverage.csv"))
write.csv(CI_data, file = paste0(COUNTRY, "_", YEAR, "_SUBNAT_DATA_CI.csv"))
write.csv(CI_E_data, file = paste0(COUNTRY, "_", YEAR, "_SUBNAT_DATA_CI_E.csv"))
write.csv(CI_Wealth_data, file = paste0(COUNTRY, "_", YEAR, "_SUBNAT_DATA_CI_Wealth.csv"))
write.csv(CI_E_Wealth_data, file = paste0(COUNTRY, "_", YEAR, "_SUBNAT_DATA_CI_E_Wealth.csv"))



### END OF PROGRAM ### All CSV files and figures are in your working directory ###
