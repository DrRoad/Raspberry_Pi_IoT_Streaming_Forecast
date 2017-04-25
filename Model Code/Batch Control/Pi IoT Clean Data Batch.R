# Set packages and functions
options(warnings = -1)  
options(scipen = 999)
options(stringsAsFactors = F)

# Select Packages to Load
pkgs <- c( "RODBC", "forecast","xgboost", "forecastxgb", "parallel",
           "Matrix", "plyr", "lubridate", "stringr", "tidyr", "dplyr")

# Load Libraries and Source Codes
sapply(pkgs, library, character.only=T )

# Set Paths 
Main_Path <- "C:/Users/dan.tetrick/Documents/Slalom-Analytics-Team/PI Temp Streaming/"
Input_Path <- paste0(Main_Path,"Input Data/")
Results_Path <- paste0(Main_Path,"Results/")
Model_Code_Path <- paste0(Main_Path, "Model Code/")
Graphics_Path <- paste0(Main_Path,"Graphics/")
SQLConn_Path <- "~/SQL Server Management Studio/Login Strings/"
Utility_Path <- paste0(Model_Code_Path,"Utility Functions/")

# Input Data from SQL
sqlConnString <-  "driver={SQL Server};server=cisdemosqlserver.database.windows.net;database=IoT_DB;Uid=dant;Pwd=SlalomDS!1;"
# myconn <- odbcDriverConnect(sqlConnString)

#Source Base Functions from R Library
Utility_Functions <- list.files(path = paste0(Model_Code_Path,"Utility Functions/"),
                                pattern = ".R",
                                full.names = T)
sapply(Utility_Functions,source)  

####################################################################################################
# Load .csv Data
#####################################################################################################
# Inputs <- list.files(path = Input_Path, pattern = ".csv", full.names = T)

# df <- lapply(Inputs, function(x) {x <- read_csv(x) %>%
#                                   setNames(c("TEMP","TIME"))}) %>%
#                                   do.call(rbind,.) %>% 
#                                   arrange(TIME)

df <- Fake_IoT_Data("2017-04-01 00:00:00", "2017-04-07 23:59:30", mean = 50, sd = 1.5)

####################################################
# Find Key Times
####################################################

Min_Date <- min(df$TIME)
Max_Date <- max(df$TIME)

Time_df <- data.frame(TIME = seq.POSIXt(Max_Date + 30, length.out = 60*24, by = "min")) %>% 
           mutate(TEMP = 0,
                  YEAR = year(TIME),
                  MONTH = month(TIME,label = T),
                  DAY = day(TIME),
                  WDAY = wday(TIME,label = T), 
                  HOUR = hour(TIME),
                  MINUTE = minute(TIME)) 

##################################################################################################
# Forecasting Data
##################################################################################################

df <- df %>% 
      mutate(TEMP = as.numeric(TEMP),
             YEAR = year(TIME),
             MONTH = month(TIME),
             DAY = day(TIME),
             WDAY = wday(TIME), 
             HOUR = hour(TIME),
             MINUTE = minute(TIME))  %>%  
      group_by(YEAR, MONTH, DAY, WDAY, HOUR, MINUTE) %>%
      summarize(TEMP = mean(TEMP))  %>%
      ungroup() %>% 
      mutate(TIME = as.POSIXct(paste0(YEAR,"-",MONTH,"-", DAY," ", HOUR,":", MINUTE))) %>% 
      select(TIME, TEMP, YEAR:MINUTE)

# Split Data and create time series objects
split <- split(df$TEMP, df$HOUR)

ts_split <- lapply(split,
                   function(x) ts(x, start = c(1, 1)
                                   , end = c(60, 7)
                                   , frequency = 7))

# Plot Time series objects 
# i = 1
# for(i in 1:length(split)){
# 
#   varNames <- names(split)
#   Summaries <- VariableSummary(split[[i]]
#                                ,decimal = 2
#                                ,name = paste0(varNames[i], "Hour Temp")
#                                ,graph = T
#                                ,pdf =TRUE
#                                ,path = Graphics_Path)
#                           }

####################################################
# Run XGBoost Forecast on all splits  
####################################################

XGBts_model <- lapply(ts_split, function(x) { xgbts(x)})

forecasts <- lapply(XGBts_model, function(x) forecast(x, h = 60))

Time_df$TEMP <- unlist(tbl_df(sapply(forecasts, function(x) x <- as.numeric(x$mean))) %>% 
                gather(HOUR, TEMP, 1:24) %>% select(TEMP))



####################################################
# Bind all data
####################################################

df_all <- tbl_df(bind_rows(df %>% select(TIME, TEMP), tbl_df(Time_df)) %>%
       select(TIME, TEMP) %>%
       mutate(TYPE = ifelse(TIME >= Max_Date,1,0),
              TIME = as.character(TIME))
)

write.csv(df_all, file = paste0(Results_Path,"Pi IoT Hourly Temp Historical and Predictions.csv"), row.names = F)

Time_df$TIME <- as.character(Time_df$TIME)
write.csv(Time_df, file = paste0(Results_Path,"Pi IoT Hourly Temp Predictions.csv"), row.names = F)

df$TIME <- as.character(df$TIME)
write.csv(df, file = paste0(Results_Path,"Pi IoT Hourly Temp Historical.csv"), row.names = F)
####################################################
# Load all data
####################################################

# Set file paths to data to import into SQL Server
LoadData1 <- file.path(Results_Path, "Pi IoT Hourly Temp Historical and Predictions.csv")
LoadData2 <- file.path(Results_Path, "Pi IoT Hourly Temp Historical.csv")
LoadData3 <- file.path(Results_Path, "Pi IoT Hourly Temp Predictions.csv")

# Create table names for data to be loaded into SQL as
sqlLoadTable1 <- "Fact_Pi_IoT_Hourly_Temp_Historical_and_Predictions"
sqlLoadTable2 <- "Fact_Pi_IoT_Hourly_Temp_Historical"
sqlLoadTable3<- "Fact_Pi_IoT_Hourly_Temp_Predictions"

# Set SQL Row Count
sqlRowsPerRead <- 200000

##########################################################
# Using a SQL Server Data Source and Compute Context
##########################################################  

# Creating an RxSqlServerData Data Source
sqlPiIoTDS <- RxSqlServerData(connectionString = sqlConnString,
                                      table = sqlLoadTable1,
                                      rowsPerRead = sqlRowsPerRead)

sqlPiIoTHistDS <- RxSqlServerData(connectionString = sqlConnString,
                              table = sqlLoadTable2,
                              rowsPerRead = sqlRowsPerRead)

sqlPiIoTPredDS <- RxSqlServerData(connectionString = sqlConnString,
                              table = sqlLoadTable3,
                              rowsPerRead = sqlRowsPerRead)

###############################################################
# Using rxDataStep to Load the Sample Data into SQL Server
###############################################################

# Load Predictions and Historical IoT Data
inTextData <- RxTextData(file = LoadData1, 
                         colClasses = c("TIME" = "character",
                                        "TEMP" = "numeric",
                                        "TYPE" = "integer"))

rxDataStep(inData = inTextData, outFile = sqlPiIoTDS, overwrite = TRUE)

# Load Historical IoT Data
inTextData <- RxTextData(file = LoadData2, 
                         colClasses = c("TIME" = "character",
                                        "TEMP" = "numeric"))

rxDataStep(inData = inTextData, outFile = sqlPiIoTHistDS, overwrite = TRUE)

# Load Predictions  IoT Data
inTextData <- RxTextData(file = LoadData3, 
                         colClasses = c("TIME" = "character",
                                        "TEMP" = "numeric"))

rxDataStep(inData = inTextData, outFile = sqlPiIoTPredDS, overwrite = TRUE)

# Extracting basic information about your data
rxGetInfo(data = sqlPiIoTDS, numRows = 5, getVarInfo =T)
rxGetInfo(data = sqlPiIoTHistDS, numRows = 5, getVarInfo =T)
rxGetInfo(data = sqlPiIoTPredDS, numRows = 5, getVarInfo =T)



