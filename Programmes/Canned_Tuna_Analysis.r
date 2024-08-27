##
##    Programme:  Canned_Tuna_Analysis.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/themes.r")
      load('Data_Output/New_Metadata_Annual.rda')
      load('Data_Output/New_Metadata_Monthly.rda')
   ##
   ##    Collect all of the individual canned tuna datasets
   ##   
         Contents <- as.data.frame(list.files(path = "Data_Raw/",  pattern = "*.rda"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         Contents <- Contents[str_detect(Contents$DataFrames, "160414"),]

         All_Data <- lapply(Contents$DataFrames, function(File){
                              load(paste0("Data_Raw/", File))  
                              X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                              return(X)})
         Canned_Tuna_Data <- do.call(rbind.fill, All_Data)
         Canned_Tuna_Data$Period <- as.Date(paste0(Canned_Tuna_Data$period,"01"),"%Y%m%d")
   ##
   ## Step 2: Merge data with the Metadata to bring through the conversion factors
   ##
      Comtrade_Fish_Data <- data.table(merge(Canned_Tuna_Data,
                                             New_Metadata_Monthly,
                                             by.x = c("Period", "reporter_code", "reporter_desc"),
                                             by.y = c("Period", "ReporterCode",  "ReporterDesc"),
                                             all.x = TRUE))
                                  
      Comtrade_Fish_Data$Domestic_Currency_FOBValue     <- Comtrade_Fish_Data$fobvalue      / Comtrade_Fish_Data$Export_Conversion_Factor
      Comtrade_Fish_Data$Domestic_Currency_CIFValue     <- Comtrade_Fish_Data$cifvalue      / Comtrade_Fish_Data$Export_Conversion_Factor
      Comtrade_Fish_Data$Domestic_Currency_PrimaryValue <- Comtrade_Fish_Data$primary_value / Comtrade_Fish_Data$Export_Conversion_Factor
      
      Comtrade_Fish_Data$Cleaned_Measure <- ifelse(str_detect(Comtrade_Fish_Data$flow_desc, "Export"), "Export", "Import")

      Comtrade_Fish_Data <- Comtrade_Fish_Data[!is.na(Comtrade_Fish_Data$Period),]
      
      
   ##
   ##    Lets just work on the volume exports and imports
   ##
      Import_Export <- Comtrade_Fish_Data[,
                                         list(Total_Gross_Wgt = sum(gross_wgt,na.rm = TRUE),
                                              Total_Net_Wgt   = sum(net_wgt,na.rm = TRUE)),
                                          by = .(cmd_code, 
                                                 Year = year(Period), 
                                                 reporter_desc, 
                                                 partner_desc,
                                                 Cleaned_Measure)]

      Check <- Import_Export[Year == 2022,
                             list(Total_Net_Wgt   = sum(Total_Net_Wgt,na.rm = TRUE)),
                              by = .(Cleaned_Measure, 
                                     reporter_desc, 
                                     partner_desc)]
      write.csv(Check, file = "Exploratory_Output/Import_Exports_Tinned_Tuna_2000.csv", row.names = FALSE)
      
      
   ##
   ## Save files our produce some final output of something
   ##

   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
