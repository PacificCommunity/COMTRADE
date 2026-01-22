##
##    Programme:  Canned_Tuna_Analysis - version 2.r
##
##    This bit has actually turned into something very interesting - I've been able to identify the international tuna value chain, from raw
##       tuna which is exported by harvesting countries to processing countries, who receive it as imports, and then from there, as exports
##       of tinned tuna from processing countries and imports of tinned tuna by consuming countries - see 
##       "S:\FAME\NC_NOU\FAME COMMON\FAME Economics\Oceanic_Fisheries\COMTRADE\Adhoc_Queries\Estiamated Tinned Tuna Volumes.xlsx"
##
##    I was able to do this in the quantities in Canned_Tuna_Analysis, Canned_Tuna_International_Trade_Map, Raw_Tuna_Analysis and Raw_Tuna_International_Trade_Map
##
##    For my next trick, I'll try in version two of the above programmes to do the same tracing trick, but this time with nominal values as well as quantities.
##       If I can, and it does work, then nominal value / quantity will give me average price. On the raw tuna into import into processing countries, this becomes their input 
##       costs. And for tinned tuna, this becomes their output costs. The difference is their manuafacturing margin, and is a nice proxy for value added.
##
##
##    Author:     James Hogan, FAME - SPC, 23 January 2026
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
   ## Step 1: Grab the fish codes
   ##
   
      All_Codes <- data.table(Description = ct_commodity_lookup(c("tuna"),return_char = TRUE))
      All_Codes$Code <- str_split_fixed(All_Codes$Description ," ", 2)[,1]
      All_Codes <- All_Codes[((as.numeric(All_Codes$Code) > 10000) & 
                              (as.numeric(All_Codes$Code) < 90000)),]

   ##
   ##    Collect all of the individual canned tuna datasets
   ##   
         Contents <- as.data.frame(list.files(path = "Data_Raw/",  pattern = "*.rda"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         Contents$Code   <- str_split_fixed(Contents$Dframe, "XX", n = 3)[,2]
         Contents <- Contents[Contents$Code %in% All_Codes$Code,]     ##    This is the bit that identifies the raw tuna

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
   ##    Lets just work on the volume exports and imports and see if we can reconcile trade flows
   ##
      Import_Export <- Comtrade_Fish_Data[,
                                         list(Total_Gross_Wgt = sum(gross_wgt/1000,na.rm = TRUE),
                                              Total_Net_Wgt   = sum(net_wgt/1000,na.rm = TRUE),
                                              Total_Primary_Value = sum(primary_value/1000,na.rm = TRUE)
                                              ),
                                          by = .(cmd_code, 
                                                 Year = year(Period), 
                                                 reporter_desc, 
                                                 partner_desc,
                                                 Cleaned_Measure)]
      AggCheck <- Import_Export[,
                                list(Total_Net_Wgt       = sum(Total_Net_Wgt,na.rm = TRUE),
                                     Total_Primary_Value = sum(Total_Primary_Value,na.rm = TRUE)),
                                 by = .(Cleaned_Measure, 
                                        Year)]
      AggCheck <- data.table::melt(AggCheck,
                                   id.var = c("Cleaned_Measure", "Year"))
                                   
      AggCheck <- data.table::dcast(AggCheck,
                                    Year + Cleaned_Measure ~ variable,
                                    value.var = "value")
      AggCheck <- AggCheck[order(AggCheck$Cleaned_Measure, AggCheck$Year),]
                                    
      AggCheck$Average_Value <- AggCheck$Total_Primary_Value / AggCheck$Total_Net_Wgt

      AggCheck <- data.table::melt(AggCheck,
                                   id.var = c("Cleaned_Measure", "Year"))
      AggCheck <- data.table::dcast(AggCheck,
                                    Year + variable ~ Cleaned_Measure,
                                    value.var = "value")
      AggCheck$Difference <- AggCheck$Export - AggCheck$Import
      AggCheck$Percent_Difference <- (AggCheck$Export / AggCheck$Import) - 1
      AggCheck <- AggCheck[order(AggCheck$variable, AggCheck$Year),]
      
      ##
      ##    Ok, quite out in quantities and values :( but weirdly sometimes avergae price show much less variance
      ##

      Check <- Import_Export[,
                             list(Total_Net_Wgt   = sum(Total_Net_Wgt,na.rm = TRUE),
                                  Total_Primary_Value = sum(Total_Primary_Value,na.rm = TRUE)),
                              by = .(Cleaned_Measure, 
                                     Year,
                                     reporter_desc, 
                                     partner_desc)]
      ##
      ##    How much of total exports and total imports are explained by the different reporter countries?
      ##
      Imports <- Import_Export[Cleaned_Measure == "Import",
                             list(Total_Net_Wgt   = sum(Total_Net_Wgt,na.rm = TRUE),
                                  Total_Primary_Value = sum(Total_Primary_Value,na.rm = TRUE)),
                              by = .(Cleaned_Measure, 
                                     Year,
                                     reporter_desc)]
                                     
      Exports <- Import_Export[Cleaned_Measure == "Export",
                             list(Total_Net_Wgt   = sum(Total_Net_Wgt,na.rm = TRUE),
                                  Total_Primary_Value = sum(Total_Primary_Value,na.rm = TRUE)),
                              by = .(Cleaned_Measure, 
                                     Year,
                                     reporter_desc)]
                                     
      Imports <- data.table::melt(Imports,
                                  id.var = c("Year", "reporter_desc", "Cleaned_Measure"))
                                     
      Imports <- data.table::dcast(Imports,
                                   Year + reporter_desc + variable ~ Cleaned_Measure,
                                    value.var = "value")
                                    
                                    
      Exports <- data.table::melt(Exports,
                                  id.var = c("Year", "reporter_desc", "Cleaned_Measure"))
                                     
      Exports <- data.table::dcast(Exports,
                                   Year + reporter_desc + variable ~ Cleaned_Measure,
                                    value.var = "value")
                                    
      Raw_Tuna_Imports_and_Exports <- merge(Imports,
                                            Exports,
                                            by = c("Year", "reporter_desc", "variable"),
                                            all = TRUE)
      
      data.frame(Raw_Tuna_Imports_and_Exports[Year == 2020])
      ##
      ##    Ok, that's kind of interesting - save it and map it
      ##
         save(Raw_Tuna_Imports_and_Exports, file = "Data_Output/Raw_Tuna_Imports_and_Exports.rda")
         
##
##    And we're done
##
