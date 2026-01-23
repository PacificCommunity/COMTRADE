##
##    Programme:  Canned_Tuna_Analysis.r
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
   ##    Collect all of the individual canned tuna datasets
   ##   
         Contents <- as.data.frame(list.files(path = "Data_Raw/",  pattern = "*.rda"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         Contents <- Contents[str_detect(Contents$DataFrames, "160414"),]     ##    This is the bit that identifies the tinned can code

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
      
      ##
      ##    Found some of the missing...
      ##
         X <- unique(Comtrade_Fish_Data$partner_desc)
         X[order(X)]
         X[order(X)][str_detect(X[order(X)], ', nes')]

         X <- unique(Comtrade_Fish_Data$reporter_desc)
         X[order(X)]
         X[order(X)][str_detect(X[order(X)], ', nes')]

      
      ##
      ##    Need to sort this out
      ##
      Comtrade_Fish_Data$Cleaned_Measure <- ifelse(Comtrade_Fish_Data$flow_desc %in% c("Export", "Re-export" ), "Export",
                                            ifelse(Comtrade_Fish_Data$flow_desc %in% c("Import", "Re-import" ), "Import", "OTHER"))
                                            
      Comtrade_Fish_Data <- Comtrade_Fish_Data[Cleaned_Measure != "OTHER"]
      Comtrade_Fish_Data <- Comtrade_Fish_Data[!is.na(Comtrade_Fish_Data$Period),]
      
#      data.frame(Comtrade_Fish_Data[(reporter_desc == "Thailand") & (year(Period) == 2023) & (Cleaned_Measure == "Import") & (partner_desc == "China")])
      
   ##
   ##    Lets just work on the volume exports and imports and see if we can reconcile trade flows
   ##
      Import_Export <- Comtrade_Fish_Data[,
                                         list(Total_Gross_Wgt = sum(gross_wgt,na.rm = TRUE),
                                              Total_Net_Wgt   = sum(net_wgt,na.rm = TRUE),
                                              Total_Primary_Value = sum(primary_value,na.rm = TRUE)
                                              ),
                                          by = .(cmd_code, 
                                                 Year = year(Period), 
                                                 reporter_desc, 
                                                 partner_desc,
                                                 Cleaned_Measure)]
      ##
      ##    if the partner_desc contains ", nes" then make another record which flips the 
      ##
         NES <- Import_Export[str_detect(partner_desc, ", nes")]
         NES$Cleaned_Measure <- ifelse(NES$Cleaned_Measure == "Import", "Export", "Import")
         NES$reporter_desc <- NES$partner_desc
         
      ##
      ## add it back to Import/Export
      ## 
         Import_Export <- rbind(Import_Export, NES)
                                                               
#      data.frame(Import_Export[(reporter_desc == "Thailand") & (Year == 2023) & (Cleaned_Measure == "Import")])
                                                 
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
      ##
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
                                    
      Canned_Tuna_Imports_and_Exports <- merge(Imports,
                                               Exports,
                                                by = c("Year", "reporter_desc", "variable"),
                                                all = TRUE)
      
      #data.frame(Canned_Tuna_Imports_and_Exports[(reporter_desc == "Thailand") & (Year == 2023)])
      ##
      ##    Ok, that's kind of interesting - save it and map it
      ##
         save(Canned_Tuna_Imports_and_Exports, file = "Data_Output/Canned_Tuna_Imports_and_Exports.rda")
         
##
##    And we're done
##
