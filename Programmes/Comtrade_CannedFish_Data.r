##
##    Programme:  Comtrade_CannedFish_Data.rv
##
##    Objective:  So, it turns out that it looks like the opening of the pacific in the late 1970s flooded the tinned 
##                fish market and could have depressed prices which made the Atlantic unprofitable.
##                Lets test this theory pulling canned tuna volumes.
##
##                I'm exploiting the work of Taro Kawamoto in his paper, "A challenge to estimate global canned tuna 
##                demand and its impact on future tuna resource management using the gamma model"
##                https://doi.org/10.1016/j.marpol.2022.105016
##
##                FAO ISSCFC* code	Description
##                037.1.1.5.6.910	- Tunas prepared or preserved, not minced, in oil
##                037.1.1.5.6.101	- Bonito (Sarda spp.), prepared or preserved, not minced, in oil
##                037.1.1.5.6.109	- Bonito (Sarda spp.), not minced, prepared, or preserved, nei
##                037.1.1.5.6.201	- Skipjack, prepared or preserved, whole or in pieces, not minced, in oil
##                037.1.1.5.6.209	- Skipjack prepared or preserved, not minced, nei
##                037.1.1.5.6.401	- Albacore (=Longfin tuna), prepared or preserved, not minced, in oil
##                037.1.1.5.6.409	- Albacore (=Longfin tuna), prep. or pres., not minced, nei
##                037.1.1.5.6.911	- Tunas prepared or preserved, not minced, nei
##                037.1.1.5.6.903	- Tunas prepared or preserved, not minced, in airtight containers
##                037.1.1.5.6.905	- Tunas prepared or preserved, not minced, not in airtight containers
##                037.1.1.5.6.909	- Tunas, flakes and grated, prepared or preserved##
##
##    Author:     James Hogan, FAME - The Pacific Community (SPC), 17 July 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      load('Data_Output/New_Metadata_Annual.rda')
      load('Data_Output/New_Metadata_Monthly.rda')
                     
   ##
   ## Step 1: Grab the tinned tuna codes
   ##
   
      All_Codes <- data.table(Description = ct_commodity_lookup(c("160414"),return_char = TRUE))
      All_Codes$Code <- str_split_fixed(All_Codes$Description ," ", 2)[,1]
      
 
   ##
   ## Step 2: Who's already been processed?
   ##
      Contents <- as.data.frame(list.files(path = "Data_Raw/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
      Contents <- Contents[str_detect(Contents$DataFrames, "COMTRADEXX"),]

      Contents$Fish_Code <- str_split_fixed(Contents$Dframe, "XX",3)[,2]
      Contents$Year      <- str_split_fixed(Contents$Dframe, "XX",3)[,3]
 
   ##
   ## Step 3: Cycle through the codes and the months
   ##
      Fish_Data <- lapply(unique(All_Codes$Code), function(Fish_Code)
                          {
                              Fish <- lapply(year(Sys.time()):1962, function(Year)
                                             {
                                                if(nrow(Contents[(Contents$Fish_Code == Fish_Code) &
                                                                 (Contents$Year      == Year),]) == 0)
                                                {
                                                   Get_Data <- ct_get_data( reporter = 'all_countries',
                                                                            partner  = 'all_countries',
                                                                            commodity_code = Fish_Code,
                                                                            start_date = Year,
                                                                            end_date   = Year,
                                                                            flow_direction = 'everything',
                                                                            freq = "M")
                                                                            
                                                   assign(paste0("COMTRADEXX", Fish_Code, "XX", Year), Get_Data)
                                                   save(list = paste0("COMTRADEXX", Fish_Code, "XX", Year), 
                                                        file = paste0("Data_Raw/COMTRADEXX", Fish_Code, "XX", Year,".rda"))
                                                   rm(list=c(as.character(paste0("COMTRADEXX", Fish_Code, "XX", Year)) ))
                                                   print(Year)
                                                }
                                             })
                              return(NULL)   
                           })
                           

   ##
   ##    Collect all of the individual COMTRADE datasets
   ##   
         Contents <- as.data.frame(list.files(path = "Data_Raw/",  pattern = "*.rda"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         Contents <- Contents[str_detect(Contents$DataFrames, "COMTRADEXX"),]

         All_Data <- lapply(Contents$DataFrames, function(File){
                              load(paste0("Data_Raw/", File))  
                              X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                              return(X)})
         Fish_Data <- do.call(rbind.fill, All_Data)
         Fish_Data$Period <- as.Date(paste0(Fish_Data$period,"01"),"%Y%m%d")
   ##
   ## Step 2: Merge data with the Metadata to bring through the conversion factors
   ##
      Comtrade_Fish_Data <- merge(Fish_Data,
                                  New_Metadata_Monthly,
                                  by.x = c("Period", "reporter_code", "reporter_desc"),
                                  by.y = c("Period", "ReporterCode",  "ReporterDesc"),
                                  all.x = TRUE)
                                  
      Comtrade_Fish_Data$Domestic_Currency_FOBValue     <- Comtrade_Fish_Data$fobvalue      / Comtrade_Fish_Data$Export_Conversion_Factor
      Comtrade_Fish_Data$Domestic_Currency_CIFValue     <- Comtrade_Fish_Data$cifvalue      / Comtrade_Fish_Data$Export_Conversion_Factor
      Comtrade_Fish_Data$Domestic_Currency_PrimaryValue <- Comtrade_Fish_Data$primary_value / Comtrade_Fish_Data$Export_Conversion_Factor

      Comtrade_Fish_Data <- Comtrade_Fish_Data[!is.na(Comtrade_Fish_Data$Period),]
   ##
   ## Save files our produce some final output of something
   ##
      save(Comtrade_Fish_Data, file = "Data_Output/Comtrade_Fish_Data.rda")
##
##    And we're done
##

