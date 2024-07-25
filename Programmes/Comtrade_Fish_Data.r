##
##    Programme:  Comtrade_Fish_Data.rv
##
##    Objective:  There's some fish hooks in the data.
##                1. Comtrade will let you pull out monthly data, however the API limits each "monthly" query to a single year
##                   so we've got to cycle through the years separately.
##
##                2. The value of trade is expressed in US dollars, and deflated by a currency conversation factor
##                   which needs to be captured at the same time (https://uncomtrade.org/docs/conversion-factors-and-current-constant-value/).
##                   I'm still trying to figure out how to isolate this information. The help documentation below is not very helpful..
##                   https://comtradedeveloper.un.org/api-details#api=comtrade-v1&operation=getmetadata
##
##                3. The free API limits the volume of data to 500 calls/day, up to 100,000 records per call, when registered and using an API key.
##                   The code therefore will not get all of the data in a day. I've built it so it doesn't try to download everything in a day,
##                   and will happily bring down the data over a period of a number of days.
##
##                   There's 122 codes in All_Codes below. And 14 years between 2010 and 2024, making 1708 separate calls. Divided by 500 means
##                   getting this data off COMTRADE will take 3 and a half days.
##
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
   ## Step 1: Grab the fish codes
   ##
   
      All_Codes <- data.table(Description = ct_commodity_lookup(c("0302","0303"),return_char = TRUE))
      All_Codes$Code <- str_split_fixed(All_Codes$Description ," ", 2)[,1]
      All_Codes <- All_Codes[((as.numeric(All_Codes$Code) > 10000) & 
                              (as.numeric(All_Codes$Code) < 90000)),]
      
 
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
                              Fish <- lapply(year(Sys.time()):2010, function(Year)
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

