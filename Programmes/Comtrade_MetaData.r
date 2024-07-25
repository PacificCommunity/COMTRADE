##
##    Programme:  Comtrade_MetaData.rv
##
##    Objective:  There's some fish hooks in the data.
##                The Comtrade data expresses the value of trade in US dollars. To do this, a conversation factor is applied to each country's
##                currency that expresses it in USD. So in order to disentangle genuine price changes from currency fluctuations, I've got to 
##                pull these import and export conversation factors, and remove their variance from the data so I can extract the correct price 
##                information.
##
##                1. The value of trade is expressed in US dollars, and deflated by a currency conversation factor
##                   which needs to be captured at the same time (https://uncomtrade.org/docs/conversion-factors-and-current-constant-value/).
##                   The help documentation below is not very helpful.. https://comtradedeveloper.un.org/api-details#api=comtrade-v1&operation=getmetadata
##
##                2. I've figured out how to access this information using old school curl, but now there's another problem: not all of the 
##                   time periods are present in the data - for example, New Zealand is missing a Import and Export conversation factor for 
##                   January 2023. If it is, then others will be. 
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
   ##    Pull the monthly metrics
   ##

      url <- "https://comtradeapi.un.org/data/v1/getMetadata/C/M/HS"
      h <- new_handle()
      handle_setopt(h, .list = list(customrequest = "GET",
                                    httpheader = paste0('Ocp-Apim-Subscription-Key: ',Sys.getenv('COMTRADE_PRIMARY'))))
      tmp <- tempfile()
      connection <- curl(url,handle = h)

      Content <- fromJSON(readLines(connection))

      Metadata <- lapply(1:length(Content[["data"]]), function(Count){
                        
                        Grr <- lapply(1:length(Content[["data"]][[Count]][["notes"]]), function(inner){
                                      return(data.frame(Period       = as.Date(paste0(Content[["data"]][[Count]][["notes"]][[inner]][["period"]], "01"), "%Y%m%d"),
                                                        ReporterCode = Content[["data"]][[Count]][["notes"]][[inner]][["reporterCode"]],
                                                        ReporterDesc = Content[["data"]][[Count]][["notes"]][[inner]][["reporterDescription"]],
                                                        Currency     = Content[["data"]][[Count]][["notes"]][[inner]][["currency"]],
                                                      
                                                        Import_Conversion_Factor = as.numeric(Content[["data"]][[Count]][["notes"]][[inner]][["importConvFactor"]]),
                                                        Export_Conversion_Factor = as.numeric(Content[["data"]][[Count]][["notes"]][[inner]][["exportConvFactor"]]),
                                                        publicationDateShort = as.Date(Content[["data"]][[Count]][["notes"]][[inner]][["publicationDateShort"]], "%Y-%m-%d")))
                               })
                        return(do.call(rbind, Grr))   
                        })

      Metadata <- do.call(rbind, Metadata)

   ##
   ##    Cycle through each of the countries (ReporterDesc), and make a template of their months and theirs years. Then fill the date template 
   ##       with the conversion factor data, carrying over perivous values where appropriate
   ##
      New_Metadata <- lapply(unique(Metadata$ReporterDesc), function(Country)
                            {
                              ##
                              ##    Create a time template
                              ##
                                 Country_Data <- Metadata[Metadata$ReporterDesc == Country,c("Period","ReporterCode","ReporterDesc","Import_Conversion_Factor","Export_Conversion_Factor", "publicationDateShort")]
                                 Time_Template <- reshape2::dcast(Country_Data,
                                                                 year(Country_Data$Period) ~ month(Country_Data$Period),
                                                                 value.var = c("ReporterCode"))
                                 names(Time_Template)[1] <- "Year"
                                 Time_Template <- reshape2::melt(Time_Template,
                                                                id.var = "Year")
                                 Time_Template <- unique(Time_Template[,1:2])
                                 Time_Template$Period <- as.Date(paste0(Time_Template$Year,"-",Time_Template$variable,"-01"),"%Y-%m-%d")
                                 
                              ##
                              ##    Left merge it back onto the original country data to identify data "holes"
                              ##
                                 Country_Data <- merge(Country_Data,
                                                       Time_Template,
                                                       by = c("Period"),
                                                       all.y = TRUE)
                                 if(nrow(Country_Data) > 1)
                                 {for(i in 2:nrow(Country_Data))
                                    {
                                       Country_Data$Import_Conversion_Factor[i] <- ifelse(is.na(Country_Data$Import_Conversion_Factor[i]) & !is.na(Country_Data$Import_Conversion_Factor[(i-1)]), Country_Data$Import_Conversion_Factor[(i-1)], Country_Data$Import_Conversion_Factor[i])
                                       Country_Data$Export_Conversion_Factor[i] <- ifelse(is.na(Country_Data$Export_Conversion_Factor[i]) & !is.na(Country_Data$Export_Conversion_Factor[(i-1)]), Country_Data$Export_Conversion_Factor[(i-1)], Country_Data$Export_Conversion_Factor[i])
                                       Country_Data$publicationDateShort[i]     <- ifelse(is.na(Country_Data$publicationDateShort[i])     & !is.na(Country_Data$publicationDateShort[(i-1)]),     Country_Data$publicationDateShort[(i-1)],     Country_Data$publicationDateShort[i])
                                    }
                                 }
                                 Country_Data$ReporterCode <- unique(Country_Data$ReporterCode)[!is.na(unique(Country_Data$ReporterCode))]
                                 Country_Data$ReporterDesc <- unique(Country_Data$ReporterDesc)[!is.na(unique(Country_Data$ReporterDesc))]
                                 
                              ##
                              ##    Duplicates... See Ireland
                              ##
                                 Keep_Me <- sqldf("Select distinct Period,
                                                                   publicationDateShort
                                                    from Country_Data
                                                    group by Period
                                                    having max(publicationDateShort)")
                                 Country_Data <- merge(Country_Data,
                                                       Keep_Me,
                                                       by = c("Period", "publicationDateShort"))
                                 Country_Data <- unique(Country_Data[,c("Period","ReporterCode","ReporterDesc","Import_Conversion_Factor","Export_Conversion_Factor")])
                                 
                              ##
                              ##    Done - send back to main 
                              ##
                                 
                              return(Country_Data)
                            })
      New_Metadata_Monthly <- do.call(rbind, New_Metadata)
      
      # New_Zealand <- Metadata[Metadata$ReporterDesc == "Ireland",]
      # New_Zealand <- New_Zealand[order(New_Zealand$Period),]
      # plot(New_Zealand$Period, New_Zealand$Import_Conversion_Factor, type = "l")

      # New_Zealand <- New_Metadata[New_Metadata$ReporterDesc == "New Zealand",]
      # New_Zealand <- New_Zealand[order(New_Zealand$Period),]
      # plot(New_Zealand$Period, New_Zealand$Import_Conversion_Factor, type = "l")

   ##
   ##    Now pull the annual metrics
   ##


      url <- "https://comtradeapi.un.org/data/v1/getMetadata/C/A/HS"
      h <- new_handle()
      handle_setopt(h, .list = list(customrequest = "GET",
                                    httpheader = paste0('Ocp-Apim-Subscription-Key: ',Sys.getenv('COMTRADE_PRIMARY'))))
      tmp <- tempfile()
      connection <- curl(url,handle = h)

      Content <- fromJSON(readLines(connection))

      Metadata <- lapply(1:length(Content[["data"]]), function(Count){
                        
                        Grr <- lapply(1:length(Content[["data"]][[Count]][["notes"]]), function(inner){
                                      return(data.frame(Period       = as.Date(paste0(Content[["data"]][[Count]][["notes"]][[inner]][["period"]], "0101"), "%Y%m%d"),
                                                        ReporterCode = Content[["data"]][[Count]][["notes"]][[inner]][["reporterCode"]],
                                                        ReporterDesc = Content[["data"]][[Count]][["notes"]][[inner]][["reporterDescription"]],
                                                        Currency     = Content[["data"]][[Count]][["notes"]][[inner]][["currency"]],
                                                      
                                                        Import_Conversion_Factor = as.numeric(Content[["data"]][[Count]][["notes"]][[inner]][["importConvFactor"]]),
                                                        Export_Conversion_Factor = as.numeric(Content[["data"]][[Count]][["notes"]][[inner]][["exportConvFactor"]]),
                                                        publicationDateShort = as.Date(Content[["data"]][[Count]][["notes"]][[inner]][["publicationDateShort"]], "%Y-%m-%d")))
                               })
                        return(do.call(rbind, Grr))   
                        })

#      Metadata <- data.table(do.call(rbind, Metadata))
      Metadata <- do.call(rbind, Metadata)

   ##
   ##    Cycle through each of the countries (ReporterDesc), and make a template of their months and theirs years. Then fill the date template 
   ##       with the conversion factor data, carrying over perivous values where appropriate
   ##
      New_Metadata_Annual <- lapply(unique(Metadata$ReporterDesc), function(Country)
                            {
                              #print(Country)
                              ##
                              ##    Create a time template
                              ##
                                 Country_Data <- Metadata[Metadata$ReporterDesc == Country,c("Period","ReporterCode","ReporterDesc","Import_Conversion_Factor","Export_Conversion_Factor", "publicationDateShort")]
                                 Time_Template <- reshape2::dcast(Country_Data,
                                                                 year(Country_Data$Period) ~ month(Country_Data$Period),
                                                                 value.var = c("ReporterCode"))
                                 names(Time_Template)[1] <- "Year"
                                 Time_Template <- reshape2::melt(Time_Template,
                                                                id.var = "Year")
                                 Time_Template <- unique(Time_Template[,1:2])
                                 Time_Template$Period <- as.Date(paste0(Time_Template$Year,"-",Time_Template$variable,"-01"),"%Y-%m-%d")
                                 
                              ##
                              ##    Left merge it back onto the original country data to identify data "holes"
                              ##
                                 Country_Data <- merge(Country_Data,
                                                       Time_Template,
                                                       by = c("Period"),
                                                       all.y = TRUE)
                                 if(nrow(Country_Data) > 1)
                                 {for(i in 2:nrow(Country_Data))
                                    {
                                       Country_Data$Import_Conversion_Factor[i] <- ifelse(is.na(Country_Data$Import_Conversion_Factor[i]) & !is.na(Country_Data$Import_Conversion_Factor[(i-1)]), Country_Data$Import_Conversion_Factor[(i-1)], Country_Data$Import_Conversion_Factor[i])
                                       Country_Data$Export_Conversion_Factor[i] <- ifelse(is.na(Country_Data$Export_Conversion_Factor[i]) & !is.na(Country_Data$Export_Conversion_Factor[(i-1)]), Country_Data$Export_Conversion_Factor[(i-1)], Country_Data$Export_Conversion_Factor[i])
                                       Country_Data$publicationDateShort[i]     <- ifelse(is.na(Country_Data$publicationDateShort[i])     & !is.na(Country_Data$publicationDateShort[(i-1)]),     Country_Data$publicationDateShort[(i-1)],     Country_Data$publicationDateShort[i])
                                    }
                                 }
                                 Country_Data$ReporterCode <- unique(Country_Data$ReporterCode)[!is.na(unique(Country_Data$ReporterCode))]
                                 Country_Data$ReporterDesc <- unique(Country_Data$ReporterDesc)[!is.na(unique(Country_Data$ReporterDesc))]
                                 
                              ##
                              ##    Duplicates... See Ireland
                              ##
                                 Keep_Me <- sqldf("Select distinct Period,
                                                                   publicationDateShort
                                                    from Country_Data
                                                    group by Period
                                                    having max(publicationDateShort)")
                                 Country_Data <- merge(Country_Data,
                                                       Keep_Me,
                                                       by = c("Period", "publicationDateShort"))
                                 Country_Data <- unique(Country_Data[,c("Period","ReporterCode","ReporterDesc","Import_Conversion_Factor","Export_Conversion_Factor")])
                                 
                              ##
                              ##    Done - send back to main 
                              ##
                                 
                              return(Country_Data)
                            })
      New_Metadata_Annual <- do.call(rbind, New_Metadata_Annual)


      # New_Zealand <- New_Metadata_Annual[New_Metadata_Annual$ReporterDesc == "New Zealand",]
      # New_Zealand <- New_Zealand[order(New_Zealand$Period),]
      # plot(New_Zealand$Period, New_Zealand$Import_Conversion_Factor, type = "l")


      # New_Zealand <- New_Metadata_Monthly[New_Metadata_Monthly$ReporterDesc == "New Zealand",]
      # New_Zealand <- New_Zealand[order(New_Zealand$Period),]
      # plot(New_Zealand$Period, New_Zealand$Import_Conversion_Factor, type = "l")





   ##
   ## Save files our produce some final output of something
   ##
      save(New_Metadata_Annual,  file = 'Data_Output/New_Metadata_Annual.rda')
      save(New_Metadata_Monthly, file = 'Data_Output/New_Metadata_Monthly.rda')
##
##    And we're done
##
