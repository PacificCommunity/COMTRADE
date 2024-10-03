##
##    Programme:  Clean_FFA_Data.r
##
##                FFA data has been pulled into R. Now clean it to make it usable.
##
##                ... of course the data is in a completely unusable excel form... 
##
##    Author:     James Hogan, FAME, 25 July 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/",  pattern = "*.rda"))
      names(Contents) = "DataFrames"
      
      Contents$Tab         <- str_trim(str_split_fixed(str_split_fixed(Contents$DataFrames, "\\.", n = 3)[,2], "XX",2)[,1])
      Contents$Spreadsheet <- str_trim(str_split_fixed(str_split_fixed(Contents$DataFrames, "\\.", n = 3)[,2], "XX",2)[,2])
      Contents$Year        <- as.numeric(str_sub(Contents$Spreadsheet, start=-4))
         
      Contents <- Contents[str_detect(Contents$DataFrames, "RAWDATA"),]
      Contents <- Contents[!is.na(Contents$Year),]
      Contents <- Contents[!(Contents$Tab %in% c("Contents","Introduction","Prices", "Intoduction")),]
      Contents <- Contents[!(Contents$Spreadsheet %in% c("FFA_Compendium_of_Economic_and_Development_Statistics_2022")),]
         
   ##
   ## Summary data has a different structure than non-summary data
   ##
         FFASummaryData <- lapply(unique(Contents$Tab[str_detect(Contents$Tab, "Summary")]), function(Dset)
                           {
                              DataSet <- Contents[Contents$Tab == Dset,]
                              
                              Data <- lapply(1:nrow(DataSet), function(File)
                                             {
                                                load(paste0("Data_Intermediate/", DataSet$DataFrames[File]))  
                                                X <- get(str_replace_all(DataSet$DataFrames[File], "\\.rda", ""))
                                                
                                                rm(list=c(as.character(str_replace_all(DataSet$DataFrames[File], "\\.rda", "") )))
                                                
                                                names(X) <- c("Measure", X[4,2:length(X)])
                                                X$Data_Row <- ifelse(str_detect(toupper(X$Measure), "CATCH"), toupper(str_trim(X$Measure)), "")
                                                for(i in 2:nrow(X))
                                                {
                                                   X$Data_Row[i] <- ifelse(((X$Data_Row[i] == "") & (X$Data_Row[(i-1)] != "")), X$Data_Row[(i-1)], X$Data_Row[i])
                                                }
                                                
                                                X <- reshape2::melt(X,
                                                                    id.vars = c("Data_Row", "Measure"),
                                                                    variable.name = "Year")
                                                X <- X[X$value != X$Year,]                    
                                                X$value <- as.numeric(str_replace_all(X$value, "\\,",""))
                                                X <- X[!is.na(X$value),]
                                                X <- X[!str_detect(toupper(X$Measure), "TOTAL"),]
                                                X$Measure <- str_trim(X$Measure)
                                                X <- X[order(X$Data_Row, X$Measure, X$Year),]
                                                X$Year <- as.numeric(as.character(X$Year))
                                                
                                                X$Spreadsheet <- unique(DataSet$Spreadsheet[File])
                                                X$Tab         <- unique(DataSet$Tab[File])
                                                
                                                return(unique(X))
                                             })
                              return(do.call(rbind, Data))
                           })
         names(FFASummaryData) <- unique(Contents$Tab[str_detect(Contents$Tab, "Summary")])
   ##
   ## Non-Summary data has quite a different structure than summary data. There's a fish specicies across the top
   ##
         FFANonSummaryData <- lapply(unique(Contents$Tab[!str_detect(Contents$Tab, "Summary")]), function(Dset)
                           {
                              DataSet <- Contents[Contents$Tab == Dset,]
                              
                              Data <- lapply(1:nrow(DataSet), function(File)
                                             {
                                                load(paste0("Data_Intermediate/", DataSet$DataFrames[File]))  
                                                X <- get(str_replace_all(DataSet$DataFrames[File], "\\.rda", ""))
                                                
                                                rm(list=c(as.character(str_replace_all(DataSet$DataFrames[File], "\\.rda", "") )))
                                                
                                                ##
                                                ##    Make a columns mapping file
                                                ##
                                                Column_Map <- data.frame(Columns = as.character(names(X)),
                                                                         Species = as.character(X[2,]),
                                                                         Year    = as.character(X[3,]))
                                                for(i in 2:nrow(Column_Map))
                                                {
                                                   Column_Map$Species[i] <- ifelse(((Column_Map$Species[i] == "") & (Column_Map$Species[(i-1)] != "")), Column_Map$Species[(i-1)], Column_Map$Species[i])
                                                }
                                                Column_Map$Columns[1] <- "Measure"
                                                names(X)[1] <- "Measure"
                                                
                                                ##
                                                ##    Now work on the data itselfs
                                                ##
                                                X$Data_Row <- ifelse(str_detect(toupper(X$Measure), "\\."), toupper(X$Measure), "")
                                                for(i in 2:nrow(X))
                                                {
                                                   X$Data_Row[i] <- ifelse(((X$Data_Row[i] == "") & (X$Data_Row[(i-1)] != "")), X$Data_Row[(i-1)], X$Data_Row[i])
                                                }
                                                
                                                X <- reshape2::melt(X,
                                                                    id.vars = c("Data_Row", "Measure"),
                                                                    variable.name = "Columns")
                                                ##
                                                ##    Bring the data and the columns together
                                                ##
                                                X <- merge(X,
                                                           Column_Map,
                                                           by = c("Columns"))
                                                X$value <- as.numeric(str_replace_all(X$value, "\\,",""))
                                                
                                                X <- X[!is.na(X$value),]
                                                X <- X[X$Measure != "",]
                                                X <- X[!str_detect(toupper(X$Measure), "TOTAL"),]
                                                X <- X[!str_detect(toupper(X$Species), "TOTAL"),]
                                                
                                                X$Measure <- str_trim(X$Measure)
                                                X <- X[order(X$Data_Row, X$Species, X$Measure, X$Year),]
                                                
                                                X$Year        <- as.numeric(as.character(X$Year))
                                                X$Spreadsheet <- unique(DataSet$Spreadsheet[File])
                                                X$Tab         <- unique(DataSet$Tab[File])
                                                
                                                return(unique(X[,c("Spreadsheet","Tab","Species", "Data_Row","Measure","Year","value")]))
                                             })
                              return(do.call(rbind, Data))
                           })
         names(FFANonSummaryData) <- unique(Contents$Tab[!str_detect(Contents$Tab, "Summary")])

   ##
   ##    Test for revisions
   ##
      Revisions <- lapply(names(FFANonSummaryData), function(Focus)
                           {
                              Test <- reshape2::dcast(FFANonSummaryData[[Focus]],
                                                      Data_Row + Measure + Species + Year ~ Spreadsheet,
                                                      value.var = "value")
                              for(i in 5:length(Test)) Test[,i][is.na(Test[,i])] <- 0
                              Revisions <- data.frame(Focus = as.character(),
                                                      Data_Row = as.character(),
                                                      Measure  = as.character(),
                                                      Species  = as.character(),
                                                      Year     = as.numeric(),
                                                      Source1  = as.character(),
                                                      Source2  = as.character(),
                                                      Value1   = as.numeric(),
                                                      Value2   = as.numeric())
                              for(j in 1:nrow(Test))
                              {
                                 for(i in 5:(length(Test)-1))
                                 {
                                    if((Test[j,i] != Test[j,(i+1)]) & (Test[j,i] != 0))
                                    {
                                       Revisions <- rbind(Revisions, data.frame(Focus = Focus,
                                                                                Data_Row = Test$Data_Row[j],
                                                                                Measure  = Test$Measure[j],
                                                                                Species  = Test$Species[j],
                                                                                Year     = Test$Year[j],
                                                                                Source1  = names(Test)[i],
                                                                                Source2  = names(Test)[(i+1)],
                                                                                Value1   = Test[j,i],
                                                                                Value2   = Test[j,(i+1)]))
                                    }
                                 }
                              }
                              return(Revisions)
                           })
      FFANonSummaryData_Revisions <- do.call(rbind, Revisions)
      FFANonSummaryData_Revisions$Difference <- FFANonSummaryData_Revisions$Value2 - FFANonSummaryData_Revisions$Value1
      FFANonSummaryData_Revisions$Relative_Difference <- (FFANonSummaryData_Revisions$Value2 / FFANonSummaryData_Revisions$Value1)-1
      

      Revisions <- lapply(names(FFASummaryData), function(Focus)
                           {
                              Test <- reshape2::dcast(FFASummaryData[[Focus]],
                                                      Data_Row + Measure + Year ~ Spreadsheet,
                                                      value.var = "value")
                              for(i in 5:length(Test)) Test[,i][is.na(Test[,i])] <- 0
                              Revisions <- data.frame(Focus = as.character(),
                                                      Data_Row = as.character(),
                                                      Measure  = as.character(),
                                                      Year     = as.numeric(),
                                                      Source1  = as.character(),
                                                      Source2  = as.character(),
                                                      Value1   = as.numeric(),
                                                      Value2   = as.numeric())
                              for(j in 1:nrow(Test))
                              {
                                 for(i in 5:(length(Test)-1))
                                 {
                                    if((Test[j,i] != Test[j,(i+1)]) & (Test[j,i] != 0))
                                    {
                                       Revisions <- rbind(Revisions, data.frame(Focus = Focus,
                                                                                Data_Row = Test$Data_Row[j],
                                                                                Measure  = Test$Measure[j],
                                                                                Year     = Test$Year[j],
                                                                                Source1  = names(Test)[i],
                                                                                Source2  = names(Test)[(i+1)],
                                                                                Value1   = Test[j,i],
                                                                                Value2   = Test[j,(i+1)]))
                                    }
                                 }
                              }
                              return(Revisions)
                           })
      FFASummaryData_Revisions <- do.call(rbind, Revisions)
      FFASummaryData_Revisions$Difference <- FFASummaryData_Revisions$Value2 - FFASummaryData_Revisions$Value1
      FFASummaryData_Revisions$Relative_Difference <- (FFASummaryData_Revisions$Value2 / FFASummaryData_Revisions$Value1)-1

   ##
   ## Clean up the FFA_Compendium_of_Economic_and_Development_Statistics_2022
   ##
      load("Data_Intermediate/RAWDATA_C. Country level dataXXFFA_Compendium_of_Economic_and_Development_Statistics_2022.rda")  
      X <- `RAWDATA_C. Country level dataXXFFA_Compendium_of_Economic_and_Development_Statistics_2022`
      
      names(X)    <- X[2,]
      names(X)[1] <- "Metrics"
      X$Metrics <- str_trim(stri_enc_toascii(X$Metrics), side = "both")
      
      X$CountryMeasure <- ifelse((str_detect(X$Metrics, regex("Catch and catch values", ignore_case = TRUE)) | 
                                  str_detect(X$Metrics, regex("Economic contribution", ignore_case = TRUE))), X$Metrics, "")
                                  
      X$FirstHeading <- ifelse((str_detect(X$Metrics, regex("National", ignore_case = TRUE)) &
                               !(str_detect(X$Metrics, regex("Note", ignore_case = TRUE)))), X$Metrics, "")
                                
      X$SecondHeading <- ifelse((str_detect(X$Metrics, regex("Catch", ignore_case = TRUE))   | 
                                 str_detect(X$Metrics, regex("vessels", ignore_case = TRUE)) |  
                                 str_detect(X$Metrics, regex("Licence", ignore_case = TRUE)) |  
                                 str_detect(X$Metrics, regex("processing", ignore_case = TRUE)) |  
                                 str_detect(X$Metrics, regex("Employment", ignore_case = TRUE)) |  
                                 str_detect(X$Metrics, regex("Exports", ignore_case = TRUE))) &
                               !(str_detect(X$Metrics, regex("Note", ignore_case = TRUE))), X$Metrics, "")
      for(i in 2:nrow(X))
      {
         X$CountryMeasure[i] <- str_trim(ifelse((X$CountryMeasure[i] == "") & (X$CountryMeasure[(i-1)] != ""), X$CountryMeasure[(i-1)], X$CountryMeasure[i]), side = "both")
         X$FirstHeading[i]   <- str_trim(ifelse((X$FirstHeading[i]   == "") & (X$FirstHeading[(i-1)]   != ""), X$FirstHeading[(i-1)],   X$FirstHeading[i]), side = "both")
         X$SecondHeading[i]  <- str_trim(ifelse((X$SecondHeading[i]  == "") & (X$SecondHeading[(i-1)]  != ""), X$SecondHeading[(i-1)],  X$SecondHeading[i]), side = "both")
         X$Units[i]          <- str_trim(ifelse((X$Units[i] == "") & (X$Units[(i-1)] != ""), X$Units[(i-1)], X$Units[i]), side = "both")
      }
      X$CountryMeasure <- str_trim(str_replace_all(X$CountryMeasure,"\032", "-"), side = "both")
      X$Country        <- str_trim(str_split_fixed(str_split_fixed(X$CountryMeasure,"-", 2)[,1], " ",2)[,2], side = "both")
      X$CountryMeasure <- str_trim(str_split_fixed(X$CountryMeasure,"-", 2)[,2], side = "both")
      X <- data.table(X)
      X <- data.table::melt(X,
                            id.vars =c("Country", "CountryMeasure","FirstHeading","SecondHeading", "Metrics","Units"),
                            variable.name = "Year")
      X$Value <- as.numeric(str_replace_all(X$value, "\\,", ""))
      
      X <- X[!is.na(X$Value),]
      X <- X[X$Metrics != "",]
      X$KillMe <- (X$Metrics == X$SecondHeading)
      X$KillMeNext <- !(str_detect(X$Metrics, regex("Licence", ignore_case = TRUE)) |  
                        str_detect(X$Metrics, regex("processing", ignore_case = TRUE)) |  
                        str_detect(X$Metrics, regex("Employment", ignore_case = TRUE)))
                                                 
      X <- data.frame(X[(X$KillMe != X$KillMeNext),])
      X$Year <- as.numeric(as.character(X$Year))
      X <- X[, c("Country", "CountryMeasure", "Year", "FirstHeading", "SecondHeading", "Metrics", "Units", "Value")]
      ##
      ##    Clean up the country names and other bits
      ##
         X$FirstHeading <- ifelse(X$FirstHeading == "National fleeta", "National fleet",X$FirstHeading)

         X$SecondHeading <- ifelse(str_detect(X$SecondHeading, regex("Exports", ignore_case = TRUE)), "Exports",
                            ifelse(str_detect(X$SecondHeading, regex("Employment", ignore_case = TRUE)), "Employment",
                            ifelse(str_detect(X$SecondHeading, regex("Licence", ignore_case = TRUE)), "Licence and access fee revenue",
                            ifelse(str_detect(X$SecondHeading, regex("Onshore", ignore_case = TRUE)), "Onshore processing volumes",X$SecondHeading))))

         X$SecondHeading <- ifelse(str_detect(X$SecondHeading, regex("Exports", ignore_case = TRUE)), "Exports",
                            ifelse(str_detect(X$SecondHeading, regex("Employment", ignore_case = TRUE)), "Employment",
                            ifelse(str_detect(X$SecondHeading, regex("Licence", ignore_case = TRUE)), "Licence and access fee revenue",
                            ifelse(str_detect(X$SecondHeading, regex("Onshore", ignore_case = TRUE)), "Onshore processing volumes",X$SecondHeading))))

         X$Metrics <- ifelse(str_detect(X$Metrics, regex("Thailand", ignore_case = TRUE)), "Thailand",
                      ifelse(str_detect(X$Metrics, regex("US", ignore_case = TRUE)),       "US",
                      ifelse(str_detect(X$Metrics, regex("Japan", ignore_case = TRUE)),    "Japan",
                      ifelse(str_detect(X$Metrics, regex("Troll", ignore_case = TRUE)),    "Troll",
                      ifelse(str_detect(X$Metrics, regex("EU", ignore_case = TRUE)),       "EU",
                      ifelse(str_detect(X$Metrics, regex("Licence", ignore_case = TRUE)),  "Licence and access fee revenue",
                      ifelse(str_detect(X$Metrics, regex("Employment", ignore_case = TRUE)), "Employment",X$Metrics)))))))
         FFA_Compendium_of_Economic_and_Development_Statistics_2022 <- data.table(X[order(X$Country, X$CountryMeasure, X$FirstHeading, X$SecondHeading, X$Metrics, X$Units, X$Year),])

   ##
   ## Save files
   ##
      save(FFASummaryData,             file = 'Data_Output/FFASummaryData.rda')
      save(FFANonSummaryData,          file = 'Data_Output/FFANonSummaryData.rda')
      
      save(FFANonSummaryData_Revisions,file = 'Data_Output/FFANonSummaryData_Revisions.rda')
      save(FFASummaryData_Revisions,   file = 'Data_Output/FFASummaryData_Revisions.rda')
      
      save(FFA_Compendium_of_Economic_and_Development_Statistics_2022,   file = 'Data_Intermediate/FFA_Compendium_of_Economic_and_Development_Statistics_2022.rda')
##
##    And we're done
##
