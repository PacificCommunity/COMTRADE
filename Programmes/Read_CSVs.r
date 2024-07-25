##
##    Programme:  Read_CSVs.r
##
##    Objective:  
##
##
##    Author:    James Hogan
##

   rm(list=ls(all=TRUE))
   
   ##
   ##    Find the spreadsheets
   ##      
      Base_Path <- paste0(getwd(), "/Data_Raw")
   
      X <- as.data.frame(system(paste0('cmd.exe /c dir /b "', Base_Path, '" /s'), intern = T), stringsAsFactors = FALSE)
      names(X) = "Details"
      Directories <- as.data.frame(X[str_detect(X$Details, "\\."),], stringsAsFactors = FALSE)
      names(Directories) = "Details"
      Directories$Biff <- str_replace_all(Directories$Details, paste0(str_replace_all(getwd(), "\\/", "\\\\\\\\"), "\\\\Data_Raw\\\\"), "")
   ##
   ##    Clean up the spreadsheet name as its save name
   ##
   Directories$Save_Name <- str_split_fixed(Directories$Biff, "\\.", n = 2)[,1]
   Directories$Save_Name <- str_replace_all(Directories$Save_Name, " ","_")
   ##
   ##    Keep only the spreadsheets
   ##
   Directories <- Directories[str_detect(Directories$Biff, ".csv"),]
  
   for(File in 1:nrow(Directories))
   {
    tic(paste("Reading CSV", Directories$Details[File]))
    assign(paste0("RAWDATA_", "XX", Directories$Save_Name[File]),  read.csv(Directories$Details[File], colClasses = "character"))
    save(list = paste0("RAWDATA_", "XX", Directories$Save_Name[File]), 
         file = paste0("Data_Intermediate/RAWDATA_", "XX", Directories$Save_Name[File], ".rda"))
    rm(list=paste0("RAWDATA_", "XX", Directories$Save_Name[File]))
    toc()

   }
##
##    And we're done
##
   