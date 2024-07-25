##
##    Programme:  Read_Spreadsheets.r
##
##    Objective:  This programme reads in all the health sector financial
##                data.  I tried it with XLConnect, and its just rubbish,
##                so now trying it with RDCOMClient
##
##                RDCOMClient hints from here: https://www.stat.berkeley.edu/~nolan/stat133/Fall05/lectures/DCOM.html
##
##    Author:    James Hogan, NZIER, 4 April 2019
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
   
   ##
   ##    A little fix to make this run quicker over the inter
   ##
  Directories <- Directories[str_detect(Directories$Biff, ".xls"),]
#   Directories <- Directories[str_detect(Directories$Biff, "nhcc"),]
   
  
   ##
   ##    Initialise some RDCOMClient parameters
   ##
      ex <- COMCreate("Excel.Application")
      ex[["Visible"]] <- TRUE
      ex[["DisplayAlerts"]] <- FALSE
      ex[["AskToUpdateLinks"]] <- FALSE
      Workbook <- ex[["workbooks"]]
      
   ##
   ##    Grab all the sheet names
   ##
     DidntRead <- data.frame(File = character(),
                              Tab = character())
                              
     for(File in 1:nrow(Directories))
      {
       tic(paste("Reading Worksheet", Directories$Details[File]))
       current_file <- Workbook$Open(Directories$Details[File])
         for(j in 1:current_file$worksheets()$count())
         {
            Name <- current_file$worksheets(j)$name()
            ##
            ##  RDCOMClient keeps breaking, so we're going save each tab as CSV and read it in
            ##
             Current_Tab <- current_file$Worksheets(as.character(Name))
             if(Current_Tab[["ProtectionMode"]] == TRUE){Current_Tab[["ProtectionMode"]] <- FALSE}
#             Current_Tab$unprotect()
             unlink("Data_Intermediate/*.csv")
             result = tryCatch({ 
                                 ## Current_Tab$Range("A1:IV60000")$RemoveSubtotal()
                                 Current_Tab$SaveAs(FileName =  paste0(str_replace_all(getwd(), "\\/", "\\\\"), "\\Data_Intermediate\\test.csv"), FileFormat = 6)
                                 ##
                                 ##     Stick everything back in an appropriately named data frame and save
                                 ##
                                  X <- read.csv(paste0(str_replace_all(getwd(), "\\/", "\\\\\\\\"), "\\\\Data_Intermediate\\\\test.csv"), colClasses = c("character"))
                                  X[] <- lapply(X, as.character)
                                  assign(paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]), X )
                                  save(list = paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]), 
                                       file = paste0("Data_Intermediate/RAWDATA_", Name, "XX", Directories$Save_Name[File], ".rda"))
                                  rm(list=paste0("RAWDATA_", Name, "XX", Directories$Save_Name[File]))
                               }, warning = function(w) {
                               }, error = function(e) {
                                   DidntRead <- rbind.fill( DidntRead , data.frame(File = Directories$Details[File],
                                                                                    Tab = Name))
                               }, finally = {
                               })  
         }
         ##
         ##     Close the RDCOMClient spreadsheet
         ##
         current_file$Close()
      toc()
     }
  ex$quit()
  unlink("Data_Intermediate/test.csv")
##
##    And we're done
##
   