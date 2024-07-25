##
##    Programme:  Data_Hub_Explore.r
##
##    Objective:  Can we estimate a production function using Island Data?
##
##    Author:     James Hogan, SPC, 23 July 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load data from somewhere
   ##
      load("Data_Intermediate/RAWDATA_XXSPC,DF_NATIONAL_ACCOUNTS,1.rda")
      Island_GDP <- `RAWDATA_XXSPC,DF_NATIONAL_ACCOUNTS,1`
      unique(Island_GDP$Indicator)

      
                     
   ##
   ## Step 1: xxxxxxxxxxx
   ##


   ##
   ## Step 2: xxxxxxxxxxx
   ##
   
   
   ##
   ## Step 3: xxxxxxxxxxx
   ##



   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
