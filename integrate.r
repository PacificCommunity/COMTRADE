##    Programme:  Comtrade.r
##
##    Objective:  Comtrade is the UN source of trade data. I've verified it against
##                StatsNZ's trade and it matches up.
##
##                This project will access the UN data, and pull down teh fisheries related
##                trade data
##
##    Plan of  :  You need an api to access the data, which can be a little bit confusing to 
##    Attack   :  get. Follow this document: https://uncomtrade.org/docs/api-subscription-keys/
##
##
##    Important:  
##    Linkages :  
##
##    Author   :  James Hogan, FAME - The Pacific Community (SPC)
##
##    Peer     :  <PROGRAMMER>, <TEAM>, <PEER REVIEWED COMPLETED>
##    Reviewer :
##
   ##
   ##    Clear the decks and load up some functionality
   ##
      rm(list=ls(all=TRUE))
      options(scipen = 999)
   ##
   ##    Core libraries
   ##
      library(ggplot2)
      library(plyr)
      library(stringr)
      library(reshape2)
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(extrafont)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)
   ##
   ##    Project-specific libraries
   ##
      library(comtradr)
      library(curl)
      library(XML)   
      library(RJSONIO)   
      library(data.table)
   
   ##
   ##    Set working directory
   ##
      setwd("c:\\Work_Related_Projects\\COMTRADE")

      Sys.setenv('COMTRADE_PRIMARY' = '9cd0ec8d460147f5956f306cbdea1cf6')
      
   ##
   ##    
   ##
      ##
      ##    Get the comtrade fish data
      ##
         source("Programmes/Comtrade_MetaData.r")  # This goes into Comtrade and pulls out the import and export conversation factors
         source("Programmes/Comtrade_Fish_Data.r") # This goes into Comtrade and pulls out any fish data that hasn't been previously extracted

      ##
      ##    Get the FFA data
      ##
         #source("Programmes/Get_FFA_Data.r")     # Uses curl to get FFA data, but doesn't work. Needs Rseleniumed. In the meantime, manually pull them down and save in data_raw
         source("Programmes/Read_Spreadsheets.r") # Read the FFA Data into R
         source("Programmes/Clean_FFA_Data.r")    # Clean the FFA Data 
         source("Programmes/FFA_Revisions.r")     # Look into the revisions

         source("Programmes/FFA_Analytics.r") # Look into the FFA Data
         
      ##
      ##    STEP 2: Do some fish analytics
      ##
         source("Programmes/Fish_Explore.r") # Looks into the comtrade data - not finished yet


      ##
      ##    Off to the side - look at some of the Data Hub Data
      ##
         source("Programmes/Read_CSVs.r") # Looks into the comtrade data - not finished yet

         
##
##   End of programme
##
