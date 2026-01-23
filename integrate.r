##    Programme:  Comtrade.r
##
##    Objective:  Comtrade is the UN source of trade data. I've verified it against
##                StatsNZ's trade and it matches up.
##
##                This project will access the UN data, and pull down the fisheries related
##                trade data.
##
##                I'm hoping to use this information to start making both throughput measures, and estimates of "world demand".
##                The theory goes that the value of fish trade depends on its world price. Not just Pacific Island Countries 
##                and Territories (PICTs) feed into world volumes. Understanding world price dynamics with respect to traded 
##                volumes would help understand PICT fisheries economics.
##
##                Update:
##                I've altered this programme to also pull in FFA Value of WCPFC-CA Tuna Fisheries data from
##                here: https://www.ffa.int/download/wcpfc-area-catch-value-estimates/
##
##                Other sources of information: https://zenodo.org/records/11410529
##                There's a newer record here: https://zenodo.org/records/15311770
##
##                with codeset described here: https://www.fao.org/fishery/en/collection/asfis/en
##
##                The FAO data was accessed on the 23 August and is the file: Data_Raw/global_nominal_catch_firms_level0_harmonized.csv 
##
##                https://www.fao.org/markets-and-trade/commodities/meat/fao-meat-price-index/en/
##
##
##                Other interesting things... Thailand Input/Output tables: https://www.nesdc.go.th/nesdb_en/ewt_news.php?nid=4429&filename=national_account
##
##                4 August 2025: Found this: https://www.fao.org/fishery/en/fishstat/fishpriceindex from here https://www.fao.org/fishery/en/fishstat
##
##
##                WORK TO DO:
##                It looks like FAO data excludes the South Pacific Regional Fisheries Management Organisation (https://www.sprfmo.int/fisheries/data-2/catch-information/)
##                ... here's a list of a whole group of others: https://www.sprfmo.int/cooperation/other-organisations/
##
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
##    Peer     :  
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
      library(lubridate)
      library(calibrate)
      library(Hmisc)
      library(RColorBrewer)
      library(stringi)
      library(sqldf)
      library(scales)
      library(RDCOMClient)
      library(extrafont)
      library(tictoc)
      library(RODBC)
      
      library(sysfonts)
      library(showtext)
            
   ##
   ##    Project-specific libraries
   ##
      library(comtradr)
      library(curl)
      library(XML)   
      library(RJSONIO)   
      library(data.table)
      library(ggrepel)

      library(strucchange)
      library(lmtest)
      library(dynlm)
      library(systemfit)
      library(tseries)
      library(cluster)
      library(nlme)
      library(plm)
      library(splines)
      library(systemfit)
      library(forecast) 
      library(sf)

      library(ggraph)
      library(ggmap)
      library(networkD3)
      library(tidygraph)
      remotes::install_github("davidsjoberg/ggsankey")
      library(ggsankey) 
      
   ##
   ##    Set working directory
   ##
      setwd("C:/Work_Related_Projects/COMTRADE")
      setwd("S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Oceanic_Fisheries\\COMTRADE")

#      Sys.setenv('COMTRADE_PRIMARY' = '9cd0ec8d460147f5956f306cbdea1cf6')
      Sys.setenv('COMTRADE_PRIMARY' = 'c894211e85524d10874eff4cf3b24475')
      
   ##
   ##    Read the raw data in
   ##
         source("Programmes/Read_CSVs.r")
         source("Programmes/Read_Spreadsheets.r")
      ##
      ##    Get the comtrade fish data
      ##
         source("Programmes/Comtrade_MetaData.r")        # This goes into Comtrade and pulls out the import and export conversation factors
         source("Programmes/Comtrade_CannedFish_Data.r") # This goes into Comtrade and pulls out any fish data that hasn't been previously extracted
         source("Programmes/Comtrade_Fish_Data.r")       # This goes into Comtrade and pulls out any canned fish data that hasn't been previously extracted
         source("Programmes/Comtrade_Ornamental_Fish_Data.r")     # This goes into Comtrade and pulls out ornamental fish for the pet fish industry

         source("Programmes/Ornamental_Fish_Analysis_Version2.r") # does that price analysis on ornamental fish

         ##
         ##    This bit has actually turned into something very interesting - I've been able to identify the international tuna value chain, from raw
         ##       tuna which is exported by harvesting countries to processing countries, who receive it as imports, and then from there, as exports
         ##       of tinned tuna from processing countries and imports of tinned tuna by consuming countries - see 
         ##       "S:\FAME\NC_NOU\FAME COMMON\FAME Economics\Oceanic_Fisheries\COMTRADE\Adhoc_Queries\Estiamated Tinned Tuna Volumes.xlsx" (made by the version 1 programmes)
         ##
         ##    I was able to do this in the quantities in Canned_Tuna_Analysis, Canned_Tuna_International_Trade_Map, Raw_Tuna_Analysis and Raw_Tuna_International_Trade_Map in 
         ##       version 1 of each of these programmes.
         ##
         ##    For my next trick, I'll try in version two of the above programmes to do the same tracing trick, but this time with nominal values as well as quantities.
         ##       If I can, and it does work, then nominal value / quantity will give me average price. On the raw tuna into import into processing countries, this becomes their input 
         ##       costs. And for tinned tuna, this becomes their output costs. The difference is their manuafacturing margin, and is a nice proxy for value added.
         ##
            source("Programmes/Canned_Tuna_Analysis_Version2.r")
            source("Programmes/Canned_Tuna_International_Trade_Map_Verison2.r") # Canned_Tuna_Analysis generated a dataset of canned tuna trade across time and countries
                                                                                # Now turn this into a Sankey plot. For every tinned can exporter, they must have received raw stock...
                                                                       
            source("Programmes/Raw_Tuna_Analysis_Version2.r")                   # Similar to the above
            source("Programmes/Raw_Tuna_International_Trade_Map_Verison2.r")    # The counterparty picture to the above
            
         ##
         ##    The output from the above process are Export_Tinned_Tuna, Import_Tinned_Tuna, Export_Raw_Tuna, Import_Raw_Tuna which represent the international Tuna
         ##       supply chain. Lets look at them in more detail
         ##
            source("Programmes/Tuna_Supply_Chains.r")
         

      ##
      ##    Get the FFA data
      ##
         #source("Programmes/Get_FFA_Data.r")     # Uses curl to get FFA data, but doesn't work. Needs Rseleniumed. In the meantime, manually pull them down and save in data_raw
         source("Programmes/Clean_FFA_Data.r")    # Clean the FFA Data 
         source("Programmes/FFA_Revisions.r")     # Look into the revisions

         source("Programmes/FFA_Analytics.r")     # Look into the FFA Data
         source("Programmes/For_Stephen_Howes.r") # Decompose who is catching what where
         
      ##
      ##    STEP 2: Do some fish analytics
      ##
         ##source("Programmes/Fish_Explore.r") # Looks into the comtrade data - not finished yet
         source("Programmes/FAO_Analysis.r")   # Looks at the FAO data - Post Peter Ellis conversation
         source("Programmes/Vessel_Metrics.r") # Based on Tiffany Vidal code, this code replicates figure 3.1 from here: https://meetings.wcpfc.int/node/23098
         source("Programmes/Present_Value_of_Harvest_Volumes_Version3.r") # Estimate the present value of the historic catch volumes.
                                                                          # Version 1 looked at basic FAO/FFA data
                                                                          # Version 2 brought in COMTRADE data and compared volumes and values.
                                                                          # Version 3 limits COMTRADE to pacific / asian countries and does the present value measures.

         source("Programmes/Fresh_Tuna_Analysis_Version2.r") # Looks at comtrade data with a pacific/non-pacific split.
         source("Programmes/Export Qty by Major Market and Decile.r") # Looks at which countries are the major exporting countries
         source("Programmes/Vessel_Metrics_Revenue_Vs_Effort_Version2.r") # Estimates the VDS Day Price, as a test on the https://github.com/emlab-ucsb/vds-mpa-paper model




      ##
      ##    Report write ups
      ##
         #rmarkdown::render("Programmes/SSAP_Value_Report_for_Victoria_Pilbeam.rmd", output_file = "C:\\Users\\jamesh\\GIT\\COMTRADE\\Product_Output\\Paper_for_Victoria.docx")
         #rmarkdown::render("Programmes/SSAP_Value_Report_For_Victoria_Pilbeam_Post_Peter_Williams.rmd", output_file = "C:\\Users\\jamesh\\GIT\\COMTRADE\\Product_Output\\Paper_for_Victoria_Post_Peter_Williams.docx")
         rmarkdown::render("Programmes/SSAP_Value_Report_For_Victoria_Pilbeam_FINAL.rmd", output_file = "S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Oceanic_Fisheries\\COMTRADE\\Product_Output\\Paper_for_Victoria_FINAL.docx")
         rmarkdown::render("Programmes/SSAP_Value_Report_For_Victoria_Pilbeam_FINAL.rmd", output_file = "C:\\From BigDisk\\GIT\\COMTRADE\\Product_Output\\Paper_for_Victoria_FINAL.docx")


      ##
      ##    CRGA Presentation
      ##
      
         rmarkdown::render("Programmes/CRGA_Presentation.rmd", output_file = "S:\\FAME\\NC_NOU\\FAME COMMON\\FAME Economics\\Oceanic_Fisheries\\COMTRADE\\Product_Output\\CRGA Presentation.pptx")                



         
##
##   End of programme
##
