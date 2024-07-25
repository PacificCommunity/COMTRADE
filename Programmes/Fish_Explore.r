##
##    Programme:  Fish_Explore.r
##
##    Objective:  We've read the comtrade data in. Now lets try looking at it.
##
##                Some of the things I'm hoping to explore are the "world" price dynamics. That asks the question:
##                1. What is "the" price? And in what currency is it defined?
##
##                   I'm thinking of making a FishBasket, at some specific point in time to reflect the size of the 
##                   country with respect to total world output.
##
##                   "Price change" is then the domestic price change multiplied by the weights within the world basket
##
##                2. Draw from the statistics from my thesis
##
##                Ease into this - start with BigEye Tuna
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
   ##    Load data from somewhere
   ##
      load("Data_Output/Comtrade_Fish_Data.rda")
      Comtrade_Fish_Data <- data.table(Comtrade_Fish_Data)
      
   ##
   ## Step 1: Estimate the output and the price change for each country from the base year onwards
   ##
   ##         Start with no purchasing country price differentiation
   ##
                                   
      BigEye <- Comtrade_Fish_Data[str_detect(Comtrade_Fish_Data$cmd_desc, "tuna") &
                                   flow_desc %in% c("Import","Export"),
                                   list(Total_Gross_Wgt                      = sum(gross_wgt,na.rm = TRUE),
                                        Total_Net_Wgt                        = sum(net_wgt,na.rm = TRUE),
                                        Total_Domestic_Currency_FOBValue     = sum(Domestic_Currency_FOBValue,na.rm = TRUE),
                                        Total_Domestic_Currency_CIFValue     = sum(Domestic_Currency_CIFValue,na.rm = TRUE),
                                        Total_Domestic_Currency_PrimaryValue = sum(primary_value,na.rm = TRUE)/1000000),
                                    by = .(cmd_code, 
                                           Period, 
                                           reporter_desc, 
                                           flow_desc)]

      ##
      ##    Estimate the base period
      ##
      Base_Period_Metrics <- BigEye[,
                                      list(Total_Gross_Wgt                      = sum(Total_Gross_Wgt,na.rm = TRUE),
                                           Total_Net_Wgt                        = sum(Total_Net_Wgt,na.rm = TRUE),
                                           Total_Domestic_Currency_FOBValue     = sum(Total_Domestic_Currency_FOBValue,na.rm = TRUE),
                                           Total_Domestic_Currency_CIFValue     = sum(Total_Domestic_Currency_CIFValue,na.rm = TRUE),
                                           Total_Domestic_Currency_PrimaryValue = sum(Total_Domestic_Currency_PrimaryValue,na.rm = TRUE)),
                                       by = .(Year = year(Period), 
                                              flow_desc)]
      Base_Period_Metrics$Domestic_Price <- (1000000*Base_Period_Metrics$Total_Domestic_Currency_PrimaryValue) / Base_Period_Metrics$Total_Net_Wgt
#      write.table(Base_Period_Metrics, file = "Exploratory_Output/Base_Period_Metrics.csv", sep = ",", row.names = FALSE)

      Imports <- Base_Period_Metrics[flow_desc == "Import"]
      Imports <- Imports[Year == 2023]
      Imports <- Imports[order(-Imports$Total_Domestic_Currency_PrimaryValue)]
      Imports

      Exports <- Base_Period_Metrics[flow_desc == "Export"]
      Exports <- Exports[Year == 2023]
      Exports <- Exports[order(-Exports$Total_Domestic_Currency_PrimaryValue)]
      Exports
      

      ggplot(Base_Period_Metrics, 
             aes(x = Year, 
                 y = Total_Net_Wgt,
                 colour = flow_desc))  + 
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.2, colour = "Brown") +
             scale_y_continuous(labels = comma)
   


   ##
   ##    Lets take a look at the price distribution - job for tomorrow
   ##
      Base_Period_Metrics <- BigEye[,
                                      list(Total_Gross_Wgt                      = sum(Total_Gross_Wgt,na.rm = TRUE),
                                           Total_Net_Wgt                        = sum(Total_Net_Wgt,na.rm = TRUE),
                                           Total_Domestic_Currency_FOBValue     = sum(Total_Domestic_Currency_FOBValue,na.rm = TRUE),
                                           Total_Domestic_Currency_CIFValue     = sum(Total_Domestic_Currency_CIFValue,na.rm = TRUE),
                                           Total_Domestic_Currency_PrimaryValue = sum(Total_Domestic_Currency_PrimaryValue,na.rm = TRUE)),
                                       by = .(cmd_code, 
                                              Year = year(Period), 
                                              flow_desc)]
      Base_Period_Metrics$Domestic_Price <- Base_Period_Metrics$Total_Domestic_Currency_PrimaryValue / Base_Period_Metrics$Total_Net_Wgt
      Base_Period_Metrics <- Base_Period_Metrics[order(Base_Period_Metrics$flow_desc, Base_Period_Metrics$Year),]

      ggplot(Base_Period_Metrics, 
             aes(x = Year, 
                 y = Total_Domestic_Currency_PrimaryValue/1000000,
                 colour = flow_desc))  + 
             geom_smooth(se = FALSE) +
             geom_point(alpha = 0.2, colour = "Brown") +
             scale_y_continuous(labels = comma)
   


      

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
