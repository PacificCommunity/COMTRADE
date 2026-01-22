##
##    Programme:  Ornamental_Fish_Analysis.r
##
##    Objective:  Kiribati are interested in whether they can crack the Ornamental (pet)
##                fish market
##
##                Here's the "inscope" fish Harmonized System Codes:
##
##                                                              Description   Code
##                                                                   <char> <char>
##                1:                            030110 - -  Ornamental fish 030110
##                2:            030111 - Fish; live, ornamental, freshwater 030111
##                3: 030119 - Fish; live, ornamental, other than freshwater 030119

##
##    Author:     James Hogan, FAME - SPC, 11 December 2025
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/themes.r")
      load('Data_Output/New_Metadata_Annual.rda')     #  COMTRADE convert locale currency to US dollares through import and export conversation 
      load('Data_Output/New_Metadata_Monthly.rda')    #  factors. These two dataframes contain this information separate - I want to be able to express
                                                      #  world tuna trade in local currency indicies, base weighed together using hte import / export
                                                      #  conversation factors at a single point in time, so change is 
   ##
   ##    Collect all of the individual fresh tuna codes
   ##   
         Inscope_Codes <- c('030110','030111','030119')
         
         Contents <- as.data.frame(list.files(path = "Data_Raw/",  pattern = "*.rda"))
         names(Contents) = "DataFrames"
         Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n = 2)[,1]
         
         Inscope_Dataframes <- do.call(rbind, lapply(Inscope_Codes, function(i)
                                                      {
                                                         return(Contents[str_detect(Contents$DataFrames, i),])
                                                      })
                                      )

         All_Data <- lapply(Inscope_Dataframes$DataFrames, function(File){
                              load(paste0("Data_Raw/", File))  
                              X <- get(str_split_fixed(File, "\\.", n = 2)[,1])
                              return(X)})
         Raw_Tuna_Data <- do.call(rbind.fill, All_Data)
         Raw_Tuna_Data$Period <- as.Date(paste0(Raw_Tuna_Data$period,"01"),"%Y%m%d")
   ##
   ## Step 2: Merge data with the Metadata to bring through the conversion factors - we want to remove this and get back to the underlying domestic currancy
   ##
      Comtrade_Fish_Data <- data.table(merge(Raw_Tuna_Data,
                                             New_Metadata_Monthly,
                                             by.x = c("Period", "reporter_code", "reporter_desc"),
                                             by.y = c("Period", "ReporterCode",  "ReporterDesc"),
                                             all.x = TRUE))
                                  
      Comtrade_Fish_Data$Domestic_Currency_FOBValue     <- Comtrade_Fish_Data$fobvalue      / Comtrade_Fish_Data$Export_Conversion_Factor
      Comtrade_Fish_Data$Domestic_Currency_CIFValue     <- Comtrade_Fish_Data$cifvalue      / Comtrade_Fish_Data$Export_Conversion_Factor
      Comtrade_Fish_Data$Domestic_Currency_PrimaryValue <- Comtrade_Fish_Data$primary_value / Comtrade_Fish_Data$Export_Conversion_Factor
      
      Comtrade_Fish_Data$Cleaned_Measure <- ifelse(str_detect(Comtrade_Fish_Data$flow_desc, "Export"), "Export", "Import")

      Comtrade_Fish_Data <- Comtrade_Fish_Data[!is.na(Comtrade_Fish_Data$Period),]
      
      
   ##
   ##    Lets just work on the volume exports and imports
   ##
      #Comtrade_Fish_Data <- Comtrade_Fish_Data[reporter_desc == "New Zealand"]
      Comtrade_Fish_Data$Year <- year(Comtrade_Fish_Data$Period)
      
      Comtrade_Fish_Data$Implicit_Price_USD2019 = Comtrade_Fish_Data$primary_value / Comtrade_Fish_Data$net_wgt
#      Comtrade_Fish_Data$Implicit_Price_USD2019 = Comtrade_Fish_Data$Domestic_Currency_PrimaryValue / Comtrade_Fish_Data$net_wgt
      
      Import_Export_in_Domestic_Currency <- Comtrade_Fish_Data[reporter_desc == "New Zealand",
                                                               list(Total_Gross_Wgt = sum(gross_wgt,na.rm = TRUE),
                                                                    Total_Net_Wgt   = sum(net_wgt,na.rm = TRUE),
                                                                    Domestic_Currency_PrimaryValue = sum(Domestic_Currency_PrimaryValue,na.rm = TRUE),
                                                                    primary_value   = sum(primary_value, na.rm = TRUE),
                                                                    Implicit_Price_Domestic_Currancy = sum(Domestic_Currency_PrimaryValue,na.rm = TRUE) / (sum(net_wgt,na.rm = TRUE))),
                                                                by = .(Year = year(Period), 
                                                                       cmd_desc = cmd_desc,
                                                                       reporter_desc = reporter_desc,
                                                                       qty_unit_abbr = qty_unit_abbr,
                                                                       partner_desc = partner_desc,
                                                                       # reporter_desc <- ifelse(reporter_desc %in% c("American Samoa","Areas, nes","Australia","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","Cook Isds",
                                                                                                                    # "Fiji","France","French Polynesia","FS Micronesia","Guam","Indonesia","Kiribati","Malaysia","Marshall Isds",
                                                                                                                    # "N. Mariana Isds","Nauru","New Caledonia", "New Zealand","Oceania, nes","Other Asia, nes","Palau","Papua New Guinea","Philippines",
                                                                                                                    # "Pitcairn","Rep. of Korea","Samoa","Solomon Isds","Thailand","Tokelau","Tonga","Tuvalu","Vanuatu","Viet Nam",
                                                                                                                    # "Christmas Isds","Japan","Niue","Singapore","Timor-Leste","Wallis and Futuna Isds","USA"), "Pacific", "Non-Pacific"), 
                                                                       # partner_desc <- ifelse(partner_desc %in% c("American Samoa","Areas, nes","Australia","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","Cook Isds",
                                                                                                                  # "Fiji","France","French Polynesia","FS Micronesia","Guam","Indonesia","Kiribati","Malaysia","Marshall Isds",
                                                                                                                  # "N. Mariana Isds","Nauru","New Caledonia", "New Zealand","Oceania, nes","Other Asia, nes","Palau","Papua New Guinea","Philippines",
                                                                                                                  # "Pitcairn","Rep. of Korea","Samoa","Solomon Isds","Thailand","Tokelau","Tonga","Tuvalu","Vanuatu","Viet Nam",
                                                                                                                  # "Christmas Isds","Japan","Niue","Singapore","Timor-Leste","Wallis and Futuna Isds","USA"), "Pacific", "Non-Pacific"),
                                                                       Cleaned_Measure)]

      ##
      ##    Lets have a small peek
      ##
      Peek <- data.frame(Import_Export_in_Domestic_Currency[(Total_Net_Wgt > 0) & (Cleaned_Measure == "Import"),
                                                         list(Total_Net_Wgt        = sum(Total_Net_Wgt,na.rm = TRUE),
                                                              primary_value        = sum(primary_value,na.rm = TRUE),
                                                              Implicit_Price_USD2019 = sum(primary_value,na.rm = TRUE) / sum(Total_Net_Wgt,na.rm = TRUE)),
                                                          by = .(Year,
                                                                 cmd_desc,
                                                                 qty_unit_abbr,
                                                                 partner_desc,
                                                                 Cleaned_Measure)])
      
      ##
      ##    Allocate the countries to wine price deciles for each year
      ##
      Years           <- unique(Comtrade_Fish_Data$Year)
      Cleaned_Measure <- unique(as.character(Comtrade_Fish_Data$Cleaned_Measure))
      
      Results <- data.frame()
      Deciles_over_time <- data.frame()
      for(j in 1:length(Cleaned_Measure))
      {
         for(i in 1:length(Years))
         {
           One_Year <- Comtrade_Fish_Data[(Comtrade_Fish_Data$Year == Years[i]) & 
                                          (Comtrade_Fish_Data$Cleaned_Measure == as.character(Cleaned_Measure[j])) &
                                          is.finite(Comtrade_Fish_Data$Implicit_Price_USD2019),]
                            
           if(nrow(One_Year) > 0)
             {
              Deciles <- data.frame(Value = quantile(One_Year$Implicit_Price_USD2019, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE))
              Deciles$Decile_Group <- row.names(Deciles)
              Deciles$ID <- 1:nrow(Deciles)
              Deciles$MatchID = Deciles$ID - 1
              Deciles <- merge(Deciles,
                               Deciles,
                               by.x = c("ID"),
                               by.y = c("MatchID"))
              Deciles$DGroup <- paste(Deciles$Decile_Group.x, Deciles$Decile_Group.y, sep = " - ")
              Deciles$Year <- Years[i]
                 
               for(i in 1:nrow(One_Year))
                  {
                     One_Year$Decile[i]      <- Deciles$DGroup[((One_Year$Implicit_Price_USD2019[i] >= Deciles$Value.x ) &
                                                                (One_Year$Implicit_Price_USD2019[i] <= Deciles$Value.y ))]
                     One_Year$DecileGroup[i] <- Deciles$ID[((One_Year$Implicit_Price_USD2019[i] >= Deciles$Value.x ) &
                                                            (One_Year$Implicit_Price_USD2019[i] <= Deciles$Value.y ))]
                  }  
               Results <- rbind.fill(Results, One_Year)
               Deciles$Cleaned_Measure <- Cleaned_Measure[j]
               Deciles_over_time       <- rbind.fill(Deciles_over_time, Deciles)
               rm(One_Year)
              }
         }
      }
          
         Deciles_over_time <- data.table(Deciles_over_time[order(Deciles_over_time$Cleaned_Measure, Deciles_over_time$DGroup, Deciles_over_time$Year),])
         #Deciles_over_time <- Deciles_over_time[Deciles_over_time$Value.y < 100,]
          
          
         AverPrices <- Deciles_over_time[,
                                         list(Average_over_Time = mean(Value.y,na.rm = TRUE)),
                                         by = .(Cleaned_Measure,
                                                DGroup)]
      
         Deciles_over_time <- merge(Deciles_over_time,
                                    AverPrices,
                                    by = c("Cleaned_Measure", "DGroup"))
         showtext_auto()
         ggplot(Deciles_over_time[(Deciles_over_time$Cleaned_Measure == "Import") & (Deciles_over_time$ID.y < 11),])      + 
          geom_line(aes(x=Year, y=Value.y), colour = SPCColours("Green"), size = 0.7) +
          geom_line(aes(x=Year, y=Average_over_Time), linetype = "dashed", colour = SPCColours("Light_Blue"), size = 0.7) +
          facet_grid(Cleaned_Measure  ~ DGroup, scales = "free", space = "free") +
          scale_y_continuous(labels = dollar, breaks = seq(0,500, 20), limits = c(0, 500)) +
          labs(title ="Average Ornamental Fish Import Price - by Decile over Time",  
               subtitle = "COMTRADE Data\n",  
               caption = "SPC - FAME\n") +
               ylab("Average USD Import Price\nPer Kilogram\n") +
               xlab("\nCalendar Year\n") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,1,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 18,
                                             family = "MyriadPro-Regular",
                                             margin = margin(0,0,0,0, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 12, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 12, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 18,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 16, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 16, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 16, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(5, "mm"),
                   legend.spacing.y = unit(1, "mm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")
               
          ggsave(paste0("Graphical_Output/Ornamental Fish Export and Import Prices Species By Origin.png"), height =1.25*16.13, width = 1.25*20.66, dpi = 165, units = c("cm"))

   Ornamental_Fish_Deciles_over_time <- Deciles_over_time
   save(Ornamental_Fish_Deciles_over_time, file = "Data_Output/Ornamental_Fish_Deciles_over_time.rda")
   
##
##
##


         

##
##    And we're done
##
