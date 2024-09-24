##
##    Programme:  Fresh_Tuna_Analysis.r
##
##    Objective:  There's a bit of a theory that the opening of the pacific tuna fisheries had the effect of 
##                flooding the market with raw tuna, depressing both the raw tuna and tinned tuna markets.
##                I've found that there is a shocking durth of monetary value measures for the volume
##                of Tuna caught, so I'm going to try and estimate some price measures using export / import 
##                information. 
##
##
##                Here's the "inscope" fish Harmonized System Codes:
##
## 1:                                                                                                            030194 - Fish; live, Atlantic and Pacific bluefin tunas (Thunnus thynnus, Thunnus orientalis)
## 2:                                                                                                                                           030195 - Fish; live, southern bluefin tunas (Thunnus maccoyii)
## 3:                          030231 - Fish; fresh or chilled, albacore or longfinned tunas (Thunnus alalunga), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 4:                                      030232 - Fish; fresh or chilled, yellowfin tunas (Thunnus albacares), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 5:               030233 - Fish; fresh or chilled, skipjack tuna (stripe-bellied bonito) (Katsuwonus pelamis), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 6:                                            030234 - Fish; fresh or chilled, bigeye tunas (Thunnus obesus), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 7: 030235 - Fish; fresh or chilled, Atlantic and Pacific bluefin tunas (Thunnus thynnus, Thunnus orientalis), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 8:                                030236 - Fish; fresh or chilled, southern bluefin tunas (Thunnus maccoyii), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 9:                                          030239 - Fish; fresh or chilled, tuna, n.e.c. in item no. 0302.3, excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0302.91 to 0302.99
## 10:                                    030341 - Fish; frozen, albacore or longfinned tunas (Thunnus alalunga), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 11:                                                030342 - Fish; frozen, yellowfin tunas (Thunnus albacares), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 12:                         030343 - Fish; frozen, skipjack tuna (stripe-bellied bonito) (Katsuwonus pelamis), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 13:                                                      030344 - Fish; frozen, bigeye tunas (Thunnus obesus), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 14:           030345 - Fish; frozen, Atlantic and Pacific bluefin tunas (Thunnus thynnus, Thunnus orientalis), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 15:                                          030346 - Fish; frozen, southern bluefin tunas (Thunnus maccoyii), excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 16:                                                    030349 - Fish; frozen, tuna, n.e.c. in item no. 0303.4, excluding fillets, fish meat of 0304, and edible fish offal of subheadings 0303.91 to 0303.99
## 17:                                                                                  030487 - Fish fillets; frozen, tunas (of the genus Thunnus), skipjack tuna (stripe-bellied bonito) (Katsuwonus pelamis)
## 18:                                                                     160414 - Fish preparations; tunas, skipjack tuna and bonito (Sarda spp.), prepared or preserved, whole or in pieces (but not minced)


##
##    Author:     James Hogan, FAME - SPC, 22 September 2024
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
         Inscope_Codes <- c('030194','030195','030231','030232','030233','030234','030235','030236','030239','030341','030342','030343','030344','030345','030346','030349','030487','160414')
         
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
      Import_Export_in_Domestic_Currency <- Comtrade_Fish_Data[(partner_desc %in% c("American Samoa","Areas, nes","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","Cook Isds",
                                                                                    "Fiji","France","French Polynesia","FS Micronesia","Guam","Indonesia","Kiribati","Malaysia","Marshall Isds",
                                                                                    "N. Mariana Isds","Nauru","New Caledonia","Oceania, nes","Other Asia, nes","Palau","Papua New Guinea","Philippines",
                                                                                    "Pitcairn","Rep. of Korea","Samoa","Solomon Isds","Thailand","Tokelau","Tonga","Tuvalu","Vanuatu","Viet Nam",
                                                                                    "Christmas Isds","Japan","Niue","Singapore","Timor-Leste","Wallis and Futuna Isds","USA")),
                                                               list(Total_Gross_Wgt = sum(gross_wgt,na.rm = TRUE),
                                                                    Total_Net_Wgt   = sum(net_wgt,na.rm = TRUE)/1000,
                                                                    Domestic_Currency_PrimaryValue = sum(Domestic_Currency_PrimaryValue,na.rm = TRUE),
                                                                    primary_value   = sum(primary_value, na.rm = TRUE),
                                                                    Implicit_Price_Domestic_Currancy = sum(Domestic_Currency_PrimaryValue,na.rm = TRUE) / (sum(net_wgt,na.rm = TRUE)/1000)),
                                                                by = .(Species <- ifelse(cmd_code %in% c('030194', '030195', '030235', '030236', '030345', '030346'), "Bluefin", 
                                                                                  ifelse(cmd_code %in% c('030231', '030341'), "Albacore", 
                                                                                  ifelse(cmd_code %in% c('030233', '030343','030487','160414'), "Skipjack", 
                                                                                  ifelse(cmd_code %in% c('030232', '030342'), "Yellowfin", 
                                                                                  ifelse(cmd_code %in% c('030234', '030344'), "Bigeye", "Tuna NEC"))))), 
                                                                       Year = year(Period), 
                                                                       reporter_desc, 
                                                                       partner_desc,
                                                                       Cleaned_Measure)]

   ##
   ##    Base weight the domestic values using conversion factors from A SPECIFIC YEAR - this expresses that domestic value as US dollars for a specific year
   ##
      Import_Export_in_Domestic_and_USD_Currency <- merge(Import_Export_in_Domestic_Currency,
                                                          New_Metadata_Annual[year(New_Metadata_Annual$Period) == 2019,c("ReporterCode", "ReporterDesc", "Import_Conversion_Factor", "Export_Conversion_Factor")],
                                                          by.x = c("reporter_desc"),
                                                          by.y = c("ReporterDesc"))
      Import_Export_in_Domestic_and_USD_Currency$PrimaryValue_USD2019 <- with(Import_Export_in_Domestic_and_USD_Currency, 
                                                                              ifelse(Cleaned_Measure == "Exports",
                                                                                     Domestic_Currency_PrimaryValue * Export_Conversion_Factor,
                                                                                     Domestic_Currency_PrimaryValue * Import_Conversion_Factor))
      ##
      ##    Lets have a small peek
      ##
      Peek <- data.frame(Import_Export_in_Domestic_and_USD_Currency[Total_Net_Wgt > 0,
                                                         list(Total_Net_Wgt        = sum(Total_Net_Wgt,na.rm = TRUE),
                                                              PrimaryValue_USD2019 = sum(PrimaryValue_USD2019,na.rm = TRUE),
                                                              primary_value        = sum(primary_value,na.rm = TRUE),
                                                              Implicit_Price_USD2019 = sum(primary_value,na.rm = TRUE) / sum(Total_Net_Wgt,na.rm = TRUE)),
                                                          by = .(Year,
                                                                 Species,
                                                                 reporter_desc,
                                                                 partner_desc,
                                                                 Cleaned_Measure)])
                                                                                     
      ##
      ##    Allocate the countries to wine price deciles for each year
      ##
      Years <- unique(Peek$Year)
      Results <- data.frame()
      Cleaned_Measure <- unique(as.character(Peek$Cleaned_Measure))
      Species <- unique(Peek$Species)
      Deciles_over_time <- data.frame()
      for(S in 1:length(Species))
      {
         for(j in 1:length(Cleaned_Measure))
         {
            for(i in 1:length(Years))
            {
              One_Year <- Peek[(Peek$Year == Years[i]) & 
                               (Peek$Cleaned_Measure == as.character(Cleaned_Measure[j])) & 
                               (Peek$Species == Species[S]),]
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
                  Deciles$Species         <- Species[S]
                  Deciles_over_time       <- rbind.fill(Deciles_over_time, Deciles)
                  rm(One_Year)
                 }
            }
         }
      }

          
         Deciles_over_time <- data.table(Deciles_over_time[order(Deciles_over_time$Species,Deciles_over_time$Cleaned_Measure, Deciles_over_time$DGroup, Deciles_over_time$Year),])
         #Deciles_over_time <- Deciles_over_time[Deciles_over_time$Value.y < 100,]
          
          
         AverPrices <- Deciles_over_time[,
                                         list(Average_over_Time = mean(Value.y,na.rm = TRUE)),
                                         by = .(Species, 
                                                Cleaned_Measure,
                                                DGroup)]
      
         Deciles_over_time <- merge(Deciles_over_time,
                                    AverPrices,
                                    by = c("Species", "Cleaned_Measure", "DGroup"))
       
         for(i in unique(AverPrices$Species))
         {
         showtext_auto()
            ggplot(Deciles_over_time[((Deciles_over_time$Species == i) & 
                                      (Deciles_over_time$ID.y < 11) &
                                      (Deciles_over_time$Year < 2024)),])      + 
             geom_line(aes(x=Year, y=Value.y), colour = SPCColours("Green"), size = 0.7) +
             geom_line(aes(x=Year, y=Average_over_Time), linetype = "dashed", colour = SPCColours("Light_Blue"), size = 0.7) +
             facet_grid(Cleaned_Measure ~ DGroup, scales = "free", space = "free") +
             scale_y_continuous(labels = dollar, breaks = seq(from = 0, to = 100000, by =2500)) +
             labs(title = paste0("Average Raw Tuna Price - by Decile over Time\nSpecies: ", i, "\n"),  
                  caption = "SPC - FAME\n") +
                  ylab("Average USD Implicit Export Price\nPer Metric Tonne\n") +
                  xlab("\nCalendar Year\n") +
                theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                theme(legend.title.align=0.5,
                      plot.margin = unit(c(1,1,1,1),"mm"),
                      panel.border = element_blank(),
                      strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                      strip.text = element_text(colour = "white", 
                                                size   = 13,
                                                family = "MyriadPro-Regular",
                                                margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                      panel.spacing = unit(1, "lines"),                                              
                      legend.text   = element_text(size = 12, family = "MyriadPro-Regular"),
                      plot.title    = element_text(size = 18, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                      plot.subtitle = element_text(size = 12, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                      plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                      axis.title    = element_text(size = 12, colour = SPCColours("Dark_Blue")),
                      axis.text.x   = element_text(size = 11, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                      axis.text.y   = element_text(size = 11, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                      legend.key.width = unit(5, "mm"),
                      legend.spacing.y = unit(1, "mm"),
                      legend.margin = margin(0, 0, 0, 0),
                      legend.position  = "bottom")
                  
             ggsave(paste0("Graphical_Output/Raw Tuna Export and Import Prices Species ", i, ".png"), height =16.13, width = 20.66, dpi = 165, units = c("cm"))
         }
         
##
##    Which markets are Exporting and Importing the volumes into?
##               
   Export_Volumes <- aggregate(Total_Net_Wgt ~ Species + Decile + Year + partner_desc,
                           data = Results[Results$Cleaned_Measure == "Import",],
                           FUN  = sum,
                           subset = Year < 2024,
                           na.action = NULL)

   AverVolumes <- aggregate(Total_Net_Wgt ~ Species + Decile + partner_desc,
                           data = Export_Volumes,
                           FUN  = mean,
                           subset = Year < 2024,
                           na.action = NULL)
                           
   names(AverVolumes)[4] ="Average_over_Time"

   Export_Volumes <- merge(Export_Volumes,
                           AverVolumes,
                           by = c("Species", "Decile", "partner_desc"))

                                    
   Export_Volumes <- Export_Volumes[order(Export_Volumes$Species, Export_Volumes$Decile, Export_Volumes$Year),]
   ##
   ##    Identify the major markets, and the minnows
   ##               
      Majors_and_Minnows <- sqldf("Select a.Species,
                                          a.Decile,
                                          a.partner_desc,
                                          a.Average_over_Time,
                                          b.Total,
                                          a.Average_over_Time / b.Total as Proportion
                                     from AverVolumes a,
                                       (select Species,
                                               Decile,
                                               sum(Average_over_Time) as total
                                         from AverVolumes
                                         group by Decile, Species) b
                                     where (a.Decile = b.Decile)
                                       and (a.Species = b.Species)
                                     order by a.Decile,a.Species,
                                           a.Average_over_Time / b.Total desc")
                                           
     Majors_and_Minnows$Major <- ifelse(Majors_and_Minnows$Proportion >= 0.05, Majors_and_Minnows$partner_desc, "Collective Minnows")
     Majors_and_Minnows <- Majors_and_Minnows[,c("partner_desc", "Species", "Decile", "Major")]

     Results <- merge(Results[Results$Cleaned_Measure == "Import",],
                      Majors_and_Minnows,
                      by = c("partner_desc", "Species", "Decile"))

   ##
   ##    Re-estimate the volumes
   ##               
      Export_Volumes <- aggregate(Total_Net_Wgt ~ Species + Decile + Year + Major,
                              data = Results,
                              FUN  = sum,
                              subset = Year < 2024,
                              na.action = NULL)

      AverVolumes <- aggregate(Total_Net_Wgt ~ Species + Decile + Major,
                              data = Export_Volumes,
                              FUN  = mean,
                              subset = Year < 2022,
                              na.action = NULL)
                              
      names(AverVolumes)[4] ="Average_over_Time"

      Export_Volumes <- merge(Export_Volumes,
                              AverVolumes,
                              by = c("Species", "Decile", "Major"))
                                       
      Export_Volumes <- Export_Volumes[order(Export_Volumes$Species, Export_Volumes$Decile, Export_Volumes$Year),]
               
      Export_Volumes$Major_Short <- str_wrap(Export_Volumes$Major, 10)

Deciles <- unique(Export_Volumes$Decile)
Species <- unique(Export_Volumes$Species)

for(j in 1:length(Species))
{

   for(i in 1:length(Deciles))
   {
      showtext_auto()
      ggplot(Export_Volumes[(Export_Volumes$Species == Species[j]) &
                            (Export_Volumes$Year < 2024) &
                            (Export_Volumes$Decile == Deciles[i]),])      + 
       geom_line(aes(x=Year, y=Total_Net_Wgt/1000), colour = SPCColours("Green"), size = 0.5) +
       geom_line(aes(x=Year, y=Average_over_Time/1000), linetype = "dashed", colour = SPCColours("Light_Blue"), size = 0.5) +
       facet_grid( . ~ Major_Short, scales = "free", space = "free") +
       scale_y_continuous(labels = comma) +
       labs(title = paste0("Exporting Raw Tuna Volumes -",Species[j], " - Major Markets"),  
            subtitle = paste0("\nPrice Decile: ", Deciles[i], "\n"),
            caption = "SPC - FAME\n") +
            ylab("Exported Tuna Volumes\nMega tonnes\n") +
            xlab("\nCalendar Year\n") +
       theme_bw(base_size=12, base_family =  "Calibri") %+replace%
       theme(legend.title.align=0.5,
             plot.margin = unit(c(1,1,1,1),"mm"),
             panel.border = element_blank(),
             strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
             strip.text = element_text(colour = "white", 
                                       size   = 13,
                                       family = "MyriadPro-Regular",
                                       margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
             panel.spacing = unit(1, "lines"),                                              
             legend.text   = element_text(size = 12, family = "MyriadPro-Regular"),
             plot.title    = element_text(size = 18, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
             plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
             plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
             plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
             axis.title    = element_text(size = 12, colour = SPCColours("Dark_Blue")),
             axis.text.x   = element_text(size = 11, colour = SPCColours("Dark_Blue"), angle = 90, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
             axis.text.y   = element_text(size = 11, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
             legend.key.width = unit(5, "mm"),
             legend.spacing.y = unit(1, "mm"),
             legend.margin = margin(0, 0, 0, 0),
             legend.position  = "bottom")

       ggsave(paste0("Graphical_Output/",Species[j], " Exported Tuna Volumes - Major Markets - Price Decile ", i,".png"), height =16.13, width = 20.66, dpi = 165, units = c("cm"))
   }
}      
##
##    And we're done
##
