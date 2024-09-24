##
##    Programme:  Present_Value_of_Harvest_Volumes.r
##
##    Objective:  SPCs Skipjack Survey and Assessment Programme (SSAP) ran from 1977 – 1981 and opened up the 
##                Pacific skipjack fisheries. Consequently, large volumes of fishing vessels came in 
##                and the volume of fish that has since been extract is huge. One of the questions Neville wants
##                me to answer is what's the present value of the historical volumes, and how does that compare
##                to the cost of the science which opened the tuna fishery.
##
##                The same issues also pop up for the value of the tuna catch to the member countries - can we 
##                go back in time and estimate those?
##
##                What makes this a complicated problem is that the volume data goes back to 1972, but doesn't come
##                with any value data. So I'm struggling for a metric that converts volumes into values.
##
##                Solutions?
##                1. Look at the long-term average price metrics from the FFA and extrapolate the price trend back
##                   in time. This assumes that the long term average price trend was stable. When it probably isnt and 
##                   wasnt.
##
##                2. Scour the internet for ANY historical measures of the value of sales from the tuna fisheries
##                   and interpolate a spline trend through those points. At least we've got things to interpolate a 
##                   line through, even if we have no idea how the historical data matches to the current day metrics.
##
##                3. Estimate the relationship between FFA average tuna prices, and its secondary product price, like tinned
##                   tuna CPI or the PPI. This assumes "pass-through" effects: that the value of rawtuna pass-through 
##                   into the value of tinned tuna, and prices changes in raw inputs are carried into price changes in
##                   tinned outputs. But if a relationship can be found, then I can use the historical tinned CPI to 
##                   estimate the raw tuna prices.
##
##                   Again, a long bow maybe, given the price dynamics - tuna cpi/ppi decreased because pacific tuna flooded 
##                   the market. If anything it meant that historical caught tuna prices should have been falling, 
##                   offsetting the benefits of volume growth.
##
##
##                   https://www.ffa.int/download/economic-development-indicators-and-statistics/
##
##
##    Author:     James Hogan, FAME - The Pacific Community (SPC)
##
##    Peer     :  
##    Reviewer :
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/themes.r")
      
   ##
   ##    Load data from somewhere
   ##
      ##
      ##    FAO data on historical volumes
      ##
         load("Data_Intermediate/RAWDATA_ASFIS_spXXASFIS_sp_2023.rda")
         load("Data_Intermediate/RAWDATA_XXglobal_nominal_catch_firms_level0_harmonized.rda")

         ASFIS <- data.table(RAWDATA_ASFIS_spXXASFIS_sp_2023)
         FAO   <- data.table(RAWDATA_XXglobal_nominal_catch_firms_level0_harmonized)

      ##
      ##    FFA data on current volumes and values
      ##
         load('Data_Intermediate/FFASummaryData.rda')
         load('Data_Intermediate/FFA_Compendium_of_Economic_and_Development_Statistics_2022.rda')
         
      ##
      ##    Grab data on CPI
      ##
         load('Data_Intermediate/RAWDATA_Spliced IndexXXSplited_Price_Indexes.rda')
         Splited_Price_Indexes <- `RAWDATA_Spliced IndexXXSplited_Price_Indexes`
         Splited_Price_Indexes <- Splited_Price_Indexes[,c("X.12","X.13")]
         names(Splited_Price_Indexes) <- c("TimePeriod", "Composite_Price")
         Splited_Price_Indexes$TimePeriod <- as.Date(Splited_Price_Indexes$TimePeriod, "%d/%m/%Y")
         Splited_Price_Indexes$Composite_Price <- as.numeric(Splited_Price_Indexes$Composite_Price)
         Splited_Price_Indexes <- Splited_Price_Indexes[!is.na(Splited_Price_Indexes$TimePeriod),]

         
      ##
      ##    Grab COMTRADE metadata
      ##
         load('Data_Output/New_Metadata_Annual.rda')     #  COMTRADE convert locale currency to US dollares through import and export conversation 
         load('Data_Output/New_Metadata_Monthly.rda')    #  factors. These two dataframes contain this information separate - I want to be able to express
                                                         #  world tuna trade in local currency indicies, base weighed together using hte import / export
                                                         #  conversation factors at a single point in time, so change is 

                     
   ##
   ## Step 1: Clean up and enrich the FAO Data
   ##
      FAO$time_start <- as.Date(FAO$time_start, "%Y-%m-%d")
      FAO$time_end   <- as.Date(FAO$time_end, "%Y-%m-%d")
      FAO$measurement_value <- as.numeric(FAO$measurement_value)
      FAO$gear_type <- as.factor(FAO$gear_type)

      FAO <- merge(FAO,
                   ASFIS[, c("ISSCAAP_Group","Taxonomic_Code","Alpha3_Code","Scientific_Name","English_name")],
                   by.x = c("species"),
                   by.y = c("Alpha3_Code"),
                   all.x= TRUE)

      Source <- data.table(source_authority = c('UN-FAO','IRD','CCSBT','IATTC','ICCAT','IOTC','WCPFC'),
                           Source_Authority = c('Food and Agriculture Organization of the United Nations',
                                                'French National Research Institute for Sustainable Development',
                                                'Commission for the Conservation of Southern Bluefin Tuna',
                                                'Inter-American Tropical Tuna Commission',
                                                'International Commission for the Conservation of Atlantic Tunas',
                                                'Indian Ocean Tuna Commission',
                                                'Western and Central Pacific Fisheries Commission'))
      FAO <- merge(FAO,
                   Source,
                   by = c("source_authority"))
                   
   ##
   ## Calculate Core Tuna
   ##
      FAO_Core_Tuna <- FAO[English_name %in% c("Albacore", "Black skipjack", "Skipjack tuna", "Bigeye tuna", "Yellowfin tuna")]
      FAO_Core_Tuna$Measure <- ifelse(FAO_Core_Tuna$English_name %in% c("Black skipjack", "Skipjack tuna"), "Skipjack",
                               ifelse(FAO_Core_Tuna$English_name %in% c("Bigeye tuna"), "Bigeye",
                               ifelse(FAO_Core_Tuna$English_name %in% c("Yellowfin tuna"), "Yellowfin",FAO_Core_Tuna$English_name)))
                               
      FAO_Core_Tuna <- FAO_Core_Tuna[source_authority == 'WCPFC',
                                      list(FAO_Catch_Volume = sum(measurement_value,na.rm = TRUE)),
                                      by = .(Year = year(time_start), 
                                             Measure)]

   ##
   ## Calculate FFA Tuna
   ##
      Catch_Value  <- FFASummaryData[["Summary of catch value"]]
      Catch_Volume <- FFASummaryData[["Summary of catch"]]

      Catch_Value  <- data.table(Catch_Value[Catch_Value$Spreadsheet == "WCPFC-CA_tuna_fisheries_2023",])
      Catch_Volume <- data.table(Catch_Volume[Catch_Volume$Spreadsheet == "WCPFC-CA_tuna_fisheries_2023",])

      Value_by_Species  <- Catch_Value[Catch_Value$Data_Row == "5.2 VALUE OF CATCH BY SPECIES",c("Measure","Year","value")]
      Volume_by_Species <- Catch_Volume[Catch_Volume$Data_Row == "4.2 CATCH BY SPECIES",c("Measure","Year","value")]
      
      Value_by_Species$Value   <- (Value_by_Species$value)
      Volume_by_Species$Volume <- Volume_by_Species$value
      
      FFA_Core_Tuna <-merge(Value_by_Species[,c("Measure","Year","Value")],
                            Volume_by_Species[,c("Measure","Year","Volume")],
                            by = c("Measure","Year"))


   ##    HOW WELL DOES COMTRADE LINE UP?
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
      
      Import_Export_in_Domestic_Currency <- Comtrade_Fish_Data[(Cleaned_Measure == "Import") &
                                                               (partner_desc %in% c("American Samoa","Areas, nes","Australia","Cambodia","China","China, Hong Kong SAR","China, Macao SAR","Cook Isds",
                                                                                    "Fiji","France","French Polynesia","FS Micronesia","Guam","Indonesia","Kiribati","Malaysia","Marshall Isds",
                                                                                    "N. Mariana Isds","Nauru","New Caledonia", "New Zealand","Oceania, nes","Other Asia, nes","Palau","Papua New Guinea","Philippines",
                                                                                    "Pitcairn","Rep. of Korea","Samoa","Solomon Isds","Thailand","Tokelau","Tonga","Tuvalu","Vanuatu","Viet Nam",
                                                                                    "Christmas Isds","Japan","Niue","Singapore","Timor-Leste","Wallis and Futuna Isds","USA")),
                                                               list(Total_Gross_Wgt = sum(gross_wgt,na.rm = TRUE),
                                                                    Total_Net_Wgt   = sum(net_wgt,na.rm = TRUE),
                                                                    Domestic_Currency_PrimaryValue = sum(primary_value,na.rm = TRUE),
                                                                    Implicit_Price_Domestic_Currancy = sum(primary_value,na.rm = TRUE) / sum(net_wgt,na.rm = TRUE)),
                                                                by = .(Measure <- ifelse(cmd_code %in% c('030194', '030195', '030235', '030236', '030345', '030346'), "Bluefin", 
                                                                                  ifelse(cmd_code %in% c('030231', '030341'), "Albacore", 
                                                                                  ifelse(cmd_code %in% c('030233', '030343','030487','160414'), "Skipjack", 
                                                                                  ifelse(cmd_code %in% c('030232', '030342'), "Yellowfin", 
                                                                                  ifelse(cmd_code %in% c('030234', '030344'), "Bigeye", "Tuna NEC"))))),
                                                                       Year = year(Period))]





      ##
      ##    Lets have a small peek
      ##
      Comtrade_Imports <- Import_Export_in_Domestic_Currency[Total_Net_Wgt > 0,
                                                         list(Comtrade_Catch = sum(Total_Net_Wgt,na.rm = TRUE)/1000,
                                                              Comtrade_Value = sum(Domestic_Currency_PrimaryValue,na.rm = TRUE)/1000000,
                                                              Implicit_Price_USD2019 = sum(Domestic_Currency_PrimaryValue,na.rm = TRUE) / (sum(Total_Net_Wgt,na.rm = TRUE)/1000)),
                                                          by = .(Year = Year,
                                                                 Measure)]
      
      Comtrade_Imports <- Comtrade_Imports[,c("Year","Measure","Comtrade_Catch","Comtrade_Value")]
      names(Comtrade_Imports)[3:4] <- c("Comtrade_Import_Catch","Comtrade_Import_Value")
      
   ##
   ##    How well does the FFA and FAO metrics line up?
   ##
   ##       Looks pretty good.
   ##
   ##
      FAO_FFA_Comparison <- merge(FAO_Core_Tuna,
                                  FFA_Core_Tuna,
                                  by = c("Measure","Year"),
                                  all= TRUE)
      FAO_FFA_Comparison <- merge(FAO_FFA_Comparison,
                                  Comtrade_Imports,
                                  by = c("Measure","Year"),
                                  all= FALSE)

      FAO_FFA_Comparison <- data.table::melt(FAO_FFA_Comparison,
                                             id.vars = c("Measure","Year"),
                                             measure.vars = c("FAO_Catch_Volume","Volume","Comtrade_Import_Catch"))
      FAO_FFA_Comparison$variable <- ifelse(as.character(FAO_FFA_Comparison$variable) == "Volume", "FFA_Volume", as.character(FAO_FFA_Comparison$variable))


      showtext_auto()

      ggplot(FAO_FFA_Comparison[!(FAO_FFA_Comparison$Measure %in% c("Tuna NEC", "Bluefin")),], 
#      ggplot(FAO_FFA_Comparison[variable != "Value"], 
             aes(x = Year, 
                 y = value/1000,
                 colour = variable))  + 
             geom_line() +
             geom_point(alpha = 0.1) +
             facet_wrap( ~ Measure,scales = "free") +
             
             scale_colour_manual(values = SPCColours()) + 
             labs(title = "Comparison in Tuna Catch Volumes (FFA vs FAO vs Comtrade)",
                  subtitle = "\nWestern and Central Pacific Fisheries Commission Area\n",
                  caption  = "Data Sources: FFA, WCPFC-CA tuna fisheries 2023 (1).xlsx, https://www.ffa.int/download/wcpfc-area-catch-value-estimates/ \nUNFAO https://zenodo.org/records/11410529\nUN Comtrade https://comtradeplus.un.org/") +
             ylab("Metrics Tonnes\n(000)") +
             xlab("Year") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,1,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 12, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(5, "mm"),
                   legend.spacing.y = unit(1, "mm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")
    ggsave("Graphical_Output/FAO_FFA_Comparison_With_Titles.png", height =(1.5*16.13), width = (1.5*20.66), dpi = 165, units = c("cm"))
    
    
      FAO_FFA_Comparison <- merge(FAO_Core_Tuna,
                                  FFA_Core_Tuna,
                                  by = c("Measure","Year"),
                                  all= TRUE)
      FAO_FFA_Comparison <- merge(FAO_FFA_Comparison,
                                  Comtrade_Imports,
                                  by = c("Measure","Year"),
                                  all= FALSE)

      FAO_FFA_Comparison <- data.table::melt(FAO_FFA_Comparison,
                                             id.vars = c("Measure","Year"),
                                             measure.vars = c("Value","Comtrade_Import_Value"))
      FAO_FFA_Comparison$variable <- ifelse(as.character(FAO_FFA_Comparison$variable) == "Value", "FFA_Value", as.character(FAO_FFA_Comparison$variable))
    
    
      ggplot(FAO_FFA_Comparison[!(FAO_FFA_Comparison$Measure %in% c("Tuna NEC", "Bluefin")),], 
             aes(x = Year, 
                 y = value,
                 colour = variable))  + 
             geom_line() +
             geom_point(alpha = 0.1) +
             facet_wrap( ~ Measure,scales = "free") +
             
             scale_colour_manual(values = SPCColours()) + 
             labs(title = "Comparison in Tuna Catch Values (FFA vs Comtrade)",
                  subtitle = "\nWestern and Central Pacific Fisheries Commission Area\n",
                  caption  = "Data Sources: FFA, WCPFC-CA tuna fisheries 2023 (1).xlsx, https://www.ffa.int/download/wcpfc-area-catch-value-estimates/ \nUN Comtrade https://comtradeplus.un.org/") +
             ylab("Metrics Tonnes\n(000)") +
             xlab("Year") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,1,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 12, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(5, "mm"),
                   legend.spacing.y = unit(1, "mm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")
                   
    ggsave("Graphical_Output/FAO_FFA_Comparison_Values.png", height =(1.5*16.13), width = (1.5*20.66), dpi = 165, units = c("cm"))
                   
                   