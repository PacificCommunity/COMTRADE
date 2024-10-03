##
##    Programme:  FFA_Analytics.r
##
##    Objective:  Lets have a look at the FFA's data. This is a bit of an experiment and a play.
##                The upshot is I'm not seeing a lot of strong price / volume relationship.
##
##                There's weak evidence that Skipjack and Bigeye share relative price/quantity dynamics,
##                but there's a lot of noise in the data. There's a lot of variability around the underlying
##                trend that I'm not willing to bet the ranch on a strong relationship existing.
##
##                Secondly, there's some econometric evidence that suggests volumes are strongly autocorrelated,
##                but negatively affected by price inflation. If prices are increasing(decreasing), then volumes
##                are falling (increasing). Causality might suggested the volume increases (decreases) are "causing" the 
##                price falls (increases). Maybe testing for Granger Causality might help identify which happens first:
##                price changes then volume changes, or volume changes then price changes.
##
##                1. Could be a poor match between the price data used by FFA, and the price information fishers consider 
##                   important for their catch effort:
##                   These might be the "wrong" prices for fishermen setting their effort:
##                 
##                2. Might be misspecification of how prices effect behaviour. 
##                   I might be missing lags, or price changes (differences between time). For example, catch effort this 
##                   year might be responding to last year's prices. Or last year's change in prices. 
##                  
##                3. Other factors might be effecting the relationship between prices and quantities. 
##                   a. Covid had a big effect which might have created a structural break in the statistical relatiopnship.
##                   b. Costs are missing from the data. They might be more important than output prices.
##                   c. Customer incomes are missing from the data.
##                   d. External weather events, like El Nino, and La Nina might be effecting the catch volumes.
##                 
##                4. Lack of a good theoretical model.
##                   How prices effect effort, which prices, and how other variables enter into the relationship is unclear. 
##                   As a result, these are just relationships in the data, and running regressions. A "better" econometric 
##                   model is needed to be able to add interpretation and correctly specify the statistical model.
##
##    Author:     James Hogan, FAME - SPC, 19 September 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions and colour palattes
   ##
      source("R/functions.r")
      source("R/themes.r")

   ##
   ##    Grab the FFA data
   ##
      load('Data_Output/FFASummaryData.rda')

   ##
   ##    Estimate the value of the tuna fish extracted
   ##
      Catch_Value  <- FFASummaryData[["Summary of catch value"]]
      Catch_Volume <- FFASummaryData[["Summary of catch"]]

      Catch_Value  <- data.table(Catch_Value[Catch_Value$Spreadsheet == "WCPFC-CA_tuna_fisheries_2023",])
      Catch_Volume <- data.table(Catch_Volume[Catch_Volume$Spreadsheet == "WCPFC-CA_tuna_fisheries_2023",])

      Value_by_Species  <- Catch_Value[Catch_Value$Data_Row == "5.2 VALUE OF CATCH BY SPECIES",c("Measure","Year","value")]
      Volume_by_Species <- Catch_Volume[Catch_Volume$Data_Row == "4.2 CATCH BY SPECIES",c("Measure","Year","value")]
      
      Value_by_Species$Value   <- Value_by_Species$value
      Volume_by_Species$Volume <- Volume_by_Species$value
      
      Summary_Values <-merge(Value_by_Species[,c("Measure","Year","Value")],
                             Volume_by_Species[,c("Measure","Year","Volume")],
                             by = c("Measure","Year"))
      Summary_Values$Average_Price_PerTonne <- (Summary_Values$Value*1000000) / Summary_Values$Volume  

   
      ggplot(Summary_Values, 
             aes(x = Volume/1000, 
                 y = Average_Price_PerTonne,
                 colour = Measure))  + 
             geom_path() +
             geom_point(alpha = 1) +
             facet_wrap( ~ Measure,scales = "free"  ) +
             
#             scale_y_continuous(breaks = seq(from = 0, to = 6000, by =1000),
#                                labels = scales::label_dollar()) +
#             scale_x_continuous(breaks = seq(from = 0, to = 2500, by =100),
#                                labels = scales::label_comma()) +
             scale_colour_manual(values = SPCColours(3:7)) + 
             labs(title = "Relationship between Tuna Prices and Quantities\n",
                  caption  = "Data Source: FFA, WCPFC-CA tuna fisheries 2023 (1).xlsx, https://www.ffa.int/download/wcpfc-area-catch-value-estimates/") +
             xlab("Metrics Tonnes\n(000)") +
             ylab("Average Price\nper Tonne") +
             geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")

   ##
   ##  Ratios? Relative prices and relative quantities?
   ##
   ##       The theory here is its relative prices that effect relative quantities, not the absolute prices of fish.
   ##       So, for example, if Albacore market prices increase relative to Skipjack market prices, then I would expect Albacore 
   ##       catch quantites to increase relative to Skipjack catch quantities. When the Albacore/Skipjack prices and quantities are plotted together,  
   ##       then high relative prices (high X values), should systematic plot against high relative quantities (high y values), and 
   ##       low relative prices (low X values) should plot against low Y values. The underlying trend line should slope upward.
   ##
   ##       If Skipjack prices are increasing while Albacore do not, then the Albacore/Skipjack prices and quantities ratio means the increasing
   ##       Skipjack price makes the relative Albacore/Skipjack ratio fall. And the higher Skipjack price makes the Albacore/Skipjack quantity
   ##       also fall - again, making an upward sloping line.
   ##
      Relative_Values <- data.table::melt(Summary_Values,
                                        id.var = c("Measure","Year"), 
                                        measure.var = c("Value","Volume","Average_Price_PerTonne"))

      Relative_Values <- data.table::dcast(Relative_Values,
                                        Year + variable ~ Measure)
                                        
      Relative_Values$Albacore_Bigeye    <- Relative_Values$Albacore / Relative_Values$Bigeye
      Relative_Values$Albacore_Skipjack  <- Relative_Values$Albacore / Relative_Values$Skipjack
      Relative_Values$Albacore_Yellowfin <- Relative_Values$Albacore / Relative_Values$Yellowfin

      Relative_Values$Bigeye_Skipjack    <- Relative_Values$Bigeye / Relative_Values$Skipjack
      Relative_Values$Bigeye_Yellowfin   <- Relative_Values$Bigeye / Relative_Values$Yellowfin
      Relative_Values$Skipjack_Yellowfin <- Relative_Values$Skipjack / Relative_Values$Yellowfin

      Relative_Values <- data.table::melt(Relative_Values,
                                          id.var = c("variable","Year"), 
                                          variable.name = "Measure",
                                          measure.var = c("Albacore_Bigeye","Albacore_Skipjack","Albacore_Yellowfin","Bigeye_Skipjack","Bigeye_Yellowfin","Skipjack_Yellowfin"))

      Relative_Values <- data.table::dcast(Relative_Values,
                                           Year + Measure ~ variable)

   ##
   ## Nope - nothing "systematic" looks like its going on here... maybe the relationship between Albacore and Yellowfin, 
   ##    but I feel like I'm drawing a long bow...
   ##
   ##    I've got negatively sloping lines for Albacore/Bigeye and Albacore/Yellowfin. This means, as the price of Albacore
   ##    increases, relative to Bigeye/Yellowfin, then the catch of Albacore falls and more Yellowfin/Bigeye is caught?
   ##    If the price of Albacore falls relative to Yellowfin, then more Albacore is caught? ... or did the catching of 
   ##    more Albacore cause it relative price compared to Yellowfin to fall?
   ##
   ##    Think about this some more...
   ##
      ggplot(Relative_Values, aes(y = Volume, x = Average_Price_PerTonne)) +
          geom_smooth(se = FALSE) +
          geom_point(alpha = .2) +
          geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") +
          scale_y_continuous() +
          facet_wrap( ~ Measure,scales = "free"  ) +
          labs(title="Relative Prices and Relative quantities") +
          xlab("Relative Price\n") +
          ylab("Relative Quantities\n") +
          theme( plot.title  = element_text(size = 14), 
                 axis.text.x = element_text(angle=90, vjust=0.5, size=9),
                 legend.position="bottom",
                 strip.text  = element_text(angle=00, vjust=0.5, size=10))                


      # AY <- Relative_Values
      # AY$Match_Year <- AY$Year + 1
      # AY <- merge(AY,
                  # AY,
                  # by.x = c("Measure", "Year"),
                  # by.y = c("Measure", "Match_Year"))
      # ggplot(AY, aes(y = Volume.y, x = Average_Price_PerTonne.x)) +
          # geom_smooth(se = FALSE) +
          # geom_point(alpha = .2) +
          # geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") +
          # facet_wrap( ~ Measure,scales = "free"  ) +
          # scale_y_continuous() +
          # labs(title="Relative Prices and Relative quantities") +
          # xlab("This Years Relative Price\n") +
          # ylab("Last Years Relative Quantities\n") +
          # theme( plot.title  = element_text(size = 14), 
                 # axis.text.x = element_text(angle=90, vjust=0.5, size=9),
                 # legend.position="bottom",
                 # strip.text  = element_text(angle=00, vjust=0.5, size=10))                


   ##
   ##    Turn these into an index
   ##
      Index_Values <- merge(Summary_Values,
                            Summary_Values[Summary_Values$Year == min(Summary_Values$Year), c("Measure","Value","Volume","Average_Price_PerTonne")],
                            by = c("Measure"))
      Index_Values$Price_Index  <- Index_Values$Average_Price_PerTonne.x / Index_Values$Average_Price_PerTonne.y                      
      Index_Values$Volume_Index <- Index_Values$Value.x / Index_Values$Value.y                      
      Index_Values$Value_Index  <- Index_Values$Volume.x / Index_Values$Volume.y                      


      ggplot(Index_Values, 
             aes(x = Volume_Index, 
                 y = Price_Index,
                 colour = Measure))  + 
             #geom_path(alpha = .2) +
             geom_smooth(se = FALSE) +
             geom_point(alpha = .2) +
             
             scale_colour_manual(values = SPCColours(3:7)) + 
             labs(title = "Tuna Catch Volumes and Average Prices",
                  caption  = "Data Source: FFA https://zenodo.org/records/11410529") +
             xlab("Metrics Tonnes\n(000)") +
             ylab("Average Price\nper Tonne") +
             geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,3,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 13,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,1.25,1.25,1.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 10, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 24, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 14, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 10,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 14, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 10, r = 0,  b = 0, l = 0, unit = "pt"),hjust = 0.5),
                   axis.text.y   = element_text(size = 14, colour = SPCColours("Dark_Blue"), angle = 00, margin = margin(t = 0,  r = 10, b = 0, l = 0, unit = "pt"),hjust = 1.0),
                   legend.key.width = unit(1, "cm"),
                   legend.spacing.y = unit(1, "cm"),
                   legend.margin = margin(10, 10, 10, 10),
                   legend.position  = "bottom")



   ##
   ##    What does this dynamic picture look like?
   ##
   ##       All over the place
   ##
   ##
      Comparative_Prices <- reshape2::melt(Index_Values,
                                           id.var = c("Year","Measure"),
                                           measure.var = c("Price_Index","Volume_Index","Value_Index"))
      Comparative_Prices <- reshape2::dcast(Comparative_Prices,
                                           Year + variable ~ Measure)

      ggplot(Comparative_Prices[Comparative_Prices$variable == "Price_Index",], 
             aes(x = Yellowfin, 
                 y = Bigeye))  + 
             geom_path(alpha = .2) +
             geom_abline(intercept = 0, slope = 1) +
             #geom_smooth(se = FALSE) +
             scale_y_continuous(breaks = seq(from = 0.0, to = 2, by =.1), limits = c(0.5,2)) +
             scale_x_continuous(breaks = seq(from = 0.0, to = 2, by =.1), limits = c(0.5,2)) +
             geom_point(alpha = .2) +
             geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") 

      ggplot(Comparative_Prices[Comparative_Prices$variable == "Price_Index",], 
             aes(x = Skipjack, 
                 y = Albacore))  + 
             geom_path(alpha = .2) +
             geom_abline(intercept = 0, slope = 1) +
             scale_y_continuous(breaks = seq(from = 0.0, to = 2, by =.1), limits = c(0.5,2)) +
             scale_x_continuous(breaks = seq(from = 0.0, to = 2, by =.1), limits = c(0.5,2)) +
             #geom_smooth(se = FALSE) +
             geom_point(alpha = .2) +
             geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") 

      ggplot(Comparative_Prices[Comparative_Prices$variable == "Price_Index",], 
             aes(x = Skipjack, 
                 y = Bigeye))  + 
             geom_path(alpha = .2) +
             geom_abline(intercept = 0, slope = 1) +
             scale_y_continuous(breaks = seq(from = 0.0, to = 2, by =.1), limits = c(0.5,1.8)) +
             scale_x_continuous(breaks = seq(from = 0.0, to = 2, by =.1), limits = c(0.5,1.8)) +
             #geom_smooth(se = FALSE) +
             geom_point(alpha = .2) +
             geom_text_repel(aes(label=Year), size=2,segment.size = 0.2, segment.colour ="red") 
    


      OLS_Regression_LogLevels                     <- Index_Values
      OLS_Regression_LogLevels$Year                <- as.Date(paste0("01/01/",OLS_Regression_LogLevels$Year), "%d/%m/%Y")
      OLS_Regression_LogLevels$Volume              <- log(OLS_Regression_LogLevels$Volume.x)
      OLS_Regression_LogLevels$Average_Price_PerTonne    <- log(OLS_Regression_LogLevels$Average_Price_PerTonne.x)
               
      OLS_Regression_LogLevels <- pdata.frame(OLS_Regression_LogLevels,
                                              index = c("Measure","Year"))

   head(lag(OLS_Regression_LogLevels$Volume, 0:2), 20)
   head(diff(OLS_Regression_LogLevels$Volume, 0:2), 20)            

               
      OLS <- plm(Volume ~ lag(Volume) + diff(Average_Price_PerTonne,1), 
   #   OLS <- plm(Volume ~ lag(Volume) + Average_Price_PerTonne + diff(Average_Price_PerTonne,0:1), 
   #   OLS <- plm(Volume ~ Average_Price_PerTonne + lag(Average_Price_PerTonne,0:3), 
                 effect = c("twoways"),
                 model  = c("within"),
                 data = OLS_Regression_LogLevels)                   
      summary(OLS)


      Residuals <- data.frame(OLS$residuals)
      Residuals$Year <- as.Date(str_split_fixed(row.names(Residuals), "-",2)[,2], "%Y-%m-%d")
      Residuals$Measure <- str_split_fixed(row.names(Residuals), "-",2)[,1]
      
      Results <- merge(OLS_Regression_LogLevels,
                       Residuals,
                       by = c("Year", "Measure"))
                       
      Results$Actual_Volume <- Results$Volume
      Results$Estimated_Volume <- Results$Volume - Results$OLS.residuals
      Results$Residuals <- Results$OLS.residuals
      
      Results$Actual_Volume    <- round(exp(Results$Actual_Volume), digits=2)                         
      Results$Estimated_Volume <- round(exp(Results$Estimated_Volume), digits=2)                         
      Results$Residual         <- round((Results$Actual_Volume - Results$Estimated_Volume), digits=2)                         

      Results <- Results[order(Results$Measure, Results$Year),]
            
      Plot_Me <- reshape2::melt(Results,
                                id.vars = c("Year", "Measure"),
                                measure.vars = c("Actual_Volume", "Estimated_Volume"),
                                factorsAsStrings = FALSE)
      Plot_Me$Year <- as.Date(as.character(Plot_Me$Year), "%Y-%m-%d")         
      
      
      ggplot(Plot_Me, aes(x = Year, y = as.numeric(value), colour = variable)) +
          geom_line(size=.3) +
          scale_y_continuous() +
          facet_wrap( ~ Measure,scales = "free"  ) +
          labs(title="Actual and Estimated Catch Volumes\n") +
          ylab("Catch Volumes\n") +
          theme( plot.title  = element_text(size = 14), 
                 axis.text.x = element_text(angle=90, vjust=0.5, size=9),
                 legend.position="bottom",
                 strip.text  = element_text(angle=00, vjust=0.5, size=10))                



      OLS <- plm(diff(Volume) ~  lag(Volume) + diff(Average_Price_PerTonne), 
                 effect = c("twoways"),
                 model  = c("within"),
                 data = OLS_Regression_LogLevels)                   
      summary(OLS)


##
##    And we're done
##


