##
##    Programme:  Tuna_Supply_Chains.r
##
##    Objective:  Lets interogate the tuna supply chain data more closely
##
##    Author:     James Hogan, FAME - SPC, 23 January 2026
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
   ##
   ##    Load data from somewhere
   ##
      load("Data_Output/Import_Raw_Tuna.rda")
      load("Data_Output/Export_Raw_Tuna.rda")
      load("Data_Output/Export_Tinned_Tuna.rda")
      load("Data_Output/Import_Tinned_Tuna.rda")

      Everyone <- rbind(Import_Raw_Tuna, Export_Raw_Tuna, Import_Tinned_Tuna, Export_Tinned_Tuna)
      Everyone$Year <- as.numeric(as.character(Everyone$Year))
      Everyone <- data.table::dcast(Everyone,
                                    reporter_desc + Year + Source ~ variable ,
                                    value.var = "value")
   ##
   ##    Estimate world prices?
   ##
      World_Prices <- Everyone[,
                                list(Total_Net_Wgt = sum(Total_Net_Wgt,na.rm = TRUE),
                                     Total_Primary_Value = sum(Total_Primary_Value,na.rm = TRUE)),
                                 by = .(Year,
                                        Source)]
                                        
      World_Prices$World_Price <- World_Prices$Total_Primary_Value / World_Prices$Total_Net_Wgt
      World_Prices <- data.table::dcast(World_Prices,
                                        Year ~ Source,
                                        value.var = "World_Price")

      ##
      ##    Raw Exports perspective
      ##
         Raw_Export_Price_Base <- merge(Everyone[Source == "Raw Exports"],
                                        World_Prices,
                                        by = c("Year"))
         names(Raw_Export_Price_Base) <- str_replace_all(names(Raw_Export_Price_Base), " ", "_")
                                        
         Raw_Export_Price_Base$PRICESRaw_Export_Rel_Raw_Export <- Raw_Export_Price_Base$Average_Price / Raw_Export_Price_Base$Raw_Exports
         Raw_Export_Price_Base$PRICESRaw_Export_Rel_Raw_Import <- Raw_Export_Price_Base$Average_Price / Raw_Export_Price_Base$Raw_Imports
         Raw_Export_Price_Base$PRICESRaw_Export_Rel_Tinned_Export <- Raw_Export_Price_Base$Average_Price / Raw_Export_Price_Base$Tinned_Exports
         Raw_Export_Price_Base$PRICESRaw_Export_Rel_Tinned_Import <- Raw_Export_Price_Base$Average_Price / Raw_Export_Price_Base$Tinned_Imports
                                     
      ##
      ##    Raw Imports perspective
      ##
         Raw_Import_Price_Base <- merge(Everyone[Source == "Raw Imports"],
                                        World_Prices,
                                        by = c("Year"))
         names(Raw_Import_Price_Base) <- str_replace_all(names(Raw_Import_Price_Base), " ", "_")
                                        
         Raw_Import_Price_Base$PRICESRaw_Import_Rel_Raw_Export <- Raw_Import_Price_Base$Average_Price / Raw_Import_Price_Base$Raw_Exports
         Raw_Import_Price_Base$PRICESRaw_Import_Rel_Raw_Import <- Raw_Import_Price_Base$Average_Price / Raw_Import_Price_Base$Raw_Imports
         Raw_Import_Price_Base$PRICESRaw_Import_Rel_Tinned_Export <- Raw_Import_Price_Base$Average_Price / Raw_Import_Price_Base$Tinned_Exports
         Raw_Import_Price_Base$PRICESRaw_Import_Rel_Tinned_Import <- Raw_Import_Price_Base$Average_Price / Raw_Import_Price_Base$Tinned_Imports
                                        
      ##
      ##    Tinned Exports perspective
      ##
         Tinned_Export_Price_Base <- merge(Everyone[Source == "Tinned Exports"],
                                           World_Prices,
                                           by = c("Year"))
         names(Tinned_Export_Price_Base) <- str_replace_all(names(Tinned_Export_Price_Base), " ", "_")
                                        
         Tinned_Export_Price_Base$PRICESTinned_Exports_Rel_Raw_Export <- Tinned_Export_Price_Base$Average_Price / Tinned_Export_Price_Base$Raw_Exports
         Tinned_Export_Price_Base$PRICESTinned_Exports_Rel_Raw_Import <- Tinned_Export_Price_Base$Average_Price / Tinned_Export_Price_Base$Raw_Imports
         Tinned_Export_Price_Base$PRICESTinned_Exports_Rel_Tinned_Export <- Tinned_Export_Price_Base$Average_Price / Tinned_Export_Price_Base$Tinned_Exports
         Tinned_Export_Price_Base$PRICESTinned_Exports_Rel_Tinned_Import <- Tinned_Export_Price_Base$Average_Price / Tinned_Export_Price_Base$Tinned_Imports
                                        
      ##
      ##    Tinned Imports perspective
      ##
         Tinned_Import_Price_Base <- merge(Everyone[Source == "Tinned Imports"],
                                           World_Prices,
                                           by = c("Year"))
         names(Tinned_Import_Price_Base) <- str_replace_all(names(Tinned_Import_Price_Base), " ", "_")
                                        
         Tinned_Import_Price_Base$PRICESTinned_Imports_Rel_Raw_Export    <- Tinned_Import_Price_Base$Average_Price / Tinned_Import_Price_Base$Raw_Exports
         Tinned_Import_Price_Base$PRICESTinned_Imports_Rel_Raw_Import    <- Tinned_Import_Price_Base$Average_Price / Tinned_Import_Price_Base$Raw_Imports
         Tinned_Import_Price_Base$PRICESTinned_Imports_Rel_Tinned_Export <- Tinned_Import_Price_Base$Average_Price / Tinned_Import_Price_Base$Tinned_Exports
         Tinned_Import_Price_Base$PRICESTinned_Imports_Rel_Tinned_Import <- Tinned_Import_Price_Base$Average_Price / Tinned_Import_Price_Base$Tinned_Imports

      Everyone <- merge(Everyone,
                        World_Prices,
                        by = c("Year"))
      names(Everyone) <- str_replace_all(names(Everyone), " ", "_")
      Everyone$Rel_Raw_Export <- Everyone$Average_Price / Everyone$Raw_Exports
      Everyone$Rel_Raw_Import <- Everyone$Average_Price / Everyone$Raw_Imports
      Everyone$Rel_Tinned_Export <- Everyone$Average_Price / Everyone$Tinned_Exports
      Everyone$Rel_Tinned_Import <- Everyone$Average_Price / Everyone$Tinned_Imports
        
   ##
   ##    Own price elasticities - Price elasticity should be negative
   ##
      ##
      ##    Raw Imports
      ##
      
         Raw_Imports_Analytical <- merge(Raw_Import_Price_Base,
                                         Tinned_Import_Price_Base[,c("Year", "reporter_desc", "PRICESTinned_Imports_Rel_Raw_Import")],
                                         by = c("Year", "reporter_desc"))
      
         Raw_Imports_Analytical <- Raw_Imports_Analytical[!is.na(PRICESRaw_Import_Rel_Raw_Import) & !is.na(PRICESTinned_Imports_Rel_Raw_Import)]
      
#         Raw_Imports_OwnPrice    <- lm(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import) + log(PRICESTinned_Imports_Rel_Raw_Import) + lag(PRICESTinned_Imports_Rel_Raw_Import) +lag(PRICESRaw_Import_Rel_Raw_Import), 
         Raw_Imports_OwnPrice    <- lm(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import) + log(PRICESTinned_Imports_Rel_Raw_Import), 
                                       data= Raw_Imports_Analytical)
         summary(Raw_Imports_OwnPrice)
         ##
         ##    Test for autocorrelation - yep
         ##
         acf(Raw_Imports_OwnPrice$residuals)
         pacf(Raw_Imports_OwnPrice$residuals)
         dwtest(Raw_Imports_OwnPrice)

         ##
         ##    Test for hetroskedasticity - yep
         ##
         bptest(Raw_Imports_OwnPrice)           
         ##
         ##    Test for structural break - nup
         ##
         sctest(Raw_Imports_OwnPrice)
         reset(Raw_Imports_OwnPrice)

         ##
         ##    Correct for autocorrelation & hetroskedasticity related to the country - now the coeffients are correctly signed and significant :) 
         ##
         
         GLS_OLS_1 <- gls(log(Total_Net_Wgt) ~ (log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import)), 
                        data=Raw_Imports_Analytical,
                        weights = varFunc(~ as.numeric(as.factor(reporter_desc))),
                        correlation = corAR1(form = ~ 1 | reporter_desc))
         summary(GLS_OLS_1)
         anova(Raw_Imports_OwnPrice, GLS_OLS_1)
         
         GLS_OLS_Estimates <- data.frame(Year = as.numeric(Raw_Imports_Analytical$Year),
                                         reporter_desc = Raw_Imports_Analytical$reporter_desc,
                                         Actual        = Raw_Imports_Analytical$Total_Net_Wgt/1000000,
                                         Estimates_GLS = as.numeric(exp(GLS_OLS_1$fitted))/1000000)
        ##
        ##  Third, lets try a system of equations.  The interactions between country and time are
        ##     captured in the system
        ##
            Analytical_Set <- pdata.frame(Raw_Imports_Analytical, index = c("reporter_desc", "Year"))
            Model <- log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import)
            ##
            ##    Grrr... unbalanced system - not going to work
            ##
            # System_Of_Equations <- systemfit(Model, "SUR",
                                             # data = Analytical_Set,
                                             # methodResidCov = "noDfCor",
                                             # residCovWeighted = TRUE )
            # summary(System_Of_Equations)
            
        ##
        ##  Finally, lets try a mixed-multilevel model.  This model assumes random coefficients that
        ##     vary by country.  
        ##
        ##
            
            Multi_Level_1 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import),
#            Multi_Level_1 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import) + lag(PRICESTinned_Imports_Rel_Raw_Import) +lag(PRICESRaw_Import_Rel_Raw_Import),
                                        data = Analytical_Set, 
                                        random = ~ (1) | reporter_desc)
                                        
            Multi_Level_2 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import),
#            Multi_Level_2 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import) + lag(PRICESTinned_Imports_Rel_Raw_Import) +lag(PRICESRaw_Import_Rel_Raw_Import),
                                        data = Analytical_Set, 
                                        random = ~ (1 + log(as.numeric(Year))) | reporter_desc)
                                        
            Multi_Level_3 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import),
#            Multi_Level_3 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import) + lag(PRICESTinned_Imports_Rel_Raw_Import) +lag(PRICESRaw_Import_Rel_Raw_Import),
                                        data = Analytical_Set, 
                                        random = ~ (1 + log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)) | reporter_desc)
                                        
            Multi_Level_4 <- lme(log(Total_Net_Wgt) ~ log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import),
                                        data = Analytical_Set, 
                                        random = ~ (1 + log(as.numeric(Year)) + log(PRICESRaw_Import_Rel_Raw_Import)+ log(PRICESTinned_Imports_Rel_Raw_Import)) | reporter_desc)

            anova(Multi_Level_1, Multi_Level_2)
            anova(Multi_Level_2, Multi_Level_3)
            anova(Multi_Level_3, Multi_Level_4)
            ##
            ##    Model 4's the best... lets take 2 because it its dynamics
            ##
            summary(Multi_Level_4)
            random.effects(Multi_Level_4)                       

        ##
        ##     Lets see how it worked
        ##
            Actual_Expected <- data.frame(Year = as.numeric(as.character(Analytical_Set$Year)),
                                          reporter_desc = str_replace_all(Analytical_Set$reporter_desc, "\\.", " "),
                                          Total_Net_Wgt = as.numeric(Analytical_Set$Total_Net_Wgt)/1000000,
                                          Estimates_MultiLevel = as.numeric(exp(Multi_Level_4$fitted[,2]))/1000000)
                                          
            Actual_Expected <- merge(Actual_Expected,
                                     GLS_OLS_Estimates,
                                     by = c("Year", "reporter_desc"))
                               
            Actual_Expected <- reshape2::melt(Actual_Expected,
                                    id.vars = c("Year", "reporter_desc"),
                                    measure.vars = c("Total_Net_Wgt", "Estimates_MultiLevel", "Estimates_GLS"))

            showtext_begin()
#            ggplot(Actual_Expected, aes(x=Year, y=value, colour=variable))     +
            ggplot(Actual_Expected[Actual_Expected$variable %in% c("Total_Net_Wgt", "Estimates_MultiLevel"),], aes(x=Year, y=value, colour=variable))     +
                   geom_line(linewidth =.5) +
                   geom_point(linewidth =.3) +
                   facet_wrap(~reporter_desc, scales="free") +
                   labs(title="World Raw Tuna Demand\nMultilevel Model\n") +
                   ylab("Demand for Raw Tuna\n mt") +
                   scale_colour_manual(values = SPCColours(1:3), name="Actual or Expected") +
                   scale_x_continuous(breaks = 2000:2025) + 
                   xlab("Time Period\n") +
                   theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                   theme(legend.title.align=0.5,
                         legend.text   = element_text(family = "MyriadPro-Regular"),
                         plot.title    = element_text(colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                         plot.subtitle = element_text(colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                         plot.caption  = element_text(colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                         plot.tag      = element_text(colour = SPCColours("Red")),
                         axis.title    = element_text(colour = SPCColours("Dark_Blue")),
                         axis.text.x   = element_text(colour = SPCColours("Dark_Blue"), angle = 90,size = 8),
                         axis.text.y   = element_text(colour = SPCColours("Dark_Blue"), angle = 00),
                         legend.key.width = unit(1, "cm"),
                         legend.spacing.y = unit(1, "cm"),
                         legend.margin = margin(10, 10, 10, 10),
                         legend.position  = "bottom", 
                         plot.margin = unit(c(1,3,1,1),"mm"),
                         panel.border = element_blank(),
                         strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                         strip.text = element_text(colour = "white", 
                                                   size   = 8,
                                                   family = "MyriadPro-Bold",
                                                   margin = margin(1.0,1.0,1.0,1.0, unit = "mm")),
                         panel.spacing = unit(1, "lines"))
            showtext_end()
         ggsave("Graphical_Output/World Raw Tuna Demand.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))
             
        ##
        ##     Aggregate it up
        ##
        Actual_Expected <- data.table(Actual_Expected)
         Raw_Tuna_Demand <- Actual_Expected[,
                                            list(Total_Net_Wgt = sum(value,na.rm = TRUE)),
                                             by = .(Year,
                                                    variable)]
                                                    
         showtext_begin()
         ggplot(Raw_Tuna_Demand[Raw_Tuna_Demand$variable %in% c("Total_Net_Wgt", "Estimates_MultiLevel"),], aes(x=Year, y=Total_Net_Wgt, colour=variable))     +
                geom_line(linewidth =1) +
                geom_point(linewidth =.5) +
                labs(title="World Raw Tuna Demand\nMultilevel Model\n") +
                ylab("Demand for Raw Tuna\n mt") +
                scale_x_continuous(breaks = 2000:2025) + 
                scale_colour_manual(values = SPCColours(1:3), name="Actual or Expected") +
                xlab("Time Period\n") +
                theme_bw(base_size=12, base_family =  "Calibri") %+replace%
                theme(legend.title.align=0.5,
                      legend.text   = element_text(family = "MyriadPro-Regular"),
                      plot.title    = element_text(colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Bold"),
                      plot.subtitle = element_text(colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                      plot.caption  = element_text(colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                      plot.tag      = element_text(colour = SPCColours("Red")),
                      axis.title    = element_text(colour = SPCColours("Dark_Blue")),
                      axis.text.x   = element_text(colour = SPCColours("Dark_Blue"), angle = 90),
                      axis.text.y   = element_text(colour = SPCColours("Dark_Blue"), angle = 00),
                      legend.key.width = unit(1, "cm"),
                      legend.spacing.y = unit(1, "cm"),
                      legend.margin = margin(10, 10, 10, 10),
                      legend.position  = "bottom", 
                      plot.margin = unit(c(1,3,1,1),"mm"),
                      panel.border = element_blank(),
                      strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                      strip.text = element_text(colour = "white", 
                                                size   = 8,
                                                family = "MyriadPro-Bold",
                                                margin = margin(1.0,1.0,1.0,1.0, unit = "mm")),
                      panel.spacing = unit(1, "lines"))
         showtext_end()
         ggsave("Graphical_Output/World Raw Tuna Aggregate Demand.png", height =(1.5)*16.13, width = (1.75)*20.66, dpi = 165, units = c("cm"))
                                        
##
##    And we're done
##
