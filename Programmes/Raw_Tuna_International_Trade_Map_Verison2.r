##
##    Programme:  Raw_Tuna_International_Trade_Map.r
##
##    Objective:  I thought it would been mapped using from GVDR_Ports_Network.r developed by Guilio Dalla Riva in
##                American Samoa vessel analysis work, but on reflection, its actually a Sankey Chart.
##
##                Code derived from Peter Ellis here: https://www.r-bloggers.com/2025/06/sankey-plots-can-work-but-need-polishing-like-any-other-graphic-by-ellis2013nz/
##
##    Author:     James Hogan, 21 January 2026
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
      
      load('Data_Spatial/Countries.rda')
      load('Data_Output/Raw_Tuna_Imports_and_Exports.rda')
      
   ##
   ##    Get rid of the minnows
   ##
#      One_Year <- Canned_Tuna_Imports_and_Exports[Year == 2020]
      One_Year <- Raw_Tuna_Imports_and_Exports

      One_Year$reporter_desc <- ifelse(str_detect(One_Year$reporter_desc, "China"), "China", One_Year$reporter_desc)

      One_Year <- One_Year[,
                             list(Import = sum(Import,na.rm = TRUE),
                                  Export = sum(Export,na.rm = TRUE)),
                              by = .(reporter_desc,
                                     variable,
                                     Year)]
      Imports <- One_Year[order(One_Year$variable, One_Year$Year, -One_Year$Import), c("Year", "reporter_desc", "variable", "Import")]
      Exports <- One_Year[order(One_Year$variable, One_Year$Year, -One_Year$Export), c("Year", "reporter_desc", "variable", "Export")]

      Import_Exports <- lapply(unique(One_Year$Year), function(year)
                              {
                                 X <- Imports[Year == year]
                                 Y <- Exports[Year == year]
                                 
                                 ##
                                 ##    I've got two measures of size - Net wgt, and primary value. Which one to make dominant?
                                 ##       Lets go with weight
                                 ##
                                 X_Weight <- X[variable == "Total_Net_Wgt"]
                                 
                                 X_Weight$Cumulative_Total = X_Weight$Import[1]
                                 X_Weight$Cumulative_Proportion =  X_Weight$Import[1]/sum(X_Weight$Import, na.rm = TRUE)
                                 for(i in 2:nrow(X_Weight))
                                    {
                                       X_Weight$Cumulative_Total[i]      <- X_Weight$Import[i] + X_Weight$Cumulative_Total[(i-1)]
                                       X_Weight$Cumulative_Proportion[i] <- X_Weight$Cumulative_Total[i] /sum(X_Weight$Import, na.rm = TRUE)
                                    }
                                 X_Weight$reporter_desc <- ifelse(X_Weight$Cumulative_Proportion < 0.99, X_Weight$reporter_desc, "Minnows")
                                 
                                 ##
                                 ##    Map size mapping back to larger dataset
                                 ##
                                 X$reporter_desc <- ifelse(X$reporter_desc %in% X_Weight$reporter_desc, X$reporter_desc, "Minnows")
                                 
                                 
                                 ##
                                 ##    Do the same trick with exports :)
                                 ##
                                 Y_Weight <- Y[variable == "Total_Net_Wgt"]

                                 Y_Weight$Cumulative_Total = Y_Weight$Export[1]
                                 Y_Weight$Cumulative_Proportion = Y_Weight$Export[1]/sum(Y_Weight$Export, na.rm = TRUE)
                                 for(i in 2:nrow(Y_Weight))
                                    {
                                       Y_Weight$Cumulative_Total[i]      <- Y_Weight$Export[i] + Y_Weight$Cumulative_Total[(i-1)]
                                       Y_Weight$Cumulative_Proportion[i] <- Y_Weight$Cumulative_Total[i] /sum(Y_Weight$Export, na.rm = TRUE)
                                    }
                                 Y_Weight$reporter_desc <- ifelse(Y_Weight$Cumulative_Proportion < 0.99, Y_Weight$reporter_desc, "Minnows")

                                 ##
                                 ##    Map size mapping back to larger dataset
                                 ##
                                 Y$reporter_desc <- ifelse(Y$reporter_desc %in% Y_Weight$reporter_desc, Y$reporter_desc, "Minnows")

                                 X <- X[,
                                        list(Import = sum(Import,na.rm = TRUE)),
                                        by = .(reporter_desc,
                                               variable,
                                               Year)]
                                        
                                 Y <- Y[,
                                         list(Export = sum(Export,na.rm = TRUE)),
                                         by = .(reporter_desc,
                                                variable,
                                                Year)]
                                 return(list(X,Y))
                              })
     Imports <- data.frame()
     Exports <- data.frame()
     for(i in 1:length(Import_Exports)) Imports <- rbind(Imports, Import_Exports[[i]][[1]])
     for(i in 1:length(Import_Exports)) Exports <- rbind(Exports, Import_Exports[[i]][[2]])

   ##
   ##    Identify which is unbalanced
   ##
   for(i in unique(Imports$Year))
     {
       ##
       ##      Value imbalance
       ##
         if(sum(Imports$Import[(Imports$Year == i) & (Imports$variable == "Total_Primary_Value")]) < sum(Exports$Export[(Exports$Year == i) & (Exports$variable == "Total_Primary_Value")]))
         {
            Imports <- rbind(Imports,
                             data.frame(reporter_desc = "Missing Imports",
                                        Year = i,
                                        variable = "Total_Primary_Value",
                                        Import = sum(Exports$Export[(Exports$Year == i) & (Exports$variable == "Total_Primary_Value")]) - sum(Imports$Import[(Imports$Year == i) & (Imports$variable == "Total_Primary_Value")])))
         } else {
            Exports <- rbind(Exports,
                             data.frame(reporter_desc = "Missing Exports",
                                        Year = i,
                                        variable = "Total_Primary_Value",
                                        Export = sum(Imports$Import[(Imports$Year == i) & (Imports$variable == "Total_Primary_Value")]) - sum(Exports$Export[(Exports$Year == i) & (Exports$variable == "Total_Primary_Value")])))
         }
       ##
       ##      Weight imbalance
       ##
         if(sum(Imports$Import[(Imports$Year == i) & (Imports$variable == "Total_Net_Wgt")]) < sum(Exports$Export[(Exports$Year == i) & (Exports$variable == "Total_Net_Wgt")]))
         {
            Imports <- rbind(Imports,
                             data.frame(reporter_desc = "Missing Imports",
                                        Year = i,
                                        variable = "Total_Net_Wgt",
                                        Import = sum(Exports$Export[(Exports$Year == i) & (Exports$variable == "Total_Net_Wgt")]) - sum(Imports$Import[(Imports$Year == i) & (Imports$variable == "Total_Net_Wgt")])))
         } else {
            Exports <- rbind(Exports,
                             data.frame(reporter_desc = "Missing Exports",
                                        Year = i,
                                        variable = "Total_Net_Wgt",
                                        Export = sum(Imports$Import[(Imports$Year == i) & (Imports$variable == "Total_Net_Wgt")]) - sum(Exports$Export[(Exports$Year == i) & (Exports$variable == "Total_Net_Wgt")])))
         }
     }

   ##
   ##    Make the mappings
   ##
      
      nodes <- data.frame(name = unique(c(Imports$reporter_desc, as.character(Exports$reporter_desc), "Exports", "Imports")))
      nodes$id = row(nodes)-1
      
      From <- merge(Exports,
                    nodes,
                    by.x = "reporter_desc",
                    by.y = "name")
      names(From)[names(From) == 'id'] <- 'IDsource'
      From$IDtarget = nodes$id[nodes$name == "Exports"]
      
      To <- merge(Imports,
                  nodes,
                  by.x = "reporter_desc",
                  by.y = "name")
      names(To)[names(To) == 'id'] <- 'IDtarget'
      To$IDsource = nodes$id[nodes$name == "Imports"]

      names(From)[names(From) == "Export"] <- "value"
      names(To)  [names(To)   == "Import"] <- "value"


      Totals <- Imports[,
                       list(value = sum(Import,na.rm = TRUE)),
                        by = .(Year,
                               variable)]
                        
      Totals$reporter_desc = "Exports"
      Totals$IDsource = nodes$id[nodes$name == "Exports"]
      Totals$IDtarget = nodes$id[nodes$name == "Imports"]
                                                       
        
      links <- rbind(From, 
                     Totals,
                     To)

   ##
   ##    Make the Sankey
   ##

      p <- sankeyNetwork(Links = links[(Year == 2021) & (variable == "Total_Primary_Value")], 
                         Nodes = nodes,
                         Source = "IDsource", 
                         Target = "IDtarget",
                         Value = "value", 
                         NodeID = "name",
                         units = " $000",
                         fontSize = 20)
      p


   ##
   ##    Make some tables
   ##
      Export_Table <- data.table::dcast(Exports,
                                        reporter_desc + Year ~variable ,
                                        value.var = "Export")
      Export_Table$Average_Price <- Export_Table$Total_Primary_Value / Export_Table$Total_Net_Wgt 
      Export_Table <- data.table::melt(Export_Table,
                                       id.var = c("reporter_desc", "Year"))
      Export_Table <- data.table::dcast(Export_Table,
                                        reporter_desc + variable ~ Year,
                                        value.var = "value")
                                        
                                        
                                        
      Import_Table <- data.table::dcast(Imports,
                                        reporter_desc + Year ~variable ,
                                        value.var = "Import")
      Import_Table$Average_Price <- Import_Table$Total_Primary_Value / Import_Table$Total_Net_Wgt 
      Import_Table <- data.table::melt(Import_Table,
                                       id.var = c("reporter_desc", "Year"))
      Import_Table <- data.table::dcast(Import_Table,
                                        reporter_desc + variable ~ Year,
                                        value.var = "value")
                                        
##
##    And Save
##
   Export_Raw_Tuna <- Export_Table                          
   Import_Raw_Tuna <- Import_Table                          
   
   save(Export_Raw_Tuna, file = "Data_Output/Export_Raw_Tuna.rda")
   save(Import_Raw_Tuna, file = "Data_Output/Import_Raw_Tuna.rda")

   