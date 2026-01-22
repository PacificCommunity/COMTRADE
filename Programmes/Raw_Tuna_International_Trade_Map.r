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
                                     Year)]

     
      Imports <- One_Year[order(One_Year$Year, -One_Year$Import), c("Year", "reporter_desc", "Import")]
      Exports <- One_Year[order(One_Year$Year, -One_Year$Export), c("Year", "reporter_desc", "Export")]

      Import_Exports <- lapply(unique(One_Year$Year), function(year)
                              {
                                 X <- Imports[Year == year]
                                 Y <- Exports[Year == year]
                                 
                                 X$Cumulative_Total = X$Import[1]
                                 X$Cumulative_Proportion =  X$Import[1]/sum(X$Import, na.rm = TRUE)
                                 for(i in 2:nrow(X))
                                    {
                                       X$Cumulative_Total[i]      <- X$Import[i] + X$Cumulative_Total[(i-1)]
                                       X$Cumulative_Proportion[i] <- X$Cumulative_Total[i] /sum(X$Import, na.rm = TRUE)
                                    }
                                 X$reporter_desc <- ifelse(X$Cumulative_Proportion < 0.95, X$reporter_desc, "Minnows")
                                 
                                 
                                 Y$Cumulative_Total = Y$Export[1]
                                 Y$Cumulative_Proportion = Y$Export[1]/sum(Y$Export, na.rm = TRUE)
                                 for(i in 2:nrow(Y))
                                    {
                                       Y$Cumulative_Total[i]      <- Y$Export[i] + Y$Cumulative_Total[(i-1)]
                                       Y$Cumulative_Proportion[i] <- Y$Cumulative_Total[i] /sum(Y$Export, na.rm = TRUE)
                                    }
                                 Y$reporter_desc <- ifelse(Y$Cumulative_Proportion < 0.95, Y$reporter_desc, "Minnows")

                                 X <- X[,
                                        list(Import = sum(Import,na.rm = TRUE)),
                                        by = .(reporter_desc,
                                               Year)]
                                        
                                 Y <- Y[,
                                         list(Export = sum(Export,na.rm = TRUE)),
                                         by = .(reporter_desc,
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
         if(sum(Imports$Import[Imports$Year == i]) < sum(Exports$Export[Exports$Year == i]))
         {
            Imports <- rbind(Imports,
                             data.frame(reporter_desc = "Missing Imports",
                                        Year = i,
                                        Import = sum(Exports$Export[Exports$Year == i]) - sum(Imports$Import[Imports$Year == i])))
         } else {
            Exports <- rbind(Exports,
                             data.frame(reporter_desc = "Missing Exports",
                                        Year = i,
                                        Export = sum(Imports$Import[Imports$Year == i]) - sum(Exports$Export[Exports$Year == i])))
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
                        by = .(Year)]
                        
      Totals$reporter_desc = "Exports"
      Totals$IDsource = nodes$id[nodes$name == "Exports"]
      Totals$IDtarget = nodes$id[nodes$name == "Imports"]
                                                       
        
      links <- rbind(From, 
                     Totals,
                     To)

   ##
   ##    Make the Sankey
   ##

      p <- sankeyNetwork(Links = links[Year == 2021], 
                         Nodes = nodes,
                         Source = "IDsource", 
                         Target = "IDtarget",
                         Value = "value", 
                         NodeID = "name",
                         units = "Tonnes",
                         fontSize = 20)
      p


   ##
   ##    Make some tables
   ##
      Export_Table <- data.table::dcast(Exports,
                                        reporter_desc ~ Year,
                                        value.var = "Export")
      Import_Table <- data.table::dcast(Imports,
                                        reporter_desc ~ Year,
                                        value.var = "Import")
