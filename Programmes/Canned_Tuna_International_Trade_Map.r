##
##    Programme:  Canned_Tuna_International_Trade_Map.r
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
      load('Data_Output/Canned_Tuna_Imports_and_Exports.rda')
      
   ##
   ##    Get rid of the minnows
   ##
      One_Year <- Canned_Tuna_Imports_and_Exports[Year == 2020]

      One_Year$reporter_desc <- ifelse(str_detect(One_Year$reporter_desc, "China"), "China", One_Year$reporter_desc)

      One_Year <- One_Year[,
                             list(Import = sum(Import,na.rm = TRUE),
                                  Export = sum(Export,na.rm = TRUE)),
                              by = .(reporter_desc)]

     
      Imports <- One_Year[order(-One_Year$Import), c("reporter_desc", "Import")]
      Exports <- One_Year[order(-One_Year$Export), c("reporter_desc", "Export")]

      Imports$Cumulative_Total = Imports$Import[1]
      Imports$Cumulative_Proportion = 0
      for(i in 2:nrow(Imports))
         {
            Imports$Cumulative_Total[i]      <- Imports$Import[i] + Imports$Cumulative_Total[(i-1)]
            Imports$Cumulative_Proportion[i] <- Imports$Cumulative_Total[i] /sum(Imports$Import, na.rm = TRUE)
         }
      Imports$reporter_desc <- ifelse(Imports$Cumulative_Proportion < 0.95, Imports$reporter_desc, "Minnows")

      Exports$Cumulative_Total = Exports$Export[1]
      Exports$Cumulative_Proportion = 0
      for(i in 2:nrow(Exports))
         {
            Exports$Cumulative_Total[i]      <- Exports$Export[i] + Exports$Cumulative_Total[(i-1)]
            Exports$Cumulative_Proportion[i] <- Exports$Cumulative_Total[i] /sum(Exports$Export, na.rm = TRUE)
         }
      Exports$reporter_desc <- ifelse(Exports$Cumulative_Proportion < 0.95 , Exports$reporter_desc, "Minnows")

      Imports <- Imports[,
                          list(Import = sum(Import,na.rm = TRUE)),
                          by = .(reporter_desc)]
      Exports <- Exports[,
                          list(Export = sum(Export,na.rm = TRUE)),
                          by = .(reporter_desc)]


   ##
   ##    Identify which is unbalanced
   ##
      if(sum(Imports$Import) < sum(Exports$Export))
      {
         Imports <- rbind(Imports,
                          data.frame(reporter_desc = "Missing Imports",
                                     Export = sum(Exports$Export) - sum(Imports$Import)))
      } else {
         Exports <- rbind(Exports,
                          data.frame(reporter_desc = "Missing Exports",
                                     Export = sum(Imports$Import) - sum(Exports$Export)))
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
        
      links <- rbind(From, 
                     data.frame(reporter_desc = "Exports",
                                value = sum(Imports$Import),
                                IDsource = nodes$id[nodes$name == "Exports"],
                                IDtarget = nodes$id[nodes$name == "Imports"]),
                     To)

   ##
   ##    Make the Sankey
   ##
      
      p <- sankeyNetwork(Links = links, 
                         Nodes = nodes,
                         Source = "IDsource", 
                         Target = "IDtarget",
                         Value = "value", 
                         NodeID = "name",
                         fontSize = 20)
      p

