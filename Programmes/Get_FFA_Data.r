##
##    Programme:  Get_FFA_Data.r
##
##                The FFA data sits here: https://www.ffa.int/publications-and-statistics/statistics/
##
##                Ok, this bits not working with bog-standard cURL. Yet another thing that needs
##                RSelenuim :-/                
##
##    Author:     James Hogan, FAME, 25 July 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Go to the FFA Data page, and pull out the estimates zip file.
   ##
      url <- "https://www.ffa.int/publications-and-statistics/statistics/"
      h <- new_handle()
      handle_setopt(h, .list = list(customrequest = "GET"))
      tmp <- tempfile()
      connection <- curl(url,handle = h)

      Content <- htmlParse(readLines(connection))

      ##
      ##    Front page
      ##
         Data_Component <- getNodeSet(Content, "//a[@href]")
         Results <- lapply(Data_Component, function(x) {
                                                        href <- xmlAttrs(x)[[1]]
                                                        href <- href[str_detect(href, "estimates")] # get estimates data
                                                       })
         Results <- do.call(rbind, Results)
      
      ##
      ##    Download page
      ##
         h <- new_handle()
         handle_setopt(h, .list = list(customrequest = "GET"))
         tmp <- tempfile()
         connection <- curl(Results[[1]],handle = h)

         Content <- htmlParse(readLines(connection))

         Data_Component <- getNodeSet(Content, "//a[@href]")
         Results <- lapply(Data_Component, function(x) {
                                                        href <- xmlAttrs(x)[[1]]
                                                        href <- href[str_detect(href, "xlsx")] # get estimates data
                                                       })
         Results <- do.call(rbind, Results)


      for(i in 1:length(Results))
      {
         download.file(url = as.character(Results[[i]]), 
                       mode = "wb",
                       destfile = paste("Data_Raw", str_split_fixed(str_split_fixed(Results[[i]],"filename=",2)[,2], "\\&",2)[,1] , sep="/"))
      }
      
          


           
   ##
   ## Step 1: xxxxxxxxxxx
   ##


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
