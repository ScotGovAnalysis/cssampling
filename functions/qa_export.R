#' Exports QA data frames to an Excel file.
#' 
#' @param list_df List of the QA data frames which are to be exported.
#' 
#' @param survey Name of survey the QA is conducted for.
#' 
#' @returns Conditionally formatted Excel file which includes the 
#' QA data frames.
#' 
#' @examples
#' qa_export(list_df = qa, survey = "scjs")


qa_export <- function(list_df, survey){
  
  wb <- createWorkbook()
  
  for(i in 1:length(list_df)){
    
    sheet <- addWorksheet(wb, names(list_df[i]))
    data <- list_df[[i]]
    name <- names(list_df[i])
    
    # loop through list of objects and write each in separate sheet
    
    writeData(wb, 
            sheet = sheet,
            x = data)
    
    # add conditional formatting to relevant columns
    
    # diff ----

    # add colour scale to columns containing the word 'diff'
    
    if(any(grepl("^diff", colnames(data)))){
      
      diff <- grep("^diff", colnames(data))
      
      conditionalFormatting(wb = wb, sheet = sheet,
                          style = c("red", "green", "red"),
                          cols = diff, rows = (1:nrow(data)+1),
                          type = "colourScale",
                          rule = c(-paf_sample.threshold, 0, paf_sample.threshold))
      }
    
    # urbrur in SHeS ----
    
    if(grepl("urbrur", name) == T & 
       survey == "shes"){
      
      diff <- grep("^diff", colnames(data))
      
      conditionalFormatting(wb = wb, sheet = sheet,
                            style = c("red", "green", "red"),
                            cols = diff, rows = (1:nrow(data)+1),
                            type = "colourScale",
                            rule = c(-shes.urbrur.threshold, 0, shes.urbrur.threshold))
    }
    
    # SIMDQ ----
    
    if(grepl("simdq", name) == T){
      
      diff <- grep("^diff", colnames(data))
      
      conditionalFormatting(wb = wb, sheet = sheet,
                            style = c("red", "green", "red"),
                            cols = diff, rows = (1:nrow(data)+1),
                            type = "colourScale",
                            rule = c(ifelse(survey == "shes",
                                            -shes.simdq.threshold,
                                            -simdq.threshold), 
                                     0, 
                                     ifelse(survey == "shes",
                                            shes.simdq.threshold,
                                            simdq.threshold)))
    }
    
    # check ----
    
  redstyle <- createStyle(bgFill = "#FF0000")
  greenstyle <- createStyle(bgFill = "#00FF00")
  
  # add red/green colouring to columns containing the word 'check'
  # green if <= 1; otherwise red
  
  if(any(grepl("^check", colnames(data)))){
    
    check <- grep("^check", colnames(data))
    shs.check <- list(paste0(" <= ", shs.stream.threshold),
                      paste0(" > ", shs.stream.threshold))
    scjs.check <- list(paste0(" <= ", scjs.stream.threshold),
                       paste0(" > ", scjs.stream.threshold))
    
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = check, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = ifelse(survey == "shs",
                                        shs.check[[1]],
                                        scjs.check[[1]]),
                          style = greenstyle)
    
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = check, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = ifelse(survey == "shs",
                                        shs.check[[2]],
                                        scjs.check[[2]]),
                          style = redstyle)
  }
  
  # overlap ----
  
  # add red/green colouring to columns containing the word 'overlap'
  # green if 'no'; otherwise red
  
  if(any(grepl("^overlap", colnames(data)))){
    
    overlap <- grep("^overlap", colnames(data))
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = overlap, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = ' == "yes"',
                          style = greenstyle)
    
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = overlap, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = ' == "no"',
                          style = redstyle)
  }
  
  # udprn ----
  
  # add red/green colouring to columns containing the word 'udprn'
  # red if udprn isn't 0 (i.e., udprn has been previously sampled)
  
  udprn <- grep("^udprn", colnames(data))
  if(names(list_df[i]) == "previously.sampled.udprn"){
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = udprn, 
                          rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = ' != 0',
                          style = redstyle)
  }
  }
  
  # export to Excel file
  path <- eval(as.name(paste0(survey, ".path")))
  saveWorkbook(wb, file = paste0(path, 
                                 Sys.Date(),
                                 "_",
                               survey,
                               ".contractorsample.",
                               syear,
                               " - QA.xlsx"), 
             overwrite = TRUE)
  }
