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
    
    # loop through list of objects and write each in separate sheet
    
    writeData(wb, 
            sheet = sheet,
            x = data)
    
    # set column width to auto to ensure everything can be read
    
    setColWidths(wb, sheet, cols = 1:ncol(data), widths = "auto")
    
    # add conditional formatting to relevant columns
    
    # add colour scale to columns containing the word 'diff'
    # range: -2.5 to 2.5
    
    if(any(grepl("^diff", colnames(data)))){
      
      diff <- grep("^diff", colnames(data))
      
      conditionalFormatting(wb = wb, sheet = sheet,
                          style = c("red", "green", "red"),
                          cols = diff, rows = (1:nrow(data)+1),
                          type = "colourScale",
                          rule = c(-2.5, 0, 2.5))
      }
  
  redstyle <- createStyle(bgFill = "#FF0000")
  greenstyle <- createStyle(bgFill = "#00FF00")
  
  # add red/green colouring to columns containing the word 'check'
  # green if <= 1; otherwise red
  
  if(any(grepl("^check", colnames(data)))){
    
    check <- grep("^check", colnames(data))
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = check, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = " <= 1",
                          style = greenstyle)
    
    conditionalFormatting(wb = wb, sheet = sheet,
                          cols = check, rows = 2:(nrow(data)+1),
                          type = "expression",
                          rule = " > 1",
                          style = redstyle)
  }
  
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
