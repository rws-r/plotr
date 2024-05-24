yearsValid <- function(data = NULL,
                       variable = NULL){
  if("year" %in% names(data)){
    
    data <- data[,c("year",variable)]
    data <- data %>% 
      group_by(year) %>% 
      filter(!is.na(.data[[variable]])) %>% 
      summarize(count = n())
    
    return(data)
  }
  
}