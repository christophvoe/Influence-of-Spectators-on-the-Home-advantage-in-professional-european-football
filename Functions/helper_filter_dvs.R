filtered_df <- function(data,dependent){
  
  data <- data %>% 
    filter(dv %in% c(dependent[1], dependent[2], dependent[3], dependent[4], 
                     dependent[5], dependent[6], dependent[7]))
}