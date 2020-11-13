
## separate_if
# Find any col containing the whole word 1B, 1C, 2B, 2C, 3B or 3C. 
# If only one col matches the pattern, this col is separated between 
# the first and second position, into two cols: "landbak" and "innvand"
# If the original col is 0, both "landbak" and "innvand" will be 0

separate_if <- function() {
  
  match_col <- DF %>% 
    purrr::map_df(., ~grepl("^1B$|^1C$|^2B$|^2C$|^3B$|^3C$", .x)) %>% 
    dplyr::select(which(colSums(.) > 0)) %>% 
    names()
  
  if(length(match_col) > 1) {
    message("More than one column contains the whole word 1B, 1C, 2A, 2C, 3B or 3C")
  }
  
  if(length(match_col) == 0){
    message("No column contains the whole word 1B, 1C, 2A, 2C, 3B or 3C")
  }
  
  if(length(match_col) == 1){
    DF <- DF %>% 
      tidyr::separate(get(match_col), into = c("landbak", "innvand"), 
                      sep = c(1), remove = TRUE, extra = "merge") %>% 
      dplyr::mutate(innvand = ifelse(innvand == "" & landbak == 0, "0", innvand))
    
    message(paste0("The column \"", match_col, "\" contain the whole word 1B, 1C, 2A, 2C, 3B or 3C and was separated into \"landbak\" and \"innvand\""))
  }
  
  return(DF)
}

# Safe wrapper around separate_if, ensuring that any code following 
# separate_if will continue executing even if separate_if fails. 
# If separate_if fails, DF is not altered.
possibly_separate_if <- possibly(separate_if, 
                                 otherwise = "Identifiation and separation of one column contaning 1B, 1C, 2A, 2C, 3B, or 3C failed.")
possibly_separate_if()