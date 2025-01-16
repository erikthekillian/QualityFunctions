hubtime = function(meco, VIP = FALSE, scale = TRUE) {
  x = meco$res_network
  h = hits_scores(x, scale = scale)
  h = as.data.frame(h$hub)
  colnames(h)[1] = "hub_score"
  hub = merge(h,meco$tax_table,by = 0, all.x = T)
  
  if (VIP == TRUE) {
  hub = hub[hub$hub_score >=0.1,]  
  }
  else{
    return(hub)
    }
}



rename_columns <- function(df, old_names, new_names) {
  # Check if lengths of old_names and new_names are equal
  if(length(old_names) != length(new_names)) {
    stop("The lengths of old_names and new_names must be equal.")
  }
  
  # Loop through the old_names and replace with new_names
  for (i in seq_along(old_names)) {
    colnames(df)[colnames(df) == old_names[i]] <- new_names[i]
  }
  
  return(df)
}
