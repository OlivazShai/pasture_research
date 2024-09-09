get_br_ch_exports = function(x){
  
  # product codes as numeric
  products = c(20110, 20120, 20130, 20210, 20220, 20230)
  
  # x is an individual baci dataframe
  br_exports = x %>% 
    filter(
      i == 76, 
      j == 156,
      k %in% products
    )
  
  # return exports
  
  return(br_exports)
  
}