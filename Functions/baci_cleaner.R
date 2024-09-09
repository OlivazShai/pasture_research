get_product_trade = function(x, products){
  
  # product is product codes as numeric
  # x is an individual baci dataframe
  
  product_trade = x %>% 
    filter(
      k %in% {{ products }}
    )
  
  # return exports
  
  return(product_trade)
  
}