


weo_parser <- function(x){

  # x <- "WEOApr2021all.xls"
  
  print(x)
  
  estimation_date <- glue("{str_sub(x, 4, 6)}-{str_sub(x, 7, 10)}") 
  type <- ifelse(str_sub(x, 14, 14) != "a","country","groups")
  
  
  df1 <- read.delim(
    file    = glue("data/{x}"),
    skipNul = TRUE,
    dec = "."
  ) %>% as_tibble()
  
  
  if (type == "country"){
    
    df2 <- df1 %>% 
      select(
        geo_name = Country,
        geo_iso = ISO,
        variable_code = WEO.Subject.Code,
        variable = Subject.Descriptor,
        units = Units,
        scale = Scale,
        starts_with("x"),
        first_estimate = Estimates.Start.After
      ) %>% 
      mutate(type = "by_countries")

  } else {
    
    df2 <- df1 %>% 
      select(
        geo_name = Country.Group.Name,
        variable_code = WEO.Subject.Code,
        variable = Subject.Descriptor,
        units = Units,
        scale = Scale,
        starts_with("X")
      ) %>% 
      mutate(
        first_estimate = NA_integer_,
        geo_iso = NA_character_,
        type = "by_country_groups"
      )
    
    
  }
  
  df3 <- df2 %>% 
    mutate(
      across(starts_with("X"), ~str_remove(.,",")),
      across(starts_with("X"), as.numeric)
    )
    
  df4 <- df3 %>% 
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      values_to = "value"
    )
  
  
  df5 <- df4 %>% 
    mutate(year = as.integer(str_remove(year, "X")))
  
  
  df_final <- df5 %>% 
    mutate(weo_date = estimation_date) %>% 
    filter(variable != "")
  
  
  
  
  return(df_final)
  
  
}
