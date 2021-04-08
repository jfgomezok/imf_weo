





path <- 'http://api.worldbank.org/v2/country?per_page=900&format=json&source=6'

locationRequest <- httr::GET(url = path)
locationResponse <- httr::content(locationRequest, as = "text", encoding = "UTF-8")

# Parse the JSON content and convert it to a data frame.
locationsJSON <- jsonlite::fromJSON(locationResponse, flatten = TRUE) %>%
  data.frame() %>% as_tibble()


# class(locationsJSON)


locationsJSON %>% count(incomeLevel.id, incomeLevel.value)

countries <- locationsJSON %>%
  # filter(incomeLevel.id %in% c("LMC","UMC")) %>% 
  select(
    geo_iso = id,
    # wbcode2  = iso2Code,
    # country_name = name,
    income_level = incomeLevel.id
  )


# countries <- countries %>% 
#   filter(!country_name %in% c("Belarus", "Belize", "Bolivia"))

