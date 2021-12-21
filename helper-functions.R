

standardize_site <- function(x) {
  
  x <- super_basic_site_clean(str_squish(x))
  
  case_when(
    x %in% c("Bamberg") ~ "University of Bamberg",
    x %in% c("HSE", "HSE (Moscow / virtual)", "Moscow HSE") ~ "HSE University",
    str_detect(x, "Hunter College") ~ "Hunter College",
    str_detect(x, "^La$|^LA$|UCLA") ~ "Los Angeles",
    x %in% c("Montreal", "Montréal") ~ "Montréal",
    x %in% c("Cape Town", "Capetown", "UCT") ~ "University of Capetown",
    str_detect(x, "FGV|DAPP|Brazil") ~ "FGV/DAPP Brazil",
    x %in% c("Maasticht") ~ "Maastricht",
    TRUE ~ as.character(x)
  )


}

super_basic_site_clean <- function(x) {
  
  case_when(  
    str_detect(str_to_lower(x), str_to_lower("HSE University")) ~ "HSE University",
    str_detect(str_to_lower(x), str_to_lower("University of Bamberg")) ~ "University of Bamberg",
    str_detect(str_to_lower(x), str_to_lower("Bay Area")) ~ "Bay Area",
    str_detect(str_to_lower(x), str_to_lower("Duke")) ~ "Duke",
    str_detect(str_to_lower(x), str_to_lower("Istanbul")) ~ "Istanbul",
    str_detect(str_to_lower(x), str_to_lower("Los Angeles")) ~ "Los Angeles",
    str_detect(str_to_lower(x), str_to_lower("Maastricht")) ~ "Maastricht",
    str_detect(str_to_lower(x), str_to_lower("Montréal")) ~ "Montréal",
    str_detect(str_to_lower(x), str_to_lower("Rutgers")) ~ "Rutgers",
    str_detect(str_to_lower(x), str_to_lower("Stellenbosch")) ~ "Stellenbosch",
    str_detect(str_to_lower(x), str_to_lower("Tucson")) ~ "Tucson",
    str_detect(str_to_lower(x), str_to_lower("Boston")) ~ "Boston",
    str_detect(str_to_lower(x), str_to_lower("Capetown")) ~ "Capetown",
    str_detect(str_to_lower(x), str_to_lower("Chicago")) ~ "Chicago",
    str_detect(str_to_lower(x), str_to_lower("Hunter College")) ~ "Hunter College",
    str_detect(str_to_lower(x), str_to_lower("Monterrey")) ~ "Monterrey",
    str_detect(str_to_lower(x), str_to_lower("Oxford")) ~ "Oxford",
    str_detect(str_to_lower(x), str_to_lower("Princeton")) ~ "Princeton",
    str_detect(str_to_lower(x), str_to_lower("RTI International")) ~ "RTI International",
    str_detect(str_to_lower(x), str_to_lower("University of Bamberg")) ~ "University of Bamberg",
    str_detect(str_to_lower(x), str_to_lower("Zurich")) ~ "Zurich",
    str_detect(str_to_lower(x), str_to_lower("Boulder")) ~ "Boulder",
    str_detect(str_to_lower(x), str_to_lower("Helsinki")) ~ "Helsinki",
    str_detect(str_to_lower(x), str_to_lower("NYU")) ~ "NYU",
    str_detect(str_to_lower(x), str_to_lower("Seattle")) ~ "Seattle",
    str_detect(str_to_lower(x), str_to_lower("Beijing")) ~ "Beijing",
    str_detect(str_to_lower(x), str_to_lower("Bologna")) ~ "Bologna",
    str_detect(str_to_lower(x), str_to_lower("FGV/DAPP Brazil")) ~ "FGV/DAPP Brazil",
    str_detect(str_to_lower(x), str_to_lower("Higher School of Economics")) ~ "Higher School of Economics",
    str_detect(str_to_lower(x), str_to_lower("Hong Kong")) ~ "Hong Kong",
    str_detect(str_to_lower(x), str_to_lower("Howard/Mathematica")) ~ "Howard/Mathematica",
    str_detect(str_to_lower(x), str_to_lower("Law")) ~ "Law",
    str_detect(str_to_lower(x), str_to_lower("Lisbon")) ~ "Lisbon",
    str_detect(str_to_lower(x), str_to_lower("London")) ~ "London",
    str_detect(str_to_lower(x), str_to_lower("Taiwan")) ~ "Taiwan",
    str_detect(str_to_lower(x), str_to_lower("Tokyo")) ~ "Tokyo",
    TRUE ~ as.character(x)
  )
  
}
