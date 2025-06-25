# ================================
# R Project: Used Car Data Collection and Cleaning
# Sites: CarSite, AutoUncle
# Final Output: Used_cars.csv
# ================================

# ================================
# 1. Install and Load Packages
# ================================
packages <- c("tidyverse", "rvest", "stringr", "httr", "xml2", "skimr", "janitor", "zoo", "dplyr")
install.packages(setdiff(packages, installed.packages()[,"Package"]), dependencies = TRUE)

library(tidyverse)
library(rvest)
library(stringr)
library(httr)
library(xml2)
library(skimr)
library(janitor)
library(zoo)
library(dplyr)

rm(list = ls())
gc()

# ================================
# 2. CarSite Scraping & Cleaning
# ================================
get_carsite_url <- function(page_num) {
  paste0("https://www.carsite.co.uk/used-car/page/", page_num)
}

safe_extract <- function(node, selector) {
  result <- node %>% html_element(selector) %>% html_text(trim = TRUE)
  if (length(result) == 0) return(NA) else return(result)
}

scrape_carsite_page <- function(page_num) {
  url <- get_carsite_url(page_num)
  cat("Scraping Page:", page_num, "\n")
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  cars <- page %>% html_elements(".paContainer")
  
  car_data <- map_df(cars, function(car) {
    data.frame(
      Car_Name = safe_extract(car, "h3.avlink a"),
      Price = safe_extract(car, "h4.advertprice"),
      Mileage = safe_extract(car, "div.section28.fl:nth-child(2)"),
      Transmission = safe_extract(car, "div.section28.fl.mr1p:nth-child(1)"),
      Fuel_Type = safe_extract(car, "div.section28.fl:nth-child(4)"),
      Engine_Size = safe_extract(car, "div.section28.fl.mr1p:nth-child(5)"),
      Doors = safe_extract(car, "div.section28.fl:nth-child(6)"),
      Color = safe_extract(car, "div.section28.fl.mr1p:nth-child(3)"),
      Year_Model = safe_extract(car, ".section.f-12.pos-rel span.tal"),
      stringsAsFactors = FALSE
    )
  })
  Sys.sleep(runif(1, 2, 5))
  return(car_data)
}

carsite_raw <- map_df(1:300, scrape_carsite_page)
View(carsite_raw)

######_________________________________ Cleaning CarSite ________________________________####

carsite_data <- carsite_raw %>%
  mutate(Price = parse_number(Price),
         Mileage = parse_number(Mileage),
         Engine_Size = parse_number(Engine_Size)) %>%
  extract(Year_Model, into = c("Year", "Make", "Model", "Car_Type"),
          regex = "^\\s*(\\d{4})\\s+((?:Land\\s+Rover)|\\S+)\\s+(.+?)\\s+(\\S+)\\s*$",
          remove = FALSE) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Fuel_Type = if_else(Fuel_Type == "-", "Hybrid", Fuel_Type))

carsite_cleaned <- carsite_data %>%
  select(Make, Model, Year, Car_Type, Price, Mileage, Transmission, Fuel_Type, Engine_Size) %>%
  rename(Car_Brand = Make,
         Car_Model = Model,
         `Price (£)` = Price,
         `Mileage (miles)` = Mileage,
         `Engine_Size (cc)` = Engine_Size,
         Fuel = Fuel_Type)

View(carsite_cleaned)

# ================================
# 3. AutoUncle Scraping & Cleaning
# ================================
base_url <- "https://www.autouncle.co.uk/en-gb/used-cars?page="
start_page <- 1
num_pages <- 100

extract_text <- function(node, selector) {
  node %>% html_element(selector) %>% html_text(trim = TRUE) %>% ifelse(. == "", NA, .)
}

scrape_page <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  page %>% html_elements(".styles_listing-item__KWJkL") %>%
    map_df(~ data.frame(
      Make_Model = extract_text(.x, ".styles_headline__TnlnI"),
      Price = extract_text(.x, ".styles_price__X069_"),
      Mileage = extract_text(.x, ".styles_label__x4zE1:nth-child(2) .styles_text__eMFHA"),
      Year = extract_text(.x, ".styles_highlighted-attributes-container__EH8XY .styles_label__x4zE1:nth-child(1) .styles_text__eMFHA"),
      Fuel_Type = extract_text(.x, ".styles_label__x4zE1:nth-child(3) .styles_text__eMFHA"),
      Transmission = extract_text(.x, ".styles_highlighted-attributes-container__EH8XY .styles_label__x4zE1:nth-child(4) .styles_text__eMFHA"),
      Power = extract_text(.x, ".styles_label__x4zE1:nth-child(6) .styles_text__eMFHA"),
      stringsAsFactors = FALSE
    ))
}

all_cars <- map_df(start_page:(start_page + num_pages - 1), function(page_num) {
  url <- paste0(base_url, page_num)
  cat("Scraping page:", page_num, "\n")
  sleep_time <- runif(1, 2, 4)
  if (runif(1) < 0.1) { Sys.sleep(runif(1, 5, 10)) }
  Sys.sleep(sleep_time)
  scrape_page(url)
})
View(all_cars)

######_________________________________ Cleaning AutoUncle ________________________________####

clean_mileage <- function(data) {
  data %>%
    mutate(Mileage = str_remove_all(Mileage, "[^0-9]"),
           Mileage = as.numeric(Mileage))
}

split_fuel_engine <- function(data) {
  data %>%
    mutate(
      Engine_Size = ifelse(str_detect(Fuel_Type, "\\d\\.\\dL"),
                           str_extract(Fuel_Type, "\\d\\.\\dL"),
                           "0.0"),
      Fuel = str_remove(Fuel_Type, "\\d\\.\\dL\\s*"),
      Fuel = str_trim(Fuel)
    )
}

convert_engine_to_cc <- function(data) {
  data %>%
    mutate(
      Engine_Size = as.numeric(str_remove(Engine_Size, "L")),
      `Engine_Size (cc)` = Engine_Size * 1000
    ) %>%
    select(-Engine_Size)
}

clean_price <- function(data) {
  data %>%
    mutate(
      `Price (£)` = as.numeric(str_remove_all(Price, "[£,]"))
    ) %>%
    select(-Price)
}

extract_car_type <- function(data) {
  car_types <- c("Saloon", "Hatchback", "SUV", "Sports Utility Vehicle",
                 "Crossover", "Coupe", "Coupé", "Convertible", "Cabriolet",
                 "Estate", "Pick[- ]?up(?: Truck)?", "Minivan", "MPV",
                 "Roadster", "Van", "Cargo Van", "City Car", "Microcar",
                 "Liftback", "Fastback", "Targa(?: Top)?", "Limousine",
                 "Supercar", "Hypercar", "4x4", "Off[- ]?Roader", "Muscle Car",
                 "Coupe SUV", "Campervan", "Recreational Vehicle", "RV",
                 "Sportback", "Panel Van", "Combi Van", "Touring",
                 "Shooting Brake", "Double Cab", "Quad Cab",
                 "Extended Cab", "Supercrew", "Hardtop", "Softtop")
  
  car_type_pattern <- paste0("(?i)(", paste(car_types, collapse = "|"), ")")
  
  data %>%
    mutate(`Car Type` = str_extract(Make_Model, car_type_pattern),
           `Car Type` = str_to_title(str_trim(`Car Type`)))
}

extract_car_brand <- function(data) {
  brands <- c("Abarth", "AC", "Acura", "Aixam", "Alfa Romeo", "Alpine", 
              "Aston Martin", "Audi", "Austin", "Bentley", "BMW", "Bugatti", 
              "Buick", "Cadillac", "Caterham", "Chevrolet", "Chrysler", 
              "Citroën", "Cupra", "Dacia", "Daewoo", "Daihatsu", "Dodge", 
              "DS Automobiles", "Eagle", "Ferrari", "Fiat", "Fisker", 
              "Ford", "Genesis", "GMC", "Great Wall", "Haval", "Holden", 
              "Honda", "Hummer", "Hyundai", "Infiniti", "Isuzu", "Jaguar", 
              "Jeep", "Kia", "Lada", "Lamborghini", "Lancia", "Land Rover", 
              "Lexus", "Lincoln", "Lotus", "Mahindra", "Maserati", "Maybach", 
              "Mazda", "McLaren", "Mercedes", "MG", "Mini", "Mitsubishi", 
              "Nissan", "Oldsmobile", "Opel", "Pagani", "Peugeot", "Plymouth", 
              "Polestar", "Pontiac", "Porsche", "Proton", "Ram", "Range Rover", 
              "Renault", "Rolls-Royce", "Rover", "Saab", "Seat", "Skoda", 
              "Smart", "SsangYong", "Subaru", "Suzuki", "Tata", "Tesla", 
              "Toyota", "Vauxhall", "Volkswagen", "VW", "Volvo", "Wiesmann", "Zotye")
  brands <- brands[order(nchar(brands), decreasing = TRUE)]
  
  data %>%
    mutate(
      Car_brand = sapply(Make_Model, function(make_model) {
        brand_found <- NA
        for (brand in brands) {
          if (str_starts(make_model, brand)) {
            brand_found <- brand
            break
          }
        }
        return(brand_found)
      }),
      Make_Model = str_trim(ifelse(is.na(Car_brand), Make_Model,
                                   str_remove(Make_Model, paste0("^", Car_brand))))
    )
}

extract_car_model <- function(data) {
  data %>%
    mutate(
      Car_model = if_else(
        str_detect(Make_Model, "\\s\\d\\.\\d"),
        str_trim(str_extract(Make_Model, ".*?(?=\\s\\d\\.\\d)")),
        str_trim(Make_Model)
      )
    )
}

clean_year <- function(data) {
  data %>%
    mutate(
      Year = case_when(
        str_detect(Year, "\\b\\d{4}\\b") ~ str_extract(Year, "\\b\\d{4}\\b"),
        TRUE ~ NA_character_
      )
    )
}

final_clean_car_model <- function(data) {
  data %>%
    mutate(
      Car_model = str_split(Car_model, "\\s+") %>% 
        lapply(function(words) {
          words_clean <- words[!str_detect(tolower(words), "dr|kwh|kw|auto")]
          paste(words_clean, collapse = " ")
        }) %>%
        unlist() %>%
        str_squish()
    )
}

# Auto Uncle Pipeline Execution
auto_cars <- all_cars %>%
  clean_mileage() %>%
  split_fuel_engine() %>%
  convert_engine_to_cc() %>%
  clean_price() %>%
  extract_car_type() %>%
  extract_car_brand() %>%
  extract_car_model() %>%
  clean_year() %>%
  final_clean_car_model() %>%
  select(-Make_Model, -Fuel_Type, -Power)

View(auto_cars)

# ================================
# 4. Merge and Export Final Dataset
# ================================

auto_cars_aligned <- auto_cars %>%
  rename(
    Car_Brand = Car_brand,
    Car_Model = Car_model,
    Car_Type = `Car Type`,
    `Mileage (miles)` = Mileage
  ) %>%
  select(
    Car_Brand, Car_Model, Year, Car_Type, `Price (£)`, `Mileage (miles)`, Transmission, Fuel, `Engine_Size (cc)`
  )

carsite_cleaned <- carsite_cleaned %>% mutate(Year = as.numeric(Year))
auto_cars_aligned <- auto_cars_aligned %>% mutate(Year = as.numeric(Year))

Used_cars <- bind_rows(carsite_cleaned, auto_cars_aligned) %>%
  rename(
    Price_in_GBP = `Price (£)`,
    Mileage_in_miles = `Mileage (miles)`,
    Engine_Size_in_cc = `Engine_Size (cc)`
  ) %>%
  select(Car_Brand, Car_Model, Year, Car_Type, Price_in_GBP, Mileage_in_miles, Transmission, Fuel, Engine_Size_in_cc) %>%
  drop_na()

View(Used_cars)
skim(Used_cars)

write.csv(Used_cars, "Used_cars.csv", row.names = FALSE)