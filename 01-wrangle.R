library(fs)
library(data.table)
library(dplyr)
library(arrow)
library(purrr)
library(readxl)
library(dthelpers)

source("https://raw.githubusercontent.com/johnmackintosh/utility-belt/refs/heads/main/agebander.R")


# fs::dir_create("./inputs")
# fs::dir_create("./outputs")
# fs::dir_create("./duck")

path <- "./inputs/data.xlsx"
all_sheets <- path %>%
  readxl::excel_sheets() %>%
  purrr::set_names()


# discard unnecessary sheet names and bind the other to one dataframe

all_sheets <- all_sheets[which(all_sheets %in% c("Persons",
                                                 "Females",
                                                 "Males"))]
all_sheets_df <- map(all_sheets,
                     \(x) read_excel(path,
                                     sheet = x,
                                     skip = 3)) |>
  list_rbind(names_to = "sex")



# set to data.table ahead of further proccessing
setDT(all_sheets_df)

all_sheets_dt <- all_sheets_df |>
  melt(id.vars = c("sex" ,"Data zone code", "Data zone name",
                   "Council area code", "Council area name"),
       variable.name = "Age", value.name = "population")


# make small output totals by datazone for joining to SIMD
# all_sheets_dt[`Council area name` == "Highland" & sheet == "Persons" & Age == "Total population", .(`Data zone code`,`Data zone name`,population)]|> fwrite("highland-datazone-totals.csv")


# total populations - need to keep these separate from the other ages
# so they can be converted to numerics

all_ages <- all_sheets_dt[Age == "Total population"][,let(Age = "all_ages")] |>
  janitor::clean_names()


# common processing -  convert ages to numeric, add age bands

# all individual ages
source_data <- all_sheets_dt[Age != "Total population"
                              ][,let(Age = gsub("[^0-9]","",Age))
                               ][Age == "90", let(Age = "90+")] |>
  janitor::clean_names()

# convert the ages to numeric
source_data[age != "90+",let(age = as.numeric(age))][]
source_data[age == "90+",let(age = 90L)][]
source_data[,let(age = as.numeric(age))][]


# create the age bands
source_data <- age_bander(source_data, age, factored = FALSE) |> setDT()


# get NHS Highland geography codes
highland_codes <- fread("datazone-hscp-lookup.txt")

source_data <- source_data |>
  left_join(highland_codes,
            by = c("data_zone_code" = "DataZone")) |>
  setDT()


# prepare to  save this as arrow / duckdb

pq_path <- "./arrow"



# partition by council area

arrow::write_dataset(source_data,
                     path = pq_path,
                     format = "parquet",
                     partitioning = "council_area_name")



# age bands for maternity request

high_extract <- source_data |> filter(!is.na(CP_Name)) |> setDT()

high_extract[,let(broad_age_band = fcase(
  data.table::between(age, 0, 15), "0-15",
  data.table::between(age, 16, 24), "16-24",
  data.table::between(age, 25, 44), "25-44",
  data.table::between(age, 45, 64), "45-64",
  data.table::between(age, 65, 74), "65-74",
  default = "75 and over"))]


# highland by CP / Age / Sex

high_extract[, .(pop = sum(population)),
             .(sex, CP_Name, age_band)] |>
  arrow::write_dataset(dataset = _,
                       path = "./outputs/cp-age-sex")

# highland by CP / Sex

high_extract[, .(pop = sum(population)),.(sex, CP_Name)]|>
  arrow::write_dataset(dataset = _,
                       path = "./outputs/cp-sex")



# highland by age/sex/ CP and intermediate zone

high_extract[, .(pop = sum(population)),
             .(sex, CP_Name,IntZone, age_band)] |>
  arrow::write_dataset(dataset = _,
                       path = "./outputs/cp-iz-age-sex")


# tallies by broad age band

high_extract[, .(pop = sum(population)),
             .(sex, CP_Name, broad_age_band)] |>
  arrow::write_dataset(dataset = _,
                       path = "./outputs/cp-broad-age-sex")


res <- high_extract[, .(pop = sum(population)), .(sex, CP_Name, broad_age_band)]


 # create tallies for printing / completing  maternity requestor's sheet

print_totals <- function(x){

out1 <- res[CP_Name == x & sex == "Persons"] |>
    dcast(CP_Name + sex ~ broad_age_band, value.var = "pop")
print(out1)

out2 <- res[CP_Name == x & sex == "Females"] |>
  dcast(CP_Name + sex ~ broad_age_band, value.var = "pop")
print(out2)

out3 <- res[CP_Name == x & sex == "Males"] |>
  dcast(CP_Name + sex ~ broad_age_band, value.var = "pop")
print(out3)

out4 <- res[CP_Name == x] |>
  #dthelpers::order_by(dt = _, sheet, CP_Name) |>
  dcast(CP_Name ~ sex,  value.var = "pop", fun.aggregate = sum)

print(out4)
}

areas <- get_uniques(res, CP_Name)
areas <- areas[,CP_Name]

map(areas, ~ print_totals(.x))

fwrite(res, "high-level-CP-ageband-pops.csv")
fwrite(res_all, "high-level-CP-pops.csv")


 ##### QUERYING ####

# to query for NHS Highland, use "Highland" and "Argyll and Bute"


## main source data
sape_pq <- open_dataset(pq_path)

query <- sape_pq |>
  filter(council_area_name == "Highland") |>
  collect()


# highland only

highpath <- "./outputs/cp-iz-age-sex"

sape_pq <- open_dataset(highpath)
query <- sape_pq |>
  filter(CP_Name == "Sutherland") |>
  collect()
