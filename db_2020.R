require(easypackages)

libraries("stringr", "dplyr", "readxl",
          "writexl", "janitor", "textclean") 
#Importing required libraries

#2020 data 

# 4th quarter 2020
inc_fin2020_4 <- read_xlsx("inc_fin2020.xlsx",
                           .name_repair = make_clean_names
)

glimpse(inc_fin2020_4)

inc_fin2020_4 <- inc_fin2020_4 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

#head(inc_fin2020_4[, 6:10])

glimpse(inc_fin2020_4)

#colnames(inc_fin2020_4)

sharedcols <- inc_fin2020_4[,1:10]
dim(sharedcols)
no_scols <- inc_fin2020_4[, -(1:10)]
dim(no_scols)

no_scols <- no_scols %>%
  rename_all(paste0, "_20_4")

glimpse(no_scols)

inc_fin2020_4_lp <- cbind(sharedcols, no_scols)

#head(inc_fin2020_4_lp)

dim(inc_fin2020_4_lp)
glimpse(inc_fin2020_4_lp)
#colnames(inc_fin2020_4_lp)

#3rd quarter 2020

inc_fin2020_3 <- read_xlsx("inc_fin2020_3.xlsx",
                           .name_repair = make_clean_names
)

glimpse(inc_fin2020_3)
dim(inc_fin2020_3)

inc_fin2020_3 <- inc_fin2020_3 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

#head(inc_fin2020_3[6:10])

glimpse(inc_fin2020_3)

sharedcols <- inc_fin2020_3[,1:10]
#dim(sharedcols)
no_scols <- inc_fin2020_3[, -(1:10)]
#dim(no_scols)

no_scols <- no_scols %>%
  rename_all(paste0, "_20_3")

inc_fin2020_3_lp <- cbind(sharedcols, no_scols)

#head(inc_fin2020_3_lp)
glimpse(inc_fin2020_3_lp)

# 2nd quarter 2020

inc_fin2020_2 <- read_xlsx("inc_fin2020_2.xlsx",
                           .name_repair = make_clean_names
)

glimpse(inc_fin2020_2)
dim(inc_fin2020_2)

inc_fin2020_2 <- inc_fin2020_2 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

head(inc_fin2020_2[,6:10])

glimpse(inc_fin2020_2)

sharedcols <- inc_fin2020_2[,1:10]
dim(sharedcols)
no_scols <- inc_fin2020_2[, -(1:10)]
dim(no_scols)


no_scols <- no_scols %>%
  rename_all(paste0, "_20_2")

inc_fin2020_2_lp <- cbind(sharedcols, no_scols)

glimpse(inc_fin2020_2_lp)

# 1st quarter 2020


inc_fin2020_1 <- read_xlsx("inc_fin2020_1.xlsx",
                           .name_repair = make_clean_names
)

glimpse(inc_fin2020_1)
dim(inc_fin2020_1)

inc_fin2020_1 <- inc_fin2020_1 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

head(inc_fin2020_1[,6:10])

glimpse(inc_fin2020_1)

sharedcols <- inc_fin2020_1[,1:10]
dim(sharedcols)
no_scols <- inc_fin2020_1[, -(1:10)]
dim(no_scols)

no_scols <- no_scols %>%
  rename_all(paste0, "_20_1")

inc_fin2020_1_lp <- cbind(sharedcols, no_scols)

glimpse(inc_fin2020_1_lp)

dim(inc_fin2020_1_lp)


dim(inc_fin2020_1_lp);dim(inc_fin2020_2_lp);dim(inc_fin2020_3_lp);dim(inc_fin2020_4_lp)

zehn_cols <- as.vector(colnames(inc_fin2020_1[,1:10]))

zehn_cols <- as.vector(colnames(sharedcols))
zehn_cols

inct_20 <- inc_fin2020_1_lp %>%
  inner_join(inc_fin2020_2_lp, by = zehn_cols) %>%
  inner_join(inc_fin2020_3_lp, by = zehn_cols) 

dim(inct_20)


funf_cols <- as.vector(colnames(sharedcols[1:5])) #Using first 5 columns instead of 10
funf_cols #Columns from 6 to 10 do not share the same information with the rest of the database

cols_exc <- inc_fin2020_4_lp[, -(6:10)] #Joining these columns with database will generate an unmatch that deletes all rows

inct_20 <- inct_20 %>%
  inner_join(cols_exc, by = funf_cols)


glimpse(inct_20)
dim(inct_20)

inct_20 <- inct_20 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(
    where(is.character), replace_non_ascii)
  )


glimpse(inct_20)

write_xlsx(inct_20, "inct2020.xlsx")

