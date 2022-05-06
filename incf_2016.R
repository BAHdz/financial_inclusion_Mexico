#require(easypackages)

easypackages::libraries("dint",
                        "dplyr",
                        "magrittr",
                        "readxl", 
                        "janitor")

#1st quarter

incf2016_1 <- read_xlsx("inc_fin2016.xlsx",
                        sheet = 1,
                        .name_repair = make_clean_names
                        )

incf2016_1 <- incf2016_1 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2016, 1))

glimpse(incf2016_1)

#2nd quarter 

incf2016_2 <- read_xlsx("inc_fin2016.xlsx",
                        sheet = 2,
                        .name_repair = make_clean_names)

incf2016_2 <- incf2016_2 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2016, 2))

glimpse(incf2016_2)

#3rd quarter

incf2016_3 <- read_xlsx("inc_fin2016.xlsx",
                        sheet = 3,
                        .name_repair = make_clean_names)

incf2016_3 <- incf2016_3 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2016, 3))

glimpse(incf2016_3)

#4rth quarter

incf2016_4 <- read_xlsx("inc_fin2016.xlsx",
                        sheet = 4,
                        .name_repair = make_clean_names)

incf2016_4 <- incf2016_4 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2016, 4))

glimpse(incf2016_4)

#Compare columns among db's

compare_df_cols(incf2016_2, incf2016_3, incf2016_4)

#Compare names for all 4 db's

summary(names(incf2016_1) == names(incf2016_2))
summary(names(incf2016_2) == names(incf2016_3)) #all matches
summary(names(incf2016_3) == names(incf2016_4)) #all matches
summary(names(incf2016_4) == names(incf2016_1)) #main mismatches are in 2016_1

incf_16_234 <- rbind(incf2016_2, incf2016_3, incf2016_4)

compare_df_cols(incf2016_1, incf_16_234)

#Compare main db to 2016_1 and 16_234

compare_df_cols(incf_16_234, incf17181020)

incf_16_234 %>%
  select(-superficie_km2) %>%
  compare_df_cols_same(incf17181020)
#Both db's are joinable, it's necessary to delete superficie_km2. 
#2016_1 db nees to be adjusted.

incf_16_234 <- incf_16_234 %>%
  select(-superficie_km2)


ncol(incf2016_1);ncol(incf_16_234)

unmc16 <- compare_df_cols(incf2016_1, incf_16_234) %>%
  filter(is.na(incf2016_1) | is.na(incf_16_234) | 
           incf2016_1 != incf_16_234)

unmc16 <- as.vector(unmc16$column_name)

incf2016_1 <- incf2016_1 %>%
  select(-contains(unmc16))

incf_16_234 <- incf_16_234 %>%
  select(-contains(unmc16))

incf_16 <- rbind(incf2016_1, incf_16_234)

## Compare 2016 with main db

unmc1620 <- compare_df_cols(incf_16, incf17181020) %>%
  filter(is.na(incf_16) | is.na(incf17181020) |
           incf_16 != incf17181020)

unmc1620 <- as.vector(unmc1620$column_name)

incf17181020 <- incf17181020 %>%
  select(-unmc1620)

#Joing both db's

incf1617181920 <- rbind(incf_16, incf17181020)

#incf1617181920 %<>% mutate(periodo = format_yq_short(periodo))

glimpse(incf1617181920)


require(writexl)

write_xlsx(incf1617181920, "incfin1620.xlsx")
