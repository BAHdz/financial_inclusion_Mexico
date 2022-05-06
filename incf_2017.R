#require(easypackages)

easypackages::libraries("dint",
                        "dplyr", 
                        "readxl", 
                        "janitor")


incf2017_1 <- read_xlsx("incf_fin2017.xlsx",
                        sheet = 1,
                        .name_repair = make_clean_names)


incf2017_1 <- incf2017_1 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2017, 1))

glimpse(incf2017_1)


#2nd quarter

incf2017_2 <- read_xlsx("incf_fin2017.xlsx",
                        sheet = 2,
                        .name_repair = make_clean_names)


incf2017_2 <- incf2017_2 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2017, 2))

#3rd quarter

incf2017_3 <- read_xlsx("incf_fin2017.xlsx",
                        sheet = 3,
                        .name_repair = make_clean_names)


incf2017_3 <- incf2017_3 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2017, 3))

#4rth quarter 

incf2017_4 <- read_xlsx("incf_fin2017.xlsx",
                        sheet = 4,
                        .name_repair = make_clean_names)


incf2017_4 <- incf2017_4 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2017, 4))


#Comparing dataframes

compare_df_cols(incf2017_1, incf2017_2, incf2017_3, incf2017_4) %>%
  filter(is.na(incf2017_1) | is.na(incf2017_2) | 
           is.na(incf2017_3) | is.na(incf2017_4)
         )
#Joining 2017_2, 3 & 4
compare_df_cols_same(incf2017_2, incf2017_3, incf2017_4)

incf_17_234 <- rbind(incf2017_2, incf2017_3, incf2017_4)

unmc17 <- compare_df_cols(incf2017_1, incf_17_234) %>%
  filter(is.na(incf2017_1) | is.na(incf_17_234) |
           incf2017_1 != incf_17_234)

unmc17v <- as.vector(unmc17$column_name)


glimpse(incf_17_234)
  
incf_17_234 <- incf_17_234 %>%
  select(-all_of(unmc17v))

compare_df_cols_same(incf2017_1, incf_17_234)

incf_17 <- rbind(incf2017_1, incf_17_234)

glimpse(incf_17) 

#Compare and join 2017 and 181920 db

compare_df_cols(incf_17, incf181920)

compare_df_cols(incf_17, incf181920) %>%
  filter(is.na(incf_17) | is.na(incf181920) | 
           incf_17 != incf181920)

#Select NA columns from both db's

unmc17as <- compare_df_cols(incf_17, incf181920) %>%
  filter(is.na(incf_17) | is.na(incf181920) | 
           incf_17 != incf181920)

unmc17asv <- as.vector(unmc17as$column_name)  
  
incf_17 <- incf_17 %>%
  select(-contains(unmc17asv))

incf181920 <- incf181920 %>%
  select(-contains(unmc17asv))

##Joining db's

compare_df_cols_same(incf_17, incf181920)

incf17181020 <- rbind(incf_17, incf181920)

glimpse(incf17181020)
