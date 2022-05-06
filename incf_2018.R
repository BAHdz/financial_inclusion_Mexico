#require(easypackages)

easypackages::libraries("dint",
                        "dplyr", 
                        "readxl", 
                        "janitor")


incf2018_1 <- read_xlsx("inc_fin2018.xlsx",
                        sheet = 1, 
                        .name_repair = make_clean_names
                        )

glimpse(incf2018_1)

incf2018_1 <- incf2018_1 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2018, 1))

#2nd quarter

incf2018_2 <- read_xlsx("inc_fin2018.xlsx",
                        sheet = 2,
                        .name_repair = make_clean_names
                        )

glimpse(incf2018_2)

incf2018_2 <- incf2018_2 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2018, 2))

# 3rd quarter

incf2018_3 <- read_xlsx("inc_fin2018.xlsx",
                        sheet = 3,
                        .name_repair = make_clean_names
                        )

glimpse(incf2018_3)

incf2018_3 <- incf2018_3 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2018, 3))

# 4rth quarter

incf2018_4 <- read_xlsx("inc_fin2018.xlsx",
                        sheet = 4,
                        .name_repair = make_clean_names
                        )

glimpse(incf2018_4)

incf2018_4 <- incf2018_4 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2018, 4))

#summary(names(incf2018_1) == names(incf2018_2)) # 
#summary(names(incf2018_2) == names(incf2018_3))
#summary(names(incf2018_3) == names(incf2018_4))
#summary(names(incf2018_4) == names(incf2018_1))

compare_df_cols(incf2018_1, incf2018_2)
compare_df_cols(incf2018_2, incf2018_3)
compare_df_cols(incf2018_3, incf2018_4)
compare_df_cols(incf2018_4, incf2018_1)

incf_18_1 <- rbind(incf2018_1, incf2018_2)
incf_18_2 <- rbind(incf2018_3, incf2018_4)


unmat_cols <- compare_df_cols(incf_18_1, incf_18_2) %>%
  filter(incf_18_1 != incf_18_2 | is.na(incf_18_1) | is.na(incf_18_2)
         )

unmat_cols

vec_col <- as.vector(unmat_cols$column_name)

incf_18_2 <- incf_18_2 %>%
  select(-vec_col)

compare_df_cols_same(incf_18_1, incf_18_2)

incf_18 <- rbind(incf_18_1, incf_18_2)

glimpse(incf_18)


## Comparing 1920 and 18 databases

compare_df_cols(incf1920, incf_18) %>%
  filter(is.na(incf1920) | is.na(incf_18) | incf1920 != incf_18
         )

unmc18 <- compare_df_cols(incf1920, incf_18) %>%
  filter(is.na(incf1920))

unmc18 <- as.vector(unmc18$column_name) 

unmc18

incf_18 <- incf_18 %>%
  select(-unmc18)

#Deselecting unmatched columns from 1920 db

unmc1920 <- compare_df_cols(incf1920, incf_18) %>%
  filter(is.na(incf_18))

unmc1920 <- as.vector(unmc1920$column_name)

unmc1920

incf1920 <- incf1920 %>%
  select(-unmc1920)

compare_df_cols_same(incf_18, incf1920)

incf181920 <- rbind(incf_18, incf1920)

glimpse(incf181920)

