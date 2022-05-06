require(easypackages)

easypackages::libraries("dint",
                        "dplyr", 
                        "readxl", 
                        "janitor")

incf2019_4 <- read_xlsx("inc_fin2019.xlsx",
                        sheet = 1,
                        .name_repair = make_clean_names
                        )


incf2019_4 <- incf2019_4 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2019, 4))
  

incf2019_3 <- read_xlsx("inc_fin2019.xlsx",
                        sheet = 2,
                        .name_repair = make_clean_names
                        )


incf2019_3 <- incf2019_3 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2019, 3))



incf2019_2 <- read_xlsx("inc_fin2019.xlsx",
                        sheet = 3,
                        .name_repair = make_clean_names)

incf2019_2 <- incf2019_2 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2019, 2))


incf2019_1 <- read_xlsx("inc_fin2019.xlsx",
                        sheet = 4,
                        .name_repair = make_clean_names)

incf2019_1 <- incf2019_1 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2019, 1))


summary(names(incf2019_1) == names(incf2019_2))
summary(names(incf2019_2) == names(incf2019_3))
summary(names(incf2019_3) == names(incf2019_4))
summary(names(incf2019_4) == names(incf2019_1))


incf_19 <- rbind(incf2019_1, incf2019_2, 
                 incf2019_3, incf2019_4)

summary(names(incf_20) == names(incf_19))
which(names(incf_20) != names(incf_19))

incf1920 <- rbind(incf_19, incf_20)


