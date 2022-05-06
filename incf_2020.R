#require(easypackages)

easypackages::libraries("dint",
                        "dplyr", 
                        "readxl", 
                        "janitor")


incf2020_4 <- read_xlsx("inc_fin2_2020.xlsx",
                        sheet = 1,
                        .name_repair = make_clean_names)

glimpse(incf2020_4)

incf2020_4 <- incf2020_4 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2020, 4))


incf2020_3 <- read_xlsx("inc_fin2_2020.xlsx",
                        sheet = 2,
                        .name_repair = make_clean_names)


incf2020_3 <- incf2020_3 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2020, 3))


incf2020_2 <- read_xlsx("inc_fin2_2020.xlsx",
                        sheet = 3,
                        .name_repair = make_clean_names)


incf2020_2 <- incf2020_2 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2020, 2))


incf2020_1 <- read_xlsx("inc_fin2_2020.xlsx",
                        sheet = 4,
                        .name_repair = make_clean_names)


incf2020_1 <- incf2020_1 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) %>%
  mutate(periodo = date_yq(2020, 1))



summary(names(incf2020_1) == names(incf2020_2)) #Check wether all columns from 1st and 2nd quarters are the same
which(names(incf2020_1) != names(incf2020_2))

vec_col <- as.vector(which(names(incf2020_1) != names(incf2020_2)))


names(incf2020_1[,vec_col])
names(incf2020_2[,vec_col])


summary(names(incf2020_2) == names(incf2020_3))
summary(names(incf2020_3) == names(incf2020_4))
#summary(names(incf2020_4) == names(incf2020_1))

incf_20 <- rbind(incf2020_2, incf2020_3, incf2020_4)

as.vector(names(incf2020_1[,vec_col]))
as.vector(names(incf_20[,vec_col]))


incf_20 <- incf_20 %>%
  rename_at(vars(
    as.vector(names(incf_20[,vec_col]))
    ),
    ~as.vector(names(incf2020_1[,vec_col]))
  )
#  rename(incf_20[,vec_col] = incf2020_1[,vec_col])

incf_20 <- rbind(incf2020_1, incf_20)

glimpse(incf_20)
