easypackages::libraries("dint",
                        "dplyr", 
                        "readxl", 
                        "janitor",
                        "magrittr")

incf2021_3 <- read_xlsx("inc_fin2021.xlsx",
                        sheet = 1,
                        .name_repair = make_clean_names)

incf2021_3 %<>% mutate(clave_municipio = as.character(clave_municipio),
                       clave_estado = as.character(clave_estado)
                       ) %>% mutate(
                         across(where(is.numeric) & contains("poblacion"), 
                                round)) %>%
  mutate(periodo = date_yq(2020, 4))

glimpse(incf2021_3)

compare_df_cols(incf2021_3, incf1617181920) %>%
  filter(is.na(incf2021_3) | is.na(incf1617181920) |
           incf2021_3 != incf1617181920)






incf2021_3 %<>% select(-(mujeres:hombres_3))

incf2021_3 %<>% select(-(sucursales_banca_comercial_2:contratos_que_utilizan_banca_movil_2)
                       )

glimpse(incf2021_3)

