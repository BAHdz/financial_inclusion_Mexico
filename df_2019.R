require(easypackages)

libraries("stringr", "dplyr", "readxl",
          "writexl", "janitor", "textclean") 
#Importing required libraries

#2019 data 

# 4th quarter 2019

inc_fin2019_4 <- read_xlsx("inc_fin2019.xlsx", 
                           sheet = 1,
                           .name_repair = make_clean_names #Clean most of spaces, dots and uppercases of column names
)

glimpse(inc_fin2019_4)


inc_fin2019_4 <- inc_fin2019_4 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round)) #Round all quantities from population

head(inc_fin2019_4[, 6:10]) #colummns that contain data about population


sharedcols <- inc_fin2019_4[,1:10] #Separate columns with shareable data about names, states and population
dim(sharedcols)
glimpse(sharedcols)
no_scols <- inc_fin2019_4[, -(1:10)] #The rest of data
dim(no_scols)
glimpse(no_scols)

no_scols <- no_scols %>%
  rename_all(paste0, "_19_4") #Add suffix to label data from 4th quarter 2019

glimpse(no_scols) #All labeled columns must appear with suffix "2019_4

inc_fin2019_4_lp <- cbind(sharedcols, no_scols)

colnames(inc_fin2019_4_lp)
glimpse(inc_fin2019_4_lp)

## 3rd quarter 2019

inc_fin2019_3 <- read_xlsx("inc_fin2019.xlsx", 
                           sheet = 2,
                           .name_repair = make_clean_names
)

inc_fin2019_3 <- inc_fin2019_3 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

#head(inc_fin2019_3[, 6:10])

glimpse(inc_fin2019_3)
colnames(inc_fin2019_3)


sharedcols <- inc_fin2019_3[,1:10]
no_scols <- inc_fin2019_3[, -(1:10)]

no_scols <- no_scols %>%
  rename_all(paste0, "_19_3")

glimpse(no_scols)


inc_fin2019_3_lp <- cbind(sharedcols, no_scols)

glimpse(inc_fin2019_3_lp)

## 2nd quarter 2019

inc_fin2019_2 <- read_xlsx("inc_fin2019.xlsx", 
                           sheet = 3,
                           .name_repair = make_clean_names
)

inc_fin2019_2 <- inc_fin2019_2 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

head(inc_fin2019_2[, 6:10])

glimpse(inc_fin2019_2)
colnames(inc_fin2019_2)


sharedcols <- inc_fin2019_2[,1:10]
no_scols <- inc_fin2019_2[, -(1:10)]

no_scols <- no_scols %>%
  rename_all(paste0, "_19_2")


inc_fin2019_2_lp <- cbind(sharedcols, no_scols)

glimpse(inc_fin2019_2_lp)

## 1st quarter 2019

inc_fin2019_1 <- read_xlsx("inc_fin2019.xlsx", 
                           sheet = 4,
                           .name_repair = make_clean_names
)

inc_fin2019_1 <- inc_fin2019_1 %>%
  mutate(across(where(is.numeric) & contains("poblacion"), 
                round))

head(inc_fin2019_1[, 6:10])

glimpse(inc_fin2019_1)


sharedcols <- inc_fin2019_1[,1:10]
no_scols <- inc_fin2019_1[, -(1:10)]

no_scols <- no_scols %>%
  rename_all(paste0, "_19_1")

inc_fin2019_1_lp <- cbind(sharedcols, no_scols)

glimpse(inc_fin2019_1_lp)

## Join data bases and write xlsx file

dim(inc_fin2019_1_lp);dim(inc_fin2019_2_lp);dim(inc_fin2019_3_lp);dim(inc_fin2019_4_lp)

zehn_cols <- as.vector(colnames(sharedcols))

#Joining all 2019 bases

inct_19 <- inc_fin2019_1_lp %>%
  inner_join(inc_fin2019_2_lp, by = zehn_cols) %>%
  inner_join(inc_fin2019_3_lp, by = zehn_cols) %>%
  inner_join(inc_fin2019_4_lp, by = zehn_cols)

#Parsing strings to ASCII characters in all database

inct_19 <- inct_19 %>%
  mutate(clave_municipio = as.character(clave_municipio),
         clave_estado = as.character(clave_estado)
  ) %>%
  mutate(across(
    where(is.character), replace_non_ascii))


glimpse(inct_19)
dim(inct_19)

# Writing xlsx file with full database

write_xlsx(inct_19, "inct2019.xlsx")

