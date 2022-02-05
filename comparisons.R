comp4 <- inc_fin2020_4_lp %>%
  select((clave_municipio:tipo_de_poblacion_2020_4))

comp3 <- inc_fin2020_3_lp %>%
  select((clave_municipio:tipo_de_poblacion_2020_3))

comp2 <- inc_fin2020_2_lp %>%
  select((clave_municipio:tipo_de_poblacion_2020_2))

comp1 <- inc_fin2020_1_lp %>%
  select((clave_municipio:tipo_de_poblacion_2020_1))

identical(comp1$poblacion_2020_1, comp2$poblacion_2020_2)
dim(comp1);dim(comp2)
summary(comp1$poblacion_2020_1 == comp2$poblacion_2020_2)

identical(comp2$poblacion_2020_2, comp3$poblacion_2020_3)
dim(comp2);dim(comp3)
summary(comp2$poblacion_2020_2 == comp3$poblacion_2020_3)

identical(comp3$poblacion_2020_3, comp4$poblacion_2020_4)
dim(comp3);dim(comp4)
summary(comp3$poblacion_2020_3 == comp4$poblacion_2020_4)

identical(comp1$poblacion_adulta_2020_1, comp2$poblacion_adulta_2020_2) 
identical(comp2$poblacion_adulta_2020_2, comp3$poblacion_adulta_2020_3)
identical(comp3$poblacion_adulta_2020_3, comp4$poblacion_adulta_2020_4)


identical(comp1$tipo_de_poblacion_2020_1, comp2$tipo_de_poblacion_2020_2)
identical(comp2$tipo_de_poblacion_2020_2, comp3$tipo_de_poblacion_2020_3)
identical(comp3$tipo_de_poblacion_2020_3, comp4$tipo_de_poblacion_2020_4)

#Comparing 2019 databases

comp4 <- inc_fin2019_4_lp %>%
  select((clave_municipio:tipo_de_poblacion_2019_4))

comp3 <- inc_fin2019_3_lp %>%
  select((clave_municipio:tipo_de_poblacion_2019_3))

comp2 <- inc_fin2019_2_lp %>%
  select((clave_municipio:tipo_de_poblacion_2019_2))

comp1 <- inc_fin2019_1_lp %>%
  select((clave_municipio:tipo_de_poblacion_2019_1))

identical(comp1$poblacion_2019_1, comp2$poblacion_2019_2)
dim(comp1);dim(comp2)
summary(comp1$poblacion_2019_1 == comp2$poblacion_2019_2)

identical(comp2$poblacion_2019_2, comp3$poblacion_2019_3)
dim(comp2);dim(comp3)
summary(comp2$poblacion_2019_2 == comp3$poblacion_2019_3)

identical(comp3$poblacion_2019_3, comp4$poblacion_2019_4)
dim(comp3);dim(comp4)
summary(comp3$poblacion_2019_3 == comp4$poblacion_2019_4)

identical(comp1[, 6:10], comp2[, 6:10])
summary(comp1[, 6:10] == comp2[, 6:10])





identical(comp1[, 1], comp2[, 1])
identical(comp1[, 2], comp2[, 2])
identical(comp1[, 3], comp2[, 3])
identical(comp1[, 4], comp2[, 4])
identical(comp1[, 5], comp2[, 5])
identical(comp1[, 6], comp2[, 6])
identical(comp1[, 7], comp2[, 7])
identical(comp1[, 8], comp2[, 8])
identical(comp1[, 9], comp2[, 9])
identical(comp1[, 10], comp2[, 10])

# Full comparison
identical(comp1, comp2)
dim(comp1);dim(comp2)
summary(comp1 == comp2)

identical(comp2, comp3)
summary(comp2 == comp3)

identical(comp3, comp4)
summary(comp3 == comp4)

identical(comp4, comp1)
summary(comp4 == comp1)
summary(comp4 != comp1)


summary(inc_fin2019_1_lp == inc_fin2019_2_lp)


summary(inc_fin2020_1_lp == inc_fin2020_2_lp) #Data frames are not equally-sized
summary(inc_fin2020_2_lp == inc_fin2020_3_lp)
summary(inc_fin2020_3_lp == inc_fin2020_4_lp)


summary(inc_fin2020_2_lp[, 1:10] == inc_fin2020_3_lp[, 1:10])
summary(inc_fin2020_3_lp[, 1:10] == inc_fin2020_4_lp[, 1:10])

comps1 <- inct_20[, zehn_cols]
comps2 <- inct_19[, zehn_cols]

comps1 <- NULL
comps2 <- NULL

dim(comps1);dim(comps2)
