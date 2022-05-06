easypackages::libraries("clipr",
                        "dint",
                        "dplyr", 
                        "readxl",
                        "forcats",
                        "janitor",
                        "ggplot2",
                        "ggthemes",
                        "gglorenz",
                        "ggloop",
                        "ggpubr",
                        "hrbrthemes",
                        "magrittr",
                        "RColorBrewer",
                        "report")


eda_dep_f <- incf1617181920 %>%
  select(region, periodo, cuentas_de_ahorro,
         tarjetas_de_debito, tarjetas_de_credito,
         deposito_al_ahorro, deposito_a_plazo,
         credito_al_consumo, credito_a_la_vivienda) %>%
  group_by(region, periodo) %>%
  summarise(total_ahorro = round(sum(cuentas_de_ahorro)),
            total_tar_deb = round(sum(tarjetas_de_debito)),
            total_tar_cred = round(sum(tarjetas_de_credito)),
            total_dep_ah = round(sum(deposito_al_ahorro)),
            total_dep_pl = round(sum(deposito_a_plazo)),
            total_cred_c = round(sum(credito_al_consumo)),
            total_cred_v = round(sum(credito_a_la_vivienda))
  ) %>%
  arrange(region, periodo) %>%
  filter(region != "Sin identificar")

glimpse(eda_dep_f)

g_test <- ggloop(eda_dep_f, aes_loop(x = total_ahorro:total_cred_v,
                                   y = total_ahorro:total_cred_v)
                 )
dev.off()

g_test$x.total_tar_deb_y.total_tar_cred %L+% #clusters detected
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired")

g_test$x.total_dep_ah_y.total_dep_pl %L+% #comparación descartada
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired")

g_test$x.total_cred_c_y.total_cred_v %L+% #clusters, 1 for a single region
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired")

g_test$x.total_ahorro_y.total_dep_ah %L+%
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") #comparacion descartada

g_test$x.total_tar_cred_y.total_dep_pl %L+% #cluster-regression
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") #+
#  stat_smooth(method = "lm", 
#              formula = y ~ x,
#              alpha = 0.7, se = F)

dev.off()

