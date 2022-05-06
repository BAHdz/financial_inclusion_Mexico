easypackages::libraries("dplyr", 
                        "readxl",
                        "forcats",
                        "janitor",
                        "ggplot2",
                        "ggthemes",
                        "gglorenz",
                        "ggloop",
                        "ggpubr",
                        "hrbrthemes",
                        "RColorBrewer")

glimpse(incf1617181920)

eda_dep_cp <- incf1617181920 %>%
  select(region, estado, periodo, cuentas_de_ahorro,
         tarjetas_de_debito, tarjetas_de_credito,
         deposito_al_ahorro, deposito_a_plazo,
         credito_al_consumo, credito_a_la_vivienda) %>%
  group_by(region, estado, periodo) %>%
  summarise(total_ahorro = round(sum(cuentas_de_ahorro)),
            total_tar_deb = round(sum(tarjetas_de_debito)),
            total_tar_cred = round(sum(tarjetas_de_credito)),
            total_dep_ah = round(sum(deposito_al_ahorro)),
            total_dep_pl = round(sum(deposito_a_plazo)),
            total_cred_c = round(sum(credito_al_consumo)),
            total_cred_v = round(sum(credito_a_la_vivienda))
  ) %>%
  arrange(region, estado, periodo) %>%
  filter(region != "Sin identificar")

glimpse(eda_dep_cp)

g_test2 <- ggloop(eda_dep_cp, aes_loop(x = total_ahorro:total_cred_v,
                                     y = total_ahorro:total_cred_v))

dev.off()

g_test2$x.total_tar_deb_y.total_tar_cred %L+%  # linear tendency, possibly clusters
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() #+
#  stat_smooth(method = "lm", 
#              formula = y ~ 0 + x,
#              se = F)

g_test2$x.total_dep_ah_y.total_dep_pl %L+% #comparación descartada
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal()

g_test2$x.total_cred_c_y.total_cred_v %L+% #clusters detected
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal()

g_test2$x.total_ahorro_y.total_dep_ah %L+% #comparación descartada
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal()

g_test2$x.total_tar_cred_y.total_dep_pl %L+% #possibly clusters
  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() #+
#  stat_smooth(method = "lm", se = F)

dev.off()

plot_list <- g_test2 %L+%
  geom_point(aes(colour = region))

