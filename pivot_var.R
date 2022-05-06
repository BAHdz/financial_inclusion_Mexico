incf2020_4_flt <- incf2020_4 %>%
  select(clave_municipio:establecimientos_con_tpv_2)

incf2020_4_flt %<>%
  select(-contains("_2")) %>%
  select(-contratos_que_utilizan_banca_movil) 

incf2020_4_flt %<>%
  select(-(sucursales_banca_comercial:sucursales_sofipo))

glimpse(incf2020_4_flt)

gtest_rob <- ggloop(incf2020_4_flt, aes_loop(x = total_sucursales:establecimientos_con_tpv,
                                         y = total_sucursales:establecimientos_con_tpv)
                    )

gtest_rob %L+%
  geom_point(aes(colour = region)) #+
#  stat_smooth(method = "lm", 
 #             formula = y ~ x, 
#              se = F) #+
#  stat_regline_equation(aes(label = ..eq.label..)
#                        )


dev.off()

incf2020_4_flt <- incf2020_4 %>%
  filter(sucursales_banca_comercial <= 250 | 
           cajeros_automaticos <= 1500)

lm_test <- lm(cajeros_automaticos ~ sucursales_banca_comercial + 0,
              data = incf2020_4_flt)

summary(lm_test)

incf2020_4_flt %>%
  ggplot() +
  aes(sucursales_banca_comercial, cajeros_automaticos) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ x + 0)

write_clip(report(lm_test))

incf2020_4_flt %>%
  ggplot() +
  aes(sucursales_banca_comercial, cajeros_automaticos, 
      colour = region) +
  geom_point()

dev.off()
