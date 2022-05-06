#require(easypackages)
easypackages::libraries("dplyr", 
                        "readxl",
                        "forcats",
                        "janitor",
                        "ggplot2",
                        "ggthemes",
                        "gglorenz",
                        "ggloop",
                        "hrbrthemes",
                        "RColorBrewer")


count(incf1617181920, region)

ggquickeda::run_ggquickeda(incf1617181920)

glimpse(incf1617181920)
head(incf1617181920)

eda_dep <- incf1617181920 %>%
  select(region, cuentas_de_ahorro,
         tarjetas_de_debito, tarjetas_de_credito,
         deposito_al_ahorro, deposito_a_plazo,
         credito_al_consumo, credito_a_la_vivienda) %>%
  group_by(region) %>%
  summarise(total_ahorro = sum(cuentas_de_ahorro),
            total_tar_deb = sum(tarjetas_de_debito),
            total_tar_cred = sum(tarjetas_de_credito),
            total_dep_ah = sum(deposito_al_ahorro),
            total_dep_pl = sum(deposito_a_plazo),
            total_cred_c = sum(credito_al_consumo),
            total_cred_v = sum(credito_a_la_vivienda)
            ) %>%
  arrange(region) %>%
  filter(region != "Sin identificar")

glimpse(eda_dep)



#incf1617181920 %>%
# ggplot(aes(x = tarjetas_de_debito, y =  tarjetas_de_credito)) +
#  geom_point() +
#  facet_grid(cols = vars(region)
#             )
#  stat_smooth(method = "lm")

eda_dep %>%
  ggplot(aes(x = periodo, group = 1)) +
  geom_col(aes(y = total_cred_v), fill = "green") +
  geom_line(aes(y = total_cred_c), colour = "darkred") +
  theme_economist_white() +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1.35, hjust = 1.1
  ))

dev.off()


eda_dep %>%
#  filter(region %in% c("Ciudad de México", "Noroeste")) %>%
  ggplot(aes(x =  total_dep_ah)) +
  stat_lorenz(desc = T, alpha = 0.6, colour = "darkred") +
#  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_hc() +
  scale_x_percent() +
  scale_y_percent() +
  annotate_ineq(eda_dep$total_cred_c, color = "darkblue") +
  labs(title = "Coeficiente de Gini para los depósitos de ahorro por región",
       subtitle = "Datos históricos desde el primer trimestre del 2016 hasta el cuarto trimestre de 2020",
       caption = "Fuente: Elaboración propia con datos del Gobierno de México (2022)."
       )

#plot_rest <- 
eda_dep %>%
  mutate(region = fct_reorder(
    region, total_dep_ah, last)
    ) %>%
  arrange(desc(total_dep_ah)) %>%
  ggplot() +
  aes(x = total_dep_ah, y = region, fill = region) +
  geom_col() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlGn") +
  theme(legend.position = "none") +
  labs(x = "", 
       y = "",
       title = "Figura 1 Total de depósitos de ahorro acumulados por región",
       subtitle = "Datos históricos desde 2016 hasta 2020"
       )

plot_rest

dev.off()  

#plot_rest +
#  scale_fill_brewer(palette = "RdYlGn") +
#  theme(legend.position = "none")
#  scale_color_gradient2(low = "red", mid = "yellow",
#                       high = "red") +
 #+
#  scale_fill_brewer(palette = "Reds")
#  coord_flip()

dev.off()
