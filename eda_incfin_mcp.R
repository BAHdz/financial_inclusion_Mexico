### Testing cmp plot & model


g_test2$x.total_tar_deb_y.total_tar_cred %L+% 
  geom_jitter(aes(colour = region), 
              size = 1.1, alpha = 0.5) +
#  geom_point(aes(colour = region), position = "jitter") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  stat_smooth(method = "lm", 
              formula = y ~ x,
              se = F,
              alpha = 0.7,
              size = 0.4,
              colour = "#D95F02") +
  stat_regline_equation(formula = y ~ x,
                        label.y = 6e+06, 
                        aes(label = ..eq.label..)
                        ) +
  stat_regline_equation(label.y = 5e+06,
                        aes(label = ..rr.label..)
                        ) +
  labs(title = "Regresión lineal entre total de tarjetas de crédito y tarjetas de débito",
       subtitle = "Datos históricos por región desde 2016 hasta 2020",
       x = "Total de tarjetas de débito",
       y = "Total de tarjetas de crédito")

dev.off()

lm_deb_cred = lm(total_tar_cred  ~ total_tar_deb, 
                 data = eda_dep_cp)

summary(lm_deb_cred)


