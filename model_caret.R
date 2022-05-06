set.seed(350)

train_sp <- rub_uah_usd$RUB %>%
  createDataPartition(p = 0.8, list = FALSE)

train_db <- rub_uah_usd[train_sp,]
test_db <- rub_uah_usd[-train_sp, ]

model_caret <- lm(RUB ~ poly(UAH, 5, raw = T),
                  data = train_db)

summary(model_caret)


predictions_cr = model_caret %>%
  predict(test_db)

predictions_cr

model_perf <- data.frame(
  RMSE_md = RMSE(predictions_cr, test_db$RUB),
  R2_md = R2(predictions_cr, test_db$RUB)
  )
model_perf

list_coef <- coefficients(model_caret)
list_coef
class(list_coef)

train_db %>%
  ggplot() +
  aes(x = date, y = RUB) +
  geom_line(colour = "#EF3B2C") +
#  stat_function(fun = coeffs_fm, colour = "#238B45") +
  stat_smooth(method = lm, 
              formula = y ~ poly(x, 5, raw = T),
              size = 0.5,
              alpha = 0.4) +
#  stat_regline_equation(label.x = as.Date("2020-08-01"),
#                        label.y = 65) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "1 years") +
  theme_hc() +
  labs(title = "Regresión lineal polinómica para el precio del dólar estadounidense en rublos",
       subtitle = "Datos históricos desde el 2017 hasta el 2022",
       x = "",
       y = "")

dev.off()
