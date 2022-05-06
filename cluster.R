set.seed(1234)

test_cols <- cbind(eda_dep_f$total_tar_deb, eda_dep_f$total_tar_cred)
test_cols

test_cols_sc <- scale(test_cols)  

dist_tar <- dist(test_cols, method = "euclidean")

hc_tar <- hclust(dist_tar, method = "complete")

clust_assign <- cutree(hc_tar, k = 3)
princomp(clust_assign)

clust_assign

plot(hc_tar)
dev.off()

test_cols_sc <- test_cols %>%
  as.data.frame() %>%
  mutate(cluster = clust_assign)

glimpse(test_cols_sc)

test_cols_sc <- test_cols_sc %>%
  rename(total_tar_deb = V1,
         total_tar_cred = V2)



ggplot(test_cols_sc,
       aes(total_tar_deb, total_tar_cred,
           color = factor(cluster))) +
  geom_point(position = "jitter") +
  theme_minimal() 

dev.off()


library(purrr)

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = test_cols, centers = k)
  model$tot.withinss
})
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
print(elbow_df)

ggplot(elbow_df,
       aes(x = k, y = tot_withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10)

library(cluster)

pam_k3 <- pam(test_cols, k = 3)
pam_k3$silinfo$widths

sil_plot <- silhouette(pam_k3)
plot(sil_plot)

pam_k3$silinfo$avg.width

sil_width <- map_dbl(2:10, function(k){
  model <- pam(x = test_cols, k = k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
  )

print(sil_df)

sil_df %>%
  ggplot() +
  aes(x = k, y = sil_width) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10)


## cluster plot

test_cols_sc <- cbind(eda_dep_f$region, test_cols_sc)

test_cols_sc %<>% rename(region = `eda_dep_f$region`)

test_cols_sc %<>% rename(Región = region,
                         Clúster = cluster)

test_cols_sc %<>% mutate(Clúster = factor(Clúster))

head(test_cols_sc)

ggplot(test_cols_sc,
       aes(total_tar_deb, total_tar_cred,
           color = Región)) +
  geom_point(aes(shape = Clúster)
             ) +
  theme_minimal() +
  labs(x = "Número de tarjetas de débito",
       y = "Número de tarjetas de crédito",
       title = "Clasificación de regiones de México de acuerdo con el número de tarjetas de crédito y de débito",
       subtitle = "Datos trimestrales históricos desde 2016 hasta 2020",
       caption = "Fuente: Elaboración propia con datos del Gobierno de México (2022)."
       )


dev.off()
