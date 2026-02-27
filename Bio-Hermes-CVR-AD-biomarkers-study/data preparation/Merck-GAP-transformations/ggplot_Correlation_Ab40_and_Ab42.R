library(readr)
dataset <- read_csv("~/files/Angelina_Bio_Hermes_R_code/Merck_GAP_Transformations/final_MBH_pg_mL_ABonly.csv", trim_ws = FALSE)
library(ggplot2)
ggplot(dataset, aes(x = AB40, y = AB42)) +
  labs(title = "Correlation between AB40 and AB42",
       x = "AB40",
       y = "AB42") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  geom_point(color = "grey3") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red2"
  )
result <- cor(dataset$AB40, dataset$AB42,
              use = "everything",
              method = "pearson"
)
print(result)