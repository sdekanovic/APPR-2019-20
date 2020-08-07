# 4. faza: Analiza podatkov
# Logistična regresija

df <- tabela2 %>% filter(!is.na(Prihranki))

prevediOcena <- function(beseda) {
  if (beseda == "dober") {
    return(1)
  } else {
    return(0)
  }
}

df$Ocena <- df$Ocena %>% lapply(prevediOcena) %>% unlist() %>% as.factor()

# train set, test set
indeksi <- createDataPartition(df$Ocena, p = 0.8, list = FALSE)
ucna <- df[indeksi, ]
testna <- df[-indeksi, ]

model1 <- glm(formula = Ocena ~ . - ID, family = binomial(link = logit), data = ucna)
#coef(summary(model1))[, 4] < 0.05

# statistično značilne spremenljivke
statSignificence <- as.logical(coef(summary(model1))[, 4] < 0.05)
statSignificence <- names(model1$coefficients[statSignificence])

df2 <- as.data.frame(coef(summary(model1)))
rownames(df2)[rownames(df2) == "`Velikost kredita`"] = "Velikost kredita"

napoved <- model1 %>% predict(testna, type = "response")
napoved[napoved > 0.5] = 1
napoved[napoved <= 0.5] = 0

tocnost <- mean(napoved == testna$Ocena)

