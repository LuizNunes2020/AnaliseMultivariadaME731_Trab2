#Pacotes Utilizados
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(car)
library(readr)
library(MASS)
library(lmtest)

# Carregar o banco de dados
db <- read.csv(file.choose())

# Converter colunas de porcentagem para numéricas e tratar valores "clutch_wins" adequadamente
db <- db %>%
  mutate(
    kill_assists_survived_traded = as.numeric(sub("%", "", kill_assists_survived_traded)) / 100,
    headshot_percentage = as.numeric(sub("%", "", headshot_percentage)) / 100,
    clutch_success_percentage = as.numeric(sub("%", "", clutch_success_percentage)) / 100,
    clutch_wins = as.numeric(sapply(strsplit(clutch_wins, "/"), function(x) as.numeric(x[1]) / as.numeric(x[2])))
  )

# Remover linhas com valores ausentes
db <- na.omit(db)
summary(db)


# Visualização da distribuição das variáveis 
num_vars <- db %>% select_if(is.numeric)

# Histograms
num_vars %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  labs(title = "Histogramas das Variáveis Numéricas")

# Boxplots
num_vars %>%
  gather(key = "Variável", value = "Valor") %>%
  ggplot(aes(y = Valor, x = reorder(Variável, Valor, median))) +
  geom_boxplot(fill = "#69b3a2", color = "#1f4e5f", outlier.color = "red", outlier.shape = 21) +
  coord_flip() +
  labs(
    title = "Distribuição das Variáveis Numéricas com Outliers Destacados",
    x = "Variáveis",
    y = "Valores"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "italic"),
    axis.title.x = element_text(face = "italic"),
    panel.grid.major.y = element_line(size = 0.1, color = "grey80"),
    panel.grid.minor = element_blank()
  )



# Gráfico de análise bivariada entre rating e outras variáveis numéricas 
num_vars %>%
  gather(key = "Variável", value = "Valor", -rating) %>%
  ggplot(aes(x = Valor, y = rating)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Variável, scales = "free_x") +
  labs(title = "Análise Bivariada entre Rating e Outras Variáveis") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Gráfico de análise bivariada entre kill_deaths e outras variáveis numéricas 
num_vars %>%
  pivot_longer(cols = -kill_deaths, names_to = "Variável", values_to = "Valor") %>%
  ggplot(aes(x = Valor, y = kill_deaths)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Variável, scales = "free_x") +
  labs(title = "Análise Bivariada entre Kill/Deaths e Outras Variáveis", y = "Kill/Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Gráfico de análise bivariada entre average_combat_score e outras variáveis numéricas 
num_vars %>%
  pivot_longer(cols = -average_combat_score, names_to = "Variável", values_to = "Valor") %>%
  ggplot(aes(x = Valor, y = average_combat_score)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Variável, scales = "free_x") +
  labs(title = "Análise Bivariada entre Average Combat Score e Outras Variáveis", y = "Average Combat Score") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Gráfico de análise bivariada entre average_damage_per_round e outras variáveis numéricas 
num_vars %>%
  pivot_longer(cols = -average_damage_per_round, names_to = "Variável", values_to = "Valor") %>%
  ggplot(aes(x = Valor, y = average_damage_per_round)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Variável, scales = "free_x") +
  labs(title = "Análise Bivariada entre Average Damage per Round e Outras Variáveis", y = "Average Damage per Round") +
  theme_minimal() +
  theme(axis.text.x = element_blank())



# Selecionar apenas as variáveis independentes para o PCA, excluindo variáveis dependentes e identificadores
predictor_vars <- db %>% dplyr::select(-player, -org, -kill_deaths, -average_damage_per_round, -average_combat_score, -rating)

# Realizar o PCA nas variáveis independentes
pca_result <- prcomp(predictor_vars, center = TRUE, scale. = TRUE)
summary(pca_result)

# Calcular o VIF para cada variável preditora
vif_model <- lm(as.formula(paste("kill_deaths ~", paste(names(predictor_vars), collapse = " + "))), data = db)
vif_values <- car::vif(vif_model)
vif_values

# Calcular a variância explicada por cada componente principal
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)

# Criar um dataframe para o scree plot
scree_plot_data <- data.frame(
  PC = 1:length(explained_variance),
  Variance = explained_variance,
  CumulativeVariance = cumulative_variance
)

# Gerar o scree plot
ggplot(scree_plot_data, aes(x = PC)) +
  geom_bar(aes(y = Variance), stat = "identity", fill = "skyblue", color = "black") +
  geom_line(aes(y = CumulativeVariance), color = "red", size = 1, linetype = "dashed") +
  geom_point(aes(y = CumulativeVariance), color = "red", size = 2) +
  geom_text(aes(y = Variance, label = sprintf("%.2f%%", Variance * 100)), 
            vjust = -0.5, color = "black", size = 3) + # Rótulos nas barras
  geom_text(aes(y = CumulativeVariance, label = sprintf("%.2f%%", CumulativeVariance * 100)), 
            vjust = -1, hjust = -0.1, color = "red", size = 3) + # Rótulos nos pontos
  labs(
    title = "Scree Plot",
    x = "Componentes Principais",
    y = "Variância Explicada",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    axis.title.x = element_text(face = "italic"),
    axis.title.y = element_text(face = "italic")
  )

# Extrair os scores das seis primeiras componentes principais
pca_scores <- as.data.frame(pca_result$x[, 1:6])  # Extrai apenas PC1, PC2, PC3, PC4, PC5 e PC6
colnames(pca_scores) <- paste0("PC", 1:6)

# Adicionar as variáveis dependentes ao conjunto de dados de componentes principais
pca_scores <- cbind(pca_scores, db %>% dplyr::select(kill_deaths, average_damage_per_round, average_combat_score, rating))

# Análise de correlação entre as componentes principais e as variáveis dependentes
GGally::ggcorr(pca_scores, label = TRUE, label_round = 2, label_alpha = TRUE, main = "Correlação entre Componentes Principais e Variáveis Dependentes")

# Executar a regressão multivariada 
mlm_valorant_pca <- lm(cbind(kill_deaths, average_damage_per_round, average_combat_score, rating) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = pca_scores)
summary(mlm_valorant_pca)

# Teste de MANOVA 
manova_result <- manova(cbind(kill_deaths, average_damage_per_round, average_combat_score, rating) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = pca_scores)

# Teste de MANOVA com diferentes testes de significância
summary(manova_result, test = "Pillai")           # Teste de Pillai
summary(manova_result, test = "Wilks")            # Teste de Wilks' Lambda
summary(manova_result, test = "Hotelling-Lawley") # Teste de Hotelling-Lawley
summary(manova_result, test = "Roy")              # Teste de Roy's Greatest Root


#Extração dos resíduos do modelo
residuals_df <- as.data.frame(residuals(mlm_valorant_pca))

#Teste de Normalidade dos Resíduos
shapiro_test_results <- apply(residuals(mlm_valorant_pca), 2, shapiro.test)
shapiro_test_results  # Teste de Shapiro-Wilk para cada variável dependente

#Teste de Homocedasticidade (Teste de variância constante)
ncv_test_results <- apply(residuals(mlm_valorant_pca), 2, function(x) car::ncvTest(lm(x ~ 1)))
ncv_test_results  # Teste de homogeneidade de variância para cada variável dependente


# Histograma dos resíduos para "kill_deaths"
ggplot(residuals_df, aes(x = kill_deaths)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma dos Resíduos - Kill/Deaths", x = "Resíduos", y = "Densidade") +
  theme_minimal()

# Histograma dos resíduos para "average_damage_per_round"
ggplot(residuals_df, aes(x = average_damage_per_round)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma dos Resíduos - Average Damage per Round", x = "Resíduos", y = "Densidade") +
  theme_minimal()

# Histograma dos resíduos para "average_combat_score"
ggplot(residuals_df, aes(x = average_combat_score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma dos Resíduos - Average Combat Score", x = "Resíduos", y = "Densidade") +
  theme_minimal()

# Histograma dos resíduos para "rating"
ggplot(residuals_df, aes(x = rating)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma dos Resíduos - Rating", x = "Resíduos", y = "Densidade") +
  theme_minimal()


# Q-Q Plot dos resíduos para cada variável dependente
qqnorm(residuals_df$kill_deaths, main = "Q-Q Plot dos Resíduos - Kill/Deaths")
qqline(residuals_df$kill_deaths, col = "red")

qqnorm(residuals_df$average_damage_per_round, main = "Q-Q Plot dos Resíduos - Average Damage per Round")
qqline(residuals_df$average_damage_per_round, col = "red")

qqnorm(residuals_df$average_combat_score, main = "Q-Q Plot dos Resíduos - Average Combat Score")
qqline(residuals_df$average_combat_score, col = "red")

qqnorm(residuals_df$rating, main = "Q-Q Plot dos Resíduos - Rating")
qqline(residuals_df$rating, col = "red")


# Extraindo os valores ajustados e os resíduos para cada variável dependente
fitted_values <- as.data.frame(fitted(mlm_valorant_pca))
residuals_df <- as.data.frame(residuals(mlm_valorant_pca))
diagnostics_data <- bind_cols(fitted_values, residuals_df)
colnames(diagnostics_data) <- c("fitted_kill_deaths", "fitted_average_damage_per_round", 
                                "fitted_average_combat_score", "fitted_rating", 
                                "resid_kill_deaths", "resid_average_damage_per_round", 
                                "resid_average_combat_score", "resid_rating")


# Gráfico para cada variável dependente
for (response in c("kill_deaths", "average_damage_per_round", "average_combat_score", "rating")) {
  p <- ggplot(diagnostics_data, aes_string(x = paste("fitted_", response, sep = ""), 
                                           y = paste("resid_", response, sep = ""))) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Resíduos vs Valores Ajustados -", response), 
         x = "Valores Ajustados", y = "Resíduos") +
    theme_minimal()
  
  print(p)  
}

# Verificar a matriz de covariância dos resíduos para avaliar a correlação entre as variáveis de resposta
# Isto corresponde à verificação de que os erros associados às variáveis dependentes podem estar correlacionados.
residuals_covariance <- cov(residuals(mlm_valorant_pca))
print(residuals_covariance)

# Calcular intervalos de confiança para as previsões de cada variável de resposta
# Isso cria intervalos de confiança para cada variável dependente com base nos preditores do modelo.
confidence_intervals <- predict(mlm_valorant_pca, interval = "confidence")
confidence_intervals_df <- as.data.frame(confidence_intervals)
print(head(confidence_intervals_df))

# Verificação da linearidade entre as principais componentes e as variáveis de resposta
# Aqui geramos gráficos para observar a linearidade entre as componentes principais (PCs) e as variáveis dependentes ajustadas.
# Isso pode indicar a adequação de um modelo linear.
# Adicionar as componentes principais ao dataframe de diagnósticos
diagnostics_data <- cbind(diagnostics_data, pca_scores[, c("PC1", "PC2")])

# Verificação da linearidade entre as principais componentes e as variáveis de resposta
for (response in c("kill_deaths", "average_damage_per_round", "average_combat_score", "rating")) {
  fitted_col <- paste("fitted_", response, sep = "")
  
  # Linearidade com PC1
  p1 <- ggplot(diagnostics_data, aes_string(x = "PC1", y = fitted_col)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Linearidade entre PC1 e Valores Ajustados -", response), 
         x = "PC1", y = "Valores Ajustados") +
    theme_minimal()
  print(p1)  
  
  # Linearidade com PC2
  p2 <- ggplot(diagnostics_data, aes_string(x = "PC2", y = fitted_col)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Linearidade entre PC2 e Valores Ajustados -", response), 
         x = "PC2", y = "Valores Ajustados") +
    theme_minimal()
  print(p2)  
}



# Teste de independência dos resíduos 
independence_test_results <- list()
for (response in c("kill_deaths", "average_damage_per_round", "average_combat_score", "rating")) {
  model <- lm(as.formula(paste(response, "~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6")), data = pca_scores)
  independence_test_results[[response]] <- dwtest(model)
}
print(independence_test_results)


# Teste de Razão de Verossimilhança para Parâmetros de Regressão
# Comparar o modelo completo com um modelo reduzido (sem algumas PCs) e calcular a razão de verossimilhança.

# Calcular a soma de quadrados residuais 
residuals_full <- residuals(mlm_valorant_pca)
ssr_full <- sum(residuals_full^2)

# Ajustar o modelo reduzido, removendo algumas componentes principais
mlm_valorant_pca_reduced <- lm(cbind(kill_deaths, average_damage_per_round, average_combat_score, rating) ~ PC1 + PC2, data = pca_scores)

# Calcular a soma de quadrados residuais para o modelo reduzido
residuals_reduced <- residuals(mlm_valorant_pca_reduced)
ssr_reduced <- sum(residuals_reduced^2)

# Calcular a estatística de razão de verossimilhança
LR_statistic <- -2 * (ssr_full - ssr_reduced)
LR_statistic

# Calcular o valor-p para a estatística de teste com base nos graus de liberdade
# Graus de liberdade = diferença no número de parâmetros estimados entre os modelos completo e reduzido
df <- 4  # ajuste conforme necessário para o número de PCs removidos
p_value <- pchisq(LR_statistic, df = df, lower.tail = FALSE)
print(LR_statistic)
print(p_value)



# Defina valores numéricos específicos para PC1, PC2, PC3, PC4, PC5, e PC6
new_data <- data.frame(PC1 = 0.5, PC2 = -0.3, PC3 = 0.2, PC4 = -0.1, PC5 = 0.4, PC6 = -0.2)

# Calcule as previsões com intervalos de previsão para as novas observações
prediction <- predict(mlm_valorant_pca, newdata = new_data, interval = "prediction")
print(prediction)





# Extrair as combinações lineares (ou "loadings") das variáveis para cada PC
loadings <- pca_result$rotation
loadings_df <- as.data.frame(loadings)
loadings_df <- rownames_to_column(loadings_df, var = "Variável")

loadings_long <- loadings_df %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Componente", values_to = "Carga")

# Gráfico de barras para os loadings dos primeiros componentes
ggplot(loadings_long %>% filter(Componente %in% c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")), 
       aes(x = reorder(Variável, abs(Carga)), y = Carga, fill = Componente)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Cargas das Variáveis em Cada Componente Principal", x = "Variável", y = "Carga") +
  theme_minimal()


# Scatter plots dos pares dos residuos
pairs(residuals_df, main = "Scatter Plots dos Pares dos Resíduos", pch = 21, bg = "lightblue")




