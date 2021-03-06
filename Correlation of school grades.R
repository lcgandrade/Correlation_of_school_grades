#library
library(tidyverse)
library(kableExtra)           #view data- kable
library(car)                  #view chart- scatter3d
library(gridExtra)            #view chart- grid.arrange
library(PerformanceAnalytics) #view chart- chart.Correlation
library(reshape2)             #view chart- melt
library(psych)                #view chart- KMO
library(rqPen)                #view chart- square
library(ggrepel)              #view chart- geom_text_repel

#load data
load("data/notasfatorial.RData")

#view data
notasfatorial %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
# estudante       notas_financas    notas_custos    notas_marketing     notas_atuarias
#
# Gabriela        5.8               4               1                   6
# Luiz Felipe     3.1               3               10                  2
# Patrícia        3.1               4               4                   4
# Gustavo         10                8               8                   8
# Letícia         3.4               2               3.2                 3.2
# ...

#analyze whether there is a correlation between costs x finances
notasfatorial %>% 
  ggplot() +
  geom_point(aes(x = notas_financas, y = notas_custos), 
             color = "dodgerblue4",
             size = 2) +
  geom_smooth(aes(x = notas_financas, y = notas_custos),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "Finance Discipline Notes",
       y = "Cost Discipline Notes") +
  theme_bw() -> A

#analyze whether there is a correlation between costs x Marketing
notasfatorial %>% 
  ggplot() +
  geom_point(aes(x = notas_custos, y = notas_marketing), 
             color = "dodgerblue4",
             size = 2) +
  geom_smooth(aes(x = notas_custos, y = notas_marketing),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "Cost Discipline Notes",
       y = "Marketing Discipline Notes") +
  theme_bw() -> B

#analyze whether there is a correlation between Marketing x Finance
notasfatorial %>% 
  ggplot() +
  geom_point(aes(x = notas_marketing, y = notas_financas), 
             color = "dodgerblue4",
             size = 2) +
  geom_smooth(aes(x = notas_marketing, y = notas_financas),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "Marketing Discipline Notes",
       y = "Finance Discipline Notes") +
  theme_bw() -> C

#view chart set
grid.arrange(A, B, C)

#create the matrix of correlations -----------------------------------
rho <- cor(notasfatorial[,2:5])

#view correlations between variables
chart.Correlation(notasfatorial[,2:5])

#create heatmap from correlations
rho %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#the KMO statistic -------------------------------------------------------
KMO(r = rho)
#   notas_financas    notas_custos    notas_marketing     notas_atuarias
#   0.81              0.69            0.21                0.73

#Bartlett's sphericity test --------------------------------------
cortest.bartlett(R = rho)
# chisq       p.value           df
# 
# 192.3685    7.978533e-39      6

#extract eigenvalues from correlation matrix  --------------------
eigenvalues_rho <- eigen(rho)

#eigenvalues
eigenvalues_rho$values
# 2.5190527 1.0004081 0.2977321 0.1828072

#adding the eigenvalues that will be equal to the amount of variables
sum(eigenvalues_rho$values)

#calculate shared variance
var_compartilhada <- (eigenvalues_rho$values/sum(eigenvalues_rho$values))
var_compartilhada
# 0.62976316 0.25010202 0.07443301 0.04570180

#the sum will be 1 or as a percentage 100%
sum(var_compartilhada)
# 1

#calculate cumulative shared variance
var_cumulativa <- cumsum(var_compartilhada)
var_cumulativa
# 0.6297632 0.8798652 0.9542982 1.0000000

#establish the maximum possible number of factors
#will always be the total number of variables in the sample
principais_componentes <- 1:sum(eigenvalues_rho$values)
principais_componentes
# 1 2 3 4

#create a database of generated insights
data.frame(principais_componentes = paste0("PC", principais_componentes),
           eigenvalue = eigenvalues_rho$values,
           var_compartilhada = var_compartilhada,
           var_cumulativa = var_cumulativa) -> relatorio_eigen
# principais                      var                 var
# componentes     eigenvalue      compartilhada       cumulativa
#
# PC1             2.5190527        0.62976316         0.6297632
# PC2             1.0004081        0.25010202         0.8798652
# PC3             0.2977321        0.07443301         0.9542982
# PC4             0.1828072        0.04570180         1.0000000

#generate report format table
relatorio_eigen %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Visually explore the sharing of shared variances between principal components
relatorio_eigen %>% 
  ggplot(aes(x = principais_componentes, 
             y = var_compartilhada,
             group = 1,
             label = paste0(round(var_compartilhada * 100,
                                  digits = 2), "%"))) +
  geom_col(fill = "dodgerblue4", color = "black") +
  geom_line(color = "darkgoldenrod3",
            size = 1.2) +
  geom_point(size = 2) +
  geom_text(size = 3, vjust = 2, color = "white") +
  labs(x = "Main Components",
       y = "Shared Variance") +
  theme_bw()

#determine the eigenvectors from the eigenvalues  ----------------
eigenvalues_rho$vectors
#   [,1]            [,2]            [,3]              [,4]
#
#   -0.56412356     0.006727733     0.8006952151      0.20151059
#   -0.58871120     0.048745833     -0.2197956321     -0.77635871
#   0.02662925      0.998736510     -0.0003555171     0.04261619
#   -0.57833780     -0.010196275    -0.5572942905     0.59568826

#visualize the weights that each variable has in each principal component 
data.frame(eigenvalues_rho$vectors) %>% 
  rename(PC1 = X1, PC2 = X2, PC3 = X3, PC4 = X4) %>% 
  mutate(var = names(notasfatorial[2:5])) %>% 
  melt(id.vars = "var") %>% 
  mutate(var = factor(var)) %>% 
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Subtitle:") +
  scale_fill_viridis_d() +
  theme_bw()

#establish the diagonal matrix of eigenvalues (L2)
#note: confirm eigenvectors with the measured eigenvalues
L2 <- diag(eigenvalues_rho$values)
L2
#   [,1]     [,2]     [,3]      [,4]
#
#   2.519053 0.000000 0.0000000 0.0000000
#   0.000000 1.000408 0.0000000 0.0000000
#   0.000000 0.000000 0.2977321 0.0000000
#   0.000000 0.000000 0.0000000 0.1828072

#through the calculated eigenvectors, we can prove that V'.rho.V = L2
prova_01 <- t(eigenvalues_rho$vectors) %*% rho %*% eigenvalues_rho$vectors
round(x = prova_01,
      digits = 14)
#   [,1]     [,2]     [,3]      [,4]
#
#   2.519053 0.000000 0.0000000 0.0000000
#   0.000000 1.000408 0.0000000 0.0000000
#   0.000000 0.000000 0.2977321 0.0000000
#   0.000000 0.000000 0.0000000 0.1828072

#calculate factor scores
#remember
eigenvalues_rho$values
# 2.5190527 1.0004081 0.2977321 0.1828072
eigenvalues_rho$vectors
#   [,1]            [,2]            [,3]              [,4]
#
#   -0.56412356     0.006727733     0.8006952151      0.20151059
#   -0.58871120     0.048745833     -0.2197956321     -0.77635871
#   0.02662925      0.998736510     -0.0003555171     0.04261619
#   -0.57833780     -0.010196275    -0.5572942905     0.59568826

#we can separately calculate the Factor Scores for each factor
#factor scores of the first factor
eigenvalues_rho$vectors[1,1] / sqrt(eigenvalues_rho$values[1])
# -0.3554313
eigenvalues_rho$vectors[2,1] / sqrt(eigenvalues_rho$values[1])
# -0.3709229
eigenvalues_rho$vectors[3,1] / sqrt(eigenvalues_rho$values[1])
# 0.016778
eigenvalues_rho$vectors[4,1] / sqrt(eigenvalues_rho$values[1])
# -0.3643871

#but the simplified form will be
scores_fatoriais <- t(eigenvalues_rho$vectors)/sqrt(eigenvalues_rho$values)
scores_fatoriais
#       [,1]          [,2]          [,3]            [,4]
#
# [1,] -0.355431256   -0.37092293   0.0167780019    -0.36438707
# [2,]  0.006726361   0.04873589    0.9985327957    -0.01019419
# [3,]  1.467419996   -0.40281558   -0.0006515498   -1.02134342
# [4,]  0.471304085   -1.81579060   0.0996730947    1.39322858

#calculate the factors
#standardize the database
notasfatorial_std <- notasfatorial %>% 
  column_to_rownames("estudante") %>% 
  scale() %>% 
  data.frame()
#                 notas           notas           notas           notas
#                 financas        custos          marketing       atuarias
#
#   Gabriela      -0.01088784     -0.290635747    -1.65009455     0.27297206
#   Luiz Felipe   -0.87551073     -0.697460700    1.53173065      -1.31870176
#   Patrícia      -0.87551073     -0.290635747    -0.58948615     -0.52286485
#   Gustavo       1.33408108      1.336664067     0.82465838      1.06880897

#create object to use as a receptacle for the k calculated factors
fatores <- list()

for(i in 1:nrow(scores_fatoriais)){
  fatores[[i]] <- rowSums(x = sweep(x = notasfatorial_std, 
                                    MARGIN = 2, 
                                    STATS = scores_fatoriais[i,], 
                                    FUN = `*`))
}

#transform factors object into dataframe
fatores_df <- data.frame((sapply(X = fatores, FUN = c)))
fatores_df

fatores_df %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#check if the calculated factors are orthogonal to each other
round(x = cor(fatores_df), 
      digits = 14)
#     X1  X2  X3  X4
#X1   1   0   0   0
#X2   0   1   0   0
#X3   0   0   1   0
#X4   0   0   0   1

#calculate factor loadings ------------------------
#Note: join original base with 'fatores_df' object:
notasfatorial_final <-  cbind(notasfatorial,
                              fatores_df) %>% 
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) 
# estudante       notas_financas ...  F1            F2              F3              F4 
#
# Gabriela        5.8                 -0.01547944   -1.66469388     -0.1766275      0.73844462           
# Luiz Felipe     3.1                 1.07610529    1.50304602      0.3420554       -0.83076984         
# Patrícia        3.1                 0.59962213    -0.60334446     -0.6332607      -0.67212429           
# Gustavo         10                  -1.34559751   0.88666982      0.3270698       -0.22705272      
# Letícia         3.4                 0.97852930    -0.92152099     0.1607774       0.37886785
# ...

#calculate correlations between original variables and factors
correlacoes_entre_fatores <- cor(notasfatorial_final[,2:9])
#                     notas         notas       ...      
#                     financas      custos      ...   F3                F4
#
# notas_financas     1.000000000    0.755923392       4.368980e-01      8.615779e-02 
# notas_custos       0.755923392    1.000000000       -1.199311e-01     -3.319396e-01
# ...
# F3                 0.436897977    -0.119931111      1.000000e+00      -8.530911e-16
# F4                 0.086157787    -0.331939621      -8.530911e-16     1.000000e+00

#visualize correlations between the original variables and factors 
correlacoes_entre_fatores %>% 
  melt() %>% 
  filter(Var1 %in% c("F1","F2","F3","F4") &
           Var2 %in% c("notas_financas","notas_custos",
                       "notas_marketing","notas_atuarias")) %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#report
correlacoes_entre_fatores %>% 
  melt() %>% 
  filter(Var1 %in% c("F1","F2","F3","F4") &
           Var2 %in% c("notas_financas","notas_custos",
                       "notas_marketing","notas_atuarias")) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") -> correlacoes_entre_fatores_df

#add squares of the factor loadings of the object_correlations_between_factors_df 
# the result must be the same as the eigenvalues
correlacoes_entre_fatores_df %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#       notas         notas         notas           notas
#       financas      custos        marketing       atuarias
# F1   -0.895350050   -0.93437438    0.0422646704    -0.91791021
# F2    0.006729105    0.04875578    0.9989402668    -0.01019835
# F3    0.436897977   -0.11993111   -0.0001939873    -0.30408668
# F4    0.086157787   -0.33193962    0.0182209608     0.25469224

correlacoes_entre_fatores_df %>% 
  mutate(eigenvalues = notas_financas^2 + notas_custos^2 +
           notas_marketing^2 + notas_atuarias^2) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#       notas         notas         notas           notas
#       financas      custos        marketing       atuarias        eigenvalues
# F1   -0.895350050   -0.93437438    0.0422646704    -0.91791021    2.5190527
# F2    0.006729105    0.04875578    0.9989402668    -0.01019835    1.0004081
# F3    0.436897977   -0.11993111   -0.0001939873    -0.30408668    0.2977321
# F4    0.086157787   -0.33193962    0.0182209608     0.25469224    0.1828072

correlacoes_entre_fatores_df %>% 
  mutate(eigenvalues = notas_financas^2 + notas_custos^2 +
           notas_marketing^2 + notas_atuarias^2) %>% 
  filter(eigenvalues > 1) %>% 
  select(-eigenvalues) %>% 
  t() %>% 
  data.frame() %>% 
  square() %>% 
  mutate(comunalidades = rowSums(.))
#                   F1            F2              comunalidades
#
#   notas_financas  0.801651713   4.528086e-05    0.8016970
#   notas_custos    0.873055491   2.377126e-03    0.8754326
#   notas_marketing 0.001786302   9.978817e-01    0.9996680
#   notas_atuarias  0.842559151   1.040064e-04    0.8426632

#generate plot
correlacoes_entre_fatores_df %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column("variáveis") %>% 
  ggplot(aes(x = F1, y = F2, label = variáveis)) +
  geom_point(color = "dodgerblue4",
             size = 2) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  labs(x = paste("Dimension 1", paste0("(",round(var_compartilhada[1] * 100, 
                                                digits = 2),"%)")),
       y = paste("Dimension 2", paste0("(",round(var_compartilhada[2] * 100, 
                                                digits = 2),"%)"))) +
  theme_bw()
