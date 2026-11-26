#==============================================================================
# Hiato salarial de gênero no Brasil - PNAD Contínua, 2º trimestre de 2026
# Trabalho de Econometria I - Aline Oliveira Freitas, Laura Bezerra Capistrano
#==============================================================================

#------------------------------- 0. PARÂMETROS --------------------------------
CAMINHO_MICRODADOS <- "C:/Users/aline/OneDrive/Documentos/Econometria I/Trabalho de Econometria I/PNADC_022026/PNADC_022026.txt"
CAMINHO_INPUT      <- "C:/Users/aline/OneDrive/Documentos/Econometria I/Trabalho de Econometria I/Dicionario_e_input_20221031/input_PNADC_trimestral.txt"
PASTA_SAIDA        <- "resultados"
dir.create(PASTA_SAIDA, showWarnings = FALSE, recursive = TRUE)

#------------------------------- 1. PACOTES -----------------------------------
library(lmtest)
library(sandwich)
library(car)
library(PNADcIBGE)
library(modelsummary)

#------------------------------- 2. LEITURA DOS DADOS -------------------------
dicionario <- c(
  UF     = "uf",
  V1022  = "situacao",
  V1028  = "peso",
  V2007  = "sexo",
  V2009  = "idade",
  V2010  = "cor_cod",
  VD3004 = "nivel_cod",
  VD3005 = "educ",
  VD4002 = "ocupado",
  VD4009 = "posicao",
  VD4011 = "ocup_cod",
  VD4019 = "renda",
  VD4031 = "horas")

dados_pnad <- read_pnadc(microdata = CAMINHO_MICRODADOS,
                         input_txt = CAMINHO_INPUT,
                         vars = names(dicionario))

names(dados_pnad)[match(names(dicionario), names(dados_pnad))] <- dicionario
for (v in dicionario) dados_pnad[[v]] <- as.numeric(as.character(dados_pnad[[v]]))
cat("Registros lidos:", nrow(dados_pnad), "\n")

#------------------------------- 3. CONSTRUÇÃO DA BASE ------------------------
base <- as.data.frame(dados_pnad[, c("uf", "peso", "sexo", "idade", "cor_cod", "nivel_cod",
                                     "educ", "ocupado", "posicao", "ocup_cod", "renda",
                                     "horas", "situacao")])

# Amostra: ocupados de 18 a 65 anos, com rendimento e horas positivos
base <- subset(base, ocupado == 1 & idade >= 18 & idade <= 65 &
                 renda > 0 & horas > 0 & !is.na(educ))

# Variável dependente: salário por hora (rendimento mensal / horas mensais)
base$salario_hora <- base$renda / (base$horas * 4.33)

# Remove 0,5% de cada cauda do salário-hora (erros de declaração)
limites <- quantile(base$salario_hora, c(0.005, 0.995))
base <- subset(base, salario_hora >= limites[1] & salario_hora <= limites[2])
base$log_sh <- log(base$salario_hora)

# Variável de interesse: dummy mulher
base$mulher <- ifelse(base$sexo == 2, 1, 0)
base$sexo_f <- factor(base$mulher, levels = c(0, 1), labels = c("Homens", "Mulheres"))

# Experiência potencial = idade - anos de estudo - 6
base$exper <- pmax(base$idade - base$educ - 6, 0)
base$exper2 <- base$exper^2 / 100

# Dummies e fatores
base$superior <- ifelse(base$nivel_cod == 7, 1, 0)
base$urbano   <- ifelse(base$situacao == 1, 1, 0)
base$formal   <- ifelse(base$posicao %in% c(1, 3, 5, 7), 1, 0)
base$cor <- factor(base$cor_cod, levels = c(1, 2, 3, 4, 5, 9),
                   labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Ignorado"))
base$cor <- droplevels(base$cor)
base$nivel <- factor(base$nivel_cod, levels = 1:7,
                     labels = c("Sem instrução", "Fund. incompleto", "Fund. completo",
                                "Médio incompleto", "Médio completo",
                                "Superior incompleto", "Superior completo"))
base$regiao <- factor(floor(base$uf / 10), levels = c(3, 1, 2, 4, 5),
                      labels = c("Sudeste", "Norte", "Nordeste", "Sul", "Centro-Oeste"))
base$ocupacao <- factor(base$ocup_cod, levels = c(5, 1:4, 6:11),
                        labels = c("Serviços e vendedores", "Diretores e gerentes",
                                   "Profissionais das ciências", "Técnicos de nível médio",
                                   "Apoio administrativo", "Agropecuária qualificada",
                                   "Operários e artesãos", "Operadores de máquinas",
                                   "Ocupações elementares", "Forças armadas e polícia",
                                   "Mal definidas"))
base$ocupacao <- droplevels(base$ocupacao)

cat("Observações na amostra final:", nrow(base), "\n")
write.csv2(base[, c("mulher", "idade", "educ", "exper", "nivel_cod", "superior", "cor_cod",
                    "urbano", "uf", "ocup_cod", "formal", "renda", "horas",
                    "salario_hora", "log_sh", "peso")],
           file.path(PASTA_SAIDA, "base_final.csv"), row.names = FALSE)

#------------------------------- 4. ESTATÍSTICAS DESCRITIVAS ------------------
descritivas <- datasummary(salario_hora + log_sh + renda + horas + idade + educ + exper +
                             superior + urbano + formal ~ N + Mean + Var + SD + Min + Max +
                             sexo_f * Mean, data = base, fmt = 3, output = "data.frame")
descritivas
write.csv2(descritivas, file.path(PASTA_SAIDA, "tab1_descritivas.csv"), row.names = FALSE)

# Número de observações por sexo
table(base$sexo_f)

# Diferença de médias do log do salário-hora entre os sexos
t.test(log_sh ~ sexo_f, data = base)

# Correlações
base$negro <- ifelse(base$cor %in% c("Preta", "Parda"), 1, 0)
correl <- cor(base[, c("log_sh", "mulher", "educ", "exper", "idade", "negro",
                       "urbano", "formal", "horas")])
correl <- round(correl, 3)
correl
write.csv2(correl, file.path(PASTA_SAIDA, "tab2_correlacoes.csv"))

# Média do salário-hora por categoria e sexo
media_por <- function(cat) {
  t <- tapply(base$salario_hora, list(base[[cat]], base$sexo_f), mean)
  t <- as.data.frame(t)
  t$razao_M_H <- t$Mulheres / t$Homens
  t$n <- as.vector(table(base[[cat]]))
  t
}
(tab_nivel    <- media_por("nivel"))
(tab_ocupacao <- media_por("ocupacao"))
write.csv2(tab_nivel,    file.path(PASTA_SAIDA, "tab3_media_nivel.csv"))
write.csv2(tab_ocupacao, file.path(PASTA_SAIDA, "tab3_media_ocupacao.csv"))

# Distribuição: proporção de cada sexo por nível de instrução
dist_nivel <- round(prop.table(table(base$nivel, base$sexo_f), 2) * 100, 1)
dist_nivel
write.csv2(dist_nivel, file.path(PASTA_SAIDA, "tab3_distribuicao_nivel.csv"))

#------------------------------- 5. GRÁFICOS ----------------------------------
cores <- c(Homens = "#2a78d6", Mulheres = "#eb6834")
set.seed(123)
amostra_graf <- base[sample(nrow(base), 6000), ]   # subamostra só para os gráficos de pontos

# Figura 1: distribuição do log do salário-hora por sexo
png(file.path(PASTA_SAIDA, "fig1_distribuicao.png"), width = 1600, height = 1000, res = 200)
dens_h <- density(base$log_sh[base$mulher == 0], adjust = 1.5)
dens_m <- density(base$log_sh[base$mulher == 1], adjust = 1.5)
plot(dens_h, col = cores["Homens"], lwd = 2, main = "",
     xlab = "Log do salário-hora (R$)", ylab = "Densidade",
     ylim = c(0, max(dens_h$y, dens_m$y)))
lines(dens_m, col = cores["Mulheres"], lwd = 2)
abline(v = log(1621 / (40 * 4.33)), lty = 2, col = "grey40")
text(log(1621 / (40 * 4.33)), max(dens_m$y) * 0.95, "salário mínimo\n(40 h/semana)",
     pos = 2, cex = 0.7, col = "grey30")
legend("topright", legend = names(cores), col = cores, lwd = 2, bty = "n")
dev.off()

# Figura 2: salário-hora médio por nível de instrução e sexo
png(file.path(PASTA_SAIDA, "fig2_nivel_instrucao.png"), width = 1800, height = 1000, res = 200)
par(mar = c(4, 4.5, 1, 1))
rotulos <- c("Sem\ninstrução", "Fund.\nincompleto", "Fund.\ncompleto", "Médio\nincompleto",
             "Médio\ncompleto", "Superior\nincompleto", "Superior\ncompleto")
bp <- barplot(t(as.matrix(tab_nivel[, c("Homens", "Mulheres")])), beside = TRUE,
              col = cores, border = NA, names.arg = rotulos, cex.names = 0.65, padj = 0.5,
              ylab = "Salário-hora médio (R$)", ylim = c(0, max(tab_nivel$Homens) * 1.15))
text(bp[2, ], tab_nivel$Mulheres, sprintf("%.2f", tab_nivel$razao_M_H),
     pos = 3, cex = 0.65, col = "grey30")
legend("topleft", legend = names(cores), fill = cores, border = NA, bty = "n")
dev.off()

# Figura 4: anos de estudo x log do salário-hora, com retas por sexo (interação)
mod_graf <- lm(log_sh ~ educ * mulher, data = base)
png(file.path(PASTA_SAIDA, "fig4_interacao_educ.png"), width = 1600, height = 1100, res = 200)
plot(jitter(amostra_graf$educ, 1.5), amostra_graf$log_sh, pch = 20, cex = 0.4,
     col = adjustcolor(cores[as.character(amostra_graf$sexo_f)], 0.35),
     xlab = "Anos de estudo", ylab = "Log do salário-hora (R$)")
cf <- coef(mod_graf)
abline(a = cf[1], b = cf[2], col = cores["Homens"], lwd = 3)
abline(a = cf[1] + cf[3], b = cf[2] + cf[4], col = cores["Mulheres"], lwd = 3)
legend("topleft", legend = names(cores), col = cores, lwd = 3, bty = "n")
dev.off()

#------------------------------- 6. REGRESSÕES POR MQO ------------------------
# (1) Regressão simples: hiato bruto
mod1 <- lm(log_sh ~ mulher, data = base)
summary(mod1)

# (2) + capital humano: anos de estudo, experiência e experiência ao quadrado
mod2 <- lm(log_sh ~ mulher + educ + exper + exper2, data = base)
summary(mod2)

# (3) + cor, situação urbana e região
mod3 <- lm(log_sh ~ mulher + educ + exper + exper2 + cor + urbano + regiao,
           data = base)
summary(mod3)

# (4) + grupamento ocupacional
mod4 <- lm(log_sh ~ mulher + educ + exper + exper2 + cor + urbano + regiao +
             ocupacao, data = base)
summary(mod4)

# (5) Interação mulher x anos de estudo, sem controle de ocupação
mod5 <- lm(log_sh ~ mulher * educ + exper + exper2 + cor + urbano + regiao, data = base)
summary(mod5)

# (6) Interação mulher x anos de estudo, com controle de ocupação
mod6 <- lm(log_sh ~ mulher * educ + exper + exper2 + cor + urbano + regiao +
             ocupacao, data = base)
summary(mod6)

# Colinearidade perfeita: experiência = idade - educ - 6.
mod_colinear <- lm(log_sh ~ mulher + educ + idade + I(idade - educ - 6), data = base)
coef(mod_colinear)

#------------------------------- 7. ERROS-PADRÃO ROBUSTOS (HC1) ---------------
modelos <- list("(1)" = mod1, "(2)" = mod2, "(3)" = mod3, "(4)" = mod4, "(5)" = mod5,
                "(6)" = mod6)
robustos <- lapply(modelos, function(m) coeftest(m, vcov. = vcovHC(m, type = "HC1")))
robustos

# Comparação dos erros-padrão convencionais e robustos no modelo (4)
ep_comp <- data.frame(
  convencional = sqrt(diag(vcov(mod4))),
  robusto_HC1  = sqrt(diag(vcovHC(mod4, type = "HC1"))))
ep_comp <- round(ep_comp, 5)
ep_comp

# Tabela de regressões com erros-padrão robustos
efeito <- sapply(modelos, function(m) sprintf("%.1f", 100 * (exp(coef(m)["mulher"]) - 1)))
extras <- data.frame(c("Cor/raça", "Região", "Ocupação", "Efeito de ser mulher (%)"),
                     rbind(c("Não", "Não", "Sim", "Sim", "Sim", "Sim"),
                           c("Não", "Não", "Sim", "Sim", "Sim", "Sim"),
                           c("Não", "Não", "Não", "Sim", "Não", "Sim"),
                           efeito))
names(extras) <- c("term", names(modelos))   # linhas extras: mesmos nomes de colunas da tabela
tab_reg <- modelsummary(modelos, vcov = "HC1", fmt = 4,
                        stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
                        coef_omit = "^cor(Amarela|Indigena|Ignorado)|^regiao|^ocupacao",
                        gof_map = c("nobs", "r.squared", "adj.r.squared"),
                        add_rows = extras, output = "data.frame")
tab_reg
write.csv2(tab_reg, file.path(PASTA_SAIDA, "tab4_regressoes.csv"))

# Figura 3: coeficiente de mulher em cada modelo, com IC de 95% (HC1)
efeito <- sapply(robustos[1:4], function(ct) ct["mulher", 1])
ep     <- sapply(robustos[1:4], function(ct) ct["mulher", 2])
pct    <- 100 * (exp(efeito) - 1)
li     <- 100 * (exp(efeito - 1.96 * ep) - 1)
ls     <- 100 * (exp(efeito + 1.96 * ep) - 1)
png(file.path(PASTA_SAIDA, "fig3_coeficientes.png"), width = 1600, height = 950, res = 200)
plot(1:4, pct, pch = 19, cex = 1.3, col = cores["Mulheres"], xaxt = "n", xlim = c(0.6, 4.6),
     ylim = c(min(li) - 2, 0), xlab = "", ylab = "Diferença salarial das mulheres (%)")
arrows(1:4, li, 1:4, ls, angle = 90, code = 3, length = 0.05, col = cores["Mulheres"], lwd = 2)
abline(h = 0, col = "grey40")
text(1:4, pct, sprintf("%.1f%%", pct), pos = 4, cex = 0.8)
axis(1, at = 1:4, cex.axis = 0.75,
     labels = c("(1) Sem controles", "(2) + Educ. e exper.", "(3) + Cor, urbano, região",
                "(4) + Ocupação"))
dev.off()

#------------------------------- 8. TESTES DE HIPÓTESES -----------------------
# Teste F 1: capital humano conjuntamente nulo
F_capital <- linearHypothesis(mod4, c("educ = 0", "exper = 0", "exper2 = 0"), white.adjust = "hc1")
F_capital

# Teste F 2: as dummies de ocupação são conjuntamente nulas?
nomes_ocup <- grep("^ocupacao", names(coef(mod4)), value = TRUE)
F_ocupacao <- linearHypothesis(mod4, paste(nomes_ocup, "= 0"), white.adjust = "hc1")
F_ocupacao

# Teste F 3: as dummies de região são conjuntamente nulas?
nomes_reg <- grep("^regiao", names(coef(mod4)), value = TRUE)
F_regiao <- linearHypothesis(mod4, paste(nomes_reg, "= 0"), white.adjust = "hc1")
F_regiao

# Teste F 4: no modelo com interação, ser mulher não afeta o salário
# (H0: mulher = 0 e mulher:educ = 0)
F_genero <- linearHypothesis(mod5, c("mulher = 0", "mulher:educ = 0"), white.adjust = "hc1")
F_genero

# Teste F de hipótese conjunta do modelo (4)
F_con <- summary(mod4)$fstatistic
F_con

#------------------------------- 9. HETEROCEDASTICIDADE -----------------------
# Breusch-Pagan; H0: homocedasticidade
bp4 <- bptest(mod4)
bp4

# Teste de White, forma especial
base$ajustado4 <- fitted(mod4)
white4 <- bptest(mod4, ~ ajustado4 + I(ajustado4^2), data = base)
white4

# Teste de White, forma completa no modelo (2)
white2_completo <- bptest(mod2, ~ mulher + educ + exper + exper2 + I(educ^2) +
                            mulher:educ + mulher:exper + educ:exper, data = base)
white2_completo

# Figura 5: resíduos x valores ajustados
png(file.path(PASTA_SAIDA, "fig5_residuos.png"), width = 1600, height = 1000, res = 200)
idx <- sample(nrow(base), 6000)
plot(fitted(mod4)[idx], resid(mod4)[idx], pch = 20, cex = 0.4,
     col = adjustcolor("#2a78d6", 0.4),
     xlab = "Log do salário-hora ajustado", ylab = "Resíduo")
abline(h = 0, col = "#eb6834", lwd = 2)
dev.off()

# Tabela de testes
linha_F  <- function(x) c(x$F[2], x$Df[2], x$`Pr(>F)`[2])    # testes F
linha_BP <- function(x) c(x$statistic, x$parameter, x$p.value)  # testes de Breusch-Pagan e White
testes <- rbind(
  "F: educ = exper = exper² = 0 (HC1)" = linha_F(F_capital),
  "F: dummies de ocupação = 0 (HC1)" = linha_F(F_ocupacao),
  "F: dummies de região = 0 (HC1)" = linha_F(F_regiao),
  "F: mulher = mulher×educ = 0, modelo (5) (HC1)" = linha_F(F_genero),
  "F de hipótese conjunta do modelo (4)" = c(F_con[1], F_con[2], pf(F_con[1], F_con[2], F_con[3], lower.tail = FALSE)),
  "Breusch-Pagan (modelo 4)" = linha_BP(bp4),
  "White, forma especial (modelo 4)" = linha_BP(white4),
  "White, forma completa (modelo 2)" = linha_BP(white2_completo))
colnames(testes) <- c("estatistica", "gl", "p_valor")
testes
write.csv2(testes, file.path(PASTA_SAIDA, "tab5_testes.csv"))
write.csv2(ep_comp, file.path(PASTA_SAIDA, "tab5_ep_comparacao.csv"))

#------------------------------- 10. OUTLIERS ---------------------------------
curtose <- function(x) mean((x - mean(x))^4) / mean((x - mean(x))^2)^2
base$residuo4 <- resid(mod4)
momentos <- datasummary(salario_hora + log_sh + residuo4 ~ curtose + Min + Max,
                        data = base, fmt = 4, output = "data.frame")
momentos
write.csv2(momentos, file.path(PASTA_SAIDA, "tab8_curtose.csv"), row.names = FALSE)

#------------------------------- 11. INTERAÇÃO --------------------------------
# Hiato estimado para diferentes anos de estudo
hiato_por_educ <- function(m, nome) {
  b <- coef(m); V <- vcovHC(m, type = "HC1")
  h <- data.frame(modelo = nome, educ = c(4, 8, 11, 12, 15, 16))
  h$efeito <- b["mulher"] + b["mulher:educ"] * h$educ
  h$ep <- sqrt(V["mulher", "mulher"] + h$educ^2 * V["mulher:educ", "mulher:educ"] +
                 2 * h$educ * V["mulher", "mulher:educ"])
  h$efeito_pct <- 100 * (exp(h$efeito) - 1)
  h
}
hiato_educ <- rbind(hiato_por_educ(mod5, "(5) sem ocupação"),
                    hiato_por_educ(mod6, "(6) com ocupação"))
hiato_educ
write.csv2(hiato_educ, file.path(PASTA_SAIDA, "tab6_hiato_por_educ.csv"), row.names = FALSE)

#------------------------------- 12. ROBUSTEZ ---------------------------------
# a) Ponderando pelo peso amostral do IBGE (V1028)
mod4_peso <- lm(formula(mod4), data = base, weights = peso)
# b) Log do rendimento mensal como variável dependente (com controle de horas)
mod4_mensal <- lm(log(renda) ~ mulher + educ + exper + exper2 + cor + urbano +
                    regiao + ocupacao + horas, data = base)
# c) Idade e idade² no lugar da experiência potencial
mod4_idade <- lm(log_sh ~ mulher + educ + idade + I(idade^2) + cor + urbano + regiao +
                   ocupacao, data = base)
# d) Apenas trabalhadores formais
mod4_formal <- lm(formula(mod4), data = subset(base, formal == 1))
# e) Dummy de ensino superior no lugar dos anos de estudo
mod4_superior <- lm(log_sh ~ mulher + superior + exper + exper2 + cor + urbano +
                      regiao + ocupacao, data = base)

rob <- list("Modelo (4) base" = mod4, "Ponderado pelo peso amostral" = mod4_peso,
            "Log do rendimento mensal" = mod4_mensal, "Idade no lugar da experiência" = mod4_idade,
            "Só trabalhadores formais" = mod4_formal, "Dummy de ensino superior" = mod4_superior)
efeito_rob <- sapply(rob, function(m) sprintf("%.1f", 100 * (exp(coef(m)["mulher"]) - 1)))
tab_rob <- modelsummary(rob, vcov = "HC1", fmt = 4, coef_map = c("mulher" = "Mulher"),
                        gof_map = "nobs", output = "data.frame",
                        add_rows = setNames(data.frame("Efeito de ser mulher (%)", t(efeito_rob)),
                                            c("term", names(rob))))
tab_rob
write.csv2(tab_rob, file.path(PASTA_SAIDA, "tab7_robustez.csv"), row.names = FALSE)
