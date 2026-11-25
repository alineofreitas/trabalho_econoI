#==============================================================================
# Hiato salarial de gênero no Brasil - PNAD Contínua, 2º trimestre de 2026
# Trabalho de Econometria I
#==============================================================================

#------------------------------- 0. PARÂMETROS --------------------------------
CAMINHO_MICRODADOS <- "C:/Users/c3009263/Documents/PNADC_022026/PNADC_022026.txt"
CAMINHO_INPUT      <- "C:/Users/c3009263/Documents/Dicionario_e_input_20221031/input_PNADC_trimestral.txt"
PASTA_SAIDA        <- "resultados"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) { CAMINHO_MICRODADOS <- args[1]; CAMINHO_INPUT <- args[2] }
if (length(args) >= 3) PASTA_SAIDA <- args[3]
dir.create(PASTA_SAIDA, showWarnings = FALSE, recursive = TRUE)

#------------------------------- 1. PACOTES -----------------------------------
install.packages("lmtest")
install.packages("sandwich")
install.packages("car")
install.packages("PNADcIBGE")

library(lmtest)    # bptest (Breusch-Pagan e White) e coeftest
library(sandwich)  # vcovHC: erros-padrão robustos à heterocedasticidade
library(car)       # linearHypothesis (teste F) e vif

#------------------------------- 2. LEITURA DOS DADOS -------------------------
# Variáveis usadas (ver dicionário do IBGE):
# UF       Unidade da Federação          V1022  Situação do domicílio (1 urbano)
# V1028    Peso amostral                 V2007  Sexo (1 homem, 2 mulher)
# V2009    Idade                         V2010  Cor ou raça
# VD3004   Nível de instrução            VD3005 Anos de estudo
# VD4002   Condição de ocupação          VD4009 Posição na ocupação
# VD4011   Grupamento ocupacional        VD4019 Rendimento habitual (todos os trabalhos)
# VD4031   Horas habituais (todos os trabalhos)
variaveis <- c("V1022", "V1028", "V2007", "V2009", "V2010", "VD3004", "VD3005",
               "VD4002", "VD4009", "VD4011", "VD4019", "VD4031")

if (requireNamespace("PNADcIBGE", quietly = TRUE)) {
  # Leitura com o pacote do IBGE (como no rascunho original)
  dados_pnad <- PNADcIBGE::read_pnadc(microdata = CAMINHO_MICRODADOS,
                                      input_txt = CAMINHO_INPUT,
                                      vars = variaveis)
} else {
  # Alternativa sem o pacote PNADcIBGE: lê as posições do arquivo de input
  # e importa só as colunas necessárias (resultado idêntico).
  input <- readLines(CAMINHO_INPUT, encoding = "latin1", warn = FALSE)
  input <- regmatches(input, regexec("@(\\d+)\\s+(\\w+)\\s+\\$?(\\d+)\\.", input))
  input <- do.call(rbind, lapply(input[lengths(input) == 4], function(x) x[2:4]))
  input <- data.frame(inicio = as.integer(input[, 1]), nome = input[, 2],
                      largura = as.integer(input[, 3]))
  input <- input[input$nome %in% c("UF", variaveis), ]
  dados_pnad <- readr::read_fwf(
    CAMINHO_MICRODADOS,
    readr::fwf_positions(input$inicio, input$inicio + input$largura - 1, input$nome),
    col_types = readr::cols(.default = "c"), progress = FALSE)
  dados_pnad <- as.data.frame(dados_pnad)
}

# Converte todas as colunas para número
for (v in c("UF", variaveis)) dados_pnad[[v]] <- as.numeric(as.character(dados_pnad[[v]]))
cat("Registros lidos:", nrow(dados_pnad), "\n")

#------------------------------- 3. CONSTRUÇÃO DA BASE ------------------------
base <- data.frame(
  uf           = dados_pnad$UF,
  peso         = dados_pnad$V1028,
  sexo         = dados_pnad$V2007,
  idade        = dados_pnad$V2009,
  cor_cod      = dados_pnad$V2010,
  nivel_cod    = dados_pnad$VD3004,
  educ         = dados_pnad$VD3005,
  ocupado      = dados_pnad$VD4002,
  posicao      = dados_pnad$VD4009,
  ocup_cod     = dados_pnad$VD4011,
  renda        = dados_pnad$VD4019,
  horas        = dados_pnad$VD4031,
  situacao     = dados_pnad$V1022
)
rm(dados_pnad); gc()

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
base$exper2 <- base$exper^2 / 100   # experiência ao quadrado (dividida por 100 para facilitar a leitura)

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
saveRDS(base, file.path(PASTA_SAIDA, "base_final.rds"))
write.csv2(base[, c("mulher", "idade", "educ", "exper", "nivel_cod", "superior", "cor_cod",
                    "urbano", "uf", "ocup_cod", "formal", "renda", "horas",
                    "salario_hora", "log_sh", "peso")],
           file.path(PASTA_SAIDA, "base_final.csv"), row.names = FALSE)

#------------------------------- 4. ESTATÍSTICAS DESCRITIVAS ------------------
head(base)
summary(base[, c("salario_hora", "log_sh", "renda", "horas", "idade", "educ", "exper")])

vars_desc <- c("salario_hora", "log_sh", "renda", "horas", "idade", "educ", "exper",
               "superior", "urbano", "formal")
descritivas <- data.frame(
  variavel = vars_desc,
  n        = sapply(vars_desc, function(v) sum(!is.na(base[[v]]))),
  media    = sapply(vars_desc, function(v) mean(base[[v]])),
  variancia = sapply(vars_desc, function(v) var(base[[v]])),
  desvio   = sapply(vars_desc, function(v) sd(base[[v]])),
  minimo   = sapply(vars_desc, function(v) min(base[[v]])),
  maximo   = sapply(vars_desc, function(v) max(base[[v]])),
  media_homens   = sapply(vars_desc, function(v) mean(base[[v]][base$mulher == 0])),
  media_mulheres = sapply(vars_desc, function(v) mean(base[[v]][base$mulher == 1]))
)
descritivas
write.csv2(descritivas, file.path(PASTA_SAIDA, "tab1_descritivas.csv"), row.names = FALSE)

# Número de observações por sexo e proporção de negros (pretos e pardos)
table(base$sexo_f)
prop.table(table(base$sexo_f, base$cor), 1)

# Diferença de médias do log do salário-hora entre os sexos (teste t)
t.test(log_sh ~ sexo_f, data = base)

# Correlações
base$negro <- ifelse(base$cor %in% c("Preta", "Parda"), 1, 0)
correl <- cor(base[, c("log_sh", "mulher", "educ", "exper", "idade", "negro",
                       "urbano", "formal", "horas")])
round(correl, 3)
write.csv2(round(correl, 3), file.path(PASTA_SAIDA, "tab2_correlacoes.csv"))

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
(tab_regiao   <- media_por("regiao"))
write.csv2(tab_nivel,    file.path(PASTA_SAIDA, "tab3_media_nivel.csv"))
write.csv2(tab_ocupacao, file.path(PASTA_SAIDA, "tab3_media_ocupacao.csv"))
write.csv2(tab_regiao,   file.path(PASTA_SAIDA, "tab3_media_regiao.csv"))

# Distribuição: proporção de cada sexo por nível de instrução
round(prop.table(table(base$nivel, base$sexo_f), 2) * 100, 1)
write.csv2(round(prop.table(table(base$nivel, base$sexo_f), 2) * 100, 1),
           file.path(PASTA_SAIDA, "tab3_distribuicao_nivel.csv"))

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

# Figura 2: boxplot do log do salário-hora por sexo
png(file.path(PASTA_SAIDA, "fig2_boxplot.png"), width = 1400, height = 1000, res = 200)
boxplot(log_sh ~ sexo_f, data = base, col = cores, outline = FALSE,
        xlab = "", ylab = "Log do salário-hora (R$)")
points(1:2, tapply(base$log_sh, base$sexo_f, mean), pch = 19, col = "black")
legend("topright", legend = "Média", pch = 19, bty = "n")
dev.off()

# Figura 3: salário-hora médio por nível de instrução e sexo
png(file.path(PASTA_SAIDA, "fig3_nivel_instrucao.png"), width = 1800, height = 1000, res = 200)
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

# R² e erro-padrão da regressão calculados à mão
SSR <- sum(resid(mod1)^2)
TSS <- sum((base$log_sh - mean(base$log_sh))^2)
R2  <- 1 - SSR / TSS
SER <- sqrt(SSR / (nrow(base) - 2))
c(R2 = R2, SER = SER)

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
mod5a <- lm(log_sh ~ mulher * educ + exper + exper2 + cor + urbano + regiao, data = base)
summary(mod5a)

# (6) Interação mulher x anos de estudo, com controle de ocupação
mod5 <- lm(log_sh ~ mulher * educ + exper + exper2 + cor + urbano + regiao +
             ocupacao, data = base)
summary(mod5)

# Colinearidade perfeita: experiência = idade - educ - 6. Se incluirmos idade junto,
# o R não consegue estimar um dos coeficientes (aparece NA).
mod_colinear <- lm(log_sh ~ mulher + educ + idade + I(idade - educ - 6), data = base)
coef(mod_colinear)

#------------------------------- 7. ERROS-PADRÃO ROBUSTOS (HC1) ---------------
modelos <- list("(1)" = mod1, "(2)" = mod2, "(3)" = mod3, "(4)" = mod4, "(5)" = mod5a,
                "(6)" = mod5)
robustos <- lapply(modelos, function(m) coeftest(m, vcov. = vcovHC(m, type = "HC1")))
robustos

# Comparação dos erros-padrão convencionais e robustos no modelo (4)
ep_comp <- data.frame(
  convencional = sqrt(diag(vcov(mod4))),
  robusto_HC1  = sqrt(diag(vcovHC(mod4, type = "HC1"))))
round(ep_comp[c("mulher", "educ", "exper", "exper2"), ], 5)

# Tabela de regressões com erros-padrão robustos
vars_tab <- c("(Intercept)", "mulher", "educ", "exper", "exper2", "urbano",
              "corPreta", "corParda", "mulher:educ")
estrelas <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
tab_reg <- do.call(cbind, lapply(names(modelos), function(k) {
  ct <- robustos[[k]]
  col <- unlist(lapply(vars_tab, function(v) {
    if (v %in% rownames(ct)) c(sprintf("%.4f%s", ct[v, 1], estrelas(ct[v, 4])),
                               sprintf("(%.4f)", ct[v, 2])) else c("", "")
  }))
  c(col, ifelse(any(grepl("^cor", names(coef(modelos[[k]])))), "Sim", "Não"),
    ifelse(any(grepl("^regiao", names(coef(modelos[[k]])))), "Sim", "Não"),
    ifelse(any(grepl("^ocupacao", names(coef(modelos[[k]])))), "Sim", "Não"),
    nobs(modelos[[k]]), sprintf("%.3f", summary(modelos[[k]])$r.squared),
    sprintf("%.3f", summary(modelos[[k]])$adj.r.squared),
    sprintf("%.1f", 100 * (exp(coef(modelos[[k]])["mulher"]) - 1)))
}))
rownames(tab_reg) <- c(as.vector(rbind(vars_tab, paste0(vars_tab, "_ep"))),
                       "Cor/raça", "Região", "Ocupação", "N", "R2", "R2 ajustado",
                       "Efeito de ser mulher (%)")
colnames(tab_reg) <- names(modelos)
tab_reg
write.csv2(tab_reg, file.path(PASTA_SAIDA, "tab4_regressoes.csv"))

# Figura 5: coeficiente de mulher em cada modelo, com IC de 95% (HC1)
efeito <- sapply(robustos[1:4], function(ct) ct["mulher", 1])
ep     <- sapply(robustos[1:4], function(ct) ct["mulher", 2])
pct    <- 100 * (exp(efeito) - 1)
li     <- 100 * (exp(efeito - 1.96 * ep) - 1)
ls     <- 100 * (exp(efeito + 1.96 * ep) - 1)
png(file.path(PASTA_SAIDA, "fig5_coeficientes.png"), width = 1600, height = 950, res = 200)
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
# Teste t (robusto) de H0: coeficiente de mulher = 0 -> ver coeftest acima.

# Teste F 1: capital humano conjuntamente nulo (educ = exper = exper² = 0)
F_capital <- linearHypothesis(mod4, c("educ = 0", "exper = 0", "exper2 = 0"),
                              white.adjust = "hc1")
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
F_genero <- linearHypothesis(mod5a, c("mulher = 0", "mulher:educ = 0"), white.adjust = "hc1")
F_genero

# Teste F de significância global do modelo (4), versão convencional
F_global <- summary(mod4)$fstatistic
F_global

#------------------------------- 9. HETEROCEDASTICIDADE -----------------------
# Breusch-Pagan; H0: homocedasticidade
bp4 <- bptest(mod4)
bp4

# Breusch-Pagan "à mão" (regressão dos resíduos ao quadrado) no modelo (2)
summary(lm(resid(mod2)^2 ~ mulher + educ + exper + exper2, data = base))
bptest(mod2)

# Teste de White, forma especial: resíduos² em ŷ e ŷ²
base$ajustado4 <- fitted(mod4)
white4 <- bptest(mod4, ~ ajustado4 + I(ajustado4^2), data = base)
white4

# Teste de White, forma completa no modelo (2): níveis, quadrados e produtos cruzados
white2_completo <- bptest(mod2, ~ mulher + educ + exper + exper2 + I(educ^2) +
                            mulher:educ + mulher:exper + educ:exper, data = base)
white2_completo

# Figura 6: resíduos x valores ajustados (diagnóstico visual)
png(file.path(PASTA_SAIDA, "fig6_residuos.png"), width = 1600, height = 1000, res = 200)
idx <- sample(nrow(base), 6000)
plot(fitted(mod4)[idx], resid(mod4)[idx], pch = 20, cex = 0.4,
     col = adjustcolor("#2a78d6", 0.4),
     xlab = "Log do salário-hora ajustado", ylab = "Resíduo")
abline(h = 0, col = "#eb6834", lwd = 2)
dev.off()

# Figura 7: observado x ajustado
png(file.path(PASTA_SAIDA, "fig7_observado_ajustado.png"), width = 1400, height = 1100, res = 200)
plot(fitted(mod4)[idx], base$log_sh[idx], pch = 20, cex = 0.4,
     col = adjustcolor("#2a78d6", 0.4),
     xlab = "Log do salário-hora ajustado", ylab = "Log do salário-hora observado")
abline(a = 0, b = 1, col = "#eb6834", lwd = 2)
legend("topleft", legend = "Linha de 45°", col = "#eb6834", lwd = 2, bty = "n")
dev.off()

# Multicolinearidade: fator de inflação da variância
vif(mod3)

# Tabela de testes
testes <- data.frame(
  teste = c("F: educ = exper = exper² = 0 (HC1)", "F: dummies de ocupação = 0 (HC1)",
            "F: dummies de região = 0 (HC1)", "F: mulher = mulher×educ = 0, modelo (5) (HC1)",
            "F global do modelo (4)", "Breusch-Pagan (modelo 4)",
            "White, forma especial (modelo 4)", "White, forma completa (modelo 2)"),
  estatistica = c(F_capital$F[2], F_ocupacao$F[2], F_regiao$F[2], F_genero$F[2],
                  F_global[1], bp4$statistic, white4$statistic, white2_completo$statistic),
  gl = c(paste(F_capital$Df[2]), paste(F_ocupacao$Df[2]), paste(F_regiao$Df[2]),
         paste(F_genero$Df[2]), paste(F_global[2], F_global[3], sep = "; "),
         bp4$parameter, white4$parameter, white2_completo$parameter),
  p_valor = c(F_capital$`Pr(>F)`[2], F_ocupacao$`Pr(>F)`[2], F_regiao$`Pr(>F)`[2],
              F_genero$`Pr(>F)`[2],
              pf(F_global[1], F_global[2], F_global[3], lower.tail = FALSE),
              bp4$p.value, white4$p.value, white2_completo$p.value))
testes
write.csv2(testes, file.path(PASTA_SAIDA, "tab5_testes.csv"), row.names = FALSE)
write.csv2(round(vif(mod3), 3), file.path(PASTA_SAIDA, "tab5_vif.csv"))
write.csv2(round(ep_comp, 5), file.path(PASTA_SAIDA, "tab5_ep_comparacao.csv"))

#------------------------------- 10. INTERAÇÃO --------------------------------
# Hiato estimado para diferentes anos de estudo: efeito = b_mulher + b_interação × educ
hiato_por_educ <- function(m, nome) {
  b <- coef(m); V <- vcovHC(m, type = "HC1")
  h <- data.frame(modelo = nome, educ = c(4, 8, 11, 12, 15, 16))
  h$efeito <- b["mulher"] + b["mulher:educ"] * h$educ
  h$ep <- sqrt(V["mulher", "mulher"] + h$educ^2 * V["mulher:educ", "mulher:educ"] +
                 2 * h$educ * V["mulher", "mulher:educ"])
  h$efeito_pct <- 100 * (exp(h$efeito) - 1)
  h
}
hiato_educ <- rbind(hiato_por_educ(mod5a, "(5) sem ocupação"),
                    hiato_por_educ(mod5, "(6) com ocupação"))
hiato_educ
write.csv2(hiato_educ, file.path(PASTA_SAIDA, "tab6_hiato_por_educ.csv"), row.names = FALSE)

#------------------------------- 11. ROBUSTEZ ---------------------------------
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
tab_rob <- do.call(rbind, lapply(names(rob), function(k) {
  ct <- coeftest(rob[[k]], vcov. = vcovHC(rob[[k]], type = "HC1"))
  data.frame(especificacao = k, coef = ct["mulher", 1], ep_hc1 = ct["mulher", 2],
             efeito_pct = 100 * (exp(ct["mulher", 1]) - 1), n = nobs(rob[[k]]))
}))
tab_rob
write.csv2(tab_rob, file.path(PASTA_SAIDA, "tab7_robustez.csv"), row.names = FALSE)

cat("\nFim. Tabelas e figuras salvas em:", normalizePath(PASTA_SAIDA), "\n")
