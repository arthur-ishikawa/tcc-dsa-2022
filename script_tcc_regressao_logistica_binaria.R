##################################################################################
#                          CARREGAMENTO DE PACOTES                               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot", "caTools", "glmmTMB", "e1071", "olsrr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##################################################################################
#                           CARREGAMENTO DO DATASET                              #
##################################################################################

dados_cardio <- read.csv("heart_disease_health_indicators_BRFSS2015.csv",
                         header = TRUE,
                         sep = ",",
                         dec = ".")
#Dimensão e nomes
dim(dados_cardio)
names(dados_cardio)

#Renomear variáveis
dados_cardio <- dados_cardio %>% rename(doenca_cardio = 1,
                                        pressao_alta = 2,
                                        colesterol_alto = 3,
                                        colesterol_check = 4,
                                        imc = 5,
                                        fumante = 6,
                                        derrame = 7,
                                        diabetes = 8,
                                        atividade_fisica = 9,
                                        consumo_frutas = 10,
                                        consumo_vegetais = 11,
                                        ingestao_alcool = 12,
                                        plano_saude = 13,
                                        consulta_medica = 14,
                                        nivel_saude = 15,
                                        nivel_mental = 16,
                                        saude_fisica = 17,
                                        dificuldade_andar = 18,
                                        sexo = 19,
                                        faixa_etaria = 20,
                                        nivel_educacao = 21,
                                        renda_anual = 22)

#Verificar nomes
names(dados_cardio)

#Resumo do dataset
summary(dados_cardio)
str(dados_cardio)

#Alterar para factor
dados_cardio <- dados_cardio %>%
  mutate(doenca_cardio = as.factor(doenca_cardio),
         pressao_alta = as.factor(pressao_alta),
         colesterol_alto = as.factor(colesterol_alto),
         colesterol_check = as.factor(colesterol_check),
         fumante = as.factor(fumante),
         derrame = as.factor(derrame),
         diabetes = as.factor(diabetes),
         atividade_fisica = as.factor(atividade_fisica),
         consumo_frutas = as.factor(consumo_frutas),
         consumo_vegetais = as.factor(consumo_vegetais),
         ingestao_alcool = as.factor(ingestao_alcool),
         plano_saude = as.factor(plano_saude),
         consulta_medica = as.factor(consulta_medica),
         nivel_saude = as.factor(nivel_saude),
         dificuldade_andar = as.factor(dificuldade_andar),
         sexo = as.factor(sexo),
         faixa_etaria = as.factor(faixa_etaria),
         nivel_educacao = as.factor(nivel_educacao),
         renda_anual = as.factor(renda_anual))

#Resumo do dataset
str(dados_cardio)
summary(dados_cardio)
glimpse(dados_cardio)

#Dummizando as variáveis
cardio_dummies <- dummy_columns(.data = dados_cardio,
                                select_columns = c("pressao_alta",
                                                   "colesterol_alto", 
                                                   "colesterol_check",
                                                   "fumante",
                                                   "derrame",
                                                   "diabetes",
                                                   "atividade_fisica",
                                                   "consumo_frutas",
                                                   "consumo_vegetais",
                                                   "ingestao_alcool",
                                                   "plano_saude",
                                                   "consulta_medica",
                                                   "nivel_saude",
                                                   "dificuldade_andar",
                                                   "sexo",
                                                   "faixa_etaria",
                                                   "nivel_educacao",
                                                   "renda_anual"),
                                remove_selected_columns = T,
                                remove_first_dummy = T)

summary(cardio_dummies)
str(cardio_dummies)

#Dividindo o dataset em 70:30 para treino e teste
set.seed(123)
split = sample.split(cardio_dummies$doenca_cardio, SplitRatio = 0.7)
treino_cardio = subset(cardio_dummies, split==TRUE)
nrow(treino_cardio)
teste_cardio = subset(cardio_dummies, split==FALSE)
nrow(teste_cardio)
summary(treino_cardio)

##################################################################################
#                                 MODELO GLM - 1 VARIÁVEL                        #
##################################################################################

#Criando o modelo GLM com apenas uma variável
modelo_cardio <- glm(formula = doenca_cardio ~ + pressao_alta_1, 
                     data = treino_cardio, 
                     family = "binomial")

#Apresentando outputs do modelo_treino_cardio
summary(modelo_cardio)

#Outro modo de apresentar os outputs do modelo_treino_cardio
summ(modelo_cardio, confint = T, digits = 3, ci.width = .95)

#Valor do LL do modelo_cardio
logLik(modelo_cardio)

##############################################################################
#            CONSTRUÇÃO DA CURVA ROC - MODELO GLM - 1 VARIÁVEL              #
##############################################################################

ROC_modelo_cardio <- roc(response = treino_cardio$doenca_cardio, 
                         predictor = modelo_cardio$fitted.values)

ggplotly(
  ggroc(ROC_modelo_cardio, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC_modelo_cardio$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC_modelo_cardio$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

##################################################################################
#                            MODELO GLM - TODAS VARIÁVEIS                        #
##################################################################################

#Criando o modelo GLM em função de todas variáveis
modelo_treino_cardio <- glm(formula = doenca_cardio ~ ., 
                            data = treino_cardio, 
                            family = "binomial")

#Apresentando outputs do modelo_treino_cardio
summary(modelo_treino_cardio)

#Outro modo de apresentar os outputs do modelo_treino_cardio
summ(modelo_treino_cardio, confint = T, digits = 3, ci.width = .95)

#Valor do LL do modelo_treino_cardio
logLik(modelo_treino_cardio)

##############################################################################
#                CONSTRUÇÃO DA CURVA ROC - TODAS VARIÁVEIS                   #
##############################################################################

ROC_modelo_treino_cardio <- roc(response = treino_cardio$doenca_cardio, 
                                predictor = modelo_treino_cardio$fitted.values)

ggplotly(
  ggroc(ROC_modelo_treino_cardio, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC_modelo_treino_cardio$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC_modelo_treino_cardio$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

##################################################################################
#                              MODELO GLM - COM STEPWISE                         #
##################################################################################

#Procedimento Stepwise
modelo_step_cardio <- step(object =  modelo_treino_cardio,
                           k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(modelo_step_cardio)

#Outro modo de apresentar os outputs do modelo_step_cardio
summ(modelo_step_cardio, confint = T, digits = 3, ci.width = .95)

#Valor do LL do modelo_step_cardio
logLik(modelo_step_cardio)

##############################################################################
#          CONSTRUÇÃO DA CURVA ROC - TODAS VARIÁVEIS COM STEPWISE            #
##############################################################################
ROC <- roc(response = treino_cardio$doenca_cardio, 
           predictor = modelo_step_cardio$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

roc.test(ROC_modelo_treino_cardio,ROC)

##############################################################################
#         COMPARAÇÃO ENTRE OS MODELOS GLM E GLM COM STEPWISE                 #
##############################################################################

#Comparando os modelos treino_cardio e modelo_step_cardio
#função lrtest do pacote lmtest
lrtest(modelo_treino_cardio, modelo_step_cardio)

##############################################################################
#                      CONSTRUÇÃO DA MATRIZ DE CONFUSÃO                      #
##############################################################################
confusionMatrix(
  table(predict(modelo_step_cardio, type = "response") >= 0.50, 
        treino_cardio$doenca_cardio == "1")[2:1, 2:1]
)

##############################################################################
#                         ANÁLISE DE SENSIBILIDADE                           #
##############################################################################

#Função prediction do pacote ROCR
predicoes <- prediction(predictions = modelo_step_cardio$fitted.values, 
                        labels = treino_cardio$doenca_cardio) 

#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extraiu do objeto 'predicoes' os 
#dados de sensitividade, de sensibilidade e de especificidade para a plotagem.

#Porém, desejamos os dados da sensitividade, então devemos fazer o seguinte 
#ajuste:
sensitividade <- dados_curva_roc@y.values[[1]] 
#extraindo dados da sensitividade do modelo

especificidade <- performance(predicoes, measure = "spec") 
#extraindo os dados da especificidade, mas também há que se fazer um ajuste para a 
#plotagem:
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_roc@x.values[[1]] 
#extraindo os cutoffs do objeto 'sensitividade'.

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Visualizando o novo data frame dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                 NOVA MATRIZ DE CONFUSÃO COM CUTOFF DE 0.10                 #
##############################################################################
confusionMatrix(
  table(predict(modelo_step_cardio, type = "response") >= 0.10, 
        treino_cardio$doenca_cardio == "1")[2:1, 2:1]
)

##############################################################################
#                    APLICANDO O MODELO NOS DADOS DE TESTE                   #
##############################################################################

predicao_dados_teste <- predict(object = modelo_step_cardio,
                                newdata = teste_cardio,
                                type = "response")

##############################################################################
#                    MATRIZ DE CONFUSÃO - DADOS DE TESTE                     #
##############################################################################

confusionMatrix(
  table(predicao_dados_teste >= 0.10, 
        teste_cardio$doenca_cardio == "1")[2:1, 2:1]
)

view(predicao_dados_teste)

##############################################################################
#                          CURVA ROC - DADOS DE TESTE                        #
##############################################################################
teste_ROC <- roc(response = teste_cardio$doenca_cardio, 
                 predictor = predicao_dados_teste)

ggplotly(
  ggroc(teste_ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(teste_ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((teste_ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

roc.test(ROC,teste_ROC)

##############################################################################
#                 APLICAÇÃO EM DADOS ALEATÓRIOS PARA TESTE                   #
##############################################################################

resultado <- if(predict(object = modelo_step_cardio,
                        data.frame(imc = 12, nivel_mental = 0, saude_fisica = 0, pressao_alta_1 = 0,
                                   colesterol_alto_1 = 0, colesterol_check_1 = 0, fumante_1 = 0,
                                   derrame_1 = 0, diabetes_1 = 0, diabetes_2 = 0, atividade_fisica_1 = 0,
                                   consumo_frutas_1 = 1, consumo_vegetais_1 = 1, ingestao_alcool_1 = 0,
                                   plano_saude_1 = 1, consulta_medica_1 = 0, nivel_saude_2 = 0,
                                   nivel_saude_3 = 0, nivel_saude_4 = 0, nivel_saude_5 = 0,
                                   dificuldade_andar_1 = 0, sexo_1 = 0, faixa_etaria_2 = 1,
                                   faixa_etaria_3 = 0, faixa_etaria_4 = 0, faixa_etaria_5 = 0,
                                   faixa_etaria_6 = 0, faixa_etaria_7 = 0, faixa_etaria_8 = 0,
                                   faixa_etaria_9 = 0, faixa_etaria_10 = 0, faixa_etaria_11 = 0,
                                   faixa_etaria_12 = 0, faixa_etaria_13 = 0, nivel_educacao_2 = 0,
                                   nivel_educacao_3 = 0, nivel_educacao_4 = 0, nivel_educacao_5 = 0,
                                   nivel_educacao_6 = 1, renda_anual_2 = 0, renda_anual_3 = 0,
                                   renda_anual_4 = 0, renda_anual_5 = 0, renda_anual_6 = 0,
                                   renda_anual_7 = 0, renda_anual_8 = 1),
                        type = "response")>=0.1)
{resultado <- "ALTA PROBABILIDADE"} else
{resultado <- "BAIXA PROBABILIDADE"}
view(resultado)