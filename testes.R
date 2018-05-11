setwd("C:\\Users\\marin\\Desktop\\Livro Curitiba")

list.files()

library(tidyverse)
library(data.table)
install.packages("gmodels")
library(gmodels)

curitiba <- fread("curitiba.csv")

##fazer um primeiro cruzamento pra ver se existe alguma célula >5
table(curitiba$v20_conexao,curitiba$v5_sexo)

#recodificar as categorias >5

curitiba$v20_conexao[curitiba$v20_conexao == 3] <- 8
curitiba$v20_conexao[curitiba$v20_conexao == 7] <- 8

#selecionar apenas as linhas da tabela de contingência que queremos
#exclui NS r NR
v20 <- as.data.frame.matrix(table(curitiba$v20_conexao,curitiba$v5_sexo)) %>%
  slice(1:6)

#teste
chisq.test(v20)


########

curitiba <- curitiba %>%
  mutate(V13b_primeira_filiacao = ifelse(v13a_ano5 == 77, v13a_ano4, v13a_ano5)) %>%
  mutate(V13b_primeira_filiacao = ifelse(V13b_primeira_filiacao == 77, v13a_ano3, V13b_primeira_filiacao)) %>%
  mutate(V13b_primeira_filiacao = ifelse(V13b_primeira_filiacao == 77, v13a_ano2, V13b_primeira_filiacao)) %>%
  mutate(V13b_primeira_filiacao = ifelse(V13b_primeira_filiacao == 77, v13a_ano1, V13b_primeira_filiacao)) %>%
  mutate(V13b_primeira_filiacao = ifelse(V13b_primeira_filiacao == 77, v12_filiado_desde, V13b_primeira_filiacao)) %>%
  mutate(v13c_idade_fili = ifelse(V13b_primeira_filiacao == 88 | V13b_primeira_filiacao == 99, NA,(V13b_primeira_filiacao - V2_anonasc))) %>%
  mutate(v13c_idade_fili = ifelse(v13c_idade_fili < 16 , NA, v13c_idade_fili))


summary(curitiba$v13c_idade_fili, na.rm = T)

idadefili <- curitiba %>%
  mutate(v13c_idade_fili = as.numeric(v13c_idade_fili)) %>%
  group_by(v5_sexo) %>%
  summarise(media_idade = mean(v13c_idade_fili, na.rm = TRUE),
            mediana_idade = median(v13c_idade_fili, na.rm = TRUE),
            desvio_idade = sd(v13c_idade_fili, na.rm = TRUE),
            minimo_idade = min(v13c_idade_fili, na.rm = TRUE),
            maximo_idade = max(v13c_idade_fili, na.rm = TRUE))


summary(curitiba$v13c_idade_fili, na.rm = T)

idade_cand <- curitiba %>%
  mutate(v2_idade = as.numeric(v2_idade)) %>%
  group_by(v5_sexo) %>%
  summarise(media_idade = mean(v2_idade, na.rm = TRUE),
            mediana_idade = median(v2_idade, na.rm = TRUE),
            desvio_idade = sd(v2_idade, na.rm = TRUE),
            minimo_idade = min(v2_idade, na.rm = TRUE),
            maximo_idade = max(v2_idade, na.rm = TRUE))


##tempo de contribuição no partido

##fazer um primeiro cruzamento pra ver se existe alguma célula >5
r<- table(curitiba$v15_horas_partido,curitiba$v5_sexo)

#recodificar as categorias >5
curitiba$v20_conexao[curitiba$v20_conexao == 3] <- 8
curitiba$v20_conexao[curitiba$v20_conexao == 7] <- 8

#selecionar apenas as linhas da tabela de contingência que queremos
#exclui NS r NR
v15 <- as.data.frame.matrix(v15) %>%
  slice(1:5)

#teste
chisq.test(v15)

#juntando as categorias pra ver se muda

#recodificar as categorias 
curitiba$v15_horas_partido2 <- curitiba$v15_horas_partido

#até 20 horas
curitiba$v15_horas_partido2[curitiba$v15_horas_partido2 == 2] <- 1
curitiba$v15_horas_partido2[curitiba$v15_horas_partido2 == 3] <- 1
#mais de 20 horas
curitiba$v15_horas_partido2[curitiba$v15_horas_partido2 == 4] <- 2
curitiba$v15_horas_partido2[curitiba$v15_horas_partido2 == 5] <- 2
#deixando NS NR como NA
curitiba$v15_horas_partido2[curitiba$v15_horas_partido2 == 88] <- NA
curitiba$v15_horas_partido2[curitiba$v15_horas_partido2 == 99] <- NA

v15_2 <- table(curitiba$v15_horas_partido2,curitiba$v5_sexo)

#teste
chisq.test(v15_2)



##fazer um primeiro cruzamento pra ver se existe alguma célula >5
v15b <- table(curitiba$v15a_horas_politica,curitiba$v5_sexo)

#recodificar as categorias >5
curitiba$v20_conexao[curitiba$v20_conexao == 3] <- 8
curitiba$v20_conexao[curitiba$v20_conexao == 7] <- 8

#selecionar apenas as linhas da tabela de contingência que queremos
#exclui NS r NR
v15b <- as.data.frame.matrix(v15b) %>%
  slice(1:5)

#teste
chisq.test(v15b)

####v14####
v14 <- table(curitiba$v14_cargo_politico_antes,curitiba$v5_sexo)


curitiba$v20_conexao[curitiba$v20_conexao == 3] <- 8
curitiba$v20_conexao[curitiba$v20_conexao == 7] <- 8


v15b <- as.data.frame.matrix(v15b) %>%
  slice(1:5)


chisq.test(v14)

####v18####
v18 <- table(curitiba$v18_motivacao,curitiba$v5_sexo)


curitiba$v18_motivacao[curitiba$v18_motivacao == 5] <- 4
curitiba$v18_motivacao[curitiba$v18_motivacao == 6] <- 4

curitiba$v18_motivacao[curitiba$v18_motivacao == 88] <- 99


v15b <- as.data.frame.matrix(v15b) %>%
  slice(1:5)


chisq.test(v18)


####v18####
v20 <- table(curitiba$v20_conexao,curitiba$v5_sexo)


curitiba$v18_motivacao[curitiba$v18_motivacao == 5] <- 4
curitiba$v18_motivacao[curitiba$v18_motivacao == 6] <- 4

curitiba$v20_conexao[curitiba$v20_conexao == 88] <- 99


v20s <- as.data.frame.matrix(v20)%>%
  filter(row_number()==2 | row_number()==3 | row_number()==4 | row_number()==7)


chisq.test(v20)

####v21####
v21 <- table(curitiba$v21_carac_eleit_vereador,curitiba$v5_sexo)


curitiba$v21_carac_eleit_vereador[curitiba$v21_carac_eleit_vereador == 3] <- 4

curitiba$v21_carac_eleit_vereador[curitiba$v21_carac_eleit_vereador == 88] <- 99


v20s <- as.data.frame.matrix(v20)%>%
  filter(row_number()==2 | row_number()==3 | row_number()==4 | row_number()==7)


chisq.test(v21)

####v32####
v32 <- table(curitiba$v32_dific_mulheres,curitiba$v5_sexo)


curitiba$v21_carac_eleit_vereador[curitiba$v21_carac_eleit_vereador == 3] <- 4

curitiba$v21_carac_eleit_vereador[curitiba$v21_carac_eleit_vereador == 88] <- 99


v32 <- as.data.frame.matrix(v32)%>%
  filter(!row_number()==4)


chisq.test(v32)


####v29####
v29 <- table(curitiba$v29a_estrat_campanha1,curitiba$v5_sexo)


curitiba$v21_carac_eleit_vereador[curitiba$v21_carac_eleit_vereador == 3] <- 4

curitiba$v21_carac_eleit_vereador[curitiba$v21_carac_eleit_vereador == 88] <- 99


v32 <- as.data.frame.matrix(v32)%>%
  filter(!row_number()==4)


chisq.test(v29)


####v27####
v27 <- table(curitiba$v27_selecao_cand,curitiba$v5_sexo)


curitiba$v27_selecao_cand[curitiba$v27_selecao_cand == 88] <- 99


v32 <- as.data.frame.matrix(v32)%>%
  filter(!row_number()==4)


chisq.test(v27)



####v26####
v26 <- table(curitiba$v26_candidato,curitiba$v5_sexo)


curitiba$v26_candidato[curitiba$v26_candidato == 5] <- 4
curitiba$v26_candidato[curitiba$v26_candidato == 6] <- 4
curitiba$v26_candidato[curitiba$v26_candidato == 7] <- 4
curitiba$v26_candidato[curitiba$v26_candidato == 8] <- 4
curitiba$v26_candidato[curitiba$v26_candidato == 9] <- 4

chisq.test(v26)


####v27####
v27b <- table(curitiba$v27_selecao_cand,curitiba$v5_sexo)


curitiba$v27_selecao_cand[curitiba$v27_selecao_cand == 3] <- 1
curitiba$v27_selecao_cand[curitiba$v27_selecao_cand == 4] <- 2
curitiba$v27_selecao_cand[curitiba$v27_selecao_cand == 5] <- 2
curitiba$v27_selecao_cand[curitiba$v27_selecao_cand == 6] <- 2

chisq.test(v27b)

