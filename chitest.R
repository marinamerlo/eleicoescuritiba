setwd("C:\\Users\\d841255\\Desktop")

list.files()

library(tidyverse)
library(data.table)

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
