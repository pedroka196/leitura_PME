library(survey)
library(xlsx)
library(tidyverse)
library(readr)
library(xlsx)

# Posicoes para obter o vetor de tamanhos
# Os dados estão em largura fixa, então é necessário pegar o tamanho das variáveis
# Label é o nome de cada variável.Consultar dicionário para obter o que interessar
posicoes <- read.xlsx("Documentacao/Layout/Layout.xls",sheetIndex = 1,colIndex = 2,startRow = 1,encoding = "UTF-8")
labels <- read.xlsx("Documentacao/Layout/Layout.xls",sheetIndex = 1,colIndex = 3,startRow = 1,encoding = "UTF-8")

# posicoes <- na.exclude(posicoes)
# labels <- na.exclude(labels)
names(posicoes) <- "posicoes"
names(labels) <- "labels"

posicoes <- posicoes %>%
  mutate(posicoes = as.character(posicoes)) %>%
  mutate(posicoes = as.integer(posicoes))

labels <- labels %>%
  mutate(labels = as.character(labels)) %>%
  filter(labels != "Código da Variável")

posicoes <- na.exclude(posicoes)
labels <- na.exclude(labels)

tamanhos <- fwf_widths(widths = posicoes$posicoes,col_names = labels$labels)

rm(posicoes,labels)
# domicilios <- read.fwf("Dados/DOM2002.txt",widths = posicoes)
# names(domicilios) <- labels$Código.de.variável

arquivos <- list.files(path = "PME Down/",pattern = "txt",full.names = T)
lista_PME <- list()
for(i in arquivos){
  # pme2 <- read_fwf(file = i,col_positions = tamanhos)
  nome <- gsub("[A-z/. ]","",i)
  nome <- sub("20","-20",nome)
  # nombres <- names(pme2)
  # lista_PME[[nome]] <- pme2
  # names(lista_PME[[nome]]) <- nombres
  saveRDS(lista_PME[[nome]],paste0("PME_RDS/",nome,".RDS"))
}

#######################################
pme2 <- readRDS("PME_RDS/01-2016.RDS")["V035"]
options( survey.lonely.psu = "adjust" )
rm(pme2)

# save# Loop de tratamento
# lista_POSES
arquivos_temp <- gsub("[A-z ./]","",arquivos)
arquivos_temp <- gsub("20","-20",arquivos_temp,".RDS")
arquivos_temp <- paste0("PME_RDS/",arquivos_temp,".RDS")
variaveis <- c("V113","V112","V211","V035","V114","V234","VD2","VD3","V401","V402","V403","V203","V461","V465")
arquivos_RDS <- list.files("PME_RDS/",full.names = T)
arquivos_RDS2 <- arquivos_RDS[!arquivos_RDS %in% arquivos_temp]
for(i in arquivos_RDS2){
  pme <- read_rds(i)[variaveis]
  pme <-subset(pme,!is.na(V113))
  pme$V113 <- as.integer(pme$V113)
  pme$V112 <- as.integer(pme$V112)
  pme$V211 <- as.integer(pme$V211)
  pme$V035 <- as.integer(pme$V035)
  pme$V114 <- as.integer(pme$V114)
  pop_totals <- unique(as.data.frame(pme[ , c( 'V035' , 'V114' ) ] ))
  
  pme <- svydesign(
    ids = ~ V113,
    strata = ~ V112 ,
    data = pme ,
    weights = ~ V211 , 
    nest = TRUE)
  
  pme <- postStratify(pme, ~ V035 , pop_totals )
  
  pme <- 
    update( 
      pme , 
      
      one = 1 ,
      
      # calculate whether each person is at least ten years of age
      pia = as.numeric( V234 >= 10 ),
      pea = as.numeric( VD3 == 1 , V234 >= 10),
      pea_a = as.numeric( VD3 == 1 , V234>=18),
      
      # determine individuals who are employed
      ocup_c = as.numeric( V401 == 1 | V402 == 1 | V403 == 1 ) ,
      #desocup_c = as.numeric( V401 == 2 | V402 == 2 | V403 == 2 ),
      
      sexo = factor( V203 , labels = c( "male" , "female" ) ) ,
      
      region = 
        factor( 
          V035 , 
          levels = c( 26 , 29 , 31 , 33 , 35 , 43 ) , 
          labels = c( "Recife" , "Salvador" , "Belo Horizonte" , 
                      "Rio de Janeiro" , "Sao Paulo" , "Porto Alegre" )
        ),
      grupo_idade = ifelse(as.numeric(V234) >= 18 & as.numeric(V234) <= 29,
                           1,0),
      grupo_idade = ifelse(as.numeric(V234) >= 30 & as.numeric(V234) <= 49,
                           2,grupo_idade),
      grupo_idade = ifelse(as.numeric(V234) >= 49 & as.numeric(V234) <= 59,
                           3,grupo_idade),
      grupo_idade = ifelse(as.numeric(V234) >= 60,
                           4,grupo_idade),
      grupo_idade = factor(grupo_idade,levels = c(0,1,2,3,4),
                           labels = c("Menor de 18",
                                      "18 a 29 anos",
                                      "30 a 49 anos",
                                      "50 a 59 anos",
                                      "60 anos ou mais")))
  
  
  
  pme <-
    update(
      pme ,
      
      # determine individuals who are unemployed
      desocup30 = as.numeric( ocup_c == 0 & !is.na( V461 ) & V465 == 1 ),
      pop_desocupada = factor(VD2,levels = c(1,2),labels = c("Trabalharam","NaoTrabalharam")),
      desocupados = pop_desocupada==1 | pop_desocupada ==2
      
      
    )
  
  pme <-
    update(
      pme ,
      
      # determine individuals who are either working or not working
      pea_c = as.numeric( ocup_c == 1 | desocup30 == 1 )
      
    )
  # pea[[i]] <- svyby(~pea_c,~ grupo_idade, pme,svytotal,na.rm=T)
  # desemprego[[i]] <- svyby(~desocup30,~ grupo_idade, pme,svytotal,na.rm=T)
  # +pea_c+desocupteste+pea+one
  temp <- cbind.data.frame(as.data.frame(svyby(~ocup_c,~ grupo_idade, pme,svytotal,na.rm=T))[1:2],
                           as.data.frame(svyby(~pea_a,~ grupo_idade, pme,svytotal,na.rm=T))[2],
                           as.data.frame(svyby(~pop_desocupada,~ grupo_idade, pme,svytotal,na.rm=T))[2:3])
  . <- sum( temp$pop_desocupadaTrabalharam,temp$pop_desocupadaNaoTrabalharam)
  data <- sub("PME_RDS/","01-",i)
  data <- sub(".RDS","",data)
  temp$Data <- as.Date.character(data,format = "%d-%m-%Y")
  
  if(i == arquivos_RDS2[1]){
    taxa_desemprego2 <- temp
  }
  else{
    taxa_desemprego2 <- rbind.data.frame(temp,taxa_desemprego2)
  }
  
  rm(pme,pop_totals,data,temp)
  
  cat("\nRodando a PME ",i)
}

taxa_desemprego3 <- rbind.data.frame(taxa_desemprego,taxa_desemprego2)
taxa_desemprego1 <- taxa_desemprego
# taxa_desemprego1$"Taxa de Desemprego" <- taxa_desemprego1$DesocupadosTotal/taxa_desemprego1$pea_a
taxa_desemprego3$"Taxa de Desemprego" <- taxa_desemprego3$DesocupadosTotal/taxa_desemprego3$pea_a
row.names(taxa_desemprego3) <- 1:nrow(taxa_desemprego3)

names(taxa_desemprego3)<- c("Idade","Ocupados","PEA","Desocupado que já trabalhou","Desocupado que nunca trabalhou","Desocupados Total","Data","Taxa Desemprego")

taxa_final <- taxa_desemprego3 %>%
  select(Data,`Taxa Desemprego`,Idade) %>%
  spread(key = Idade,value = `Taxa Desemprego`)

ocupados_final <- taxa_desemprego3 %>%
  select(Data,Ocupados,Idade) %>%
  spread(key = Idade,value = Ocupados)

PEA_final <- taxa_desemprego3 %>%
  select(Data,PEA,Idade) %>%
  spread(key = Idade,value = PEA)

desocupado_final <- taxa_desemprego3 %>%
  select(Data,`Desocupados Total`,Idade) %>%
  spread(key = Idade,value = `Desocupados Total`)

trabalhou_final <- taxa_desemprego3 %>%
  select(Data,`Desocupado que já trabalhou`,Idade) %>%
  spread(key = Idade,value = `Desocupado que já trabalhou`)

ntrabalhou_final <- taxa_desemprego3 %>%
  select(Data,`Desocupado que nunca trabalhou`,Idade) %>%
  spread(key = Idade,value = `Desocupado que nunca trabalhou`)


write.xlsx(taxa_final,file = "PME.xlsx",sheetName = "Taxa Desemprego")
write.xlsx(ocupados_final,file = "PME.xlsx",sheetName = "Ocupados",append = T)
write.xlsx(PEA_final,file = "PME.xlsx",sheetName = "PEA",append = T)
write.xlsx(desocupado_final,file = "PME.xlsx",sheetName = "Desocupados Total",append = T)
write.xlsx(trabalhou_final,file = "PME.xlsx",sheetName = "Desocupado que já trabalhou",append = T)
write.xlsx(ntrabalhou_final,file = "PME.xlsx",sheetName = "Desocupado que nunca trabalhou",append = T)

