# Análise de Dados em R para a solução de Problemas de Negócio
# Fonte de dados: Kaggle
# Nome do conjunto de dados: superstore sales
# Link para o conjunto de dados: https://www.kaggle.com/code/tamader/superstore-sales

# Perguntas de Negócio: 

# Qual é a tendência de vendas ao longo do tempo? (Com base na coluna "Order.Date" e "Sales")

# Qual é o produto mais vendido?

# Qual é a loja com as vendas mais altas? (Com base nas colunas "City" e "Sales")

# Qual é a Região com as vendas mais altas?

# Quais são os dias da semana com as maiores vendas? (Com base nas colunas "Order.Date" e "Sales")

# Qual é a relação entre a categoria do produto e suas vendas? (Com base nas colunas "Category" e "Sales")

# Qual é a relação entre a subcategoria do produto e suas vendas? (Com base nas colunas "Sub.Category" e "Sales")

# Qual é o segmento de clientes com as maiores vendas? (Com base nas colunas "Segment" e "Sales")

# Qual é a distribuição geográfica das vendas? (Com base nas colunas "City", "State", "Region" e "Sales")

# Qual é a relação entre o modo de envio e as vendas? (Com base nas colunas "Ship.Mode" e "Sales")


# Importando Bibliotecas
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(lubridate)

# Carregando conjuntos de Dados
df = read.csv("train.csv")


# Verificnado as Informaçoes do Dataframe
dim(df) # 9800 x 18
glimpse(df)
summary(df)


# Visualizando o Dataframe
View(df)
head(df)


# Verificando a existeecia de valores NA

# Criando a função
verifica_na = function(x){
  colSums(is.na(x))
}

# Chmanado o Dataframe dentro da função criada acima
verifica_na(df) # Existem 11 Observações com valores NA presentes na coluna "Postal.code"

# Retornando as linas com valores NA
linhas_na <- which(rowSums(is.na(df)) > 0)
print(df[linhas_na, ])

# Removendo as linhas volores NA do Dataframe
df <- na.omit(df)

# Verificando a quantidade de linhas
dim(df) # 9789 x 18


# Verificando se existem linhas duplicadas
verifica_duble = function(x){
  sum(duplicated(x))
}

# Chamando o Dataframe dentro da função criada acima
verifica_duble(df) # Não existe duplicidade nos dados


# Verificando se existe valores inconsistentes na variável Sales
which(df$Sales <= 0) # Nãoo existe valores menores que 0 nessa variável


# Convertendo para o tipo .date as colunas que correspondem a datas
df$Order.Date <- dmy(df$Order.Date)
df$Ship.Date <- dmy(df$Ship.Date) 

# Verificando a alteração
str(df$Order.Date)
str(df$Ship.Date)


# Excluindo a coluna Row.ID
df$Row.ID <- NULL
dim(df) # 9789 x 17

# Verificando os Valores únicos da variável Country
colnames(df)

#Criando um variável com as colunas de interesse do Dataframe
lista_1 <- colnames(df[, c("Ship.Mode", "Segment", "Country", "Region", "Category", "Sub.Category")])

# Função que retorna os valores únicos das colunas de interesses selecionadas aciama
lapply(df[ ,lista_1], unique)


# Categorizando as variáveis acima para o tipo factor

# "Ship.Mode", "Segment", "Country", "Region", "Category", "Sub.Category"

categoria <- function(x){
  as.factor(x)
}

# Chamando o datagrame na função criada acima
df[ , lista_1] <- lapply(df[ , lista_1], categoria)

# Verificando o resultado
glimpse(df[, lista_1])


#### Respondendo as Perguntas de Negócio ####

# Pergunta 1: Qual é a tendência de vendas ao longo do tempo? 
# (Com base na coluna "Order.Date" e "Sales")

pergunta_1 <- df %>%
  group_by(Order.Date) %>%
  summarise(Total.Sales = sum(Sales))

View(pergunta_1)

ggplot(pergunta_1, aes(Order.Date, Total.Sales)) +
  geom_line() +
  ggtitle("Tendência de vendas ao longo do tempo") +
  xlab("Data") +
  ylab("Vendas acumuladas") + 
  ggtitle("Relação Tempo e Total de Vendas")


# Pergunta 2: Qual é o produto mais vendido?

pergunta_2 <- df %>%
  group_by(Product.Name) %>%
  select(Product.Name )%>%
  summarise(Total.Vendas = n())%>%
  arrange(desc(Total.Vendas))

View(pergunta_2)

# Retorando o Produto mais vendido
mais_vendido = pergunta_2[1, ]
mais_vendido # Staple envelope


# Pergunta 3: Qual é a loja com as vendas mais altas? (Com base nas colunas "City" e "Sales")
pergunta_3 <- df %>%
  group_by(City) %>%
  select(City, Sales)%>%
  summarise(Total.Sales = sum(Sales))%>%
  arrange(desc(Total.Sales))

pergunta_3

mais_vendido_city <- pergunta_3[1, ]
mais_vendido_city # New York City


# Pergunta 3: Qual é a Região com as vendas mais altas?
pergunta_4 <- df %>%
  group_by(Region) %>%
  select(City, Sales, Region)%>%
  summarise(Total.Sales = sum(Sales))%>%
  arrange(desc(Total.Sales))

pergunta_4

mais_vendido_region <- pergunta_4[1, ]
mais_vendido_region # West 

ggplot(pergunt_4, aes(y = Total.Sales, x = Region, fill = Region)) +
  geom_bar(stat = "identity") +
  xlab("Regions") + 
  ylab("Vendas acumuladas") +
  scale_fill_brewer(type = "qual", palette = "Paired") + 
  ggtitle("Relação entre Região e Total de Vendas")


# Pergunta 5: Quais são os dias da semana com as maiores vendas? 
# (Com base nas colunas "Order.Date" e "Sales")
pergunta_5 <- df %>%
  mutate(Week.Days = wday(Order.Date, label = TRUE))%>%
  group_by(Week.Days)%>%
  select(Week.Days, Sales)%>%
  summarise(Total.Sales = sum(Sales))%>%
  arrange(Week.Days)

pergunta_5
  
ggplot(pergunta_5, aes(y = Total.Sales, x = Week.Days, fill = Week.Days)) +
  geom_bar(stat = "identity") +
  xlab("Dias da Semana") + 
  ylab("Vendas acumuladas") +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  ggtitle("Relação entre Média de Vendas e Total de Vendas")


# pergunta 6: Qual é a relação entre a categoria do produto e suas vendas? 
# (Com base nas colunas "Category" e "Sales")
pergunta_6 <- df %>%
  group_by(Category) %>%
  summarize(Media_Vendas = mean(Sales)) %>%
  ggplot(aes(x = Category, y = Media_Vendas, fill = Category)) +
  geom_bar(stat = "identity") +
  xlab("Categoria de Produto") + 
  ylab("Vendas Médias") +
  ggtitle("Relação entre Categoria do Produto e Média de Vendas")

pergunta_6


# Pergunta 7: Qual é a relação entre a subcategoria do produto e suas vendas? 
# (Com base nas colunas "Sub.Category" e "Sales")
pergunta_7 <- df%>%
  group_by(Sub.Category)%>%
  summarise(Media_Vendas = mean(Sales))%>%
  ggplot(aes(x = Sub.Category, y = Media_Vendas, fill = Sub.Category))+
  geom_bar(stat = "identity")+
  xlab("Sub-Categoria de Produtos")+
  ylab("Média Vendas")+
  ggtitle("Relação entre Sub-Catgoria de Produto e Média de Vendas")

pergunta_7


# Qual é o segmento de clientes com as maiores vendas? (Com base nas colunas "Segment" e "Sales")
pergunta_8 <- df%>%
  group_by(Segment)%>%
  select(Segment, Sales)%>%
  summarise(Total_Sales = sum(Sales))%>%
  ggplot(aes(x = Segment, y = Total_Sales, fill = Segment)) +
  geom_bar(stat = "identity") +
  xlab("Segmento de clientes") + 
  ylab("Total de Vendas") +
  ggtitle("Relação entre Segmento de clientes e Total de Vendas")

pergunta_8 # Consumer


# Pergunta 9: Qual é a distribuição geográfica das vendas? 
# (Com base nas colunas "City", "State", "Region" e "Sales")

pergunta_9 <- df %>%
  group_by(Region) %>%
  summarise(Total_Vendas = sum(Sales)) %>%
  ggplot(aes(x = "", y = Total_Vendas, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  ggtitle("Distribuição geográfica das vendas") +
  xlab("") +
  ylab("Vendas acumuladas") +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

pergunta_9


# Pergunta 10: # Qual é a relação entre o modo de envio e as vendas? 
# (Com base nas colunas "Ship.Mode" e "Sales")
pergunta_10 <- df %>%
  group_by(Ship.Mode) %>%
  summarise(Total_Vendas = sum(Sales)) %>%
  ggplot(aes(y = Total_Vendas, x = Ship.Mode, fill = Ship.Mode)) +
  geom_bar(stat = "identity", orientation = "x")+
  coord_flip()+
  ylab("Total de Vendas") +
  xlab("Modo de Envio") +
  ggtitle("Relação entre Total de Vendas e Modo de Envio")

pergunta_10

































































































































































