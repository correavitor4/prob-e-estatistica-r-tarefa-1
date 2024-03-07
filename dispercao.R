#imports
library(ggplot2)


data <- read.csv("dados_estudantes.csv", sep = ';')

altura = data$Alt
peso = data$Peso
idade = data$Idade



#Gráfico dispercao entre altura e peso
altura_peso_data_frame = data.frame(
  altura,
  peso
)
alt_peso_plot = ggplot(
  altura_peso_data_frame,
  aes(
    x = altura,
    y = peso
  )
)+
  geom_point()+
  xlab("Altura")+
  ylab("Peso")+
  labs(title = "Gráfico de dispersão de altura x peso")+
  theme(plot.title = element_text(hjust = 0.5))
print(alt_peso_plot)



#Gráfico de dispersão entre altura e idade
altura_idade_data_frame = data.frame(
  altura,
  idade
)
alt_idade_plot = ggplot(
  altura_idade_data_frame,
  aes(
    y = altura,
    x = idade
  )
)+
  geom_point()+
  labs(title = "Gráfico de dispersão entre altura e idade")
print(alt_idade_plot)