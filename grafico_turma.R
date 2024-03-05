#imports
library(ggplot2)


data <- read.csv("dados_estudantes.csv", sep = ';')


#Tabela turmas e gráfico de turmas
contagem_turmas = table(data$Turma)
contagem_turmas_table = data.frame(Turma = names(contagem_turmas), Frequencia = as.numeric(contagem_turmas))
pie(
  contagem_turmas_table$Frequencia,
  labels = paste(
    contagem_turmas_table$Turma, 
    ": ", 
    contagem_turmas_table$Frequencia
  ),
  main = "Gráfico de distribuição de alunos por turma"
)



#Tabela sexo gráfico sexo
contagem_sexo = table(data$Sexo)
contagem_sexo_table = data.frame(
  Sexo = names(contagem_sexo),
  Frenquencia = as.integer(contagem_sexo)
)
pie(
  contagem_sexo_table$Frenquencia,
  labels = paste(
    contagem_sexo_table$Sexo,
    ": ",
    contagem_sexo_table$Frenquencia
  ),
  main = "Gráfico de distribuição de alunos por sexo"
)


#Tabela de idade e gráfico de idade
contagem_idade = table(data$Idade)
contagem_idade_table = data.frame(
  Idade = names (contagem_idade),
  Frequencia = as.integer(contagem_idade)
)
idade_grafico = ggplot(
  contagem_idade_table,
  aes(
    x = Idade, 
    y = Frequencia
  )
)+ 
  geom_bar(stat = "identity") +
  labs(title = "Frequências das idades dos alunos") +
  theme(plot.title = element_text(hjust = 0.5))
print(idade_grafico)


#Tabela de altura e gráfico de altura
altura = data$Alt
n_classes = ceiling(log(length(altura), 2))
amplitude_de_classe = (max(altura) - min (altura))/n_classes
intervalos = seq(min(altura), max(altura), amplitude_de_classe)
classes = cut(altura, breaks = intervalos, include.lowest = TRUE)
tabela_classes = as.data.frame(table(classes))
plot_altura = ggplot(data = tabela_classes, aes(x = classes, y = Freq)) + 
  geom_bar(stat = "identity")+
  labs(title = "Ocorrência de alunos por estatura")+
  xlab("Intervalo de estatura")+
  ylab("Número de ocorrências")+
  theme(plot.title = element_text(hjust = 0.5))
print(plot_altura)


#Tabela de peso e gráfico de peso
peso = data$Peso
n_classes_peso = ceiling(sqrt(length(peso)))
amplitude_de_classe_peso = ceiling((max(peso) - min(peso)) / n_classes_peso)
intervalos_peso = seq(min(peso), max(peso) + amplitude_de_classe_peso, amplitude_de_classe_peso)
classes_peso = cut(peso, breaks = intervalos_peso, include.lowest = TRUE)
tabela_classes_peso = table(classes_peso)
classes_peso_data_frame = as.data.frame(tabela_classes_peso)
ggplot(classes_peso_data_frame, aes(classes_peso, Freq)) +
  geom_bar(stat = "identity")+
  labs(title = "Distruição de alunos por faixa de peso")+
  xlab("Intervalo de peso")+
  ylab("Número de alunos")+
  theme(plot.title = element_text(hjust = 0.5))


#Tabela de numero de filhos e gráfico de número de filhos
filhos = data$Filhos
filhos_factor_levels = min(filhos): max(filhos)
filhos_factor = factor(data$Filhos, levels = filhos_factor_levels)
filhos_table = table(filhos_factor)
filhos_data_frame = data.frame(
  "NumeroDeFilhos" = names(filhos_table),
  "Frequencia" = as.vector(filhos_table))

res = is.na(filhos_data_frame)
