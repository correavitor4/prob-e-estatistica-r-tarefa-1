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
peso_plot = ggplot(classes_peso_data_frame, aes(classes_peso, Freq)) +
  geom_bar(stat = "identity")+
  labs(title = "Distruição de alunos por faixa de peso")+
  xlab("Intervalo de peso")+
  ylab("Número de alunos")+
  theme(plot.title = element_text(hjust = 0.5))
print(peso_plot)

#Tabela de numero de filhos e gráfico de número de filhos
filhos = data$Filhos
filhos_factor_levels = min(filhos): max(filhos)
filhos_factor = factor(data$Filhos, levels = filhos_factor_levels)
filhos_table = table(filhos_factor)
filhos_data_frame = data.frame(
  "NumeroDeFilhos" = names(filhos_table),
  "Frequencia" = as.vector(filhos_table))

filhos_plot = ggplot(
  filhos_data_frame,
  aes(
    x= NumeroDeFilhos,
    y= Frequencia
  )
)+
  geom_bar(stat = "identity")+
  labs(title = "Frequência de alunos por quantidade de filhos")+
  xlab("Quantidade de filhos")+
  ylab("Alunos que possuem essa quantidade de filhos")+
  theme(plot.title = element_text(hjust = 0.5))
print(filhos_plot)


#Tabela fumantes e gráfico fumantes
fumantes_table = table(data$Fuma)
fumantes_data_frame = data.frame(
  "Fuma" = names(fumantes_table),
  "Porcentagem" = as.vector(fumantes_table))

pie(
  fumantes_table,
  labels = paste(names(fumantes_table), ": ", fumantes_table),
  main = "Fumantes")



#Tabela toler e gráfico Toler
toler_table = data.frame(table(data$Toler))
colnames(toler_table) = c("Toler", "Frequencia")
toler_names_description = c("Indiferente", "Incomoda Muito", "Incomoda Pouco")
pie(
  table(data$Toler),
  main = "Tolerância ao cigarro",
  labels = paste(toler_names_description, ": ", table(data$Toler))
)



#Tabela exerc e gráfico exerc
exerc_table = table(data$Exerc)
exerc_data_frame = data.frame(exerc_table)
colnames(exerc_data_frame) = c("Horas de atividade física semanal", "Frequência")
exerc_plot = ggplot(
  exerc_data_frame,
  aes(
    x = `Horas de atividade física semanal`,
    y = Frequência
  )
)+ geom_bar(
  stat = "identity"
  )+
  xlab("Horas de atividade física semanal")+
  ylab("Contagem dos alunos")+
  labs(title = "Contagem de alunos por tempo de atividade física semanal (em horas)")
print(exerc_plot)



#Tabela cine e gráfico cine
cine_table = table(data$Cine)
cine_data_frame = data.frame(cine_table)
colnames(cine_data_frame) = c("Número idas ao cinema por semana", "Número de alunos")
cine_plot = ggplot(
  cine_data_frame,
  aes(
    x = `Número idas ao cinema por semana`,
    y = `Número de alunos`
  )
)+
  geom_bar(
    stat = "identity"
  )+
  labs(title = "Contagem de alunos por número de idas semanais ao cinema")+
  theme(plot.title = element_text(hjust = 0.5))
print(cine_plot)

#Tabela opcine e gráfico opcine
opcine_table = table(data$OpCine)
opcine_data_frame = data.frame(opcine_table)
colnames(opcine_data_frame) = c("Opinião", "Contagem de alunos")
levels(opcine_data_frame$Opinião) = c("Regular e Boa", "Muito Boa")
pie(
  opcine_data_frame$`Contagem de alunos`,
  main = "Opinião a respeito das salas de cinema na cidade",
  labels = paste(opcine_data_frame$`Opinião`, ":", opcine_data_frame$`Contagem de alunos`)
)



#Tabela TV e plot
n_classes_tv = ceiling(sqrt(length(data$TV)))
amplitude_classe_tv = ceiling((max(data$TV) - min(data$TV))/n_classes_tv)
intervalo_tv = seq(min(data$TV), max(data$TV), amplitude_classe_tv)
classes_tv = cut(data$TV, intervalo_tv, include.lowest = TRUE)
tv_data_frame = as.data.frame(table(classes_tv))
colnames(tv_data_frame) = c("Intervalos de horas assistidas de TV semanais", "Número de alunos")
tv_plot = ggplot(tv_data_frame, aes(x = `Intervalos de horas assistidas de TV semanais`, y = `Número de alunos`)) +
  geom_col() +
  labs(x = "Intervalos de horas assistidas de TV semanais", y = "Número de alunos")+
  labs(title = "Contagem de alunos por horas de TV assistidas semanalmente")+
  theme(plot.title = element_text(hjust = 0.5))
print(tv_plot)



#Tabela OpTv e plot
op_tv_data_frame = data.frame(table(data$OpTV))
levels(op_tv_data_frame$Var1) = c("Boa", "Média", "Não Sabe", "Ruim")
colnames(op_tv_data_frame) = c("Opinião", "Número de alunos")
pie(
  op_tv_data_frame$`Número de alunos`,
  labels = paste(op_tv_data_frame$`Opinião`, ": ", op_tv_data_frame$`Número de alunos`),
  main = "Opinião dos alunos a respeito da qualidade da programação de TV"
)
