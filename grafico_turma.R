#imports
library(ggplot2)
library(gridExtra)

data <- read.csv("dados_estudantes.csv", sep = ';')


#Tabela turmas e gráfico de turmas
contagem_turmas = table(data$Turma)
contagem_turmas_table = data.frame(Turma = names(contagem_turmas), Frequencia = as.numeric(contagem_turmas))
contagem_turmas_grob = tableGrob(contagem_turmas_table)
grid.arrange(
  contagem_turmas_grob,
  pie(
    contagem_turmas_table$Frequencia,
    labels = paste(
      contagem_turmas_table$Turma, 
      ": ", 
      contagem_turmas_table$Frequencia
    ),
    main = "Gráfico de distribuição de alunos por turma"
    ),
  nrow = 1
)



#Tabela sexo gráfico sexo
contagem_sexo = table(data$Sexo)
contagem_sexo_table = data.frame(
  Sexo = names(contagem_sexo),
  Frenquencia = as.integer(contagem_sexo)
)
contagem_sexo_grob = tableGrob(contagem_sexo_table)
grid.arrange(
  contagem_sexo_grob,
  ggplot(
    contagem_sexo_table,
    aes(
      x = Sexo,
      y= Frenquencia
    )
  )+
    geom_bar(stat = "identity"),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)


#Tabela de idade e gráfico de idade
contagem_idade = table(data$Idade)
contagem_idade_table = data.frame(
  Idade = names (contagem_idade),
  Frequencia = as.integer(contagem_idade)
)
contagem_idade_grob = tableGrob(contagem_idade_table)
grid.arrange(
  contagem_idade_grob,
  idade_grafico = ggplot(
    contagem_idade_table,
    aes(
      x = Idade, 
      y = Frequencia
    )
  )+ 
    geom_bar(stat = "identity") +
    labs(title = "Frequências das idades dos alunos") +
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)


#Tabela de altura e gráfico de altura
altura = data$Alt
n_classes = ceiling(log(length(altura), 2))
amplitude_de_classe = (max(altura) - min (altura))/n_classes
intervalos = seq(min(altura), max(altura), amplitude_de_classe)
classes = cut(altura, breaks = intervalos, include.lowest = TRUE)
tabela_classes = as.data.frame(table(classes))
altura_grob = tableGrob(tabela_classes)
grid.arrange(
  altura_grob,
  plot_altura = ggplot(data = tabela_classes, aes(x = classes, y = Freq)) + 
    geom_bar(stat = "identity")+
    labs(title = "Ocorrência de alunos por estatura")+
    xlab("Intervalo de estatura")+
    ylab("Número de ocorrências")+
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)


#Tabela de peso e gráfico de peso
peso = data$Peso
n_classes_peso = ceiling(sqrt(length(peso)))
amplitude_de_classe_peso = ceiling((max(peso) - min(peso)) / n_classes_peso)
intervalos_peso = seq(min(peso), max(peso) + amplitude_de_classe_peso, amplitude_de_classe_peso)
classes_peso = cut(peso, breaks = intervalos_peso, include.lowest = TRUE)
tabela_classes_peso = table(classes_peso)
classes_peso_data_frame = as.data.frame(tabela_classes_peso)
peso_grob = tableGrob(classes_peso_data_frame)
grid.arrange(
  peso_grob,
  peso_plot = ggplot(classes_peso_data_frame, aes(classes_peso, Freq)) +
    geom_bar(stat = "identity")+
    labs(title = "Distruição de alunos por faixa de peso")+
    xlab("Intervalo de peso")+
    ylab("Número de alunos")+
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)

#Tabela de numero de filhos e gráfico de número de filhos
filhos = data$Filhos
filhos_factor_levels = min(filhos): max(filhos)
filhos_factor = factor(data$Filhos, levels = filhos_factor_levels)
filhos_table = table(filhos_factor)
filhos_data_frame = data.frame(
  "NumeroDeFilhos" = names(filhos_table),
  "Frequencia" = as.vector(filhos_table))
filhos_grob = tableGrob(
  filhos_data_frame,
  cols = c("Número de filhos", "Frequência")
)
grid.arrange(
  filhos_grob,
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
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)


#Tabela fumantes e gráfico fumantes
fumantes_table = table(data$Fuma)
fumantes_data_frame = data.frame(
  "Fuma" = names(fumantes_table),
  "Contagem" = as.vector(fumantes_table))
fumantes_grob = tableGrob(fumantes_data_frame)
grid.arrange(
  fumantes_grob,
  pie(
    fumantes_table,
    labels = paste(names(fumantes_table), ": ", fumantes_table),
    main = "Fumantes"),
  nrow = 1,
  ncol = 2,
  widths = c(1/4, 3/4)
)


#Tabela toler e gráfico Toler
toler_table = data.frame(table(data$Toler))
colnames(toler_table) = c("Tolerância", "Frequencia")
toler_grob = tableGrob(toler_table)
grid.arrange(
  toler_grob,
  ggplot(
    toler_table,
    aes(
      x = Tolerância,
      y = Frequencia
    )
  )+
    geom_bar(
      stat = "identity"
    ),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)



#Tabela exerc e gráfico exerc
exerc_table = table(data$Exerc)
exerc_data_frame = data.frame(exerc_table)
colnames(exerc_data_frame) = c("Horas de atividade física semanal", "Frequência")
exerc_grob = tableGrob(exerc_data_frame)
grid.arrange(
  exerc_grob,
  exerc_plot = ggplot(
    exerc_data_frame,
    aes(
      x = `Horas de atividade física semanal`,
      y = `Frequência`
    )
  )+ geom_bar(
    stat = "identity"
    )+
    xlab("Horas de atividade física semanal")+
    ylab("Contagem dos alunos")+
    labs(title = "Contagem de alunos por tempo de atividade física semanal (em horas)"),
  nrow = 1,
  ncol = 2
)



#Tabela cine e gráfico cine
cine_table = table(data$Cine)
cine_data_frame = data.frame(cine_table)
colnames(cine_data_frame) = c("Número idas ao cinema por semana", "Número de alunos")
cine_grob = tableGrob(cine_data_frame)
grid.arrange(
  cine_grob,
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
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 1,
  ncol = 2
)


#Tabela opcine e gráfico opcine
opcine_table = table(data$OpCine)
opcine_data_frame = data.frame(opcine_table)
colnames(opcine_data_frame) = c("Opinião", "Contagem de alunos")
levels(opcine_data_frame$Opinião) = c("Regular e Boa", "Muito Boa")
opcine_grob = tableGrob(opcine_data_frame)
grid.arrange(
  opcine_grob,
  pie(
    opcine_data_frame$`Contagem de alunos`,
    main = "Opinião a respeito das salas de cinema na cidade",
    labels = paste(opcine_data_frame$`Opinião`, ":", opcine_data_frame$`Contagem de alunos`)
  ),
  nrow = 1,
  ncol = 2,
  widths = c(1/3, 2/3)
)



#Tabela TV e plot
n_classes_tv = ceiling(sqrt(length(data$TV)))
amplitude_classe_tv = ceiling((max(data$TV) - min(data$TV))/n_classes_tv)
intervalo_tv = seq(min(data$TV), max(data$TV), amplitude_classe_tv)
classes_tv = cut(data$TV, intervalo_tv, include.lowest = TRUE)
tv_data_frame = as.data.frame(table(classes_tv))
colnames(tv_data_frame) = c("Intervalos de horas assistidas de TV semanais", "Número de alunos")
tv_grob = tableGrob(tv_data_frame)
grid.arrange(
  tv_grob,
  tv_plot = ggplot(tv_data_frame, aes(x = `Intervalos de horas assistidas de TV semanais`, y = `Número de alunos`)) +
    geom_col() +
    labs(x = "Intervalos de horas assistidas de TV semanais", y = "Número de alunos")+
    labs(title = "Contagem de alunos por horas de TV assistidas semanalmente")+
    theme(plot.title = element_text(hjust = 0.5))
)



#Tabela OpTv e plot
op_tv_data_frame = data.frame(table(data$OpTV))
levels(op_tv_data_frame$Var1) = c("Boa", "Média", "Não Sabe", "Ruim")
colnames(op_tv_data_frame) = c("Opinião", "Número de alunos")
op_tv_grob = tableGrob(op_tv_data_frame)
grid.arrange(
  pie(
    op_tv_data_frame$`Número de alunos`,
    labels = paste(op_tv_data_frame$`Opinião`, ": ", op_tv_data_frame$`Número de alunos`),
    main = "Opinião dos alunos a respeito da qualidade da programação de TV"
  ),
  op_tv_grob,
  nrow = 2,
  heights = c(3/4, 1/4)
)
