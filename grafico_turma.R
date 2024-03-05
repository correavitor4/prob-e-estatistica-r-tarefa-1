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
  # Frequencia = as.integer(contagem_idade)
)
ggplot(
  contagem_idade_table,
  aes(
    x = Idade, 
    y = Frequencia
  )
)+ 
  geom_bar(stat = "identity") +
  labs(title = "Frequências das idades dos alunos") +
  theme(plot.title = element_text(hjust = 0.5))
