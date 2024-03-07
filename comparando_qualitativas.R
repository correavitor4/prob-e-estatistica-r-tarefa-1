#imports
library(ggplot2)

#Reading CSV
data <- read.csv("dados_estudantes.csv", sep = ';')

sexo = data$Sexo
fuma = data$Fuma
opcine = data$OpCine


#Sexo e Fumar
sexo_fuma_table = table(sexo, fuma)
sexo_fuma_prop_table = prop.table(sexo_fuma_table, margin = 1)
sexo_fuma_prop_data_frame = data.frame(sexo_fuma_prop_table)
apenas_fumantes = 
  sexo_fuma_prop_data_frame[sexo_fuma_prop_data_frame$fuma == 'SIM' & sexo_fuma_prop_data_frame$sexo != "NI", ]
sexo_fumantes_plot = ggplot(
  apenas_fumantes,
  aes(
    x = sexo,
    y = Freq
  ),
)+
  geom_bar(
    stat = "identity"
  )+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Sexo", y = "Percentual", title = "Percentual de Fumantes por Sexo")
print(sexo_fumantes_plot)



#Sexo e opinião sobre o cinema
table_sexo_opcine = table(sexo, opcine)
table_sexo_opcine_prop = prop.table(table_sexo_opcine, 1)
sexo_op_cine_data_frame = data.frame(table_sexo_opcine_prop)
apenas_muito_bom = sexo_op_cine_data_frame[sexo_op_cine_data_frame$opcine == "B" & sexo_op_cine_data_frame$sexo != "NI", ]
apenas_bom_plot = 
  ggplot(apenas_muito_bom, aes(x = sexo, y = Freq)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Sexo", y = "Percentual", title = "Percentual de Opiniões 'Muito Bom' sobre o Cinema por Sexo")
print(apenas_bom_plot)