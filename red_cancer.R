##########		Redes de Cáncer Integradas		#########
# @lunysska
# @dario
# @miguel
# @ana
#
# Dec, 2018


#Modificar la ruta asignada para los archivos
setwd( "/Users/virg/Downloads" )

source( 'util.R' )
source( 'medidas_centralidad.R' )

# Instalar paquetes y cargar sus librerías
usa_paquete( "igraph", "tcltk" )

# Leer y asignar al archivo csv de interacciones
# de genese en cancer de cerebro

#cancer <- read.csv("string_interactions_pulmon.csv",
#                   header = TRUE, as.is = TRUE)
cancer <- read.csv("Redes_Cancer.csv",
                   header = TRUE, as.is = TRUE)

cancer
head(cancer)

# Generar red de interaccion
red_cancer <- graph_from_data_frame(cancer, directed = FALSE)


# Me aparece un error:
# Error in seq_len(no) : argument must be
# coercible to non-negative integer

V( red_cancer )$size <- (grado_de_nodos*20)/74

color_tipo_cancer <- c( "bisque", "cadetblue1", "black", "yellow" )
color_tipo_cancer
V(red_cancer)$Tipo
colorgen[V(red_amistadsc)$num_gen]
V(red_cancer)$color <- color_tipo_cancer[V(red_cancer)$Tipo]

plot( red_cancer,
      vertex.color="lightsteelblue2",
      #vertex.frame.color="black",
      edge.color="black", 
      #edge.width=4,
      #arrow.size=5,
      vertex.label=NA)

tkplot(red_cancer)



red_cancer
plot(red_cancer)

print_all(red_cancer)

class(red_cancer)
mode(red_cancer)
attributes(red_cancer)

# Abre una ventana que permite manipular el
# grafo


# Pimpeando el grafo
plot(red_cancer, vertex.size = 20,
     vertex.color="lightsteelblue2",
     vertex.frame.color="black",
     vertex.label.color = "black",
     edge.width=4, edge.color= "gray85")

# Para hacer una matriz
as_adj(red_cancer) # 157 x 157 sparse Matrix of class "dgCMatrix"
A <- as_adj(red_cancer)
A  

# dim -> para ver la dimension de la tabla
dim(A)


E(red_cancer) # enlista los vinculos de la red
V(red_cancer) # enlista los nodos de la red


# Obtener atributos de los vinculos: names 

V(red_cancer)$name	
# Por omision extrae el atributo “name” del “from” “to” (primeras dos columnas) del archivo


# red simplificada completa "red_cancer_simpl" <- simplify
# No bucles, no direcciones y no multimvinculos

red_cancer_simpl <- simplify(red_cancer)
red_cancer_simpl
plot(red_cancer_simpl)

# Para remover multivinculos y loops como si fuera directo "simplify"
red_cancer_simple <- simplify(red_cancer, remove.multiple = TRUE,
                              remove.loops = TRUE)
plot(red_cancer_simple)

encuentra_eigen_centralidad( red_cancer )
encuentra_cercarnia_grado( red_cancer )
encuentra_rango_grado( red_cancer )
encuentra_cercarnia_intermediacion( red_cancer )

