# Medida de centralidad de cercania, cuantos
#             pasos son requeridos para alcanzar a cada uno
#             de los otros nodos desde el nodo dado.
encuentra_centralidad_cercania <- function( red ){
  closeness( red )
}

#Medida de Cercanía por Grado
encuentra_cercarnia_grado <- function( red ){
  grado_de_nodos <- degree( red )
  sort( grado_de_nodos, decreasing = TRUE )
  hist( grado_de_nodos )
  plot( degree_distribution( red ) )
  
  #le agregamos un atributo, (size) según el grado de los nodos
  V( red )$size <- (grado_de_nodos*20)/74
  
  
  color_tipo_cancer <- c( "bisque", "cadetblue1", "black", "yellow" )
  
  plot( red,
       vertex.color="lightsteelblue2",
       vertex.frame.color="black",
       edge.color="black", 
       edge.width=4,
       #arrow.size=5,
       layout=layout.random )
}

#Crea el rango del grado de una red
#Primera posición: máximo
#Segunda posicion: mínimo
encuentra_rango_grado <- function( red ){
  gmed <- degree( red )
  max_value <- max( gmed )
  min_value <- min( gmed )
  rango <- c( max_value, min_value )
  return (rango)
}

# Medida de intermediacion
#             La intermediacion del nodo y del vinculo
#             se define como el numero de geodesicas 
#             (caminos cortos) que pasan a traves de un
#             nodo o vinculo.
encuentra_cercarnia_intermediacion <- function( red ){
  
  intermediacion <- betweenness(red, directed=FALSE)
  V(red)$size <- grado_de_nodos*2
  plot(red,
       vertex.color="lightsteelblue2",
       vertex.frame.color="black",
       edge.color="gray85", 
       edge.width=4)
  #sort( intermediacion, decreasing = TRUE )
  hist(intermediacion,
       col="lightgreen",
       main="Histograma de intermediacion",
       xlab="Intermediacion de la grafica",
       xlim=c(15,35),  
       ylim=c(0, .20))
}

encuentra_eigen_centralidad <- function( red ){
  eigen_centrality(red_cancer)
}

#Eigen Vector
#Exhibe quien de los nodos está más conectado
encuentra_Eigen_Vector <- function( red ){
  # 1. Creamos una matriz de adyacencia
  matrix_adj <- as_adj( red_cancer )
  # 2. Se crea un vector de 1´s
  #2.1 Guardamos la cantidad de nodos
  num_nodos <- gorder( red_cancer )
  #2.2 Creamos el vector de 1´s usando la cantida de nodos
  vector_unos <- as.vector( rep( 1, num_nodos) )
  #3.Estamos calculando la centralidad de eigen-vector
  #operador de multiplicacion matrix*vector
  eigen_vector <- matrix_adj %*% vector_unos
  #3.1.Como pueden ser muchos nodos, entonces los ordenamos
  eigen_vector[order(eigen_vector[,1], decreasing = TRUE),]  
}