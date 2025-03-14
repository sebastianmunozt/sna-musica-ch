## Cargo paquetes
pacman::p_load(pacman, tidyverse, networkD3,igraph, htmlwidgets, openxlsx, readr)

## Abro bases
edges <- read.xlsx("data/edges_data.xlsx", sheet = 1)
nodes <- read.xlsx("data/nodes_data.xlsx", sheet = 1)

head(nodes)
head(edges)

table(nodes$genero_r)

edges <- edges[edges$id_0 %in% nodes$spotify_id & 
                          edges$id_01 %in% nodes$spotify_id, ]




# Calcular el grado (número de conexiones) de cada nodo usando las columnas "id_0" y "id_01" de edges
node_degrees <- edges %>%
  pivot_longer(cols = c("id_0", "id_01"), values_to = "node") %>%
  count(node) %>%
  rename(spotify_id = node, degree = n)

# Unir la información del grado a la tabla de nodos.
# Para los nodos que no aparecen en la tabla de aristas se asigna un grado mínimo de 1.
nodes <- nodes %>%
  left_join(node_degrees, by = "spotify_id") %>%
  mutate(degree = ifelse(is.na(degree), 1, degree))

# Crear la tabla de nodos para networkD3.
# Se escala el tamaño del nodo en función del grado y se asigna un único grupo (1) a todos.
nodes_d3 <- nodes %>%
  mutate(size = degree * 5,
         group = 1) %>%      # Todos los nodos tendrán el mismo grupo, por lo que se usará el color predeterminado
  select(id = spotify_id, name, size, group)

# Procesar la tabla de aristas para networkD3:
# Se convierte el identificador de cada nodo a índices basados en 0.
edges_d3 <- edges %>%
  mutate(source = match(id_0, nodes$spotify_id) - 1,
         target = match(id_01, nodes$spotify_id) - 1)

# ---------------------------
# Creación del grafo interactivo sin colores diferenciados
# ---------------------------
network_simple <- forceNetwork(
  Links = edges_d3,
  Nodes = nodes_d3,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "group",    # Al asignar el mismo grupo, se usará el color por defecto para todos
  Nodesize = "size",
  fontSize = 14,
  linkDistance = 100,
  zoom = TRUE,
  opacity = 0.85
)

# Visualizar el grafo en el panel Viewer de RStudio
print(network_simple)

htmlwidgets::saveWidget(network_simple, "network_simple.html", selfcontained = TRUE)

#####################

