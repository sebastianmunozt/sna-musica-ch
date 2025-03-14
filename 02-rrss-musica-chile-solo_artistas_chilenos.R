
## Cargo paquetes
pacman::p_load(pacman, tidyverse, networkD3,igraph, htmlwidgets, openxlsx, readr)



# -------------------------------------------------------------
# 1. Filtrar los nodos chilenos a partir de la variable "country"
#    Se asume que los artistas chilenos tienen "chile" en la columna country.
# -------------------------------------------------------------

edges <- read.xlsx("data/edges_data.xlsx", sheet = 1)
nodes <- read.xlsx("data/nodes_data.xlsx", sheet = 1)

nodes_chile <- nodes %>%
  filter(country == "chile")

# -------------------------------------------------------------
# 2. Filtrar las aristas para incluir solo aquellas cuyos nodos
#    existan en la base de nodos chilenos.
# -------------------------------------------------------------
filtered_edges <- edges[edges$id_0 %in% nodes_chile$spotify_id & 
                          edges$id_01 %in% nodes_chile$spotify_id, ]

# Opcional: revisar algunos registros filtrados
print(head(filtered_edges))

# -------------------------------------------------------------
# 3. Calcular el grado (número de conexiones) de cada nodo en 
#    base a las aristas filtradas
# -------------------------------------------------------------
node_degrees <- filtered_edges %>%
  pivot_longer(cols = c("id_0", "id_01"), values_to = "node") %>%
  count(node) %>%
  rename(spotify_id = node, degree = n)

# -------------------------------------------------------------
# 4. Unir la información del grado a la base de nodos chilenos
#    Si un nodo no tiene aristas se le asigna un grado mínimo de 1.
# -------------------------------------------------------------
nodes_chile <- nodes_chile %>%
  left_join(node_degrees, by = "spotify_id") %>%
  mutate(degree = ifelse(is.na(.data$degree), 1, .data$degree))


# -------------------------------------------------------------
# 5. Preparar la tabla de nodos para networkD3:
#    Se escala el tamaño del nodo (por ejemplo, degree * 5) y se asigna un grupo único.
# -------------------------------------------------------------
nodes_d3 <- nodes_chile %>%
  mutate(size = degree * 5,
         group = 1) %>%    # Todos en el mismo grupo, usará el color predeterminado
  select(id = spotify_id, name, size, group)

# -------------------------------------------------------------
# 6. Procesar la tabla de aristas para networkD3:
#    Convertir los identificadores (id_0 e id_01) a índices basados en 0.
# -------------------------------------------------------------
edges_d3 <- filtered_edges %>%
  mutate(source = match(id_0, nodes_chile$spotify_id) - 1,
         target = match(id_01, nodes_chile$spotify_id) - 1)

# Opcional: verificar que los índices se han generado correctamente (sin NA)
print(head(edges_d3))
print(sum(is.na(edges_d3$source)))
print(sum(is.na(edges_d3$target)))

# -------------------------------------------------------------
# 7. Generar el grafo interactivo sin diferenciación de colores
# -------------------------------------------------------------
network_chile <- forceNetwork(
  Links = edges_d3,
  Nodes = nodes_d3,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "group",     # Con un único grupo se usará el color predeterminado
  Nodesize = "size",
  fontSize = 14,
  linkDistance = 100,
  zoom = TRUE,
  opacity = 0.85
)

# Mostrar el grafo en el Viewer de RStudio
print(network_chile)

# -------------------------------------------------------------
# 8. Guardar el widget en un archivo HTML auto-contenido
# -------------------------------------------------------------
htmlwidgets::saveWidget(network_chile, "network_chile.html", selfcontained = TRUE)
