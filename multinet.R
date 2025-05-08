library(multinet)

# Load your multiplex network (replace with your data)
net <- read_ml("data/Rising Waters, Resilient Communities - cascade_effects_data - 2025-04-30.csv")

# Default plot
plot(net, 
     vertex.labels = actors_ml(net),  # Show node names
     show.layer.names = TRUE,         # Display layer labels
     layer.names.cex = 1.2)           # Enlarge layer titles
# Force-directed layout for multilayer networks
plot(net, 
#     layout = layout.multiforce.ml,
     grid = c(1,4),          # Arrange layers in 1 row, 3 columns
     mai = c(0.05,0.05,0.05,0.05))  # Reduce margins
# Detect communities first
# Detect communities
communities <- glouvain_ml(net, gamma=1, omega=1)  # gamma=resolution, omega=inter-layer coupling

# Preview output
head(communities)

# Plot with communities
plot(net,
     com = communities,
     com.cex = 1.3,          # Expand community boundaries
     vertex.color = "membership")  # Color by community

Q <- modularity_ml(net, communities, gamma=1, omega=1)
print(paste("Modularity:", round(Q, 3)))

net_igraph <- as.igraph(net, layers = NULL)
plot(net_igraph)
