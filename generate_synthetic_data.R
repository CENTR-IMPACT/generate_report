library(readr)
library(glue)
library(this.path)

# Set the working directory to the source file location
setwd(this.path::here())

# Get the current date to identify this stage of the project
current_date <<- as.Date(Sys.time())

# Insert the name of the project here
project_name <<- "The Collaborative Project"

build_filename <- function(dataset_name) {
  return(glue("data/{project_name} - {dataset_name}_data - {current_date}.csv"))
}

# PROJECT DYNAMICS DATA

# Create a data frame named 'project_dynamics_score'.
dynamics_df <- data.frame(
  # Create a column named 'domain' with repeating domain names.
  # The 'rep()' function is used to repeat each domain name four times.
  domain = c(
    rep("Contexts", 4),
    rep("Partnerships", 4),
    rep("Research", 4),
    rep("Learning", 4),
    rep("Outcomes", 4)
  ),
  # Create a column named 'dimension' with the names of different dimensions
  # associated with the domains.
  dimension = c(
    "Challenge", "Diversity", "Resources", "Trust",
    "Beneficence", "Decisions", "Reflection", "Tools",
    "Design", "Time", "Questions", "Voice",
    "Civic Learning", "Integration", "Learning Goals", "Reciprocity",
    "Capabilities", "Goals", "Outputs", "Sustainability"
  ),
  dimension_value = round(runif(n = 20, min = 0.156, max = 1), 2)
)

dynamics_filename <- build_filename("dynamics")
write_csv(dynamics_df, dynamics_filename)

#### PROJECT ALIGNMENT

number_of_researchers <- sample(1:10, 1)
number_of_partners <- sample((number_of_researchers + 1):15, 1)
total_participants <- sum(number_of_researchers, number_of_partners)

alignment_df <- data.frame(
  alignment = c(
    rep("Goals", total_participants),
    rep("Values", total_participants),
    rep("Roles", total_participants),
    rep("Resources", total_participants),
    rep("Activities", total_participants),
    rep("Empowerment", total_participants),
    rep("Outputs", total_participants),
    rep("Outcomes", total_participants)
  ),
  role = rep(rep(c("researcher", "partner"), times = c(number_of_researchers, number_of_partners)), times = 8),
  rating = round(runif((8 * total_participants), 0.36, 1), 2)
)

alignment_filename <- build_filename("alignment")
write_csv(alignment_df, alignment_filename)

# CASCADE EFFECTS

# Function to create the social network dataframe
create_social_network <- function() {
  # Parameters for Layer 1
  n_layer1 <- sample(3:10, 1)  # Random number between 3 and 10 agents in Layer 1
  
  # Create Layer 1 agents (starting from 1)
  next_id <- 1
  layer1_agents <- next_id:(next_id + n_layer1 - 1)
  next_id <- next_id + n_layer1
  
  # Create agent mapping (optional for undirected edge list, but kept for completeness)
  agent_layers <- data.frame(agent_id = layer1_agents, layer = rep(1, length(layer1_agents)))
  
  # Connections will be stored as a list of data frames initially
  all_connections_list <- list()
  
  # Connect all Layer 1 agents with each other (undirected)
  # Generate all unique pairs to avoid duplicates in undirected network
  if (n_layer1 > 1) {
    layer1_pairs <- combn(layer1_agents, 2)
    layer1_connections <- data.frame(
      from = layer1_pairs[1, ],
      to = layer1_pairs[2, ],
      layer = 1 # Associate layer with the connection within Layer 1
    )
    all_connections_list[[length(all_connections_list) + 1]] <- layer1_connections
  }
  
  # Parameters for Layer 2
  layer2_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  layer2_agents <- c()
  
  for (agent in layer1_agents) {
    n_connections <- sample(1:3, 1) #sample(2:4, 1)
    new_agents <- next_id:(next_id + n_connections - 1)
    next_id <- next_id + n_connections
    layer2_agents <- c(layer2_agents, new_agents)
    
    # Add to agent mapping
    agent_layers <- rbind(agent_layers,
                          data.frame(agent_id = new_agents, layer = rep(2, length(new_agents))))
    
    # Layer 1 to Layer 2 connections (undirected)
    new_connections <- data.frame(
      from = rep(agent, n_connections),
      to = new_agents,
      layer = rep(2, n_connections) # Associate layer with the higher layer agent
    )
    
    layer2_connections <- rbind(layer2_connections, new_connections)
  }
  
  # Make some Layer 2 agents connected to multiple Layer 1 agents (up to 42%)
   unique_layer2_agents <- unique(layer2_agents)
   n_layer2 <- length(unique_layer2_agents)
  # n_multi_connected <- floor(n_layer2 * 0.18) #0.42)
  # 
  # if (n_multi_connected > 0 && n_layer1 > 1) {
  #   multi_connected_agents <- sample(unique_layer2_agents, n_multi_connected)
  #   
  #   additional_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  #   
  #   for (agent in multi_connected_agents) {
  #     # Get current Layer 1 agents connected to this Layer 2 agent
  #     current_l1 <- layer2_connections$from[layer2_connections$to == agent]
  #     # Find available Layer 1 agents not already connected
  #     available_l1 <- setdiff(layer1_agents, current_l1)
  #     
  #     if (length(available_l1) > 0) {
  #       # Connect to 1 additional Layer 1 agent (undirected)
  #       new_l1 <- sample(available_l1, 1)
  #       additional_connections <- rbind(additional_connections,
  #                                       data.frame(from = new_l1, to = agent, layer = 2)) # Associate layer with the higher layer agent
  #     }
  #   }
  #  
  #  layer2_connections <- rbind(layer2_connections, additional_connections)
  #}
  all_connections_list[[length(all_connections_list) + 1]] <- layer2_connections
  
  
  # Connect up to 50% of Layer 2 agents with each other (undirected)
  layer2_internal_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  
  if (n_layer2 > 1) {
    unique_layer2_agents <- unique(layer2_agents) # Ensure unique agents
    n_possible_pairs_l2 <- choose(n_layer2, 2) # Number of unique undirected pairs
    max_connections_l2 <- floor(n_possible_pairs_l2 * 0.36) #0.5) # 50% of possible unique undirected connections
    n_connections_l2 <- sample(0:max_connections_l2, 1)
    
    if (n_connections_l2 > 0) {
      # Get all unique undirected pairs
      potential_pairs_l2 <- combn(unique_layer2_agents, 2)
      potential_connections_l2 <- data.frame(from = potential_pairs_l2[1, ], to = potential_pairs_l2[2, ])
      
      if (nrow(potential_connections_l2) > 0) {
        # Sample connections
        selected_indices_l2 <- sample(1:nrow(potential_connections_l2),
                                      min(n_connections_l2, nrow(potential_connections_l2)))
        selected_connections_l2 <- potential_connections_l2[selected_indices_l2, ]
        
        layer2_internal_connections <- data.frame(
          from = selected_connections_l2$from,
          to = selected_connections_l2$to,
          layer = rep(2, nrow(selected_connections_l2)) # Associate layer with the connection within Layer 2
        )
      }
    }
  }
  all_connections_list[[length(all_connections_list) + 1]] <- layer2_internal_connections
  
  
  # Parameters for Layer 3
  layer3_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  layer3_agents <- c()
  
  # Each Layer 2 agent has 50% chance of connecting to 1-2 Layer 3 agents (undirected)
  unique_layer2_agents <- unique(layer2_agents) # Ensure unique agents
  for (agent in unique_layer2_agents) {
    if (runif(1) <= 0.72) {  # 50% chance
      n_connections <- sample(1:3, 1) #(1:2, 1)
      new_agents <- next_id:(next_id + n_connections - 1)
      next_id <- next_id + n_connections
      layer3_agents <- c(layer3_agents, new_agents)
      
      # Add to agent mapping
      agent_layers <- rbind(agent_layers,
                            data.frame(agent_id = new_agents, layer = rep(3, length(new_agents))))
      
      new_connections <- data.frame(
        from = rep(agent, n_connections),
        to = new_agents,
        layer = rep(3, n_connections)  # Associate layer with the higher layer agent
      )
      
      layer3_connections <- rbind(layer3_connections, new_connections)
    }
  }
  all_connections_list[[length(all_connections_list) + 1]] <- layer3_connections
  
  
  # Connect up to 20% of Layer 3 agents with each other (undirected)
  layer3_internal_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  
  unique_layer3_agents <- unique(layer3_agents)
  n_layer3 <- length(unique_layer3_agents)
  
  if (n_layer3 > 1) {
    n_possible_pairs_l3 <- choose(n_layer3, 2) # Number of unique undirected pairs
    max_connections_l3 <- floor(n_possible_pairs_l3 * 0.10)  # 20% of max possible unique undirected connections
    n_connections_l3 <- sample(0:max_connections_l3, 1)
    
    if (n_connections_l3 > 0) {
      # Create all possible unique undirected connections
      potential_pairs_l3 <- combn(unique_layer3_agents, 2)
      potential_connections_l3 <- data.frame(from = potential_pairs_l3[1, ], to = potential_pairs_l3[2, ])
      
      
      if (nrow(potential_connections_l3) > 0) {
        # Sample connections
        selected_indices_l3 <- sample(1:nrow(potential_connections_l3),
                                      min(n_connections_l3, nrow(potential_connections_l3)))
        selected_connections_l3 <- potential_connections_l3[selected_indices_l3, ]
        
        layer3_internal_connections <- data.frame(
          from = selected_connections_l3$from,
          to = selected_connections_l3$to,
          layer = rep(3, nrow(selected_connections_l3))  # Associate layer with the connection within Layer 3
        )
      }
    }
  }
  all_connections_list[[length(all_connections_list) + 1]] <- layer3_internal_connections
  
  
  # Combine all connections
  all_connections <- do.call(rbind, all_connections_list)
  
  # Standardize undirected edges and remove duplicates
  # Ensure 'from' is always the smaller ID and 'to' is the larger ID
  standardized_connections <- as.data.frame(t(apply(all_connections[, c("from", "to")], 1, sort)))
  colnames(standardized_connections) <- c("from", "to")
  
  # Keep the 'layer' attribute, assuming it's associated with the connection itself
  # If there are duplicate undirected edges with different layer values (due to the way they were generated),
  # this will keep the layer value of the first occurrence after sorting.
  # A more sophisticated approach might be needed depending on how layers should be handled in undirected duplicates.
  standardized_connections$layer <- all_connections$layer
  
  # Remove duplicate rows based on the standardized 'from' and 'to'
  unique_connections <- unique(standardized_connections)
  
  
  # Return dataframe
  return(unique_connections)
}

# Generate the network
cascade_effects_df <- create_social_network()
#layer <- cascade_effects_df$layer
# cascade_effects_df <- cascade_effects_df |>
#   select(from, to) |>
#   arrange(from, to)
# g <- graph_from_data_frame(
#   d = cascade_effects_df,
#   directed = FALSE#,
#   #vertices = layer
# )
# ggraph(g) +
#   geom_edge_link() +
#   geom_node_point(aes(color = V(g)$layer)) +
#   theme_graph(background = 'white')
plot(g)

# Save to CSV

cascade_effects_filename <- build_filename("cascade_effects")
write_csv(cascade_effects_df, cascade_effects_filename)

# Project Indicators

indicators_df <- data.frame(
  indicator = c(
    "Community Partners", "Engagement Hours", "Individuals Served",
    "Infrastructure Tools", "Output Products", "Students Involved",
    "Successful Outcomes"
  )
)
indicators_df$value <- sample(0:30, nrow(indicators_df))

indicators_filename <- build_filename("indicators")
write_csv(indicators_df, indicators_filename)
