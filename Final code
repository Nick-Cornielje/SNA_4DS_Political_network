# CLEANING CODE

# Read data and extract relevant columns 
path <- "news_clf_title_preprocessed_validated.xlsx"
raw  <- readxl::read_excel(path)

norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
wanted <- c("outlet","id","article_url","date","title","politicians_depicted","Issues_matches")

find_col <- function(target) {
  i <- which(norm(names(raw)) == norm(target))
  if (length(i)) i[1] else NA_integer_
}

sel_idx <- sapply(wanted, find_col)
if (any(is.na(sel_idx))) stop("Missing columns: ", paste(wanted[is.na(sel_idx)], collapse = ", "))

df <- raw[, sel_idx]
names(df) <- wanted

# Parse politicians list 
df <- df[!is.na(df$politicians_depicted) & trimws(df$politicians_depicted) != "[]", ]

parse_list <- function(x) {
  lapply(x, function(s) {
    s <- gsub("\\[|\\]|'", "", s)
    parts <- unlist(strsplit(s, "[,;|]"))
    trimws(parts[nzchar(parts)])
  })
}
df$politicians_depicted <- parse_list(df$politicians_depicted)

# Create weighted edge list 
edges <- data.frame(
  from = rep(df$outlet, sapply(df$politicians_depicted, length)),
  to   = unlist(df$politicians_depicted),
  stringsAsFactors = FALSE
)
edges <- edges[edges$to != "", ]

# Aggregate duplicates into edge weights
edges <- aggregate(list(weight = rep(1, nrow(edges))),
                   by = list(from = edges$from, to = edges$to),
                   FUN = sum)

# Create bipartite network 
net <- snafun::to_network(edges, bipartite = TRUE)


# Clean network 
net <- snafun::remove_loops(net)
net <- snafun::remove_isolates(net)

# Add vertex attributes (outlet vs politician)
n_outlets <- length(unique(edges$from))
n_total   <- network::network.size(net)
n_pols    <- n_total - n_outlets

snafun::add_vertex_attributes(
  net, "mode",
  value = c(rep("outlet", n_outlets), rep("politician", n_pols))
)

network::list.vertex.attributes(net)
# Save the network
save(net, file="bipartite_net.Rdata")

# Create edge weights for plot 
ew <- network::get.edge.attribute(net, "weight")
ew_scaled <- (ew / max(ew)) * 8
network::set.edge.attribute(net, "weight", ew_scaled)

# Plot bipartite graph 
edge_colors <- gray(1 - ew / max(ew))  # darker = higher weight

network::plot.network(
  net,
  edge.lwd = ew_scaled,
  edge.col = edge_colors,
  vertex.col = "skyblue",
  vertex.border = "darkblue",
  vertex.cex = 3,
  displaylabels = TRUE,
  label.cex = 0.7,
  label.col = "black",
  main = "Outlet–Politician Bipartite Network"
)

# Quick network stats
cat("Is bipartite:", snafun::is_bipartite(net), "\n")
cat("Vertices:", snafun::count_vertices(net), "\n")
cat("Edges:", snafun::count_edges(net), "\n\n")


# Build article–politician incidence 
articles <- as.character(df$id)
pols     <- sort(unique(unlist(df$politicians_depicted)))

P <- matrix(0, nrow = length(pols), ncol = length(articles),
            dimnames = list(pols, articles))

for (i in seq_along(df$politicians_depicted)) {
  ps <- df$politicians_depicted[[i]]
  if (length(ps)) {
    P[ps, as.character(df$id[i])] <- 1
  }
}

# Projection: co-appearance in the same article
PP <- P %*% t(P)      # weighted co-occurrence
diag(PP) <- 0

# Build igraph projection (undirected, weighted)
pol_proj <- igraph::graph_from_adjacency_matrix(
  PP, mode = "undirected", weighted = TRUE, diag = FALSE
)



# Edge widths for plotting
ew <- igraph::E(pol_proj)$weight
ew_plot <- (ew / max(ew)) * 10

# Load ideology file (must contain Person, Seats, ProgressiveConservative, LeftRight)
ideol <- utils::read.csv("Political_Orientation_Data.csv", stringsAsFactors = FALSE)

# Required columns
need <- c("Person","Seats","ProgressiveConservative","LeftRight", "Age", "Gender")
miss <- setdiff(need, names(ideol))
if (length(miss)) stop("Missing columns in Political_Orientation_Data.csv: ", paste(miss, collapse=", "))

# Keep only politicians that exist in the projection network
vnames <- igraph::V(pol_proj)$name
ideol_sub <- ideol[ideol$Person %in% vnames, need]

# Remove missing ideological coordinates
ideol_sub <- ideol_sub[stats::complete.cases(
  ideol_sub$LeftRight,
  ideol_sub$ProgressiveConservative
), ]

# Induce subgraph on matching politicians
pol_proj <- igraph::induced_subgraph(
  pol_proj,
  vids = igraph::V(pol_proj)[name %in% ideol_sub$Person]
)

# Reorder ideology rows to match current vertex order
ideol_sub <- ideol_sub[match(igraph::V(pol_proj)$name, ideol_sub$Person), ]



# Add vertex attributes for future calculations
igraph::V(pol_proj)$LeftRight               <- ideol_sub$LeftRight
igraph::V(pol_proj)$ProgressiveConservative <- ideol_sub$ProgressiveConservative
igraph::V(pol_proj)$Seats                   <- ideol_sub$Seats
igraph::V(pol_proj)$Age                     <- ideol_sub$Age
igraph::V(pol_proj)$Gender                     <- ideol_sub$Gender


coords <- cbind(
  ideol_sub$LeftRight,
  ideol_sub$ProgressiveConservative
)

# Validate coordinates
if (any(!is.finite(coords))) {
  stop("Some ideology coordinates are non-finite. Check LeftRight / ProgressiveConservative values.")
}
if (all(apply(coords, 2, var) == 0)) {
  stop("Ideology coordinates have zero variance and cannot form a 2D layout.")
}

# Normalize & jitter to avoid overlapping labels
coords <- scale(coords) * 5
set.seed(11)
coords <- coords + matrix(stats::rnorm(length(coords), sd = 0.02), ncol = 2)

# Store coordinates as a vertex attribute (optional but useful)
igraph::V(pol_proj)$layout_x <- coords[,1]
igraph::V(pol_proj)$layout_y <- coords[,2]

save(pol_proj, file = "political_projection_with_attributes.Rdata")
igraph::vertex_attr_names(pol_proj)


# Read data and extract relevant columns 
path <- "news_clf_title_preprocessed_validated.xlsx"
raw  <- readxl::read_excel(path)

norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
wanted <- c("outlet","id","article_url","date","title","politicians_depicted","Issues_matches")

find_col <- function(target) {
  i <- which(norm(names(raw)) == norm(target))
  if (length(i)) i[1] else NA_integer_
}

sel_idx <- sapply(wanted, find_col)
if (any(is.na(sel_idx))) stop("Missing columns: ", paste(wanted[is.na(sel_idx)], collapse = ", "))

df <- raw[, sel_idx]
names(df) <- wanted

# Parse politicians list
df <- df[!is.na(df$politicians_depicted) & trimws(df$politicians_depicted) != "[]", ]

parse_list <- function(x) {
  lapply(x, function(s) {
    s <- gsub("\\[|\\]|'", "", s)
    parts <- unlist(strsplit(s, "[,;|]"))
    trimws(parts[nzchar(parts)])
  })
}
df$politicians_depicted <- parse_list(df$politicians_depicted)

# Create weighted edge list 
edges <- data.frame(
  from = rep(df$outlet, sapply(df$politicians_depicted, length)),
  to   = unlist(df$politicians_depicted),
  stringsAsFactors = FALSE
)
edges <- edges[edges$to != "", ]

# Aggregate duplicates into edge weights
edges <- aggregate(list(weight = rep(1, nrow(edges))),
                   by = list(from = edges$from, to = edges$to),
                   FUN = sum)

# Create bipartite network 
net <- snafun::to_network(edges, bipartite = TRUE)


# Clean network 
net <- snafun::remove_loops(net)
net <- snafun::remove_isolates(net)

# Add vertex attributes (outlet vs politician) 
n_outlets <- length(unique(edges$from))
n_total   <- network::network.size(net)
n_pols    <- n_total - n_outlets

snafun::add_vertex_attributes(
  net, "mode",
  value = c(rep("outlet", n_outlets), rep("politician", n_pols))
)

network::list.vertex.attributes(net)
# Save the network 
save(net, file="bipartite_net.Rdata")

#  Create edge weights for plot 
ew <- network::get.edge.attribute(net, "weight")
ew_scaled <- (ew / max(ew)) * 8
network::set.edge.attribute(net, "weight", ew_scaled)

# Plot bipartite graph 
edge_colors <- gray(1 - ew / max(ew))  # darker = higher weight

network::plot.network(
  net,
  edge.lwd = ew_scaled,
  edge.col = edge_colors,
  vertex.col = "skyblue",
  vertex.border = "darkblue",
  vertex.cex = 3,
  displaylabels = TRUE,
  label.cex = 0.7,
  label.col = "black",
  main = "Outlet–Politician Bipartite Network"
)

# Quick network stats
cat("Is bipartite:", snafun::is_bipartite(net), "\n")
cat("Vertices:", snafun::count_vertices(net), "\n")
cat("Edges:", snafun::count_edges(net), "\n\n")

# Build article–politician incidence 
articles <- as.character(df$id)
pols     <- sort(unique(unlist(df$politicians_depicted)))

P <- matrix(0, nrow = length(pols), ncol = length(articles),
            dimnames = list(pols, articles))

for (i in seq_along(df$politicians_depicted)) {
  ps <- df$politicians_depicted[[i]]
  if (length(ps)) {
    P[ps, as.character(df$id[i])] <- 1
  }
}

# Projection: co-appearance in the same article 
PP <- P %*% t(P)      # weighted co-occurrence
diag(PP) <- 0

# Build igraph projection (undirected, weighted) 
pol_proj <- igraph::graph_from_adjacency_matrix(
  PP, mode = "undirected", weighted = TRUE, diag = FALSE
)



# Edge widths for plotting
ew <- igraph::E(pol_proj)$weight
ew_plot <- (ew / max(ew)) * 10


# Load ideology file (must contain Person, Seats, ProgressiveConservative, LeftRight)
ideol <- utils::read.csv("Political_Orientation_Data.csv", stringsAsFactors = FALSE)

# Required columns
need <- c("Person","Seats","ProgressiveConservative","LeftRight","Age","Gender")
miss <- setdiff(need, names(ideol))
if (length(miss)) stop("Missing columns in Political_Orientation_Data.csv: ", paste(miss, collapse=", "))

# Keep only politicians that exist in the projection network
vnames <- igraph::V(pol_proj)$name
ideol_sub <- ideol[ideol$Person %in% vnames, need]

# Remove missing ideological coordinates
ideol_sub <- ideol_sub[stats::complete.cases(
  ideol_sub$LeftRight,
  ideol_sub$ProgressiveConservative
), ]

# Induce subgraph on matching politicians
pol_proj <- igraph::induced_subgraph(
  pol_proj,
  vids = igraph::V(pol_proj)[name %in% ideol_sub$Person]
)

# Reorder ideology rows to match current vertex order
ideol_sub <- ideol_sub[match(igraph::V(pol_proj)$name, ideol_sub$Person), ]

# Add vertex attributes for future calculations
igraph::V(pol_proj)$LeftRight               <- ideol_sub$LeftRight
igraph::V(pol_proj)$ProgressiveConservative <- ideol_sub$ProgressiveConservative
igraph::V(pol_proj)$Seats                   <- ideol_sub$Seats
igraph::V(pol_proj)$Age                     <- ideol_sub$Age
igraph::V(pol_proj)$Gender                  <- ideol_sub$Gender

coords <- cbind(
  ideol_sub$LeftRight,
  ideol_sub$ProgressiveConservative
)

# Validate coordinates
if (any(!is.finite(coords))) {
  stop("Some ideology coordinates are non-finite. Check LeftRight / ProgressiveConservative values.")
}
if (all(apply(coords, 2, var) == 0)) {
  stop("Ideology coordinates have zero variance and cannot form a 2D layout.")
}

# Normalize & jitter to avoid overlapping labels
coords <- scale(coords) * 5
set.seed(11)
coords <- coords + matrix(stats::rnorm(length(coords), sd = 0.02), ncol = 2)

# Store coordinates as a vertex attribute (optional but useful)
igraph::V(pol_proj)$layout_x <- coords[,1]
igraph::V(pol_proj)$layout_y <- coords[,2]

save(pol_proj, file = "political_projection_with_attributes.Rdata")
igraph::vertex_attr_names(pol_proj)


# RQ1 CODE
## MRQAP models
## Loading the projection 

load("political_projection_with_attributes.Rdata")

# Weighted co-appearance matrix from the projection
Y <- igraph::as_adjacency_matrix(pol_proj, attr = "weight", sparse = FALSE)

Y
# Ensure numeric matrix
Y <- as.matrix(Y)

# Vertex attributes 
seats <- igraph::V(pol_proj)$Seats
lr    <- igraph::V(pol_proj)$LeftRight
pc    <- igraph::V(pol_proj)$ProgressiveConservative
age   <- igraph::V(pol_proj)$Age
gender <- igraph::V(pol_proj)$Gender



# H1: Seat product matrix
SeatMat <- outer(seats, seats, "*")

#  Model 1: Seats only (H1)
model_seats <- sna::netlm(
  y = Y,
  x = list(SeatMat),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)

#model_seats <- model_seats$names <- c("Intcpt", "Seats")

# H2: Ideology similarity


# 1D distances
LRdist <- abs(outer(lr, lr, "-"))
PCdist <- abs(outer(pc, pc, "-"))

# 2D Euclidean distance
IdeologyDist <- sqrt(LRdist^2 + PCdist^2)

# Convert to similarity (higher = more similar)
LRsim  <- max(LRdist)  - LRdist
PCsim  <- max(PCdist)  - PCdist
IdeoSim <- max(IdeologyDist) - IdeologyDist  # full 2D similarity

# Model 2: Ideology similarity only (H2)
model_ideo <- sna::netlm(
  y = Y,
  x = list(IdeoSim),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)


# Control Variables
AgeDist <- abs(outer(age, age, "-"))
GenderSame <- outer(gender, gender, "==") * 1



#model_ideo <- model_ideo$names <- c("Intcpt", "Ideology")

# Compare models


print(summary(model_seats))


print(summary(model_ideo))

# Optional: Combined model 
model_both <- sna::netlm(
  y = Y,
  x = list(SeatMat, IdeoSim),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)
model_both$names <- c("Intcpt", "Seats", "Ideology")

print(summary(model_both))

model_control <- sna::netlm(
  y = Y,
  x = list(SeatMat, IdeoSim, AgeDist, GenderSame),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)
model_control$names <- c("Intcpt", "Seats", "Ideology", "Age", "Gender")




summary(model_control)

# PREP BIPARTITE

## Create Dataset

path <- "news_clf_title_preprocessed_validated.xlsx"
raw  <- readxl::read_excel(path)

norm_name <- function(x) tolower(gsub("[^a-z0-9]", "", x))
wanted    <- c("outlet","id","article_url","date","title",
               "politicians_depicted","Issues_matches")

find_col <- function(target) {
  i <- which(norm_name(names(raw)) == norm_name(target))
  if (length(i)) i[1] else NA_integer_
}

sel_idx <- sapply(wanted, find_col)
if (any(is.na(sel_idx))) stop("Missing columns in Excel: ", paste(wanted[is.na(sel_idx)], collapse=", "))

df <- raw[, sel_idx]
names(df) <- wanted

## Drop rows with empty politician lists
df <- df[!is.na(df$politicians_depicted) & trimws(df$politicians_depicted) != "[]", ]

# Create list of politicians

parse_list <- function(x) {
  lapply(x, function(s) {
    s <- gsub("\\[|\\]|'", "", s)
    parts <- unlist(strsplit(s, "[,;|]"))
    trimws(parts[nzchar(parts)])
  })
}
df$politicians_depicted <- parse_list(df$politicians_depicted)

## Create the weighted edge list

edge_list <- data.frame(
  from   = rep(df$outlet, sapply(df$politicians_depicted, length)),
  to     = unlist(df$politicians_depicted),
  weight = 1,
  stringsAsFactors = FALSE
)
edge_list <- edge_list[edge_list$to != "", ]

## Aggregate duplicates → edge weights = appearance counts
edge_list <- aggregate(
  list(weight = edge_list$weight),
  by = list(from  = edge_list$from,
            to    = edge_list$to),
  FUN = sum
)

## Create plot for given edge weights
## 
# Extract edge weights
w <- edge_list$weight

# Define all unique thresholds
x_vals <- sort(unique(w))

# For each x, count edges with weight >= x
y_counts <- sapply(x_vals, function(x) sum(w >= x))

# Compute statistics
w_mean  <- mean(w)
w_med   <- median(w)
w_q25   <- quantile(w, 0.25)
w_q75   <- quantile(w, 0.75)

# Plot
plot(
  x_vals, y_counts,
  type = "b",
  xlab = "Minimum Edge Weight (x)",
  ylab = "Count of Edges with Weight ≥ x",
  main = "Edge Count for Minimum Weight Threshold"
)

# Add statistical vertical lines
abline(v = w_mean, col = "red", lwd = 2, lty = 2)
abline(v = w_med,  col = "blue", lwd = 2, lty = 2)
abline(v = w_q25,  col = "darkgreen", lwd = 2, lty = 3)
abline(v = w_q75,  col = "purple", lwd = 2, lty = 3)

# Legend
legend(
  "topright",
  legend = c(
    paste0("Mean = ", round(w_mean, 2)),
    paste0("Median = ", round(w_med, 2)),
    paste0("25th % = ", round(w_q25, 2)),
    paste0("75th % = ", round(w_q75, 2))
  ),
  col = c("red","blue","darkgreen","purple"),
  lwd = 2,
  lty = c(2,2,3,3),
  bty = "n"
)

## Create cutoff point for Bipartite graph
## By changing cutoff value the cutoff for minimum weight is changed
## So cutofff <- 8 means that an edge is created if and only if there are at least
## 8 images of that politician in the associated news paper 

cutoff <- 12
edge_list$binary <- ifelse(edge_list$weight >= cutoff, 1L, 0L)
edge_list
# Check if it works
cat("Before cutoff sum weights:", sum(edge_list$weight), "\n")
cat("After binary edge count (≥10):", sum(edge_list$binary), "\n")


outlets <- sort(unique(edge_list$from))
pols    <- sort(unique(edge_list$to))

B <- matrix(0L, nrow = length(outlets), ncol = length(pols), dimnames = list(outlets, pols))

for (i in seq_len(nrow(edge_list))) {
  if (edge_list$binary[i] == 1L) {
    B[ edge_list$from[i], edge_list$to[i] ] <- 1L
  }
}
print(edge_list)

## Create the bipartite graph here

# Mode1 = outlets, Mode2 = politicians
net_bip <- network::network(
  B,
  matrix.type = "bipartite",
  directed    = FALSE,
  bipartite   = nrow(B)
)

snafun::g_density(net_bip)

# Store names in seperate list
vertex_names <- c(rownames(B), colnames(B))
network::set.vertex.attribute(net_bip, "vertex.names", vertex_names)

# Set mode labels as vertex attributes
mode_attr <- c(rep("outlet", nrow(B)), rep("politician", ncol(B)))
network::set.vertex.attribute(net_bip, "mode", mode_attr)

## Add attributes from the political orientation data csv
## This contains the left right value, progressive conservative value
## and the amount of seats attained in the 2021 elections

attr_tbl <- utils::read.csv("Political_Orientation_Data.csv", stringsAsFactors = FALSE)

need <- c("Person","Seats","ProgressiveConservative","LeftRight")
miss <- setdiff(need, names(attr_tbl))
if (length(miss)) stop("Missing columns in attribute table: ", paste(miss, collapse=", "))

# Match politicians to the rows in the file
vnames <- network::get.vertex.attribute(net_bip, "vertex.names")
match_idx <- match(vnames, attr_tbl$Person)

seats <- ifelse(!is.na(match_idx), attr_tbl$Seats[match_idx], 0)
lr    <- ifelse(!is.na(match_idx), attr_tbl$LeftRight[match_idx], 0)
pc    <- ifelse(!is.na(match_idx), attr_tbl$ProgressiveConservative[match_idx], 0)

# Set them as vertex attributes to the bipartite
network::set.vertex.attribute(net_bip, "Seats",    seats)
network::set.vertex.attribute(net_bip, "LeftRight", lr)
network::set.vertex.attribute(net_bip, "ProgCons",  pc)

# Check the plot for amount of vertices connected
plot(net_bip)

## Save the network after cleaning

save(net_bip, file="bipartite_net_clean.Rdata")

# RQ2

#Load in the bipartite from prep_bipartite

load("bipartite_net_clean.Rdata")  # object = net_bip

# All checks to see if loading went correctly
cat("Directed? ", network::is.directed(net_bip), "\n")
cat("Nodes:    ", network::network.size(net_bip), "\n")
cat("Mode1 ct:  ", network::get.network.attribute(net_bip, "bipartite"), "\n")
cat("Vertex at: ", paste(network::list.vertex.attributes(net_bip), collapse=", "), "\n")
cat("Edge at:   ", paste(network::list.edge.attributes(net_bip),   collapse=", "), "\n\n")

## Fit ERGMS to the bipartite

m1 <- ergm::ergm(
  net_bip ~ edges,
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 5000,
    seed            = 123
  )
)

m2 <- ergm::ergm(
  net_bip ~ edges + nodecov("Seats"),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 5000,
    seed            = 123
  )
)

m3 <- ergm::ergm(
  net_bip ~ edges + nodecov("LeftRight") + nodecov("ProgCons"),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 5000,
    seed            = 123
  )
)

m4 <- ergm::ergm(
  net_bip ~ edges +
    nodecov("LeftRight") + 
    nodecov("ProgCons") +
    nodecov("Seats") + 
    b2star(2),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 5000,
    parallel = 6,
    seed            = 123
  )
)


m5 <- ergm::ergm(
  net_bip ~ 
    edges +
    nodecov("LeftRight") + 
    nodecov("ProgCons") +
    nodecov("Seats") +
    b1star(2),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 50000,
    MCMLE.maxit = 20,
    parallel = 6,
    seed            = 135
  )
)



m6 <- ergm::ergm(
  net_bip ~ 
    edges +
    nodecov("LeftRight") + 
    nodecov("ProgCons") +
    nodecov("Seats") +
    b2star(2) +
    b1star(2),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 50000,
    MCMLE.maxit = 20,
    parallel = 6,
    seed            = 124
  )
)

texreg::screenreg(list(m1, m2, m3, m4, m5, m6))

ergm::search.ergmTerms()


ergm::mcmc.diagnostics(m4)
ergm::mcmc.diagnostics(m5)
ergm::mcmc.diagnostics(m6)


plot(ergm::gof(m1))
plot(ergm::gof(m2))
plot(ergm::gof(m3))
plot(ergm::gof(m4))
plot(ergm::gof(m5))
plot(ergm::gof(m6))





