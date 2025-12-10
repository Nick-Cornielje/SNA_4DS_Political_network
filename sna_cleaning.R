# ======================================================
# STEP 1: BUILD & PLOT OUTLET–POLITICIAN BIPARTITE NETWORK
# ======================================================

# --- Read data and extract relevant columns ---
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

# --- Parse politicians list ---
df <- df[!is.na(df$politicians_depicted) & trimws(df$politicians_depicted) != "[]", ]

parse_list <- function(x) {
  lapply(x, function(s) {
    s <- gsub("\\[|\\]|'", "", s)
    parts <- unlist(strsplit(s, "[,;|]"))
    trimws(parts[nzchar(parts)])
  })
}
df$politicians_depicted <- parse_list(df$politicians_depicted)

# --- Create weighted edge list ---
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

# --- Create bipartite network ---
net <- snafun::to_network(edges, bipartite = TRUE)


# --- Clean network ---
net <- snafun::remove_loops(net)
net <- snafun::remove_isolates(net)

# --- Add vertex attributes (outlet vs politician) ---
n_outlets <- length(unique(edges$from))
n_total   <- network::network.size(net)
n_pols    <- n_total - n_outlets

snafun::add_vertex_attributes(
  net, "mode",
  value = c(rep("outlet", n_outlets), rep("politician", n_pols))
)

network::list.vertex.attributes(net)
# --- Save the network ---
save(net, file="bipartite_net.Rdata")

# --- Create edge weights for plot ---
ew <- network::get.edge.attribute(net, "weight")
ew_scaled <- (ew / max(ew)) * 8
network::set.edge.attribute(net, "weight", ew_scaled)

# --- Plot bipartite graph ---
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

# ======================================================
# POLITICIAN–POLITICIAN PROJECTION (SAME ARTICLE)
# ======================================================

# --- Build article–politician incidence ---
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

# --- Projection: co-appearance in the same article ---
PP <- P %*% t(P)      # weighted co-occurrence
diag(PP) <- 0

# --- Build igraph projection (undirected, weighted) ---
pol_proj <- igraph::graph_from_adjacency_matrix(
  PP, mode = "undirected", weighted = TRUE, diag = FALSE
)



# Edge widths for plotting
ew <- igraph::E(pol_proj)$weight
ew_plot <- (ew / max(ew)) * 10

# ======================================================
# STEP 2B: LOAD IDEOLOGY + SEATS & ADD ATTRIBUTES
# ======================================================

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

# ======================================================
# STEP 2C: ADD ATTRIBUTES TO THE PROJECTION NETWORK
# ======================================================

# Add vertex attributes for future calculations
igraph::V(pol_proj)$LeftRight               <- ideol_sub$LeftRight
igraph::V(pol_proj)$ProgressiveConservative <- ideol_sub$ProgressiveConservative
igraph::V(pol_proj)$Seats                   <- ideol_sub$Seats
igraph::V(pol_proj)$Age                     <- ideol_sub$Age
igraph::V(pol_proj)$Gender                     <- ideol_sub$Gender

# ======================================================
# STEP 2D: GENERATE IDEOLOGICAL COORDINATES FOR PLOTTING
# ======================================================

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

# ======================================================
# STEP 2E: SAVE THE UPDATED PROJECTION NETWORK
# ======================================================

save(pol_proj, file = "political_projection_with_attributes.Rdata")
igraph::vertex_attr_names(pol_proj)

## New thing:
## 

# ======================================================
# STEP 1: BUILD & PLOT OUTLET–POLITICIAN BIPARTITE NETWORK
# ======================================================

# --- Read data and extract relevant columns ---
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

# --- Parse politicians list ---
df <- df[!is.na(df$politicians_depicted) & trimws(df$politicians_depicted) != "[]", ]

parse_list <- function(x) {
  lapply(x, function(s) {
    s <- gsub("\\[|\\]|'", "", s)
    parts <- unlist(strsplit(s, "[,;|]"))
    trimws(parts[nzchar(parts)])
  })
}
df$politicians_depicted <- parse_list(df$politicians_depicted)

# --- Create weighted edge list ---
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

# --- Create bipartite network ---
net <- snafun::to_network(edges, bipartite = TRUE)


# --- Clean network ---
net <- snafun::remove_loops(net)
net <- snafun::remove_isolates(net)

# --- Add vertex attributes (outlet vs politician) ---
n_outlets <- length(unique(edges$from))
n_total   <- network::network.size(net)
n_pols    <- n_total - n_outlets

snafun::add_vertex_attributes(
  net, "mode",
  value = c(rep("outlet", n_outlets), rep("politician", n_pols))
)

network::list.vertex.attributes(net)
# --- Save the network ---
save(net, file="bipartite_net.Rdata")

# --- Create edge weights for plot ---
ew <- network::get.edge.attribute(net, "weight")
ew_scaled <- (ew / max(ew)) * 8
network::set.edge.attribute(net, "weight", ew_scaled)

# --- Plot bipartite graph ---
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

# ======================================================
# POLITICIAN–POLITICIAN PROJECTION (SAME ARTICLE)
# ======================================================

# --- Build article–politician incidence ---
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

# --- Projection: co-appearance in the same article ---
PP <- P %*% t(P)      # weighted co-occurrence
diag(PP) <- 0

# --- Build igraph projection (undirected, weighted) ---
pol_proj <- igraph::graph_from_adjacency_matrix(
  PP, mode = "undirected", weighted = TRUE, diag = FALSE
)



# Edge widths for plotting
ew <- igraph::E(pol_proj)$weight
ew_plot <- (ew / max(ew)) * 10

# ======================================================
# STEP 2B: LOAD IDEOLOGY + SEATS & ADD ATTRIBUTES
# ======================================================

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

# ======================================================
# STEP 2C: ADD ATTRIBUTES TO THE PROJECTION NETWORK
# ======================================================

# Add vertex attributes for future calculations
igraph::V(pol_proj)$LeftRight               <- ideol_sub$LeftRight
igraph::V(pol_proj)$ProgressiveConservative <- ideol_sub$ProgressiveConservative
igraph::V(pol_proj)$Seats                   <- ideol_sub$Seats
igraph::V(pol_proj)$Age                     <- ideol_sub$Age
igraph::V(pol_proj)$Gender                  <- ideol_sub$Gender

# ======================================================
# STEP 2D: GENERATE IDEOLOGICAL COORDINATES FOR PLOTTING
# ======================================================

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

# ======================================================
# STEP 2E: SAVE THE UPDATED PROJECTION NETWORK
# ======================================================

save(pol_proj, file = "political_projection_with_attributes.Rdata")
igraph::vertex_attr_names(pol_proj)




