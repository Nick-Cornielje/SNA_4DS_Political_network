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
