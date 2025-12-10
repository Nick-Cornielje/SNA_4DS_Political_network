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


# --------------------------
# H1: Seat product matrix
# --------------------------
SeatMat <- outer(seats, seats, "*")

# ---- Model 1: Seats only (H1)
model_seats <- sna::netlm(
  y = Y,
  x = list(SeatMat),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)

#model_seats <- model_seats$names <- c("Intcpt", "Seats")
# --------------------------
# H2: Ideology similarity
# --------------------------

# 1D distances
LRdist <- abs(outer(lr, lr, "-"))
PCdist <- abs(outer(pc, pc, "-"))

# 2D Euclidean distance
IdeologyDist <- sqrt(LRdist^2 + PCdist^2)

# Convert to similarity (higher = more similar)
LRsim  <- max(LRdist)  - LRdist
PCsim  <- max(PCdist)  - PCdist
IdeoSim <- max(IdeologyDist) - IdeologyDist  # full 2D similarity

# ---- Model 2: Ideology similarity only (H2)
model_ideo <- sna::netlm(
  y = Y,
  x = list(IdeoSim),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)

# --------------------------
# Control Variables
# --------------------------
AgeDist <- abs(outer(age, age, "-"))
GenderSame <- outer(gender, gender, "==") * 1



#model_ideo <- model_ideo$names <- c("Intcpt", "Ideology")
# ------------------------------------------------------
# Compare models
# ------------------------------------------------------

cat("\n===== MRQAP: Effect of Seats (H1) =====\n")
print(summary(model_seats))

cat("\n===== MRQAP: Effect of Ideological Similarity (H2) =====\n")
print(summary(model_ideo))

# Optional: Combined model (if you want later)
model_both <- sna::netlm(
  y = Y,
  x = list(SeatMat, IdeoSim),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)
model_both$names <- c("Intcpt", "Seats", "Ideology")
cat("\n===== MRQAP: Combined Model (Seats + Ideology) =====\n")
print(summary(model_both))

model_control <- sna::netlm(
  y = Y,
  x = list(SeatMat, IdeoSim, AgeDist, GenderSame),
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qapspp"
)
model_control$names <- c("Intcpt", "Seats", "Ideology", "Age", "Gender")
cat("\n===== MRQAP: Combined Model with control (Seats + Ideology + Age + Gender) =====\n")
summary(model_control)
texreg::screenreg(list(model_seats, model_ideo, model_both, model_control))

## Extra model -- >Log model
## 

Ylog <- log(1 + Y)
SeatNorm <- SeatMat / max(SeatMat)
alpha <- 1 / sd(as.vector(IdeologyDist))
IdeoExp <- exp(-alpha * IdeologyDist)

# Add a weight which weig
deg <- rowSums(Y)   # weighted node strength
DegMat <- outer(deg, deg, "*")
DegNorm <- DegMat / max(DegMat)


model_control <- sna::netlm(
  y = Ylog,
  x = list(DegNorm, SeatNorm, IdeoExp),  # or DegNorm instead of PopNorm
  intercept = TRUE,
  mode = "undirected",
  nullhyp = "qap"
)
summary(model_control)


