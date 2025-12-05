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
  net_bip ~ edges + absdiff("LeftRight") + absdiff("ProgCons"),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 5000,
    seed            = 123
  )
)

m4 <- ergm::ergm(
  net_bip ~ edges +
    absdiff("LeftRight") + 
    absdiff("ProgCons") +
    nodecov("Seats") + 
    b2star(2),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 5000,
    seed            = 123
  )
)


m5 <- ergm::ergm(
  net_bip ~ 
    edges +
    absdiff("LeftRight") + 
    absdiff("ProgCons") +
    nodecov("Seats") +
    b2degree(d=1:4),
  control = ergm::control.ergm(
    MCMC.burnin     = 20000,
    MCMC.interval   = 1000,
    MCMC.samplesize = 50000,
    MCMLE.maxit = 20,
    seed            = 42
  )
)

texreg::screenreg(list(m1, m2, m3, m4, m5))

ergm::search.ergmTerms()


ergm::mcmc.diagnostics(m4)
ergm::mcmc.diagnostics(m5)

plot(ergm::gof(m1))
plot(ergm::gof(m2))
plot(ergm::gof(m3))
plot(ergm::gof(m4))
plot(ergm::gof(m5))

## Print summaries

summary(m1)
summary(m2)
summary(m3)
summary(m4)


