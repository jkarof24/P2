library(rgl)

# Genererer tilfælde tal
n <- 2500                           # Antal værdier
set.seed(42)                        # Sætter seed
mt_vals <- runif(n)                 # Bruger default mersenne twister til at generere

par(mgp = c(1.5, 0.4, 0))  # Axis label closer to axis
par(mar = c(3, 3, 2, 1))   # Optional: tighter outer margins (bottom, left, top, right)

#2d plot
plot(
  mt_vals[1:(n - 1)],
  mt_vals[2:n],
  xlab = "X[n]", ylab = "X[n+1]",
  pch = 16, 
  cex = 0.4,
  col = "black",
  asp = 1,
  axes = FALSE
)

box()


#opsætning til 3d-plot
x <- mt_vals[1:(n - 2)]
y <- mt_vals[2:(n - 1)]
z <- mt_vals[3:n]

#3d plot
plot3d(
  x, y, z,
  col = "black", 
  size = 3,
  xlab = "X[n]", ylab = "X[n+1]", zlab = "X[n+2]",
)

#ændrer synsvinkel
rgl.viewpoint(theta = 40, phi = 10, fov = 60, zoom = 1)

