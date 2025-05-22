library(rgl)

# ---- Step 1: Generate random numbers from Mersenne Twister ----
n <- 400                            # How many values to generate
set.seed(42)                        # Set seed for reproducibility
mt_vals <- runif(n)                 # Uses Mersenne Twister by default

par(mgp = c(1.5, 0.4, 0))  # Axis label closer to axis
par(mar = c(3, 3, 2, 1))   # Optional: tighter outer margins (bottom, left, top, right)

# ---- Step 2: 2D Spectral Test Plot ----
# Plot (X[n], X[n+1])
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


# ---- Step 3: 3D Spectral Test Plot ----
x <- mt_vals[1:(n - 2)]
y <- mt_vals[2:(n - 1)]
z <- mt_vals[3:n]

plot3d(
  x, y, z,
  col = "black", 
  size = 3,
  xlab = "X[n]", ylab = "X[n+1]", zlab = "X[n+2]",
)

rgl.viewpoint(theta = 40, phi = 10, fov = 60, zoom = 1)

rgl.snapshot("C:/Users/YourName/Documents/GitHub/P2/latex/Latex fixet/billder")
