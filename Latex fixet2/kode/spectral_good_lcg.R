library(rgl)

#Laver funktion og assigner værdier
generate_lcg <- function(n, seed = 21) {
  a <- 3141592621     #Multiplier
  c <- 1    #Increment
  m <- 2^32     #Modulus
  
  x <- numeric(n)     # variable til de tilfældige tal
  x[1] <- seed        # seed
  
  #Generate the sequence using the recurrence relation
  for (i in 2:n) {
    x[i] <- (a * x[i - 1] + c) %% m  # Apply LCG formula and wrap with modulus
  }
  
  return(x / m)       #normalisere til interval [0, 1)
}

#Generering
n <- 2500                          #Antal værdier
random_numbers <- generate_lcg(n) #laver sekvensen

#2D plottet
#plotter nummer-par: X[n] vs X[n+1]
plot(
  random_numbers[1:(n - 1)],      # X[n]
  random_numbers[2:n],            # X[n+1]
  xlab = "x[n]",                  # X-axis label
  ylab = "X[n+1]",                # Y-axis label
  pch = 16, 
  cex = 0.4,
  col = "black",
  asp = 1,
  axes = FALSE
)

box()

#3D plottet
#Laver 3 akser : X[n], X[n+1], X[n+2]
x <- random_numbers[1:(n - 2)]    # X[n]
y <- random_numbers[2:(n - 1)]    # X[n+1]
z <- random_numbers[3:n]          # X[n+2]

#lidt mere plot
plot3d(
  x, y, z,                       #Koordinater
  col = "black",                 #farve
  size = 3,                      #størrelse på prikker
  xlab = "X[n]",                 #x-label
  ylab = "X[n+1]",               #y-label
  zlab = "X[n+2]",               #z-label
)

#skifter vinklen
rgl.viewpoint(theta = 40, phi = 10, fov = 60, zoom = 1)
#gemmer et billede af plottet
rgl.snapshot("C:/Users/YourName/Documents/GitHub/P2/latex/Latex fixet/billder")
