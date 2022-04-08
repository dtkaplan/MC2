# Make a movie of gradient descent in configuration space.

grad_descent <- function(objf, start, stepsize, nstep=10, ...) {
  startval <- objf(start)
  where <- matrix(c(start, startval),
                  ncol=length(start)+1, nrow=nstep+1,
                  byrow=TRUE)
  for (k in 1:nstep) {
    output <- objf(start)
    direction <- numDeriv::grad(objf, start)
    direction <- direction/sqrt(sum(direction^2))
    start <- start - stepsize*direction
    where[k+1,] <- c(start, output)
  }

  where
}

vector_SM_3 <- function(x) {
    L01sq <- x[1]^2 + x[2]^2 - 0.5
    L12sq <- (x[3]-x[1])^2 + (x[4]-x[2])^2 - 0.5
    L23sq <- (x[5]-x[3])^2 + (x[6]-x[4])^2 - 0.5
    L34sq <- (x[5] - 3.2)^2 + (x[6] + 1.1)^2 - 0.5
    gravity <- sum(x[c(2,4,6)])

    0.8*(L01sq + L12sq + L23sq + L34sq) + 2*gravity
}

potential_slice <- function(X, n) {
  pos <- X[n,]
  fun <- function(y1, y2) {
    res <- y1
    for (k in 1:length(y1)) {
      pos[c(4,6)] <- c(y1[k], y2[k])
      res[k] <- vector_SM_3(pos)
    }

    res
  }
  ypos <-tibble(y1 = X[1:n, 4], y2 = X[1:n, 6])
  gradient_plot(fun(y1, y2) ~ y1 & y2, domain(y1=-3.5:0, y2=-3.5:0),
                npts=12) %>%
    gf_refine(coord_fixed()) %>%
    gf_path(y2 ~ y1, data = ypos, color="red", inherit=FALSE)

}

potential_slice2 <- function(X, n) {
  pos <- X[n,]
  fun <- function(x1, y1) {
    res <- y1
    for (k in 1:length(x1)) {
      pos[c(1,2)] <- c(x1[k], y1[k])
      res[k] <- vector_SM_3(pos)
    }

    res
  }
  ypos <-tibble(x1 = X[1:n, 1], y1 = X[1:n, 2])
  gradient_plot(fun(x1, y1) ~ x1 & y1, domain(x1=0:2, y1=-2.8:0),
                npts=12) %>%
    gf_refine(coord_fixed()) %>%
    gf_path(y1 ~ x1, data = ypos, color="red", inherit=FALSE)

}

plot_config <- function(X, n=100) {
  row <- X[n,]
  energy <- row[7]
  config <- tibble(x = c(row[1], row[3], row[5]),
                   y = c(row[2], row[4], row[6]),
                   label = paste0("M", 1:3))
  P1 <- gf_path(y ~ x, data = config, linetype="dotted") %>%
    gf_point(y ~ x, data = config, size = 9) %>%
    gf_lims(x=c(0,3.5), y = c(-3.5,0)) %>%
    gf_text(y ~ x, label=~label, color="white") %>%
    gf_segment(0 + (-1.1) ~ 0 + 3.5, size=3) %>%
    gf_segment(0 + y ~ 0 + x, data = config[1,], linetype="dotted") %>%
    gf_segment(y + (-1.1) ~ x + 3.5, data = config[3,], linetype="dotted") %>%
    gf_text(0 ~ 2.3, label=paste("Pot. Energy =", round(energy,3))) %>%
    gf_refine(coord_fixed())
  P2 <- potential_slice(X, n)
  P3 <- potential_slice2(X, n)
  gridExtra::grid.arrange(P2, P3, P1, nrow=2)

}

X <- grad_descent(vector_SM_3,
                  c(2,0, 1, 0, 3.5, -1),
                  #c(1,-.4,2,-.7,3,-1),
                  stepsize=0.075, nstep=100)


animation::saveGIF({

  for (k in c(1:50, seq(51,100, by=2))) {
    # dev.hold()
    print(plot_config(X, k))
    #print(arrow_plot(2, T, h=0.5))

  }
}, movie.name = "foo.gif"
)

system("convert -delay 1x10 foo.gif www/SM_3.gif")

# system("convert -delay 1x20 raw.gif arrow-plot.gif")

# Then run this in the shell
# Change frame rate shell command
# convert -delay 1x30 raw.gif arrow-plot.gif
