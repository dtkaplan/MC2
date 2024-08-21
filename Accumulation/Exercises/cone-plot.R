# Drawing the cone layout diagram

make_rand_fun <- function(seed = NULL , center=0, width=5, aveval=0) {
  rawf <- doodle_fun(~ x, seed = seed)
  offset <- mean(rawf(seq(-3,3,100)))
  function(x) { rawf((x - center)/(width/7)) - offset + aveval}
}

cone_diagram <- function(f,
                         dom=bounds(x=c(0,10)), # endpoints of plot
                         total_area = 10,
                         events_per_unit_area = 60,
                         startx = 0,
                         startn=0, #initial stock at time startx
                         left = NA, # limits of integration
                         right = NA
                         ) {
  xmin <- dom[[1]][1] # for convenience
  xmax <- dom[[1]][2]
  total_events = total_area * events_per_unit_area
  # find total number of events, positive or negative
  Fabs <- antiD(abs(f(x)) ~ x)
  # number of actual events, according to f(x)
  n_events <- diff(Fabs(dom[[1]]))

  # scaling the derivative to produce the specified
  # total number of events
  Points <- tibble::tibble(
    x = seq(xmin, xmax, length = 200),
    y = f(x),
    absy = abs(y))
  top <- mean(Points$absy)
  unsigned_area_under_curve <- top*diff(dom[[1]])
  rate_fun <- function(x) (total_area*f(x)/unsigned_area_under_curve)
  Points$bigY <- Points$y * total_events/n_events
  rect_area <- top * diff(dom[[1]])
  # How many random numbers to generate to get
  # a total of <total_events> (roughly)
  npts <- ceiling(unsigned_area_under_curve * events_per_unit_area)
  ## Generate the actual events
  Points <- tibble::tibble(
    x = seq(xmin, xmax, length = npts),
    y = f(x),
    absy = abs(y),
    yval = top * runif(npts)
  ) %>%
    filter(yval <= absy) %>%
    mutate(val = sign(y),
           color = ifelse(val < 0, "orange3", "black"),
           shape = ifelse(val < 0, "-", "+"))
  # the anti-derivative to be plotted
  F <- antiD(rate_fun(x) ~ x)

  # make the plots
  P1 <- slice_plot(rate_fun(x) ~ x, dom, npts = 500)
  if (!(is.null(left) || is.na(left) ||
        is.null(right) || is.na(right))) {
    Keepers <- tibble::tibble(
      x = seq(left, right, length = 500),
      y = rate_fun(x))
    P1 <- P1 %>% gf_ribbon(0 + y ~ x, data = Keepers, inherit=FALSE,
                           fill = "dodgerblue", color=NA, alpha = 0.3) %>%
      gf_ribbon(0 + y ~ x, data = Keepers %>% filter(y < 0),
                color = NA, fill = "orange3", alpha = 0.3, inherit = FALSE)
  }

    # gf_labs(y = fname) %>%
    # gf_theme( axis.title.x = element_blank(),
    #           axis.text.x = element_blank())

  P2 <- gf_vline(xintercept = ~ x, data = Points,
                 color = ~ color,
                 alpha = 0.3) %>%
    gf_refine(scale_color_identity(),
              scale_shape_identity(),
              scale_y_continuous(limits = c(-1,1),
                                 breaks=0, labels="00")) %>%
    gf_theme( axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid = element_blank()
    )


  F2 <- function(x) F(x) - F(startx) + startn

  P3 <- slice_plot(F2(x) ~ x, dom)
    if (!(is.null(left) || is.na(left) ||
          is.null(right) || is.na(right))) {
     Dots <- tibble::tibble(x = c(left, right),
                            y = F2(x))
     P3 <- P3 %>% gf_point(y ~ x, data = Dots, color="dodgerblue")
    }

    return(list(fplot = P1, eplot = P2, Fplot = P3, f = rate_fun,
                F = F))
}
