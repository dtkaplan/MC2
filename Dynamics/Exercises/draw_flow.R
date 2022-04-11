draw_flow <- function(seed=1996, center = c(0,0), width=5,
                      dx = 0.1, dy=0.1, ngrid=31, arrow=1) {
  xrange <- center[1] + width*c(-1,1)
  yrange <- center[2] + width*c(-1,1)
  Grid <- expand.grid(x=seq(xrange[1], xrange[2], length=ngrid),
                      y=seq(yrange[1], yrange[2],  length=ngrid))
  # This one is always the same, so zooming in will work
  Grid2 <- expand.grid(x = seq(-7, 7), y = seq(-7, 7))
  dom <- domain(x=!!xrange, y=!!yrange)
  xraw <- rfun(~ x + y, seed = seed)
  yraw <- rfun(~ x + y, seed = seed + 1)
  Stats <- Grid2 %>%
    mutate(xvals = xraw(x,y),
           yvals = yraw(x,y)) %>%
    summarize(midx = mean(xvals), midy = mean(yvals),
              sdx = sd(xvals), sdy = sd(yvals))

  xfun <- function(x, y) x + dx*(xraw(x,y)-Stats$midx)/Stats$sdx
  yfun <- function(x, y) y + dy*(yraw(x,y)-Stats$midy)/Stats$sdy
  Arrows <- Grid %>%
    mutate(xend = xfun(x,y), yend=yfun(x,y))
  contour_plot(xfun(x,y) - x ~ x + y, domain=dom,
               contour_color="dodgerblue", contours_at = c(0),
               skip=0, labels=FALSE) %>%
    contour_plot(yfun(x,y) - y ~ x + y,
                 contour_color="orange3", contours_at = c(0),
                 skip=0, filled=FALSE, labels=FALSE) %>%
    gf_segment(y + yend ~ x + xend, data = Arrows, size=arrow*0.5) %>%
    gf_point(yend ~ xend, data = Arrows, size = arrow, alpha=0.3) %>%
    gf_refine(coord_fixed())
}
