library(oce)
time <- Sys.time()
place <- "Halifax, Nova Scotia"
longitude <- -63.57
latitude <- 44.68
height <- 2                            # cm (convenient for a sheet of paper)
shadow <- function(time=Sys.time(), longitude=-63.57, latitude=44.68, height=2)
{
    sa <- sunAngle(time, longitude, latitude)
    daytime <- sa$altitude > 0
    angle <- (sa$azimuth + 180)[daytime]
    length <- (height / tan(sa$altitude * pi / 180))[daytime]
    angle <- ifelse(angle > 180, angle-360, angle)
    x <- length * sin(angle * pi / 180)
    y <- length * cos(angle * pi / 180)
    list(time=time[daytime],
         sun=list(altitude=sa$altitude[daytime], azimuth=sa$azimuth[daytime]),
         shadow=list(height=height, length=length, angle=angle, x=x, y=y))
}
r <- shadow(time=Sys.time(), longitude=longitude, latitude=latitude, height=height)
TIME <- format(time, "%Y-%b-%d %H:%M %Z")
LON <- if (longitude < 0) paste0(abs(longitude), "W") else paste0(longitude, "E")
LAT <- if (latitude < 0) paste0(abs(lattude), "S") else paste0(latitude, "N")

R <- 15                                # cm (suitable for letter-size paper)
lim <- c(-1,1) * R

if (!interactive()) png("stick_shadow.png", unit="in", width=7, height=7, res=100, pointsize=11)
par(mar=c(2,2,7,2))
if (r$shadow$length > 0) {
    plot(lim, lim, xlim=lim, ylim=lim, asp=1, type="n", xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")

    ## outer circle
    denom <- 30
    theta <- seq(0, 360, 1)
    x <- R * sin(theta * pi / 180)
    y <- R * cos(theta * pi / 180)
    lines(x, y)

    ## inner circle (stick height)
    lines(r$shadow$height * sin(theta * pi / 180),
          r$shadow$height * cos(theta * pi / 180),
          col="gray")

    ## Small segments at 1deg
    xx <- (R-R/denom) * sin(theta * pi / 180)
    yy <- (R-R/denom) * cos(theta * pi / 180)
    segments(x, y, xx, yy)
    ## Longer segments at 5deg
    theta <- seq(0, 360, 5)
    x <- R * sin(theta * pi / 180)
    y <- R * cos(theta * pi / 180)
    xx <- (R-1.4*R/denom) * sin(theta * pi / 180)
    yy <- (R-1.4*R/denom) * cos(theta * pi / 180)
    segments(x, y, xx, yy)
    ## Still longer segments at 10deg
    theta <- seq(0, 360, 10)
    x <- R * sin(theta * pi / 180)
    y <- R * cos(theta * pi / 180)
    xx <- (R-2*R/denom) * sin(theta * pi / 180)
    yy <- (R-2*R/denom) * cos(theta * pi / 180)
    segments(x, y, xx, yy)
    ## Labels
    theta <- seq(0, 350, 10)
    x <- (R + 2.2*R/denom) * sin(theta * pi / 180)
    y <- (R + 2.2*R/denom) * cos(theta * pi / 180)
    text(x, y, theta, xpd=NA)
    text(R-3*R/denom, 0, "E")
    text(0, R-3*R/denom, "N")
    text(0, -R+3*R/denom, "S")
    text(-R+3*R/denom, 0, "W")

    ## Centre, guiding line, shadow line
    points(0, 0, pch=20)
    X <- R * sin(r$shadow$angle*pi/180)
    Y <- R * cos(r$shadow$angle*pi/180)
    lines(c(0, X), c(0, Y), col="gray")
    lines(c(0, r$shadow$x), c(0, r$shadow$y), lwd=2)

    ANGLE <- sprintf("%.1f %s", abs(r$shadow$angle), if (r$shadow$angle >= 0) "clockwise" else "anticlockwise")
    mtext(TIME, line=6, adj=0)
    mtext(place, line=6, adj=0.5)
    mtext(paste(LON, LAT), line=6, adj=1)
    msg <- sprintf("A stick of height %.2f cm\ncasts a shadow of length %.2f cm\n at angle %s degrees from North",
                   height, r$shadow$length, ANGLE)
    mtext(msg, line=2)
} else {
    plot(c(-1,1), c(-1,1), type="n", axes=FALSE, xlab="", ylab="")
    text(0, 0, sprintf("%s\n%s, %s\nsun below horizon", TIME, LON, LAT))
}

if (!interactive()) dev.off()


