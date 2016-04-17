#dt <- data.frame(a = c(1.8, 0.7, 1.8, 1.2, 1.2, 0.5),
#	          b = c(1.0, 1.0, 1.0, -0.5, 0.5, 0),
#		  c = c(0.0, 0.0, 0.25, 0.2, 0.0, 0.1),
#                 s = c(0, 1, 0, 1, 1, 0))
#tht <- seq(-3, 3, .01)
#
#title <- "Item Information Functions for Six Items"
#
#dir <- "..Panko/Documents/" #or# dir <- getwd()
#
#filename <- "iif_plot.jpg"
#
#type <- "iif" #or# type <- "icc" #or# type <- "logl #or# type <- likl

irtPlot <- function(dat,
                    theta,
                    title = NULL,
                    ddir = getwd(),
                    save = FALSE,
                    type,
                    filename = "plot1.jpg",
                    dpi = 800,
                    height = 6.5,
                    width = 10,
                    itmNam = NULL) {

    library(ggplot2)
    source("irtConv.R")

    if (length(itmNam) > 1) {

        item = factor(rep(itmNam, each = length(theta)))

    } else {

        item = factor(rep(1:nrow(dat), each = length(theta)))

    }

    #type icc
    if(type == "icc") {

        prb <- as.vector(apply(dt, 1, calcP))

        itms <- data.frame(prb = prb,theta = rep(theta, nrow(dat)),item =item)

        ylb <- expression(atop(P(theta),))

        itmplot <- plotThings(itms, x1 = itms$theta, y1 = itms$prb, color = itms$item)

        if (save == TRUE) {

            ggsave(itmplot, file = paste0(ddir,"/",filename),  dpi = dpi, height = height, width = width)

            print(itmplot)

        } else {

            print(itmplot)

        }

    #type iif
    } else if(type == "iif") {

        inf <- as.vector(apply(dat, 1, calcI))

        itms <- data.frame(inf = inf, theta = rep(theta, nrow(dat)),item = item)

        ylb <- expression(atop(P(theta),))

        itmplot <- plotThings(itms, x1 = itms$theta, y1 = itms$inf, color = itms$item)

        if (save == TRUE) {

            ggsave(itmplot, file = paste0(ddir,"/",filename), dpi = dpi, height = height, width = width)

            print(itmplot)

        } else {

            print(itmplot)

        }

    #type logl/likl
    } else if(type == "likl"|type == "logl") {

        itms <- loglik(dat, theta)

        if(type == "likl") {

            y1 <- itms$likl
            ylb <- "Likelihood \n"

        } else {

            y1 <- itms$logl
            ylb <- "Log-likelihood \n"
        }

        itmplot <- plotThings(itms, x1 = itms$theta, y1 = y1, color = "red")

        if (save == TRUE) {

            ggsave(itmplot, file = paste0(ddir,"/",filename),  dpi = dpi, height = height, width = width)

            print(itmplot)

        } else {
            print(itmplot)

        }

    } else {

        stop("Please provide a valid plot type, comrade")

    }

}


# type icc calculation
calcP <- function(dat, theta = tht) {
    iprb <- dat["c"] + (1 - dat["c"])*((exp(1.7*dat["a"]*(theta-dat["b"])))/(1+(exp(1.7*dat["a"]*(theta-dat["b"])))))

    return(iprb)
}

# type iif calculation
calcI <- function(dat, theta = tht) {
    inf <- (1.7^2*dat["a"]^2*(1 - dat["c"]))/((dat["c"]+exp(1.7*dat["a"]*(theta-dat["b"])))*(1+exp(-1.7*dat["a"]*(theta-dat["b"])))^2)

    return(inf)
}

# type likl/logl calculation
loglik <- function(dat, theta) {

dt <- data.frame(theta = rep(theta, each = nrow(dat)), dat)

prb <- rep(NA, nrow(dt))
out <- rep(NA, nrow(dt))

for (i in 1:nrow(dt)) {

    if(dt$s[i] == 0) {

        prb[i] <- 1 - (dt$c[i] + (1 - dt$c[i])*((exp(1.7*dt$a[i]*(dt$theta[i]-dt$b[i])))/(1+(exp(1.7*dt$a[i]*(dt$theta[i]-dt$b[i]))))))

    } else {
        prb[i] <- dt$c[i] + (1 - dt$c[i])*((exp(1.7*dt$a[i]*(dt$theta[i]-dt$b[i])))/(1+(exp(1.7*dt$a[i]*(dt$theta[i]-dt$b[i])))))

    }

}

    grp <- as.factor(rep(1:length(theta), each = nrow(dat)))

    out <- aggregate(prb, by = list(grp), FUN = prod)

    colnames(out) <- c("theta", "likl")

    out$theta <- as.numeric(theta)

    llkl <- data.frame(out, logl = log(out$likl))

    return(llkl)

}

# plots!
plotThings <- function(itms, ttl = title, x1, y1, color, ylbs = ylb) {

    ggplot(itms, aes(x = x1, y = y1)) +
        geom_line(aes(color = color), size = 1) +
        ggtitle(paste(title, '\n')) +
        xlab(expression(atop(,theta))) +
        ylab(ylb) +
        theme(axis.title = element_text(size = 14, face = "italic"), title = element_text(size = 15, face = "bold"))

}

#irtPlot(dat = dat, theta = theta, title = title, save = TRUE, type = type, filename = filename)
