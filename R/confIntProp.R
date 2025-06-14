#' Confidence intervals for proportions, vectorized over all arguments
#'
#' This function simply computes confidence intervals for proportions.
#'
#' This function is the adapted source code of [binom.test()]. It
#' uses [pbeta()], with some lines of code taken from the
#' [binom.test()] source. Specifically, the count for the low
#' category is specified as first 'shape argument' to [pbeta()], and
#' the total count (either the sum of the count for the low category and the
#' count for the high category, or the total number of cases if
#' `compareHiToLo` is `FALSE`) minus the count for the low category
#' as the second 'shape argument'.
#'
#' @param x The number of 'successes', i.e. the number of events, observations,
#' or cases that one is interested in.
#' @param n The total number of cases or observatons.
#' @param conf.level The confidence level.
#' @param plot Whether to plot the confidence interval in the binomial distribution.
#' @return The confidence interval bounds in a twodimensional matrix, with the
#' first column containing the lower bound and the second column containing the
#' upper bound.
#' @author Unknown (see [binom.test()]; adapted by Gjalt-Jorn Peters)
#'
#' Maintainer: Gjalt-Jorn Peters <rock@@opens.science>
#' @seealso [binom.test()]
#' @keywords univar htest
#' @examples
#'
#'   ### Simple case
#'   rock::confIntProp(84, 200);
#'
#'   ### Using vectors
#'   rock::confIntProp(c(2,3), c(10, 20), conf.level=c(.90, .95, .99));
#'
#' @export confIntProp
confIntProp <- function(x, n, conf.level = .95, plot=FALSE) {

  originalN <- n;
  originalX <- x;

  if (length(x) > length(n)) {
    if (length(n) == 1) {
      n <- rep(n, length(x));
    } else {
      stop("Length of 'x' is ", length(x),
           ", but length of 'n' is ", length(n), ".");
    }
  } else if (length(x) < length(n)) {
    if (length(x) == 1) {
      x <- rep(x, length(n));
    } else {
      stop("Length of 'x' is ", length(x),
           ", but length of 'n' is ", length(n), ".");
    }
  }

  ### Adapted from binom.test
  p.L <- function(x, n, alpha) {
    if (x == 0)
      0
    else stats::qbeta(alpha, x, n - x + 1)
  }
  p.U <- function(x, n, alpha) {
    if (x == n)
      1
    else stats::qbeta(1 - alpha, x + 1, n - x)
  }
  ### END binom.test code

  ### Vectorized both for x/n pairs and for confidence levels;
  ### first process for each confidence level
  res <- sapply(conf.level, function(confLevel) {
    ### Then for x/n pairs for this given confidence level
    return(sapply(seq_along(originalX), function(i) {
      ### Then for n
      return(sapply(seq_along(originalN), function(j) {
        return(c(x[i] / n[j],
                 p.L(x=x[i],
                     n=n[j],
                     alpha=(1 - confLevel)/2),
                 p.U(x=x[i],
                     n=n[j],
                     alpha=(1 - confLevel)/2)));
      }));
    }));
  });

  if (ncol(res) > 1) {
    res <- lapply(1:ncol(res), function(confI) {
      rslt <- matrix(res[, confI], ncol=3, byrow=TRUE);
      rownames(rslt) <- paste0(rslt[, 1], ", ",
                               rep(100*conf.level[confI], each=nrow(rslt)), "%");
      rslt <- rslt[, -1];

      if (is.null(ncol(rslt))) {
        names(rslt) <- c('ci.lo', 'ci.hi');
      } else {
        colnames(rslt) <- c('ci.lo', 'ci.hi');
      }
      return(rslt);
    });
    res <- do.call(rbind, res);
    if (is.null(row.names(res))) {
      row.names(res) <- paste0(x/n, ", ", 100*conf.level, "%");
    }
  } else {
    res <- matrix(res, ncol=3, byrow=TRUE);
    rownames(res) <- paste0(res[, 1], ", ",
                            rep(100*conf.level, each=nrow(res)), "%");
    res <- res[, -1, drop=FALSE];
    colnames(res) <- c('ci.lo', 'ci.hi');
  }

  if (plot && (length(n)==1) && (length(x)==1)) {
    plotDf <- data.frame(x=seq(0, n, by=1),
                         y=stats::dbinom(seq(0, n, by=1), size=n, prob=x/n));
    attr(res, 'plot') <- ggplot2::ggplot(plotDf,
                                         ggplot2::aes_string(x='x', y='y')) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal(base_size=16) +
      ggplot2::scale_x_continuous(name="Frequency",
                                  sec.axis=ggplot2::dup_axis(trans=~./n,
                                                             name="Percentage",
                                                             breaks=round(sort(c(res, 0, .25, .5, .75, 1)), 2))) +
      ggplot2::geom_vline(xintercept=n*res[, 'ci.lo']) +
      ggplot2::geom_vline(xintercept=n*res[, 'ci.hi']) +
      ggplot2::labs(y = "Density");
  }

  return(res);
}
