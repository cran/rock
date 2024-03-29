% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbind_dfs.R
\name{rbind_dfs}
\alias{rbind_dfs}
\title{Simple alternative for rbind.fill or bind_rows}
\usage{
rbind_dfs(x, y, clearRowNames = TRUE)
}
\arguments{
\item{x}{One dataframe}

\item{y}{Another dataframe}

\item{clearRowNames}{Whether to clear row names (to avoid duplication)}
}
\value{
The merged dataframe
}
\description{
Simple alternative for rbind.fill or bind_rows
}
\examples{
rbind_dfs(Orange, mtcars);
}
