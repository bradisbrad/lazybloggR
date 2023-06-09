% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_post.R
\name{create_post}
\alias{create_post}
\title{Create Post}
\usage{
create_post(title, tags = NULL, ..., author = Sys.getenv("DEFAULT_AUTHOR"))
}
\arguments{
\item{title}{Title of new post}

\item{tags}{Categories for new post}

\item{...}{Additional categories}

\item{author}{Author of post}
}
\description{
Create Post
}
\examples{
create_post('My Cool Post')
}
