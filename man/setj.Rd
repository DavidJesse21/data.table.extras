% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setj.R
\name{setj}
\alias{setj}
\alias{setj_at}
\alias{setj_if}
\alias{setj_grep}
\title{Conditional column manipulation by reference}
\usage{
setj_at(DT, cols, .f, ...)

setj_if(DT, .p, .f, ...)

setj_grep(DT, pattern, .f, ...)
}
\arguments{
\item{DT}{(\code{data.table::data.table()})\cr
A \code{data.table}.}

\item{cols}{(\code{numeric()} or \code{character()})\cr
A vector of integer column indices or character column names.}

\item{.f}{(\verb{function()})\cr
A function that will be applied by reference to the matching columns}

\item{...}{Additional function arguments you want to supply to \code{.f}.}

\item{.p}{(\verb{function()})\cr
A predicate function that will be applied to all columns and returns
a logical vector.}

\item{pattern}{(\code{character(1)})\cr
A regular expression pattern as used by \code{grep()}.}
}
\value{
(\code{data.table::data.table()})\cr
The functions invisibly return the modified \code{data.table}.
}
\description{
These functions manipulate existing columns of your \code{data.table} by reference
based on a selection of columns, a predicate function or a regex pattern that the
column name must match.
}
\details{
\code{setj_at()} takes an integer (!) vector of column indices or a character vector of
column names as input for \code{cols} and applies \code{.f} on the corresponding columns.
If you supply indices or column names which don't exist in your \code{data.table}
you will get an informative message.

\code{setj_if()} takes a predicate function as input for \code{.p}.
The function \code{.f} will then be applied to those columns for which
\code{.p} returns \code{TRUE}.
If the predicate function returns \code{FALSE} for all columns you will get an
informative message.

\code{setj_grep()} manipulates columns based on regular expression pattern matches.
For that you need to pass a single regex pattern to the \code{pattern} argument.
If the pattern doesn't match any of your \code{data.table} columns you will get
an informative message.
}
