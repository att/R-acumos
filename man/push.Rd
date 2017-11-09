\name{push}
\alias{push}
\alias{auth}
\title{
  Push a component into the Acumos repository
}
\description{
  \code{push} pushes a component into the Acumos repository.

  \code{auth} obtains an authentication token to be used with
  \code{push} where required.
}
\usage{
push(url, metadata = "component.json", payload = "component.bin",
     proto = "component.proto", token, ...)
auth(url, user, password)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{URL for the POST request}
  \item{metadata}{name of the metadata file}
  \item{payload}{name of the model payload file}
  \item{proto}{name of the protobuf schema file}
  \item{token}{token obtained from \code{auth} (optional)}
  \item{user}{user name to use for authentication}
  \item{password}{password to use for authentication}
  \item{\dots}{any additional arguments passed the the \code{POST()} method}
}
%\details{
%}
\value{
  \code{push}: invisibly, result of the request (may change in the
  future)

  \code{auth}: authentication token
}
%\references{
%}
\author{
  Simon Urbanek
}
%\note{
%}
\seealso{
  \code{\link{compose}}
}
\keyword{interface}
