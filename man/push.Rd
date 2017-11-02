\name{push}
\alias{push}
\title{
  Push a component into the Acumos repository
}
\description{
  Pushes a component into the Acumos repository.
}
\usage{
push(url, metadata = "component.json", payload = "component.bin", proto = "component.proto", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{URL for the POST request}
  \item{metadata}{name of the metadata file}
  \item{payload}{name of the model payload file}
  \item{proto}{name of the protobuf schema file}
  \item{\dots}{any additional arguments passed the the \code{POST()} method}
}
%\details{
%}
\value{
  Invisibly, result of the request (may change in the future)
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
