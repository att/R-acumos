\name{compose}
\alias{compose}
\title{
  Compose a Acumos microservice
}
\description{
  \code{compose} generates everything necessary to create a Acumos microservice.
}
\usage{
compose(predict, transform, fit, generate, service, initialize,
        aux = list(), name = "R Component", file = "component.amc")
}
\arguments{
  \item{predict}{predict function (optional)}
  \item{transform}{transform function (optional)}
  \item{fit}{fit function (optional)}
  \item{generate}{generate function (optional)}
  \item{service}{function handling additional non-Acumos requests
    (optional)}
  \item{initialize}{function for any one-shot initializations of the
    environment}
  \item{aux}{list of any auxiliary objects that are to be passed to the
  global workspace of the component.}
  \item{name}{string, name of the component}
  \item{file}{string, filename for the resulting component file}
}
\details{
  A regular component will have at least one of the three functions
  \code{predict}, \code{transform} or \code{fit} set in which case
  those functions are exposed via the Acumos API.

  A special component can instead provide the \code{generate} function
  only in which case the \code{generate} function is called upon
  instantiation instead of serving the Acumos API. This is useful
  when adapting from other inputs than Acumos since the
  \code{generate} function can use arbitrary input methods and then use
  Acumos API to push to other Acumos components. Similarly,
  non-Acumos requests can be served using the \code{service} call-back
  function with the signature
  \code{function(path, query, body, headers)} where \code{body} is
  \code{NULL} or a raw vector. This interface is experimental and
  currently not part of the offical API.

  The functions can have two special arguments \code{inputs} and
  \code{output} which are used to define the types of the input and
  output data. They have to be named string vectors where the names
  will match formats of the functions and the string specifies the
  input type (class). At this point only \code{"character"},
  \code{"integer"}, \code{"numeric"} and \code{"raw"} are
  supported. If those arguments are not present, they default to
  \code{c(x="character")}. If the result of the function is a list, it
  is assumed that the list holds the outputs, otherwise only one
  output is used.

  The \code{compose()} function is called mainly for its side-effect
  of creating the Acumos API component file, at this point it is a (ZIP)
  bundle of \code{meta.json} (metadata), \code{component.bin}
  (serialized R functions and code) and \code{component.proto} (I/O
  definition)
}
\value{
  Structure describing the component (parsed content of the JSON
  description).
}
\seealso{
\code{\link{run}}
}
\author{
Simon Urbanek
}
\examples{
## pass-through component
compose(transform=identity, name="identity")

## simple addition
compose(transform=function(a, b, inputs=c(a="numeric", b="numeric"), outputs=c(x="numeric")) a + b, name="Addition")

## silly RF trained on Iris
library(randomForest)
compose(predict=function(..., inputs=lapply(iris[-5], class)) as.character(predict(rf, as.data.frame(list(...)))),
        aux = list(rf = randomForest(Species ~ ., data=iris)),
        name="Random Forest")

}
\keyword{interface}
