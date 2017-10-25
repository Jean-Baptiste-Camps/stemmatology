\encoding{utf8}
\name{VL.pValues}
\alias{VL.pValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pValues of variant locations}
\description{
This functions aims at distinguishing random polygenesis from contamination, using Fisher exact test.
}
\usage{
VL.pValues(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{%
  a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number.
}
}
\details{
In order to assess the signiﬁcance of each variant location, and distinguish
random polygenesis from contamination (i.e. copying based on several exemplars), this function crosses two by two every possible pair of variant locations, and applies a Fisher exact test. It then counts the number of configurations in which a VL has passed the significance threshold (i.e. p < 0.05).
}
\value{
A numeric matric with the counts for each variant location.
%TODO: fonction à revoir et affiner.
}
\references{
Jean-Baptiste Camps, « Detecting Contaminations in Textual Traditions
Computer Assisted and Traditional Methods »,  Leeds, International Medieval Congress, 2013, unpublished paper, \url{https://www.academia.edu/3825633/Detecting_Contaminations_in_Textual_Traditions_Computer_Assisted_and_Traditional_Methods}.

Jean-Baptiste Camps, « Sélection des lieux variants et construction d’un
stemma : nouvelles expérimentations », 27e CILPR, Nancy, 2013, unpublished.
}
\author{
Jean-Baptiste Camps
}
%\note{
%%  ~~further notes~~
%}

\note{This function is still in active development.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PCC.Exploratory}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}

\keyword{stemmatology}
\keyword{stemma}