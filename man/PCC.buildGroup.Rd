\name{PCC.buildGroup}
\alias{PCC.buildGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC.buildgroup
}
\description{
\code{PCC.buildgroup} groups together witnesses in relevant clusters.
}
\usage{
PCC.buildGroup(x, limit = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A PCC.disagreement object.
}
  \item{limit}{
  The maximum number of severe disagreements allowed for two witnesses in the same group. Default (and advised) value: \code{0}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stemma}
\keyword{construction}% __ONLY ONE__ keyword per line
\keyword{stemmatology}
