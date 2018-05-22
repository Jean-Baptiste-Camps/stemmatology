\encoding{utf8}
\name{PCC.distribute}
\alias{PCC.distribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCC.distribute
}
\description{
\code{PCC.distribute} is a function that takes as input a character matrix of comma separated alternate readings, and creates a new database where they are  distributed in new variant locations.
}
\usage{
PCC.distribute(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character matrix, containing comma separated alternate readings.
}
}
\details{
The function helps treating databases including alternate readings. Having two or more entries in a same cell can cause treatment problems. Here, the table is expanded to accomodate all the readings each in one cell. 
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
Jean-Baptiste Camps & Florian Cafiero
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
\keyword{stemmatology}
\keyword{data-manipulation}%vérif keywords
