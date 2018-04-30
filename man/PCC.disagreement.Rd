\encoding{utf8}
\name{PCC.disagreement}
\alias{PCC.disagreement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCC.disagreement 
}
\description{
  The PCC.disagreement function helps spotting disagreements (and agreements) between manuscripts. For a given numeric matrix, representing the variants in different manuscripts, it locates disagreements (benigne or severe), agreements and omissions in common between manuscripts.
}
\usage{
PCC.disagreement(x, omissionsAsReadings = FALSE)
}

\arguments{
  \item{x}{
 a numeric matrix, with manuscripts in colums, variants in rows, and readings coded by a number.
}
  \item{omissionsAsReadings}{
     logical; if TRUE, omissions are considered as readings.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns:
  \item{database }{The original database.}
  \item{severeDisagreement }{A list of the severe disagreements between manuscripts.}
  \item{benigneDisagreement }{A list of the benigne disagreements between manuscripts.}
  \item{agreements}{A list of agreements between manuscripts.}
  \item{omissionsInCommon }{A list of all the omissions in common between manuscripts(if omissionsAsReadings is set to TRUE, this will be NA).}
  \item{omissionsOriented }{A list of the omissions present in a manuscript but not in another (if omissionsAsReadings is set to TRUE, this will be NA).}

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
