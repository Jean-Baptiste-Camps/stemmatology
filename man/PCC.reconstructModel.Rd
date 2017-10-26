\encoding{utf8}
\name{PCC.reconstructModel}
\alias{PCC.reconstructModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC.reconstructModel
}
\description{
\code{PCC.reconstructModel} examines coherent clusters of witnesses (PCC.buildGroup), to either identify their model in the tradition, either suggest a virtual model for the group.  
}
\usage{
PCC.reconstructModel(x, omissionsAsReadings = FALSE, recoverNAs = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
A PCCbuildGroup object.
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings. They are taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).
     
     Default: \code{FALSE}.
}
%  \item{limit}{
%The maximum number of severe disagreements expected for witnesses to be in the same group. 
%
%    Default: \code{0}.
%}
%TODO: faut-il implementer qqchose de ce genre pour cette fonction?
 \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. 
  
  Default: \code{TRUE}.
}
}
\details{
This function takes PCC.buildGroup objects as input. It computes the characteristics of a model of any group, and compares it to the existing witnesses. If a witness has the same characteristics as the computed model, it is identified as the model for the group. If no witness seems to be a good fit, the function adds a virtual model to the tradition.
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

\examples{

}

\seealso{
More about the next step: \code{\link{PCC.Stemma}}.
More about the input: \code{\link{PCC.buildGroup}}
}
