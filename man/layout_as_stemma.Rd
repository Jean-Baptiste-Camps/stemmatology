\name{layout_as_stemma}
\alias{layout_as_stemma}
\title{layout_as_stemma}
\description{
\code{layout_as_stemma} creates a tree-like layout from an edgelist,
where nodes are placed horizontally according to
a measure of distance from their parent node.}
\usage{
layout_as_stemma(x)
}
\arguments{
  \item{x}{an edgelist containing, as a third column, the distance between the 
  two nodes.}
}
\details{
The distance between the nodes will usually correspond to the number of 
different readings (disagreements and omissions). If a node has several
parents, the function will consider only the distance from the last parent
in topological order.}
\value{
A layout, i.e. a matrix of two columns, giving x,y coordinates for each node.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Jean-Baptiste Camps}

\section{Warning}{This function is experimental. Horizontal overlapping may occur
has a result.}

\seealso{
\code{\link{PCC.Stemma}}, \code{\link{PCC.reconstructModel}}.}
\examples{
edgelist = structure(
    c("{ABC}", "{ABC}", "{ABC}", "D", "A","A","G",
        "A", "B", "C", "E", "F","G","H",
        1,5,3,10,3,4,5), .Dim = c(7L, 3L)
  )
g = igraph::graph_from_edgelist(edgelist[,1:2], directed = TRUE)
layout = layout_as_stemma(edgelist)
plot(g, layout = layout)
}
\keyword{stemmatology}
\keyword{graphs}
