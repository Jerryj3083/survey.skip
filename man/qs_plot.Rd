\name{qs_plot}
\alias{qs_plot}

\title{
Plotting the relationship of two questions
}
\description{
This function plots two questions against each other in a scatterplot, including the NA responses whic are normally removed.
}
\usage{
qs_plot(x, y, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{string, question name of the x axis question
}
  \item{y}{string, question name of the y axis qusetion
}
  \item{data}{survey_sp, the object obtained from the data_sm function
}
}
\value{
Creates a scatterplot of the two qustions against each other with NA values of the axis sides.
}

\author{
Jerry Jiang
}
\keyword{aplot}