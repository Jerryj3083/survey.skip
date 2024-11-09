\name{data_sm}
\alias{data_sm}

\title{
Create the shadow matrix of a survey data set

}
\description{
This function creates the shadow matrix of a given survey data set and skip logic file. It will also produce the data set with computed values if it is specified in the skip logic.
}
\usage{
data_sm(data, skip_logic)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test4}{data.frame, survey data set
}
  \item{skip_logic2}{list, skip logic file of the JSON file once passed through fromJSON()
}
}
\details{
Complex survey design that use skip patterns produce an unavoidable NA response type that originate from skipped qusetions. To find the types of all responses, skip logic file that is build following a specific structure its used with the survey data to create the shadow matrix.
}
\value{

data_sm returns a value of class "survey_sp" consisting of six components.

\item{data}{the data frame of the survey data set. The is the same as the input survey data set with an addition of the computed values for the computed questions. The column names in this are also changed to the same ones used in the shadow matrix}
\item{sm}{the shadow matrix of the corresponding survey data set and skip logic}
\item{questions}{the questions names that are asked for each respondent}
\item{columns}{the column number of the asked questions for each respondent}
\item{not_next}{a list of data frames, each storing the questions that are not chosen as the next question for a row}
\item{col_names}{shadow matrix column names}


}
\references{
%% ~put references to the literature/web site here ~
}
\author{Jerry Jiang
}
\keyword{list}