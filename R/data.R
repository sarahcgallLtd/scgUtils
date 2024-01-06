#' Wave 25 of the 2014-2023 British Election Study Internet Panel
#'
#' The `survey` dataset is from the British Electoral Study. It is purely as a way to practice
#' using the functions and charts within this package. The fieldwork for the survey was conducted
#' by YouGov between 5 May and 23 May, 2023. The practice dataset has n=5000 respondents and 65 variables.
#' The original dataset has 570 variables and n=30,407 respondents with 63% retained from Wave 23 (May 2022)
#' and 74% from Wave 24 (December 2022).  The dataset includes a cumulative weight (wt).
#'
#' A full description of themethodology and questionnaire can be found here:
#' \url{https://www.britishelectionstudy.com/wp-content/uploads/2023/11/Bes_wave25Documentation.pdf}
#'
#' @format A data frame with 5,000 rows and 65 variables:
#' \describe{
#'   \item{id}{unique identifier of respondent}
#'   \item{wt}{weight variable}
#'   \item{turnoutUKGeneral}{Likelihood to vote in general election}
#'   \item{generalElectionVote}{General election vote intention (recalled vote in post-election waves)}
#'   \item{partyIdStrength}{Strength of party identification}
#'   \item{partyId}{Party identification}
#'   \item{partyIdSqueeze}{Party ID if no party to first ID question}
#'   \item{mii}{Most Important Issue}
#'   \item{mii_cat}{MII manual coding}
#'   \item{small_mii_cat}{MII (manually-coded-collapsed)}
#'   \item{LRAL_mii_cat}{Dimension of MII}
#'   \item{bestOnMII}{Best party on most important issue}
#'   \item{polAttention}{Attention to Politics}
#'   \item{pidWeThey}{When I speak about this party, I usually say "we" instead of "they".}
#'   \item{pidInterestedOthers}{I am interested in what other people think about this party.}
#'   \item{pidCriticiseParty}{When people criticize this party, it feels like a personal insult.}
#'   \item{pidCommonParty}{I have a lot in common with other supporters of this party.}
#'   \item{pidConnected}{When I meet someone who supports this party, I feel connected with this person.}
#'   \item{pidPraiseGood}{When people praise this party, it makes me feel good.}
#'   \item{pidWeTheyb}{When I speak about this party, I usually say "we" instead of "they".}
#'   \item{pidInterestedOthersb}{I am interested in what other people think about this party.}
#'   \item{pidCriticisePartyb}{When people criticize this party, it feels like a personal insult.}
#'   \item{pidCommonPartyb}{I have a lot in common with other supporters of this party.}
#'   \item{pidConnectedb}{When I meet someone who supports this party, I feel connected with this person.}
#'   \item{pidPraiseGoodb}{When people praise this party, it makes me feel good.}
#'   \item{likeSunak}{Like-dislike: Rishi Sunak}
#'   \item{likeStarmer}{Like-dislike: Keir Starmer}
#'   \item{likeCon}{Like-dislike: Conservatives}
#'   \item{likeLab}{Like-dislike: Labour}
#'   \item{likeLD}{Like-dislike: Liberal Democrats}
#'   \item{likeSNP}{Like-dislike: Scottish National Party}
#'   \item{likePC}{Like-dislike: Plaid Cymru}
#'   \item{likeBrexitParty}{Like-dislike: Brexit Party}
#'   \item{likeGrn}{Like-dislike: Green Party}
#'   \item{country}{Country}
#'   \item{gender}{Gender}
#'   \item{age}{Age}
#'   \item{ageGroup}{Age group}
#'   \item{pcon}{Parliamentary Constituency}
#'   \item{p_education}{Education qualification (highest attained)}
#'   \item{p_work_stat}{Which of these applies to you?}
#'   \item{p_hh_children}{How many of the people in your household are under 18?}
#'   \item{p_housing}{Do you own or rent the home in which you live?}
#'   \item{p_gross_household}{Gross household income}
#'   \item{p_gross_personal}{Gross personal income}
#'   \item{p_hh_size}{How many people, including yourself, are there in your household? Please includ}
#'   \item{p_socgrade}{Social Grade}
#'   \item{p_disability}{Are your day-to-day activities limited because of a health problem or disabilit}
#'   \item{p_sexuality}{Which of the following best describes your sexuality?}
#'   \item{p_job_sector}{What kind of organisation do you work for?}
#'   \item{p_education_age}{Age completed formal education}
#'   \item{p_marital}{What is your current marital or relationship status?}
#'   \item{p_paper_read}{Which daily newspaper do you read most often?}
#'   \item{p_religion}{Do you regard yourself as belonging to any particular religion, and if so, to w}
#'   \item{p_parent}{Are you a parent or guardian?}
#'   \item{p_country_birth}{Country of birth}
#'   \item{p_ethnicity}{To which of these groups do you consider you belong?}
#'   \item{p_past_vote_2010}{2010 GE vote choice}
#'   \item{p_past_vote_2005}{2005 GE vote choice}
#'   \item{p_past_vote_2015}{2015 GE vote choice}
#'   \item{p_past_vote_2017}{2017 GE vote choice}
#'   \item{p_past_vote_2019}{2019 GE vote choice}
#'   \item{p_eurefvote}{EU referendum vote (earliest recorded)}
#'   \item{p_edlevel}{Education level}
#'   \item{p_eurefturnout}{EU referendum turnout (earliest recorded)}
#' }
#' @source This dataset was sourced from the British Electoral Study website (Wave 25).
#' The raw data are available from \url{https://www.britishelectionstudy.com/data-object/wave-25-of-the-2014-2023-british-election-study-internet-panel/}.
"survey"