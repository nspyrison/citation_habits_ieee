# citation_habits_ieee
- (R) Analysis of the survey: 'Reading and citation habits of visualization researchers'
- writing done on Overleaf

Cloning and modifying the analysis from the similarly structured: 
https://github.com/csoderberg/Sloan_survey/tree/master

____
**Excepts from initial submission:**

<!--\section{Additional analysis} -->

## Differences across positions


In our pre-registration, we had stated that we would ``conduct a Spearman ranked correlation of the Likert scale responses in general and across experience levels''. While we had done so, we found that the output is vast and hard to interpret---the full cohort would be a $p*p$ matrix as compared across the same size matrices for several levels of position. In this case, it would mean comparing 5 unique $20*20$ matrices. This is compounded by the difficultly of interpretation. Once an extreme value is identified, it is hard to meaningfully interpret what a difference in correlation of 2 Likert items means between two different levels of position.

In the spirit of comparing the difference across position, we resort to the approach from Soderberg \etal \cite{soderberg_credibility_nodate}: reducing the distribution within each Likert item across the different positions in tabular form, and coloring cells based on the mean response. In large, there does not seem to be evidence that there is a difference in the distributions of Likert items across positions. We can see some slight variations in the results across positions. They remain modest however, and might be resulting noise in our relatively small sample. 
<!--- %This is in line with \autoref{fig:violins}, there is some support a variations, but not enough to conclude statistical significance.% --->


## Additional analysis

Going beyond our pre-registered analysis, we also performed some preliminary factor analysis. Using parallel factor analysis on the 20 Likert items suggest keeping 4 factors is sufficient. We extracted a four-factor fit using maximum likelihood (ML) and oblimin rotation, the fit had TLI = 0.39 and an RMSEA of 0.087, 90\% CI [0.061, 0.117]. This would suggest that the true intrinsic dimensionality of these 20 Likert items is sufficiently approximated in 4 linear components. This seems a bit aggressive of a dimension reduction relative to other dimensionality estimation methods. For comparison, Minkowski-Bouligand (box-counting fractal) suggests 6.54, correlation fractal dimension suggests 6.04, and Maximum Likelihood Estimation suggests 8. These are various takes on how ``compactly'', or how few factors can approximate the variation within the 20 Likert items. It is interesting to note that parallel factor analysis suggests 4 factors, while other methods suggest keeping at least 6.

We also examined the optional text-based responses in our survey.
22 respondents specified an additional method for how they source data. %, with a few listing multiple in the same text field. 
Of this, the majority (15) of sources were some form of database, such as Semantic Scholar, Scopus, Microsoft Academic, KeyVis, and even their own university's library. This was not surprising, as it would not have been possible to exhaustively list all of these options in the survey, and they are all similar in nature.
There were a few interesting sources that we did miss in hindsight however, such as directly accessing personal webpages and blogs (4) and via social media such as Slack or Twitter (2).
For the importance of criteria in determining whether to read a paper, 18 responses where given---11 of which made some reference to the paper's relevance being important. We had intentionally chosen not to include this as a Likert item, due to relevance not being immediately obvious when given only a paper's metadata. Other responses included the clarity of writing (2), presence of clear images and/or video figures (1), and easy access to the actual publication PDF (1).
Only 7 responses were given for alternatives to assess venue quality, 5 of which mention the respondent's own familiarity with the venue. While this makes sense, their past experiences may have also been shaped by some of the other factors we included. The other two responses were the venue's relevance to their own topic, and interestingly the sponsor of the venue (e.g., IEEE Computer Society).
