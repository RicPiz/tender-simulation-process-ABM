# tender-simulation-process-ABM

## Overview

An example of an ABM simulation concerning a tender context. The goal of the model is to check the robustness of agents’ bidding decision making in different bidding configurations, starting with a study of decision-making and behavioral models in the literature. Simulation essentially corresponds to a repeated “game” in which a number N tenders are played, in which it is possible to set the number of repetitions and the various combinations of reference variables. In this way one can explore the range of possible behaviors and isolate those of greatest interest. Once played, there will be a bidding phase and an evaluation phase in each tender.

![Model Diagram](https://github.com/RicPiz/tender-simulation-process-ABM/blob/e61080ff7ee37354fc826f1a685cafa1233c7a4a/model%20_diagram.jpg)

Agent-players are characterized by a certain level of experience, risk attitude, and quality. They start from a homogeneous situation, in which the variables are the same for everyone and it is possible to set initial configurations. Experience starts at 1 and changes over time depending on the results achieved: that of winners grows less than that of losers to the extent one chooses. Risk attitude is related to players’ level of experience: less experience corresponds to propensity and an increase in the base bid by a certain percentage, more experience corresponds to aversion and a decrease in the base bid by a certain percentage. The size of the percentage in question is also arbitrarily set within a defined range of values. Players also have memory of both their own bids and winning bids and will use this information to calculate the adjustment, the percentage deviation given by the ratio of their bidding history to the winning bid, which will also influence the size of the final bid. Agent-evaluators may have different behavioral attitudes from each other (average and extreme) that affect their assessment, which is based on specific threshold values. These rest on the maximum and minimum ratio that there can be between the quality and price ranges used. The evaluators then assign scores to the bids received based on their character, which varies from tender to tender. From the evaluation phase, a winner will emerge, corresponding to the player with the best score based on the quality-price ratio of their bid.


# Design concept

The model is based on assumptions from decision-making and behavior encountered in the literature. It assumes that participants have already made the decision to bid (*bid/no bid decision*), that they consider the opportunity profitable, and that they have the financial means to participate. Their goal will then simply be to win the bid and be awarded the contract.

The players' decision-making process is autonomous and is shaped by a number of weighted factors that influence the final bid size. In the literature, experience, risk aptitude, and quality are considered decisive elements in bidding strategy. Experience and risk attitude are interrelated, change over time, and influence future choices. Several studies show that experience and risk aptitude are inversely proportional. Inexperienced subjects sin by overconfidence, which makes their choices riskier and open to large potential profits and, in turn, large potential losses. In contrast, more experienced individuals tend to make less risky decisions, preferring a strategy that is profitable and competitive in the long run.

In this case, because it is a specific, repetitive, and not too dynamic context, we will assume that experience increases as well as risk aversion as time progresses. Being a highly competitive environment and characterized by imperfect information for agents, strategies will tend to conform in the long run, as participants by referring to previous winning bids, will make an adjustment on their bid future as indicated by the following formula:

$$\left( {1 - \frac{{\sum\limits_{i = 1}^N {\frac{{{b_i}}}{{{w_i}}}} }}{N}} \right) \cdot 100$$

A summation of the ratios of own bids, represented by $b_i$, and winning bids, represented by $w_i$, is made by dividing by $N$, which corresponds to the number of bids made. The resulting value is subtracted from 1 and multiplied by 100 to derive the average percentage deviation between own bids and winning bids. The bid size must be low enough to be competitive but high enough to be profitable. In this sense, by knowing the winning price of previous bids, players can calibrate that for future bids. This and the focus of the bidding strategies most discussed in the literature.

The object of the decision is thus the bid price. Agents start from a baseline of homogeneity in terms of experience while the level of quality offered will be subjective. The attitude to risk is related to the level of experience and will vary over time as experience changes. In this way, a certain heterogeneity will be formed in the short term dictated by the results of individual competitions, and then arrive at more homogeneous situations over time. The size of the bid will turn out to be influenced by these factors.

The evaluators' decision-making process is autonomous, based on their aptitude for scoring in compliance with threshold values, the bid award criterion based on best value for money, and the experience of the players. In contrast to the lowest price criterion, the best value-for-money criterion meets the quality standards required by the most modern and complex projects. It is pointed out in the literature that this has resulted in a shift from an evaluation based solely on price to a multi-criteria one called the Most Economically Advantageous Tender (MEAT) thus going on to change the decision framework of both evaluators and bidders.
