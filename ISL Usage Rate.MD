Usage Rate is a concept borrowed from basketball analysis. According to Martin Hawkes-Teeter (who is also the inspiration for this analysis), Usage Rate is an attempt to measure the influence each player has in a team's attack by looking at the percentage of a team's possessions he "uses", or has the final action of a possession, weighted by minutes played.

I have collected data from the last 3 seasons of the Indian Super League (the 10-team seasons from 2017-18) and put Hawkes-Teeter's concept of Usage Rate into practice, with a few tweaks.

A possessions framework is used, which is different from larger accumulations of individual counting stats like per-game or per-90s. Each possession can have a series of events like a successful pass, a successful dribble, a cross, an assist and a shot. Another example can be a successful pass, a successful pass, an unsuccessful pass. 

Only shots which are not direct free kicks or penalties are considered, while possessions are considered to end upon the occurence of the following events:
1) Shot
2) Inaccurate pass
3) Unsuccessful take-on
5) Dispossessed
4) Foul by opposition
5) Offside

Due to a lack of better data, I have made an assumption that dribbles are equal to take-ons (which shouldn't be the case in reality). Also, shots after a corner or a free kick or throw in are taken into account despite them being set pieces or very short possessions in general.

Furthermore, the dataset consists of only league stage Indian Super League matches, not the final stage games.

The attempt here is to find out who have been the most influential players for their respective teams over the past few seasons, and a few other derived analyses from the Usage Rate concept.

R for data visualization and data wrangling, RShiny for data presentation via webapp, and MS Excel for data cleaning and data wrangling, have been used.
