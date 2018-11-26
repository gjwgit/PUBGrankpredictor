BattleGrounds
=============

PlayerUnknown's BattleGrounds (PUBG) is an online multiplayer battle
royale game which has enjoyed massive popularity. With over 50 million
copies sold it's the fifth best selling game of all time and has
millions of active monthly players.

This demonstration, based on Anshul Bansal's
[notebook](https://notebooks.azure.com/user2209/libraries/pubgRankPredict),
explores a publicly available dataset with the aim to predict if a
player would make it to the top 10 position in the game. Based on the
player's after game statistics, the model is able to predict the
player position. Such results could be published on an online portal
or in media prior to any tournaments happening for cash prizes.

What's the best strategy to be in the top ten positions in PUBG?
Should you sit in one spot and hide your way into victory, or do you
need to be the top shot? Let's see if the data might tell us.

The Data Science Template was developed by Graham Williams based on
The Essentials of Data Science (2017)
<https://essentials.togaware.com>. The Template was applied to the
PUBG dataset by Anshul Bansal using a Jupyter Notebook <>. It was
transformed into a ML Hub package by Graham and Anshul.

Visit the github repository for more details:
<https://github.com/anshul2209/PUBGrankpredictor>

# Dataset

The team at PUBG has made official game data available for the public
to explore and scavenge outside of "The Blue Circle." The data was
pulled from Kaggle which itself collected the data through the
official PUBG Developer API -
<https://developer.pubg.com/apps?locale=en>

The dataset has column winPlacePerc, from which we are inferring our
target variable that is_top_ten (Player qualifies to be in top 10)

## Data fields

* DBNOs - Number of enemy players knocked.
* assists - Number of enemy players this player damaged that were
  killed by teammates.
* boosts - Number of boost items used.
* damageDealt - Total damage dealt. Note: Self inflicted damage is
  subtracted.
* headshotKills - Number of enemy players killed with headshots.
* heals - Number of healing items used.
* Id - Player’s Id
* killPlace - Ranking in match of number of enemy players killed.
* killPoints - Kills-based external ranking of player. (Think of this
  as an Elo ranking where only kills matter.) If there is a value
  other than -1 in rankPoints, then any 0 in killPoints should be
  treated as a “None”.
* killStreaks - Max number of enemy players killed in a short amount
  of time.
* kills - Number of enemy players killed.
* longestKill - Longest distance between player and player killed at
  time of death. This may be misleading, as downing a player and
  driving away may lead to a large longestKill stat.
* matchDuration - Duration of match in seconds.
* matchId - ID to identify match. There are no matches that are in
  both the training and testing set.
* matchType - String identifying the game mode that the data comes
  from. The standard modes are “solo”, “duo”, “squad”, “solo-fpp”,
  “duo-fpp”, and “squad-fpp”; other modes are from events or custom
  matches.
* rankPoints - Elo-like ranking of player. This ranking is
  inconsistent and is being deprecated in the API’s next version, so
  use with caution. Value of -1 takes place of “None”.
* revives - Number of times this player revived teammates.
* rideDistance - Total distance traveled in vehicles measured in
  meters.
* roadKills - Number of kills while in a vehicle.
* swimDistance - Total distance traveled by swimming measured in
  meters.
* teamKills - Number of times this player killed a teammate.
* vehicleDestroys - Number of vehicles destroyed.
* walkDistance - Total distance traveled on foot measured in meters.
* weaponsAcquired - Number of weapons picked up.
* winPoints - Win-based external ranking of player. (Think of this as
  an Elo ranking where only winning matters.)  If there is a value
  other than -1 in rankPoints, then any 0 in winPoints should be
  treated as a “None”.
* groupId - ID to identify a group within a match. If the same group
  of players plays in different matches, they will have a different
  groupId each time.
* numGroups - Number of groups we have data for in the match.
* maxPlace - Worst placement we have data for in the match. This may
  not match with numGroups, as sometimes the data skips over
  placements.
* winPlacePerc - This is a percentile winning placement, where 1
  corresponds to 1st place, and 0 corresponds to last place in the
  match. It is calculated off of maxPlace, not numGroups, so it is
  possible to have missing chunks in a match.
* is_top_ten - The target of prediction. Is the player present in top
  10 position.  "

