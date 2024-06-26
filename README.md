# FootballAnalysisTool
Shiny App to analyze Opta data in order to extract insights that determine teams styles of playing, important players, and stenghts and weaknesses.

The aim of this app is to automatize the process of creating Rival reports through various visualizations, analyzing teams through the different game phases, such as static attacks, transitions or set pieces, and gaining knowledge about how they are set up in attack and defense in all this phases of the game.

## Pre-Processing
1. To reach the final product, I first collected data from different sources. To have the eventing of the matches, I extracted them from [WhoScored](https://es.whoscored.com), which uses the Opta eventing. This operation can be found in [WhoscoredData.R](DataProcessing/WhoscoredData.R).

2. Then, I realised this eventing that Whoscored provided did not include shots information such as xG or xGOT, so I extracted it from [FotMob](https://www.fotmob.com). This can be found in [FotmobShots.R](DataProcessing/FotmobShots.R).

3. After having match eventing and shots information, I merged them to have a more compact eventing file. This union is in the file [Union_Eventing_Shots.R](DataProcessing/Union_Eventing_Shots.R).

4. Once the eventing information was ready, I proceeded to collect players information. To complete that task, I created a dataset using [TransferMarkt](https://www.transfermarkt.es) as the main source to have the information of players from numerous European Leagues. This operation is performed in [TransfermarktData.R](DataProcessing/TransfermarktData.R).

5. To finish this previous data processing task, I connected the players information from Opta with the Transfermarkt dataset, and solved discrepancies between both sources. This can be found in [Union_Transfermarkt.R](DataProcessing/Union_Transfermarkt.R).

6. At this point, I managed a solid eventing and players dataset, and I was ready to start creating the Shiny App. However, I wanted to create an algorythm that differenciated the phases present in a football game, to add context to the events in my dataset. This algorythm to add more valuable information to the eventing dataset is located in [GamePhases.R](DataProcessing/GamePhases.R).

7. [GraphicsTest.R](DataProcessing/GraphicsTest.R) is a Support Script to test visualizations of different aspects I analyzed, and to pass them to the App Script.


## Shiny App ([app.R](ShinyApp/app.R))
After setting the data to be manipulated, I created the the app, that allows the user to perform:

1. Individual match analysis, dividing the actions in the different phases of the game

2. Analysis of own team by season or for a period of time to choose, and by competition. Being divided in the different game phases, it allows to see your own team strengths, weaknesses and most decisive players.

3. Rival analysis by season or specific period of time, and by competition. Being divided in the different game phases, it allows to see if a team is dangerous pressing and counter-attacking, via organized attacks, set pieces... and to design strategies based on decrease this effect.

4. Identification of differential and important players in each phase of the game

5. Visualization and Reports creation support
