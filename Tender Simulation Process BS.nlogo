;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TENDER SIMULATION PROCESS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
  id-active-tender
  winning-bids-list
]

breed [tenders tender]
breed [evaluators evalutator]
breed [players player]

tenders-own [
  id
  value
  winner
  bids
]

evaluators-own [
  attitude
  rates-list
  threshold-high-max
  threshold-high-min
  threshold-med-max
  threshold-med-min
  threshold-low-max
  threshold-low-min
]

players-own [
  experience
  quality
  risk-attitude
  current-winner?
  my-bid
  bid-strategy
  bid-adjustment  ; corrective action based on the ratio of bids to winning bids
  history-bids
  winner?
]

to setup
  clear-all
  set id-active-tender 0
  set winning-bids-list []
  build-players ; N.B. this MUST be before the others, players must have id in [0, N-PLAYERS - 1]
  build-evaluators
  reset-ticks
end

to simulate-n-rounds
  repeat number-rounds [go]
end

to go
  if ticks = number-rounds [stop]
  simulate-tender
  make-bids
  evaluate-winner
  update-strategies
  tick
end

to build-evaluators
  create-evaluators 3 [
    set shape "person"
    set color red
    setxy (who - 1) (max-pycor - 2)
    set attitude one-of ["extreme" "medium"]
  ]
end

to build-players
  create-players number-players [
  set shape "person"
  set color green
  setxy (who - 5) (min-pycor + 2)
  set experience 1
  set bid-strategy (random 51) + 50  ; value between 50 and 100
  set risk-attitude 0
  set bid-adjustment 0
  set history-bids []
  set current-winner? false
 ]
end

to simulate-tender
  ask tenders [die]
  ask links [die]
  set id-active-tender id-active-tender + 1
  create-tenders 1 [
    set id id-active-tender
    set shape "flag"
    set color yellow
    set size 2
    set value 100  ; tender value fixed at 100
  ]
end

to make-bids
  let v [value] of one-of tenders with [id = id-active-tender]
  output-print "Bidding phase"
  output-print word "Tender value : " v

  ask players [
    set quality (random quality-base) + 5    ; random quality from 5 min to 10 max

    if [experience] of one-of players <= 5 [
      set risk-attitude propension  ; bid-strategy influenced by risk-attitude (propensity, +x%) and adj (+/-)
      set my-bid ((bid-strategy + (bid-strategy * risk-attitude)) * (1 + bid-adjustment))
    ]

    if [experience] of one-of players > 5 and [experience] of one-of players <= 10 [
      set risk-attitude avversion-5<exp<10   ; bid-strategy influenced by risk-attitude (aversion, -x%) and adj (+/-)
      set my-bid ((bid-strategy - (bid-strategy * risk-attitude)) * (1 + bid-adjustment))
    ]

    if [experience] of one-of players > 10 [
      set risk-attitude avversion-exp>10   ; bid-strategy influenced by risk-attitude (aversion, -x%) and adj (+/-)
      set my-bid ((bid-strategy - (bid-strategy * risk-attitude)) * (1 + bid-adjustment))
    ]

  if my-bid <= 0 [ set my-bid 1 ]

  create-link-to one-of tenders [
    set label-color white
    set label [who] of myself
  ]

  output-print (sentence "Player" who "bid" precision my-bid 3 "with quality" quality "and with experience" experience)
 ]
end

to evaluate-winner
  output-print "Evaluation phase"

  let players-list sort-by < [who] of players with [my-bid > 0]    ; player list
  let bids-list []                                                 ; bids list
  let players-exp []                                               ; player experience list
  let quality-list []                                              ; player quality list
  let best-value-list []                                           ; price/quality ratio list
  foreach players-list [
    p ->
    if [my-bid] of player p > 0 [
    set bids-list lput (precision [my-bid] of player p 3) bids-list]
    set players-exp lput (precision [experience] of player p 3) players-exp
    set quality-list lput ([quality] of player p) quality-list
    set best-value-list lput ([quality] of player p / precision [my-bid] of player p 3) best-value-list]

  ask evaluators [
    let max-rate 0
    let mid-rate 0
    let min-rate 0

    if attitude = "medium" [
      set max-rate one-of [7 7.5 8]
      set mid-rate one-of [5.5 6 6.5]
      set min-rate one-of [4 4.5 5]
    ]

    if attitude = "extreme" [
      set max-rate one-of [9 9.5 10]
      set mid-rate one-of [5 5.5 6]
      set min-rate one-of [2 2.5 3]
    ]

    ; thresholds based on maximum and minimum ratios that there can be between quality and price ranges used
    ; [max = (10/50 = 0.2); min = (5/100 = 0.05)]
    set threshold-high-max 0.2        ; high threshold from 0.15 to 0.2
    set threshold-high-min 0.15
    set threshold-med-max 0.15        ; medium threshold from 0.1 to 0.14
    set threshold-med-min 0.1
    set threshold-low-max 0.1         ; low threshold from 0.05 to 0.09
    set threshold-low-min 0.05

    set rates-list []                          ; rating list
    foreach best-value-list [b ->
      let p 0
      if b >= threshold-low-min and b < threshold-low-max [set p min-rate] ; if value in low threshold, assign min-rate
      if b >= threshold-med-min and b < threshold-med-max [set p mid-rate] ; if value within medium threshold, assign mid-rate
      if b >= threshold-high-min and b <= threshold-high-max [set p max-rate] ; if value in high threshold, assign max-rate
      if b < threshold-low-min [set p 1]    ; if value below the minimum threshold, assign rate 1
      if b > threshold-high-max [set p 1]   ; If value above the maximum threshold, assign rate 1
      set rates-list lput p rates-list
    ]

    output-print (sentence "Evaluator" who "give rates:" rates-list)
  ]

  output-print "Final Decision"


  let total-sum []
  foreach players-list [
    i ->
    let index position i players-list                                           ; identifies players position in the list
    set total-sum lput (sum [item index rates-list] of evaluators) total-sum    ; sum of votes per player

    output-print (sentence "Player" i "get evaluation" item index total-sum)
  ]

  let count-max-rates 0
  let max-rate max total-sum
  let max-index position max-rate total-sum

  foreach total-sum [ ; find how many maxima there are in total-sum
    s ->
    if (s = max-rate) [
      set count-max-rates count-max-rates + 1
      ask player position s total-sum [ set current-winner? true]
    ]
  ]

  ask players [set winner? false]
  ifelse count-max-rates = 1
  [
    ask player max-index [set winner? true]
    output-print (sentence "The winner is" max-index "with rate" max-rate
                  "and with experience" [experience] of player max-index)
  ]
  [
    let max-exp max [experience] of players with [current-winner?]
    ask one-of players with [current-winner? and max-exp = experience] [set winner? true]
    output-print (sentence "I found " count players with [winner?] "winners")
  ]

end

to update-strategies
  ask players [
    set history-bids fput my-bid history-bids                     ; create history-bids by adding my-bid
  ]
  ask players[
    ifelse winner? = true [
      if ticks = length winning-bids-list [
        set winning-bids-list fput my-bid winning-bids-list]      ; create a list of winning bids with my winning bids
      set experience experience + winner-exp]                     ; winner experience increases by tot
    [
      set experience experience + loser-exp                       ; losers experience increases by tot
    ]
  ]

  ask players [
    let c 0
    foreach range (length winning-bids-list) [
      i ->
      let a item i winning-bids-list
      let b item i history-bids
      set c (c + (b / a))                                         ; ratio between my-bid and winning bids
    ]
    if length winning-bids-list > 0 [
      set bid-adjustment (1 - (c / length winning-bids-list))]    ; set up adjustment based on the deviation %
  ]
end

to-report bids-bs
  report [my-bid] of players                          ; reporter for BS on all player offers
end

to-report win-bid
  report [my-bid] of players with [winner? = true]    ; reporter for BS on players' winning bids
end

to-report bid-adj
  report [bid-adjustment * 100] of players            ; reporter for BS on adjustment
end
@#$#@#$#@
GRAPHICS-WINDOW
445
10
911
477
-1
-1
13.9
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
125
113
297
146
number-players
number-players
2
10
10.0
1
1
NIL
HORIZONTAL

BUTTON
129
12
192
45
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
220
12
283
45
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
124
174
296
207
number-rounds
number-rounds
1
100
1.0
1
1
NIL
HORIZONTAL

OUTPUT
961
10
1452
583
11

BUTTON
142
59
273
92
NIL
simulate-n-rounds
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
123
231
295
264
quality-base
quality-base
0
6
0.0
1
1
NIL
HORIZONTAL

SLIDER
123
283
295
316
propension
propension
0.05
0.1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
122
334
294
367
avversion-5<exp<10
avversion-5<exp<10
0.025
0.05
0.05
0.005
1
NIL
HORIZONTAL

SLIDER
121
388
293
421
avversion-exp>10
avversion-exp>10
0.05
0.1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
120
443
292
476
winner-exp
winner-exp
0.25
0.50
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
120
503
292
536
loser-exp
loser-exp
0.50
1
1.0
0.05
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

An example of an ABM simulation concerning a tender context. The goal of the model is to check the robustness of agents' bidding decision making in different bidding configurations, starting with a study of decision-making and behavioral models in the literature. Simulation essentially corresponds to a repeated "game" in which a number N tenders are played, in which it is possible to set the number of repetitions and the various combinations of reference variables. In this way one can explore the range of possible behaviors and isolate those of greatest interest. Once played, there will be a bidding phase and an evaluation phase in each tender.

## HOW IT WORKS

Agent-players are characterized by a certain level of experience, risk attitude, and quality. They start from a homogeneous situation, in which the variables are the same for everyone and it is possible to set initial configurations. Experience starts at 1 and changes over time depending on the results achieved: that of winners grows less than that of losers to the extent one chooses. Risk attitude is related to players' level of experience: less experience corresponds to propensity and an increase in the base bid by a certain percentage, more experience corresponds to aversion and a decrease in the base bid by a certain percentage. The size of the percentage in question is also arbitrarily set within a defined range of values. Players also have memory of both their own bids and winning bids and will use this information to calculate the adjustment, the percentage deviation given by the ratio of their bidding history to the winning bid, which will also influence the size of the final bid. Agent-evaluators may have different behavioral attitudes from each other (average and extreme) that affect their assessment, which is based on specific threshold values. These rest on the maximum and minimum ratio that there can be between the quality and price ranges used. The evaluators then assign scores to the bids received based on their character, which varies from tender to tender. From the evaluation phase, a winner will emerge, corresponding to the player with the best score based on the quality-price ratio of their bid.

## HOW TO USE IT

Items in interface tab:

- **Number of players**: from a minimum of 2 to a maximum of 10. In this way, it is possible to observe how the higher or lower number of players affects the performance of players and the size of the supply.

- **Number of rounds**: from a minimum of 1 to a maximum of 100. This makes it possible to explore short-, medium- and long-term scenarios.

- **Quality**: from a minimum of 5 to a maximum of 10. 5 represents a mediocre technical bid, while 10 represents a high technical bid. Values of less than five were not contemplated because, since the evaluation criterion is that of best value for money, a particularly low technical offer would not make sense and would be discarded immediately.

- **Risk propensity**: a minimum of 5% to a maximum of 10%. This means that agents with a certain level of experience (less than or equal to 5) will increase their bid by a percentage between 5 and 10. Larger values were not chosen because since it is a downward bidding overstretch in the size of the bid would not would prove competitive.

- **Risk aversion**: is divided into two categories, distinguished by the relative experience level of the players. For the experience range 5-10 (inclusive), players will lower their bid by 2.5 percent minimum and 5 percent maximum. In contrast, players with experience greater than 10 will lower their bid by 5% minimum and 1% maximum. This underscores how risk attitude is related to experience level and how as experience increases the latter, aversion also increases.

- **Experience level**: the basic starting value is 1, and this variable is also divided into two categories, the one for winners and the one for losers. For the former the increase is a minimum of 0.25 and a maximum of 0.50, and for the latter it is a minimum of 0.50 and a maximum of 1. This is because we assume that losers learn faster and gain more experience than winners, leading in the long run to a more or less even situation.

## THINGS TO TRY

The tool used for building the experiment and running the simulation is the BehaviorSpace, which is built into NetLogo and allows the model to be run many times by systematically varying the settings and recording the results of each individual execution of the same, a process called parameter sweeping.

## EXTENDING THE MODEL

The weight of certain effects in defined situations could be investigated, such as the effect of recourse, a very common practice that influences the performance of tenders, or the effect of different ratios of technical and economic offers within the MEAT award criterion, which would certainly require substantial modifications to bidding strategies.

## CREDITS AND REFERENCES

Made by Riccardo Pizzuti;
Linkedin: https://www.linkedin.com/in/ricpiz92/
GitHub: https://github.com/RicPiz
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Tender Process Simulation 1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>simulate-n-rounds</go>
    <timeLimit steps="100"/>
    <metric>bids-bs</metric>
    <metric>win-bid</metric>
    <metric>bid-adj</metric>
    <enumeratedValueSet variable="number-players">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-rounds">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quality-base">
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propension">
      <value value="0.05"/>
      <value value="0.07"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avversion-5&lt;exp&lt;10">
      <value value="0.025"/>
      <value value="0.035"/>
      <value value="0.045"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avversion-exp&gt;10">
      <value value="0.05"/>
      <value value="0.07"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winner-exp">
      <value value="0.25"/>
      <value value="0.35"/>
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="loser-exp">
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
