BeginPackage["Monitoring`"];

GenerateHistogram;

Get[FileNameJoin[{FileNameDrop[$InputFileName, -1], "Utilities.wl"}]];

Begin["`Private`"];

GenerateHistogram[configFile_, date_DateObject] :=
    Module[{config, term, data, formatted, total, rect, labels},
        config= Get[configFile];
        term = Lookup[config, "term"];
        data = GetData[StringTemplate[Lookup[config, "path-template"]][term, "final", DateString[date, {"Year","-","Month","-","Day"}]]];

        formatted = Normal[Counts[data[All, "Type"]]];
        total = Total[formatted];
        rect = generateSquares[{0, 0}, {5, 1}, Values[formatted]];
        labels = Keys[formatted];

        Labeled[
            Graphics[{
                EdgeForm[White],
                ColorData[StyleSetting["ColorProfile"]][0],
                MapThread[
                    Tooltip[
                        Rectangle @@ #1,
                        Row[{
                            #2,
                            ToString[Round[100*N[Lookup[formatted, #2, 0]/total], 0.01]] <>"%"},
                            Spacer[2],
                            BaseStyle -> {FontFamily -> StyleSetting["FontFamily"], FontSize -> StyleSetting["FontSize"]}
                        ],
                        TooltipDelay -> 0.6
                    ] &,
                    {rect, labels}
                ]},
                ImageSize -> Full,
                PlotRange -> {{0, 5}, {0, 1}}
            ],
            Style[
                StringJoin[
                    "Article Type Percentages for ",
                    term
                ],
                FontFamily -> StyleSetting["FontFamily"],
                FontSize -> StyleSetting["TitleFontSize"]
            ],
            Top
        ]
    ]

generateSquares =
    Function[{lowerleft, upperRight, tallies},
        If[
            Length[tallies] == 1,
            (* If there's only one element, it fills the space. *)
            {{lowerleft, upperRight}},
            (* else *)
            Module[{result = {}, total = Total[tallies]},
                Module[{area, tr = upperRight, originalLL = lowerleft, init = lowerleft},
                    area = Area[Rectangle[originalLL, tr]];
                    Do[
                        (* for each element *)
                        Module[{new, height = tr[[2]] - init[[2]],
                            width = tr[[1]] - init[[1]], parea, end = init},
                            new = {init, init};
                            parea = (tallies[[i]]/total);
                            If[
                                height <= width,
                                end[[2]] = init[[2]] + height;
                                end[[1]] = init[[1]] + N[(parea*area)/height];
                                init = {end[[1]], init[[2]]},
                                (* else *)
                                end[[1]] = init[[1]] + width;
                                end[[2]] = init[[2]] + N[(parea*area)/width];
                                init = {init[[1]], end[[2]]}
                            ];
                            new[[2]] = end;
                            result = Append[result, new];
                        ],
                        {i, 1, Length[tallies]}
                    ]
                ];
                result
            ]
        ]
    ];

generateSquareChart[tallies_List, colors_List, keys_List, total_] :=
    Graphics[{
        EdgeForm[White],
        MapThread[{
            #1,
            Tooltip[
                Rectangle @@ #2,
                Column[{
                    #3,
                    ToString[Round[N[#4/total]*100, 0.01]] <> "%"},
                    Alignment -> Left
                ],
                TooltipDelay -> 0.6
            ]} &,
            {
                colors,
                generateSquares[{0, 0}, {5, 1}, tallies],
                keys,
                tallies
            }
        ]},
        ImageSize -> Full,
        PlotRange -> {{0, 5}, {0, 1}}
    ]

generateSquareChart[tallies_List, color_, keys_List, total_] :=
    Graphics[{
        color,
        EdgeForm[White],
        MapThread[
            Tooltip[
                Rectangle @@ #1,
                Column[{
                    #2,
                    ToString[Round[N[#3/total]*100, 0.01]] <> "%"},
                    Alignment -> Left
                ],
                TooltipDelay -> 0.6
            ] &,
            {
                generateSquares[{0, 0}, {5, 1}, tallies],
                keys,
                tallies
            }
        ]},
        ImageSize -> Full,
        PlotRange -> {{0, 5}, {0, 1}}
    ]

generateSquareChart[min_, max_, tallies_List, color_, keys_List, total_] :=
    Graphics[{
        color,
        EdgeForm[White],
        MapThread[
            Tooltip[
                Rectangle @@ #1,
                Column[{
                    #2,
                    ToString[Round[N[#3/total]*100, 0.01]] <> "%"},
                    Alignment -> Left
                ],
                TooltipDelay -> 0.6
            ] &,
            {
                generateSquares[min, max, tallies],
                keys,
                tallies
            }
        ]},
        ImageSize -> Full,
        PlotRange -> Transpose[{min, max}]
    ]

End[];

EndPackage[];