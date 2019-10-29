BeginPackage["Monitoring`"];

GenerateTimeSeries;

Get[FileNameJoin[{FileNameDrop[$InputFileName, -1], "Utilities.wl"}]];

Begin["`Private`"];

GenerateTimeSeries[configFile_, date_DateObject] :=
    Module[{config, term, data, formatted},
        config= Get[configFile];
        term = Lookup[config, "term"];
        data = GetData[StringTemplate[Lookup[config, "path-template"]][term, "final", DateString[date, {"Year","-","Month","-","Day"}]]];
        Print[data];
        formatted = data[All, "Timestamp"];

        DateHistogram[
            formatted,
            formatRange[date],
            AspectRatio -> StyleSetting["AspectRation"],
            ChartLayout -> "Stacked",
            ChartLegends -> None,
            ChartStyle -> Directive[
                Opacity[0.75],
                EdgeForm[Directive[Opacity[0], Thickness[0]]],
                ColorData[StyleSetting["ColorProfile"]][0]
            ],
            ImageSize -> StyleSetting["ImageSize"],
            PlotLabel -> Style[
                StringJoin[term, " Mention Timeline"],
                FontFamily -> StyleSetting["FontFamily"],
                FontSize -> StyleSetting["TitleFontSize"]
            ],
            PlotTheme -> StyleSetting["Theme"],
            Ticks -> {Automatic, None}
        ]
    ]

formatRange[date_] :=
    Append[
        Map[
            DateObject[#, TimeObject[{0, 0, 0}]]&,
            {date-Quantity[1, "Days"], date}
        ],
        Quantity[1, "Hours"]
    ]

End[];

EndPackage[];