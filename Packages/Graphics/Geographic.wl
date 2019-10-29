BeginPackage["Monitoring`"];

GenerateMap;

Get[FileNameJoin[{FileNameDrop[$InputFileName, -1], "Utilities.wl"}]];

Begin["`Private`"];

GenerateMap[configFile_, date_DateObject] :=
    Module[{config, term, data, formatted},
        config= Get[configFile];
        term = Lookup[config, "term"];
        data = GetData[StringTemplate[Lookup[config, "path-template"]][term, "final", DateString[date, {"Year","-","Month","-","Day"}]]];

        formatted = Normal[Counts[data[All, "Country"][DeleteMissing]]];

        GeoBubbleChart[
            Total /@ formatted,
            ChartStyle -> Directive[Opacity[1], ColorData[StyleSetting["ColorProfile"]][0]],
            GeoBackground -> StyleSetting["GeoBackground"],
            GeoProjection -> StyleSetting["GeoProjection"],
            GeoRange -> "World",
            GeoZoomLevel -> 2,
            ImageSize -> StyleSetting["ImageSize"],
            PlotLabel -> Style[
                StringJoin["Geographics Distribution of ", term, " Mentions"],
                FontFamily -> StyleSetting["FontFamily"],
                FontSize -> StyleSetting["TitleFontSize"]
            ]
        ]
    ]

End[];

EndPackage[];