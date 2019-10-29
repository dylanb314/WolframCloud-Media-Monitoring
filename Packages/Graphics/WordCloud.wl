BeginPackage["Monitoring`"];

GenerateWordCloud;

Get[FileNameJoin[{FileNameDrop[$InputFileName, -1], "Utilities.wl"}]];

Begin["`Private`"];

GenerateWordCloud[configFile_, date_DateObject] :=
    Module[{config, term, data, formatted},
        config= Get[configFile];
        term = Lookup[config, "term"];
        data = GetData[StringTemplate[Lookup[config, "path-template"]][term, "final", DateString[date, {"Year","-","Month","-","Day"}]]];

        formatted = Normal[data[All, "Text"][DeleteMissing][DeleteStopwords][StringSplit][Flatten][Select[DictionaryWordQ[#] &]][ToLowerCase][Counts]];

        WordCloud[
            formatted,
            ColorFunction -> StyleSetting["ColorProfile"],
            ImageSize -> StyleSetting["ImageSize"]
        ]
    ]

End[];

EndPackage[];