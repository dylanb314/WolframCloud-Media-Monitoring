BeginPackage["Monitoring`"];

ProcessData;

Begin["`Private`"];

ProcessData[config_, date_DateObject] :=
    Module[{configData, term, raw, final, rawData, sorted},
        configData = Get[config];
        term = Lookup[configData, "term"];
        raw = StringTemplate[Lookup[configData, "path-template"]][term, "raw", DateString[date, {"Year","-","Month","-","Day"}]];
        final = StringTemplate[Lookup[configData, "path-template"]][term, "final", DateString[date, {"Year","-","Month","-","Day"}]];
        rawData = CloudImport[raw, "MX"];
        sorted = DataFormatting[rawData, term];
        CloudExport[sorted, "MX", final]
    ]
ProcessData[___] := $Failed

DataFormatting[raw_, term_] :=
    Module[{finalData},
        finalData = Map[
            Association[Through[
                Table[formattedValue[key], {key, $ProcessedKeys}][#, term]
            ]]&,
            raw
        ];
        SortBy[finalData, #["Timestamp"]&]
    ]

formattedValue["Timestamp"][entry_, _] :=
    Rule[
        "Timestamp",
        TimeZoneConvert[
            First[
                StringCases[
                    entry["published"],
                    date__ ~~ "." ~~ _ ~~ _ ~~ _ ~~ s : "+" | "-" ~~ tHour : DigitCharacter .. ~~ ":" ~~ tMin : DigitCharacter .. :>
                        DateObject[
                            {
                                date,
                                {"Year", "-", "Month", "-", "Day", "T", "Hour24", ":", "Minute", ":", "Second"}
                            },
                            TimeZone ->
                                Replace[s, {"+" -> 1, "-" -> -1}] * (FromDigits[tHour] + FromDigits[tMin] / 60)
                        ]
                ]
            ]
        ]
    ]
formattedValue["Title"][entry_, _] := Rule["Title", Replace[entry["thread"]["title"], "" -> None]]
formattedValue["Author"][entry_, _] := Rule["Author", Replace[entry["author"], "" -> Missing["Unknown"]]]
formattedValue["Text"][entry_, _] := Rule["Text", entry["text"]]
formattedValue["URL"][entry_, _] := Rule["URL", entry["url"]]
formattedValue["Type"][entry_, _] := Rule["Type", entry["thread"]["site_type"]]
formattedValue["Country"][entry_, _] :=
    Rule[
        "Country",
        Replace[
            Interpreter["Country"][entry["thread"]["country"]],
            _Failure ->
                If[
                    StringMatchQ[entry["thread"]["country"], "EU"],
                    EntityClass["Country", "EuropeanUnion"],
                    Missing["Unavailable"]
                ]
        ]
    ]
formattedValue["Language"][entry_, _] := Rule["Language", Interpreter["Language"][entry["language"]]]
formattedValue["Counts"][entry_, _] :=
    Rule[
        "Counts",
        <|
            "Replies" -> entry["thread"]["replies_count"],
            "Participants" -> entry["thread"]["participants_count"]
        |>
    ]
formattedValue["Scores"][entry_, term_] :=
    Rule[
        "Scores",
        <|
            "Spam" -> entry["thread"]["spam_score"],
            "Performance" -> entry["thread"]["performance_score"],
            "Sentiment" ->
                If[
                    StringMatchQ[entry["language"], "english", IgnoreCase -> True],
                    With[{prob = Merge[
                            Map[
                                Classify["Sentiment", #, "Probabilities"] &,
                                Select[
                                    StringSplit[entry["text"], PunctuationCharacter],
                                    (* TextSentences[entry["text"]], *)
                                    StringContainsQ[
                                        WordBoundary ~~ ToLowerCase[term] ~~ WordBoundary,
                                        IgnoreCase -> True
                                    ]
                                ]
                            ],
                            Mean
                        ]},
                        If[
                            prob === <||>,
                            Missing["Indeterminate"],
                            Round[
                                Replace[
                                    Lookup[
                                        Association[
                                            Map[Reverse, Normal[prob]]
                                        ],
                                        Max[prob],
                                        0
                                    ],
                                    {
                                        "Neutral" -> 0,
                                        "Positive" -> 1,
                                        "Negative" -> -1
                                    }
                                    ] * Replace[
                                    Max[prob],
                                    -\[Infinity] -> 0
                                ],
                                0.001
                            ]
                        ]
                    ],
                    Missing["NotAvailable"]
                ]
        |>
    ]
formattedValue["DomainRank"][entry_, _] := Rule["DomainRank", Replace[entry["thread"]["domain_rank"], Null -> Missing["NotAvailable"]]]
formattedValue["Social"][entry_, _] := Rule["Social", entry["thread"]["social"]]

$ProcessedKeys = {
    "Timestamp",
    "Title",
    "Author",
    "Text",
    "URL",
    "Type",
    "Country",
    "Language",
    "Counts",
    "Scores",
    "DomainRank",
    "Social"
};

End[];

EndPackage[];