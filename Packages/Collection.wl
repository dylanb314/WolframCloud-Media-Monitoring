BeginPackage["Monitoring`"];

GatherData;

Begin["`Private`"];

GatherData[config_, start_DateObject] :=
    Module[{token, path, configData, term, dataset},
        (* Get Config Data *)
        configData = Get[config];
        token = Lookup[configData, "token"];
        term = Lookup[configData, "term"];
        path = StringTemplate[Lookup[configData, "path-template"]][term, "raw", DateString[start, {"Year","-","Month","-","Day"}]];
        (* Import Data *)
        dataset = DataReading[token, term, start];
        (* Store Raw Data *)
        CloudExport[FieldFilter[KeyFilter[dataset]], "MX", path]
    ]
GatherData[___] := $Failed

DataReading[token_, term_, start_DateObject] :=
    Module[{response, json, posts},
        (* Call API *)
        response = APIRequest[token, term, start];
        If[
            response["StatusCode"] =!= 200,
            Return[$Failed]
        ];
        (* Import JSON *)
        json = ImportFormatJSON[
            response["Body"],
            First[
                StringCases[
                    response["ContentType"],
                    ___~~" charset="~~e__ :> ToUpperCase[e]
                ]
            ]
        ];
        If[FailureQ[json], Return[$Failed]];
        posts = Lookup[json, "posts"];
        Dataset[
            Replace[
                posts,
                {r__Rule} :> Association[r],
                Infinity
            ]
        ]
    ]

APIRequest[token_, term_, start_DateObject] :=
    Module[{request, response, encoding},
        request = HTTPRequest[
            "http://webhose.io/filterWebContent",
            <|
                "Query" -> {
                    "token" -> token,
                    "format" -> "json",
                    "sort" -> "published",
                    "q" -> StringRiffle[
                        {
                            "text:\""<>term<>"\"",
                            "language:english",
                            "is_first:true",
                            "domain_rank:<5000",
                            StringJoin[{
                                "published:>",
                                ToString[1000*UnixTime[DateObject[start, TimeObject[{0,0,0}]]]]
                            }]
                        },
                        " "
                    ]
                }
            |>
        ];

        URLRead[request, {"StatusCode", "Body", "ContentType"}]
    ]

ImportFormatJSON[body_, encoding_] :=
    Module[{encoded},
        encoded = ToString[body, CharacterEncoding -> encoding];
        Check[
            ImportString[encoded, "JSON"],
            $Failed
        ]
    ]

$MajorKeys = {
    "text",
    "url",
    "language",
    "thread",
    "published",
    "author"
};

KeyFilter[dataset_] :=
    Map[
        KeySelect[
            #,
            MemberQ[
                $MajorKeys,
                #
            ]&
        ]&,
        dataset
    ];

$MinorFields = {
    "title",
    "replies_count",
    "participant_count",
    "site_type",
    "country",
    "spam_score",
    "performance_score",
    "domain_rank",
    "social"
};

FieldFilter[dataset_] :=
    MapAt[
        KeySelect[
            #,
            MemberQ[
                $MinorFields,
                #
            ]&
        ]&,
        dataset,
        {All, Key["thread"]}
    ];

End[];

EndPackage[];